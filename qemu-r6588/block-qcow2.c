/*
 * Block driver for the QCOW version 2 format
 *
 * Copyright (c) 2004-2006 Fabrice Bellard
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
#include "qemu-common.h"
#include "block_int.h"
#include <zlib.h>
#include "aes.h"
#include <assert.h>

/*
  Differences with QCOW:

  - Support for multiple incremental snapshots.
  - Memory management by reference counts.
  - Clusters which have a reference count of one have the bit
    QCOW_OFLAG_COPIED to optimize write performance.
  - Size of compressed clusters is stored in sectors to reduce bit usage
    in the cluster offsets.
  - Support for storing additional data (such as the VM state) in the
    snapshots.
  - If a backing store is used, the cluster size is not constrained
    (could be backported to QCOW).
  - L2 tables have always a size of one cluster.
*/

//#define DEBUG_ALLOC
//#define DEBUG_ALLOC2

#define QCOW_MAGIC (('Q' << 24) | ('F' << 16) | ('I' << 8) | 0xfb)
#define QCOW_VERSION 2

#define QCOW_CRYPT_NONE 0
#define QCOW_CRYPT_AES  1

#define QCOW_MAX_CRYPT_CLUSTERS 32

/* indicate that the refcount of the referenced cluster is exactly one. */
#define QCOW_OFLAG_COPIED     (1LL << 63)
/* indicate that the cluster is compressed (they never have the copied flag) */
#define QCOW_OFLAG_COMPRESSED (1LL << 62)

#define REFCOUNT_SHIFT 1 /* refcount size is 2 bytes */

typedef struct QCowHeader {
    uint32_t magic;
    uint32_t version;
    uint64_t backing_file_offset;
    uint32_t backing_file_size;
    uint32_t cluster_bits;
    uint64_t size; /* in bytes */
    uint32_t crypt_method;
    uint32_t l1_size; /* XXX: save number of clusters instead ? */
    uint64_t l1_table_offset;
    uint64_t refcount_table_offset;
    uint32_t refcount_table_clusters;
    uint32_t nb_snapshots;
    uint64_t snapshots_offset;
} QCowHeader;

typedef struct __attribute__((packed)) QCowSnapshotHeader {
    /* header is 8 byte aligned */
    uint64_t l1_table_offset;

    uint32_t l1_size;
    uint16_t id_str_size;
    uint16_t name_size;

    uint32_t date_sec;
    uint32_t date_nsec;

    uint64_t vm_clock_nsec;

    uint32_t vm_state_size;
    uint32_t extra_data_size; /* for extension */
    /* extra data follows */
    /* id_str follows */
    /* name follows  */
} QCowSnapshotHeader;

#define L2_CACHE_SIZE 16

typedef struct QCowSnapshot {
    uint64_t l1_table_offset;
    uint32_t l1_size;
    char *id_str;
    char *name;
    uint32_t vm_state_size;
    uint32_t date_sec;
    uint32_t date_nsec;
    uint64_t vm_clock_nsec;
} QCowSnapshot;

typedef struct BDRVQcowState {
    BlockDriverState *hd;
    int cluster_bits;
    int cluster_size;
    int cluster_sectors;
    int l2_bits;
    int l2_size;
    int l1_size;
    int l1_vm_state_index;
    int csize_shift;
    int csize_mask;
    uint64_t cluster_offset_mask;
    uint64_t l1_table_offset;
    uint64_t *l1_table;
    uint64_t *l2_cache;
    uint64_t l2_cache_offsets[L2_CACHE_SIZE];
    uint32_t l2_cache_counts[L2_CACHE_SIZE];
    uint8_t *cluster_cache;
    uint8_t *cluster_data;
    uint64_t cluster_cache_offset;

    uint64_t *refcount_table;
    uint64_t refcount_table_offset;
    uint32_t refcount_table_size;
    uint64_t refcount_block_cache_offset;
    uint16_t *refcount_block_cache;
    int64_t free_cluster_index;
    int64_t free_byte_offset;

    uint32_t crypt_method; /* current crypt method, 0 if no key yet */
    uint32_t crypt_method_header;
    AES_KEY aes_encrypt_key;
    AES_KEY aes_decrypt_key;

    int64_t highest_alloc; /* highest cluester allocated (in clusters) */
    int64_t nc_free;       /* num of free clusters below highest_alloc */

    uint64_t snapshots_offset;
    int snapshots_size;
    int nb_snapshots;
    QCowSnapshot *snapshots;
} BDRVQcowState;

static int decompress_cluster(BDRVQcowState *s, uint64_t cluster_offset);
static int qcow_read(BlockDriverState *bs, int64_t sector_num,
                     uint8_t *buf, int nb_sectors);
static int qcow_read_snapshots(BlockDriverState *bs);
static void qcow_free_snapshots(BlockDriverState *bs);
static int refcount_init(BlockDriverState *bs);
static void refcount_close(BlockDriverState *bs);
static int get_refcount(BlockDriverState *bs, int64_t cluster_index);
static int update_cluster_refcount(BlockDriverState *bs,
                                   int64_t cluster_index,
                                   int addend);
static void update_refcount(BlockDriverState *bs,
                            int64_t offset, int64_t length,
                            int addend);
static int64_t alloc_clusters(BlockDriverState *bs, int64_t size);
static int64_t alloc_bytes(BlockDriverState *bs, int size);
static void free_clusters(BlockDriverState *bs,
                          int64_t offset, int64_t size);
#ifdef DEBUG_ALLOC
static void check_refcounts(BlockDriverState *bs);
#endif
static void scan_refcount(BlockDriverState *bs, int64_t *high, int64_t *free);


static int qcow_probe(const uint8_t *buf, int buf_size, const char *filename)
{
    const QCowHeader *cow_header = (const void *)buf;

    if (buf_size >= sizeof(QCowHeader) &&
        be32_to_cpu(cow_header->magic) == QCOW_MAGIC &&
        be32_to_cpu(cow_header->version) == QCOW_VERSION)
        return 100;
    else
        return 0;
}

static int qcow_open(BlockDriverState *bs, const char *filename, int flags)
{
    BDRVQcowState *s = bs->opaque;
    int len, i, shift, ret;
    QCowHeader header;

    /* Performance is terrible right now with cache=writethrough due mainly
     * to reference count updates.  If the user does not explicitly specify
     * a caching type, force to writeback caching.
     */
    if ((flags & BDRV_O_CACHE_DEF)) {
        flags |= BDRV_O_CACHE_WB;
        flags &= ~BDRV_O_CACHE_DEF;
    }
    ret = bdrv_file_open(&s->hd, filename, flags);
    if (ret < 0)
        return ret;
    if (bdrv_pread(s->hd, 0, &header, sizeof(header)) != sizeof(header))
        goto fail;
    be32_to_cpus(&header.magic);
    be32_to_cpus(&header.version);
    be64_to_cpus(&header.backing_file_offset);
    be32_to_cpus(&header.backing_file_size);
    be64_to_cpus(&header.size);
    be32_to_cpus(&header.cluster_bits);
    be32_to_cpus(&header.crypt_method);
    be64_to_cpus(&header.l1_table_offset);
    be32_to_cpus(&header.l1_size);
    be64_to_cpus(&header.refcount_table_offset);
    be32_to_cpus(&header.refcount_table_clusters);
    be64_to_cpus(&header.snapshots_offset);
    be32_to_cpus(&header.nb_snapshots);

    if (header.magic != QCOW_MAGIC || header.version != QCOW_VERSION)
        goto fail;
    if (header.size <= 1 ||
        header.cluster_bits < 9 ||
        header.cluster_bits > 16)
        goto fail;
    if (header.crypt_method > QCOW_CRYPT_AES)
        goto fail;
    s->crypt_method_header = header.crypt_method;
    if (s->crypt_method_header)
        bs->encrypted = 1;
    s->cluster_bits = header.cluster_bits;
    s->cluster_size = 1 << s->cluster_bits;
    s->cluster_sectors = 1 << (s->cluster_bits - 9);
    s->l2_bits = s->cluster_bits - 3; /* L2 is always one cluster */
    s->l2_size = 1 << s->l2_bits;
    bs->total_sectors = header.size / 512;
    s->csize_shift = (62 - (s->cluster_bits - 8));
    s->csize_mask = (1 << (s->cluster_bits - 8)) - 1;
    s->cluster_offset_mask = (1LL << s->csize_shift) - 1;
    s->refcount_table_offset = header.refcount_table_offset;
    s->refcount_table_size =
        header.refcount_table_clusters << (s->cluster_bits - 3);

    s->snapshots_offset = header.snapshots_offset;
    s->nb_snapshots = header.nb_snapshots;

    /* read the level 1 table */
    s->l1_size = header.l1_size;
    shift = s->cluster_bits + s->l2_bits;
    s->l1_vm_state_index = (header.size + (1LL << shift) - 1) >> shift;
    /* the L1 table must contain at least enough entries to put
       header.size bytes */
    if (s->l1_size < s->l1_vm_state_index)
        goto fail;
    s->l1_table_offset = header.l1_table_offset;
    s->l1_table = qemu_malloc(s->l1_size * sizeof(uint64_t));
    if (bdrv_pread(s->hd, s->l1_table_offset, s->l1_table, s->l1_size * sizeof(uint64_t)) !=
        s->l1_size * sizeof(uint64_t))
        goto fail;
    for(i = 0;i < s->l1_size; i++) {
        be64_to_cpus(&s->l1_table[i]);
    }
    /* alloc L2 cache */
    s->l2_cache = qemu_malloc(s->l2_size * L2_CACHE_SIZE * sizeof(uint64_t));
    s->cluster_cache = qemu_malloc(s->cluster_size);
    /* one more sector for decompressed data alignment */
    s->cluster_data = qemu_malloc(QCOW_MAX_CRYPT_CLUSTERS * s->cluster_size
                                  + 512);
    s->cluster_cache_offset = -1;

    if (refcount_init(bs) < 0)
        goto fail;

    scan_refcount(bs, &s->highest_alloc, &s->nc_free);

    /* read the backing file name */
    if (header.backing_file_offset != 0) {
        len = header.backing_file_size;
        if (len > 1023)
            len = 1023;
        if (bdrv_pread(s->hd, header.backing_file_offset, bs->backing_file, len) != len)
            goto fail;
        bs->backing_file[len] = '\0';
    }
    if (qcow_read_snapshots(bs) < 0)
        goto fail;

#ifdef DEBUG_ALLOC
    check_refcounts(bs);
#endif
    return 0;

 fail:
    qcow_free_snapshots(bs);
    refcount_close(bs);
    qemu_free(s->l1_table);
    qemu_free(s->l2_cache);
    qemu_free(s->cluster_cache);
    qemu_free(s->cluster_data);
    bdrv_delete(s->hd);
    return -1;
}

static int qcow_set_key(BlockDriverState *bs, const char *key)
{
    BDRVQcowState *s = bs->opaque;
    uint8_t keybuf[16];
    int len, i;

    memset(keybuf, 0, 16);
    len = strlen(key);
    if (len > 16)
        len = 16;
    /* XXX: we could compress the chars to 7 bits to increase
       entropy */
    for(i = 0;i < len;i++) {
        keybuf[i] = key[i];
    }
    s->crypt_method = s->crypt_method_header;

    if (AES_set_encrypt_key(keybuf, 128, &s->aes_encrypt_key) != 0)
        return -1;
    if (AES_set_decrypt_key(keybuf, 128, &s->aes_decrypt_key) != 0)
        return -1;
#if 0
    /* test */
    {
        uint8_t in[16];
        uint8_t out[16];
        uint8_t tmp[16];
        for(i=0;i<16;i++)
            in[i] = i;
        AES_encrypt(in, tmp, &s->aes_encrypt_key);
        AES_decrypt(tmp, out, &s->aes_decrypt_key);
        for(i = 0; i < 16; i++)
            printf(" %02x", tmp[i]);
        printf("\n");
        for(i = 0; i < 16; i++)
            printf(" %02x", out[i]);
        printf("\n");
    }
#endif
    return 0;
}

/* The crypt function is compatible with the linux cryptoloop
   algorithm for < 4 GB images. NOTE: out_buf == in_buf is
   supported */
static void encrypt_sectors(BDRVQcowState *s, int64_t sector_num,
                            uint8_t *out_buf, const uint8_t *in_buf,
                            int nb_sectors, int enc,
                            const AES_KEY *key)
{
    union {
        uint64_t ll[2];
        uint8_t b[16];
    } ivec;
    int i;

    for(i = 0; i < nb_sectors; i++) {
        ivec.ll[0] = cpu_to_le64(sector_num);
        ivec.ll[1] = 0;
        AES_cbc_encrypt(in_buf, out_buf, 512, key,
                        ivec.b, enc);
        sector_num++;
        in_buf += 512;
        out_buf += 512;
    }
}

static int copy_sectors(BlockDriverState *bs, uint64_t start_sect,
                        uint64_t cluster_offset, int n_start, int n_end)
{
    BDRVQcowState *s = bs->opaque;
    int n, ret;

    n = n_end - n_start;
    if (n <= 0)
        return 0;
    ret = qcow_read(bs, start_sect + n_start, s->cluster_data, n);
    if (ret < 0)
        return ret;
    if (s->crypt_method) {
        encrypt_sectors(s, start_sect + n_start,
                        s->cluster_data,
                        s->cluster_data, n, 1,
                        &s->aes_encrypt_key);
    }
    ret = bdrv_write(s->hd, (cluster_offset >> 9) + n_start,
                     s->cluster_data, n);
    if (ret < 0)
        return ret;
    return 0;
}

static void l2_cache_reset(BlockDriverState *bs)
{
    BDRVQcowState *s = bs->opaque;

    memset(s->l2_cache, 0, s->l2_size * L2_CACHE_SIZE * sizeof(uint64_t));
    memset(s->l2_cache_offsets, 0, L2_CACHE_SIZE * sizeof(uint64_t));
    memset(s->l2_cache_counts, 0, L2_CACHE_SIZE * sizeof(uint32_t));
}

static inline int l2_cache_new_entry(BlockDriverState *bs)
{
    BDRVQcowState *s = bs->opaque;
    uint32_t min_count;
    int min_index, i;

    /* find a new entry in the least used one */
    min_index = 0;
    min_count = 0xffffffff;
    for(i = 0; i < L2_CACHE_SIZE; i++) {
        if (s->l2_cache_counts[i] < min_count) {
            min_count = s->l2_cache_counts[i];
            min_index = i;
        }
    }
    return min_index;
}

static int64_t align_offset(int64_t offset, int n)
{
    offset = (offset + n - 1) & ~(n - 1);
    return offset;
}

static int grow_l1_table(BlockDriverState *bs, int min_size)
{
    BDRVQcowState *s = bs->opaque;
    int new_l1_size, new_l1_size2, ret, i;
    uint64_t *new_l1_table;
    uint64_t new_l1_table_offset;
    uint8_t data[12];

    new_l1_size = s->l1_size;
    if (min_size <= new_l1_size)
        return 0;
    while (min_size > new_l1_size) {
        new_l1_size = (new_l1_size * 3 + 1) / 2;
    }
#ifdef DEBUG_ALLOC2
    printf("grow l1_table from %d to %d\n", s->l1_size, new_l1_size);
#endif

    new_l1_size2 = sizeof(uint64_t) * new_l1_size;
    new_l1_table = qemu_mallocz(new_l1_size2);
    memcpy(new_l1_table, s->l1_table, s->l1_size * sizeof(uint64_t));

    /* write new table (align to cluster) */
    new_l1_table_offset = alloc_clusters(bs, new_l1_size2);

    for(i = 0; i < s->l1_size; i++)
        new_l1_table[i] = cpu_to_be64(new_l1_table[i]);
    ret = bdrv_pwrite(s->hd, new_l1_table_offset, new_l1_table, new_l1_size2);
    if (ret != new_l1_size2)
        goto fail;
    for(i = 0; i < s->l1_size; i++)
        new_l1_table[i] = be64_to_cpu(new_l1_table[i]);

    /* set new table */
    cpu_to_be32w((uint32_t*)data, new_l1_size);
    cpu_to_be64w((uint64_t*)(data + 4), new_l1_table_offset);
    if (bdrv_pwrite(s->hd, offsetof(QCowHeader, l1_size), data,
                sizeof(data)) != sizeof(data))
        goto fail;
    qemu_free(s->l1_table);
    free_clusters(bs, s->l1_table_offset, s->l1_size * sizeof(uint64_t));
    s->l1_table_offset = new_l1_table_offset;
    s->l1_table = new_l1_table;
    s->l1_size = new_l1_size;
    return 0;
 fail:
    qemu_free(s->l1_table);
    return -EIO;
}

/*
 * seek_l2_table
 *
 * seek l2_offset in the l2_cache table
 * if not found, return NULL,
 * if found,
 *   increments the l2 cache hit count of the entry,
 *   if counter overflow, divide by two all counters
 *   return the pointer to the l2 cache entry
 *
 */

static uint64_t *seek_l2_table(BDRVQcowState *s, uint64_t l2_offset)
{
    int i, j;

    for(i = 0; i < L2_CACHE_SIZE; i++) {
        if (l2_offset == s->l2_cache_offsets[i]) {
            /* increment the hit count */
            if (++s->l2_cache_counts[i] == 0xffffffff) {
                for(j = 0; j < L2_CACHE_SIZE; j++) {
                    s->l2_cache_counts[j] >>= 1;
                }
            }
            return s->l2_cache + (i << s->l2_bits);
        }
    }
    return NULL;
}

/*
 * l2_load
 *
 * Loads a L2 table into memory. If the table is in the cache, the cache
 * is used; otherwise the L2 table is loaded from the image file.
 *
 * Returns a pointer to the L2 table on success, or NULL if the read from
 * the image file failed.
 */

static uint64_t *l2_load(BlockDriverState *bs, uint64_t l2_offset)
{
    BDRVQcowState *s = bs->opaque;
    int min_index;
    uint64_t *l2_table;

    /* seek if the table for the given offset is in the cache */

    l2_table = seek_l2_table(s, l2_offset);
    if (l2_table != NULL)
        return l2_table;

    /* not found: load a new entry in the least used one */

    min_index = l2_cache_new_entry(bs);
    l2_table = s->l2_cache + (min_index << s->l2_bits);
    if (bdrv_pread(s->hd, l2_offset, l2_table, s->l2_size * sizeof(uint64_t)) !=
        s->l2_size * sizeof(uint64_t))
        return NULL;
    s->l2_cache_offsets[min_index] = l2_offset;
    s->l2_cache_counts[min_index] = 1;

    return l2_table;
}

/*
 * l2_allocate
 *
 * Allocate a new l2 entry in the file. If l1_index points to an already
 * used entry in the L2 table (i.e. we are doing a copy on write for the L2
 * table) copy the contents of the old L2 table into the newly allocated one.
 * Otherwise the new table is initialized with zeros.
 *
 */

static uint64_t *l2_allocate(BlockDriverState *bs, int l1_index)
{
    BDRVQcowState *s = bs->opaque;
    int min_index;
    uint64_t old_l2_offset, tmp;
    uint64_t *l2_table, l2_offset;

    old_l2_offset = s->l1_table[l1_index];

    /* allocate a new l2 entry */

    l2_offset = alloc_clusters(bs, s->l2_size * sizeof(uint64_t));

    /* update the L1 entry */

    s->l1_table[l1_index] = l2_offset | QCOW_OFLAG_COPIED;

    tmp = cpu_to_be64(l2_offset | QCOW_OFLAG_COPIED);
    if (bdrv_pwrite(s->hd, s->l1_table_offset + l1_index * sizeof(tmp),
                    &tmp, sizeof(tmp)) != sizeof(tmp))
        return NULL;

    /* allocate a new entry in the l2 cache */

    min_index = l2_cache_new_entry(bs);
    l2_table = s->l2_cache + (min_index << s->l2_bits);

    if (old_l2_offset == 0) {
        /* if there was no old l2 table, clear the new table */
        memset(l2_table, 0, s->l2_size * sizeof(uint64_t));
    } else {
        /* if there was an old l2 table, read it from the disk */
        if (bdrv_pread(s->hd, old_l2_offset,
                       l2_table, s->l2_size * sizeof(uint64_t)) !=
            s->l2_size * sizeof(uint64_t))
            return NULL;
    }
    /* write the l2 table to the file */
    if (bdrv_pwrite(s->hd, l2_offset,
                    l2_table, s->l2_size * sizeof(uint64_t)) !=
        s->l2_size * sizeof(uint64_t))
        return NULL;

    /* update the l2 cache entry */

    s->l2_cache_offsets[min_index] = l2_offset;
    s->l2_cache_counts[min_index] = 1;

    return l2_table;
}

static int size_to_clusters(BDRVQcowState *s, int64_t size)
{
    return (size + (s->cluster_size - 1)) >> s->cluster_bits;
}

static int count_contiguous_clusters(uint64_t nb_clusters, int cluster_size,
        uint64_t *l2_table, uint64_t start, uint64_t mask)
{
    int i;
    uint64_t offset = be64_to_cpu(l2_table[0]) & ~mask;

    if (!offset)
        return 0;

    for (i = start; i < start + nb_clusters; i++)
        if (offset + i * cluster_size != (be64_to_cpu(l2_table[i]) & ~mask))
            break;

	return (i - start);
}

static int count_contiguous_free_clusters(uint64_t nb_clusters, uint64_t *l2_table)
{
    int i = 0;

    while(nb_clusters-- && l2_table[i] == 0)
        i++;

    return i;
}

/*
 * get_cluster_offset
 *
 * For a given offset of the disk image, return cluster offset in
 * qcow2 file.
 *
 * on entry, *num is the number of contiguous clusters we'd like to
 * access following offset.
 *
 * on exit, *num is the number of contiguous clusters we can read.
 *
 * Return 1, if the offset is found
 * Return 0, otherwise.
 *
 */

static uint64_t get_cluster_offset(BlockDriverState *bs,
                                   uint64_t offset, int *num)
{
    BDRVQcowState *s = bs->opaque;
    int l1_index, l2_index;
    uint64_t l2_offset, *l2_table, cluster_offset;
    int l1_bits, c;
    int index_in_cluster, nb_available, nb_needed, nb_clusters;

    index_in_cluster = (offset >> 9) & (s->cluster_sectors - 1);
    nb_needed = *num + index_in_cluster;

    l1_bits = s->l2_bits + s->cluster_bits;

    /* compute how many bytes there are between the offset and
     * the end of the l1 entry
     */

    nb_available = (1 << l1_bits) - (offset & ((1 << l1_bits) - 1));

    /* compute the number of available sectors */

    nb_available = (nb_available >> 9) + index_in_cluster;

    cluster_offset = 0;

    /* seek the the l2 offset in the l1 table */

    l1_index = offset >> l1_bits;
    if (l1_index >= s->l1_size)
        goto out;

    l2_offset = s->l1_table[l1_index];

    /* seek the l2 table of the given l2 offset */

    if (!l2_offset)
        goto out;

    /* load the l2 table in memory */

    l2_offset &= ~QCOW_OFLAG_COPIED;
    l2_table = l2_load(bs, l2_offset);
    if (l2_table == NULL)
        return 0;

    /* find the cluster offset for the given disk offset */

    l2_index = (offset >> s->cluster_bits) & (s->l2_size - 1);
    cluster_offset = be64_to_cpu(l2_table[l2_index]);
    nb_clusters = size_to_clusters(s, nb_needed << 9);

    if (!cluster_offset) {
        /* how many empty clusters ? */
        c = count_contiguous_free_clusters(nb_clusters, &l2_table[l2_index]);
    } else {
        /* how many allocated clusters ? */
        c = count_contiguous_clusters(nb_clusters, s->cluster_size,
                &l2_table[l2_index], 0, QCOW_OFLAG_COPIED);
    }

   nb_available = (c * s->cluster_sectors);
out:
    if (nb_available > nb_needed)
        nb_available = nb_needed;

    *num = nb_available - index_in_cluster;

    return cluster_offset & ~QCOW_OFLAG_COPIED;
}

/*
 * free_any_clusters
 *
 * free clusters according to its type: compressed or not
 *
 */

static void free_any_clusters(BlockDriverState *bs,
                              uint64_t cluster_offset, int nb_clusters)
{
    BDRVQcowState *s = bs->opaque;

    /* free the cluster */

    if (cluster_offset & QCOW_OFLAG_COMPRESSED) {
        int nb_csectors;
        nb_csectors = ((cluster_offset >> s->csize_shift) &
                       s->csize_mask) + 1;
        free_clusters(bs, (cluster_offset & s->cluster_offset_mask) & ~511,
                      nb_csectors * 512);
        return;
    }

    free_clusters(bs, cluster_offset, nb_clusters << s->cluster_bits);

    return;
}

/*
 * get_cluster_table
 *
 * for a given disk offset, load (and allocate if needed)
 * the l2 table.
 *
 * the l2 table offset in the qcow2 file and the cluster index
 * in the l2 table are given to the caller.
 *
 */

static int get_cluster_table(BlockDriverState *bs, uint64_t offset,
                             uint64_t **new_l2_table,
                             uint64_t *new_l2_offset,
                             int *new_l2_index)
{
    BDRVQcowState *s = bs->opaque;
    int l1_index, l2_index, ret;
    uint64_t l2_offset, *l2_table;

    /* seek the the l2 offset in the l1 table */

    l1_index = offset >> (s->l2_bits + s->cluster_bits);
    if (l1_index >= s->l1_size) {
        ret = grow_l1_table(bs, l1_index + 1);
        if (ret < 0)
            return 0;
    }
    l2_offset = s->l1_table[l1_index];

    /* seek the l2 table of the given l2 offset */

    if (l2_offset & QCOW_OFLAG_COPIED) {
        /* load the l2 table in memory */
        l2_offset &= ~QCOW_OFLAG_COPIED;
        l2_table = l2_load(bs, l2_offset);
        if (l2_table == NULL)
            return 0;
    } else {
        if (l2_offset)
            free_clusters(bs, l2_offset, s->l2_size * sizeof(uint64_t));
        l2_table = l2_allocate(bs, l1_index);
        if (l2_table == NULL)
            return 0;
        l2_offset = s->l1_table[l1_index] & ~QCOW_OFLAG_COPIED;
    }

    /* find the cluster offset for the given disk offset */

    l2_index = (offset >> s->cluster_bits) & (s->l2_size - 1);

    *new_l2_table = l2_table;
    *new_l2_offset = l2_offset;
    *new_l2_index = l2_index;

    return 1;
}

/*
 * alloc_compressed_cluster_offset
 *
 * For a given offset of the disk image, return cluster offset in
 * qcow2 file.
 *
 * If the offset is not found, allocate a new compressed cluster.
 *
 * Return the cluster offset if successful,
 * Return 0, otherwise.
 *
 */

static uint64_t alloc_compressed_cluster_offset(BlockDriverState *bs,
                                                uint64_t offset,
                                                int compressed_size)
{
    BDRVQcowState *s = bs->opaque;
    int l2_index, ret;
    uint64_t l2_offset, *l2_table, cluster_offset;
    int nb_csectors;

    ret = get_cluster_table(bs, offset, &l2_table, &l2_offset, &l2_index);
    if (ret == 0)
        return 0;

    cluster_offset = be64_to_cpu(l2_table[l2_index]);
    if (cluster_offset & QCOW_OFLAG_COPIED)
        return cluster_offset & ~QCOW_OFLAG_COPIED;

    if (cluster_offset)
        free_any_clusters(bs, cluster_offset, 1);

    cluster_offset = alloc_bytes(bs, compressed_size);
    nb_csectors = ((cluster_offset + compressed_size - 1) >> 9) -
                  (cluster_offset >> 9);

    cluster_offset |= QCOW_OFLAG_COMPRESSED |
                      ((uint64_t)nb_csectors << s->csize_shift);

    /* update L2 table */

    /* compressed clusters never have the copied flag */

    l2_table[l2_index] = cpu_to_be64(cluster_offset);
    if (bdrv_pwrite(s->hd,
                    l2_offset + l2_index * sizeof(uint64_t),
                    l2_table + l2_index,
                    sizeof(uint64_t)) != sizeof(uint64_t))
        return 0;

    return cluster_offset;
}

typedef struct QCowL2Meta
{
    uint64_t offset;
    int n_start;
    int nb_available;
    int nb_clusters;
} QCowL2Meta;

static int alloc_cluster_link_l2(BlockDriverState *bs, uint64_t cluster_offset,
        QCowL2Meta *m)
{
    BDRVQcowState *s = bs->opaque;
    int i, j = 0, l2_index, ret;
    uint64_t *old_cluster, start_sect, l2_offset, *l2_table;

    if (m->nb_clusters == 0)
        return 0;

    old_cluster = qemu_malloc(m->nb_clusters * sizeof(uint64_t));

    /* copy content of unmodified sectors */
    start_sect = (m->offset & ~(s->cluster_size - 1)) >> 9;
    if (m->n_start) {
        ret = copy_sectors(bs, start_sect, cluster_offset, 0, m->n_start);
        if (ret < 0)
            goto err;
    }

    if (m->nb_available & (s->cluster_sectors - 1)) {
        uint64_t end = m->nb_available & ~(uint64_t)(s->cluster_sectors - 1);
        ret = copy_sectors(bs, start_sect + end, cluster_offset + (end << 9),
                m->nb_available - end, s->cluster_sectors);
        if (ret < 0)
            goto err;
    }

    ret = -EIO;
    /* update L2 table */
    if (!get_cluster_table(bs, m->offset, &l2_table, &l2_offset, &l2_index))
        goto err;

    for (i = 0; i < m->nb_clusters; i++) {
        if(l2_table[l2_index + i] != 0)
            old_cluster[j++] = l2_table[l2_index + i];

        l2_table[l2_index + i] = cpu_to_be64((cluster_offset +
                    (i << s->cluster_bits)) | QCOW_OFLAG_COPIED);
     }

    if (bdrv_pwrite(s->hd, l2_offset + l2_index * sizeof(uint64_t),
                l2_table + l2_index, m->nb_clusters * sizeof(uint64_t)) !=
            m->nb_clusters * sizeof(uint64_t))
        goto err;

    for (i = 0; i < j; i++)
        free_any_clusters(bs, old_cluster[i], 1);

    ret = 0;
err:
    qemu_free(old_cluster);
    return ret;
 }

/*
 * alloc_cluster_offset
 *
 * For a given offset of the disk image, return cluster offset in
 * qcow2 file.
 *
 * If the offset is not found, allocate a new cluster.
 *
 * Return the cluster offset if successful,
 * Return 0, otherwise.
 *
 */

static uint64_t alloc_cluster_offset(BlockDriverState *bs,
                                     uint64_t offset,
                                     int n_start, int n_end,
                                     int *num, QCowL2Meta *m)
{
    BDRVQcowState *s = bs->opaque;
    int l2_index, ret;
    uint64_t l2_offset, *l2_table, cluster_offset;
    int nb_clusters, i = 0;

    ret = get_cluster_table(bs, offset, &l2_table, &l2_offset, &l2_index);
    if (ret == 0)
        return 0;

    nb_clusters = size_to_clusters(s, n_end << 9);

    nb_clusters = MIN(nb_clusters, s->l2_size - l2_index);

    cluster_offset = be64_to_cpu(l2_table[l2_index]);

    /* We keep all QCOW_OFLAG_COPIED clusters */

    if (cluster_offset & QCOW_OFLAG_COPIED) {
        nb_clusters = count_contiguous_clusters(nb_clusters, s->cluster_size,
                &l2_table[l2_index], 0, 0);

        cluster_offset &= ~QCOW_OFLAG_COPIED;
        m->nb_clusters = 0;

        goto out;
    }

    /* for the moment, multiple compressed clusters are not managed */

    if (cluster_offset & QCOW_OFLAG_COMPRESSED)
        nb_clusters = 1;

    /* how many available clusters ? */

    while (i < nb_clusters) {
        i += count_contiguous_clusters(nb_clusters - i, s->cluster_size,
                &l2_table[l2_index], i, 0);

        if(be64_to_cpu(l2_table[l2_index + i]))
            break;

        i += count_contiguous_free_clusters(nb_clusters - i,
                &l2_table[l2_index + i]);

        cluster_offset = be64_to_cpu(l2_table[l2_index + i]);

        if ((cluster_offset & QCOW_OFLAG_COPIED) ||
                (cluster_offset & QCOW_OFLAG_COMPRESSED))
            break;
    }
    nb_clusters = i;

    /* allocate a new cluster */

    cluster_offset = alloc_clusters(bs, nb_clusters * s->cluster_size);

    /* save info needed for meta data update */
    m->offset = offset;
    m->n_start = n_start;
    m->nb_clusters = nb_clusters;

out:
    m->nb_available = MIN(nb_clusters << (s->cluster_bits - 9), n_end);

    *num = m->nb_available - n_start;

    return cluster_offset;
}

static int qcow_is_allocated(BlockDriverState *bs, int64_t sector_num,
                             int nb_sectors, int *pnum)
{
    uint64_t cluster_offset;

    *pnum = nb_sectors;
    cluster_offset = get_cluster_offset(bs, sector_num << 9, pnum);

    return (cluster_offset != 0);
}

static int decompress_buffer(uint8_t *out_buf, int out_buf_size,
                             const uint8_t *buf, int buf_size)
{
    z_stream strm1, *strm = &strm1;
    int ret, out_len;

    memset(strm, 0, sizeof(*strm));

    strm->next_in = (uint8_t *)buf;
    strm->avail_in = buf_size;
    strm->next_out = out_buf;
    strm->avail_out = out_buf_size;

    ret = inflateInit2(strm, -12);
    if (ret != Z_OK)
        return -1;
    ret = inflate(strm, Z_FINISH);
    out_len = strm->next_out - out_buf;
    if ((ret != Z_STREAM_END && ret != Z_BUF_ERROR) ||
        out_len != out_buf_size) {
        inflateEnd(strm);
        return -1;
    }
    inflateEnd(strm);
    return 0;
}

static int decompress_cluster(BDRVQcowState *s, uint64_t cluster_offset)
{
    int ret, csize, nb_csectors, sector_offset;
    uint64_t coffset;

    coffset = cluster_offset & s->cluster_offset_mask;
    if (s->cluster_cache_offset != coffset) {
        nb_csectors = ((cluster_offset >> s->csize_shift) & s->csize_mask) + 1;
        sector_offset = coffset & 511;
        csize = nb_csectors * 512 - sector_offset;
        ret = bdrv_read(s->hd, coffset >> 9, s->cluster_data, nb_csectors);
        if (ret < 0) {
            return -1;
        }
        if (decompress_buffer(s->cluster_cache, s->cluster_size,
                              s->cluster_data + sector_offset, csize) < 0) {
            return -1;
        }
        s->cluster_cache_offset = coffset;
    }
    return 0;
}

/* handle reading after the end of the backing file */
static int backing_read1(BlockDriverState *bs,
                         int64_t sector_num, uint8_t *buf, int nb_sectors)
{
    int n1;
    if ((sector_num + nb_sectors) <= bs->total_sectors)
        return nb_sectors;
    if (sector_num >= bs->total_sectors)
        n1 = 0;
    else
        n1 = bs->total_sectors - sector_num;
    memset(buf + n1 * 512, 0, 512 * (nb_sectors - n1));
    return n1;
}

static int qcow_read(BlockDriverState *bs, int64_t sector_num,
                     uint8_t *buf, int nb_sectors)
{
    BDRVQcowState *s = bs->opaque;
    int ret, index_in_cluster, n, n1;
    uint64_t cluster_offset;

    while (nb_sectors > 0) {
        n = nb_sectors;
        cluster_offset = get_cluster_offset(bs, sector_num << 9, &n);
        index_in_cluster = sector_num & (s->cluster_sectors - 1);
        if (!cluster_offset) {
            if (bs->backing_hd) {
                /* read from the base image */
                n1 = backing_read1(bs->backing_hd, sector_num, buf, n);
                if (n1 > 0) {
                    ret = bdrv_read(bs->backing_hd, sector_num, buf, n1);
                    if (ret < 0)
                        return -1;
                }
            } else {
                memset(buf, 0, 512 * n);
            }
        } else if (cluster_offset & QCOW_OFLAG_COMPRESSED) {
            if (decompress_cluster(s, cluster_offset) < 0)
                return -1;
            memcpy(buf, s->cluster_cache + index_in_cluster * 512, 512 * n);
        } else {
            ret = bdrv_pread(s->hd, cluster_offset + index_in_cluster * 512, buf, n * 512);
            if (ret != n * 512)
                return -1;
            if (s->crypt_method) {
                encrypt_sectors(s, sector_num, buf, buf, n, 0,
                                &s->aes_decrypt_key);
            }
        }
        nb_sectors -= n;
        sector_num += n;
        buf += n * 512;
    }
    return 0;
}

static int qcow_write(BlockDriverState *bs, int64_t sector_num,
                     const uint8_t *buf, int nb_sectors)
{
    BDRVQcowState *s = bs->opaque;
    int ret, index_in_cluster, n;
    uint64_t cluster_offset;
    int n_end;
    QCowL2Meta l2meta;

    while (nb_sectors > 0) {
        index_in_cluster = sector_num & (s->cluster_sectors - 1);
        n_end = index_in_cluster + nb_sectors;
        if (s->crypt_method &&
            n_end > QCOW_MAX_CRYPT_CLUSTERS * s->cluster_sectors)
            n_end = QCOW_MAX_CRYPT_CLUSTERS * s->cluster_sectors;
        cluster_offset = alloc_cluster_offset(bs, sector_num << 9,
                                              index_in_cluster,
                                              n_end, &n, &l2meta);
        if (!cluster_offset)
            return -1;
        if (s->crypt_method) {
            encrypt_sectors(s, sector_num, s->cluster_data, buf, n, 1,
                            &s->aes_encrypt_key);
            ret = bdrv_pwrite(s->hd, cluster_offset + index_in_cluster * 512,
                              s->cluster_data, n * 512);
        } else {
            ret = bdrv_pwrite(s->hd, cluster_offset + index_in_cluster * 512, buf, n * 512);
        }
        if (ret != n * 512 || alloc_cluster_link_l2(bs, cluster_offset, &l2meta) < 0) {
            free_any_clusters(bs, cluster_offset, l2meta.nb_clusters);
            return -1;
        }
        nb_sectors -= n;
        sector_num += n;
        buf += n * 512;
    }
    s->cluster_cache_offset = -1; /* disable compressed cache */
    return 0;
}

typedef struct QCowAIOCB {
    BlockDriverAIOCB common;
    int64_t sector_num;
    uint8_t *buf;
    int nb_sectors;
    int n;
    uint64_t cluster_offset;
    uint8_t *cluster_data;
    BlockDriverAIOCB *hd_aiocb;
    QEMUBH *bh;
    QCowL2Meta l2meta;
} QCowAIOCB;

static void qcow_aio_read_cb(void *opaque, int ret);
static void qcow_aio_read_bh(void *opaque)
{
    QCowAIOCB *acb = opaque;
    qemu_bh_delete(acb->bh);
    acb->bh = NULL;
    qcow_aio_read_cb(opaque, 0);
}

static int qcow_schedule_bh(QEMUBHFunc *cb, QCowAIOCB *acb)
{
    if (acb->bh)
        return -EIO;

    acb->bh = qemu_bh_new(cb, acb);
    if (!acb->bh)
        return -EIO;

    qemu_bh_schedule(acb->bh);

    return 0;
}

static void qcow_aio_read_cb(void *opaque, int ret)
{
    QCowAIOCB *acb = opaque;
    BlockDriverState *bs = acb->common.bs;
    BDRVQcowState *s = bs->opaque;
    int index_in_cluster, n1;

    acb->hd_aiocb = NULL;
    if (ret < 0) {
fail:
        acb->common.cb(acb->common.opaque, ret);
        qemu_aio_release(acb);
        return;
    }

    /* post process the read buffer */
    if (!acb->cluster_offset) {
        /* nothing to do */
    } else if (acb->cluster_offset & QCOW_OFLAG_COMPRESSED) {
        /* nothing to do */
    } else {
        if (s->crypt_method) {
            encrypt_sectors(s, acb->sector_num, acb->buf, acb->buf,
                            acb->n, 0,
                            &s->aes_decrypt_key);
        }
    }

    acb->nb_sectors -= acb->n;
    acb->sector_num += acb->n;
    acb->buf += acb->n * 512;

    if (acb->nb_sectors == 0) {
        /* request completed */
        acb->common.cb(acb->common.opaque, 0);
        qemu_aio_release(acb);
        return;
    }

    /* prepare next AIO request */
    acb->n = acb->nb_sectors;
    acb->cluster_offset = get_cluster_offset(bs, acb->sector_num << 9, &acb->n);
    index_in_cluster = acb->sector_num & (s->cluster_sectors - 1);

    if (!acb->cluster_offset) {
        if (bs->backing_hd) {
            /* read from the base image */
            n1 = backing_read1(bs->backing_hd, acb->sector_num,
                               acb->buf, acb->n);
            if (n1 > 0) {
                acb->hd_aiocb = bdrv_aio_read(bs->backing_hd, acb->sector_num,
                                    acb->buf, acb->n, qcow_aio_read_cb, acb);
                if (acb->hd_aiocb == NULL)
                    goto fail;
            } else {
                ret = qcow_schedule_bh(qcow_aio_read_bh, acb);
                if (ret < 0)
                    goto fail;
            }
        } else {
            /* Note: in this case, no need to wait */
            memset(acb->buf, 0, 512 * acb->n);
            ret = qcow_schedule_bh(qcow_aio_read_bh, acb);
            if (ret < 0)
                goto fail;
        }
    } else if (acb->cluster_offset & QCOW_OFLAG_COMPRESSED) {
        /* add AIO support for compressed blocks ? */
        if (decompress_cluster(s, acb->cluster_offset) < 0)
            goto fail;
        memcpy(acb->buf,
               s->cluster_cache + index_in_cluster * 512, 512 * acb->n);
        ret = qcow_schedule_bh(qcow_aio_read_bh, acb);
        if (ret < 0)
            goto fail;
    } else {
        if ((acb->cluster_offset & 511) != 0) {
            ret = -EIO;
            goto fail;
        }
        acb->hd_aiocb = bdrv_aio_read(s->hd,
                            (acb->cluster_offset >> 9) + index_in_cluster,
                            acb->buf, acb->n, qcow_aio_read_cb, acb);
        if (acb->hd_aiocb == NULL)
            goto fail;
    }
}

static QCowAIOCB *qcow_aio_setup(BlockDriverState *bs,
        int64_t sector_num, uint8_t *buf, int nb_sectors,
        BlockDriverCompletionFunc *cb, void *opaque)
{
    QCowAIOCB *acb;

    acb = qemu_aio_get(bs, cb, opaque);
    if (!acb)
        return NULL;
    acb->hd_aiocb = NULL;
    acb->sector_num = sector_num;
    acb->buf = buf;
    acb->nb_sectors = nb_sectors;
    acb->n = 0;
    acb->cluster_offset = 0;
    acb->l2meta.nb_clusters = 0;
    return acb;
}

static BlockDriverAIOCB *qcow_aio_read(BlockDriverState *bs,
        int64_t sector_num, uint8_t *buf, int nb_sectors,
        BlockDriverCompletionFunc *cb, void *opaque)
{
    QCowAIOCB *acb;

    acb = qcow_aio_setup(bs, sector_num, buf, nb_sectors, cb, opaque);
    if (!acb)
        return NULL;

    qcow_aio_read_cb(acb, 0);
    return &acb->common;
}

static void qcow_aio_write_cb(void *opaque, int ret)
{
    QCowAIOCB *acb = opaque;
    BlockDriverState *bs = acb->common.bs;
    BDRVQcowState *s = bs->opaque;
    int index_in_cluster;
    const uint8_t *src_buf;
    int n_end;

    acb->hd_aiocb = NULL;

    if (ret < 0) {
    fail:
        acb->common.cb(acb->common.opaque, ret);
        qemu_aio_release(acb);
        return;
    }

    if (alloc_cluster_link_l2(bs, acb->cluster_offset, &acb->l2meta) < 0) {
        free_any_clusters(bs, acb->cluster_offset, acb->l2meta.nb_clusters);
        goto fail;
    }

    acb->nb_sectors -= acb->n;
    acb->sector_num += acb->n;
    acb->buf += acb->n * 512;

    if (acb->nb_sectors == 0) {
        /* request completed */
        acb->common.cb(acb->common.opaque, 0);
        qemu_aio_release(acb);
        return;
    }

    index_in_cluster = acb->sector_num & (s->cluster_sectors - 1);
    n_end = index_in_cluster + acb->nb_sectors;
    if (s->crypt_method &&
        n_end > QCOW_MAX_CRYPT_CLUSTERS * s->cluster_sectors)
        n_end = QCOW_MAX_CRYPT_CLUSTERS * s->cluster_sectors;

    acb->cluster_offset = alloc_cluster_offset(bs, acb->sector_num << 9,
                                          index_in_cluster,
                                          n_end, &acb->n, &acb->l2meta);
    if (!acb->cluster_offset || (acb->cluster_offset & 511) != 0) {
        ret = -EIO;
        goto fail;
    }
    if (s->crypt_method) {
        if (!acb->cluster_data) {
            acb->cluster_data = qemu_mallocz(QCOW_MAX_CRYPT_CLUSTERS *
                                             s->cluster_size);
        }
        encrypt_sectors(s, acb->sector_num, acb->cluster_data, acb->buf,
                        acb->n, 1, &s->aes_encrypt_key);
        src_buf = acb->cluster_data;
    } else {
        src_buf = acb->buf;
    }
    acb->hd_aiocb = bdrv_aio_write(s->hd,
                                   (acb->cluster_offset >> 9) + index_in_cluster,
                                   src_buf, acb->n,
                                   qcow_aio_write_cb, acb);
    if (acb->hd_aiocb == NULL)
        goto fail;
}

static BlockDriverAIOCB *qcow_aio_write(BlockDriverState *bs,
        int64_t sector_num, const uint8_t *buf, int nb_sectors,
        BlockDriverCompletionFunc *cb, void *opaque)
{
    BDRVQcowState *s = bs->opaque;
    QCowAIOCB *acb;

    s->cluster_cache_offset = -1; /* disable compressed cache */

    acb = qcow_aio_setup(bs, sector_num, (uint8_t*)buf, nb_sectors, cb, opaque);
    if (!acb)
        return NULL;

    qcow_aio_write_cb(acb, 0);
    return &acb->common;
}

static void qcow_aio_cancel(BlockDriverAIOCB *blockacb)
{
    QCowAIOCB *acb = (QCowAIOCB *)blockacb;
    if (acb->hd_aiocb)
        bdrv_aio_cancel(acb->hd_aiocb);
    qemu_aio_release(acb);
}

static void qcow_close(BlockDriverState *bs)
{
    BDRVQcowState *s = bs->opaque;
    qemu_free(s->l1_table);
    qemu_free(s->l2_cache);
    qemu_free(s->cluster_cache);
    qemu_free(s->cluster_data);
    refcount_close(bs);
    bdrv_delete(s->hd);
}

/* XXX: use std qcow open function ? */
typedef struct QCowCreateState {
    int cluster_size;
    int cluster_bits;
    uint16_t *refcount_block;
    uint64_t *refcount_table;
    int64_t l1_table_offset;
    int64_t refcount_table_offset;
    int64_t refcount_block_offset;
} QCowCreateState;

static void create_refcount_update(QCowCreateState *s,
                                   int64_t offset, int64_t size)
{
    int refcount;
    int64_t start, last, cluster_offset;
    uint16_t *p;

    start = offset & ~(s->cluster_size - 1);
    last = (offset + size - 1)  & ~(s->cluster_size - 1);
    for(cluster_offset = start; cluster_offset <= last;
        cluster_offset += s->cluster_size) {
        p = &s->refcount_block[cluster_offset >> s->cluster_bits];
        refcount = be16_to_cpu(*p);
        refcount++;
        *p = cpu_to_be16(refcount);
    }
}

static int qcow_create(const char *filename, int64_t total_size,
                      const char *backing_file, int flags)
{
    int fd, header_size, backing_filename_len, l1_size, i, shift, l2_bits;
    QCowHeader header;
    uint64_t tmp, offset;
    QCowCreateState s1, *s = &s1;

    memset(s, 0, sizeof(*s));

    fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0644);
    if (fd < 0)
        return -1;
    memset(&header, 0, sizeof(header));
    header.magic = cpu_to_be32(QCOW_MAGIC);
    header.version = cpu_to_be32(QCOW_VERSION);
    header.size = cpu_to_be64(total_size * 512);
    header_size = sizeof(header);
    backing_filename_len = 0;
    if (backing_file) {
        header.backing_file_offset = cpu_to_be64(header_size);
        backing_filename_len = strlen(backing_file);
        header.backing_file_size = cpu_to_be32(backing_filename_len);
        header_size += backing_filename_len;
    }
    s->cluster_bits = 12;  /* 4 KB clusters */
    s->cluster_size = 1 << s->cluster_bits;
    header.cluster_bits = cpu_to_be32(s->cluster_bits);
    header_size = (header_size + 7) & ~7;
    if (flags & BLOCK_FLAG_ENCRYPT) {
        header.crypt_method = cpu_to_be32(QCOW_CRYPT_AES);
    } else {
        header.crypt_method = cpu_to_be32(QCOW_CRYPT_NONE);
    }
    l2_bits = s->cluster_bits - 3;
    shift = s->cluster_bits + l2_bits;
    l1_size = (((total_size * 512) + (1LL << shift) - 1) >> shift);
    offset = align_offset(header_size, s->cluster_size);
    s->l1_table_offset = offset;
    header.l1_table_offset = cpu_to_be64(s->l1_table_offset);
    header.l1_size = cpu_to_be32(l1_size);
    offset += align_offset(l1_size * sizeof(uint64_t), s->cluster_size);

    s->refcount_table = qemu_mallocz(s->cluster_size);
    s->refcount_block = qemu_mallocz(s->cluster_size);

    s->refcount_table_offset = offset;
    header.refcount_table_offset = cpu_to_be64(offset);
    header.refcount_table_clusters = cpu_to_be32(1);
    offset += s->cluster_size;

    s->refcount_table[0] = cpu_to_be64(offset);
    s->refcount_block_offset = offset;
    offset += s->cluster_size;

    /* update refcounts */
    create_refcount_update(s, 0, header_size);
    create_refcount_update(s, s->l1_table_offset, l1_size * sizeof(uint64_t));
    create_refcount_update(s, s->refcount_table_offset, s->cluster_size);
    create_refcount_update(s, s->refcount_block_offset, s->cluster_size);

    /* write all the data */
    write(fd, &header, sizeof(header));
    if (backing_file) {
        write(fd, backing_file, backing_filename_len);
    }
    lseek(fd, s->l1_table_offset, SEEK_SET);
    tmp = 0;
    for(i = 0;i < l1_size; i++) {
        write(fd, &tmp, sizeof(tmp));
    }
    lseek(fd, s->refcount_table_offset, SEEK_SET);
    write(fd, s->refcount_table, s->cluster_size);

    lseek(fd, s->refcount_block_offset, SEEK_SET);
    write(fd, s->refcount_block, s->cluster_size);

    qemu_free(s->refcount_table);
    qemu_free(s->refcount_block);
    close(fd);
    return 0;
}

static int qcow_make_empty(BlockDriverState *bs)
{
#if 0
    /* XXX: not correct */
    BDRVQcowState *s = bs->opaque;
    uint32_t l1_length = s->l1_size * sizeof(uint64_t);
    int ret;

    memset(s->l1_table, 0, l1_length);
    if (bdrv_pwrite(s->hd, s->l1_table_offset, s->l1_table, l1_length) < 0)
        return -1;
    ret = bdrv_truncate(s->hd, s->l1_table_offset + l1_length);
    if (ret < 0)
        return ret;

    l2_cache_reset(bs);
#endif
    return 0;
}

/* XXX: put compressed sectors first, then all the cluster aligned
   tables to avoid losing bytes in alignment */
static int qcow_write_compressed(BlockDriverState *bs, int64_t sector_num,
                                 const uint8_t *buf, int nb_sectors)
{
    BDRVQcowState *s = bs->opaque;
    z_stream strm;
    int ret, out_len;
    uint8_t *out_buf;
    uint64_t cluster_offset;

    if (nb_sectors == 0) {
        /* align end of file to a sector boundary to ease reading with
           sector based I/Os */
        cluster_offset = bdrv_getlength(s->hd);
        cluster_offset = (cluster_offset + 511) & ~511;
        bdrv_truncate(s->hd, cluster_offset);
        return 0;
    }

    if (nb_sectors != s->cluster_sectors)
        return -EINVAL;

    out_buf = qemu_malloc(s->cluster_size + (s->cluster_size / 1000) + 128);

    /* best compression, small window, no zlib header */
    memset(&strm, 0, sizeof(strm));
    ret = deflateInit2(&strm, Z_DEFAULT_COMPRESSION,
                       Z_DEFLATED, -12,
                       9, Z_DEFAULT_STRATEGY);
    if (ret != 0) {
        qemu_free(out_buf);
        return -1;
    }

    strm.avail_in = s->cluster_size;
    strm.next_in = (uint8_t *)buf;
    strm.avail_out = s->cluster_size;
    strm.next_out = out_buf;

    ret = deflate(&strm, Z_FINISH);
    if (ret != Z_STREAM_END && ret != Z_OK) {
        qemu_free(out_buf);
        deflateEnd(&strm);
        return -1;
    }
    out_len = strm.next_out - out_buf;

    deflateEnd(&strm);

    if (ret != Z_STREAM_END || out_len >= s->cluster_size) {
        /* could not compress: write normal cluster */
        qcow_write(bs, sector_num, buf, s->cluster_sectors);
    } else {
        cluster_offset = alloc_compressed_cluster_offset(bs, sector_num << 9,
                                              out_len);
        if (!cluster_offset)
            return -1;
        cluster_offset &= s->cluster_offset_mask;
        if (bdrv_pwrite(s->hd, cluster_offset, out_buf, out_len) != out_len) {
            qemu_free(out_buf);
            return -1;
        }
    }

    qemu_free(out_buf);
    return 0;
}

static void qcow_flush(BlockDriverState *bs)
{
    BDRVQcowState *s = bs->opaque;
    bdrv_flush(s->hd);
}

static int qcow_get_info(BlockDriverState *bs, BlockDriverInfo *bdi)
{
    BDRVQcowState *s = bs->opaque;
    bdi->cluster_size = s->cluster_size;
    bdi->vm_state_offset = (int64_t)s->l1_vm_state_index <<
        (s->cluster_bits + s->l2_bits);
    bdi->highest_alloc = s->highest_alloc << s->cluster_bits;
    bdi->num_free_bytes = s->nc_free  << s->cluster_bits;
    return 0;
}

/*********************************************************/
/* snapshot support */

/* update the refcounts of snapshots and the copied flag */
static int update_snapshot_refcount(BlockDriverState *bs,
                                    int64_t l1_table_offset,
                                    int l1_size,
                                    int addend)
{
    BDRVQcowState *s = bs->opaque;
    uint64_t *l1_table, *l2_table, l2_offset, offset, l1_size2, l1_allocated;
    int64_t old_offset, old_l2_offset;
    int l2_size, i, j, l1_modified, l2_modified, nb_csectors, refcount;

    l2_cache_reset(bs);

    l2_table = NULL;
    l1_table = NULL;
    l1_size2 = l1_size * sizeof(uint64_t);
    l1_allocated = 0;
    if (l1_table_offset != s->l1_table_offset) {
        l1_table = qemu_malloc(l1_size2);
        l1_allocated = 1;
        if (bdrv_pread(s->hd, l1_table_offset,
                       l1_table, l1_size2) != l1_size2)
            goto fail;
        for(i = 0;i < l1_size; i++)
            be64_to_cpus(&l1_table[i]);
    } else {
        assert(l1_size == s->l1_size);
        l1_table = s->l1_table;
        l1_allocated = 0;
    }

    l2_size = s->l2_size * sizeof(uint64_t);
    l2_table = qemu_malloc(l2_size);
    l1_modified = 0;
    for(i = 0; i < l1_size; i++) {
        l2_offset = l1_table[i];
        if (l2_offset) {
            old_l2_offset = l2_offset;
            l2_offset &= ~QCOW_OFLAG_COPIED;
            l2_modified = 0;
            if (bdrv_pread(s->hd, l2_offset, l2_table, l2_size) != l2_size)
                goto fail;
            for(j = 0; j < s->l2_size; j++) {
                offset = be64_to_cpu(l2_table[j]);
                if (offset != 0) {
                    old_offset = offset;
                    offset &= ~QCOW_OFLAG_COPIED;
                    if (offset & QCOW_OFLAG_COMPRESSED) {
                        nb_csectors = ((offset >> s->csize_shift) &
                                       s->csize_mask) + 1;
                        if (addend != 0)
                            update_refcount(bs, (offset & s->cluster_offset_mask) & ~511,
                                            nb_csectors * 512, addend);
                        /* compressed clusters are never modified */
                        refcount = 2;
                    } else {
                        if (addend != 0) {
                            refcount = update_cluster_refcount(bs, offset >> s->cluster_bits, addend);
                        } else {
                            refcount = get_refcount(bs, offset >> s->cluster_bits);
                        }
                    }

                    if (refcount == 1) {
                        offset |= QCOW_OFLAG_COPIED;
                    }
                    if (offset != old_offset) {
                        l2_table[j] = cpu_to_be64(offset);
                        l2_modified = 1;
                    }
                }
            }
            if (l2_modified) {
                if (bdrv_pwrite(s->hd,
                                l2_offset, l2_table, l2_size) != l2_size)
                    goto fail;
            }

            if (addend != 0) {
                refcount = update_cluster_refcount(bs, l2_offset >> s->cluster_bits, addend);
            } else {
                refcount = get_refcount(bs, l2_offset >> s->cluster_bits);
            }
            if (refcount == 1) {
                l2_offset |= QCOW_OFLAG_COPIED;
            }
            if (l2_offset != old_l2_offset) {
                l1_table[i] = l2_offset;
                l1_modified = 1;
            }
        }
    }
    if (l1_modified) {
        for(i = 0; i < l1_size; i++)
            cpu_to_be64s(&l1_table[i]);
        if (bdrv_pwrite(s->hd, l1_table_offset, l1_table,
                        l1_size2) != l1_size2)
            goto fail;
        for(i = 0; i < l1_size; i++)
            be64_to_cpus(&l1_table[i]);
    }
    if (l1_allocated)
        qemu_free(l1_table);
    qemu_free(l2_table);
    return 0;
 fail:
    if (l1_allocated)
        qemu_free(l1_table);
    qemu_free(l2_table);
    return -EIO;
}

static void qcow_free_snapshots(BlockDriverState *bs)
{
    BDRVQcowState *s = bs->opaque;
    int i;

    for(i = 0; i < s->nb_snapshots; i++) {
        qemu_free(s->snapshots[i].name);
        qemu_free(s->snapshots[i].id_str);
    }
    qemu_free(s->snapshots);
    s->snapshots = NULL;
    s->nb_snapshots = 0;
}

static int qcow_read_snapshots(BlockDriverState *bs)
{
    BDRVQcowState *s = bs->opaque;
    QCowSnapshotHeader h;
    QCowSnapshot *sn;
    int i, id_str_size, name_size;
    int64_t offset;
    uint32_t extra_data_size;

    if (!s->nb_snapshots) {
        s->snapshots = NULL;
        s->snapshots_size = 0;
        return 0;
    }

    offset = s->snapshots_offset;
    s->snapshots = qemu_mallocz(s->nb_snapshots * sizeof(QCowSnapshot));
    for(i = 0; i < s->nb_snapshots; i++) {
        offset = align_offset(offset, 8);
        if (bdrv_pread(s->hd, offset, &h, sizeof(h)) != sizeof(h))
            goto fail;
        offset += sizeof(h);
        sn = s->snapshots + i;
        sn->l1_table_offset = be64_to_cpu(h.l1_table_offset);
        sn->l1_size = be32_to_cpu(h.l1_size);
        sn->vm_state_size = be32_to_cpu(h.vm_state_size);
        sn->date_sec = be32_to_cpu(h.date_sec);
        sn->date_nsec = be32_to_cpu(h.date_nsec);
        sn->vm_clock_nsec = be64_to_cpu(h.vm_clock_nsec);
        extra_data_size = be32_to_cpu(h.extra_data_size);

        id_str_size = be16_to_cpu(h.id_str_size);
        name_size = be16_to_cpu(h.name_size);

        offset += extra_data_size;

        sn->id_str = qemu_malloc(id_str_size + 1);
        if (bdrv_pread(s->hd, offset, sn->id_str, id_str_size) != id_str_size)
            goto fail;
        offset += id_str_size;
        sn->id_str[id_str_size] = '\0';

        sn->name = qemu_malloc(name_size + 1);
        if (bdrv_pread(s->hd, offset, sn->name, name_size) != name_size)
            goto fail;
        offset += name_size;
        sn->name[name_size] = '\0';
    }
    s->snapshots_size = offset - s->snapshots_offset;
    return 0;
 fail:
    qcow_free_snapshots(bs);
    return -1;
}

/* add at the end of the file a new list of snapshots */
static int qcow_write_snapshots(BlockDriverState *bs)
{
    BDRVQcowState *s = bs->opaque;
    QCowSnapshot *sn;
    QCowSnapshotHeader h;
    int i, name_size, id_str_size, snapshots_size;
    uint64_t data64;
    uint32_t data32;
    int64_t offset, snapshots_offset;

    /* compute the size of the snapshots */
    offset = 0;
    for(i = 0; i < s->nb_snapshots; i++) {
        sn = s->snapshots + i;
        offset = align_offset(offset, 8);
        offset += sizeof(h);
        offset += strlen(sn->id_str);
        offset += strlen(sn->name);
    }
    snapshots_size = offset;

    snapshots_offset = alloc_clusters(bs, snapshots_size);
    offset = snapshots_offset;

    for(i = 0; i < s->nb_snapshots; i++) {
        sn = s->snapshots + i;
        memset(&h, 0, sizeof(h));
        h.l1_table_offset = cpu_to_be64(sn->l1_table_offset);
        h.l1_size = cpu_to_be32(sn->l1_size);
        h.vm_state_size = cpu_to_be32(sn->vm_state_size);
        h.date_sec = cpu_to_be32(sn->date_sec);
        h.date_nsec = cpu_to_be32(sn->date_nsec);
        h.vm_clock_nsec = cpu_to_be64(sn->vm_clock_nsec);

        id_str_size = strlen(sn->id_str);
        name_size = strlen(sn->name);
        h.id_str_size = cpu_to_be16(id_str_size);
        h.name_size = cpu_to_be16(name_size);
        offset = align_offset(offset, 8);
        if (bdrv_pwrite(s->hd, offset, &h, sizeof(h)) != sizeof(h))
            goto fail;
        offset += sizeof(h);
        if (bdrv_pwrite(s->hd, offset, sn->id_str, id_str_size) != id_str_size)
            goto fail;
        offset += id_str_size;
        if (bdrv_pwrite(s->hd, offset, sn->name, name_size) != name_size)
            goto fail;
        offset += name_size;
    }

    /* update the various header fields */
    data64 = cpu_to_be64(snapshots_offset);
    if (bdrv_pwrite(s->hd, offsetof(QCowHeader, snapshots_offset),
                    &data64, sizeof(data64)) != sizeof(data64))
        goto fail;
    data32 = cpu_to_be32(s->nb_snapshots);
    if (bdrv_pwrite(s->hd, offsetof(QCowHeader, nb_snapshots),
                    &data32, sizeof(data32)) != sizeof(data32))
        goto fail;

    /* free the old snapshot table */
    free_clusters(bs, s->snapshots_offset, s->snapshots_size);
    s->snapshots_offset = snapshots_offset;
    s->snapshots_size = snapshots_size;
    return 0;
 fail:
    return -1;
}

static void find_new_snapshot_id(BlockDriverState *bs,
                                 char *id_str, int id_str_size)
{
    BDRVQcowState *s = bs->opaque;
    QCowSnapshot *sn;
    int i, id, id_max = 0;

    for(i = 0; i < s->nb_snapshots; i++) {
        sn = s->snapshots + i;
        id = strtoul(sn->id_str, NULL, 10);
        if (id > id_max)
            id_max = id;
    }
    snprintf(id_str, id_str_size, "%d", id_max + 1);
}

static int find_snapshot_by_id(BlockDriverState *bs, const char *id_str)
{
    BDRVQcowState *s = bs->opaque;
    int i;

    for(i = 0; i < s->nb_snapshots; i++) {
        if (!strcmp(s->snapshots[i].id_str, id_str))
            return i;
    }
    return -1;
}

static int find_snapshot_by_id_or_name(BlockDriverState *bs, const char *name)
{
    BDRVQcowState *s = bs->opaque;
    int i, ret;

    ret = find_snapshot_by_id(bs, name);
    if (ret >= 0)
        return ret;
    for(i = 0; i < s->nb_snapshots; i++) {
        if (!strcmp(s->snapshots[i].name, name))
            return i;
    }
    return -1;
}

/* if no id is provided, a new one is constructed */
static int qcow_snapshot_create(BlockDriverState *bs,
                                QEMUSnapshotInfo *sn_info)
{
    BDRVQcowState *s = bs->opaque;
    QCowSnapshot *snapshots1, sn1, *sn = &sn1;
    int i, ret;
    uint64_t *l1_table = NULL;

    memset(sn, 0, sizeof(*sn));

    if (sn_info->id_str[0] == '\0') {
        /* compute a new id */
        find_new_snapshot_id(bs, sn_info->id_str, sizeof(sn_info->id_str));
    }

    /* check that the ID is unique */
    if (find_snapshot_by_id(bs, sn_info->id_str) >= 0)
        return -ENOENT;

    sn->id_str = qemu_strdup(sn_info->id_str);
    if (!sn->id_str)
        goto fail;
    sn->name = qemu_strdup(sn_info->name);
    if (!sn->name)
        goto fail;
    sn->vm_state_size = sn_info->vm_state_size;
    sn->date_sec = sn_info->date_sec;
    sn->date_nsec = sn_info->date_nsec;
    sn->vm_clock_nsec = sn_info->vm_clock_nsec;

    ret = update_snapshot_refcount(bs, s->l1_table_offset, s->l1_size, 1);
    if (ret < 0)
        goto fail;

    /* create the L1 table of the snapshot */
    sn->l1_table_offset = alloc_clusters(bs, s->l1_size * sizeof(uint64_t));
    sn->l1_size = s->l1_size;

    l1_table = qemu_malloc(s->l1_size * sizeof(uint64_t));
    for(i = 0; i < s->l1_size; i++) {
        l1_table[i] = cpu_to_be64(s->l1_table[i]);
    }
    if (bdrv_pwrite(s->hd, sn->l1_table_offset,
                    l1_table, s->l1_size * sizeof(uint64_t)) !=
        (s->l1_size * sizeof(uint64_t)))
        goto fail;
    qemu_free(l1_table);
    l1_table = NULL;

    snapshots1 = qemu_malloc((s->nb_snapshots + 1) * sizeof(QCowSnapshot));
    if (s->snapshots) {
        memcpy(snapshots1, s->snapshots, s->nb_snapshots * sizeof(QCowSnapshot));
        qemu_free(s->snapshots);
    }
    s->snapshots = snapshots1;
    s->snapshots[s->nb_snapshots++] = *sn;

    if (qcow_write_snapshots(bs) < 0)
        goto fail;
#ifdef DEBUG_ALLOC
    check_refcounts(bs);
#endif
    return 0;
 fail:
    qemu_free(sn->name);
    qemu_free(l1_table);
    return -1;
}

/* copy the snapshot 'snapshot_name' into the current disk image */
static int qcow_snapshot_goto(BlockDriverState *bs,
                              const char *snapshot_id)
{
    BDRVQcowState *s = bs->opaque;
    QCowSnapshot *sn;
    int i, snapshot_index, l1_size2;

    snapshot_index = find_snapshot_by_id_or_name(bs, snapshot_id);
    if (snapshot_index < 0)
        return -ENOENT;
    sn = &s->snapshots[snapshot_index];

    if (update_snapshot_refcount(bs, s->l1_table_offset, s->l1_size, -1) < 0)
        goto fail;

    if (grow_l1_table(bs, sn->l1_size) < 0)
        goto fail;

    s->l1_size = sn->l1_size;
    l1_size2 = s->l1_size * sizeof(uint64_t);
    /* copy the snapshot l1 table to the current l1 table */
    if (bdrv_pread(s->hd, sn->l1_table_offset,
                   s->l1_table, l1_size2) != l1_size2)
        goto fail;
    if (bdrv_pwrite(s->hd, s->l1_table_offset,
                    s->l1_table, l1_size2) != l1_size2)
        goto fail;
    for(i = 0;i < s->l1_size; i++) {
        be64_to_cpus(&s->l1_table[i]);
    }

    if (update_snapshot_refcount(bs, s->l1_table_offset, s->l1_size, 1) < 0)
        goto fail;

#ifdef DEBUG_ALLOC
    check_refcounts(bs);
#endif
    return 0;
 fail:
    return -EIO;
}

static int qcow_snapshot_delete(BlockDriverState *bs, const char *snapshot_id)
{
    BDRVQcowState *s = bs->opaque;
    QCowSnapshot *sn;
    int snapshot_index, ret;

    snapshot_index = find_snapshot_by_id_or_name(bs, snapshot_id);
    if (snapshot_index < 0)
        return -ENOENT;
    sn = &s->snapshots[snapshot_index];

    ret = update_snapshot_refcount(bs, sn->l1_table_offset, sn->l1_size, -1);
    if (ret < 0)
        return ret;
    /* must update the copied flag on the current cluster offsets */
    ret = update_snapshot_refcount(bs, s->l1_table_offset, s->l1_size, 0);
    if (ret < 0)
        return ret;
    free_clusters(bs, sn->l1_table_offset, sn->l1_size * sizeof(uint64_t));

    qemu_free(sn->id_str);
    qemu_free(sn->name);
    memmove(sn, sn + 1, (s->nb_snapshots - snapshot_index - 1) * sizeof(*sn));
    s->nb_snapshots--;
    ret = qcow_write_snapshots(bs);
    if (ret < 0) {
        /* XXX: restore snapshot if error ? */
        return ret;
    }
#ifdef DEBUG_ALLOC
    check_refcounts(bs);
#endif
    return 0;
}

static int qcow_snapshot_list(BlockDriverState *bs,
                              QEMUSnapshotInfo **psn_tab)
{
    BDRVQcowState *s = bs->opaque;
    QEMUSnapshotInfo *sn_tab, *sn_info;
    QCowSnapshot *sn;
    int i;

    sn_tab = qemu_mallocz(s->nb_snapshots * sizeof(QEMUSnapshotInfo));
    for(i = 0; i < s->nb_snapshots; i++) {
        sn_info = sn_tab + i;
        sn = s->snapshots + i;
        pstrcpy(sn_info->id_str, sizeof(sn_info->id_str),
                sn->id_str);
        pstrcpy(sn_info->name, sizeof(sn_info->name),
                sn->name);
        sn_info->vm_state_size = sn->vm_state_size;
        sn_info->date_sec = sn->date_sec;
        sn_info->date_nsec = sn->date_nsec;
        sn_info->vm_clock_nsec = sn->vm_clock_nsec;
    }
    *psn_tab = sn_tab;
    return s->nb_snapshots;
}

/*********************************************************/
/* refcount handling */

static int refcount_init(BlockDriverState *bs)
{
    BDRVQcowState *s = bs->opaque;
    int ret, refcount_table_size2, i;

    s->refcount_block_cache = qemu_malloc(s->cluster_size);
    refcount_table_size2 = s->refcount_table_size * sizeof(uint64_t);
    s->refcount_table = qemu_malloc(refcount_table_size2);
    if (s->refcount_table_size > 0) {
        ret = bdrv_pread(s->hd, s->refcount_table_offset,
                         s->refcount_table, refcount_table_size2);
        if (ret != refcount_table_size2)
            goto fail;
        for(i = 0; i < s->refcount_table_size; i++)
            be64_to_cpus(&s->refcount_table[i]);
    }
    return 0;
 fail:
    return -ENOMEM;
}

static void refcount_close(BlockDriverState *bs)
{
    BDRVQcowState *s = bs->opaque;
    qemu_free(s->refcount_block_cache);
    qemu_free(s->refcount_table);
}


static int load_refcount_block(BlockDriverState *bs,
                               int64_t refcount_block_offset)
{
    BDRVQcowState *s = bs->opaque;
    int ret;
    ret = bdrv_pread(s->hd, refcount_block_offset, s->refcount_block_cache,
                     s->cluster_size);
    if (ret != s->cluster_size)
        return -EIO;
    s->refcount_block_cache_offset = refcount_block_offset;
    return 0;
}

static void scan_refcount(BlockDriverState *bs, int64_t *high, int64_t *free)
{
    BDRVQcowState *s = bs->opaque;
    int64_t refcnt_index, cluster_index, cluster_end, h = 0, f = 0;
    int64_t tail = 0; /* do not count last consecutive free entries */

    for (refcnt_index=0; refcnt_index < s->refcount_table_size; refcnt_index++){
        if (s->refcount_table[refcnt_index] == 0) {
            f += 1 << (s->cluster_bits - REFCOUNT_SHIFT);
            tail += 1 << (s->cluster_bits - REFCOUNT_SHIFT);
            continue;
        }
        cluster_index = refcnt_index << (s->cluster_bits - REFCOUNT_SHIFT);
        cluster_end = (refcnt_index + 1) << (s->cluster_bits - REFCOUNT_SHIFT);
        for ( ; cluster_index < cluster_end; cluster_index++) {
            if (get_refcount(bs, cluster_index) == 0) {
                f++;
                tail++;
            }
            else {
                h = cluster_index;
                tail = 0;
            }
        }
    }

    f -= tail;
    if (free)
        *free = f;
    if (high)
        *high = (h+1);
}

static int get_refcount(BlockDriverState *bs, int64_t cluster_index)
{
    BDRVQcowState *s = bs->opaque;
    int refcount_table_index, block_index;
    int64_t refcount_block_offset;

    refcount_table_index = cluster_index >> (s->cluster_bits - REFCOUNT_SHIFT);
    if (refcount_table_index >= s->refcount_table_size)
        return 0;
    refcount_block_offset = s->refcount_table[refcount_table_index];
    if (!refcount_block_offset)
        return 0;
    if (refcount_block_offset != s->refcount_block_cache_offset) {
        /* better than nothing: return allocated if read error */
        if (load_refcount_block(bs, refcount_block_offset) < 0)
            return 1;
    }
    block_index = cluster_index &
        ((1 << (s->cluster_bits - REFCOUNT_SHIFT)) - 1);
    return be16_to_cpu(s->refcount_block_cache[block_index]);
}

/* return < 0 if error */
static int64_t alloc_clusters_noref(BlockDriverState *bs, int64_t size)
{
    BDRVQcowState *s = bs->opaque;
    int i, nb_clusters;

    nb_clusters = size_to_clusters(s, size);
retry:
    for(i = 0; i < nb_clusters; i++) {
        int64_t i = s->free_cluster_index++;
        if (get_refcount(bs, i) != 0)
            goto retry;
    }
#ifdef DEBUG_ALLOC2
    printf("alloc_clusters: size=%lld -> %lld\n",
            size,
            (s->free_cluster_index - nb_clusters) << s->cluster_bits);
#endif

    if (s->highest_alloc < s->free_cluster_index) {
        s->nc_free += (s->free_cluster_index - s->highest_alloc);
        s->highest_alloc = s->free_cluster_index;
    }

    return (s->free_cluster_index - nb_clusters) << s->cluster_bits;
}

static int64_t alloc_clusters(BlockDriverState *bs, int64_t size)
{
    int64_t offset;

    offset = alloc_clusters_noref(bs, size);
    update_refcount(bs, offset, size, 1);
    return offset;
}

/* only used to allocate compressed sectors. We try to allocate
   contiguous sectors. size must be <= cluster_size */
static int64_t alloc_bytes(BlockDriverState *bs, int size)
{
    BDRVQcowState *s = bs->opaque;
    int64_t offset, cluster_offset;
    int free_in_cluster;

    assert(size > 0 && size <= s->cluster_size);
    if (s->free_byte_offset == 0) {
        s->free_byte_offset = alloc_clusters(bs, s->cluster_size);
    }
 redo:
    free_in_cluster = s->cluster_size -
        (s->free_byte_offset & (s->cluster_size - 1));
    if (size <= free_in_cluster) {
        /* enough space in current cluster */
        offset = s->free_byte_offset;
        s->free_byte_offset += size;
        free_in_cluster -= size;
        if (free_in_cluster == 0)
            s->free_byte_offset = 0;
        if ((offset & (s->cluster_size - 1)) != 0)
            update_cluster_refcount(bs, offset >> s->cluster_bits, 1);
    } else {
        offset = alloc_clusters(bs, s->cluster_size);
        cluster_offset = s->free_byte_offset & ~(s->cluster_size - 1);
        if ((cluster_offset + s->cluster_size) == offset) {
            /* we are lucky: contiguous data */
            offset = s->free_byte_offset;
            update_cluster_refcount(bs, offset >> s->cluster_bits, 1);
            s->free_byte_offset += size;
        } else {
            s->free_byte_offset = offset;
            goto redo;
        }
    }
    return offset;
}

static void free_clusters(BlockDriverState *bs,
                          int64_t offset, int64_t size)
{
    update_refcount(bs, offset, size, -1);
}

static int grow_refcount_table(BlockDriverState *bs, int min_size)
{
    BDRVQcowState *s = bs->opaque;
    int new_table_size, new_table_size2, refcount_table_clusters, i, ret;
    uint64_t *new_table;
    int64_t table_offset;
    uint8_t data[12];
    int old_table_size;
    int64_t old_table_offset;

    if (min_size <= s->refcount_table_size)
        return 0;
    /* compute new table size */
    refcount_table_clusters = s->refcount_table_size >> (s->cluster_bits - 3);
    for(;;) {
        if (refcount_table_clusters == 0) {
            refcount_table_clusters = 1;
        } else {
            refcount_table_clusters = (refcount_table_clusters * 3 + 1) / 2;
        }
        new_table_size = refcount_table_clusters << (s->cluster_bits - 3);
        if (min_size <= new_table_size)
            break;
    }
#ifdef DEBUG_ALLOC2
    printf("grow_refcount_table from %d to %d\n",
           s->refcount_table_size,
           new_table_size);
#endif
    new_table_size2 = new_table_size * sizeof(uint64_t);
    new_table = qemu_mallocz(new_table_size2);
    memcpy(new_table, s->refcount_table,
           s->refcount_table_size * sizeof(uint64_t));
    for(i = 0; i < s->refcount_table_size; i++)
        cpu_to_be64s(&new_table[i]);
    /* Note: we cannot update the refcount now to avoid recursion */
    table_offset = alloc_clusters_noref(bs, new_table_size2);
    ret = bdrv_pwrite(s->hd, table_offset, new_table, new_table_size2);
    if (ret != new_table_size2)
        goto fail;
    for(i = 0; i < s->refcount_table_size; i++)
        be64_to_cpus(&new_table[i]);

    cpu_to_be64w((uint64_t*)data, table_offset);
    cpu_to_be32w((uint32_t*)(data + 8), refcount_table_clusters);
    if (bdrv_pwrite(s->hd, offsetof(QCowHeader, refcount_table_offset),
                    data, sizeof(data)) != sizeof(data))
        goto fail;
    qemu_free(s->refcount_table);
    old_table_offset = s->refcount_table_offset;
    old_table_size = s->refcount_table_size;
    s->refcount_table = new_table;
    s->refcount_table_size = new_table_size;
    s->refcount_table_offset = table_offset;

    update_refcount(bs, table_offset, new_table_size2, 1);
    free_clusters(bs, old_table_offset, old_table_size * sizeof(uint64_t));
    return 0;
 fail:
    free_clusters(bs, table_offset, new_table_size2);
    qemu_free(new_table);
    return -EIO;
}

/* addend must be 1 or -1 */
/* XXX: cache several refcount block clusters ? */
static int update_cluster_refcount(BlockDriverState *bs,
                                   int64_t cluster_index,
                                   int addend)
{
    BDRVQcowState *s = bs->opaque;
    int64_t offset, refcount_block_offset;
    int ret, refcount_table_index, block_index, refcount;
    uint64_t data64;

    refcount_table_index = cluster_index >> (s->cluster_bits - REFCOUNT_SHIFT);
    if (refcount_table_index >= s->refcount_table_size) {
        if (addend < 0)
            return -EINVAL;
        ret = grow_refcount_table(bs, refcount_table_index + 1);
        if (ret < 0)
            return ret;
    }
    refcount_block_offset = s->refcount_table[refcount_table_index];
    if (!refcount_block_offset) {
        if (addend < 0)
            return -EINVAL;
        /* create a new refcount block */
        /* Note: we cannot update the refcount now to avoid recursion */
        offset = alloc_clusters_noref(bs, s->cluster_size);
        memset(s->refcount_block_cache, 0, s->cluster_size);
        ret = bdrv_pwrite(s->hd, offset, s->refcount_block_cache, s->cluster_size);
        if (ret != s->cluster_size)
            return -EINVAL;
        s->refcount_table[refcount_table_index] = offset;
        data64 = cpu_to_be64(offset);
        ret = bdrv_pwrite(s->hd, s->refcount_table_offset +
                          refcount_table_index * sizeof(uint64_t),
                          &data64, sizeof(data64));
        if (ret != sizeof(data64))
            return -EINVAL;

        refcount_block_offset = offset;
        s->refcount_block_cache_offset = offset;
        update_refcount(bs, offset, s->cluster_size, 1);
    } else {
        if (refcount_block_offset != s->refcount_block_cache_offset) {
            if (load_refcount_block(bs, refcount_block_offset) < 0)
                return -EIO;
        }
    }
    /* we can update the count and save it */
    block_index = cluster_index &
        ((1 << (s->cluster_bits - REFCOUNT_SHIFT)) - 1);
    refcount = be16_to_cpu(s->refcount_block_cache[block_index]);

    if (refcount == 1 && addend == -1)
        s->nc_free += 1;
    else if (refcount == 0 && addend == 1)
        s->nc_free -= 1;

    refcount += addend;
    if (refcount < 0 || refcount > 0xffff)
        return -EINVAL;
    if (refcount == 0 && cluster_index < s->free_cluster_index) {
        s->free_cluster_index = cluster_index;
    }
    s->refcount_block_cache[block_index] = cpu_to_be16(refcount);
    if (bdrv_pwrite(s->hd,
                    refcount_block_offset + (block_index << REFCOUNT_SHIFT),
                    &s->refcount_block_cache[block_index], 2) != 2)
        return -EIO;
    return refcount;
}

static void update_refcount(BlockDriverState *bs,
                            int64_t offset, int64_t length,
                            int addend)
{
    BDRVQcowState *s = bs->opaque;
    int64_t start, last, cluster_offset;

#ifdef DEBUG_ALLOC2
    printf("update_refcount: offset=%lld size=%lld addend=%d\n",
           offset, length, addend);
#endif
    if (length <= 0)
        return;
    start = offset & ~(s->cluster_size - 1);
    last = (offset + length - 1) & ~(s->cluster_size - 1);
    for(cluster_offset = start; cluster_offset <= last;
        cluster_offset += s->cluster_size) {
        update_cluster_refcount(bs, cluster_offset >> s->cluster_bits, addend);
    }
}

#ifdef DEBUG_ALLOC
static void inc_refcounts(BlockDriverState *bs,
                          uint16_t *refcount_table,
                          int refcount_table_size,
                          int64_t offset, int64_t size)
{
    BDRVQcowState *s = bs->opaque;
    int64_t start, last, cluster_offset;
    int k;

    if (size <= 0)
        return;

    start = offset & ~(s->cluster_size - 1);
    last = (offset + size - 1) & ~(s->cluster_size - 1);
    for(cluster_offset = start; cluster_offset <= last;
        cluster_offset += s->cluster_size) {
        k = cluster_offset >> s->cluster_bits;
        if (k < 0 || k >= refcount_table_size) {
            printf("ERROR: invalid cluster offset=0x%llx\n", cluster_offset);
        } else {
            if (++refcount_table[k] == 0) {
                printf("ERROR: overflow cluster offset=0x%llx\n", cluster_offset);
            }
        }
    }
}

static int check_refcounts_l1(BlockDriverState *bs,
                              uint16_t *refcount_table,
                              int refcount_table_size,
                              int64_t l1_table_offset, int l1_size,
                              int check_copied)
{
    BDRVQcowState *s = bs->opaque;
    uint64_t *l1_table, *l2_table, l2_offset, offset, l1_size2;
    int l2_size, i, j, nb_csectors, refcount;

    l2_table = NULL;
    l1_size2 = l1_size * sizeof(uint64_t);

    inc_refcounts(bs, refcount_table, refcount_table_size,
                  l1_table_offset, l1_size2);

    l1_table = qemu_malloc(l1_size2);
    if (bdrv_pread(s->hd, l1_table_offset,
                   l1_table, l1_size2) != l1_size2)
        goto fail;
    for(i = 0;i < l1_size; i++)
        be64_to_cpus(&l1_table[i]);

    l2_size = s->l2_size * sizeof(uint64_t);
    l2_table = qemu_malloc(l2_size);
    for(i = 0; i < l1_size; i++) {
        l2_offset = l1_table[i];
        if (l2_offset) {
            if (check_copied) {
                refcount = get_refcount(bs, (l2_offset & ~QCOW_OFLAG_COPIED) >> s->cluster_bits);
                if ((refcount == 1) != ((l2_offset & QCOW_OFLAG_COPIED) != 0)) {
                    printf("ERROR OFLAG_COPIED: l2_offset=%llx refcount=%d\n",
                           l2_offset, refcount);
                }
            }
            l2_offset &= ~QCOW_OFLAG_COPIED;
            if (bdrv_pread(s->hd, l2_offset, l2_table, l2_size) != l2_size)
                goto fail;
            for(j = 0; j < s->l2_size; j++) {
                offset = be64_to_cpu(l2_table[j]);
                if (offset != 0) {
                    if (offset & QCOW_OFLAG_COMPRESSED) {
                        if (offset & QCOW_OFLAG_COPIED) {
                            printf("ERROR: cluster %lld: copied flag must never be set for compressed clusters\n",
                                   offset >> s->cluster_bits);
                            offset &= ~QCOW_OFLAG_COPIED;
                        }
                        nb_csectors = ((offset >> s->csize_shift) &
                                       s->csize_mask) + 1;
                        offset &= s->cluster_offset_mask;
                        inc_refcounts(bs, refcount_table,
                                      refcount_table_size,
                                      offset & ~511, nb_csectors * 512);
                    } else {
                        if (check_copied) {
                            refcount = get_refcount(bs, (offset & ~QCOW_OFLAG_COPIED) >> s->cluster_bits);
                            if ((refcount == 1) != ((offset & QCOW_OFLAG_COPIED) != 0)) {
                                printf("ERROR OFLAG_COPIED: offset=%llx refcount=%d\n",
                                       offset, refcount);
                            }
                        }
                        offset &= ~QCOW_OFLAG_COPIED;
                        inc_refcounts(bs, refcount_table,
                                      refcount_table_size,
                                      offset, s->cluster_size);
                    }
                }
            }
            inc_refcounts(bs, refcount_table,
                          refcount_table_size,
                          l2_offset,
                          s->cluster_size);
        }
    }
    qemu_free(l1_table);
    qemu_free(l2_table);
    return 0;
 fail:
    printf("ERROR: I/O error in check_refcounts_l1\n");
    qemu_free(l1_table);
    qemu_free(l2_table);
    return -EIO;
}

static void check_refcounts(BlockDriverState *bs)
{
    BDRVQcowState *s = bs->opaque;
    int64_t size;
    int nb_clusters, refcount1, refcount2, i;
    QCowSnapshot *sn;
    uint16_t *refcount_table;

    size = bdrv_getlength(s->hd);
    nb_clusters = size_to_clusters(s, size);
    refcount_table = qemu_mallocz(nb_clusters * sizeof(uint16_t));

    /* header */
    inc_refcounts(bs, refcount_table, nb_clusters,
                  0, s->cluster_size);

    check_refcounts_l1(bs, refcount_table, nb_clusters,
                       s->l1_table_offset, s->l1_size, 1);

    /* snapshots */
    for(i = 0; i < s->nb_snapshots; i++) {
        sn = s->snapshots + i;
        check_refcounts_l1(bs, refcount_table, nb_clusters,
                           sn->l1_table_offset, sn->l1_size, 0);
    }
    inc_refcounts(bs, refcount_table, nb_clusters,
                  s->snapshots_offset, s->snapshots_size);

    /* refcount data */
    inc_refcounts(bs, refcount_table, nb_clusters,
                  s->refcount_table_offset,
                  s->refcount_table_size * sizeof(uint64_t));
    for(i = 0; i < s->refcount_table_size; i++) {
        int64_t offset;
        offset = s->refcount_table[i];
        if (offset != 0) {
            inc_refcounts(bs, refcount_table, nb_clusters,
                          offset, s->cluster_size);
        }
    }

    /* compare ref counts */
    for(i = 0; i < nb_clusters; i++) {
        refcount1 = get_refcount(bs, i);
        refcount2 = refcount_table[i];
        if (refcount1 != refcount2)
            printf("ERROR cluster %d refcount=%d reference=%d\n",
                   i, refcount1, refcount2);
    }

    qemu_free(refcount_table);
}

#if 0
static void dump_refcounts(BlockDriverState *bs)
{
    BDRVQcowState *s = bs->opaque;
    int64_t nb_clusters, k, k1, size;
    int refcount;

    size = bdrv_getlength(s->hd);
    nb_clusters = size_to_clusters(s, size);
    for(k = 0; k < nb_clusters;) {
        k1 = k;
        refcount = get_refcount(bs, k);
        k++;
        while (k < nb_clusters && get_refcount(bs, k) == refcount)
            k++;
        printf("%lld: refcount=%d nb=%lld\n", k, refcount, k - k1);
    }
}
#endif
#endif

BlockDriver bdrv_qcow2 = {
    "qcow2",
    sizeof(BDRVQcowState),
    qcow_probe,
    qcow_open,
    NULL,
    NULL,
    qcow_close,
    qcow_create,
    qcow_flush,
    qcow_is_allocated,
    qcow_set_key,
    qcow_make_empty,

    .bdrv_aio_read = qcow_aio_read,
    .bdrv_aio_write = qcow_aio_write,
    .bdrv_aio_cancel = qcow_aio_cancel,
    .aiocb_size = sizeof(QCowAIOCB),
    .bdrv_write_compressed = qcow_write_compressed,

    .bdrv_snapshot_create = qcow_snapshot_create,
    .bdrv_snapshot_goto = qcow_snapshot_goto,
    .bdrv_snapshot_delete = qcow_snapshot_delete,
    .bdrv_snapshot_list = qcow_snapshot_list,
    .bdrv_get_info = qcow_get_info,
};
