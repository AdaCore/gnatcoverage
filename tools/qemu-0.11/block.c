/*
 * QEMU System Emulator block driver
 *
 * Copyright (c) 2003 Fabrice Bellard
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
#include "config-host.h"
#ifdef HOST_BSD
/* include native header before sys-queue.h */
#include <sys/queue.h>
#endif

#include "qemu-common.h"
#include "monitor.h"
#include "block_int.h"
#include "module.h"

#ifdef HOST_BSD
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#ifndef __DragonFly__
#include <sys/disk.h>
#endif
#endif

#ifdef _WIN32
#include <windows.h>
#endif

#define SECTOR_BITS 9
#define SECTOR_SIZE (1 << SECTOR_BITS)

static BlockDriverAIOCB *bdrv_aio_readv_em(BlockDriverState *bs,
        int64_t sector_num, QEMUIOVector *qiov, int nb_sectors,
        BlockDriverCompletionFunc *cb, void *opaque);
static BlockDriverAIOCB *bdrv_aio_writev_em(BlockDriverState *bs,
        int64_t sector_num, QEMUIOVector *qiov, int nb_sectors,
        BlockDriverCompletionFunc *cb, void *opaque);
static int bdrv_read_em(BlockDriverState *bs, int64_t sector_num,
                        uint8_t *buf, int nb_sectors);
static int bdrv_write_em(BlockDriverState *bs, int64_t sector_num,
                         const uint8_t *buf, int nb_sectors);

BlockDriverState *bdrv_first;

static BlockDriver *first_drv;

int path_is_absolute(const char *path)
{
    const char *p;
#ifdef _WIN32
    /* specific case for names like: "\\.\d:" */
    if (*path == '/' || *path == '\\')
        return 1;
#endif
    p = strchr(path, ':');
    if (p)
        p++;
    else
        p = path;
#ifdef _WIN32
    return (*p == '/' || *p == '\\');
#else
    return (*p == '/');
#endif
}

/* if filename is absolute, just copy it to dest. Otherwise, build a
   path to it by considering it is relative to base_path. URL are
   supported. */
void path_combine(char *dest, int dest_size,
                  const char *base_path,
                  const char *filename)
{
    const char *p, *p1;
    int len;

    if (dest_size <= 0)
        return;
    if (path_is_absolute(filename)) {
        pstrcpy(dest, dest_size, filename);
    } else {
        p = strchr(base_path, ':');
        if (p)
            p++;
        else
            p = base_path;
        p1 = strrchr(base_path, '/');
#ifdef _WIN32
        {
            const char *p2;
            p2 = strrchr(base_path, '\\');
            if (!p1 || p2 > p1)
                p1 = p2;
        }
#endif
        if (p1)
            p1++;
        else
            p1 = base_path;
        if (p1 > p)
            p = p1;
        len = p - base_path;
        if (len > dest_size - 1)
            len = dest_size - 1;
        memcpy(dest, base_path, len);
        dest[len] = '\0';
        pstrcat(dest, dest_size, filename);
    }
}

void bdrv_register(BlockDriver *bdrv)
{
    if (!bdrv->bdrv_aio_readv) {
        /* add AIO emulation layer */
        bdrv->bdrv_aio_readv = bdrv_aio_readv_em;
        bdrv->bdrv_aio_writev = bdrv_aio_writev_em;
    } else if (!bdrv->bdrv_read) {
        /* add synchronous IO emulation layer */
        bdrv->bdrv_read = bdrv_read_em;
        bdrv->bdrv_write = bdrv_write_em;
    }
    bdrv->next = first_drv;
    first_drv = bdrv;
}

/* create a new block device (by default it is empty) */
BlockDriverState *bdrv_new(const char *device_name)
{
    BlockDriverState **pbs, *bs;

    bs = qemu_mallocz(sizeof(BlockDriverState));
    pstrcpy(bs->device_name, sizeof(bs->device_name), device_name);
    if (device_name[0] != '\0') {
        /* insert at the end */
        pbs = &bdrv_first;
        while (*pbs != NULL)
            pbs = &(*pbs)->next;
        *pbs = bs;
    }
    return bs;
}

BlockDriver *bdrv_find_format(const char *format_name)
{
    BlockDriver *drv1;
    for(drv1 = first_drv; drv1 != NULL; drv1 = drv1->next) {
        if (!strcmp(drv1->format_name, format_name))
            return drv1;
    }
    return NULL;
}

int bdrv_create(BlockDriver *drv, const char* filename,
    QEMUOptionParameter *options)
{
    if (!drv->bdrv_create)
        return -ENOTSUP;

    return drv->bdrv_create(filename, options);
}

#ifdef _WIN32
void get_tmp_filename(char *filename, int size)
{
    char temp_dir[MAX_PATH];

    GetTempPath(MAX_PATH, temp_dir);
    GetTempFileName(temp_dir, "qem", 0, filename);
}
#else
void get_tmp_filename(char *filename, int size)
{
    int fd;
    const char *tmpdir;
    /* XXX: race condition possible */
    tmpdir = getenv("TMPDIR");
    if (!tmpdir)
        tmpdir = "/tmp";
    snprintf(filename, size, "%s/vl.XXXXXX", tmpdir);
    fd = mkstemp(filename);
    close(fd);
}
#endif

#ifdef _WIN32
static int is_windows_drive_prefix(const char *filename)
{
    return (((filename[0] >= 'a' && filename[0] <= 'z') ||
             (filename[0] >= 'A' && filename[0] <= 'Z')) &&
            filename[1] == ':');
}

int is_windows_drive(const char *filename)
{
    if (is_windows_drive_prefix(filename) &&
        filename[2] == '\0')
        return 1;
    if (strstart(filename, "\\\\.\\", NULL) ||
        strstart(filename, "//./", NULL))
        return 1;
    return 0;
}
#endif

static BlockDriver *find_protocol(const char *filename)
{
    BlockDriver *drv1;
    char protocol[128];
    int len;
    const char *p;

#ifdef _WIN32
    if (is_windows_drive(filename) ||
        is_windows_drive_prefix(filename))
        return bdrv_find_format("raw");
#endif
    p = strchr(filename, ':');
    if (!p)
        return bdrv_find_format("raw");
    len = p - filename;
    if (len > sizeof(protocol) - 1)
        len = sizeof(protocol) - 1;
    memcpy(protocol, filename, len);
    protocol[len] = '\0';
    for(drv1 = first_drv; drv1 != NULL; drv1 = drv1->next) {
        if (drv1->protocol_name &&
            !strcmp(drv1->protocol_name, protocol))
            return drv1;
    }
    return NULL;
}

/*
 * Detect host devices. By convention, /dev/cdrom[N] is always
 * recognized as a host CDROM.
 */
static BlockDriver *find_hdev_driver(const char *filename)
{
    int score_max = 0, score;
    BlockDriver *drv = NULL, *d;

    for (d = first_drv; d; d = d->next) {
        if (d->bdrv_probe_device) {
            score = d->bdrv_probe_device(filename);
            if (score > score_max) {
                score_max = score;
                drv = d;
            }
        }
    }

    return drv;
}

static BlockDriver *find_image_format(const char *filename)
{
    int ret, score, score_max;
    BlockDriver *drv1, *drv;
    uint8_t buf[2048];
    BlockDriverState *bs;

    drv = find_protocol(filename);
    /* no need to test disk image formats for vvfat */
    if (drv && strcmp(drv->format_name, "vvfat") == 0)
        return drv;

    ret = bdrv_file_open(&bs, filename, BDRV_O_RDONLY);
    if (ret < 0)
        return NULL;
    ret = bdrv_pread(bs, 0, buf, sizeof(buf));
    bdrv_delete(bs);
    if (ret < 0) {
        return NULL;
    }

    score_max = 0;
    for(drv1 = first_drv; drv1 != NULL; drv1 = drv1->next) {
        if (drv1->bdrv_probe) {
            score = drv1->bdrv_probe(buf, ret, filename);
            if (score > score_max) {
                score_max = score;
                drv = drv1;
            }
        }
    }
    return drv;
}

int bdrv_file_open(BlockDriverState **pbs, const char *filename, int flags)
{
    BlockDriverState *bs;
    int ret;

    bs = bdrv_new("");
    ret = bdrv_open2(bs, filename, flags | BDRV_O_FILE, NULL);
    if (ret < 0) {
        bdrv_delete(bs);
        return ret;
    }
    bs->growable = 1;
    *pbs = bs;
    return 0;
}

int bdrv_open(BlockDriverState *bs, const char *filename, int flags)
{
    return bdrv_open2(bs, filename, flags, NULL);
}

int bdrv_open2(BlockDriverState *bs, const char *filename, int flags,
               BlockDriver *drv)
{
    int ret, open_flags;
    char tmp_filename[PATH_MAX];
    char backing_filename[PATH_MAX];

    bs->read_only = 0;
    bs->is_temporary = 0;
    bs->encrypted = 0;
    bs->valid_key = 0;
    /* buffer_alignment defaulted to 512, drivers can change this value */
    bs->buffer_alignment = 512;

    if (flags & BDRV_O_SNAPSHOT) {
        BlockDriverState *bs1;
        int64_t total_size;
        int is_protocol = 0;
        BlockDriver *bdrv_qcow2;
        QEMUOptionParameter *options;

        /* if snapshot, we create a temporary backing file and open it
           instead of opening 'filename' directly */

        /* if there is a backing file, use it */
        bs1 = bdrv_new("");
        ret = bdrv_open2(bs1, filename, 0, drv);
        if (ret < 0) {
            bdrv_delete(bs1);
            return ret;
        }
        total_size = bdrv_getlength(bs1) >> SECTOR_BITS;

        if (bs1->drv && bs1->drv->protocol_name)
            is_protocol = 1;

        bdrv_delete(bs1);

        get_tmp_filename(tmp_filename, sizeof(tmp_filename));

        /* Real path is meaningless for protocols */
        if (is_protocol)
            snprintf(backing_filename, sizeof(backing_filename),
                     "%s", filename);
        else
            realpath(filename, backing_filename);

        bdrv_qcow2 = bdrv_find_format("qcow2");
        options = parse_option_parameters("", bdrv_qcow2->create_options, NULL);

        set_option_parameter_int(options, BLOCK_OPT_SIZE, total_size * 512);
        set_option_parameter(options, BLOCK_OPT_BACKING_FILE, backing_filename);
        if (drv) {
            set_option_parameter(options, BLOCK_OPT_BACKING_FMT,
                drv->format_name);
        }

        ret = bdrv_create(bdrv_qcow2, tmp_filename, options);
        if (ret < 0) {
            return ret;
        }

        filename = tmp_filename;
        drv = bdrv_qcow2;
        bs->is_temporary = 1;
    }

    pstrcpy(bs->filename, sizeof(bs->filename), filename);
    if (flags & BDRV_O_FILE) {
        drv = find_protocol(filename);
    } else if (!drv) {
        drv = find_hdev_driver(filename);
        if (!drv) {
            drv = find_image_format(filename);
        }
    }
    if (!drv) {
        ret = -ENOENT;
        goto unlink_and_fail;
    }
    bs->drv = drv;
    bs->opaque = qemu_mallocz(drv->instance_size);
    /* Note: for compatibility, we open disk image files as RDWR, and
       RDONLY as fallback */
    if (!(flags & BDRV_O_FILE))
        open_flags = BDRV_O_RDWR | (flags & BDRV_O_CACHE_MASK);
    else
        open_flags = flags & ~(BDRV_O_FILE | BDRV_O_SNAPSHOT);
    ret = drv->bdrv_open(bs, filename, open_flags);
    if ((ret == -EACCES || ret == -EPERM) && !(flags & BDRV_O_FILE)) {
        ret = drv->bdrv_open(bs, filename, open_flags & ~BDRV_O_RDWR);
        bs->read_only = 1;
    }
    if (ret < 0) {
        qemu_free(bs->opaque);
        bs->opaque = NULL;
        bs->drv = NULL;
    unlink_and_fail:
        if (bs->is_temporary)
            unlink(filename);
        return ret;
    }
    if (drv->bdrv_getlength) {
        bs->total_sectors = bdrv_getlength(bs) >> SECTOR_BITS;
    }
#ifndef _WIN32
    if (bs->is_temporary) {
        unlink(filename);
    }
#endif
    if (bs->backing_file[0] != '\0') {
        /* if there is a backing file, use it */
        BlockDriver *back_drv = NULL;
        bs->backing_hd = bdrv_new("");
        path_combine(backing_filename, sizeof(backing_filename),
                     filename, bs->backing_file);
        if (bs->backing_format[0] != '\0')
            back_drv = bdrv_find_format(bs->backing_format);
        ret = bdrv_open2(bs->backing_hd, backing_filename, open_flags,
                         back_drv);
        if (ret < 0) {
            bdrv_close(bs);
            return ret;
        }
    }

    if (!bdrv_key_required(bs)) {
        /* call the change callback */
        bs->media_changed = 1;
        if (bs->change_cb)
            bs->change_cb(bs->change_opaque);
    }
    return 0;
}

void bdrv_close(BlockDriverState *bs)
{
    if (bs->drv) {
        if (bs->backing_hd)
            bdrv_delete(bs->backing_hd);
        bs->drv->bdrv_close(bs);
        qemu_free(bs->opaque);
#ifdef _WIN32
        if (bs->is_temporary) {
            unlink(bs->filename);
        }
#endif
        bs->opaque = NULL;
        bs->drv = NULL;

        /* call the change callback */
        bs->media_changed = 1;
        if (bs->change_cb)
            bs->change_cb(bs->change_opaque);
    }
}

void bdrv_delete(BlockDriverState *bs)
{
    BlockDriverState **pbs;

    pbs = &bdrv_first;
    while (*pbs != bs && *pbs != NULL)
        pbs = &(*pbs)->next;
    if (*pbs == bs)
        *pbs = bs->next;

    bdrv_close(bs);
    qemu_free(bs);
}

/*
 * Run consistency checks on an image
 *
 * Returns the number of errors or -errno when an internal error occurs
 */
int bdrv_check(BlockDriverState *bs)
{
    if (bs->drv->bdrv_check == NULL) {
        return -ENOTSUP;
    }

    return bs->drv->bdrv_check(bs);
}

/* commit COW file into the raw image */
int bdrv_commit(BlockDriverState *bs)
{
    BlockDriver *drv = bs->drv;
    int64_t i, total_sectors;
    int n, j;
    unsigned char sector[512];

    if (!drv)
        return -ENOMEDIUM;

    if (bs->read_only) {
	return -EACCES;
    }

    if (!bs->backing_hd) {
	return -ENOTSUP;
    }

    total_sectors = bdrv_getlength(bs) >> SECTOR_BITS;
    for (i = 0; i < total_sectors;) {
        if (drv->bdrv_is_allocated(bs, i, 65536, &n)) {
            for(j = 0; j < n; j++) {
                if (bdrv_read(bs, i, sector, 1) != 0) {
                    return -EIO;
                }

                if (bdrv_write(bs->backing_hd, i, sector, 1) != 0) {
                    return -EIO;
                }
                i++;
	    }
	} else {
            i += n;
        }
    }

    if (drv->bdrv_make_empty)
	return drv->bdrv_make_empty(bs);

    return 0;
}

static int bdrv_check_byte_request(BlockDriverState *bs, int64_t offset,
                                   size_t size)
{
    int64_t len;

    if (!bdrv_is_inserted(bs))
        return -ENOMEDIUM;

    if (bs->growable)
        return 0;

    len = bdrv_getlength(bs);

    if (offset < 0)
        return -EIO;

    if ((offset > len) || (len - offset < size))
        return -EIO;

    return 0;
}

static int bdrv_check_request(BlockDriverState *bs, int64_t sector_num,
                              int nb_sectors)
{
    return bdrv_check_byte_request(bs, sector_num * 512, nb_sectors * 512);
}

/* return < 0 if error. See bdrv_write() for the return codes */
int bdrv_read(BlockDriverState *bs, int64_t sector_num,
              uint8_t *buf, int nb_sectors)
{
    BlockDriver *drv = bs->drv;

    if (!drv)
        return -ENOMEDIUM;
    if (bdrv_check_request(bs, sector_num, nb_sectors))
        return -EIO;

    return drv->bdrv_read(bs, sector_num, buf, nb_sectors);
}

/* Return < 0 if error. Important errors are:
  -EIO         generic I/O error (may happen for all errors)
  -ENOMEDIUM   No media inserted.
  -EINVAL      Invalid sector number or nb_sectors
  -EACCES      Trying to write a read-only device
*/
int bdrv_write(BlockDriverState *bs, int64_t sector_num,
               const uint8_t *buf, int nb_sectors)
{
    BlockDriver *drv = bs->drv;
    if (!bs->drv)
        return -ENOMEDIUM;
    if (bs->read_only)
        return -EACCES;
    if (bdrv_check_request(bs, sector_num, nb_sectors))
        return -EIO;

    return drv->bdrv_write(bs, sector_num, buf, nb_sectors);
}

int bdrv_pread(BlockDriverState *bs, int64_t offset,
               void *buf, int count1)
{
    uint8_t tmp_buf[SECTOR_SIZE];
    int len, nb_sectors, count;
    int64_t sector_num;

    count = count1;
    /* first read to align to sector start */
    len = (SECTOR_SIZE - offset) & (SECTOR_SIZE - 1);
    if (len > count)
        len = count;
    sector_num = offset >> SECTOR_BITS;
    if (len > 0) {
        if (bdrv_read(bs, sector_num, tmp_buf, 1) < 0)
            return -EIO;
        memcpy(buf, tmp_buf + (offset & (SECTOR_SIZE - 1)), len);
        count -= len;
        if (count == 0)
            return count1;
        sector_num++;
        buf += len;
    }

    /* read the sectors "in place" */
    nb_sectors = count >> SECTOR_BITS;
    if (nb_sectors > 0) {
        if (bdrv_read(bs, sector_num, buf, nb_sectors) < 0)
            return -EIO;
        sector_num += nb_sectors;
        len = nb_sectors << SECTOR_BITS;
        buf += len;
        count -= len;
    }

    /* add data from the last sector */
    if (count > 0) {
        if (bdrv_read(bs, sector_num, tmp_buf, 1) < 0)
            return -EIO;
        memcpy(buf, tmp_buf, count);
    }
    return count1;
}

int bdrv_pwrite(BlockDriverState *bs, int64_t offset,
                const void *buf, int count1)
{
    uint8_t tmp_buf[SECTOR_SIZE];
    int len, nb_sectors, count;
    int64_t sector_num;

    count = count1;
    /* first write to align to sector start */
    len = (SECTOR_SIZE - offset) & (SECTOR_SIZE - 1);
    if (len > count)
        len = count;
    sector_num = offset >> SECTOR_BITS;
    if (len > 0) {
        if (bdrv_read(bs, sector_num, tmp_buf, 1) < 0)
            return -EIO;
        memcpy(tmp_buf + (offset & (SECTOR_SIZE - 1)), buf, len);
        if (bdrv_write(bs, sector_num, tmp_buf, 1) < 0)
            return -EIO;
        count -= len;
        if (count == 0)
            return count1;
        sector_num++;
        buf += len;
    }

    /* write the sectors "in place" */
    nb_sectors = count >> SECTOR_BITS;
    if (nb_sectors > 0) {
        if (bdrv_write(bs, sector_num, buf, nb_sectors) < 0)
            return -EIO;
        sector_num += nb_sectors;
        len = nb_sectors << SECTOR_BITS;
        buf += len;
        count -= len;
    }

    /* add data from the last sector */
    if (count > 0) {
        if (bdrv_read(bs, sector_num, tmp_buf, 1) < 0)
            return -EIO;
        memcpy(tmp_buf, buf, count);
        if (bdrv_write(bs, sector_num, tmp_buf, 1) < 0)
            return -EIO;
    }
    return count1;
}

/**
 * Truncate file to 'offset' bytes (needed only for file protocols)
 */
int bdrv_truncate(BlockDriverState *bs, int64_t offset)
{
    BlockDriver *drv = bs->drv;
    if (!drv)
        return -ENOMEDIUM;
    if (!drv->bdrv_truncate)
        return -ENOTSUP;
    return drv->bdrv_truncate(bs, offset);
}

/**
 * Length of a file in bytes. Return < 0 if error or unknown.
 */
int64_t bdrv_getlength(BlockDriverState *bs)
{
    BlockDriver *drv = bs->drv;
    if (!drv)
        return -ENOMEDIUM;
    if (!drv->bdrv_getlength) {
        /* legacy mode */
        return bs->total_sectors * SECTOR_SIZE;
    }
    return drv->bdrv_getlength(bs);
}

/* return 0 as number of sectors if no device present or error */
void bdrv_get_geometry(BlockDriverState *bs, uint64_t *nb_sectors_ptr)
{
    int64_t length;
    length = bdrv_getlength(bs);
    if (length < 0)
        length = 0;
    else
        length = length >> SECTOR_BITS;
    *nb_sectors_ptr = length;
}

struct partition {
        uint8_t boot_ind;           /* 0x80 - active */
        uint8_t head;               /* starting head */
        uint8_t sector;             /* starting sector */
        uint8_t cyl;                /* starting cylinder */
        uint8_t sys_ind;            /* What partition type */
        uint8_t end_head;           /* end head */
        uint8_t end_sector;         /* end sector */
        uint8_t end_cyl;            /* end cylinder */
        uint32_t start_sect;        /* starting sector counting from 0 */
        uint32_t nr_sects;          /* nr of sectors in partition */
} __attribute__((packed));

/* try to guess the disk logical geometry from the MSDOS partition table. Return 0 if OK, -1 if could not guess */
static int guess_disk_lchs(BlockDriverState *bs,
                           int *pcylinders, int *pheads, int *psectors)
{
    uint8_t buf[512];
    int ret, i, heads, sectors, cylinders;
    struct partition *p;
    uint32_t nr_sects;
    uint64_t nb_sectors;

    bdrv_get_geometry(bs, &nb_sectors);

    ret = bdrv_read(bs, 0, buf, 1);
    if (ret < 0)
        return -1;
    /* test msdos magic */
    if (buf[510] != 0x55 || buf[511] != 0xaa)
        return -1;
    for(i = 0; i < 4; i++) {
        p = ((struct partition *)(buf + 0x1be)) + i;
        nr_sects = le32_to_cpu(p->nr_sects);
        if (nr_sects && p->end_head) {
            /* We make the assumption that the partition terminates on
               a cylinder boundary */
            heads = p->end_head + 1;
            sectors = p->end_sector & 63;
            if (sectors == 0)
                continue;
            cylinders = nb_sectors / (heads * sectors);
            if (cylinders < 1 || cylinders > 16383)
                continue;
            *pheads = heads;
            *psectors = sectors;
            *pcylinders = cylinders;
#if 0
            printf("guessed geometry: LCHS=%d %d %d\n",
                   cylinders, heads, sectors);
#endif
            return 0;
        }
    }
    return -1;
}

void bdrv_guess_geometry(BlockDriverState *bs, int *pcyls, int *pheads, int *psecs)
{
    int translation, lba_detected = 0;
    int cylinders, heads, secs;
    uint64_t nb_sectors;

    /* if a geometry hint is available, use it */
    bdrv_get_geometry(bs, &nb_sectors);
    bdrv_get_geometry_hint(bs, &cylinders, &heads, &secs);
    translation = bdrv_get_translation_hint(bs);
    if (cylinders != 0) {
        *pcyls = cylinders;
        *pheads = heads;
        *psecs = secs;
    } else {
        if (guess_disk_lchs(bs, &cylinders, &heads, &secs) == 0) {
            if (heads > 16) {
                /* if heads > 16, it means that a BIOS LBA
                   translation was active, so the default
                   hardware geometry is OK */
                lba_detected = 1;
                goto default_geometry;
            } else {
                *pcyls = cylinders;
                *pheads = heads;
                *psecs = secs;
                /* disable any translation to be in sync with
                   the logical geometry */
                if (translation == BIOS_ATA_TRANSLATION_AUTO) {
                    bdrv_set_translation_hint(bs,
                                              BIOS_ATA_TRANSLATION_NONE);
                }
            }
        } else {
        default_geometry:
            /* if no geometry, use a standard physical disk geometry */
            cylinders = nb_sectors / (16 * 63);

            if (cylinders > 16383)
                cylinders = 16383;
            else if (cylinders < 2)
                cylinders = 2;
            *pcyls = cylinders;
            *pheads = 16;
            *psecs = 63;
            if ((lba_detected == 1) && (translation == BIOS_ATA_TRANSLATION_AUTO)) {
                if ((*pcyls * *pheads) <= 131072) {
                    bdrv_set_translation_hint(bs,
                                              BIOS_ATA_TRANSLATION_LARGE);
                } else {
                    bdrv_set_translation_hint(bs,
                                              BIOS_ATA_TRANSLATION_LBA);
                }
            }
        }
        bdrv_set_geometry_hint(bs, *pcyls, *pheads, *psecs);
    }
}

void bdrv_set_geometry_hint(BlockDriverState *bs,
                            int cyls, int heads, int secs)
{
    bs->cyls = cyls;
    bs->heads = heads;
    bs->secs = secs;
}

void bdrv_set_type_hint(BlockDriverState *bs, int type)
{
    bs->type = type;
    bs->removable = ((type == BDRV_TYPE_CDROM ||
                      type == BDRV_TYPE_FLOPPY));
}

void bdrv_set_translation_hint(BlockDriverState *bs, int translation)
{
    bs->translation = translation;
}

void bdrv_get_geometry_hint(BlockDriverState *bs,
                            int *pcyls, int *pheads, int *psecs)
{
    *pcyls = bs->cyls;
    *pheads = bs->heads;
    *psecs = bs->secs;
}

int bdrv_get_type_hint(BlockDriverState *bs)
{
    return bs->type;
}

int bdrv_get_translation_hint(BlockDriverState *bs)
{
    return bs->translation;
}

int bdrv_is_removable(BlockDriverState *bs)
{
    return bs->removable;
}

int bdrv_is_read_only(BlockDriverState *bs)
{
    return bs->read_only;
}

int bdrv_is_sg(BlockDriverState *bs)
{
    return bs->sg;
}

/* XXX: no longer used */
void bdrv_set_change_cb(BlockDriverState *bs,
                        void (*change_cb)(void *opaque), void *opaque)
{
    bs->change_cb = change_cb;
    bs->change_opaque = opaque;
}

int bdrv_is_encrypted(BlockDriverState *bs)
{
    if (bs->backing_hd && bs->backing_hd->encrypted)
        return 1;
    return bs->encrypted;
}

int bdrv_key_required(BlockDriverState *bs)
{
    BlockDriverState *backing_hd = bs->backing_hd;

    if (backing_hd && backing_hd->encrypted && !backing_hd->valid_key)
        return 1;
    return (bs->encrypted && !bs->valid_key);
}

int bdrv_set_key(BlockDriverState *bs, const char *key)
{
    int ret;
    if (bs->backing_hd && bs->backing_hd->encrypted) {
        ret = bdrv_set_key(bs->backing_hd, key);
        if (ret < 0)
            return ret;
        if (!bs->encrypted)
            return 0;
    }
    if (!bs->encrypted || !bs->drv || !bs->drv->bdrv_set_key)
        return -1;
    ret = bs->drv->bdrv_set_key(bs, key);
    if (ret < 0) {
        bs->valid_key = 0;
    } else if (!bs->valid_key) {
        bs->valid_key = 1;
        /* call the change callback now, we skipped it on open */
        bs->media_changed = 1;
        if (bs->change_cb)
            bs->change_cb(bs->change_opaque);
    }
    return ret;
}

void bdrv_get_format(BlockDriverState *bs, char *buf, int buf_size)
{
    if (!bs->drv) {
        buf[0] = '\0';
    } else {
        pstrcpy(buf, buf_size, bs->drv->format_name);
    }
}

void bdrv_iterate_format(void (*it)(void *opaque, const char *name),
                         void *opaque)
{
    BlockDriver *drv;

    for (drv = first_drv; drv != NULL; drv = drv->next) {
        it(opaque, drv->format_name);
    }
}

BlockDriverState *bdrv_find(const char *name)
{
    BlockDriverState *bs;

    for (bs = bdrv_first; bs != NULL; bs = bs->next) {
        if (!strcmp(name, bs->device_name))
            return bs;
    }
    return NULL;
}

void bdrv_iterate(void (*it)(void *opaque, BlockDriverState *bs), void *opaque)
{
    BlockDriverState *bs;

    for (bs = bdrv_first; bs != NULL; bs = bs->next) {
        it(opaque, bs);
    }
}

const char *bdrv_get_device_name(BlockDriverState *bs)
{
    return bs->device_name;
}

void bdrv_flush(BlockDriverState *bs)
{
    if (!bs->drv)
        return;
    if (bs->drv->bdrv_flush)
        bs->drv->bdrv_flush(bs);
    if (bs->backing_hd)
        bdrv_flush(bs->backing_hd);
}

void bdrv_flush_all(void)
{
    BlockDriverState *bs;

    for (bs = bdrv_first; bs != NULL; bs = bs->next)
        if (bs->drv && !bdrv_is_read_only(bs) && 
            (!bdrv_is_removable(bs) || bdrv_is_inserted(bs)))
            bdrv_flush(bs);
}

/*
 * Returns true iff the specified sector is present in the disk image. Drivers
 * not implementing the functionality are assumed to not support backing files,
 * hence all their sectors are reported as allocated.
 *
 * 'pnum' is set to the number of sectors (including and immediately following
 * the specified sector) that are known to be in the same
 * allocated/unallocated state.
 *
 * 'nb_sectors' is the max value 'pnum' should be set to.
 */
int bdrv_is_allocated(BlockDriverState *bs, int64_t sector_num, int nb_sectors,
	int *pnum)
{
    int64_t n;
    if (!bs->drv->bdrv_is_allocated) {
        if (sector_num >= bs->total_sectors) {
            *pnum = 0;
            return 0;
        }
        n = bs->total_sectors - sector_num;
        *pnum = (n < nb_sectors) ? (n) : (nb_sectors);
        return 1;
    }
    return bs->drv->bdrv_is_allocated(bs, sector_num, nb_sectors, pnum);
}

void bdrv_info(Monitor *mon)
{
    BlockDriverState *bs;

    for (bs = bdrv_first; bs != NULL; bs = bs->next) {
        monitor_printf(mon, "%s:", bs->device_name);
        monitor_printf(mon, " type=");
        switch(bs->type) {
        case BDRV_TYPE_HD:
            monitor_printf(mon, "hd");
            break;
        case BDRV_TYPE_CDROM:
            monitor_printf(mon, "cdrom");
            break;
        case BDRV_TYPE_FLOPPY:
            monitor_printf(mon, "floppy");
            break;
        }
        monitor_printf(mon, " removable=%d", bs->removable);
        if (bs->removable) {
            monitor_printf(mon, " locked=%d", bs->locked);
        }
        if (bs->drv) {
            monitor_printf(mon, " file=");
            monitor_print_filename(mon, bs->filename);
            if (bs->backing_file[0] != '\0') {
                monitor_printf(mon, " backing_file=");
                monitor_print_filename(mon, bs->backing_file);
            }
            monitor_printf(mon, " ro=%d", bs->read_only);
            monitor_printf(mon, " drv=%s", bs->drv->format_name);
            monitor_printf(mon, " encrypted=%d", bdrv_is_encrypted(bs));
        } else {
            monitor_printf(mon, " [not inserted]");
        }
        monitor_printf(mon, "\n");
    }
}

/* The "info blockstats" command. */
void bdrv_info_stats(Monitor *mon)
{
    BlockDriverState *bs;

    for (bs = bdrv_first; bs != NULL; bs = bs->next) {
        monitor_printf(mon, "%s:"
                       " rd_bytes=%" PRIu64
                       " wr_bytes=%" PRIu64
                       " rd_operations=%" PRIu64
                       " wr_operations=%" PRIu64
                       "\n",
                       bs->device_name,
                       bs->rd_bytes, bs->wr_bytes,
                       bs->rd_ops, bs->wr_ops);
    }
}

const char *bdrv_get_encrypted_filename(BlockDriverState *bs)
{
    if (bs->backing_hd && bs->backing_hd->encrypted)
        return bs->backing_file;
    else if (bs->encrypted)
        return bs->filename;
    else
        return NULL;
}

void bdrv_get_backing_filename(BlockDriverState *bs,
                               char *filename, int filename_size)
{
    if (!bs->backing_hd) {
        pstrcpy(filename, filename_size, "");
    } else {
        pstrcpy(filename, filename_size, bs->backing_file);
    }
}

int bdrv_write_compressed(BlockDriverState *bs, int64_t sector_num,
                          const uint8_t *buf, int nb_sectors)
{
    BlockDriver *drv = bs->drv;
    if (!drv)
        return -ENOMEDIUM;
    if (!drv->bdrv_write_compressed)
        return -ENOTSUP;
    if (bdrv_check_request(bs, sector_num, nb_sectors))
        return -EIO;
    return drv->bdrv_write_compressed(bs, sector_num, buf, nb_sectors);
}

int bdrv_get_info(BlockDriverState *bs, BlockDriverInfo *bdi)
{
    BlockDriver *drv = bs->drv;
    if (!drv)
        return -ENOMEDIUM;
    if (!drv->bdrv_get_info)
        return -ENOTSUP;
    memset(bdi, 0, sizeof(*bdi));
    return drv->bdrv_get_info(bs, bdi);
}

int bdrv_save_vmstate(BlockDriverState *bs, const uint8_t *buf,
                      int64_t pos, int size)
{
    BlockDriver *drv = bs->drv;
    if (!drv)
        return -ENOMEDIUM;
    if (!drv->bdrv_save_vmstate)
        return -ENOTSUP;
    return drv->bdrv_save_vmstate(bs, buf, pos, size);
}

int bdrv_load_vmstate(BlockDriverState *bs, uint8_t *buf,
                      int64_t pos, int size)
{
    BlockDriver *drv = bs->drv;
    if (!drv)
        return -ENOMEDIUM;
    if (!drv->bdrv_load_vmstate)
        return -ENOTSUP;
    return drv->bdrv_load_vmstate(bs, buf, pos, size);
}

/**************************************************************/
/* handling of snapshots */

int bdrv_snapshot_create(BlockDriverState *bs,
                         QEMUSnapshotInfo *sn_info)
{
    BlockDriver *drv = bs->drv;
    if (!drv)
        return -ENOMEDIUM;
    if (!drv->bdrv_snapshot_create)
        return -ENOTSUP;
    return drv->bdrv_snapshot_create(bs, sn_info);
}

int bdrv_snapshot_goto(BlockDriverState *bs,
                       const char *snapshot_id)
{
    BlockDriver *drv = bs->drv;
    if (!drv)
        return -ENOMEDIUM;
    if (!drv->bdrv_snapshot_goto)
        return -ENOTSUP;
    return drv->bdrv_snapshot_goto(bs, snapshot_id);
}

int bdrv_snapshot_delete(BlockDriverState *bs, const char *snapshot_id)
{
    BlockDriver *drv = bs->drv;
    if (!drv)
        return -ENOMEDIUM;
    if (!drv->bdrv_snapshot_delete)
        return -ENOTSUP;
    return drv->bdrv_snapshot_delete(bs, snapshot_id);
}

int bdrv_snapshot_list(BlockDriverState *bs,
                       QEMUSnapshotInfo **psn_info)
{
    BlockDriver *drv = bs->drv;
    if (!drv)
        return -ENOMEDIUM;
    if (!drv->bdrv_snapshot_list)
        return -ENOTSUP;
    return drv->bdrv_snapshot_list(bs, psn_info);
}

#define NB_SUFFIXES 4

char *get_human_readable_size(char *buf, int buf_size, int64_t size)
{
    static const char suffixes[NB_SUFFIXES] = "KMGT";
    int64_t base;
    int i;

    if (size <= 999) {
        snprintf(buf, buf_size, "%" PRId64, size);
    } else {
        base = 1024;
        for(i = 0; i < NB_SUFFIXES; i++) {
            if (size < (10 * base)) {
                snprintf(buf, buf_size, "%0.1f%c",
                         (double)size / base,
                         suffixes[i]);
                break;
            } else if (size < (1000 * base) || i == (NB_SUFFIXES - 1)) {
                snprintf(buf, buf_size, "%" PRId64 "%c",
                         ((size + (base >> 1)) / base),
                         suffixes[i]);
                break;
            }
            base = base * 1024;
        }
    }
    return buf;
}

char *bdrv_snapshot_dump(char *buf, int buf_size, QEMUSnapshotInfo *sn)
{
    char buf1[128], date_buf[128], clock_buf[128];
#ifdef _WIN32
    struct tm *ptm;
#else
    struct tm tm;
#endif
    time_t ti;
    int64_t secs;

    if (!sn) {
        snprintf(buf, buf_size,
                 "%-10s%-20s%7s%20s%15s",
                 "ID", "TAG", "VM SIZE", "DATE", "VM CLOCK");
    } else {
        ti = sn->date_sec;
#ifdef _WIN32
        ptm = localtime(&ti);
        strftime(date_buf, sizeof(date_buf),
                 "%Y-%m-%d %H:%M:%S", ptm);
#else
        localtime_r(&ti, &tm);
        strftime(date_buf, sizeof(date_buf),
                 "%Y-%m-%d %H:%M:%S", &tm);
#endif
        secs = sn->vm_clock_nsec / 1000000000;
        snprintf(clock_buf, sizeof(clock_buf),
                 "%02d:%02d:%02d.%03d",
                 (int)(secs / 3600),
                 (int)((secs / 60) % 60),
                 (int)(secs % 60),
                 (int)((sn->vm_clock_nsec / 1000000) % 1000));
        snprintf(buf, buf_size,
                 "%-10s%-20s%7s%20s%15s",
                 sn->id_str, sn->name,
                 get_human_readable_size(buf1, sizeof(buf1), sn->vm_state_size),
                 date_buf,
                 clock_buf);
    }
    return buf;
}


/**************************************************************/
/* async I/Os */

BlockDriverAIOCB *bdrv_aio_readv(BlockDriverState *bs, int64_t sector_num,
                                 QEMUIOVector *qiov, int nb_sectors,
                                 BlockDriverCompletionFunc *cb, void *opaque)
{
    BlockDriver *drv = bs->drv;
    BlockDriverAIOCB *ret;

    if (!drv)
        return NULL;
    if (bdrv_check_request(bs, sector_num, nb_sectors))
        return NULL;

    ret = drv->bdrv_aio_readv(bs, sector_num, qiov, nb_sectors,
                              cb, opaque);

    if (ret) {
	/* Update stats even though technically transfer has not happened. */
	bs->rd_bytes += (unsigned) nb_sectors * SECTOR_SIZE;
	bs->rd_ops ++;
    }

    return ret;
}

BlockDriverAIOCB *bdrv_aio_writev(BlockDriverState *bs, int64_t sector_num,
                                  QEMUIOVector *qiov, int nb_sectors,
                                  BlockDriverCompletionFunc *cb, void *opaque)
{
    BlockDriver *drv = bs->drv;
    BlockDriverAIOCB *ret;

    if (!drv)
        return NULL;
    if (bs->read_only)
        return NULL;
    if (bdrv_check_request(bs, sector_num, nb_sectors))
        return NULL;

    ret = drv->bdrv_aio_writev(bs, sector_num, qiov, nb_sectors,
                               cb, opaque);

    if (ret) {
	/* Update stats even though technically transfer has not happened. */
	bs->wr_bytes += (unsigned) nb_sectors * SECTOR_SIZE;
	bs->wr_ops ++;
    }

    return ret;
}

void bdrv_aio_cancel(BlockDriverAIOCB *acb)
{
    acb->pool->cancel(acb);
}


/**************************************************************/
/* async block device emulation */

typedef struct BlockDriverAIOCBSync {
    BlockDriverAIOCB common;
    QEMUBH *bh;
    int ret;
    /* vector translation state */
    QEMUIOVector *qiov;
    uint8_t *bounce;
    int is_write;
} BlockDriverAIOCBSync;

static void bdrv_aio_cancel_em(BlockDriverAIOCB *blockacb)
{
    BlockDriverAIOCBSync *acb = (BlockDriverAIOCBSync *)blockacb;
    qemu_bh_delete(acb->bh);
    acb->bh = NULL;
    qemu_aio_release(acb);
}

static AIOPool bdrv_em_aio_pool = {
    .aiocb_size         = sizeof(BlockDriverAIOCBSync),
    .cancel             = bdrv_aio_cancel_em,
};

static void bdrv_aio_bh_cb(void *opaque)
{
    BlockDriverAIOCBSync *acb = opaque;

    if (!acb->is_write)
        qemu_iovec_from_buffer(acb->qiov, acb->bounce, acb->qiov->size);
    qemu_vfree(acb->bounce);
    acb->common.cb(acb->common.opaque, acb->ret);
    qemu_bh_delete(acb->bh);
    acb->bh = NULL;
    qemu_aio_release(acb);
}

static BlockDriverAIOCB *bdrv_aio_rw_vector(BlockDriverState *bs,
                                            int64_t sector_num,
                                            QEMUIOVector *qiov,
                                            int nb_sectors,
                                            BlockDriverCompletionFunc *cb,
                                            void *opaque,
                                            int is_write)

{
    BlockDriverAIOCBSync *acb;

    acb = qemu_aio_get(&bdrv_em_aio_pool, bs, cb, opaque);
    acb->is_write = is_write;
    acb->qiov = qiov;
    acb->bounce = qemu_blockalign(bs, qiov->size);

    if (!acb->bh)
        acb->bh = qemu_bh_new(bdrv_aio_bh_cb, acb);

    if (is_write) {
        qemu_iovec_to_buffer(acb->qiov, acb->bounce);
        acb->ret = bdrv_write(bs, sector_num, acb->bounce, nb_sectors);
    } else {
        acb->ret = bdrv_read(bs, sector_num, acb->bounce, nb_sectors);
    }

    qemu_bh_schedule(acb->bh);

    return &acb->common;
}

static BlockDriverAIOCB *bdrv_aio_readv_em(BlockDriverState *bs,
        int64_t sector_num, QEMUIOVector *qiov, int nb_sectors,
        BlockDriverCompletionFunc *cb, void *opaque)
{
    return bdrv_aio_rw_vector(bs, sector_num, qiov, nb_sectors, cb, opaque, 0);
}

static BlockDriverAIOCB *bdrv_aio_writev_em(BlockDriverState *bs,
        int64_t sector_num, QEMUIOVector *qiov, int nb_sectors,
        BlockDriverCompletionFunc *cb, void *opaque)
{
    return bdrv_aio_rw_vector(bs, sector_num, qiov, nb_sectors, cb, opaque, 1);
}

/**************************************************************/
/* sync block device emulation */

static void bdrv_rw_em_cb(void *opaque, int ret)
{
    *(int *)opaque = ret;
}

#define NOT_DONE 0x7fffffff

static int bdrv_read_em(BlockDriverState *bs, int64_t sector_num,
                        uint8_t *buf, int nb_sectors)
{
    int async_ret;
    BlockDriverAIOCB *acb;
    struct iovec iov;
    QEMUIOVector qiov;

    async_ret = NOT_DONE;
    iov.iov_base = (void *)buf;
    iov.iov_len = nb_sectors * 512;
    qemu_iovec_init_external(&qiov, &iov, 1);
    acb = bdrv_aio_readv(bs, sector_num, &qiov, nb_sectors,
        bdrv_rw_em_cb, &async_ret);
    if (acb == NULL)
        return -1;

    while (async_ret == NOT_DONE) {
        qemu_aio_wait();
    }

    return async_ret;
}

static int bdrv_write_em(BlockDriverState *bs, int64_t sector_num,
                         const uint8_t *buf, int nb_sectors)
{
    int async_ret;
    BlockDriverAIOCB *acb;
    struct iovec iov;
    QEMUIOVector qiov;

    async_ret = NOT_DONE;
    iov.iov_base = (void *)buf;
    iov.iov_len = nb_sectors * 512;
    qemu_iovec_init_external(&qiov, &iov, 1);
    acb = bdrv_aio_writev(bs, sector_num, &qiov, nb_sectors,
        bdrv_rw_em_cb, &async_ret);
    if (acb == NULL)
        return -1;
    while (async_ret == NOT_DONE) {
        qemu_aio_wait();
    }
    return async_ret;
}

void bdrv_init(void)
{
    module_call_init(MODULE_INIT_BLOCK);
}

void *qemu_aio_get(AIOPool *pool, BlockDriverState *bs,
                   BlockDriverCompletionFunc *cb, void *opaque)
{
    BlockDriverAIOCB *acb;

    if (pool->free_aiocb) {
        acb = pool->free_aiocb;
        pool->free_aiocb = acb->next;
    } else {
        acb = qemu_mallocz(pool->aiocb_size);
        acb->pool = pool;
    }
    acb->bs = bs;
    acb->cb = cb;
    acb->opaque = opaque;
    return acb;
}

void qemu_aio_release(void *p)
{
    BlockDriverAIOCB *acb = (BlockDriverAIOCB *)p;
    AIOPool *pool = acb->pool;
    acb->next = pool->free_aiocb;
    pool->free_aiocb = acb;
}

/**************************************************************/
/* removable device support */

/**
 * Return TRUE if the media is present
 */
int bdrv_is_inserted(BlockDriverState *bs)
{
    BlockDriver *drv = bs->drv;
    int ret;
    if (!drv)
        return 0;
    if (!drv->bdrv_is_inserted)
        return 1;
    ret = drv->bdrv_is_inserted(bs);
    return ret;
}

/**
 * Return TRUE if the media changed since the last call to this
 * function. It is currently only used for floppy disks
 */
int bdrv_media_changed(BlockDriverState *bs)
{
    BlockDriver *drv = bs->drv;
    int ret;

    if (!drv || !drv->bdrv_media_changed)
        ret = -ENOTSUP;
    else
        ret = drv->bdrv_media_changed(bs);
    if (ret == -ENOTSUP)
        ret = bs->media_changed;
    bs->media_changed = 0;
    return ret;
}

/**
 * If eject_flag is TRUE, eject the media. Otherwise, close the tray
 */
int bdrv_eject(BlockDriverState *bs, int eject_flag)
{
    BlockDriver *drv = bs->drv;
    int ret;

    if (bs->locked) {
        return -EBUSY;
    }

    if (!drv || !drv->bdrv_eject) {
        ret = -ENOTSUP;
    } else {
        ret = drv->bdrv_eject(bs, eject_flag);
    }
    if (ret == -ENOTSUP) {
        if (eject_flag)
            bdrv_close(bs);
        ret = 0;
    }

    return ret;
}

int bdrv_is_locked(BlockDriverState *bs)
{
    return bs->locked;
}

/**
 * Lock or unlock the media (if it is locked, the user won't be able
 * to eject it manually).
 */
void bdrv_set_locked(BlockDriverState *bs, int locked)
{
    BlockDriver *drv = bs->drv;

    bs->locked = locked;
    if (drv && drv->bdrv_set_locked) {
        drv->bdrv_set_locked(bs, locked);
    }
}

/* needed for generic scsi interface */

int bdrv_ioctl(BlockDriverState *bs, unsigned long int req, void *buf)
{
    BlockDriver *drv = bs->drv;

    if (drv && drv->bdrv_ioctl)
        return drv->bdrv_ioctl(bs, req, buf);
    return -ENOTSUP;
}

BlockDriverAIOCB *bdrv_aio_ioctl(BlockDriverState *bs,
        unsigned long int req, void *buf,
        BlockDriverCompletionFunc *cb, void *opaque)
{
    BlockDriver *drv = bs->drv;

    if (drv && drv->bdrv_aio_ioctl)
        return drv->bdrv_aio_ioctl(bs, req, buf, cb, opaque);
    return NULL;
}

void *qemu_blockalign(BlockDriverState *bs, size_t size)
{
    return qemu_memalign((bs && bs->buffer_alignment) ? bs->buffer_alignment : 512, size);
}
