#ifndef BLOCK_H
#define BLOCK_H

#include "qemu-aio.h"
#include "qemu-common.h"
#include "qemu-option.h"

/* block.c */
typedef struct BlockDriver BlockDriver;

typedef struct BlockDriverInfo {
    /* in bytes, 0 if irrelevant */
    int cluster_size;
    /* offset at which the VM state can be saved (0 if not possible) */
    int64_t vm_state_offset;
} BlockDriverInfo;

typedef struct QEMUSnapshotInfo {
    char id_str[128]; /* unique snapshot id */
    /* the following fields are informative. They are not needed for
       the consistency of the snapshot */
    char name[256]; /* user choosen name */
    uint32_t vm_state_size; /* VM state info size */
    uint32_t date_sec; /* UTC date of the snapshot */
    uint32_t date_nsec;
    uint64_t vm_clock_nsec; /* VM clock relative to boot */
} QEMUSnapshotInfo;

#define BDRV_O_RDONLY      0x0000
#define BDRV_O_RDWR        0x0002
#define BDRV_O_ACCESS      0x0003
#define BDRV_O_CREAT       0x0004 /* create an empty file */
#define BDRV_O_SNAPSHOT    0x0008 /* open the file read only and save writes in a snapshot */
#define BDRV_O_FILE        0x0010 /* open as a raw file (do not try to
                                     use a disk image format on top of
                                     it (default for
                                     bdrv_file_open()) */
#define BDRV_O_NOCACHE     0x0020 /* do not use the host page cache */
#define BDRV_O_CACHE_WB    0x0040 /* use write-back caching */

#define BDRV_O_CACHE_MASK  (BDRV_O_NOCACHE | BDRV_O_CACHE_WB)

void bdrv_info(Monitor *mon);
void bdrv_info_stats(Monitor *mon);

void bdrv_init(void);
BlockDriver *bdrv_find_format(const char *format_name);
int bdrv_create(BlockDriver *drv, const char* filename,
    QEMUOptionParameter *options);
int bdrv_create2(BlockDriver *drv,
                 const char *filename, int64_t size_in_sectors,
                 const char *backing_file, const char *backing_format,
                 int flags);
BlockDriverState *bdrv_new(const char *device_name);
void bdrv_delete(BlockDriverState *bs);
int bdrv_file_open(BlockDriverState **pbs, const char *filename, int flags);
int bdrv_open(BlockDriverState *bs, const char *filename, int flags);
int bdrv_open2(BlockDriverState *bs, const char *filename, int flags,
               BlockDriver *drv);
void bdrv_close(BlockDriverState *bs);
int bdrv_check(BlockDriverState *bs);
int bdrv_read(BlockDriverState *bs, int64_t sector_num,
              uint8_t *buf, int nb_sectors);
int bdrv_write(BlockDriverState *bs, int64_t sector_num,
               const uint8_t *buf, int nb_sectors);
int bdrv_pread(BlockDriverState *bs, int64_t offset,
               void *buf, int count);
int bdrv_pwrite(BlockDriverState *bs, int64_t offset,
                const void *buf, int count);
int bdrv_truncate(BlockDriverState *bs, int64_t offset);
int64_t bdrv_getlength(BlockDriverState *bs);
void bdrv_get_geometry(BlockDriverState *bs, uint64_t *nb_sectors_ptr);
void bdrv_guess_geometry(BlockDriverState *bs, int *pcyls, int *pheads, int *psecs);
int bdrv_commit(BlockDriverState *bs);
void bdrv_register(BlockDriver *bdrv);

/* async block I/O */
typedef struct BlockDriverAIOCB BlockDriverAIOCB;
typedef void BlockDriverCompletionFunc(void *opaque, int ret);

BlockDriverAIOCB *bdrv_aio_readv(BlockDriverState *bs, int64_t sector_num,
                                 QEMUIOVector *iov, int nb_sectors,
                                 BlockDriverCompletionFunc *cb, void *opaque);
BlockDriverAIOCB *bdrv_aio_writev(BlockDriverState *bs, int64_t sector_num,
                                  QEMUIOVector *iov, int nb_sectors,
                                  BlockDriverCompletionFunc *cb, void *opaque);
void bdrv_aio_cancel(BlockDriverAIOCB *acb);

/* sg packet commands */
int bdrv_ioctl(BlockDriverState *bs, unsigned long int req, void *buf);
BlockDriverAIOCB *bdrv_aio_ioctl(BlockDriverState *bs,
        unsigned long int req, void *buf,
        BlockDriverCompletionFunc *cb, void *opaque);

/* Ensure contents are flushed to disk.  */
void bdrv_flush(BlockDriverState *bs);
void bdrv_flush_all(void);

int bdrv_is_allocated(BlockDriverState *bs, int64_t sector_num, int nb_sectors,
	int *pnum);

#define BDRV_TYPE_HD     0
#define BDRV_TYPE_CDROM  1
#define BDRV_TYPE_FLOPPY 2
#define BIOS_ATA_TRANSLATION_AUTO   0
#define BIOS_ATA_TRANSLATION_NONE   1
#define BIOS_ATA_TRANSLATION_LBA    2
#define BIOS_ATA_TRANSLATION_LARGE  3
#define BIOS_ATA_TRANSLATION_RECHS  4

void bdrv_set_geometry_hint(BlockDriverState *bs,
                            int cyls, int heads, int secs);
void bdrv_set_type_hint(BlockDriverState *bs, int type);
void bdrv_set_translation_hint(BlockDriverState *bs, int translation);
void bdrv_get_geometry_hint(BlockDriverState *bs,
                            int *pcyls, int *pheads, int *psecs);
int bdrv_get_type_hint(BlockDriverState *bs);
int bdrv_get_translation_hint(BlockDriverState *bs);
int bdrv_is_removable(BlockDriverState *bs);
int bdrv_is_read_only(BlockDriverState *bs);
int bdrv_is_sg(BlockDriverState *bs);
int bdrv_is_inserted(BlockDriverState *bs);
int bdrv_media_changed(BlockDriverState *bs);
int bdrv_is_locked(BlockDriverState *bs);
void bdrv_set_locked(BlockDriverState *bs, int locked);
int bdrv_eject(BlockDriverState *bs, int eject_flag);
void bdrv_set_change_cb(BlockDriverState *bs,
                        void (*change_cb)(void *opaque), void *opaque);
void bdrv_get_format(BlockDriverState *bs, char *buf, int buf_size);
BlockDriverState *bdrv_find(const char *name);
void bdrv_iterate(void (*it)(void *opaque, BlockDriverState *bs),
                  void *opaque);
int bdrv_is_encrypted(BlockDriverState *bs);
int bdrv_key_required(BlockDriverState *bs);
int bdrv_set_key(BlockDriverState *bs, const char *key);
int bdrv_query_missing_keys(void);
void bdrv_iterate_format(void (*it)(void *opaque, const char *name),
                         void *opaque);
const char *bdrv_get_device_name(BlockDriverState *bs);
int bdrv_write_compressed(BlockDriverState *bs, int64_t sector_num,
                          const uint8_t *buf, int nb_sectors);
int bdrv_get_info(BlockDriverState *bs, BlockDriverInfo *bdi);

const char *bdrv_get_encrypted_filename(BlockDriverState *bs);
void bdrv_get_backing_filename(BlockDriverState *bs,
                               char *filename, int filename_size);
int bdrv_snapshot_create(BlockDriverState *bs,
                         QEMUSnapshotInfo *sn_info);
int bdrv_snapshot_goto(BlockDriverState *bs,
                       const char *snapshot_id);
int bdrv_snapshot_delete(BlockDriverState *bs, const char *snapshot_id);
int bdrv_snapshot_list(BlockDriverState *bs,
                       QEMUSnapshotInfo **psn_info);
char *bdrv_snapshot_dump(char *buf, int buf_size, QEMUSnapshotInfo *sn);

char *get_human_readable_size(char *buf, int buf_size, int64_t size);
int path_is_absolute(const char *path);
void path_combine(char *dest, int dest_size,
                  const char *base_path,
                  const char *filename);

int bdrv_save_vmstate(BlockDriverState *bs, const uint8_t *buf,
                      int64_t pos, int size);

int bdrv_load_vmstate(BlockDriverState *bs, uint8_t *buf,
                      int64_t pos, int size);

#endif
