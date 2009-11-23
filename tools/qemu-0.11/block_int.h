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
#ifndef BLOCK_INT_H
#define BLOCK_INT_H

#include "block.h"
#include "qemu-option.h"

#define BLOCK_FLAG_ENCRYPT	1
#define BLOCK_FLAG_COMPRESS	2
#define BLOCK_FLAG_COMPAT6	4

#define BLOCK_OPT_SIZE          "size"
#define BLOCK_OPT_ENCRYPT       "encryption"
#define BLOCK_OPT_COMPAT6       "compat6"
#define BLOCK_OPT_BACKING_FILE  "backing_file"
#define BLOCK_OPT_BACKING_FMT   "backing_fmt"
#define BLOCK_OPT_CLUSTER_SIZE  "cluster_size"

typedef struct AIOPool {
    void (*cancel)(BlockDriverAIOCB *acb);
    int aiocb_size;
    BlockDriverAIOCB *free_aiocb;
} AIOPool;

struct BlockDriver {
    const char *format_name;
    int instance_size;
    int (*bdrv_probe)(const uint8_t *buf, int buf_size, const char *filename);
    int (*bdrv_probe_device)(const char *filename);
    int (*bdrv_open)(BlockDriverState *bs, const char *filename, int flags);
    int (*bdrv_read)(BlockDriverState *bs, int64_t sector_num,
                     uint8_t *buf, int nb_sectors);
    int (*bdrv_write)(BlockDriverState *bs, int64_t sector_num,
                      const uint8_t *buf, int nb_sectors);
    void (*bdrv_close)(BlockDriverState *bs);
    int (*bdrv_create)(const char *filename, QEMUOptionParameter *options);
    void (*bdrv_flush)(BlockDriverState *bs);
    int (*bdrv_is_allocated)(BlockDriverState *bs, int64_t sector_num,
                             int nb_sectors, int *pnum);
    int (*bdrv_set_key)(BlockDriverState *bs, const char *key);
    int (*bdrv_make_empty)(BlockDriverState *bs);
    /* aio */
    BlockDriverAIOCB *(*bdrv_aio_readv)(BlockDriverState *bs,
        int64_t sector_num, QEMUIOVector *qiov, int nb_sectors,
        BlockDriverCompletionFunc *cb, void *opaque);
    BlockDriverAIOCB *(*bdrv_aio_writev)(BlockDriverState *bs,
        int64_t sector_num, QEMUIOVector *qiov, int nb_sectors,
        BlockDriverCompletionFunc *cb, void *opaque);

    const char *protocol_name;
    int (*bdrv_truncate)(BlockDriverState *bs, int64_t offset);
    int64_t (*bdrv_getlength)(BlockDriverState *bs);
    int (*bdrv_write_compressed)(BlockDriverState *bs, int64_t sector_num,
                                 const uint8_t *buf, int nb_sectors);

    int (*bdrv_snapshot_create)(BlockDriverState *bs,
                                QEMUSnapshotInfo *sn_info);
    int (*bdrv_snapshot_goto)(BlockDriverState *bs,
                              const char *snapshot_id);
    int (*bdrv_snapshot_delete)(BlockDriverState *bs, const char *snapshot_id);
    int (*bdrv_snapshot_list)(BlockDriverState *bs,
                              QEMUSnapshotInfo **psn_info);
    int (*bdrv_get_info)(BlockDriverState *bs, BlockDriverInfo *bdi);

    int (*bdrv_save_vmstate)(BlockDriverState *bs, const uint8_t *buf,
                             int64_t pos, int size);
    int (*bdrv_load_vmstate)(BlockDriverState *bs, uint8_t *buf,
                             int64_t pos, int size);

    /* removable device specific */
    int (*bdrv_is_inserted)(BlockDriverState *bs);
    int (*bdrv_media_changed)(BlockDriverState *bs);
    int (*bdrv_eject)(BlockDriverState *bs, int eject_flag);
    int (*bdrv_set_locked)(BlockDriverState *bs, int locked);

    /* to control generic scsi devices */
    int (*bdrv_ioctl)(BlockDriverState *bs, unsigned long int req, void *buf);
    BlockDriverAIOCB *(*bdrv_aio_ioctl)(BlockDriverState *bs,
        unsigned long int req, void *buf,
        BlockDriverCompletionFunc *cb, void *opaque);

    /* List of options for creating images, terminated by name == NULL */
    QEMUOptionParameter *create_options;


    /* Returns number of errors in image, -errno for internal errors */
    int (*bdrv_check)(BlockDriverState* bs);

    struct BlockDriver *next;
};

struct BlockDriverState {
    int64_t total_sectors; /* if we are reading a disk image, give its
                              size in sectors */
    int read_only; /* if true, the media is read only */
    int removable; /* if true, the media can be removed */
    int locked;    /* if true, the media cannot temporarily be ejected */
    int encrypted; /* if true, the media is encrypted */
    int valid_key; /* if true, a valid encryption key has been set */
    int sg;        /* if true, the device is a /dev/sg* */
    /* event callback when inserting/removing */
    void (*change_cb)(void *opaque);
    void *change_opaque;

    BlockDriver *drv; /* NULL means no media */
    void *opaque;

    char filename[1024];
    char backing_file[1024]; /* if non zero, the image is a diff of
                                this file image */
    char backing_format[16]; /* if non-zero and backing_file exists */
    int is_temporary;
    int media_changed;

    BlockDriverState *backing_hd;
    /* async read/write emulation */

    void *sync_aiocb;

    /* I/O stats (display with "info blockstats"). */
    uint64_t rd_bytes;
    uint64_t wr_bytes;
    uint64_t rd_ops;
    uint64_t wr_ops;

    /* Whether the disk can expand beyond total_sectors */
    int growable;

    /* the memory alignment required for the buffers handled by this driver */
    int buffer_alignment;

    /* NOTE: the following infos are only hints for real hardware
       drivers. They are not used by the block driver */
    int cyls, heads, secs, translation;
    int type;
    char device_name[32];
    BlockDriverState *next;
    void *private;
};

struct BlockDriverAIOCB {
    AIOPool *pool;
    BlockDriverState *bs;
    BlockDriverCompletionFunc *cb;
    void *opaque;
    BlockDriverAIOCB *next;
};

void get_tmp_filename(char *filename, int size);

void *qemu_aio_get(AIOPool *pool, BlockDriverState *bs,
                   BlockDriverCompletionFunc *cb, void *opaque);
void qemu_aio_release(void *p);

void *qemu_blockalign(BlockDriverState *bs, size_t size);

extern BlockDriverState *bdrv_first;

#ifdef _WIN32
int is_windows_drive(const char *filename);
#endif

#endif /* BLOCK_INT_H */
