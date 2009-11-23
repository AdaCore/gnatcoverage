#ifndef MONITOR_H
#define MONITOR_H

#include "qemu-common.h"
#include "qemu-char.h"
#include "block.h"

extern Monitor *cur_mon;

/* flags for monitor_init */
#define MONITOR_IS_DEFAULT    0x01
#define MONITOR_USE_READLINE  0x02

void monitor_init(CharDriverState *chr, int flags);

int monitor_suspend(Monitor *mon);
void monitor_resume(Monitor *mon);

void monitor_read_bdrv_key_start(Monitor *mon, BlockDriverState *bs,
                                 BlockDriverCompletionFunc *completion_cb,
                                 void *opaque);

int monitor_get_fd(Monitor *mon, const char *fdname);

void monitor_vprintf(Monitor *mon, const char *fmt, va_list ap);
void monitor_printf(Monitor *mon, const char *fmt, ...)
    __attribute__ ((__format__ (__printf__, 2, 3)));
void monitor_print_filename(Monitor *mon, const char *filename);
void monitor_flush(Monitor *mon);

#endif /* !MONITOR_H */
