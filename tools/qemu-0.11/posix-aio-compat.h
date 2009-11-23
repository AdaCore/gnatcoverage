/*
 * QEMU posix-aio emulation
 *
 * Copyright IBM, Corp. 2008
 *
 * Authors:
 *  Anthony Liguori   <aliguori@us.ibm.com>
 *
 * This work is licensed under the terms of the GNU GPL, version 2.  See
 * the COPYING file in the top-level directory.
 *
 */

#ifndef QEMU_POSIX_AIO_COMPAT_H
#define QEMU_POSIX_AIO_COMPAT_H

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

#include "sys-queue.h"

#define QEMU_PAIO_CANCELED     0x01
#define QEMU_PAIO_NOTCANCELED  0x02
#define QEMU_PAIO_ALLDONE      0x03

struct qemu_paiocb
{
    int aio_fildes;
    union {
        struct iovec *aio_iov;
	void *aio_ioctl_buf;
    };
    int aio_niov;
    size_t aio_nbytes;
#define aio_ioctl_cmd   aio_nbytes /* for QEMU_PAIO_IOCTL */
    int ev_signo;
    off_t aio_offset;
    unsigned aio_flags;
/* 512 byte alignment required for buffer, offset and length */
#define QEMU_AIO_SECTOR_ALIGNED	0x01

    /* private */
    TAILQ_ENTRY(qemu_paiocb) node;
    int aio_type;
#define QEMU_PAIO_READ         0x01
#define QEMU_PAIO_WRITE        0x02
#define QEMU_PAIO_IOCTL        0x03
    ssize_t ret;
    int active;
};

struct qemu_paioinit
{
    unsigned int aio_threads;
    unsigned int aio_num;
    unsigned int aio_idle_time;
};

int qemu_paio_init(struct qemu_paioinit *aioinit);
int qemu_paio_read(struct qemu_paiocb *aiocb);
int qemu_paio_write(struct qemu_paiocb *aiocb);
int qemu_paio_ioctl(struct qemu_paiocb *aiocb);
int qemu_paio_error(struct qemu_paiocb *aiocb);
ssize_t qemu_paio_return(struct qemu_paiocb *aiocb);
int qemu_paio_cancel(int fd, struct qemu_paiocb *aiocb);

#endif
