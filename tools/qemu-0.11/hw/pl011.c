/*
 * Arm PrimeCell PL011 UART
 *
 * Copyright (c) 2006 CodeSourcery.
 * Written by Paul Brook
 *
 * This code is licenced under the GPL.
 */

#include "sysbus.h"
#include "qemu-char.h"

typedef struct {
    SysBusDevice busdev;
    uint32_t readbuff;
    uint32_t flags;
    uint32_t lcr;
    uint32_t cr;
    uint32_t dmacr;
    uint32_t int_enabled;
    uint32_t int_level;
    uint32_t read_fifo[16];
    uint32_t ilpr;
    uint32_t ibrd;
    uint32_t fbrd;
    uint32_t ifl;
    int read_pos;
    int read_count;
    int read_trigger;
    CharDriverState *chr;
    qemu_irq irq;
    const unsigned char *id;
} pl011_state;

#define PL011_INT_TX 0x20
#define PL011_INT_RX 0x10

#define PL011_FLAG_TXFE 0x80
#define PL011_FLAG_RXFF 0x40
#define PL011_FLAG_TXFF 0x20
#define PL011_FLAG_RXFE 0x10

static const unsigned char pl011_id_arm[8] =
  { 0x11, 0x10, 0x14, 0x00, 0x0d, 0xf0, 0x05, 0xb1 };
static const unsigned char pl011_id_luminary[8] =
  { 0x11, 0x00, 0x18, 0x01, 0x0d, 0xf0, 0x05, 0xb1 };

static void pl011_update(pl011_state *s)
{
    uint32_t flags;

    flags = s->int_level & s->int_enabled;
    qemu_set_irq(s->irq, flags != 0);
}

static uint32_t pl011_read(void *opaque, target_phys_addr_t offset)
{
    pl011_state *s = (pl011_state *)opaque;
    uint32_t c;

    if (offset >= 0xfe0 && offset < 0x1000) {
        return s->id[(offset - 0xfe0) >> 2];
    }
    switch (offset >> 2) {
    case 0: /* UARTDR */
        s->flags &= ~PL011_FLAG_RXFF;
        c = s->read_fifo[s->read_pos];
        if (s->read_count > 0) {
            s->read_count--;
            if (++s->read_pos == 16)
                s->read_pos = 0;
        }
        if (s->read_count == 0) {
            s->flags |= PL011_FLAG_RXFE;
        }
        if (s->read_count == s->read_trigger - 1)
            s->int_level &= ~ PL011_INT_RX;
        pl011_update(s);
        qemu_chr_accept_input(s->chr);
        return c;
    case 1: /* UARTCR */
        return 0;
    case 6: /* UARTFR */
        return s->flags;
    case 8: /* UARTILPR */
        return s->ilpr;
    case 9: /* UARTIBRD */
        return s->ibrd;
    case 10: /* UARTFBRD */
        return s->fbrd;
    case 11: /* UARTLCR_H */
        return s->lcr;
    case 12: /* UARTCR */
        return s->cr;
    case 13: /* UARTIFLS */
        return s->ifl;
    case 14: /* UARTIMSC */
        return s->int_enabled;
    case 15: /* UARTRIS */
        return s->int_level;
    case 16: /* UARTMIS */
        return s->int_level & s->int_enabled;
    case 18: /* UARTDMACR */
        return s->dmacr;
    default:
        hw_error("pl011_read: Bad offset %x\n", (int)offset);
        return 0;
    }
}

static void pl011_set_read_trigger(pl011_state *s)
{
#if 0
    /* The docs say the RX interrupt is triggered when the FIFO exceeds
       the threshold.  However linux only reads the FIFO in response to an
       interrupt.  Triggering the interrupt when the FIFO is non-empty seems
       to make things work.  */
    if (s->lcr & 0x10)
        s->read_trigger = (s->ifl >> 1) & 0x1c;
    else
#endif
        s->read_trigger = 1;
}

static void pl011_write(void *opaque, target_phys_addr_t offset,
                          uint32_t value)
{
    pl011_state *s = (pl011_state *)opaque;
    unsigned char ch;

    switch (offset >> 2) {
    case 0: /* UARTDR */
        /* ??? Check if transmitter is enabled.  */
        ch = value;
        if (s->chr)
            qemu_chr_write(s->chr, &ch, 1);
        s->int_level |= PL011_INT_TX;
        pl011_update(s);
        break;
    case 1: /* UARTCR */
        s->cr = value;
        break;
    case 6: /* UARTFR */
        /* Writes to Flag register are ignored.  */
        break;
    case 8: /* UARTUARTILPR */
        s->ilpr = value;
        break;
    case 9: /* UARTIBRD */
        s->ibrd = value;
        break;
    case 10: /* UARTFBRD */
        s->fbrd = value;
        break;
    case 11: /* UARTLCR_H */
        s->lcr = value;
        pl011_set_read_trigger(s);
        break;
    case 12: /* UARTCR */
        /* ??? Need to implement the enable and loopback bits.  */
        s->cr = value;
        break;
    case 13: /* UARTIFS */
        s->ifl = value;
        pl011_set_read_trigger(s);
        break;
    case 14: /* UARTIMSC */
        s->int_enabled = value;
        pl011_update(s);
        break;
    case 17: /* UARTICR */
        s->int_level &= ~value;
        pl011_update(s);
        break;
    case 18: /* UARTDMACR */
        s->dmacr = value;
        if (value & 3)
            hw_error("PL011: DMA not implemented\n");
        break;
    default:
        hw_error("pl011_write: Bad offset %x\n", (int)offset);
    }
}

static int pl011_can_receive(void *opaque)
{
    pl011_state *s = (pl011_state *)opaque;

    if (s->lcr & 0x10)
        return s->read_count < 16;
    else
        return s->read_count < 1;
}

static void pl011_put_fifo(void *opaque, uint32_t value)
{
    pl011_state *s = (pl011_state *)opaque;
    int slot;

    slot = s->read_pos + s->read_count;
    if (slot >= 16)
        slot -= 16;
    s->read_fifo[slot] = value;
    s->read_count++;
    s->flags &= ~PL011_FLAG_RXFE;
    if (s->cr & 0x10 || s->read_count == 16) {
        s->flags |= PL011_FLAG_RXFF;
    }
    if (s->read_count == s->read_trigger) {
        s->int_level |= PL011_INT_RX;
        pl011_update(s);
    }
}

static void pl011_receive(void *opaque, const uint8_t *buf, int size)
{
    pl011_put_fifo(opaque, *buf);
}

static void pl011_event(void *opaque, int event)
{
    if (event == CHR_EVENT_BREAK)
        pl011_put_fifo(opaque, 0x400);
}

static CPUReadMemoryFunc *pl011_readfn[] = {
   pl011_read,
   pl011_read,
   pl011_read
};

static CPUWriteMemoryFunc *pl011_writefn[] = {
   pl011_write,
   pl011_write,
   pl011_write
};

static void pl011_save(QEMUFile *f, void *opaque)
{
    pl011_state *s = (pl011_state *)opaque;
    int i;

    qemu_put_be32(f, s->readbuff);
    qemu_put_be32(f, s->flags);
    qemu_put_be32(f, s->lcr);
    qemu_put_be32(f, s->cr);
    qemu_put_be32(f, s->dmacr);
    qemu_put_be32(f, s->int_enabled);
    qemu_put_be32(f, s->int_level);
    for (i = 0; i < 16; i++)
        qemu_put_be32(f, s->read_fifo[i]);
    qemu_put_be32(f, s->ilpr);
    qemu_put_be32(f, s->ibrd);
    qemu_put_be32(f, s->fbrd);
    qemu_put_be32(f, s->ifl);
    qemu_put_be32(f, s->read_pos);
    qemu_put_be32(f, s->read_count);
    qemu_put_be32(f, s->read_trigger);
}

static int pl011_load(QEMUFile *f, void *opaque, int version_id)
{
    pl011_state *s = (pl011_state *)opaque;
    int i;

    if (version_id != 1)
        return -EINVAL;

    s->readbuff = qemu_get_be32(f);
    s->flags = qemu_get_be32(f);
    s->lcr = qemu_get_be32(f);
    s->cr = qemu_get_be32(f);
    s->dmacr = qemu_get_be32(f);
    s->int_enabled = qemu_get_be32(f);
    s->int_level = qemu_get_be32(f);
    for (i = 0; i < 16; i++)
        s->read_fifo[i] = qemu_get_be32(f);
    s->ilpr = qemu_get_be32(f);
    s->ibrd = qemu_get_be32(f);
    s->fbrd = qemu_get_be32(f);
    s->ifl = qemu_get_be32(f);
    s->read_pos = qemu_get_be32(f);
    s->read_count = qemu_get_be32(f);
    s->read_trigger = qemu_get_be32(f);

    return 0;
}

static void pl011_init(SysBusDevice *dev, const unsigned char *id)
{
    int iomemtype;
    pl011_state *s = FROM_SYSBUS(pl011_state, dev);

    iomemtype = cpu_register_io_memory(pl011_readfn,
                                       pl011_writefn, s);
    sysbus_init_mmio(dev, 0x1000,iomemtype);
    sysbus_init_irq(dev, &s->irq);
    s->id = id;
    s->chr = qdev_init_chardev(&dev->qdev);

    s->read_trigger = 1;
    s->ifl = 0x12;
    s->cr = 0x300;
    s->flags = 0x90;
    if (s->chr) {
        qemu_chr_add_handlers(s->chr, pl011_can_receive, pl011_receive,
                              pl011_event, s);
    }
    register_savevm("pl011_uart", -1, 1, pl011_save, pl011_load, s);
}

static void pl011_init_arm(SysBusDevice *dev)
{
    pl011_init(dev, pl011_id_arm);
}

static void pl011_init_luminary(SysBusDevice *dev)
{
    pl011_init(dev, pl011_id_luminary);
}

static void pl011_register_devices(void)
{
    sysbus_register_dev("pl011", sizeof(pl011_state),
                        pl011_init_arm);
    sysbus_register_dev("pl011_luminary", sizeof(pl011_state),
                        pl011_init_luminary);
}

device_init(pl011_register_devices)
