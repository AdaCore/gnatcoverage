/*
 * QEMU Sparc SLAVIO aux io port emulation
 *
 * Copyright (c) 2005 Fabrice Bellard
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

#include "sun4m.h"
#include "sysemu.h"
#include "sysbus.h"

/* debug misc */
//#define DEBUG_MISC

/*
 * This is the auxio port, chip control and system control part of
 * chip STP2001 (Slave I/O), also produced as NCR89C105. See
 * http://www.ibiblio.org/pub/historic-linux/early-ports/Sparc/NCR/NCR89C105.txt
 *
 * This also includes the PMC CPU idle controller.
 */

#ifdef DEBUG_MISC
#define MISC_DPRINTF(fmt, ...)                                  \
    do { printf("MISC: " fmt , ## __VA_ARGS__); } while (0)
#else
#define MISC_DPRINTF(fmt, ...)
#endif

typedef struct MiscState {
    SysBusDevice busdev;
    qemu_irq irq;
    uint8_t config;
    uint8_t aux1, aux2;
    uint8_t diag, mctrl;
    uint32_t sysctrl;
    uint16_t leds;
    qemu_irq fdc_tc;
} MiscState;

typedef struct APCState {
    SysBusDevice busdev;
    qemu_irq cpu_halt;
} APCState;

#define MISC_SIZE 1
#define SYSCTRL_SIZE 4

#define MISC_LEDS 0x01600000
#define MISC_CFG  0x01800000
#define MISC_DIAG 0x01a00000
#define MISC_MDM  0x01b00000
#define MISC_SYS  0x01f00000

#define AUX1_TC        0x02

#define AUX2_PWROFF    0x01
#define AUX2_PWRINTCLR 0x02
#define AUX2_PWRFAIL   0x20

#define CFG_PWRINTEN   0x08

#define SYS_RESET      0x01
#define SYS_RESETSTAT  0x02

static void slavio_misc_update_irq(void *opaque)
{
    MiscState *s = opaque;

    if ((s->aux2 & AUX2_PWRFAIL) && (s->config & CFG_PWRINTEN)) {
        MISC_DPRINTF("Raise IRQ\n");
        qemu_irq_raise(s->irq);
    } else {
        MISC_DPRINTF("Lower IRQ\n");
        qemu_irq_lower(s->irq);
    }
}

static void slavio_misc_reset(void *opaque)
{
    MiscState *s = opaque;

    // Diagnostic and system control registers not cleared in reset
    s->config = s->aux1 = s->aux2 = s->mctrl = 0;
}

void slavio_set_power_fail(void *opaque, int power_failing)
{
    MiscState *s = opaque;

    MISC_DPRINTF("Power fail: %d, config: %d\n", power_failing, s->config);
    if (power_failing && (s->config & CFG_PWRINTEN)) {
        s->aux2 |= AUX2_PWRFAIL;
    } else {
        s->aux2 &= ~AUX2_PWRFAIL;
    }
    slavio_misc_update_irq(s);
}

static void slavio_cfg_mem_writeb(void *opaque, target_phys_addr_t addr,
                                  uint32_t val)
{
    MiscState *s = opaque;

    MISC_DPRINTF("Write config %2.2x\n", val & 0xff);
    s->config = val & 0xff;
    slavio_misc_update_irq(s);
}

static uint32_t slavio_cfg_mem_readb(void *opaque, target_phys_addr_t addr)
{
    MiscState *s = opaque;
    uint32_t ret = 0;

    ret = s->config;
    MISC_DPRINTF("Read config %2.2x\n", ret);
    return ret;
}

static CPUReadMemoryFunc *slavio_cfg_mem_read[3] = {
    slavio_cfg_mem_readb,
    NULL,
    NULL,
};

static CPUWriteMemoryFunc *slavio_cfg_mem_write[3] = {
    slavio_cfg_mem_writeb,
    NULL,
    NULL,
};

static void slavio_diag_mem_writeb(void *opaque, target_phys_addr_t addr,
                                   uint32_t val)
{
    MiscState *s = opaque;

    MISC_DPRINTF("Write diag %2.2x\n", val & 0xff);
    s->diag = val & 0xff;
}

static uint32_t slavio_diag_mem_readb(void *opaque, target_phys_addr_t addr)
{
    MiscState *s = opaque;
    uint32_t ret = 0;

    ret = s->diag;
    MISC_DPRINTF("Read diag %2.2x\n", ret);
    return ret;
}

static CPUReadMemoryFunc *slavio_diag_mem_read[3] = {
    slavio_diag_mem_readb,
    NULL,
    NULL,
};

static CPUWriteMemoryFunc *slavio_diag_mem_write[3] = {
    slavio_diag_mem_writeb,
    NULL,
    NULL,
};

static void slavio_mdm_mem_writeb(void *opaque, target_phys_addr_t addr,
                                  uint32_t val)
{
    MiscState *s = opaque;

    MISC_DPRINTF("Write modem control %2.2x\n", val & 0xff);
    s->mctrl = val & 0xff;
}

static uint32_t slavio_mdm_mem_readb(void *opaque, target_phys_addr_t addr)
{
    MiscState *s = opaque;
    uint32_t ret = 0;

    ret = s->mctrl;
    MISC_DPRINTF("Read modem control %2.2x\n", ret);
    return ret;
}

static CPUReadMemoryFunc *slavio_mdm_mem_read[3] = {
    slavio_mdm_mem_readb,
    NULL,
    NULL,
};

static CPUWriteMemoryFunc *slavio_mdm_mem_write[3] = {
    slavio_mdm_mem_writeb,
    NULL,
    NULL,
};

static void slavio_aux1_mem_writeb(void *opaque, target_phys_addr_t addr,
                                   uint32_t val)
{
    MiscState *s = opaque;

    MISC_DPRINTF("Write aux1 %2.2x\n", val & 0xff);
    if (val & AUX1_TC) {
        // Send a pulse to floppy terminal count line
        if (s->fdc_tc) {
            qemu_irq_raise(s->fdc_tc);
            qemu_irq_lower(s->fdc_tc);
        }
        val &= ~AUX1_TC;
    }
    s->aux1 = val & 0xff;
}

static uint32_t slavio_aux1_mem_readb(void *opaque, target_phys_addr_t addr)
{
    MiscState *s = opaque;
    uint32_t ret = 0;

    ret = s->aux1;
    MISC_DPRINTF("Read aux1 %2.2x\n", ret);

    return ret;
}

static CPUReadMemoryFunc *slavio_aux1_mem_read[3] = {
    slavio_aux1_mem_readb,
    NULL,
    NULL,
};

static CPUWriteMemoryFunc *slavio_aux1_mem_write[3] = {
    slavio_aux1_mem_writeb,
    NULL,
    NULL,
};

static void slavio_aux2_mem_writeb(void *opaque, target_phys_addr_t addr,
                                   uint32_t val)
{
    MiscState *s = opaque;

    val &= AUX2_PWRINTCLR | AUX2_PWROFF;
    MISC_DPRINTF("Write aux2 %2.2x\n", val);
    val |= s->aux2 & AUX2_PWRFAIL;
    if (val & AUX2_PWRINTCLR) // Clear Power Fail int
        val &= AUX2_PWROFF;
    s->aux2 = val;
    if (val & AUX2_PWROFF)
        qemu_system_shutdown_request();
    slavio_misc_update_irq(s);
}

static uint32_t slavio_aux2_mem_readb(void *opaque, target_phys_addr_t addr)
{
    MiscState *s = opaque;
    uint32_t ret = 0;

    ret = s->aux2;
    MISC_DPRINTF("Read aux2 %2.2x\n", ret);

    return ret;
}

static CPUReadMemoryFunc *slavio_aux2_mem_read[3] = {
    slavio_aux2_mem_readb,
    NULL,
    NULL,
};

static CPUWriteMemoryFunc *slavio_aux2_mem_write[3] = {
    slavio_aux2_mem_writeb,
    NULL,
    NULL,
};

static void apc_mem_writeb(void *opaque, target_phys_addr_t addr, uint32_t val)
{
    APCState *s = opaque;

    MISC_DPRINTF("Write power management %2.2x\n", val & 0xff);
    qemu_irq_raise(s->cpu_halt);
}

static uint32_t apc_mem_readb(void *opaque, target_phys_addr_t addr)
{
    uint32_t ret = 0;

    MISC_DPRINTF("Read power management %2.2x\n", ret);
    return ret;
}

static CPUReadMemoryFunc *apc_mem_read[3] = {
    apc_mem_readb,
    NULL,
    NULL,
};

static CPUWriteMemoryFunc *apc_mem_write[3] = {
    apc_mem_writeb,
    NULL,
    NULL,
};

static uint32_t slavio_sysctrl_mem_readl(void *opaque, target_phys_addr_t addr)
{
    MiscState *s = opaque;
    uint32_t ret = 0;

    switch (addr) {
    case 0:
        ret = s->sysctrl;
        break;
    default:
        break;
    }
    MISC_DPRINTF("Read system control %08x\n", ret);
    return ret;
}

static void slavio_sysctrl_mem_writel(void *opaque, target_phys_addr_t addr,
                                      uint32_t val)
{
    MiscState *s = opaque;

    MISC_DPRINTF("Write system control %08x\n", val);
    switch (addr) {
    case 0:
        if (val & SYS_RESET) {
            s->sysctrl = SYS_RESETSTAT;
            qemu_system_reset_request();
        }
        break;
    default:
        break;
    }
}

static CPUReadMemoryFunc *slavio_sysctrl_mem_read[3] = {
    NULL,
    NULL,
    slavio_sysctrl_mem_readl,
};

static CPUWriteMemoryFunc *slavio_sysctrl_mem_write[3] = {
    NULL,
    NULL,
    slavio_sysctrl_mem_writel,
};

static uint32_t slavio_led_mem_readw(void *opaque, target_phys_addr_t addr)
{
    MiscState *s = opaque;
    uint32_t ret = 0;

    switch (addr) {
    case 0:
        ret = s->leds;
        break;
    default:
        break;
    }
    MISC_DPRINTF("Read diagnostic LED %04x\n", ret);
    return ret;
}

static void slavio_led_mem_writew(void *opaque, target_phys_addr_t addr,
                                  uint32_t val)
{
    MiscState *s = opaque;

    MISC_DPRINTF("Write diagnostic LED %04x\n", val & 0xffff);
    switch (addr) {
    case 0:
        s->leds = val;
        break;
    default:
        break;
    }
}

static CPUReadMemoryFunc *slavio_led_mem_read[3] = {
    NULL,
    slavio_led_mem_readw,
    NULL,
};

static CPUWriteMemoryFunc *slavio_led_mem_write[3] = {
    NULL,
    slavio_led_mem_writew,
    NULL,
};

static void slavio_misc_save(QEMUFile *f, void *opaque)
{
    MiscState *s = opaque;
    uint32_t tmp = 0;
    uint8_t tmp8;

    qemu_put_be32s(f, &tmp); /* ignored, was IRQ.  */
    qemu_put_8s(f, &s->config);
    qemu_put_8s(f, &s->aux1);
    qemu_put_8s(f, &s->aux2);
    qemu_put_8s(f, &s->diag);
    qemu_put_8s(f, &s->mctrl);
    tmp8 = s->sysctrl & 0xff;
    qemu_put_8s(f, &tmp8);
}

static int slavio_misc_load(QEMUFile *f, void *opaque, int version_id)
{
    MiscState *s = opaque;
    uint32_t tmp;
    uint8_t tmp8;

    if (version_id != 1)
        return -EINVAL;

    qemu_get_be32s(f, &tmp);
    qemu_get_8s(f, &s->config);
    qemu_get_8s(f, &s->aux1);
    qemu_get_8s(f, &s->aux2);
    qemu_get_8s(f, &s->diag);
    qemu_get_8s(f, &s->mctrl);
    qemu_get_8s(f, &tmp8);
    s->sysctrl = (uint32_t)tmp8;
    return 0;
}

void *slavio_misc_init(target_phys_addr_t base,
                       target_phys_addr_t aux1_base,
                       target_phys_addr_t aux2_base, qemu_irq irq,
                       qemu_irq fdc_tc)
{
    DeviceState *dev;
    SysBusDevice *s;
    MiscState *d;

    dev = qdev_create(NULL, "slavio_misc");
    qdev_init(dev);
    s = sysbus_from_qdev(dev);
    if (base) {
        /* 8 bit registers */
        /* Slavio control */
        sysbus_mmio_map(s, 0, base + MISC_CFG);
        /* Diagnostics */
        sysbus_mmio_map(s, 1, base + MISC_DIAG);
        /* Modem control */
        sysbus_mmio_map(s, 2, base + MISC_MDM);
        /* 16 bit registers */
        /* ss600mp diag LEDs */
        sysbus_mmio_map(s, 3, base + MISC_LEDS);
        /* 32 bit registers */
        /* System control */
        sysbus_mmio_map(s, 4, base + MISC_SYS);
    }
    if (aux1_base) {
        /* AUX 1 (Misc System Functions) */
        sysbus_mmio_map(s, 5, aux1_base);
    }
    if (aux2_base) {
        /* AUX 2 (Software Powerdown Control) */
        sysbus_mmio_map(s, 6, aux2_base);
    }
    sysbus_connect_irq(s, 0, irq);
    sysbus_connect_irq(s, 1, fdc_tc);

    d = FROM_SYSBUS(MiscState, s);

    return d;
}

static void apc_init1(SysBusDevice *dev)
{
    APCState *s = FROM_SYSBUS(APCState, dev);
    int io;

    sysbus_init_irq(dev, &s->cpu_halt);

    /* Power management (APC) XXX: not a Slavio device */
    io = cpu_register_io_memory(apc_mem_read, apc_mem_write, s);
    sysbus_init_mmio(dev, MISC_SIZE, io);
}

void apc_init(target_phys_addr_t power_base, qemu_irq cpu_halt)
{
    DeviceState *dev;
    SysBusDevice *s;

    dev = qdev_create(NULL, "apc");
    qdev_init(dev);
    s = sysbus_from_qdev(dev);
    /* Power management (APC) XXX: not a Slavio device */
    sysbus_mmio_map(s, 0, power_base);
    sysbus_connect_irq(s, 0, cpu_halt);
}

static void slavio_misc_init1(SysBusDevice *dev)
{
    MiscState *s = FROM_SYSBUS(MiscState, dev);
    int io;

    sysbus_init_irq(dev, &s->irq);
    sysbus_init_irq(dev, &s->fdc_tc);

    /* 8 bit registers */
    /* Slavio control */
    io = cpu_register_io_memory(slavio_cfg_mem_read,
                                slavio_cfg_mem_write, s);
    sysbus_init_mmio(dev, MISC_SIZE, io);

    /* Diagnostics */
    io = cpu_register_io_memory(slavio_diag_mem_read,
                                slavio_diag_mem_write, s);
    sysbus_init_mmio(dev, MISC_SIZE, io);

    /* Modem control */
    io = cpu_register_io_memory(slavio_mdm_mem_read,
                                slavio_mdm_mem_write, s);
    sysbus_init_mmio(dev, MISC_SIZE, io);

    /* 16 bit registers */
    /* ss600mp diag LEDs */
    io = cpu_register_io_memory(slavio_led_mem_read,
                                slavio_led_mem_write, s);
    sysbus_init_mmio(dev, MISC_SIZE, io);

    /* 32 bit registers */
    /* System control */
    io = cpu_register_io_memory(slavio_sysctrl_mem_read,
                                slavio_sysctrl_mem_write, s);
    sysbus_init_mmio(dev, SYSCTRL_SIZE, io);

    /* AUX 1 (Misc System Functions) */
    io = cpu_register_io_memory(slavio_aux1_mem_read,
                                slavio_aux1_mem_write, s);
    sysbus_init_mmio(dev, MISC_SIZE, io);

    /* AUX 2 (Software Powerdown Control) */
    io = cpu_register_io_memory(slavio_aux2_mem_read,
                                slavio_aux2_mem_write, s);
    sysbus_init_mmio(dev, MISC_SIZE, io);

    register_savevm("slavio_misc", -1, 1, slavio_misc_save, slavio_misc_load,
                    s);
    qemu_register_reset(slavio_misc_reset, s);
    slavio_misc_reset(s);
}

static SysBusDeviceInfo slavio_misc_info = {
    .init = slavio_misc_init1,
    .qdev.name  = "slavio_misc",
    .qdev.size  = sizeof(MiscState),
};

static SysBusDeviceInfo apc_info = {
    .init = apc_init1,
    .qdev.name  = "apc",
    .qdev.size  = sizeof(MiscState),
};

static void slavio_misc_register_devices(void)
{
    sysbus_register_withprop(&slavio_misc_info);
    sysbus_register_withprop(&apc_info);
}

device_init(slavio_misc_register_devices)
