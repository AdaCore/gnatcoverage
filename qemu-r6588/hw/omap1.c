/*
 * TI OMAP processors emulation.
 *
 * Copyright (C) 2006-2008 Andrzej Zaborowski  <balrog@zabor.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 or
 * (at your option) version 3 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */
#include "hw.h"
#include "arm-misc.h"
#include "omap.h"
#include "sysemu.h"
#include "qemu-timer.h"
#include "qemu-char.h"
#include "soc_dma.h"
/* We use pc-style serial ports.  */
#include "pc.h"

/* Should signal the TCMI/GPMC */
uint32_t omap_badwidth_read8(void *opaque, target_phys_addr_t addr)
{
    uint8_t ret;

    OMAP_8B_REG(addr);
    cpu_physical_memory_read(addr, (void *) &ret, 1);
    return ret;
}

void omap_badwidth_write8(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    uint8_t val8 = value;

    OMAP_8B_REG(addr);
    cpu_physical_memory_write(addr, (void *) &val8, 1);
}

uint32_t omap_badwidth_read16(void *opaque, target_phys_addr_t addr)
{
    uint16_t ret;

    OMAP_16B_REG(addr);
    cpu_physical_memory_read(addr, (void *) &ret, 2);
    return ret;
}

void omap_badwidth_write16(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    uint16_t val16 = value;

    OMAP_16B_REG(addr);
    cpu_physical_memory_write(addr, (void *) &val16, 2);
}

uint32_t omap_badwidth_read32(void *opaque, target_phys_addr_t addr)
{
    uint32_t ret;

    OMAP_32B_REG(addr);
    cpu_physical_memory_read(addr, (void *) &ret, 4);
    return ret;
}

void omap_badwidth_write32(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    OMAP_32B_REG(addr);
    cpu_physical_memory_write(addr, (void *) &value, 4);
}

/* Interrupt Handlers */
struct omap_intr_handler_bank_s {
    uint32_t irqs;
    uint32_t inputs;
    uint32_t mask;
    uint32_t fiq;
    uint32_t sens_edge;
    uint32_t swi;
    unsigned char priority[32];
};

struct omap_intr_handler_s {
    qemu_irq *pins;
    qemu_irq parent_intr[2];
    unsigned char nbanks;
    int level_only;

    /* state */
    uint32_t new_agr[2];
    int sir_intr[2];
    int autoidle;
    uint32_t mask;
    struct omap_intr_handler_bank_s bank[];
};

static void omap_inth_sir_update(struct omap_intr_handler_s *s, int is_fiq)
{
    int i, j, sir_intr, p_intr, p, f;
    uint32_t level;
    sir_intr = 0;
    p_intr = 255;

    /* Find the interrupt line with the highest dynamic priority.
     * Note: 0 denotes the hightest priority.
     * If all interrupts have the same priority, the default order is IRQ_N,
     * IRQ_N-1,...,IRQ_0. */
    for (j = 0; j < s->nbanks; ++j) {
        level = s->bank[j].irqs & ~s->bank[j].mask &
                (is_fiq ? s->bank[j].fiq : ~s->bank[j].fiq);
        for (f = ffs(level), i = f - 1, level >>= f - 1; f; i += f,
                        level >>= f) {
            p = s->bank[j].priority[i];
            if (p <= p_intr) {
                p_intr = p;
                sir_intr = 32 * j + i;
            }
            f = ffs(level >> 1);
        }
    }
    s->sir_intr[is_fiq] = sir_intr;
}

static inline void omap_inth_update(struct omap_intr_handler_s *s, int is_fiq)
{
    int i;
    uint32_t has_intr = 0;

    for (i = 0; i < s->nbanks; ++i)
        has_intr |= s->bank[i].irqs & ~s->bank[i].mask &
                (is_fiq ? s->bank[i].fiq : ~s->bank[i].fiq);

    if (s->new_agr[is_fiq] & has_intr & s->mask) {
        s->new_agr[is_fiq] = 0;
        omap_inth_sir_update(s, is_fiq);
        qemu_set_irq(s->parent_intr[is_fiq], 1);
    }
}

#define INT_FALLING_EDGE	0
#define INT_LOW_LEVEL		1

static void omap_set_intr(void *opaque, int irq, int req)
{
    struct omap_intr_handler_s *ih = (struct omap_intr_handler_s *) opaque;
    uint32_t rise;

    struct omap_intr_handler_bank_s *bank = &ih->bank[irq >> 5];
    int n = irq & 31;

    if (req) {
        rise = ~bank->irqs & (1 << n);
        if (~bank->sens_edge & (1 << n))
            rise &= ~bank->inputs;

        bank->inputs |= (1 << n);
        if (rise) {
            bank->irqs |= rise;
            omap_inth_update(ih, 0);
            omap_inth_update(ih, 1);
        }
    } else {
        rise = bank->sens_edge & bank->irqs & (1 << n);
        bank->irqs &= ~rise;
        bank->inputs &= ~(1 << n);
    }
}

/* Simplified version with no edge detection */
static void omap_set_intr_noedge(void *opaque, int irq, int req)
{
    struct omap_intr_handler_s *ih = (struct omap_intr_handler_s *) opaque;
    uint32_t rise;

    struct omap_intr_handler_bank_s *bank = &ih->bank[irq >> 5];
    int n = irq & 31;

    if (req) {
        rise = ~bank->inputs & (1 << n);
        if (rise) {
            bank->irqs |= bank->inputs |= rise;
            omap_inth_update(ih, 0);
            omap_inth_update(ih, 1);
        }
    } else
        bank->irqs = (bank->inputs &= ~(1 << n)) | bank->swi;
}

static uint32_t omap_inth_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_intr_handler_s *s = (struct omap_intr_handler_s *) opaque;
    int i, offset = addr;
    int bank_no = offset >> 8;
    int line_no;
    struct omap_intr_handler_bank_s *bank = &s->bank[bank_no];
    offset &= 0xff;

    switch (offset) {
    case 0x00:	/* ITR */
        return bank->irqs;

    case 0x04:	/* MIR */
        return bank->mask;

    case 0x10:	/* SIR_IRQ_CODE */
    case 0x14:  /* SIR_FIQ_CODE */
        if (bank_no != 0)
            break;
        line_no = s->sir_intr[(offset - 0x10) >> 2];
        bank = &s->bank[line_no >> 5];
        i = line_no & 31;
        if (((bank->sens_edge >> i) & 1) == INT_FALLING_EDGE)
            bank->irqs &= ~(1 << i);
        return line_no;

    case 0x18:	/* CONTROL_REG */
        if (bank_no != 0)
            break;
        return 0;

    case 0x1c:	/* ILR0 */
    case 0x20:	/* ILR1 */
    case 0x24:	/* ILR2 */
    case 0x28:	/* ILR3 */
    case 0x2c:	/* ILR4 */
    case 0x30:	/* ILR5 */
    case 0x34:	/* ILR6 */
    case 0x38:	/* ILR7 */
    case 0x3c:	/* ILR8 */
    case 0x40:	/* ILR9 */
    case 0x44:	/* ILR10 */
    case 0x48:	/* ILR11 */
    case 0x4c:	/* ILR12 */
    case 0x50:	/* ILR13 */
    case 0x54:	/* ILR14 */
    case 0x58:	/* ILR15 */
    case 0x5c:	/* ILR16 */
    case 0x60:	/* ILR17 */
    case 0x64:	/* ILR18 */
    case 0x68:	/* ILR19 */
    case 0x6c:	/* ILR20 */
    case 0x70:	/* ILR21 */
    case 0x74:	/* ILR22 */
    case 0x78:	/* ILR23 */
    case 0x7c:	/* ILR24 */
    case 0x80:	/* ILR25 */
    case 0x84:	/* ILR26 */
    case 0x88:	/* ILR27 */
    case 0x8c:	/* ILR28 */
    case 0x90:	/* ILR29 */
    case 0x94:	/* ILR30 */
    case 0x98:	/* ILR31 */
        i = (offset - 0x1c) >> 2;
        return (bank->priority[i] << 2) |
                (((bank->sens_edge >> i) & 1) << 1) |
                ((bank->fiq >> i) & 1);

    case 0x9c:	/* ISR */
        return 0x00000000;

    }
    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_inth_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_intr_handler_s *s = (struct omap_intr_handler_s *) opaque;
    int i, offset = addr;
    int bank_no = offset >> 8;
    struct omap_intr_handler_bank_s *bank = &s->bank[bank_no];
    offset &= 0xff;

    switch (offset) {
    case 0x00:	/* ITR */
        /* Important: ignore the clearing if the IRQ is level-triggered and
           the input bit is 1 */
        bank->irqs &= value | (bank->inputs & bank->sens_edge);
        return;

    case 0x04:	/* MIR */
        bank->mask = value;
        omap_inth_update(s, 0);
        omap_inth_update(s, 1);
        return;

    case 0x10:	/* SIR_IRQ_CODE */
    case 0x14:	/* SIR_FIQ_CODE */
        OMAP_RO_REG(addr);
        break;

    case 0x18:	/* CONTROL_REG */
        if (bank_no != 0)
            break;
        if (value & 2) {
            qemu_set_irq(s->parent_intr[1], 0);
            s->new_agr[1] = ~0;
            omap_inth_update(s, 1);
        }
        if (value & 1) {
            qemu_set_irq(s->parent_intr[0], 0);
            s->new_agr[0] = ~0;
            omap_inth_update(s, 0);
        }
        return;

    case 0x1c:	/* ILR0 */
    case 0x20:	/* ILR1 */
    case 0x24:	/* ILR2 */
    case 0x28:	/* ILR3 */
    case 0x2c:	/* ILR4 */
    case 0x30:	/* ILR5 */
    case 0x34:	/* ILR6 */
    case 0x38:	/* ILR7 */
    case 0x3c:	/* ILR8 */
    case 0x40:	/* ILR9 */
    case 0x44:	/* ILR10 */
    case 0x48:	/* ILR11 */
    case 0x4c:	/* ILR12 */
    case 0x50:	/* ILR13 */
    case 0x54:	/* ILR14 */
    case 0x58:	/* ILR15 */
    case 0x5c:	/* ILR16 */
    case 0x60:	/* ILR17 */
    case 0x64:	/* ILR18 */
    case 0x68:	/* ILR19 */
    case 0x6c:	/* ILR20 */
    case 0x70:	/* ILR21 */
    case 0x74:	/* ILR22 */
    case 0x78:	/* ILR23 */
    case 0x7c:	/* ILR24 */
    case 0x80:	/* ILR25 */
    case 0x84:	/* ILR26 */
    case 0x88:	/* ILR27 */
    case 0x8c:	/* ILR28 */
    case 0x90:	/* ILR29 */
    case 0x94:	/* ILR30 */
    case 0x98:	/* ILR31 */
        i = (offset - 0x1c) >> 2;
        bank->priority[i] = (value >> 2) & 0x1f;
        bank->sens_edge &= ~(1 << i);
        bank->sens_edge |= ((value >> 1) & 1) << i;
        bank->fiq &= ~(1 << i);
        bank->fiq |= (value & 1) << i;
        return;

    case 0x9c:	/* ISR */
        for (i = 0; i < 32; i ++)
            if (value & (1 << i)) {
                omap_set_intr(s, 32 * bank_no + i, 1);
                return;
            }
        return;
    }
    OMAP_BAD_REG(addr);
}

static CPUReadMemoryFunc *omap_inth_readfn[] = {
    omap_badwidth_read32,
    omap_badwidth_read32,
    omap_inth_read,
};

static CPUWriteMemoryFunc *omap_inth_writefn[] = {
    omap_inth_write,
    omap_inth_write,
    omap_inth_write,
};

void omap_inth_reset(struct omap_intr_handler_s *s)
{
    int i;

    for (i = 0; i < s->nbanks; ++i){
        s->bank[i].irqs = 0x00000000;
        s->bank[i].mask = 0xffffffff;
        s->bank[i].sens_edge = 0x00000000;
        s->bank[i].fiq = 0x00000000;
        s->bank[i].inputs = 0x00000000;
        s->bank[i].swi = 0x00000000;
        memset(s->bank[i].priority, 0, sizeof(s->bank[i].priority));

        if (s->level_only)
            s->bank[i].sens_edge = 0xffffffff;
    }

    s->new_agr[0] = ~0;
    s->new_agr[1] = ~0;
    s->sir_intr[0] = 0;
    s->sir_intr[1] = 0;
    s->autoidle = 0;
    s->mask = ~0;

    qemu_set_irq(s->parent_intr[0], 0);
    qemu_set_irq(s->parent_intr[1], 0);
}

struct omap_intr_handler_s *omap_inth_init(target_phys_addr_t base,
                unsigned long size, unsigned char nbanks, qemu_irq **pins,
                qemu_irq parent_irq, qemu_irq parent_fiq, omap_clk clk)
{
    int iomemtype;
    struct omap_intr_handler_s *s = (struct omap_intr_handler_s *)
            qemu_mallocz(sizeof(struct omap_intr_handler_s) +
                            sizeof(struct omap_intr_handler_bank_s) * nbanks);

    s->parent_intr[0] = parent_irq;
    s->parent_intr[1] = parent_fiq;
    s->nbanks = nbanks;
    s->pins = qemu_allocate_irqs(omap_set_intr, s, nbanks * 32);
    if (pins)
        *pins = s->pins;

    omap_inth_reset(s);

    iomemtype = cpu_register_io_memory(0, omap_inth_readfn,
                    omap_inth_writefn, s);
    cpu_register_physical_memory(base, size, iomemtype);

    return s;
}

static uint32_t omap2_inth_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_intr_handler_s *s = (struct omap_intr_handler_s *) opaque;
    int offset = addr;
    int bank_no, line_no;
    struct omap_intr_handler_bank_s *bank = 0;

    if ((offset & 0xf80) == 0x80) {
        bank_no = (offset & 0x60) >> 5;
        if (bank_no < s->nbanks) {
            offset &= ~0x60;
            bank = &s->bank[bank_no];
        }
    }

    switch (offset) {
    case 0x00:	/* INTC_REVISION */
        return 0x21;

    case 0x10:	/* INTC_SYSCONFIG */
        return (s->autoidle >> 2) & 1;

    case 0x14:	/* INTC_SYSSTATUS */
        return 1;						/* RESETDONE */

    case 0x40:	/* INTC_SIR_IRQ */
        return s->sir_intr[0];

    case 0x44:	/* INTC_SIR_FIQ */
        return s->sir_intr[1];

    case 0x48:	/* INTC_CONTROL */
        return (!s->mask) << 2;					/* GLOBALMASK */

    case 0x4c:	/* INTC_PROTECTION */
        return 0;

    case 0x50:	/* INTC_IDLE */
        return s->autoidle & 3;

    /* Per-bank registers */
    case 0x80:	/* INTC_ITR */
        return bank->inputs;

    case 0x84:	/* INTC_MIR */
        return bank->mask;

    case 0x88:	/* INTC_MIR_CLEAR */
    case 0x8c:	/* INTC_MIR_SET */
        return 0;

    case 0x90:	/* INTC_ISR_SET */
        return bank->swi;

    case 0x94:	/* INTC_ISR_CLEAR */
        return 0;

    case 0x98:	/* INTC_PENDING_IRQ */
        return bank->irqs & ~bank->mask & ~bank->fiq;

    case 0x9c:	/* INTC_PENDING_FIQ */
        return bank->irqs & ~bank->mask & bank->fiq;

    /* Per-line registers */
    case 0x100 ... 0x300:	/* INTC_ILR */
        bank_no = (offset - 0x100) >> 7;
        if (bank_no > s->nbanks)
            break;
        bank = &s->bank[bank_no];
        line_no = (offset & 0x7f) >> 2;
        return (bank->priority[line_no] << 2) |
                ((bank->fiq >> line_no) & 1);
    }
    OMAP_BAD_REG(addr);
    return 0;
}

static void omap2_inth_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_intr_handler_s *s = (struct omap_intr_handler_s *) opaque;
    int offset = addr;
    int bank_no, line_no;
    struct omap_intr_handler_bank_s *bank = 0;

    if ((offset & 0xf80) == 0x80) {
        bank_no = (offset & 0x60) >> 5;
        if (bank_no < s->nbanks) {
            offset &= ~0x60;
            bank = &s->bank[bank_no];
        }
    }

    switch (offset) {
    case 0x10:	/* INTC_SYSCONFIG */
        s->autoidle &= 4;
        s->autoidle |= (value & 1) << 2;
        if (value & 2)						/* SOFTRESET */
            omap_inth_reset(s);
        return;

    case 0x48:	/* INTC_CONTROL */
        s->mask = (value & 4) ? 0 : ~0;				/* GLOBALMASK */
        if (value & 2) {					/* NEWFIQAGR */
            qemu_set_irq(s->parent_intr[1], 0);
            s->new_agr[1] = ~0;
            omap_inth_update(s, 1);
        }
        if (value & 1) {					/* NEWIRQAGR */
            qemu_set_irq(s->parent_intr[0], 0);
            s->new_agr[0] = ~0;
            omap_inth_update(s, 0);
        }
        return;

    case 0x4c:	/* INTC_PROTECTION */
        /* TODO: Make a bitmap (or sizeof(char)map) of access privileges
         * for every register, see Chapter 3 and 4 for privileged mode.  */
        if (value & 1)
            fprintf(stderr, "%s: protection mode enable attempt\n",
                            __FUNCTION__);
        return;

    case 0x50:	/* INTC_IDLE */
        s->autoidle &= ~3;
        s->autoidle |= value & 3;
        return;

    /* Per-bank registers */
    case 0x84:	/* INTC_MIR */
        bank->mask = value;
        omap_inth_update(s, 0);
        omap_inth_update(s, 1);
        return;

    case 0x88:	/* INTC_MIR_CLEAR */
        bank->mask &= ~value;
        omap_inth_update(s, 0);
        omap_inth_update(s, 1);
        return;

    case 0x8c:	/* INTC_MIR_SET */
        bank->mask |= value;
        return;

    case 0x90:	/* INTC_ISR_SET */
        bank->irqs |= bank->swi |= value;
        omap_inth_update(s, 0);
        omap_inth_update(s, 1);
        return;

    case 0x94:	/* INTC_ISR_CLEAR */
        bank->swi &= ~value;
        bank->irqs = bank->swi & bank->inputs;
        return;

    /* Per-line registers */
    case 0x100 ... 0x300:	/* INTC_ILR */
        bank_no = (offset - 0x100) >> 7;
        if (bank_no > s->nbanks)
            break;
        bank = &s->bank[bank_no];
        line_no = (offset & 0x7f) >> 2;
        bank->priority[line_no] = (value >> 2) & 0x3f;
        bank->fiq &= ~(1 << line_no);
        bank->fiq |= (value & 1) << line_no;
        return;

    case 0x00:	/* INTC_REVISION */
    case 0x14:	/* INTC_SYSSTATUS */
    case 0x40:	/* INTC_SIR_IRQ */
    case 0x44:	/* INTC_SIR_FIQ */
    case 0x80:	/* INTC_ITR */
    case 0x98:	/* INTC_PENDING_IRQ */
    case 0x9c:	/* INTC_PENDING_FIQ */
        OMAP_RO_REG(addr);
        return;
    }
    OMAP_BAD_REG(addr);
}

static CPUReadMemoryFunc *omap2_inth_readfn[] = {
    omap_badwidth_read32,
    omap_badwidth_read32,
    omap2_inth_read,
};

static CPUWriteMemoryFunc *omap2_inth_writefn[] = {
    omap2_inth_write,
    omap2_inth_write,
    omap2_inth_write,
};

struct omap_intr_handler_s *omap2_inth_init(target_phys_addr_t base,
                int size, int nbanks, qemu_irq **pins,
                qemu_irq parent_irq, qemu_irq parent_fiq,
                omap_clk fclk, omap_clk iclk)
{
    int iomemtype;
    struct omap_intr_handler_s *s = (struct omap_intr_handler_s *)
            qemu_mallocz(sizeof(struct omap_intr_handler_s) +
                            sizeof(struct omap_intr_handler_bank_s) * nbanks);

    s->parent_intr[0] = parent_irq;
    s->parent_intr[1] = parent_fiq;
    s->nbanks = nbanks;
    s->level_only = 1;
    s->pins = qemu_allocate_irqs(omap_set_intr_noedge, s, nbanks * 32);
    if (pins)
        *pins = s->pins;

    omap_inth_reset(s);

    iomemtype = cpu_register_io_memory(0, omap2_inth_readfn,
                    omap2_inth_writefn, s);
    cpu_register_physical_memory(base, size, iomemtype);

    return s;
}

/* MPU OS timers */
struct omap_mpu_timer_s {
    qemu_irq irq;
    omap_clk clk;
    uint32_t val;
    int64_t time;
    QEMUTimer *timer;
    QEMUBH *tick;
    int64_t rate;
    int it_ena;

    int enable;
    int ptv;
    int ar;
    int st;
    uint32_t reset_val;
};

static inline uint32_t omap_timer_read(struct omap_mpu_timer_s *timer)
{
    uint64_t distance = qemu_get_clock(vm_clock) - timer->time;

    if (timer->st && timer->enable && timer->rate)
        return timer->val - muldiv64(distance >> (timer->ptv + 1),
                        timer->rate, ticks_per_sec);
    else
        return timer->val;
}

static inline void omap_timer_sync(struct omap_mpu_timer_s *timer)
{
    timer->val = omap_timer_read(timer);
    timer->time = qemu_get_clock(vm_clock);
}

static inline void omap_timer_update(struct omap_mpu_timer_s *timer)
{
    int64_t expires;

    if (timer->enable && timer->st && timer->rate) {
        timer->val = timer->reset_val;	/* Should skip this on clk enable */
        expires = muldiv64((uint64_t) timer->val << (timer->ptv + 1),
                        ticks_per_sec, timer->rate);

        /* If timer expiry would be sooner than in about 1 ms and
         * auto-reload isn't set, then fire immediately.  This is a hack
         * to make systems like PalmOS run in acceptable time.  PalmOS
         * sets the interval to a very low value and polls the status bit
         * in a busy loop when it wants to sleep just a couple of CPU
         * ticks.  */
        if (expires > (ticks_per_sec >> 10) || timer->ar)
            qemu_mod_timer(timer->timer, timer->time + expires);
        else
            qemu_bh_schedule(timer->tick);
    } else
        qemu_del_timer(timer->timer);
}

static void omap_timer_fire(void *opaque)
{
    struct omap_mpu_timer_s *timer = opaque;

    if (!timer->ar) {
        timer->val = 0;
        timer->st = 0;
    }

    if (timer->it_ena)
        /* Edge-triggered irq */
        qemu_irq_pulse(timer->irq);
}

static void omap_timer_tick(void *opaque)
{
    struct omap_mpu_timer_s *timer = (struct omap_mpu_timer_s *) opaque;

    omap_timer_sync(timer);
    omap_timer_fire(timer);
    omap_timer_update(timer);
}

static void omap_timer_clk_update(void *opaque, int line, int on)
{
    struct omap_mpu_timer_s *timer = (struct omap_mpu_timer_s *) opaque;

    omap_timer_sync(timer);
    timer->rate = on ? omap_clk_getrate(timer->clk) : 0;
    omap_timer_update(timer);
}

static void omap_timer_clk_setup(struct omap_mpu_timer_s *timer)
{
    omap_clk_adduser(timer->clk,
                    qemu_allocate_irqs(omap_timer_clk_update, timer, 1)[0]);
    timer->rate = omap_clk_getrate(timer->clk);
}

static uint32_t omap_mpu_timer_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_mpu_timer_s *s = (struct omap_mpu_timer_s *) opaque;

    switch (addr) {
    case 0x00:	/* CNTL_TIMER */
        return (s->enable << 5) | (s->ptv << 2) | (s->ar << 1) | s->st;

    case 0x04:	/* LOAD_TIM */
        break;

    case 0x08:	/* READ_TIM */
        return omap_timer_read(s);
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_mpu_timer_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_mpu_timer_s *s = (struct omap_mpu_timer_s *) opaque;

    switch (addr) {
    case 0x00:	/* CNTL_TIMER */
        omap_timer_sync(s);
        s->enable = (value >> 5) & 1;
        s->ptv = (value >> 2) & 7;
        s->ar = (value >> 1) & 1;
        s->st = value & 1;
        omap_timer_update(s);
        return;

    case 0x04:	/* LOAD_TIM */
        s->reset_val = value;
        return;

    case 0x08:	/* READ_TIM */
        OMAP_RO_REG(addr);
        break;

    default:
        OMAP_BAD_REG(addr);
    }
}

static CPUReadMemoryFunc *omap_mpu_timer_readfn[] = {
    omap_badwidth_read32,
    omap_badwidth_read32,
    omap_mpu_timer_read,
};

static CPUWriteMemoryFunc *omap_mpu_timer_writefn[] = {
    omap_badwidth_write32,
    omap_badwidth_write32,
    omap_mpu_timer_write,
};

static void omap_mpu_timer_reset(struct omap_mpu_timer_s *s)
{
    qemu_del_timer(s->timer);
    s->enable = 0;
    s->reset_val = 31337;
    s->val = 0;
    s->ptv = 0;
    s->ar = 0;
    s->st = 0;
    s->it_ena = 1;
}

struct omap_mpu_timer_s *omap_mpu_timer_init(target_phys_addr_t base,
                qemu_irq irq, omap_clk clk)
{
    int iomemtype;
    struct omap_mpu_timer_s *s = (struct omap_mpu_timer_s *)
            qemu_mallocz(sizeof(struct omap_mpu_timer_s));

    s->irq = irq;
    s->clk = clk;
    s->timer = qemu_new_timer(vm_clock, omap_timer_tick, s);
    s->tick = qemu_bh_new(omap_timer_fire, s);
    omap_mpu_timer_reset(s);
    omap_timer_clk_setup(s);

    iomemtype = cpu_register_io_memory(0, omap_mpu_timer_readfn,
                    omap_mpu_timer_writefn, s);
    cpu_register_physical_memory(base, 0x100, iomemtype);

    return s;
}

/* Watchdog timer */
struct omap_watchdog_timer_s {
    struct omap_mpu_timer_s timer;
    uint8_t last_wr;
    int mode;
    int free;
    int reset;
};

static uint32_t omap_wd_timer_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_watchdog_timer_s *s = (struct omap_watchdog_timer_s *) opaque;

    switch (addr) {
    case 0x00:	/* CNTL_TIMER */
        return (s->timer.ptv << 9) | (s->timer.ar << 8) |
                (s->timer.st << 7) | (s->free << 1);

    case 0x04:	/* READ_TIMER */
        return omap_timer_read(&s->timer);

    case 0x08:	/* TIMER_MODE */
        return s->mode << 15;
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_wd_timer_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_watchdog_timer_s *s = (struct omap_watchdog_timer_s *) opaque;

    switch (addr) {
    case 0x00:	/* CNTL_TIMER */
        omap_timer_sync(&s->timer);
        s->timer.ptv = (value >> 9) & 7;
        s->timer.ar = (value >> 8) & 1;
        s->timer.st = (value >> 7) & 1;
        s->free = (value >> 1) & 1;
        omap_timer_update(&s->timer);
        break;

    case 0x04:	/* LOAD_TIMER */
        s->timer.reset_val = value & 0xffff;
        break;

    case 0x08:	/* TIMER_MODE */
        if (!s->mode && ((value >> 15) & 1))
            omap_clk_get(s->timer.clk);
        s->mode |= (value >> 15) & 1;
        if (s->last_wr == 0xf5) {
            if ((value & 0xff) == 0xa0) {
                if (s->mode) {
                    s->mode = 0;
                    omap_clk_put(s->timer.clk);
                }
            } else {
                /* XXX: on T|E hardware somehow this has no effect,
                 * on Zire 71 it works as specified.  */
                s->reset = 1;
                qemu_system_reset_request();
            }
        }
        s->last_wr = value & 0xff;
        break;

    default:
        OMAP_BAD_REG(addr);
    }
}

static CPUReadMemoryFunc *omap_wd_timer_readfn[] = {
    omap_badwidth_read16,
    omap_wd_timer_read,
    omap_badwidth_read16,
};

static CPUWriteMemoryFunc *omap_wd_timer_writefn[] = {
    omap_badwidth_write16,
    omap_wd_timer_write,
    omap_badwidth_write16,
};

static void omap_wd_timer_reset(struct omap_watchdog_timer_s *s)
{
    qemu_del_timer(s->timer.timer);
    if (!s->mode)
        omap_clk_get(s->timer.clk);
    s->mode = 1;
    s->free = 1;
    s->reset = 0;
    s->timer.enable = 1;
    s->timer.it_ena = 1;
    s->timer.reset_val = 0xffff;
    s->timer.val = 0;
    s->timer.st = 0;
    s->timer.ptv = 0;
    s->timer.ar = 0;
    omap_timer_update(&s->timer);
}

struct omap_watchdog_timer_s *omap_wd_timer_init(target_phys_addr_t base,
                qemu_irq irq, omap_clk clk)
{
    int iomemtype;
    struct omap_watchdog_timer_s *s = (struct omap_watchdog_timer_s *)
            qemu_mallocz(sizeof(struct omap_watchdog_timer_s));

    s->timer.irq = irq;
    s->timer.clk = clk;
    s->timer.timer = qemu_new_timer(vm_clock, omap_timer_tick, &s->timer);
    omap_wd_timer_reset(s);
    omap_timer_clk_setup(&s->timer);

    iomemtype = cpu_register_io_memory(0, omap_wd_timer_readfn,
                    omap_wd_timer_writefn, s);
    cpu_register_physical_memory(base, 0x100, iomemtype);

    return s;
}

/* 32-kHz timer */
struct omap_32khz_timer_s {
    struct omap_mpu_timer_s timer;
};

static uint32_t omap_os_timer_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_32khz_timer_s *s = (struct omap_32khz_timer_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    switch (offset) {
    case 0x00:	/* TVR */
        return s->timer.reset_val;

    case 0x04:	/* TCR */
        return omap_timer_read(&s->timer);

    case 0x08:	/* CR */
        return (s->timer.ar << 3) | (s->timer.it_ena << 2) | s->timer.st;

    default:
        break;
    }
    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_os_timer_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_32khz_timer_s *s = (struct omap_32khz_timer_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    switch (offset) {
    case 0x00:	/* TVR */
        s->timer.reset_val = value & 0x00ffffff;
        break;

    case 0x04:	/* TCR */
        OMAP_RO_REG(addr);
        break;

    case 0x08:	/* CR */
        s->timer.ar = (value >> 3) & 1;
        s->timer.it_ena = (value >> 2) & 1;
        if (s->timer.st != (value & 1) || (value & 2)) {
            omap_timer_sync(&s->timer);
            s->timer.enable = value & 1;
            s->timer.st = value & 1;
            omap_timer_update(&s->timer);
        }
        break;

    default:
        OMAP_BAD_REG(addr);
    }
}

static CPUReadMemoryFunc *omap_os_timer_readfn[] = {
    omap_badwidth_read32,
    omap_badwidth_read32,
    omap_os_timer_read,
};

static CPUWriteMemoryFunc *omap_os_timer_writefn[] = {
    omap_badwidth_write32,
    omap_badwidth_write32,
    omap_os_timer_write,
};

static void omap_os_timer_reset(struct omap_32khz_timer_s *s)
{
    qemu_del_timer(s->timer.timer);
    s->timer.enable = 0;
    s->timer.it_ena = 0;
    s->timer.reset_val = 0x00ffffff;
    s->timer.val = 0;
    s->timer.st = 0;
    s->timer.ptv = 0;
    s->timer.ar = 1;
}

struct omap_32khz_timer_s *omap_os_timer_init(target_phys_addr_t base,
                qemu_irq irq, omap_clk clk)
{
    int iomemtype;
    struct omap_32khz_timer_s *s = (struct omap_32khz_timer_s *)
            qemu_mallocz(sizeof(struct omap_32khz_timer_s));

    s->timer.irq = irq;
    s->timer.clk = clk;
    s->timer.timer = qemu_new_timer(vm_clock, omap_timer_tick, &s->timer);
    omap_os_timer_reset(s);
    omap_timer_clk_setup(&s->timer);

    iomemtype = cpu_register_io_memory(0, omap_os_timer_readfn,
                    omap_os_timer_writefn, s);
    cpu_register_physical_memory(base, 0x800, iomemtype);

    return s;
}

/* Ultra Low-Power Device Module */
static uint32_t omap_ulpd_pm_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;
    uint16_t ret;

    switch (addr) {
    case 0x14:	/* IT_STATUS */
        ret = s->ulpd_pm_regs[addr >> 2];
        s->ulpd_pm_regs[addr >> 2] = 0;
        qemu_irq_lower(s->irq[1][OMAP_INT_GAUGE_32K]);
        return ret;

    case 0x18:	/* Reserved */
    case 0x1c:	/* Reserved */
    case 0x20:	/* Reserved */
    case 0x28:	/* Reserved */
    case 0x2c:	/* Reserved */
        OMAP_BAD_REG(addr);
    case 0x00:	/* COUNTER_32_LSB */
    case 0x04:	/* COUNTER_32_MSB */
    case 0x08:	/* COUNTER_HIGH_FREQ_LSB */
    case 0x0c:	/* COUNTER_HIGH_FREQ_MSB */
    case 0x10:	/* GAUGING_CTRL */
    case 0x24:	/* SETUP_ANALOG_CELL3_ULPD1 */
    case 0x30:	/* CLOCK_CTRL */
    case 0x34:	/* SOFT_REQ */
    case 0x38:	/* COUNTER_32_FIQ */
    case 0x3c:	/* DPLL_CTRL */
    case 0x40:	/* STATUS_REQ */
        /* XXX: check clk::usecount state for every clock */
    case 0x48:	/* LOCL_TIME */
    case 0x4c:	/* APLL_CTRL */
    case 0x50:	/* POWER_CTRL */
        return s->ulpd_pm_regs[addr >> 2];
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static inline void omap_ulpd_clk_update(struct omap_mpu_state_s *s,
                uint16_t diff, uint16_t value)
{
    if (diff & (1 << 4))				/* USB_MCLK_EN */
        omap_clk_onoff(omap_findclk(s, "usb_clk0"), (value >> 4) & 1);
    if (diff & (1 << 5))				/* DIS_USB_PVCI_CLK */
        omap_clk_onoff(omap_findclk(s, "usb_w2fc_ck"), (~value >> 5) & 1);
}

static inline void omap_ulpd_req_update(struct omap_mpu_state_s *s,
                uint16_t diff, uint16_t value)
{
    if (diff & (1 << 0))				/* SOFT_DPLL_REQ */
        omap_clk_canidle(omap_findclk(s, "dpll4"), (~value >> 0) & 1);
    if (diff & (1 << 1))				/* SOFT_COM_REQ */
        omap_clk_canidle(omap_findclk(s, "com_mclk_out"), (~value >> 1) & 1);
    if (diff & (1 << 2))				/* SOFT_SDW_REQ */
        omap_clk_canidle(omap_findclk(s, "bt_mclk_out"), (~value >> 2) & 1);
    if (diff & (1 << 3))				/* SOFT_USB_REQ */
        omap_clk_canidle(omap_findclk(s, "usb_clk0"), (~value >> 3) & 1);
}

static void omap_ulpd_pm_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;
    int64_t now, ticks;
    int div, mult;
    static const int bypass_div[4] = { 1, 2, 4, 4 };
    uint16_t diff;

    switch (addr) {
    case 0x00:	/* COUNTER_32_LSB */
    case 0x04:	/* COUNTER_32_MSB */
    case 0x08:	/* COUNTER_HIGH_FREQ_LSB */
    case 0x0c:	/* COUNTER_HIGH_FREQ_MSB */
    case 0x14:	/* IT_STATUS */
    case 0x40:	/* STATUS_REQ */
        OMAP_RO_REG(addr);
        break;

    case 0x10:	/* GAUGING_CTRL */
        /* Bits 0 and 1 seem to be confused in the OMAP 310 TRM */
        if ((s->ulpd_pm_regs[addr >> 2] ^ value) & 1) {
            now = qemu_get_clock(vm_clock);

            if (value & 1)
                s->ulpd_gauge_start = now;
            else {
                now -= s->ulpd_gauge_start;

                /* 32-kHz ticks */
                ticks = muldiv64(now, 32768, ticks_per_sec);
                s->ulpd_pm_regs[0x00 >> 2] = (ticks >>  0) & 0xffff;
                s->ulpd_pm_regs[0x04 >> 2] = (ticks >> 16) & 0xffff;
                if (ticks >> 32)	/* OVERFLOW_32K */
                    s->ulpd_pm_regs[0x14 >> 2] |= 1 << 2;

                /* High frequency ticks */
                ticks = muldiv64(now, 12000000, ticks_per_sec);
                s->ulpd_pm_regs[0x08 >> 2] = (ticks >>  0) & 0xffff;
                s->ulpd_pm_regs[0x0c >> 2] = (ticks >> 16) & 0xffff;
                if (ticks >> 32)	/* OVERFLOW_HI_FREQ */
                    s->ulpd_pm_regs[0x14 >> 2] |= 1 << 1;

                s->ulpd_pm_regs[0x14 >> 2] |= 1 << 0;	/* IT_GAUGING */
                qemu_irq_raise(s->irq[1][OMAP_INT_GAUGE_32K]);
            }
        }
        s->ulpd_pm_regs[addr >> 2] = value;
        break;

    case 0x18:	/* Reserved */
    case 0x1c:	/* Reserved */
    case 0x20:	/* Reserved */
    case 0x28:	/* Reserved */
    case 0x2c:	/* Reserved */
        OMAP_BAD_REG(addr);
    case 0x24:	/* SETUP_ANALOG_CELL3_ULPD1 */
    case 0x38:	/* COUNTER_32_FIQ */
    case 0x48:	/* LOCL_TIME */
    case 0x50:	/* POWER_CTRL */
        s->ulpd_pm_regs[addr >> 2] = value;
        break;

    case 0x30:	/* CLOCK_CTRL */
        diff = s->ulpd_pm_regs[addr >> 2] ^ value;
        s->ulpd_pm_regs[addr >> 2] = value & 0x3f;
        omap_ulpd_clk_update(s, diff, value);
        break;

    case 0x34:	/* SOFT_REQ */
        diff = s->ulpd_pm_regs[addr >> 2] ^ value;
        s->ulpd_pm_regs[addr >> 2] = value & 0x1f;
        omap_ulpd_req_update(s, diff, value);
        break;

    case 0x3c:	/* DPLL_CTRL */
        /* XXX: OMAP310 TRM claims bit 3 is PLL_ENABLE, and bit 4 is
         * omitted altogether, probably a typo.  */
        /* This register has identical semantics with DPLL(1:3) control
         * registers, see omap_dpll_write() */
        diff = s->ulpd_pm_regs[addr >> 2] & value;
        s->ulpd_pm_regs[addr >> 2] = value & 0x2fff;
        if (diff & (0x3ff << 2)) {
            if (value & (1 << 4)) {			/* PLL_ENABLE */
                div = ((value >> 5) & 3) + 1;		/* PLL_DIV */
                mult = MIN((value >> 7) & 0x1f, 1);	/* PLL_MULT */
            } else {
                div = bypass_div[((value >> 2) & 3)];	/* BYPASS_DIV */
                mult = 1;
            }
            omap_clk_setrate(omap_findclk(s, "dpll4"), div, mult);
        }

        /* Enter the desired mode.  */
        s->ulpd_pm_regs[addr >> 2] =
                (s->ulpd_pm_regs[addr >> 2] & 0xfffe) |
                ((s->ulpd_pm_regs[addr >> 2] >> 4) & 1);

        /* Act as if the lock is restored.  */
        s->ulpd_pm_regs[addr >> 2] |= 2;
        break;

    case 0x4c:	/* APLL_CTRL */
        diff = s->ulpd_pm_regs[addr >> 2] & value;
        s->ulpd_pm_regs[addr >> 2] = value & 0xf;
        if (diff & (1 << 0))				/* APLL_NDPLL_SWITCH */
            omap_clk_reparent(omap_findclk(s, "ck_48m"), omap_findclk(s,
                                    (value & (1 << 0)) ? "apll" : "dpll4"));
        break;

    default:
        OMAP_BAD_REG(addr);
    }
}

static CPUReadMemoryFunc *omap_ulpd_pm_readfn[] = {
    omap_badwidth_read16,
    omap_ulpd_pm_read,
    omap_badwidth_read16,
};

static CPUWriteMemoryFunc *omap_ulpd_pm_writefn[] = {
    omap_badwidth_write16,
    omap_ulpd_pm_write,
    omap_badwidth_write16,
};

static void omap_ulpd_pm_reset(struct omap_mpu_state_s *mpu)
{
    mpu->ulpd_pm_regs[0x00 >> 2] = 0x0001;
    mpu->ulpd_pm_regs[0x04 >> 2] = 0x0000;
    mpu->ulpd_pm_regs[0x08 >> 2] = 0x0001;
    mpu->ulpd_pm_regs[0x0c >> 2] = 0x0000;
    mpu->ulpd_pm_regs[0x10 >> 2] = 0x0000;
    mpu->ulpd_pm_regs[0x18 >> 2] = 0x01;
    mpu->ulpd_pm_regs[0x1c >> 2] = 0x01;
    mpu->ulpd_pm_regs[0x20 >> 2] = 0x01;
    mpu->ulpd_pm_regs[0x24 >> 2] = 0x03ff;
    mpu->ulpd_pm_regs[0x28 >> 2] = 0x01;
    mpu->ulpd_pm_regs[0x2c >> 2] = 0x01;
    omap_ulpd_clk_update(mpu, mpu->ulpd_pm_regs[0x30 >> 2], 0x0000);
    mpu->ulpd_pm_regs[0x30 >> 2] = 0x0000;
    omap_ulpd_req_update(mpu, mpu->ulpd_pm_regs[0x34 >> 2], 0x0000);
    mpu->ulpd_pm_regs[0x34 >> 2] = 0x0000;
    mpu->ulpd_pm_regs[0x38 >> 2] = 0x0001;
    mpu->ulpd_pm_regs[0x3c >> 2] = 0x2211;
    mpu->ulpd_pm_regs[0x40 >> 2] = 0x0000; /* FIXME: dump a real STATUS_REQ */
    mpu->ulpd_pm_regs[0x48 >> 2] = 0x960;
    mpu->ulpd_pm_regs[0x4c >> 2] = 0x08;
    mpu->ulpd_pm_regs[0x50 >> 2] = 0x08;
    omap_clk_setrate(omap_findclk(mpu, "dpll4"), 1, 4);
    omap_clk_reparent(omap_findclk(mpu, "ck_48m"), omap_findclk(mpu, "dpll4"));
}

static void omap_ulpd_pm_init(target_phys_addr_t base,
                struct omap_mpu_state_s *mpu)
{
    int iomemtype = cpu_register_io_memory(0, omap_ulpd_pm_readfn,
                    omap_ulpd_pm_writefn, mpu);

    cpu_register_physical_memory(base, 0x800, iomemtype);
    omap_ulpd_pm_reset(mpu);
}

/* OMAP Pin Configuration */
static uint32_t omap_pin_cfg_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;

    switch (addr) {
    case 0x00:	/* FUNC_MUX_CTRL_0 */
    case 0x04:	/* FUNC_MUX_CTRL_1 */
    case 0x08:	/* FUNC_MUX_CTRL_2 */
        return s->func_mux_ctrl[addr >> 2];

    case 0x0c:	/* COMP_MODE_CTRL_0 */
        return s->comp_mode_ctrl[0];

    case 0x10:	/* FUNC_MUX_CTRL_3 */
    case 0x14:	/* FUNC_MUX_CTRL_4 */
    case 0x18:	/* FUNC_MUX_CTRL_5 */
    case 0x1c:	/* FUNC_MUX_CTRL_6 */
    case 0x20:	/* FUNC_MUX_CTRL_7 */
    case 0x24:	/* FUNC_MUX_CTRL_8 */
    case 0x28:	/* FUNC_MUX_CTRL_9 */
    case 0x2c:	/* FUNC_MUX_CTRL_A */
    case 0x30:	/* FUNC_MUX_CTRL_B */
    case 0x34:	/* FUNC_MUX_CTRL_C */
    case 0x38:	/* FUNC_MUX_CTRL_D */
        return s->func_mux_ctrl[(addr >> 2) - 1];

    case 0x40:	/* PULL_DWN_CTRL_0 */
    case 0x44:	/* PULL_DWN_CTRL_1 */
    case 0x48:	/* PULL_DWN_CTRL_2 */
    case 0x4c:	/* PULL_DWN_CTRL_3 */
        return s->pull_dwn_ctrl[(addr & 0xf) >> 2];

    case 0x50:	/* GATE_INH_CTRL_0 */
        return s->gate_inh_ctrl[0];

    case 0x60:	/* VOLTAGE_CTRL_0 */
        return s->voltage_ctrl[0];

    case 0x70:	/* TEST_DBG_CTRL_0 */
        return s->test_dbg_ctrl[0];

    case 0x80:	/* MOD_CONF_CTRL_0 */
        return s->mod_conf_ctrl[0];
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static inline void omap_pin_funcmux0_update(struct omap_mpu_state_s *s,
                uint32_t diff, uint32_t value)
{
    if (s->compat1509) {
        if (diff & (1 << 9))			/* BLUETOOTH */
            omap_clk_onoff(omap_findclk(s, "bt_mclk_out"),
                            (~value >> 9) & 1);
        if (diff & (1 << 7))			/* USB.CLKO */
            omap_clk_onoff(omap_findclk(s, "usb.clko"),
                            (value >> 7) & 1);
    }
}

static inline void omap_pin_funcmux1_update(struct omap_mpu_state_s *s,
                uint32_t diff, uint32_t value)
{
    if (s->compat1509) {
        if (diff & (1 << 31))			/* MCBSP3_CLK_HIZ_DI */
            omap_clk_onoff(omap_findclk(s, "mcbsp3.clkx"),
                            (value >> 31) & 1);
        if (diff & (1 << 1))			/* CLK32K */
            omap_clk_onoff(omap_findclk(s, "clk32k_out"),
                            (~value >> 1) & 1);
    }
}

static inline void omap_pin_modconf1_update(struct omap_mpu_state_s *s,
                uint32_t diff, uint32_t value)
{
    if (diff & (1 << 31))			/* CONF_MOD_UART3_CLK_MODE_R */
         omap_clk_reparent(omap_findclk(s, "uart3_ck"),
                         omap_findclk(s, ((value >> 31) & 1) ?
                                 "ck_48m" : "armper_ck"));
    if (diff & (1 << 30))			/* CONF_MOD_UART2_CLK_MODE_R */
         omap_clk_reparent(omap_findclk(s, "uart2_ck"),
                         omap_findclk(s, ((value >> 30) & 1) ?
                                 "ck_48m" : "armper_ck"));
    if (diff & (1 << 29))			/* CONF_MOD_UART1_CLK_MODE_R */
         omap_clk_reparent(omap_findclk(s, "uart1_ck"),
                         omap_findclk(s, ((value >> 29) & 1) ?
                                 "ck_48m" : "armper_ck"));
    if (diff & (1 << 23))			/* CONF_MOD_MMC_SD_CLK_REQ_R */
         omap_clk_reparent(omap_findclk(s, "mmc_ck"),
                         omap_findclk(s, ((value >> 23) & 1) ?
                                 "ck_48m" : "armper_ck"));
    if (diff & (1 << 12))			/* CONF_MOD_COM_MCLK_12_48_S */
         omap_clk_reparent(omap_findclk(s, "com_mclk_out"),
                         omap_findclk(s, ((value >> 12) & 1) ?
                                 "ck_48m" : "armper_ck"));
    if (diff & (1 << 9))			/* CONF_MOD_USB_HOST_HHC_UHO */
         omap_clk_onoff(omap_findclk(s, "usb_hhc_ck"), (value >> 9) & 1);
}

static void omap_pin_cfg_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;
    uint32_t diff;

    switch (addr) {
    case 0x00:	/* FUNC_MUX_CTRL_0 */
        diff = s->func_mux_ctrl[addr >> 2] ^ value;
        s->func_mux_ctrl[addr >> 2] = value;
        omap_pin_funcmux0_update(s, diff, value);
        return;

    case 0x04:	/* FUNC_MUX_CTRL_1 */
        diff = s->func_mux_ctrl[addr >> 2] ^ value;
        s->func_mux_ctrl[addr >> 2] = value;
        omap_pin_funcmux1_update(s, diff, value);
        return;

    case 0x08:	/* FUNC_MUX_CTRL_2 */
        s->func_mux_ctrl[addr >> 2] = value;
        return;

    case 0x0c:	/* COMP_MODE_CTRL_0 */
        s->comp_mode_ctrl[0] = value;
        s->compat1509 = (value != 0x0000eaef);
        omap_pin_funcmux0_update(s, ~0, s->func_mux_ctrl[0]);
        omap_pin_funcmux1_update(s, ~0, s->func_mux_ctrl[1]);
        return;

    case 0x10:	/* FUNC_MUX_CTRL_3 */
    case 0x14:	/* FUNC_MUX_CTRL_4 */
    case 0x18:	/* FUNC_MUX_CTRL_5 */
    case 0x1c:	/* FUNC_MUX_CTRL_6 */
    case 0x20:	/* FUNC_MUX_CTRL_7 */
    case 0x24:	/* FUNC_MUX_CTRL_8 */
    case 0x28:	/* FUNC_MUX_CTRL_9 */
    case 0x2c:	/* FUNC_MUX_CTRL_A */
    case 0x30:	/* FUNC_MUX_CTRL_B */
    case 0x34:	/* FUNC_MUX_CTRL_C */
    case 0x38:	/* FUNC_MUX_CTRL_D */
        s->func_mux_ctrl[(addr >> 2) - 1] = value;
        return;

    case 0x40:	/* PULL_DWN_CTRL_0 */
    case 0x44:	/* PULL_DWN_CTRL_1 */
    case 0x48:	/* PULL_DWN_CTRL_2 */
    case 0x4c:	/* PULL_DWN_CTRL_3 */
        s->pull_dwn_ctrl[(addr & 0xf) >> 2] = value;
        return;

    case 0x50:	/* GATE_INH_CTRL_0 */
        s->gate_inh_ctrl[0] = value;
        return;

    case 0x60:	/* VOLTAGE_CTRL_0 */
        s->voltage_ctrl[0] = value;
        return;

    case 0x70:	/* TEST_DBG_CTRL_0 */
        s->test_dbg_ctrl[0] = value;
        return;

    case 0x80:	/* MOD_CONF_CTRL_0 */
        diff = s->mod_conf_ctrl[0] ^ value;
        s->mod_conf_ctrl[0] = value;
        omap_pin_modconf1_update(s, diff, value);
        return;

    default:
        OMAP_BAD_REG(addr);
    }
}

static CPUReadMemoryFunc *omap_pin_cfg_readfn[] = {
    omap_badwidth_read32,
    omap_badwidth_read32,
    omap_pin_cfg_read,
};

static CPUWriteMemoryFunc *omap_pin_cfg_writefn[] = {
    omap_badwidth_write32,
    omap_badwidth_write32,
    omap_pin_cfg_write,
};

static void omap_pin_cfg_reset(struct omap_mpu_state_s *mpu)
{
    /* Start in Compatibility Mode.  */
    mpu->compat1509 = 1;
    omap_pin_funcmux0_update(mpu, mpu->func_mux_ctrl[0], 0);
    omap_pin_funcmux1_update(mpu, mpu->func_mux_ctrl[1], 0);
    omap_pin_modconf1_update(mpu, mpu->mod_conf_ctrl[0], 0);
    memset(mpu->func_mux_ctrl, 0, sizeof(mpu->func_mux_ctrl));
    memset(mpu->comp_mode_ctrl, 0, sizeof(mpu->comp_mode_ctrl));
    memset(mpu->pull_dwn_ctrl, 0, sizeof(mpu->pull_dwn_ctrl));
    memset(mpu->gate_inh_ctrl, 0, sizeof(mpu->gate_inh_ctrl));
    memset(mpu->voltage_ctrl, 0, sizeof(mpu->voltage_ctrl));
    memset(mpu->test_dbg_ctrl, 0, sizeof(mpu->test_dbg_ctrl));
    memset(mpu->mod_conf_ctrl, 0, sizeof(mpu->mod_conf_ctrl));
}

static void omap_pin_cfg_init(target_phys_addr_t base,
                struct omap_mpu_state_s *mpu)
{
    int iomemtype = cpu_register_io_memory(0, omap_pin_cfg_readfn,
                    omap_pin_cfg_writefn, mpu);

    cpu_register_physical_memory(base, 0x800, iomemtype);
    omap_pin_cfg_reset(mpu);
}

/* Device Identification, Die Identification */
static uint32_t omap_id_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;

    switch (addr) {
    case 0xfffe1800:	/* DIE_ID_LSB */
        return 0xc9581f0e;
    case 0xfffe1804:	/* DIE_ID_MSB */
        return 0xa8858bfa;

    case 0xfffe2000:	/* PRODUCT_ID_LSB */
        return 0x00aaaafc;
    case 0xfffe2004:	/* PRODUCT_ID_MSB */
        return 0xcafeb574;

    case 0xfffed400:	/* JTAG_ID_LSB */
        switch (s->mpu_model) {
        case omap310:
            return 0x03310315;
        case omap1510:
            return 0x03310115;
        default:
            cpu_abort(cpu_single_env, "%s: bad mpu model\n", __FUNCTION__);
        }
        break;

    case 0xfffed404:	/* JTAG_ID_MSB */
        switch (s->mpu_model) {
        case omap310:
            return 0xfb57402f;
        case omap1510:
            return 0xfb47002f;
        default:
            cpu_abort(cpu_single_env, "%s: bad mpu model\n", __FUNCTION__);
        }
        break;
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_id_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    OMAP_BAD_REG(addr);
}

static CPUReadMemoryFunc *omap_id_readfn[] = {
    omap_badwidth_read32,
    omap_badwidth_read32,
    omap_id_read,
};

static CPUWriteMemoryFunc *omap_id_writefn[] = {
    omap_badwidth_write32,
    omap_badwidth_write32,
    omap_id_write,
};

static void omap_id_init(struct omap_mpu_state_s *mpu)
{
    int iomemtype = cpu_register_io_memory(0, omap_id_readfn,
                    omap_id_writefn, mpu);
    cpu_register_physical_memory_offset(0xfffe1800, 0x800, iomemtype, 0xfffe1800);
    cpu_register_physical_memory_offset(0xfffed400, 0x100, iomemtype, 0xfffed400);
    if (!cpu_is_omap15xx(mpu))
        cpu_register_physical_memory_offset(0xfffe2000, 0x800, iomemtype, 0xfffe2000);
}

/* MPUI Control (Dummy) */
static uint32_t omap_mpui_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;

    switch (addr) {
    case 0x00:	/* CTRL */
        return s->mpui_ctrl;
    case 0x04:	/* DEBUG_ADDR */
        return 0x01ffffff;
    case 0x08:	/* DEBUG_DATA */
        return 0xffffffff;
    case 0x0c:	/* DEBUG_FLAG */
        return 0x00000800;
    case 0x10:	/* STATUS */
        return 0x00000000;

    /* Not in OMAP310 */
    case 0x14:	/* DSP_STATUS */
    case 0x18:	/* DSP_BOOT_CONFIG */
        return 0x00000000;
    case 0x1c:	/* DSP_MPUI_CONFIG */
        return 0x0000ffff;
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_mpui_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;

    switch (addr) {
    case 0x00:	/* CTRL */
        s->mpui_ctrl = value & 0x007fffff;
        break;

    case 0x04:	/* DEBUG_ADDR */
    case 0x08:	/* DEBUG_DATA */
    case 0x0c:	/* DEBUG_FLAG */
    case 0x10:	/* STATUS */
    /* Not in OMAP310 */
    case 0x14:	/* DSP_STATUS */
        OMAP_RO_REG(addr);
    case 0x18:	/* DSP_BOOT_CONFIG */
    case 0x1c:	/* DSP_MPUI_CONFIG */
        break;

    default:
        OMAP_BAD_REG(addr);
    }
}

static CPUReadMemoryFunc *omap_mpui_readfn[] = {
    omap_badwidth_read32,
    omap_badwidth_read32,
    omap_mpui_read,
};

static CPUWriteMemoryFunc *omap_mpui_writefn[] = {
    omap_badwidth_write32,
    omap_badwidth_write32,
    omap_mpui_write,
};

static void omap_mpui_reset(struct omap_mpu_state_s *s)
{
    s->mpui_ctrl = 0x0003ff1b;
}

static void omap_mpui_init(target_phys_addr_t base,
                struct omap_mpu_state_s *mpu)
{
    int iomemtype = cpu_register_io_memory(0, omap_mpui_readfn,
                    omap_mpui_writefn, mpu);

    cpu_register_physical_memory(base, 0x100, iomemtype);

    omap_mpui_reset(mpu);
}

/* TIPB Bridges */
struct omap_tipb_bridge_s {
    qemu_irq abort;

    int width_intr;
    uint16_t control;
    uint16_t alloc;
    uint16_t buffer;
    uint16_t enh_control;
};

static uint32_t omap_tipb_bridge_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_tipb_bridge_s *s = (struct omap_tipb_bridge_s *) opaque;

    switch (addr) {
    case 0x00:	/* TIPB_CNTL */
        return s->control;
    case 0x04:	/* TIPB_BUS_ALLOC */
        return s->alloc;
    case 0x08:	/* MPU_TIPB_CNTL */
        return s->buffer;
    case 0x0c:	/* ENHANCED_TIPB_CNTL */
        return s->enh_control;
    case 0x10:	/* ADDRESS_DBG */
    case 0x14:	/* DATA_DEBUG_LOW */
    case 0x18:	/* DATA_DEBUG_HIGH */
        return 0xffff;
    case 0x1c:	/* DEBUG_CNTR_SIG */
        return 0x00f8;
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_tipb_bridge_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_tipb_bridge_s *s = (struct omap_tipb_bridge_s *) opaque;

    switch (addr) {
    case 0x00:	/* TIPB_CNTL */
        s->control = value & 0xffff;
        break;

    case 0x04:	/* TIPB_BUS_ALLOC */
        s->alloc = value & 0x003f;
        break;

    case 0x08:	/* MPU_TIPB_CNTL */
        s->buffer = value & 0x0003;
        break;

    case 0x0c:	/* ENHANCED_TIPB_CNTL */
        s->width_intr = !(value & 2);
        s->enh_control = value & 0x000f;
        break;

    case 0x10:	/* ADDRESS_DBG */
    case 0x14:	/* DATA_DEBUG_LOW */
    case 0x18:	/* DATA_DEBUG_HIGH */
    case 0x1c:	/* DEBUG_CNTR_SIG */
        OMAP_RO_REG(addr);
        break;

    default:
        OMAP_BAD_REG(addr);
    }
}

static CPUReadMemoryFunc *omap_tipb_bridge_readfn[] = {
    omap_badwidth_read16,
    omap_tipb_bridge_read,
    omap_tipb_bridge_read,
};

static CPUWriteMemoryFunc *omap_tipb_bridge_writefn[] = {
    omap_badwidth_write16,
    omap_tipb_bridge_write,
    omap_tipb_bridge_write,
};

static void omap_tipb_bridge_reset(struct omap_tipb_bridge_s *s)
{
    s->control = 0xffff;
    s->alloc = 0x0009;
    s->buffer = 0x0000;
    s->enh_control = 0x000f;
}

struct omap_tipb_bridge_s *omap_tipb_bridge_init(target_phys_addr_t base,
                qemu_irq abort_irq, omap_clk clk)
{
    int iomemtype;
    struct omap_tipb_bridge_s *s = (struct omap_tipb_bridge_s *)
            qemu_mallocz(sizeof(struct omap_tipb_bridge_s));

    s->abort = abort_irq;
    omap_tipb_bridge_reset(s);

    iomemtype = cpu_register_io_memory(0, omap_tipb_bridge_readfn,
                    omap_tipb_bridge_writefn, s);
    cpu_register_physical_memory(base, 0x100, iomemtype);

    return s;
}

/* Dummy Traffic Controller's Memory Interface */
static uint32_t omap_tcmi_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;
    uint32_t ret;

    switch (addr) {
    case 0x00:	/* IMIF_PRIO */
    case 0x04:	/* EMIFS_PRIO */
    case 0x08:	/* EMIFF_PRIO */
    case 0x0c:	/* EMIFS_CONFIG */
    case 0x10:	/* EMIFS_CS0_CONFIG */
    case 0x14:	/* EMIFS_CS1_CONFIG */
    case 0x18:	/* EMIFS_CS2_CONFIG */
    case 0x1c:	/* EMIFS_CS3_CONFIG */
    case 0x24:	/* EMIFF_MRS */
    case 0x28:	/* TIMEOUT1 */
    case 0x2c:	/* TIMEOUT2 */
    case 0x30:	/* TIMEOUT3 */
    case 0x3c:	/* EMIFF_SDRAM_CONFIG_2 */
    case 0x40:	/* EMIFS_CFG_DYN_WAIT */
        return s->tcmi_regs[addr >> 2];

    case 0x20:	/* EMIFF_SDRAM_CONFIG */
        ret = s->tcmi_regs[addr >> 2];
        s->tcmi_regs[addr >> 2] &= ~1; /* XXX: Clear SLRF on SDRAM access */
        /* XXX: We can try using the VGA_DIRTY flag for this */
        return ret;
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_tcmi_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;

    switch (addr) {
    case 0x00:	/* IMIF_PRIO */
    case 0x04:	/* EMIFS_PRIO */
    case 0x08:	/* EMIFF_PRIO */
    case 0x10:	/* EMIFS_CS0_CONFIG */
    case 0x14:	/* EMIFS_CS1_CONFIG */
    case 0x18:	/* EMIFS_CS2_CONFIG */
    case 0x1c:	/* EMIFS_CS3_CONFIG */
    case 0x20:	/* EMIFF_SDRAM_CONFIG */
    case 0x24:	/* EMIFF_MRS */
    case 0x28:	/* TIMEOUT1 */
    case 0x2c:	/* TIMEOUT2 */
    case 0x30:	/* TIMEOUT3 */
    case 0x3c:	/* EMIFF_SDRAM_CONFIG_2 */
    case 0x40:	/* EMIFS_CFG_DYN_WAIT */
        s->tcmi_regs[addr >> 2] = value;
        break;
    case 0x0c:	/* EMIFS_CONFIG */
        s->tcmi_regs[addr >> 2] = (value & 0xf) | (1 << 4);
        break;

    default:
        OMAP_BAD_REG(addr);
    }
}

static CPUReadMemoryFunc *omap_tcmi_readfn[] = {
    omap_badwidth_read32,
    omap_badwidth_read32,
    omap_tcmi_read,
};

static CPUWriteMemoryFunc *omap_tcmi_writefn[] = {
    omap_badwidth_write32,
    omap_badwidth_write32,
    omap_tcmi_write,
};

static void omap_tcmi_reset(struct omap_mpu_state_s *mpu)
{
    mpu->tcmi_regs[0x00 >> 2] = 0x00000000;
    mpu->tcmi_regs[0x04 >> 2] = 0x00000000;
    mpu->tcmi_regs[0x08 >> 2] = 0x00000000;
    mpu->tcmi_regs[0x0c >> 2] = 0x00000010;
    mpu->tcmi_regs[0x10 >> 2] = 0x0010fffb;
    mpu->tcmi_regs[0x14 >> 2] = 0x0010fffb;
    mpu->tcmi_regs[0x18 >> 2] = 0x0010fffb;
    mpu->tcmi_regs[0x1c >> 2] = 0x0010fffb;
    mpu->tcmi_regs[0x20 >> 2] = 0x00618800;
    mpu->tcmi_regs[0x24 >> 2] = 0x00000037;
    mpu->tcmi_regs[0x28 >> 2] = 0x00000000;
    mpu->tcmi_regs[0x2c >> 2] = 0x00000000;
    mpu->tcmi_regs[0x30 >> 2] = 0x00000000;
    mpu->tcmi_regs[0x3c >> 2] = 0x00000003;
    mpu->tcmi_regs[0x40 >> 2] = 0x00000000;
}

static void omap_tcmi_init(target_phys_addr_t base,
                struct omap_mpu_state_s *mpu)
{
    int iomemtype = cpu_register_io_memory(0, omap_tcmi_readfn,
                    omap_tcmi_writefn, mpu);

    cpu_register_physical_memory(base, 0x100, iomemtype);
    omap_tcmi_reset(mpu);
}

/* Digital phase-locked loops control */
static uint32_t omap_dpll_read(void *opaque, target_phys_addr_t addr)
{
    struct dpll_ctl_s *s = (struct dpll_ctl_s *) opaque;

    if (addr == 0x00)	/* CTL_REG */
        return s->mode;

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_dpll_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct dpll_ctl_s *s = (struct dpll_ctl_s *) opaque;
    uint16_t diff;
    static const int bypass_div[4] = { 1, 2, 4, 4 };
    int div, mult;

    if (addr == 0x00) {	/* CTL_REG */
        /* See omap_ulpd_pm_write() too */
        diff = s->mode & value;
        s->mode = value & 0x2fff;
        if (diff & (0x3ff << 2)) {
            if (value & (1 << 4)) {			/* PLL_ENABLE */
                div = ((value >> 5) & 3) + 1;		/* PLL_DIV */
                mult = MIN((value >> 7) & 0x1f, 1);	/* PLL_MULT */
            } else {
                div = bypass_div[((value >> 2) & 3)];	/* BYPASS_DIV */
                mult = 1;
            }
            omap_clk_setrate(s->dpll, div, mult);
        }

        /* Enter the desired mode.  */
        s->mode = (s->mode & 0xfffe) | ((s->mode >> 4) & 1);

        /* Act as if the lock is restored.  */
        s->mode |= 2;
    } else {
        OMAP_BAD_REG(addr);
    }
}

static CPUReadMemoryFunc *omap_dpll_readfn[] = {
    omap_badwidth_read16,
    omap_dpll_read,
    omap_badwidth_read16,
};

static CPUWriteMemoryFunc *omap_dpll_writefn[] = {
    omap_badwidth_write16,
    omap_dpll_write,
    omap_badwidth_write16,
};

static void omap_dpll_reset(struct dpll_ctl_s *s)
{
    s->mode = 0x2002;
    omap_clk_setrate(s->dpll, 1, 1);
}

static void omap_dpll_init(struct dpll_ctl_s *s, target_phys_addr_t base,
                omap_clk clk)
{
    int iomemtype = cpu_register_io_memory(0, omap_dpll_readfn,
                    omap_dpll_writefn, s);

    s->dpll = clk;
    omap_dpll_reset(s);

    cpu_register_physical_memory(base, 0x100, iomemtype);
}

/* UARTs */
struct omap_uart_s {
    target_phys_addr_t base;
    SerialState *serial; /* TODO */
    struct omap_target_agent_s *ta;
    omap_clk fclk;
    qemu_irq irq;

    uint8_t eblr;
    uint8_t syscontrol;
    uint8_t wkup;
    uint8_t cfps;
    uint8_t mdr[2];
    uint8_t scr;
    uint8_t clksel;
};

void omap_uart_reset(struct omap_uart_s *s)
{
    s->eblr = 0x00;
    s->syscontrol = 0;
    s->wkup = 0x3f;
    s->cfps = 0x69;
    s->clksel = 0;
}

struct omap_uart_s *omap_uart_init(target_phys_addr_t base,
                qemu_irq irq, omap_clk fclk, omap_clk iclk,
                qemu_irq txdma, qemu_irq rxdma, CharDriverState *chr)
{
    struct omap_uart_s *s = (struct omap_uart_s *)
            qemu_mallocz(sizeof(struct omap_uart_s));

    s->base = base;
    s->fclk = fclk;
    s->irq = irq;
    s->serial = serial_mm_init(base, 2, irq, omap_clk_getrate(fclk)/16,
                               chr ?: qemu_chr_open("null", "null", NULL), 1);

    return s;
}

static uint32_t omap_uart_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_uart_s *s = (struct omap_uart_s *) opaque;

    addr &= 0xff;
    switch (addr) {
    case 0x20:	/* MDR1 */
        return s->mdr[0];
    case 0x24:	/* MDR2 */
        return s->mdr[1];
    case 0x40:	/* SCR */
        return s->scr;
    case 0x44:	/* SSR */
        return 0x0;
    case 0x48:	/* EBLR (OMAP2) */
        return s->eblr;
    case 0x4C:	/* OSC_12M_SEL (OMAP1) */
        return s->clksel;
    case 0x50:	/* MVR */
        return 0x30;
    case 0x54:	/* SYSC (OMAP2) */
        return s->syscontrol;
    case 0x58:	/* SYSS (OMAP2) */
        return 1;
    case 0x5c:	/* WER (OMAP2) */
        return s->wkup;
    case 0x60:	/* CFPS (OMAP2) */
        return s->cfps;
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_uart_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_uart_s *s = (struct omap_uart_s *) opaque;

    addr &= 0xff;
    switch (addr) {
    case 0x20:	/* MDR1 */
        s->mdr[0] = value & 0x7f;
        break;
    case 0x24:	/* MDR2 */
        s->mdr[1] = value & 0xff;
        break;
    case 0x40:	/* SCR */
        s->scr = value & 0xff;
        break;
    case 0x48:	/* EBLR (OMAP2) */
        s->eblr = value & 0xff;
        break;
    case 0x4C:	/* OSC_12M_SEL (OMAP1) */
        s->clksel = value & 1;
        break;
    case 0x44:	/* SSR */
    case 0x50:	/* MVR */
    case 0x58:	/* SYSS (OMAP2) */
        OMAP_RO_REG(addr);
        break;
    case 0x54:	/* SYSC (OMAP2) */
        s->syscontrol = value & 0x1d;
        if (value & 2)
            omap_uart_reset(s);
        break;
    case 0x5c:	/* WER (OMAP2) */
        s->wkup = value & 0x7f;
        break;
    case 0x60:	/* CFPS (OMAP2) */
        s->cfps = value & 0xff;
        break;
    default:
        OMAP_BAD_REG(addr);
    }
}

static CPUReadMemoryFunc *omap_uart_readfn[] = {
    omap_uart_read,
    omap_uart_read,
    omap_badwidth_read8,
};

static CPUWriteMemoryFunc *omap_uart_writefn[] = {
    omap_uart_write,
    omap_uart_write,
    omap_badwidth_write8,
};

struct omap_uart_s *omap2_uart_init(struct omap_target_agent_s *ta,
                qemu_irq irq, omap_clk fclk, omap_clk iclk,
                qemu_irq txdma, qemu_irq rxdma, CharDriverState *chr)
{
    target_phys_addr_t base = omap_l4_attach(ta, 0, 0);
    struct omap_uart_s *s = omap_uart_init(base, irq,
                    fclk, iclk, txdma, rxdma, chr);
    int iomemtype = cpu_register_io_memory(0, omap_uart_readfn,
                    omap_uart_writefn, s);

    s->ta = ta;

    cpu_register_physical_memory(base + 0x20, 0x100, iomemtype);

    return s;
}

void omap_uart_attach(struct omap_uart_s *s, CharDriverState *chr)
{
    /* TODO: Should reuse or destroy current s->serial */
    s->serial = serial_mm_init(s->base, 2, s->irq,
                    omap_clk_getrate(s->fclk) / 16,
                    chr ?: qemu_chr_open("null", "null", NULL), 1);
}

/* MPU Clock/Reset/Power Mode Control */
static uint32_t omap_clkm_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;

    switch (addr) {
    case 0x00:	/* ARM_CKCTL */
        return s->clkm.arm_ckctl;

    case 0x04:	/* ARM_IDLECT1 */
        return s->clkm.arm_idlect1;

    case 0x08:	/* ARM_IDLECT2 */
        return s->clkm.arm_idlect2;

    case 0x0c:	/* ARM_EWUPCT */
        return s->clkm.arm_ewupct;

    case 0x10:	/* ARM_RSTCT1 */
        return s->clkm.arm_rstct1;

    case 0x14:	/* ARM_RSTCT2 */
        return s->clkm.arm_rstct2;

    case 0x18:	/* ARM_SYSST */
        return (s->clkm.clocking_scheme << 11) | s->clkm.cold_start;

    case 0x1c:	/* ARM_CKOUT1 */
        return s->clkm.arm_ckout1;

    case 0x20:	/* ARM_CKOUT2 */
        break;
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static inline void omap_clkm_ckctl_update(struct omap_mpu_state_s *s,
                uint16_t diff, uint16_t value)
{
    omap_clk clk;

    if (diff & (1 << 14)) {				/* ARM_INTHCK_SEL */
        if (value & (1 << 14))
            /* Reserved */;
        else {
            clk = omap_findclk(s, "arminth_ck");
            omap_clk_reparent(clk, omap_findclk(s, "tc_ck"));
        }
    }
    if (diff & (1 << 12)) {				/* ARM_TIMXO */
        clk = omap_findclk(s, "armtim_ck");
        if (value & (1 << 12))
            omap_clk_reparent(clk, omap_findclk(s, "clkin"));
        else
            omap_clk_reparent(clk, omap_findclk(s, "ck_gen1"));
    }
    /* XXX: en_dspck */
    if (diff & (3 << 10)) {				/* DSPMMUDIV */
        clk = omap_findclk(s, "dspmmu_ck");
        omap_clk_setrate(clk, 1 << ((value >> 10) & 3), 1);
    }
    if (diff & (3 << 8)) {				/* TCDIV */
        clk = omap_findclk(s, "tc_ck");
        omap_clk_setrate(clk, 1 << ((value >> 8) & 3), 1);
    }
    if (diff & (3 << 6)) {				/* DSPDIV */
        clk = omap_findclk(s, "dsp_ck");
        omap_clk_setrate(clk, 1 << ((value >> 6) & 3), 1);
    }
    if (diff & (3 << 4)) {				/* ARMDIV */
        clk = omap_findclk(s, "arm_ck");
        omap_clk_setrate(clk, 1 << ((value >> 4) & 3), 1);
    }
    if (diff & (3 << 2)) {				/* LCDDIV */
        clk = omap_findclk(s, "lcd_ck");
        omap_clk_setrate(clk, 1 << ((value >> 2) & 3), 1);
    }
    if (diff & (3 << 0)) {				/* PERDIV */
        clk = omap_findclk(s, "armper_ck");
        omap_clk_setrate(clk, 1 << ((value >> 0) & 3), 1);
    }
}

static inline void omap_clkm_idlect1_update(struct omap_mpu_state_s *s,
                uint16_t diff, uint16_t value)
{
    omap_clk clk;

    if (value & (1 << 11))				/* SETARM_IDLE */
        cpu_interrupt(s->env, CPU_INTERRUPT_HALT);
    if (!(value & (1 << 10)))				/* WKUP_MODE */
        qemu_system_shutdown_request();	/* XXX: disable wakeup from IRQ */

#define SET_CANIDLE(clock, bit)				\
    if (diff & (1 << bit)) {				\
        clk = omap_findclk(s, clock);			\
        omap_clk_canidle(clk, (value >> bit) & 1);	\
    }
    SET_CANIDLE("mpuwd_ck", 0)				/* IDLWDT_ARM */
    SET_CANIDLE("armxor_ck", 1)				/* IDLXORP_ARM */
    SET_CANIDLE("mpuper_ck", 2)				/* IDLPER_ARM */
    SET_CANIDLE("lcd_ck", 3)				/* IDLLCD_ARM */
    SET_CANIDLE("lb_ck", 4)				/* IDLLB_ARM */
    SET_CANIDLE("hsab_ck", 5)				/* IDLHSAB_ARM */
    SET_CANIDLE("tipb_ck", 6)				/* IDLIF_ARM */
    SET_CANIDLE("dma_ck", 6)				/* IDLIF_ARM */
    SET_CANIDLE("tc_ck", 6)				/* IDLIF_ARM */
    SET_CANIDLE("dpll1", 7)				/* IDLDPLL_ARM */
    SET_CANIDLE("dpll2", 7)				/* IDLDPLL_ARM */
    SET_CANIDLE("dpll3", 7)				/* IDLDPLL_ARM */
    SET_CANIDLE("mpui_ck", 8)				/* IDLAPI_ARM */
    SET_CANIDLE("armtim_ck", 9)				/* IDLTIM_ARM */
}

static inline void omap_clkm_idlect2_update(struct omap_mpu_state_s *s,
                uint16_t diff, uint16_t value)
{
    omap_clk clk;

#define SET_ONOFF(clock, bit)				\
    if (diff & (1 << bit)) {				\
        clk = omap_findclk(s, clock);			\
        omap_clk_onoff(clk, (value >> bit) & 1);	\
    }
    SET_ONOFF("mpuwd_ck", 0)				/* EN_WDTCK */
    SET_ONOFF("armxor_ck", 1)				/* EN_XORPCK */
    SET_ONOFF("mpuper_ck", 2)				/* EN_PERCK */
    SET_ONOFF("lcd_ck", 3)				/* EN_LCDCK */
    SET_ONOFF("lb_ck", 4)				/* EN_LBCK */
    SET_ONOFF("hsab_ck", 5)				/* EN_HSABCK */
    SET_ONOFF("mpui_ck", 6)				/* EN_APICK */
    SET_ONOFF("armtim_ck", 7)				/* EN_TIMCK */
    SET_CANIDLE("dma_ck", 8)				/* DMACK_REQ */
    SET_ONOFF("arm_gpio_ck", 9)				/* EN_GPIOCK */
    SET_ONOFF("lbfree_ck", 10)				/* EN_LBFREECK */
}

static inline void omap_clkm_ckout1_update(struct omap_mpu_state_s *s,
                uint16_t diff, uint16_t value)
{
    omap_clk clk;

    if (diff & (3 << 4)) {				/* TCLKOUT */
        clk = omap_findclk(s, "tclk_out");
        switch ((value >> 4) & 3) {
        case 1:
            omap_clk_reparent(clk, omap_findclk(s, "ck_gen3"));
            omap_clk_onoff(clk, 1);
            break;
        case 2:
            omap_clk_reparent(clk, omap_findclk(s, "tc_ck"));
            omap_clk_onoff(clk, 1);
            break;
        default:
            omap_clk_onoff(clk, 0);
        }
    }
    if (diff & (3 << 2)) {				/* DCLKOUT */
        clk = omap_findclk(s, "dclk_out");
        switch ((value >> 2) & 3) {
        case 0:
            omap_clk_reparent(clk, omap_findclk(s, "dspmmu_ck"));
            break;
        case 1:
            omap_clk_reparent(clk, omap_findclk(s, "ck_gen2"));
            break;
        case 2:
            omap_clk_reparent(clk, omap_findclk(s, "dsp_ck"));
            break;
        case 3:
            omap_clk_reparent(clk, omap_findclk(s, "ck_ref14"));
            break;
        }
    }
    if (diff & (3 << 0)) {				/* ACLKOUT */
        clk = omap_findclk(s, "aclk_out");
        switch ((value >> 0) & 3) {
        case 1:
            omap_clk_reparent(clk, omap_findclk(s, "ck_gen1"));
            omap_clk_onoff(clk, 1);
            break;
        case 2:
            omap_clk_reparent(clk, omap_findclk(s, "arm_ck"));
            omap_clk_onoff(clk, 1);
            break;
        case 3:
            omap_clk_reparent(clk, omap_findclk(s, "ck_ref14"));
            omap_clk_onoff(clk, 1);
            break;
        default:
            omap_clk_onoff(clk, 0);
        }
    }
}

static void omap_clkm_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;
    uint16_t diff;
    omap_clk clk;
    static const char *clkschemename[8] = {
        "fully synchronous", "fully asynchronous", "synchronous scalable",
        "mix mode 1", "mix mode 2", "bypass mode", "mix mode 3", "mix mode 4",
    };

    switch (addr) {
    case 0x00:	/* ARM_CKCTL */
        diff = s->clkm.arm_ckctl ^ value;
        s->clkm.arm_ckctl = value & 0x7fff;
        omap_clkm_ckctl_update(s, diff, value);
        return;

    case 0x04:	/* ARM_IDLECT1 */
        diff = s->clkm.arm_idlect1 ^ value;
        s->clkm.arm_idlect1 = value & 0x0fff;
        omap_clkm_idlect1_update(s, diff, value);
        return;

    case 0x08:	/* ARM_IDLECT2 */
        diff = s->clkm.arm_idlect2 ^ value;
        s->clkm.arm_idlect2 = value & 0x07ff;
        omap_clkm_idlect2_update(s, diff, value);
        return;

    case 0x0c:	/* ARM_EWUPCT */
        diff = s->clkm.arm_ewupct ^ value;
        s->clkm.arm_ewupct = value & 0x003f;
        return;

    case 0x10:	/* ARM_RSTCT1 */
        diff = s->clkm.arm_rstct1 ^ value;
        s->clkm.arm_rstct1 = value & 0x0007;
        if (value & 9) {
            qemu_system_reset_request();
            s->clkm.cold_start = 0xa;
        }
        if (diff & ~value & 4) {				/* DSP_RST */
            omap_mpui_reset(s);
            omap_tipb_bridge_reset(s->private_tipb);
            omap_tipb_bridge_reset(s->public_tipb);
        }
        if (diff & 2) {						/* DSP_EN */
            clk = omap_findclk(s, "dsp_ck");
            omap_clk_canidle(clk, (~value >> 1) & 1);
        }
        return;

    case 0x14:	/* ARM_RSTCT2 */
        s->clkm.arm_rstct2 = value & 0x0001;
        return;

    case 0x18:	/* ARM_SYSST */
        if ((s->clkm.clocking_scheme ^ (value >> 11)) & 7) {
            s->clkm.clocking_scheme = (value >> 11) & 7;
            printf("%s: clocking scheme set to %s\n", __FUNCTION__,
                            clkschemename[s->clkm.clocking_scheme]);
        }
        s->clkm.cold_start &= value & 0x3f;
        return;

    case 0x1c:	/* ARM_CKOUT1 */
        diff = s->clkm.arm_ckout1 ^ value;
        s->clkm.arm_ckout1 = value & 0x003f;
        omap_clkm_ckout1_update(s, diff, value);
        return;

    case 0x20:	/* ARM_CKOUT2 */
    default:
        OMAP_BAD_REG(addr);
    }
}

static CPUReadMemoryFunc *omap_clkm_readfn[] = {
    omap_badwidth_read16,
    omap_clkm_read,
    omap_badwidth_read16,
};

static CPUWriteMemoryFunc *omap_clkm_writefn[] = {
    omap_badwidth_write16,
    omap_clkm_write,
    omap_badwidth_write16,
};

static uint32_t omap_clkdsp_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;

    switch (addr) {
    case 0x04:	/* DSP_IDLECT1 */
        return s->clkm.dsp_idlect1;

    case 0x08:	/* DSP_IDLECT2 */
        return s->clkm.dsp_idlect2;

    case 0x14:	/* DSP_RSTCT2 */
        return s->clkm.dsp_rstct2;

    case 0x18:	/* DSP_SYSST */
        return (s->clkm.clocking_scheme << 11) | s->clkm.cold_start |
                (s->env->halted << 6);	/* Quite useless... */
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static inline void omap_clkdsp_idlect1_update(struct omap_mpu_state_s *s,
                uint16_t diff, uint16_t value)
{
    omap_clk clk;

    SET_CANIDLE("dspxor_ck", 1);			/* IDLXORP_DSP */
}

static inline void omap_clkdsp_idlect2_update(struct omap_mpu_state_s *s,
                uint16_t diff, uint16_t value)
{
    omap_clk clk;

    SET_ONOFF("dspxor_ck", 1);				/* EN_XORPCK */
}

static void omap_clkdsp_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;
    uint16_t diff;

    switch (addr) {
    case 0x04:	/* DSP_IDLECT1 */
        diff = s->clkm.dsp_idlect1 ^ value;
        s->clkm.dsp_idlect1 = value & 0x01f7;
        omap_clkdsp_idlect1_update(s, diff, value);
        break;

    case 0x08:	/* DSP_IDLECT2 */
        s->clkm.dsp_idlect2 = value & 0x0037;
        diff = s->clkm.dsp_idlect1 ^ value;
        omap_clkdsp_idlect2_update(s, diff, value);
        break;

    case 0x14:	/* DSP_RSTCT2 */
        s->clkm.dsp_rstct2 = value & 0x0001;
        break;

    case 0x18:	/* DSP_SYSST */
        s->clkm.cold_start &= value & 0x3f;
        break;

    default:
        OMAP_BAD_REG(addr);
    }
}

static CPUReadMemoryFunc *omap_clkdsp_readfn[] = {
    omap_badwidth_read16,
    omap_clkdsp_read,
    omap_badwidth_read16,
};

static CPUWriteMemoryFunc *omap_clkdsp_writefn[] = {
    omap_badwidth_write16,
    omap_clkdsp_write,
    omap_badwidth_write16,
};

static void omap_clkm_reset(struct omap_mpu_state_s *s)
{
    if (s->wdt && s->wdt->reset)
        s->clkm.cold_start = 0x6;
    s->clkm.clocking_scheme = 0;
    omap_clkm_ckctl_update(s, ~0, 0x3000);
    s->clkm.arm_ckctl = 0x3000;
    omap_clkm_idlect1_update(s, s->clkm.arm_idlect1 ^ 0x0400, 0x0400);
    s->clkm.arm_idlect1 = 0x0400;
    omap_clkm_idlect2_update(s, s->clkm.arm_idlect2 ^ 0x0100, 0x0100);
    s->clkm.arm_idlect2 = 0x0100;
    s->clkm.arm_ewupct = 0x003f;
    s->clkm.arm_rstct1 = 0x0000;
    s->clkm.arm_rstct2 = 0x0000;
    s->clkm.arm_ckout1 = 0x0015;
    s->clkm.dpll1_mode = 0x2002;
    omap_clkdsp_idlect1_update(s, s->clkm.dsp_idlect1 ^ 0x0040, 0x0040);
    s->clkm.dsp_idlect1 = 0x0040;
    omap_clkdsp_idlect2_update(s, ~0, 0x0000);
    s->clkm.dsp_idlect2 = 0x0000;
    s->clkm.dsp_rstct2 = 0x0000;
}

static void omap_clkm_init(target_phys_addr_t mpu_base,
                target_phys_addr_t dsp_base, struct omap_mpu_state_s *s)
{
    int iomemtype[2] = {
        cpu_register_io_memory(0, omap_clkm_readfn, omap_clkm_writefn, s),
        cpu_register_io_memory(0, omap_clkdsp_readfn, omap_clkdsp_writefn, s),
    };

    s->clkm.arm_idlect1 = 0x03ff;
    s->clkm.arm_idlect2 = 0x0100;
    s->clkm.dsp_idlect1 = 0x0002;
    omap_clkm_reset(s);
    s->clkm.cold_start = 0x3a;

    cpu_register_physical_memory(mpu_base, 0x100, iomemtype[0]);
    cpu_register_physical_memory(dsp_base, 0x1000, iomemtype[1]);
}

/* MPU I/O */
struct omap_mpuio_s {
    qemu_irq irq;
    qemu_irq kbd_irq;
    qemu_irq *in;
    qemu_irq handler[16];
    qemu_irq wakeup;

    uint16_t inputs;
    uint16_t outputs;
    uint16_t dir;
    uint16_t edge;
    uint16_t mask;
    uint16_t ints;

    uint16_t debounce;
    uint16_t latch;
    uint8_t event;

    uint8_t buttons[5];
    uint8_t row_latch;
    uint8_t cols;
    int kbd_mask;
    int clk;
};

static void omap_mpuio_set(void *opaque, int line, int level)
{
    struct omap_mpuio_s *s = (struct omap_mpuio_s *) opaque;
    uint16_t prev = s->inputs;

    if (level)
        s->inputs |= 1 << line;
    else
        s->inputs &= ~(1 << line);

    if (((1 << line) & s->dir & ~s->mask) && s->clk) {
        if ((s->edge & s->inputs & ~prev) | (~s->edge & ~s->inputs & prev)) {
            s->ints |= 1 << line;
            qemu_irq_raise(s->irq);
            /* TODO: wakeup */
        }
        if ((s->event & (1 << 0)) &&		/* SET_GPIO_EVENT_MODE */
                (s->event >> 1) == line)	/* PIN_SELECT */
            s->latch = s->inputs;
    }
}

static void omap_mpuio_kbd_update(struct omap_mpuio_s *s)
{
    int i;
    uint8_t *row, rows = 0, cols = ~s->cols;

    for (row = s->buttons + 4, i = 1 << 4; i; row --, i >>= 1)
        if (*row & cols)
            rows |= i;

    qemu_set_irq(s->kbd_irq, rows && !s->kbd_mask && s->clk);
    s->row_latch = ~rows;
}

static uint32_t omap_mpuio_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_mpuio_s *s = (struct omap_mpuio_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;
    uint16_t ret;

    switch (offset) {
    case 0x00:	/* INPUT_LATCH */
        return s->inputs;

    case 0x04:	/* OUTPUT_REG */
        return s->outputs;

    case 0x08:	/* IO_CNTL */
        return s->dir;

    case 0x10:	/* KBR_LATCH */
        return s->row_latch;

    case 0x14:	/* KBC_REG */
        return s->cols;

    case 0x18:	/* GPIO_EVENT_MODE_REG */
        return s->event;

    case 0x1c:	/* GPIO_INT_EDGE_REG */
        return s->edge;

    case 0x20:	/* KBD_INT */
        return (~s->row_latch & 0x1f) && !s->kbd_mask;

    case 0x24:	/* GPIO_INT */
        ret = s->ints;
        s->ints &= s->mask;
        if (ret)
            qemu_irq_lower(s->irq);
        return ret;

    case 0x28:	/* KBD_MASKIT */
        return s->kbd_mask;

    case 0x2c:	/* GPIO_MASKIT */
        return s->mask;

    case 0x30:	/* GPIO_DEBOUNCING_REG */
        return s->debounce;

    case 0x34:	/* GPIO_LATCH_REG */
        return s->latch;
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_mpuio_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_mpuio_s *s = (struct omap_mpuio_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;
    uint16_t diff;
    int ln;

    switch (offset) {
    case 0x04:	/* OUTPUT_REG */
        diff = (s->outputs ^ value) & ~s->dir;
        s->outputs = value;
        while ((ln = ffs(diff))) {
            ln --;
            if (s->handler[ln])
                qemu_set_irq(s->handler[ln], (value >> ln) & 1);
            diff &= ~(1 << ln);
        }
        break;

    case 0x08:	/* IO_CNTL */
        diff = s->outputs & (s->dir ^ value);
        s->dir = value;

        value = s->outputs & ~s->dir;
        while ((ln = ffs(diff))) {
            ln --;
            if (s->handler[ln])
                qemu_set_irq(s->handler[ln], (value >> ln) & 1);
            diff &= ~(1 << ln);
        }
        break;

    case 0x14:	/* KBC_REG */
        s->cols = value;
        omap_mpuio_kbd_update(s);
        break;

    case 0x18:	/* GPIO_EVENT_MODE_REG */
        s->event = value & 0x1f;
        break;

    case 0x1c:	/* GPIO_INT_EDGE_REG */
        s->edge = value;
        break;

    case 0x28:	/* KBD_MASKIT */
        s->kbd_mask = value & 1;
        omap_mpuio_kbd_update(s);
        break;

    case 0x2c:	/* GPIO_MASKIT */
        s->mask = value;
        break;

    case 0x30:	/* GPIO_DEBOUNCING_REG */
        s->debounce = value & 0x1ff;
        break;

    case 0x00:	/* INPUT_LATCH */
    case 0x10:	/* KBR_LATCH */
    case 0x20:	/* KBD_INT */
    case 0x24:	/* GPIO_INT */
    case 0x34:	/* GPIO_LATCH_REG */
        OMAP_RO_REG(addr);
        return;

    default:
        OMAP_BAD_REG(addr);
        return;
    }
}

static CPUReadMemoryFunc *omap_mpuio_readfn[] = {
    omap_badwidth_read16,
    omap_mpuio_read,
    omap_badwidth_read16,
};

static CPUWriteMemoryFunc *omap_mpuio_writefn[] = {
    omap_badwidth_write16,
    omap_mpuio_write,
    omap_badwidth_write16,
};

static void omap_mpuio_reset(struct omap_mpuio_s *s)
{
    s->inputs = 0;
    s->outputs = 0;
    s->dir = ~0;
    s->event = 0;
    s->edge = 0;
    s->kbd_mask = 0;
    s->mask = 0;
    s->debounce = 0;
    s->latch = 0;
    s->ints = 0;
    s->row_latch = 0x1f;
    s->clk = 1;
}

static void omap_mpuio_onoff(void *opaque, int line, int on)
{
    struct omap_mpuio_s *s = (struct omap_mpuio_s *) opaque;

    s->clk = on;
    if (on)
        omap_mpuio_kbd_update(s);
}

struct omap_mpuio_s *omap_mpuio_init(target_phys_addr_t base,
                qemu_irq kbd_int, qemu_irq gpio_int, qemu_irq wakeup,
                omap_clk clk)
{
    int iomemtype;
    struct omap_mpuio_s *s = (struct omap_mpuio_s *)
            qemu_mallocz(sizeof(struct omap_mpuio_s));

    s->irq = gpio_int;
    s->kbd_irq = kbd_int;
    s->wakeup = wakeup;
    s->in = qemu_allocate_irqs(omap_mpuio_set, s, 16);
    omap_mpuio_reset(s);

    iomemtype = cpu_register_io_memory(0, omap_mpuio_readfn,
                    omap_mpuio_writefn, s);
    cpu_register_physical_memory(base, 0x800, iomemtype);

    omap_clk_adduser(clk, qemu_allocate_irqs(omap_mpuio_onoff, s, 1)[0]);

    return s;
}

qemu_irq *omap_mpuio_in_get(struct omap_mpuio_s *s)
{
    return s->in;
}

void omap_mpuio_out_set(struct omap_mpuio_s *s, int line, qemu_irq handler)
{
    if (line >= 16 || line < 0)
        cpu_abort(cpu_single_env, "%s: No GPIO line %i\n", __FUNCTION__, line);
    s->handler[line] = handler;
}

void omap_mpuio_key(struct omap_mpuio_s *s, int row, int col, int down)
{
    if (row >= 5 || row < 0)
        cpu_abort(cpu_single_env, "%s: No key %i-%i\n",
                        __FUNCTION__, col, row);

    if (down)
        s->buttons[row] |= 1 << col;
    else
        s->buttons[row] &= ~(1 << col);

    omap_mpuio_kbd_update(s);
}

/* General-Purpose I/O */
struct omap_gpio_s {
    qemu_irq irq;
    qemu_irq *in;
    qemu_irq handler[16];

    uint16_t inputs;
    uint16_t outputs;
    uint16_t dir;
    uint16_t edge;
    uint16_t mask;
    uint16_t ints;
    uint16_t pins;
};

static void omap_gpio_set(void *opaque, int line, int level)
{
    struct omap_gpio_s *s = (struct omap_gpio_s *) opaque;
    uint16_t prev = s->inputs;

    if (level)
        s->inputs |= 1 << line;
    else
        s->inputs &= ~(1 << line);

    if (((s->edge & s->inputs & ~prev) | (~s->edge & ~s->inputs & prev)) &
                    (1 << line) & s->dir & ~s->mask) {
        s->ints |= 1 << line;
        qemu_irq_raise(s->irq);
    }
}

static uint32_t omap_gpio_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_gpio_s *s = (struct omap_gpio_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    switch (offset) {
    case 0x00:	/* DATA_INPUT */
        return s->inputs & s->pins;

    case 0x04:	/* DATA_OUTPUT */
        return s->outputs;

    case 0x08:	/* DIRECTION_CONTROL */
        return s->dir;

    case 0x0c:	/* INTERRUPT_CONTROL */
        return s->edge;

    case 0x10:	/* INTERRUPT_MASK */
        return s->mask;

    case 0x14:	/* INTERRUPT_STATUS */
        return s->ints;

    case 0x18:	/* PIN_CONTROL (not in OMAP310) */
        OMAP_BAD_REG(addr);
        return s->pins;
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_gpio_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_gpio_s *s = (struct omap_gpio_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;
    uint16_t diff;
    int ln;

    switch (offset) {
    case 0x00:	/* DATA_INPUT */
        OMAP_RO_REG(addr);
        return;

    case 0x04:	/* DATA_OUTPUT */
        diff = (s->outputs ^ value) & ~s->dir;
        s->outputs = value;
        while ((ln = ffs(diff))) {
            ln --;
            if (s->handler[ln])
                qemu_set_irq(s->handler[ln], (value >> ln) & 1);
            diff &= ~(1 << ln);
        }
        break;

    case 0x08:	/* DIRECTION_CONTROL */
        diff = s->outputs & (s->dir ^ value);
        s->dir = value;

        value = s->outputs & ~s->dir;
        while ((ln = ffs(diff))) {
            ln --;
            if (s->handler[ln])
                qemu_set_irq(s->handler[ln], (value >> ln) & 1);
            diff &= ~(1 << ln);
        }
        break;

    case 0x0c:	/* INTERRUPT_CONTROL */
        s->edge = value;
        break;

    case 0x10:	/* INTERRUPT_MASK */
        s->mask = value;
        break;

    case 0x14:	/* INTERRUPT_STATUS */
        s->ints &= ~value;
        if (!s->ints)
            qemu_irq_lower(s->irq);
        break;

    case 0x18:	/* PIN_CONTROL (not in OMAP310 TRM) */
        OMAP_BAD_REG(addr);
        s->pins = value;
        break;

    default:
        OMAP_BAD_REG(addr);
        return;
    }
}

/* *Some* sources say the memory region is 32-bit.  */
static CPUReadMemoryFunc *omap_gpio_readfn[] = {
    omap_badwidth_read16,
    omap_gpio_read,
    omap_badwidth_read16,
};

static CPUWriteMemoryFunc *omap_gpio_writefn[] = {
    omap_badwidth_write16,
    omap_gpio_write,
    omap_badwidth_write16,
};

static void omap_gpio_reset(struct omap_gpio_s *s)
{
    s->inputs = 0;
    s->outputs = ~0;
    s->dir = ~0;
    s->edge = ~0;
    s->mask = ~0;
    s->ints = 0;
    s->pins = ~0;
}

struct omap_gpio_s *omap_gpio_init(target_phys_addr_t base,
                qemu_irq irq, omap_clk clk)
{
    int iomemtype;
    struct omap_gpio_s *s = (struct omap_gpio_s *)
            qemu_mallocz(sizeof(struct omap_gpio_s));

    s->irq = irq;
    s->in = qemu_allocate_irqs(omap_gpio_set, s, 16);
    omap_gpio_reset(s);

    iomemtype = cpu_register_io_memory(0, omap_gpio_readfn,
                    omap_gpio_writefn, s);
    cpu_register_physical_memory(base, 0x1000, iomemtype);

    return s;
}

qemu_irq *omap_gpio_in_get(struct omap_gpio_s *s)
{
    return s->in;
}

void omap_gpio_out_set(struct omap_gpio_s *s, int line, qemu_irq handler)
{
    if (line >= 16 || line < 0)
        cpu_abort(cpu_single_env, "%s: No GPIO line %i\n", __FUNCTION__, line);
    s->handler[line] = handler;
}

/* MicroWire Interface */
struct omap_uwire_s {
    qemu_irq txirq;
    qemu_irq rxirq;
    qemu_irq txdrq;

    uint16_t txbuf;
    uint16_t rxbuf;
    uint16_t control;
    uint16_t setup[5];

    struct uwire_slave_s *chip[4];
};

static void omap_uwire_transfer_start(struct omap_uwire_s *s)
{
    int chipselect = (s->control >> 10) & 3;		/* INDEX */
    struct uwire_slave_s *slave = s->chip[chipselect];

    if ((s->control >> 5) & 0x1f) {			/* NB_BITS_WR */
        if (s->control & (1 << 12))			/* CS_CMD */
            if (slave && slave->send)
                slave->send(slave->opaque,
                                s->txbuf >> (16 - ((s->control >> 5) & 0x1f)));
        s->control &= ~(1 << 14);			/* CSRB */
        /* TODO: depending on s->setup[4] bits [1:0] assert an IRQ or
         * a DRQ.  When is the level IRQ supposed to be reset?  */
    }

    if ((s->control >> 0) & 0x1f) {			/* NB_BITS_RD */
        if (s->control & (1 << 12))			/* CS_CMD */
            if (slave && slave->receive)
                s->rxbuf = slave->receive(slave->opaque);
        s->control |= 1 << 15;				/* RDRB */
        /* TODO: depending on s->setup[4] bits [1:0] assert an IRQ or
         * a DRQ.  When is the level IRQ supposed to be reset?  */
    }
}

static uint32_t omap_uwire_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_uwire_s *s = (struct omap_uwire_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    switch (offset) {
    case 0x00:	/* RDR */
        s->control &= ~(1 << 15);			/* RDRB */
        return s->rxbuf;

    case 0x04:	/* CSR */
        return s->control;

    case 0x08:	/* SR1 */
        return s->setup[0];
    case 0x0c:	/* SR2 */
        return s->setup[1];
    case 0x10:	/* SR3 */
        return s->setup[2];
    case 0x14:	/* SR4 */
        return s->setup[3];
    case 0x18:	/* SR5 */
        return s->setup[4];
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_uwire_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_uwire_s *s = (struct omap_uwire_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    switch (offset) {
    case 0x00:	/* TDR */
        s->txbuf = value;				/* TD */
        if ((s->setup[4] & (1 << 2)) &&			/* AUTO_TX_EN */
                        ((s->setup[4] & (1 << 3)) ||	/* CS_TOGGLE_TX_EN */
                         (s->control & (1 << 12)))) {	/* CS_CMD */
            s->control |= 1 << 14;			/* CSRB */
            omap_uwire_transfer_start(s);
        }
        break;

    case 0x04:	/* CSR */
        s->control = value & 0x1fff;
        if (value & (1 << 13))				/* START */
            omap_uwire_transfer_start(s);
        break;

    case 0x08:	/* SR1 */
        s->setup[0] = value & 0x003f;
        break;

    case 0x0c:	/* SR2 */
        s->setup[1] = value & 0x0fc0;
        break;

    case 0x10:	/* SR3 */
        s->setup[2] = value & 0x0003;
        break;

    case 0x14:	/* SR4 */
        s->setup[3] = value & 0x0001;
        break;

    case 0x18:	/* SR5 */
        s->setup[4] = value & 0x000f;
        break;

    default:
        OMAP_BAD_REG(addr);
        return;
    }
}

static CPUReadMemoryFunc *omap_uwire_readfn[] = {
    omap_badwidth_read16,
    omap_uwire_read,
    omap_badwidth_read16,
};

static CPUWriteMemoryFunc *omap_uwire_writefn[] = {
    omap_badwidth_write16,
    omap_uwire_write,
    omap_badwidth_write16,
};

static void omap_uwire_reset(struct omap_uwire_s *s)
{
    s->control = 0;
    s->setup[0] = 0;
    s->setup[1] = 0;
    s->setup[2] = 0;
    s->setup[3] = 0;
    s->setup[4] = 0;
}

struct omap_uwire_s *omap_uwire_init(target_phys_addr_t base,
                qemu_irq *irq, qemu_irq dma, omap_clk clk)
{
    int iomemtype;
    struct omap_uwire_s *s = (struct omap_uwire_s *)
            qemu_mallocz(sizeof(struct omap_uwire_s));

    s->txirq = irq[0];
    s->rxirq = irq[1];
    s->txdrq = dma;
    omap_uwire_reset(s);

    iomemtype = cpu_register_io_memory(0, omap_uwire_readfn,
                    omap_uwire_writefn, s);
    cpu_register_physical_memory(base, 0x800, iomemtype);

    return s;
}

void omap_uwire_attach(struct omap_uwire_s *s,
                struct uwire_slave_s *slave, int chipselect)
{
    if (chipselect < 0 || chipselect > 3) {
        fprintf(stderr, "%s: Bad chipselect %i\n", __FUNCTION__, chipselect);
        exit(-1);
    }

    s->chip[chipselect] = slave;
}

/* Pseudonoise Pulse-Width Light Modulator */
static void omap_pwl_update(struct omap_mpu_state_s *s)
{
    int output = (s->pwl.clk && s->pwl.enable) ? s->pwl.level : 0;

    if (output != s->pwl.output) {
        s->pwl.output = output;
        printf("%s: Backlight now at %i/256\n", __FUNCTION__, output);
    }
}

static uint32_t omap_pwl_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    switch (offset) {
    case 0x00:	/* PWL_LEVEL */
        return s->pwl.level;
    case 0x04:	/* PWL_CTRL */
        return s->pwl.enable;
    }
    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_pwl_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    switch (offset) {
    case 0x00:	/* PWL_LEVEL */
        s->pwl.level = value;
        omap_pwl_update(s);
        break;
    case 0x04:	/* PWL_CTRL */
        s->pwl.enable = value & 1;
        omap_pwl_update(s);
        break;
    default:
        OMAP_BAD_REG(addr);
        return;
    }
}

static CPUReadMemoryFunc *omap_pwl_readfn[] = {
    omap_pwl_read,
    omap_badwidth_read8,
    omap_badwidth_read8,
};

static CPUWriteMemoryFunc *omap_pwl_writefn[] = {
    omap_pwl_write,
    omap_badwidth_write8,
    omap_badwidth_write8,
};

static void omap_pwl_reset(struct omap_mpu_state_s *s)
{
    s->pwl.output = 0;
    s->pwl.level = 0;
    s->pwl.enable = 0;
    s->pwl.clk = 1;
    omap_pwl_update(s);
}

static void omap_pwl_clk_update(void *opaque, int line, int on)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;

    s->pwl.clk = on;
    omap_pwl_update(s);
}

static void omap_pwl_init(target_phys_addr_t base, struct omap_mpu_state_s *s,
                omap_clk clk)
{
    int iomemtype;

    omap_pwl_reset(s);

    iomemtype = cpu_register_io_memory(0, omap_pwl_readfn,
                    omap_pwl_writefn, s);
    cpu_register_physical_memory(base, 0x800, iomemtype);

    omap_clk_adduser(clk, qemu_allocate_irqs(omap_pwl_clk_update, s, 1)[0]);
}

/* Pulse-Width Tone module */
static uint32_t omap_pwt_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    switch (offset) {
    case 0x00:	/* FRC */
        return s->pwt.frc;
    case 0x04:	/* VCR */
        return s->pwt.vrc;
    case 0x08:	/* GCR */
        return s->pwt.gcr;
    }
    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_pwt_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    switch (offset) {
    case 0x00:	/* FRC */
        s->pwt.frc = value & 0x3f;
        break;
    case 0x04:	/* VRC */
        if ((value ^ s->pwt.vrc) & 1) {
            if (value & 1)
                printf("%s: %iHz buzz on\n", __FUNCTION__, (int)
                                /* 1.5 MHz from a 12-MHz or 13-MHz PWT_CLK */
                                ((omap_clk_getrate(s->pwt.clk) >> 3) /
                                 /* Pre-multiplexer divider */
                                 ((s->pwt.gcr & 2) ? 1 : 154) /
                                 /* Octave multiplexer */
                                 (2 << (value & 3)) *
                                 /* 101/107 divider */
                                 ((value & (1 << 2)) ? 101 : 107) *
                                 /*  49/55 divider */
                                 ((value & (1 << 3)) ?  49 : 55) *
                                 /*  50/63 divider */
                                 ((value & (1 << 4)) ?  50 : 63) *
                                 /*  80/127 divider */
                                 ((value & (1 << 5)) ?  80 : 127) /
                                 (107 * 55 * 63 * 127)));
            else
                printf("%s: silence!\n", __FUNCTION__);
        }
        s->pwt.vrc = value & 0x7f;
        break;
    case 0x08:	/* GCR */
        s->pwt.gcr = value & 3;
        break;
    default:
        OMAP_BAD_REG(addr);
        return;
    }
}

static CPUReadMemoryFunc *omap_pwt_readfn[] = {
    omap_pwt_read,
    omap_badwidth_read8,
    omap_badwidth_read8,
};

static CPUWriteMemoryFunc *omap_pwt_writefn[] = {
    omap_pwt_write,
    omap_badwidth_write8,
    omap_badwidth_write8,
};

static void omap_pwt_reset(struct omap_mpu_state_s *s)
{
    s->pwt.frc = 0;
    s->pwt.vrc = 0;
    s->pwt.gcr = 0;
}

static void omap_pwt_init(target_phys_addr_t base, struct omap_mpu_state_s *s,
                omap_clk clk)
{
    int iomemtype;

    s->pwt.clk = clk;
    omap_pwt_reset(s);

    iomemtype = cpu_register_io_memory(0, omap_pwt_readfn,
                    omap_pwt_writefn, s);
    cpu_register_physical_memory(base, 0x800, iomemtype);
}

/* Real-time Clock module */
struct omap_rtc_s {
    qemu_irq irq;
    qemu_irq alarm;
    QEMUTimer *clk;

    uint8_t interrupts;
    uint8_t status;
    int16_t comp_reg;
    int running;
    int pm_am;
    int auto_comp;
    int round;
    struct tm alarm_tm;
    time_t alarm_ti;

    struct tm current_tm;
    time_t ti;
    uint64_t tick;
};

static void omap_rtc_interrupts_update(struct omap_rtc_s *s)
{
    /* s->alarm is level-triggered */
    qemu_set_irq(s->alarm, (s->status >> 6) & 1);
}

static void omap_rtc_alarm_update(struct omap_rtc_s *s)
{
    s->alarm_ti = mktimegm(&s->alarm_tm);
    if (s->alarm_ti == -1)
        printf("%s: conversion failed\n", __FUNCTION__);
}

static inline uint8_t omap_rtc_bcd(int num)
{
    return ((num / 10) << 4) | (num % 10);
}

static inline int omap_rtc_bin(uint8_t num)
{
    return (num & 15) + 10 * (num >> 4);
}

static uint32_t omap_rtc_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_rtc_s *s = (struct omap_rtc_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;
    uint8_t i;

    switch (offset) {
    case 0x00:	/* SECONDS_REG */
        return omap_rtc_bcd(s->current_tm.tm_sec);

    case 0x04:	/* MINUTES_REG */
        return omap_rtc_bcd(s->current_tm.tm_min);

    case 0x08:	/* HOURS_REG */
        if (s->pm_am)
            return ((s->current_tm.tm_hour > 11) << 7) |
                    omap_rtc_bcd(((s->current_tm.tm_hour - 1) % 12) + 1);
        else
            return omap_rtc_bcd(s->current_tm.tm_hour);

    case 0x0c:	/* DAYS_REG */
        return omap_rtc_bcd(s->current_tm.tm_mday);

    case 0x10:	/* MONTHS_REG */
        return omap_rtc_bcd(s->current_tm.tm_mon + 1);

    case 0x14:	/* YEARS_REG */
        return omap_rtc_bcd(s->current_tm.tm_year % 100);

    case 0x18:	/* WEEK_REG */
        return s->current_tm.tm_wday;

    case 0x20:	/* ALARM_SECONDS_REG */
        return omap_rtc_bcd(s->alarm_tm.tm_sec);

    case 0x24:	/* ALARM_MINUTES_REG */
        return omap_rtc_bcd(s->alarm_tm.tm_min);

    case 0x28:	/* ALARM_HOURS_REG */
        if (s->pm_am)
            return ((s->alarm_tm.tm_hour > 11) << 7) |
                    omap_rtc_bcd(((s->alarm_tm.tm_hour - 1) % 12) + 1);
        else
            return omap_rtc_bcd(s->alarm_tm.tm_hour);

    case 0x2c:	/* ALARM_DAYS_REG */
        return omap_rtc_bcd(s->alarm_tm.tm_mday);

    case 0x30:	/* ALARM_MONTHS_REG */
        return omap_rtc_bcd(s->alarm_tm.tm_mon + 1);

    case 0x34:	/* ALARM_YEARS_REG */
        return omap_rtc_bcd(s->alarm_tm.tm_year % 100);

    case 0x40:	/* RTC_CTRL_REG */
        return (s->pm_am << 3) | (s->auto_comp << 2) |
                (s->round << 1) | s->running;

    case 0x44:	/* RTC_STATUS_REG */
        i = s->status;
        s->status &= ~0x3d;
        return i;

    case 0x48:	/* RTC_INTERRUPTS_REG */
        return s->interrupts;

    case 0x4c:	/* RTC_COMP_LSB_REG */
        return ((uint16_t) s->comp_reg) & 0xff;

    case 0x50:	/* RTC_COMP_MSB_REG */
        return ((uint16_t) s->comp_reg) >> 8;
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_rtc_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_rtc_s *s = (struct omap_rtc_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;
    struct tm new_tm;
    time_t ti[2];

    switch (offset) {
    case 0x00:	/* SECONDS_REG */
#ifdef ALMDEBUG
        printf("RTC SEC_REG <-- %02x\n", value);
#endif
        s->ti -= s->current_tm.tm_sec;
        s->ti += omap_rtc_bin(value);
        return;

    case 0x04:	/* MINUTES_REG */
#ifdef ALMDEBUG
        printf("RTC MIN_REG <-- %02x\n", value);
#endif
        s->ti -= s->current_tm.tm_min * 60;
        s->ti += omap_rtc_bin(value) * 60;
        return;

    case 0x08:	/* HOURS_REG */
#ifdef ALMDEBUG
        printf("RTC HRS_REG <-- %02x\n", value);
#endif
        s->ti -= s->current_tm.tm_hour * 3600;
        if (s->pm_am) {
            s->ti += (omap_rtc_bin(value & 0x3f) & 12) * 3600;
            s->ti += ((value >> 7) & 1) * 43200;
        } else
            s->ti += omap_rtc_bin(value & 0x3f) * 3600;
        return;

    case 0x0c:	/* DAYS_REG */
#ifdef ALMDEBUG
        printf("RTC DAY_REG <-- %02x\n", value);
#endif
        s->ti -= s->current_tm.tm_mday * 86400;
        s->ti += omap_rtc_bin(value) * 86400;
        return;

    case 0x10:	/* MONTHS_REG */
#ifdef ALMDEBUG
        printf("RTC MTH_REG <-- %02x\n", value);
#endif
        memcpy(&new_tm, &s->current_tm, sizeof(new_tm));
        new_tm.tm_mon = omap_rtc_bin(value);
        ti[0] = mktimegm(&s->current_tm);
        ti[1] = mktimegm(&new_tm);

        if (ti[0] != -1 && ti[1] != -1) {
            s->ti -= ti[0];
            s->ti += ti[1];
        } else {
            /* A less accurate version */
            s->ti -= s->current_tm.tm_mon * 2592000;
            s->ti += omap_rtc_bin(value) * 2592000;
        }
        return;

    case 0x14:	/* YEARS_REG */
#ifdef ALMDEBUG
        printf("RTC YRS_REG <-- %02x\n", value);
#endif
        memcpy(&new_tm, &s->current_tm, sizeof(new_tm));
        new_tm.tm_year += omap_rtc_bin(value) - (new_tm.tm_year % 100);
        ti[0] = mktimegm(&s->current_tm);
        ti[1] = mktimegm(&new_tm);

        if (ti[0] != -1 && ti[1] != -1) {
            s->ti -= ti[0];
            s->ti += ti[1];
        } else {
            /* A less accurate version */
            s->ti -= (s->current_tm.tm_year % 100) * 31536000;
            s->ti += omap_rtc_bin(value) * 31536000;
        }
        return;

    case 0x18:	/* WEEK_REG */
        return;	/* Ignored */

    case 0x20:	/* ALARM_SECONDS_REG */
#ifdef ALMDEBUG
        printf("ALM SEC_REG <-- %02x\n", value);
#endif
        s->alarm_tm.tm_sec = omap_rtc_bin(value);
        omap_rtc_alarm_update(s);
        return;

    case 0x24:	/* ALARM_MINUTES_REG */
#ifdef ALMDEBUG
        printf("ALM MIN_REG <-- %02x\n", value);
#endif
        s->alarm_tm.tm_min = omap_rtc_bin(value);
        omap_rtc_alarm_update(s);
        return;

    case 0x28:	/* ALARM_HOURS_REG */
#ifdef ALMDEBUG
        printf("ALM HRS_REG <-- %02x\n", value);
#endif
        if (s->pm_am)
            s->alarm_tm.tm_hour =
                    ((omap_rtc_bin(value & 0x3f)) % 12) +
                    ((value >> 7) & 1) * 12;
        else
            s->alarm_tm.tm_hour = omap_rtc_bin(value);
        omap_rtc_alarm_update(s);
        return;

    case 0x2c:	/* ALARM_DAYS_REG */
#ifdef ALMDEBUG
        printf("ALM DAY_REG <-- %02x\n", value);
#endif
        s->alarm_tm.tm_mday = omap_rtc_bin(value);
        omap_rtc_alarm_update(s);
        return;

    case 0x30:	/* ALARM_MONTHS_REG */
#ifdef ALMDEBUG
        printf("ALM MON_REG <-- %02x\n", value);
#endif
        s->alarm_tm.tm_mon = omap_rtc_bin(value);
        omap_rtc_alarm_update(s);
        return;

    case 0x34:	/* ALARM_YEARS_REG */
#ifdef ALMDEBUG
        printf("ALM YRS_REG <-- %02x\n", value);
#endif
        s->alarm_tm.tm_year = omap_rtc_bin(value);
        omap_rtc_alarm_update(s);
        return;

    case 0x40:	/* RTC_CTRL_REG */
#ifdef ALMDEBUG
        printf("RTC CONTROL <-- %02x\n", value);
#endif
        s->pm_am = (value >> 3) & 1;
        s->auto_comp = (value >> 2) & 1;
        s->round = (value >> 1) & 1;
        s->running = value & 1;
        s->status &= 0xfd;
        s->status |= s->running << 1;
        return;

    case 0x44:	/* RTC_STATUS_REG */
#ifdef ALMDEBUG
        printf("RTC STATUSL <-- %02x\n", value);
#endif
        s->status &= ~((value & 0xc0) ^ 0x80);
        omap_rtc_interrupts_update(s);
        return;

    case 0x48:	/* RTC_INTERRUPTS_REG */
#ifdef ALMDEBUG
        printf("RTC INTRS <-- %02x\n", value);
#endif
        s->interrupts = value;
        return;

    case 0x4c:	/* RTC_COMP_LSB_REG */
#ifdef ALMDEBUG
        printf("RTC COMPLSB <-- %02x\n", value);
#endif
        s->comp_reg &= 0xff00;
        s->comp_reg |= 0x00ff & value;
        return;

    case 0x50:	/* RTC_COMP_MSB_REG */
#ifdef ALMDEBUG
        printf("RTC COMPMSB <-- %02x\n", value);
#endif
        s->comp_reg &= 0x00ff;
        s->comp_reg |= 0xff00 & (value << 8);
        return;

    default:
        OMAP_BAD_REG(addr);
        return;
    }
}

static CPUReadMemoryFunc *omap_rtc_readfn[] = {
    omap_rtc_read,
    omap_badwidth_read8,
    omap_badwidth_read8,
};

static CPUWriteMemoryFunc *omap_rtc_writefn[] = {
    omap_rtc_write,
    omap_badwidth_write8,
    omap_badwidth_write8,
};

static void omap_rtc_tick(void *opaque)
{
    struct omap_rtc_s *s = opaque;

    if (s->round) {
        /* Round to nearest full minute.  */
        if (s->current_tm.tm_sec < 30)
            s->ti -= s->current_tm.tm_sec;
        else
            s->ti += 60 - s->current_tm.tm_sec;

        s->round = 0;
    }

    memcpy(&s->current_tm, localtime(&s->ti), sizeof(s->current_tm));

    if ((s->interrupts & 0x08) && s->ti == s->alarm_ti) {
        s->status |= 0x40;
        omap_rtc_interrupts_update(s);
    }

    if (s->interrupts & 0x04)
        switch (s->interrupts & 3) {
        case 0:
            s->status |= 0x04;
            qemu_irq_pulse(s->irq);
            break;
        case 1:
            if (s->current_tm.tm_sec)
                break;
            s->status |= 0x08;
            qemu_irq_pulse(s->irq);
            break;
        case 2:
            if (s->current_tm.tm_sec || s->current_tm.tm_min)
                break;
            s->status |= 0x10;
            qemu_irq_pulse(s->irq);
            break;
        case 3:
            if (s->current_tm.tm_sec ||
                            s->current_tm.tm_min || s->current_tm.tm_hour)
                break;
            s->status |= 0x20;
            qemu_irq_pulse(s->irq);
            break;
        }

    /* Move on */
    if (s->running)
        s->ti ++;
    s->tick += 1000;

    /*
     * Every full hour add a rough approximation of the compensation
     * register to the 32kHz Timer (which drives the RTC) value. 
     */
    if (s->auto_comp && !s->current_tm.tm_sec && !s->current_tm.tm_min)
        s->tick += s->comp_reg * 1000 / 32768;

    qemu_mod_timer(s->clk, s->tick);
}

static void omap_rtc_reset(struct omap_rtc_s *s)
{
    struct tm tm;

    s->interrupts = 0;
    s->comp_reg = 0;
    s->running = 0;
    s->pm_am = 0;
    s->auto_comp = 0;
    s->round = 0;
    s->tick = qemu_get_clock(rt_clock);
    memset(&s->alarm_tm, 0, sizeof(s->alarm_tm));
    s->alarm_tm.tm_mday = 0x01;
    s->status = 1 << 7;
    qemu_get_timedate(&tm, 0);
    s->ti = mktimegm(&tm);

    omap_rtc_alarm_update(s);
    omap_rtc_tick(s);
}

struct omap_rtc_s *omap_rtc_init(target_phys_addr_t base,
                qemu_irq *irq, omap_clk clk)
{
    int iomemtype;
    struct omap_rtc_s *s = (struct omap_rtc_s *)
            qemu_mallocz(sizeof(struct omap_rtc_s));

    s->irq = irq[0];
    s->alarm = irq[1];
    s->clk = qemu_new_timer(rt_clock, omap_rtc_tick, s);

    omap_rtc_reset(s);

    iomemtype = cpu_register_io_memory(0, omap_rtc_readfn,
                    omap_rtc_writefn, s);
    cpu_register_physical_memory(base, 0x800, iomemtype);

    return s;
}

/* Multi-channel Buffered Serial Port interfaces */
struct omap_mcbsp_s {
    qemu_irq txirq;
    qemu_irq rxirq;
    qemu_irq txdrq;
    qemu_irq rxdrq;

    uint16_t spcr[2];
    uint16_t rcr[2];
    uint16_t xcr[2];
    uint16_t srgr[2];
    uint16_t mcr[2];
    uint16_t pcr;
    uint16_t rcer[8];
    uint16_t xcer[8];
    int tx_rate;
    int rx_rate;
    int tx_req;
    int rx_req;

    struct i2s_codec_s *codec;
    QEMUTimer *source_timer;
    QEMUTimer *sink_timer;
};

static void omap_mcbsp_intr_update(struct omap_mcbsp_s *s)
{
    int irq;

    switch ((s->spcr[0] >> 4) & 3) {			/* RINTM */
    case 0:
        irq = (s->spcr[0] >> 1) & 1;			/* RRDY */
        break;
    case 3:
        irq = (s->spcr[0] >> 3) & 1;			/* RSYNCERR */
        break;
    default:
        irq = 0;
        break;
    }

    if (irq)
        qemu_irq_pulse(s->rxirq);

    switch ((s->spcr[1] >> 4) & 3) {			/* XINTM */
    case 0:
        irq = (s->spcr[1] >> 1) & 1;			/* XRDY */
        break;
    case 3:
        irq = (s->spcr[1] >> 3) & 1;			/* XSYNCERR */
        break;
    default:
        irq = 0;
        break;
    }

    if (irq)
        qemu_irq_pulse(s->txirq);
}

static void omap_mcbsp_rx_newdata(struct omap_mcbsp_s *s)
{
    if ((s->spcr[0] >> 1) & 1)				/* RRDY */
        s->spcr[0] |= 1 << 2;				/* RFULL */
    s->spcr[0] |= 1 << 1;				/* RRDY */
    qemu_irq_raise(s->rxdrq);
    omap_mcbsp_intr_update(s);
}

static void omap_mcbsp_source_tick(void *opaque)
{
    struct omap_mcbsp_s *s = (struct omap_mcbsp_s *) opaque;
    static const int bps[8] = { 0, 1, 1, 2, 2, 2, -255, -255 };

    if (!s->rx_rate)
        return;
    if (s->rx_req)
        printf("%s: Rx FIFO overrun\n", __FUNCTION__);

    s->rx_req = s->rx_rate << bps[(s->rcr[0] >> 5) & 7];

    omap_mcbsp_rx_newdata(s);
    qemu_mod_timer(s->source_timer, qemu_get_clock(vm_clock) + ticks_per_sec);
}

static void omap_mcbsp_rx_start(struct omap_mcbsp_s *s)
{
    if (!s->codec || !s->codec->rts)
        omap_mcbsp_source_tick(s);
    else if (s->codec->in.len) {
        s->rx_req = s->codec->in.len;
        omap_mcbsp_rx_newdata(s);
    }
}

static void omap_mcbsp_rx_stop(struct omap_mcbsp_s *s)
{
    qemu_del_timer(s->source_timer);
}

static void omap_mcbsp_rx_done(struct omap_mcbsp_s *s)
{
    s->spcr[0] &= ~(1 << 1);				/* RRDY */
    qemu_irq_lower(s->rxdrq);
    omap_mcbsp_intr_update(s);
}

static void omap_mcbsp_tx_newdata(struct omap_mcbsp_s *s)
{
    s->spcr[1] |= 1 << 1;				/* XRDY */
    qemu_irq_raise(s->txdrq);
    omap_mcbsp_intr_update(s);
}

static void omap_mcbsp_sink_tick(void *opaque)
{
    struct omap_mcbsp_s *s = (struct omap_mcbsp_s *) opaque;
    static const int bps[8] = { 0, 1, 1, 2, 2, 2, -255, -255 };

    if (!s->tx_rate)
        return;
    if (s->tx_req)
        printf("%s: Tx FIFO underrun\n", __FUNCTION__);

    s->tx_req = s->tx_rate << bps[(s->xcr[0] >> 5) & 7];

    omap_mcbsp_tx_newdata(s);
    qemu_mod_timer(s->sink_timer, qemu_get_clock(vm_clock) + ticks_per_sec);
}

static void omap_mcbsp_tx_start(struct omap_mcbsp_s *s)
{
    if (!s->codec || !s->codec->cts)
        omap_mcbsp_sink_tick(s);
    else if (s->codec->out.size) {
        s->tx_req = s->codec->out.size;
        omap_mcbsp_tx_newdata(s);
    }
}

static void omap_mcbsp_tx_done(struct omap_mcbsp_s *s)
{
    s->spcr[1] &= ~(1 << 1);				/* XRDY */
    qemu_irq_lower(s->txdrq);
    omap_mcbsp_intr_update(s);
    if (s->codec && s->codec->cts)
        s->codec->tx_swallow(s->codec->opaque);
}

static void omap_mcbsp_tx_stop(struct omap_mcbsp_s *s)
{
    s->tx_req = 0;
    omap_mcbsp_tx_done(s);
    qemu_del_timer(s->sink_timer);
}

static void omap_mcbsp_req_update(struct omap_mcbsp_s *s)
{
    int prev_rx_rate, prev_tx_rate;
    int rx_rate = 0, tx_rate = 0;
    int cpu_rate = 1500000;	/* XXX */

    /* TODO: check CLKSTP bit */
    if (s->spcr[1] & (1 << 6)) {			/* GRST */
        if (s->spcr[0] & (1 << 0)) {			/* RRST */
            if ((s->srgr[1] & (1 << 13)) &&		/* CLKSM */
                            (s->pcr & (1 << 8))) {	/* CLKRM */
                if (~s->pcr & (1 << 7))			/* SCLKME */
                    rx_rate = cpu_rate /
                            ((s->srgr[0] & 0xff) + 1);	/* CLKGDV */
            } else
                if (s->codec)
                    rx_rate = s->codec->rx_rate;
        }

        if (s->spcr[1] & (1 << 0)) {			/* XRST */
            if ((s->srgr[1] & (1 << 13)) &&		/* CLKSM */
                            (s->pcr & (1 << 9))) {	/* CLKXM */
                if (~s->pcr & (1 << 7))			/* SCLKME */
                    tx_rate = cpu_rate /
                            ((s->srgr[0] & 0xff) + 1);	/* CLKGDV */
            } else
                if (s->codec)
                    tx_rate = s->codec->tx_rate;
        }
    }
    prev_tx_rate = s->tx_rate;
    prev_rx_rate = s->rx_rate;
    s->tx_rate = tx_rate;
    s->rx_rate = rx_rate;

    if (s->codec)
        s->codec->set_rate(s->codec->opaque, rx_rate, tx_rate);

    if (!prev_tx_rate && tx_rate)
        omap_mcbsp_tx_start(s);
    else if (s->tx_rate && !tx_rate)
        omap_mcbsp_tx_stop(s);

    if (!prev_rx_rate && rx_rate)
        omap_mcbsp_rx_start(s);
    else if (prev_tx_rate && !tx_rate)
        omap_mcbsp_rx_stop(s);
}

static uint32_t omap_mcbsp_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_mcbsp_s *s = (struct omap_mcbsp_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;
    uint16_t ret;

    switch (offset) {
    case 0x00:	/* DRR2 */
        if (((s->rcr[0] >> 5) & 7) < 3)			/* RWDLEN1 */
            return 0x0000;
        /* Fall through.  */
    case 0x02:	/* DRR1 */
        if (s->rx_req < 2) {
            printf("%s: Rx FIFO underrun\n", __FUNCTION__);
            omap_mcbsp_rx_done(s);
        } else {
            s->tx_req -= 2;
            if (s->codec && s->codec->in.len >= 2) {
                ret = s->codec->in.fifo[s->codec->in.start ++] << 8;
                ret |= s->codec->in.fifo[s->codec->in.start ++];
                s->codec->in.len -= 2;
            } else
                ret = 0x0000;
            if (!s->tx_req)
                omap_mcbsp_rx_done(s);
            return ret;
        }
        return 0x0000;

    case 0x04:	/* DXR2 */
    case 0x06:	/* DXR1 */
        return 0x0000;

    case 0x08:	/* SPCR2 */
        return s->spcr[1];
    case 0x0a:	/* SPCR1 */
        return s->spcr[0];
    case 0x0c:	/* RCR2 */
        return s->rcr[1];
    case 0x0e:	/* RCR1 */
        return s->rcr[0];
    case 0x10:	/* XCR2 */
        return s->xcr[1];
    case 0x12:	/* XCR1 */
        return s->xcr[0];
    case 0x14:	/* SRGR2 */
        return s->srgr[1];
    case 0x16:	/* SRGR1 */
        return s->srgr[0];
    case 0x18:	/* MCR2 */
        return s->mcr[1];
    case 0x1a:	/* MCR1 */
        return s->mcr[0];
    case 0x1c:	/* RCERA */
        return s->rcer[0];
    case 0x1e:	/* RCERB */
        return s->rcer[1];
    case 0x20:	/* XCERA */
        return s->xcer[0];
    case 0x22:	/* XCERB */
        return s->xcer[1];
    case 0x24:	/* PCR0 */
        return s->pcr;
    case 0x26:	/* RCERC */
        return s->rcer[2];
    case 0x28:	/* RCERD */
        return s->rcer[3];
    case 0x2a:	/* XCERC */
        return s->xcer[2];
    case 0x2c:	/* XCERD */
        return s->xcer[3];
    case 0x2e:	/* RCERE */
        return s->rcer[4];
    case 0x30:	/* RCERF */
        return s->rcer[5];
    case 0x32:	/* XCERE */
        return s->xcer[4];
    case 0x34:	/* XCERF */
        return s->xcer[5];
    case 0x36:	/* RCERG */
        return s->rcer[6];
    case 0x38:	/* RCERH */
        return s->rcer[7];
    case 0x3a:	/* XCERG */
        return s->xcer[6];
    case 0x3c:	/* XCERH */
        return s->xcer[7];
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_mcbsp_writeh(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_mcbsp_s *s = (struct omap_mcbsp_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    switch (offset) {
    case 0x00:	/* DRR2 */
    case 0x02:	/* DRR1 */
        OMAP_RO_REG(addr);
        return;

    case 0x04:	/* DXR2 */
        if (((s->xcr[0] >> 5) & 7) < 3)			/* XWDLEN1 */
            return;
        /* Fall through.  */
    case 0x06:	/* DXR1 */
        if (s->tx_req > 1) {
            s->tx_req -= 2;
            if (s->codec && s->codec->cts) {
                s->codec->out.fifo[s->codec->out.len ++] = (value >> 8) & 0xff;
                s->codec->out.fifo[s->codec->out.len ++] = (value >> 0) & 0xff;
            }
            if (s->tx_req < 2)
                omap_mcbsp_tx_done(s);
        } else
            printf("%s: Tx FIFO overrun\n", __FUNCTION__);
        return;

    case 0x08:	/* SPCR2 */
        s->spcr[1] &= 0x0002;
        s->spcr[1] |= 0x03f9 & value;
        s->spcr[1] |= 0x0004 & (value << 2);		/* XEMPTY := XRST */
        if (~value & 1)					/* XRST */
            s->spcr[1] &= ~6;
        omap_mcbsp_req_update(s);
        return;
    case 0x0a:	/* SPCR1 */
        s->spcr[0] &= 0x0006;
        s->spcr[0] |= 0xf8f9 & value;
        if (value & (1 << 15))				/* DLB */
            printf("%s: Digital Loopback mode enable attempt\n", __FUNCTION__);
        if (~value & 1) {				/* RRST */
            s->spcr[0] &= ~6;
            s->rx_req = 0;
            omap_mcbsp_rx_done(s);
        }
        omap_mcbsp_req_update(s);
        return;

    case 0x0c:	/* RCR2 */
        s->rcr[1] = value & 0xffff;
        return;
    case 0x0e:	/* RCR1 */
        s->rcr[0] = value & 0x7fe0;
        return;
    case 0x10:	/* XCR2 */
        s->xcr[1] = value & 0xffff;
        return;
    case 0x12:	/* XCR1 */
        s->xcr[0] = value & 0x7fe0;
        return;
    case 0x14:	/* SRGR2 */
        s->srgr[1] = value & 0xffff;
        omap_mcbsp_req_update(s);
        return;
    case 0x16:	/* SRGR1 */
        s->srgr[0] = value & 0xffff;
        omap_mcbsp_req_update(s);
        return;
    case 0x18:	/* MCR2 */
        s->mcr[1] = value & 0x03e3;
        if (value & 3)					/* XMCM */
            printf("%s: Tx channel selection mode enable attempt\n",
                            __FUNCTION__);
        return;
    case 0x1a:	/* MCR1 */
        s->mcr[0] = value & 0x03e1;
        if (value & 1)					/* RMCM */
            printf("%s: Rx channel selection mode enable attempt\n",
                            __FUNCTION__);
        return;
    case 0x1c:	/* RCERA */
        s->rcer[0] = value & 0xffff;
        return;
    case 0x1e:	/* RCERB */
        s->rcer[1] = value & 0xffff;
        return;
    case 0x20:	/* XCERA */
        s->xcer[0] = value & 0xffff;
        return;
    case 0x22:	/* XCERB */
        s->xcer[1] = value & 0xffff;
        return;
    case 0x24:	/* PCR0 */
        s->pcr = value & 0x7faf;
        return;
    case 0x26:	/* RCERC */
        s->rcer[2] = value & 0xffff;
        return;
    case 0x28:	/* RCERD */
        s->rcer[3] = value & 0xffff;
        return;
    case 0x2a:	/* XCERC */
        s->xcer[2] = value & 0xffff;
        return;
    case 0x2c:	/* XCERD */
        s->xcer[3] = value & 0xffff;
        return;
    case 0x2e:	/* RCERE */
        s->rcer[4] = value & 0xffff;
        return;
    case 0x30:	/* RCERF */
        s->rcer[5] = value & 0xffff;
        return;
    case 0x32:	/* XCERE */
        s->xcer[4] = value & 0xffff;
        return;
    case 0x34:	/* XCERF */
        s->xcer[5] = value & 0xffff;
        return;
    case 0x36:	/* RCERG */
        s->rcer[6] = value & 0xffff;
        return;
    case 0x38:	/* RCERH */
        s->rcer[7] = value & 0xffff;
        return;
    case 0x3a:	/* XCERG */
        s->xcer[6] = value & 0xffff;
        return;
    case 0x3c:	/* XCERH */
        s->xcer[7] = value & 0xffff;
        return;
    }

    OMAP_BAD_REG(addr);
}

static void omap_mcbsp_writew(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_mcbsp_s *s = (struct omap_mcbsp_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    if (offset == 0x04) {				/* DXR */
        if (((s->xcr[0] >> 5) & 7) < 3)			/* XWDLEN1 */
            return;
        if (s->tx_req > 3) {
            s->tx_req -= 4;
            if (s->codec && s->codec->cts) {
                s->codec->out.fifo[s->codec->out.len ++] =
                        (value >> 24) & 0xff;
                s->codec->out.fifo[s->codec->out.len ++] =
                        (value >> 16) & 0xff;
                s->codec->out.fifo[s->codec->out.len ++] =
                        (value >> 8) & 0xff;
                s->codec->out.fifo[s->codec->out.len ++] =
                        (value >> 0) & 0xff;
            }
            if (s->tx_req < 4)
                omap_mcbsp_tx_done(s);
        } else
            printf("%s: Tx FIFO overrun\n", __FUNCTION__);
        return;
    }

    omap_badwidth_write16(opaque, addr, value);
}

static CPUReadMemoryFunc *omap_mcbsp_readfn[] = {
    omap_badwidth_read16,
    omap_mcbsp_read,
    omap_badwidth_read16,
};

static CPUWriteMemoryFunc *omap_mcbsp_writefn[] = {
    omap_badwidth_write16,
    omap_mcbsp_writeh,
    omap_mcbsp_writew,
};

static void omap_mcbsp_reset(struct omap_mcbsp_s *s)
{
    memset(&s->spcr, 0, sizeof(s->spcr));
    memset(&s->rcr, 0, sizeof(s->rcr));
    memset(&s->xcr, 0, sizeof(s->xcr));
    s->srgr[0] = 0x0001;
    s->srgr[1] = 0x2000;
    memset(&s->mcr, 0, sizeof(s->mcr));
    memset(&s->pcr, 0, sizeof(s->pcr));
    memset(&s->rcer, 0, sizeof(s->rcer));
    memset(&s->xcer, 0, sizeof(s->xcer));
    s->tx_req = 0;
    s->rx_req = 0;
    s->tx_rate = 0;
    s->rx_rate = 0;
    qemu_del_timer(s->source_timer);
    qemu_del_timer(s->sink_timer);
}

struct omap_mcbsp_s *omap_mcbsp_init(target_phys_addr_t base,
                qemu_irq *irq, qemu_irq *dma, omap_clk clk)
{
    int iomemtype;
    struct omap_mcbsp_s *s = (struct omap_mcbsp_s *)
            qemu_mallocz(sizeof(struct omap_mcbsp_s));

    s->txirq = irq[0];
    s->rxirq = irq[1];
    s->txdrq = dma[0];
    s->rxdrq = dma[1];
    s->sink_timer = qemu_new_timer(vm_clock, omap_mcbsp_sink_tick, s);
    s->source_timer = qemu_new_timer(vm_clock, omap_mcbsp_source_tick, s);
    omap_mcbsp_reset(s);

    iomemtype = cpu_register_io_memory(0, omap_mcbsp_readfn,
                    omap_mcbsp_writefn, s);
    cpu_register_physical_memory(base, 0x800, iomemtype);

    return s;
}

static void omap_mcbsp_i2s_swallow(void *opaque, int line, int level)
{
    struct omap_mcbsp_s *s = (struct omap_mcbsp_s *) opaque;

    if (s->rx_rate) {
        s->rx_req = s->codec->in.len;
        omap_mcbsp_rx_newdata(s);
    }
}

static void omap_mcbsp_i2s_start(void *opaque, int line, int level)
{
    struct omap_mcbsp_s *s = (struct omap_mcbsp_s *) opaque;

    if (s->tx_rate) {
        s->tx_req = s->codec->out.size;
        omap_mcbsp_tx_newdata(s);
    }
}

void omap_mcbsp_i2s_attach(struct omap_mcbsp_s *s, struct i2s_codec_s *slave)
{
    s->codec = slave;
    slave->rx_swallow = qemu_allocate_irqs(omap_mcbsp_i2s_swallow, s, 1)[0];
    slave->tx_start = qemu_allocate_irqs(omap_mcbsp_i2s_start, s, 1)[0];
}

/* LED Pulse Generators */
struct omap_lpg_s {
    QEMUTimer *tm;

    uint8_t control;
    uint8_t power;
    int64_t on;
    int64_t period;
    int clk;
    int cycle;
};

static void omap_lpg_tick(void *opaque)
{
    struct omap_lpg_s *s = opaque;

    if (s->cycle)
        qemu_mod_timer(s->tm, qemu_get_clock(rt_clock) + s->period - s->on);
    else
        qemu_mod_timer(s->tm, qemu_get_clock(rt_clock) + s->on);

    s->cycle = !s->cycle;
    printf("%s: LED is %s\n", __FUNCTION__, s->cycle ? "on" : "off");
}

static void omap_lpg_update(struct omap_lpg_s *s)
{
    int64_t on, period = 1, ticks = 1000;
    static const int per[8] = { 1, 2, 4, 8, 12, 16, 20, 24 };

    if (~s->control & (1 << 6))					/* LPGRES */
        on = 0;
    else if (s->control & (1 << 7))				/* PERM_ON */
        on = period;
    else {
        period = muldiv64(ticks, per[s->control & 7],		/* PERCTRL */
                        256 / 32);
        on = (s->clk && s->power) ? muldiv64(ticks,
                        per[(s->control >> 3) & 7], 256) : 0;	/* ONCTRL */
    }

    qemu_del_timer(s->tm);
    if (on == period && s->on < s->period)
        printf("%s: LED is on\n", __FUNCTION__);
    else if (on == 0 && s->on)
        printf("%s: LED is off\n", __FUNCTION__);
    else if (on && (on != s->on || period != s->period)) {
        s->cycle = 0;
        s->on = on;
        s->period = period;
        omap_lpg_tick(s);
        return;
    }

    s->on = on;
    s->period = period;
}

static void omap_lpg_reset(struct omap_lpg_s *s)
{
    s->control = 0x00;
    s->power = 0x00;
    s->clk = 1;
    omap_lpg_update(s);
}

static uint32_t omap_lpg_read(void *opaque, target_phys_addr_t addr)
{
    struct omap_lpg_s *s = (struct omap_lpg_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    switch (offset) {
    case 0x00:	/* LCR */
        return s->control;

    case 0x04:	/* PMR */
        return s->power;
    }

    OMAP_BAD_REG(addr);
    return 0;
}

static void omap_lpg_write(void *opaque, target_phys_addr_t addr,
                uint32_t value)
{
    struct omap_lpg_s *s = (struct omap_lpg_s *) opaque;
    int offset = addr & OMAP_MPUI_REG_MASK;

    switch (offset) {
    case 0x00:	/* LCR */
        if (~value & (1 << 6))					/* LPGRES */
            omap_lpg_reset(s);
        s->control = value & 0xff;
        omap_lpg_update(s);
        return;

    case 0x04:	/* PMR */
        s->power = value & 0x01;
        omap_lpg_update(s);
        return;

    default:
        OMAP_BAD_REG(addr);
        return;
    }
}

static CPUReadMemoryFunc *omap_lpg_readfn[] = {
    omap_lpg_read,
    omap_badwidth_read8,
    omap_badwidth_read8,
};

static CPUWriteMemoryFunc *omap_lpg_writefn[] = {
    omap_lpg_write,
    omap_badwidth_write8,
    omap_badwidth_write8,
};

static void omap_lpg_clk_update(void *opaque, int line, int on)
{
    struct omap_lpg_s *s = (struct omap_lpg_s *) opaque;

    s->clk = on;
    omap_lpg_update(s);
}

struct omap_lpg_s *omap_lpg_init(target_phys_addr_t base, omap_clk clk)
{
    int iomemtype;
    struct omap_lpg_s *s = (struct omap_lpg_s *)
            qemu_mallocz(sizeof(struct omap_lpg_s));

    s->tm = qemu_new_timer(rt_clock, omap_lpg_tick, s);

    omap_lpg_reset(s);

    iomemtype = cpu_register_io_memory(0, omap_lpg_readfn,
                    omap_lpg_writefn, s);
    cpu_register_physical_memory(base, 0x800, iomemtype);

    omap_clk_adduser(clk, qemu_allocate_irqs(omap_lpg_clk_update, s, 1)[0]);

    return s;
}

/* MPUI Peripheral Bridge configuration */
static uint32_t omap_mpui_io_read(void *opaque, target_phys_addr_t addr)
{
    if (addr == OMAP_MPUI_BASE)	/* CMR */
        return 0xfe4d;

    OMAP_BAD_REG(addr);
    return 0;
}

static CPUReadMemoryFunc *omap_mpui_io_readfn[] = {
    omap_badwidth_read16,
    omap_mpui_io_read,
    omap_badwidth_read16,
};

static CPUWriteMemoryFunc *omap_mpui_io_writefn[] = {
    omap_badwidth_write16,
    omap_badwidth_write16,
    omap_badwidth_write16,
};

static void omap_setup_mpui_io(struct omap_mpu_state_s *mpu)
{
    int iomemtype = cpu_register_io_memory(0, omap_mpui_io_readfn,
                    omap_mpui_io_writefn, mpu);
    cpu_register_physical_memory(OMAP_MPUI_BASE, 0x7fff, iomemtype);
}

/* General chip reset */
static void omap1_mpu_reset(void *opaque)
{
    struct omap_mpu_state_s *mpu = (struct omap_mpu_state_s *) opaque;

    omap_inth_reset(mpu->ih[0]);
    omap_inth_reset(mpu->ih[1]);
    omap_dma_reset(mpu->dma);
    omap_mpu_timer_reset(mpu->timer[0]);
    omap_mpu_timer_reset(mpu->timer[1]);
    omap_mpu_timer_reset(mpu->timer[2]);
    omap_wd_timer_reset(mpu->wdt);
    omap_os_timer_reset(mpu->os_timer);
    omap_lcdc_reset(mpu->lcd);
    omap_ulpd_pm_reset(mpu);
    omap_pin_cfg_reset(mpu);
    omap_mpui_reset(mpu);
    omap_tipb_bridge_reset(mpu->private_tipb);
    omap_tipb_bridge_reset(mpu->public_tipb);
    omap_dpll_reset(&mpu->dpll[0]);
    omap_dpll_reset(&mpu->dpll[1]);
    omap_dpll_reset(&mpu->dpll[2]);
    omap_uart_reset(mpu->uart[0]);
    omap_uart_reset(mpu->uart[1]);
    omap_uart_reset(mpu->uart[2]);
    omap_mmc_reset(mpu->mmc);
    omap_mpuio_reset(mpu->mpuio);
    omap_gpio_reset(mpu->gpio);
    omap_uwire_reset(mpu->microwire);
    omap_pwl_reset(mpu);
    omap_pwt_reset(mpu);
    omap_i2c_reset(mpu->i2c[0]);
    omap_rtc_reset(mpu->rtc);
    omap_mcbsp_reset(mpu->mcbsp1);
    omap_mcbsp_reset(mpu->mcbsp2);
    omap_mcbsp_reset(mpu->mcbsp3);
    omap_lpg_reset(mpu->led[0]);
    omap_lpg_reset(mpu->led[1]);
    omap_clkm_reset(mpu);
    cpu_reset(mpu->env);
}

static const struct omap_map_s {
    target_phys_addr_t phys_dsp;
    target_phys_addr_t phys_mpu;
    uint32_t size;
    const char *name;
} omap15xx_dsp_mm[] = {
    /* Strobe 0 */
    { 0xe1010000, 0xfffb0000, 0x800, "UART1 BT" },		/* CS0 */
    { 0xe1010800, 0xfffb0800, 0x800, "UART2 COM" },		/* CS1 */
    { 0xe1011800, 0xfffb1800, 0x800, "McBSP1 audio" },		/* CS3 */
    { 0xe1012000, 0xfffb2000, 0x800, "MCSI2 communication" },	/* CS4 */
    { 0xe1012800, 0xfffb2800, 0x800, "MCSI1 BT u-Law" },	/* CS5 */
    { 0xe1013000, 0xfffb3000, 0x800, "uWire" },			/* CS6 */
    { 0xe1013800, 0xfffb3800, 0x800, "I^2C" },			/* CS7 */
    { 0xe1014000, 0xfffb4000, 0x800, "USB W2FC" },		/* CS8 */
    { 0xe1014800, 0xfffb4800, 0x800, "RTC" },			/* CS9 */
    { 0xe1015000, 0xfffb5000, 0x800, "MPUIO" },			/* CS10 */
    { 0xe1015800, 0xfffb5800, 0x800, "PWL" },			/* CS11 */
    { 0xe1016000, 0xfffb6000, 0x800, "PWT" },			/* CS12 */
    { 0xe1017000, 0xfffb7000, 0x800, "McBSP3" },		/* CS14 */
    { 0xe1017800, 0xfffb7800, 0x800, "MMC" },			/* CS15 */
    { 0xe1019000, 0xfffb9000, 0x800, "32-kHz timer" },		/* CS18 */
    { 0xe1019800, 0xfffb9800, 0x800, "UART3" },			/* CS19 */
    { 0xe101c800, 0xfffbc800, 0x800, "TIPB switches" },		/* CS25 */
    /* Strobe 1 */
    { 0xe101e000, 0xfffce000, 0x800, "GPIOs" },			/* CS28 */

    { 0 }
};

static void omap_setup_dsp_mapping(const struct omap_map_s *map)
{
    int io;

    for (; map->phys_dsp; map ++) {
        io = cpu_get_physical_page_desc(map->phys_mpu);

        cpu_register_physical_memory(map->phys_dsp, map->size, io);
    }
}

void omap_mpu_wakeup(void *opaque, int irq, int req)
{
    struct omap_mpu_state_s *mpu = (struct omap_mpu_state_s *) opaque;

    if (mpu->env->halted)
        cpu_interrupt(mpu->env, CPU_INTERRUPT_EXITTB);
}

static const struct dma_irq_map omap1_dma_irq_map[] = {
    { 0, OMAP_INT_DMA_CH0_6 },
    { 0, OMAP_INT_DMA_CH1_7 },
    { 0, OMAP_INT_DMA_CH2_8 },
    { 0, OMAP_INT_DMA_CH3 },
    { 0, OMAP_INT_DMA_CH4 },
    { 0, OMAP_INT_DMA_CH5 },
    { 1, OMAP_INT_1610_DMA_CH6 },
    { 1, OMAP_INT_1610_DMA_CH7 },
    { 1, OMAP_INT_1610_DMA_CH8 },
    { 1, OMAP_INT_1610_DMA_CH9 },
    { 1, OMAP_INT_1610_DMA_CH10 },
    { 1, OMAP_INT_1610_DMA_CH11 },
    { 1, OMAP_INT_1610_DMA_CH12 },
    { 1, OMAP_INT_1610_DMA_CH13 },
    { 1, OMAP_INT_1610_DMA_CH14 },
    { 1, OMAP_INT_1610_DMA_CH15 }
};

/* DMA ports for OMAP1 */
static int omap_validate_emiff_addr(struct omap_mpu_state_s *s,
                target_phys_addr_t addr)
{
    return addr >= OMAP_EMIFF_BASE && addr < OMAP_EMIFF_BASE + s->sdram_size;
}

static int omap_validate_emifs_addr(struct omap_mpu_state_s *s,
                target_phys_addr_t addr)
{
    return addr >= OMAP_EMIFS_BASE && addr < OMAP_EMIFF_BASE;
}

static int omap_validate_imif_addr(struct omap_mpu_state_s *s,
                target_phys_addr_t addr)
{
    return addr >= OMAP_IMIF_BASE && addr < OMAP_IMIF_BASE + s->sram_size;
}

static int omap_validate_tipb_addr(struct omap_mpu_state_s *s,
                target_phys_addr_t addr)
{
    return addr >= 0xfffb0000 && addr < 0xffff0000;
}

static int omap_validate_local_addr(struct omap_mpu_state_s *s,
                target_phys_addr_t addr)
{
    return addr >= OMAP_LOCALBUS_BASE && addr < OMAP_LOCALBUS_BASE + 0x1000000;
}

static int omap_validate_tipb_mpui_addr(struct omap_mpu_state_s *s,
                target_phys_addr_t addr)
{
    return addr >= 0xe1010000 && addr < 0xe1020004;
}

struct omap_mpu_state_s *omap310_mpu_init(unsigned long sdram_size,
                const char *core)
{
    int i;
    struct omap_mpu_state_s *s = (struct omap_mpu_state_s *)
            qemu_mallocz(sizeof(struct omap_mpu_state_s));
    ram_addr_t imif_base, emiff_base;
    qemu_irq *cpu_irq;
    qemu_irq dma_irqs[6];
    int sdindex;

    if (!core)
        core = "ti925t";

    /* Core */
    s->mpu_model = omap310;
    s->env = cpu_init(core);
    if (!s->env) {
        fprintf(stderr, "Unable to find CPU definition\n");
        exit(1);
    }
    s->sdram_size = sdram_size;
    s->sram_size = OMAP15XX_SRAM_SIZE;

    s->wakeup = qemu_allocate_irqs(omap_mpu_wakeup, s, 1)[0];

    /* Clocks */
    omap_clk_init(s);

    /* Memory-mapped stuff */
    cpu_register_physical_memory(OMAP_EMIFF_BASE, s->sdram_size,
                    (emiff_base = qemu_ram_alloc(s->sdram_size)) | IO_MEM_RAM);
    cpu_register_physical_memory(OMAP_IMIF_BASE, s->sram_size,
                    (imif_base = qemu_ram_alloc(s->sram_size)) | IO_MEM_RAM);

    omap_clkm_init(0xfffece00, 0xe1008000, s);

    cpu_irq = arm_pic_init_cpu(s->env);
    s->ih[0] = omap_inth_init(0xfffecb00, 0x100, 1, &s->irq[0],
                    cpu_irq[ARM_PIC_CPU_IRQ], cpu_irq[ARM_PIC_CPU_FIQ],
                    omap_findclk(s, "arminth_ck"));
    s->ih[1] = omap_inth_init(0xfffe0000, 0x800, 1, &s->irq[1],
                    s->ih[0]->pins[OMAP_INT_15XX_IH2_IRQ], NULL,
                    omap_findclk(s, "arminth_ck"));

    for (i = 0; i < 6; i ++)
        dma_irqs[i] =
                s->irq[omap1_dma_irq_map[i].ih][omap1_dma_irq_map[i].intr];
    s->dma = omap_dma_init(0xfffed800, dma_irqs, s->irq[0][OMAP_INT_DMA_LCD],
                           s, omap_findclk(s, "dma_ck"), omap_dma_3_1);

    s->port[emiff    ].addr_valid = omap_validate_emiff_addr;
    s->port[emifs    ].addr_valid = omap_validate_emifs_addr;
    s->port[imif     ].addr_valid = omap_validate_imif_addr;
    s->port[tipb     ].addr_valid = omap_validate_tipb_addr;
    s->port[local    ].addr_valid = omap_validate_local_addr;
    s->port[tipb_mpui].addr_valid = omap_validate_tipb_mpui_addr;

    /* Register SDRAM and SRAM DMA ports for fast transfers.  */
    soc_dma_port_add_mem_ram(s->dma,
                    emiff_base, OMAP_EMIFF_BASE, s->sdram_size);
    soc_dma_port_add_mem_ram(s->dma,
                    imif_base, OMAP_IMIF_BASE, s->sram_size);

    s->timer[0] = omap_mpu_timer_init(0xfffec500,
                    s->irq[0][OMAP_INT_TIMER1],
                    omap_findclk(s, "mputim_ck"));
    s->timer[1] = omap_mpu_timer_init(0xfffec600,
                    s->irq[0][OMAP_INT_TIMER2],
                    omap_findclk(s, "mputim_ck"));
    s->timer[2] = omap_mpu_timer_init(0xfffec700,
                    s->irq[0][OMAP_INT_TIMER3],
                    omap_findclk(s, "mputim_ck"));

    s->wdt = omap_wd_timer_init(0xfffec800,
                    s->irq[0][OMAP_INT_WD_TIMER],
                    omap_findclk(s, "armwdt_ck"));

    s->os_timer = omap_os_timer_init(0xfffb9000,
                    s->irq[1][OMAP_INT_OS_TIMER],
                    omap_findclk(s, "clk32-kHz"));

    s->lcd = omap_lcdc_init(0xfffec000, s->irq[0][OMAP_INT_LCD_CTRL],
                    omap_dma_get_lcdch(s->dma), imif_base, emiff_base,
                    omap_findclk(s, "lcd_ck"));

    omap_ulpd_pm_init(0xfffe0800, s);
    omap_pin_cfg_init(0xfffe1000, s);
    omap_id_init(s);

    omap_mpui_init(0xfffec900, s);

    s->private_tipb = omap_tipb_bridge_init(0xfffeca00,
                    s->irq[0][OMAP_INT_BRIDGE_PRIV],
                    omap_findclk(s, "tipb_ck"));
    s->public_tipb = omap_tipb_bridge_init(0xfffed300,
                    s->irq[0][OMAP_INT_BRIDGE_PUB],
                    omap_findclk(s, "tipb_ck"));

    omap_tcmi_init(0xfffecc00, s);

    s->uart[0] = omap_uart_init(0xfffb0000, s->irq[1][OMAP_INT_UART1],
                    omap_findclk(s, "uart1_ck"),
                    omap_findclk(s, "uart1_ck"),
                    s->drq[OMAP_DMA_UART1_TX], s->drq[OMAP_DMA_UART1_RX],
                    serial_hds[0]);
    s->uart[1] = omap_uart_init(0xfffb0800, s->irq[1][OMAP_INT_UART2],
                    omap_findclk(s, "uart2_ck"),
                    omap_findclk(s, "uart2_ck"),
                    s->drq[OMAP_DMA_UART2_TX], s->drq[OMAP_DMA_UART2_RX],
                    serial_hds[0] ? serial_hds[1] : 0);
    s->uart[2] = omap_uart_init(0xfffb9800, s->irq[0][OMAP_INT_UART3],
                    omap_findclk(s, "uart3_ck"),
                    omap_findclk(s, "uart3_ck"),
                    s->drq[OMAP_DMA_UART3_TX], s->drq[OMAP_DMA_UART3_RX],
                    serial_hds[0] && serial_hds[1] ? serial_hds[2] : 0);

    omap_dpll_init(&s->dpll[0], 0xfffecf00, omap_findclk(s, "dpll1"));
    omap_dpll_init(&s->dpll[1], 0xfffed000, omap_findclk(s, "dpll2"));
    omap_dpll_init(&s->dpll[2], 0xfffed100, omap_findclk(s, "dpll3"));

    sdindex = drive_get_index(IF_SD, 0, 0);
    if (sdindex == -1) {
        fprintf(stderr, "qemu: missing SecureDigital device\n");
        exit(1);
    }
    s->mmc = omap_mmc_init(0xfffb7800, drives_table[sdindex].bdrv,
                    s->irq[1][OMAP_INT_OQN], &s->drq[OMAP_DMA_MMC_TX],
                    omap_findclk(s, "mmc_ck"));

    s->mpuio = omap_mpuio_init(0xfffb5000,
                    s->irq[1][OMAP_INT_KEYBOARD], s->irq[1][OMAP_INT_MPUIO],
                    s->wakeup, omap_findclk(s, "clk32-kHz"));

    s->gpio = omap_gpio_init(0xfffce000, s->irq[0][OMAP_INT_GPIO_BANK1],
                    omap_findclk(s, "arm_gpio_ck"));

    s->microwire = omap_uwire_init(0xfffb3000, &s->irq[1][OMAP_INT_uWireTX],
                    s->drq[OMAP_DMA_UWIRE_TX], omap_findclk(s, "mpuper_ck"));

    omap_pwl_init(0xfffb5800, s, omap_findclk(s, "armxor_ck"));
    omap_pwt_init(0xfffb6000, s, omap_findclk(s, "armxor_ck"));

    s->i2c[0] = omap_i2c_init(0xfffb3800, s->irq[1][OMAP_INT_I2C],
                    &s->drq[OMAP_DMA_I2C_RX], omap_findclk(s, "mpuper_ck"));

    s->rtc = omap_rtc_init(0xfffb4800, &s->irq[1][OMAP_INT_RTC_TIMER],
                    omap_findclk(s, "clk32-kHz"));

    s->mcbsp1 = omap_mcbsp_init(0xfffb1800, &s->irq[1][OMAP_INT_McBSP1TX],
                    &s->drq[OMAP_DMA_MCBSP1_TX], omap_findclk(s, "dspxor_ck"));
    s->mcbsp2 = omap_mcbsp_init(0xfffb1000, &s->irq[0][OMAP_INT_310_McBSP2_TX],
                    &s->drq[OMAP_DMA_MCBSP2_TX], omap_findclk(s, "mpuper_ck"));
    s->mcbsp3 = omap_mcbsp_init(0xfffb7000, &s->irq[1][OMAP_INT_McBSP3TX],
                    &s->drq[OMAP_DMA_MCBSP3_TX], omap_findclk(s, "dspxor_ck"));

    s->led[0] = omap_lpg_init(0xfffbd000, omap_findclk(s, "clk32-kHz"));
    s->led[1] = omap_lpg_init(0xfffbd800, omap_findclk(s, "clk32-kHz"));

    /* Register mappings not currenlty implemented:
     * MCSI2 Comm	fffb2000 - fffb27ff (not mapped on OMAP310)
     * MCSI1 Bluetooth	fffb2800 - fffb2fff (not mapped on OMAP310)
     * USB W2FC		fffb4000 - fffb47ff
     * Camera Interface	fffb6800 - fffb6fff
     * USB Host		fffba000 - fffba7ff
     * FAC		fffba800 - fffbafff
     * HDQ/1-Wire	fffbc000 - fffbc7ff
     * TIPB switches	fffbc800 - fffbcfff
     * Mailbox		fffcf000 - fffcf7ff
     * Local bus IF	fffec100 - fffec1ff
     * Local bus MMU	fffec200 - fffec2ff
     * DSP MMU		fffed200 - fffed2ff
     */

    omap_setup_dsp_mapping(omap15xx_dsp_mm);
    omap_setup_mpui_io(s);

    qemu_register_reset(omap1_mpu_reset, s);

    return s;
}
