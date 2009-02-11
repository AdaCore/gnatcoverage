/*
 * ARM PrimeCell Timer modules.
 *
 * Copyright (c) 2005-2006 CodeSourcery.
 * Written by Paul Brook
 *
 * This code is licenced under the GPL.
 */

#include "hw.h"
#include "qemu-timer.h"
#include "primecell.h"

/* Common timer implementation.  */

#define TIMER_CTRL_ONESHOT      (1 << 0)
#define TIMER_CTRL_32BIT        (1 << 1)
#define TIMER_CTRL_DIV1         (0 << 2)
#define TIMER_CTRL_DIV16        (1 << 2)
#define TIMER_CTRL_DIV256       (2 << 2)
#define TIMER_CTRL_IE           (1 << 5)
#define TIMER_CTRL_PERIODIC     (1 << 6)
#define TIMER_CTRL_ENABLE       (1 << 7)

typedef struct {
    ptimer_state *timer;
    uint32_t control;
    uint32_t limit;
    int freq;
    int int_level;
    qemu_irq irq;
} arm_timer_state;

/* Check all active timers, and schedule the next timer interrupt.  */

static void arm_timer_update(arm_timer_state *s)
{
    /* Update interrupts.  */
    if (s->int_level && (s->control & TIMER_CTRL_IE)) {
        qemu_irq_raise(s->irq);
    } else {
        qemu_irq_lower(s->irq);
    }
}

static uint32_t arm_timer_read(void *opaque, target_phys_addr_t offset)
{
    arm_timer_state *s = (arm_timer_state *)opaque;

    switch (offset >> 2) {
    case 0: /* TimerLoad */
    case 6: /* TimerBGLoad */
        return s->limit;
    case 1: /* TimerValue */
        return ptimer_get_count(s->timer);
    case 2: /* TimerControl */
        return s->control;
    case 4: /* TimerRIS */
        return s->int_level;
    case 5: /* TimerMIS */
        if ((s->control & TIMER_CTRL_IE) == 0)
            return 0;
        return s->int_level;
    default:
        cpu_abort (cpu_single_env, "arm_timer_read: Bad offset %x\n",
                   (int)offset);
        return 0;
    }
}

/* Reset the timer limit after settings have changed.  */
static void arm_timer_recalibrate(arm_timer_state *s, int reload)
{
    uint32_t limit;

    if ((s->control & TIMER_CTRL_PERIODIC) == 0) {
        /* Free running.  */
        if (s->control & TIMER_CTRL_32BIT)
            limit = 0xffffffff;
        else
            limit = 0xffff;
    } else {
          /* Periodic.  */
          limit = s->limit;
    }
    ptimer_set_limit(s->timer, limit, reload);
}

static void arm_timer_write(void *opaque, target_phys_addr_t offset,
                            uint32_t value)
{
    arm_timer_state *s = (arm_timer_state *)opaque;
    int freq;

    switch (offset >> 2) {
    case 0: /* TimerLoad */
        s->limit = value;
        arm_timer_recalibrate(s, 1);
        break;
    case 1: /* TimerValue */
        /* ??? Linux seems to want to write to this readonly register.
           Ignore it.  */
        break;
    case 2: /* TimerControl */
        if (s->control & TIMER_CTRL_ENABLE) {
            /* Pause the timer if it is running.  This may cause some
               inaccuracy dure to rounding, but avoids a whole lot of other
               messyness.  */
            ptimer_stop(s->timer);
        }
        s->control = value;
        freq = s->freq;
        /* ??? Need to recalculate expiry time after changing divisor.  */
        switch ((value >> 2) & 3) {
        case 1: freq >>= 4; break;
        case 2: freq >>= 8; break;
        }
        arm_timer_recalibrate(s, 0);
        ptimer_set_freq(s->timer, freq);
        if (s->control & TIMER_CTRL_ENABLE) {
            /* Restart the timer if still enabled.  */
            ptimer_run(s->timer, (s->control & TIMER_CTRL_ONESHOT) != 0);
        }
        break;
    case 3: /* TimerIntClr */
        s->int_level = 0;
        break;
    case 6: /* TimerBGLoad */
        s->limit = value;
        arm_timer_recalibrate(s, 0);
        break;
    default:
        cpu_abort (cpu_single_env, "arm_timer_write: Bad offset %x\n",
                   (int)offset);
    }
    arm_timer_update(s);
}

static void arm_timer_tick(void *opaque)
{
    arm_timer_state *s = (arm_timer_state *)opaque;
    s->int_level = 1;
    arm_timer_update(s);
}

static void arm_timer_save(QEMUFile *f, void *opaque)
{
    arm_timer_state *s = (arm_timer_state *)opaque;
    qemu_put_be32(f, s->control);
    qemu_put_be32(f, s->limit);
    qemu_put_be32(f, s->int_level);
    qemu_put_ptimer(f, s->timer);
}

static int arm_timer_load(QEMUFile *f, void *opaque, int version_id)
{
    arm_timer_state *s = (arm_timer_state *)opaque;

    if (version_id != 1)
        return -EINVAL;

    s->control = qemu_get_be32(f);
    s->limit = qemu_get_be32(f);
    s->int_level = qemu_get_be32(f);
    qemu_get_ptimer(f, s->timer);
    return 0;
}

static void *arm_timer_init(uint32_t freq, qemu_irq irq)
{
    arm_timer_state *s;
    QEMUBH *bh;

    s = (arm_timer_state *)qemu_mallocz(sizeof(arm_timer_state));
    s->irq = irq;
    s->freq = freq;
    s->control = TIMER_CTRL_IE;

    bh = qemu_bh_new(arm_timer_tick, s);
    s->timer = ptimer_init(bh);
    register_savevm("arm_timer", -1, 1, arm_timer_save, arm_timer_load, s);
    return s;
}

/* ARM PrimeCell SP804 dual timer module.
   Docs for this device don't seem to be publicly available.  This
   implementation is based on guesswork, the linux kernel sources and the
   Integrator/CP timer modules.  */

typedef struct {
    void *timer[2];
    int level[2];
    qemu_irq irq;
} sp804_state;

/* Merge the IRQs from the two component devices.  */
static void sp804_set_irq(void *opaque, int irq, int level)
{
    sp804_state *s = (sp804_state *)opaque;

    s->level[irq] = level;
    qemu_set_irq(s->irq, s->level[0] || s->level[1]);
}

static uint32_t sp804_read(void *opaque, target_phys_addr_t offset)
{
    sp804_state *s = (sp804_state *)opaque;

    /* ??? Don't know the PrimeCell ID for this device.  */
    if (offset < 0x20) {
        return arm_timer_read(s->timer[0], offset);
    } else {
        return arm_timer_read(s->timer[1], offset - 0x20);
    }
}

static void sp804_write(void *opaque, target_phys_addr_t offset,
                        uint32_t value)
{
    sp804_state *s = (sp804_state *)opaque;

    if (offset < 0x20) {
        arm_timer_write(s->timer[0], offset, value);
    } else {
        arm_timer_write(s->timer[1], offset - 0x20, value);
    }
}

static CPUReadMemoryFunc *sp804_readfn[] = {
   sp804_read,
   sp804_read,
   sp804_read
};

static CPUWriteMemoryFunc *sp804_writefn[] = {
   sp804_write,
   sp804_write,
   sp804_write
};

static void sp804_save(QEMUFile *f, void *opaque)
{
    sp804_state *s = (sp804_state *)opaque;
    qemu_put_be32(f, s->level[0]);
    qemu_put_be32(f, s->level[1]);
}

static int sp804_load(QEMUFile *f, void *opaque, int version_id)
{
    sp804_state *s = (sp804_state *)opaque;

    if (version_id != 1)
        return -EINVAL;

    s->level[0] = qemu_get_be32(f);
    s->level[1] = qemu_get_be32(f);
    return 0;
}

void sp804_init(uint32_t base, qemu_irq irq)
{
    int iomemtype;
    sp804_state *s;
    qemu_irq *qi;

    s = (sp804_state *)qemu_mallocz(sizeof(sp804_state));
    qi = qemu_allocate_irqs(sp804_set_irq, s, 2);
    s->irq = irq;
    /* ??? The timers are actually configurable between 32kHz and 1MHz, but
       we don't implement that.  */
    s->timer[0] = arm_timer_init(1000000, qi[0]);
    s->timer[1] = arm_timer_init(1000000, qi[1]);
    iomemtype = cpu_register_io_memory(0, sp804_readfn,
                                       sp804_writefn, s);
    cpu_register_physical_memory(base, 0x00001000, iomemtype);
    register_savevm("sp804", -1, 1, sp804_save, sp804_load, s);
}


/* Integrator/CP timer module.  */

typedef struct {
    void *timer[3];
} icp_pit_state;

static uint32_t icp_pit_read(void *opaque, target_phys_addr_t offset)
{
    icp_pit_state *s = (icp_pit_state *)opaque;
    int n;

    /* ??? Don't know the PrimeCell ID for this device.  */
    n = offset >> 8;
    if (n > 3)
        cpu_abort(cpu_single_env, "sp804_read: Bad timer %d\n", n);

    return arm_timer_read(s->timer[n], offset & 0xff);
}

static void icp_pit_write(void *opaque, target_phys_addr_t offset,
                          uint32_t value)
{
    icp_pit_state *s = (icp_pit_state *)opaque;
    int n;

    n = offset >> 8;
    if (n > 3)
        cpu_abort(cpu_single_env, "sp804_write: Bad timer %d\n", n);

    arm_timer_write(s->timer[n], offset & 0xff, value);
}


static CPUReadMemoryFunc *icp_pit_readfn[] = {
   icp_pit_read,
   icp_pit_read,
   icp_pit_read
};

static CPUWriteMemoryFunc *icp_pit_writefn[] = {
   icp_pit_write,
   icp_pit_write,
   icp_pit_write
};

void icp_pit_init(uint32_t base, qemu_irq *pic, int irq)
{
    int iomemtype;
    icp_pit_state *s;

    s = (icp_pit_state *)qemu_mallocz(sizeof(icp_pit_state));
    /* Timer 0 runs at the system clock speed (40MHz).  */
    s->timer[0] = arm_timer_init(40000000, pic[irq]);
    /* The other two timers run at 1MHz.  */
    s->timer[1] = arm_timer_init(1000000, pic[irq + 1]);
    s->timer[2] = arm_timer_init(1000000, pic[irq + 2]);

    iomemtype = cpu_register_io_memory(0, icp_pit_readfn,
                                       icp_pit_writefn, s);
    cpu_register_physical_memory(base, 0x00001000, iomemtype);
    /* This device has no state to save/restore.  The component timers will
       save themselves.  */
}
