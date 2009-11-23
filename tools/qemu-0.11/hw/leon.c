/*
 * QEMU Leon2 System Emulator
 *
 * Copyright (c) 2009 AdaCore
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
#include "hw.h"
#include "qemu-timer.h"
#include "qemu-char.h"
#include "sysemu.h"
#include "boards.h"

//#define DEBUG_IO

#ifdef DEBUG_IO
#define DPRINTF(fmt, args...)                           \
    do { printf("Leon: " fmt , ##args); } while (0)
#else
#define DPRINTF(fmt, args...)
#endif

/* Default system clock.  */
#define CPU_CLK (80 * 1000 * 1000)

/* Leon registers.  */
#define MCFG1 0x00
#define MCFG2 0x04
#define MCFG3 0x08

#define CCR   0x14
#define CCR_MASK 0x00e13fff
#define CCR_INIT 0xf7100000

#define TIMC1 0x40
#define TIMR1 0x44
#define TIMCTR1 0x48
#define WDG 0x4c
#define TIMC2 0x50
#define TIMR2 0x54
#define TIMCTR2 0x58
#define SCAC 0x60
#define SCAR 0x64

#define UAD1 0x70
#define UAS1 0x74
#define UAC1 0x78
#define UASCA1 0x7c
#define UAD2 0x80
#define UAS2 0x84
#define UAC2 0x88
#define UASCA2 0x8c

#define ITMP 0x90
#define ITP 0x94
#define ITF 0x98
#define ITC 0x9c

#define IODAT 0xa0
#define IODIR 0xa4
#define IOIT 0xa8

#define UAS_DR 0x01
#define UAS_TS 0x02
#define UAS_TH 0x04
#define UAS_BR 0x08
#define UAS_OV 0x10
#define UAS_PE 0x20
#define UAS_FE 0x40

#define UAC_RE 0x01
#define UAC_TE 0x02
#define UAC_RI 0x04
#define UAC_TI 0x08
#define UAC_PS 0x10
#define UAC_PE 0x20
#define UAC_FL 0x40
#define UAC_LB 0x80
#define UAC_EC 0x100

#define TIMCTR_EN 0x01
#define TIMCTR_RL 0x02
#define TIMCTR_LD 0x04

#define PROM_FILENAME        "u-boot.bin"

#define MAX_PILS 16

static void main_cpu_reset(void *opaque)
{
    CPUState *env = opaque;

    cpu_reset(env);
    env->halted = 0;
}

typedef struct LeonUartState
{
    uint32_t uas;
    uint32_t uac;
    uint32_t uasca;
    uint8_t rcv;
    CharDriverState *chr;
    qemu_irq irq;
} LeonUartState;

struct LeonTimerState
{
    uint32_t rld;
    uint32_t ctr;

    uint32_t stopped_count; /* Count when stopped.  */
    uint32_t scar; /* Copy of iostate->scar.  */

    uint64_t load_time;
    uint64_t next_time;
    QEMUTimer *timer;
    qemu_irq irq;
};

struct LeonIntState {
    uint32_t itmp;
    uint32_t itp;
    uint32_t itf;
    CPUState *env;
};

typedef struct LeonIoState
{
    uint32_t mcfg[3];

    struct LeonIntState intctl;

    uint32_t ccr;
    uint32_t scar;
    uint32_t wdg;
    uint32_t iodata;
    uint32_t iodir;
    uint32_t ioit;
    struct LeonTimerState timr1;
    struct LeonTimerState timr2;
    LeonUartState uart1;
    LeonUartState uart2;
} LeonIoState;

static struct LeonIntState *leon_intctl;

static void leon_check_irqs(struct LeonIntState *s)
{
    uint32_t pend = (s->itp | s->itf) & s->itmp;
    uint32_t m;
    int i;
    int num = 0;
    CPUSPARCState *env = s->env;

    /* First level 1 */
    m = pend & (s->itmp >> 16);
    if (m != 0) {
	for (i = 15; i != 0; i--)
	    if (m & (1 << i)) {
		num = i;
		break;
	    }
    }
    /* Level 0 */
    if (num == 0) {
	m = pend & ~(s->itmp >> 16);
	if (m != 0) {
	    for (i = 15; i != 0; i--)
		if (m & (1 << i)) {
		    num = i;
		    break;
		}
	}
    }

#if 0
    printf ("Leon2 check interrupt: num=%d int_index=0x%02x "
	    "pend=%04x itp=%04x, itmp=%04x\n",
	    num, env->interrupt_index, pend, s->itp, s->itmp);
#endif

    if (num && (env->interrupt_index == 0 ||
		(env->interrupt_index & ~15) == TT_EXTINT)) {
	int old_interrupt = env->interrupt_index;

	env->interrupt_index = TT_EXTINT | num;
	if (old_interrupt != env->interrupt_index) {
	    DPRINTF("Set CPU IRQ %d\n", num);
	    cpu_interrupt(env, CPU_INTERRUPT_HARD);
	}
    } else if (!num && (env->interrupt_index & ~15) == TT_EXTINT) {
        DPRINTF("Reset CPU IRQ %d\n", env->interrupt_index & 15);
        env->interrupt_index = 0;
        cpu_reset_interrupt(env, CPU_INTERRUPT_HARD);
    }
}

void leon2_intctl_ack(CPUSPARCState *env, int intno)
{
    uint32_t mask;

    intno &= 15;
    mask = 1 << intno;

    DPRINTF ("intctl ack %d\n", intno);

    /* Clear registers.  */
    leon_intctl->itp &= ~mask;
    leon_intctl->itf &= ~mask;

    leon_check_irqs(leon_intctl);
}

static void leon_set_irq(void *opaque, int irq, int level)
{
    struct LeonIntState *s = opaque;

    if (level) {
        DPRINTF("Raise CPU IRQ %d\n", irq);
	s->itp = 1 << irq;
        leon_check_irqs(s);
    } else {
        DPRINTF("Lower CPU IRQ %d\n", irq);
        s->itp &= ~(1 << irq);
	leon_check_irqs(s);
    }
}

static void leon_uart_check_irq(struct LeonUartState *s)
{
    if (((s->uas & UAS_DR) && (s->uac & UAC_RI))
	|| (!(s->uas & UAS_TH) && (s->uac & UAC_TI)))
	qemu_set_irq(s->irq, 1);
    else
	qemu_set_irq(s->irq, 0);
}

static int leon_uart_can_receive(void *opaque)
{
    LeonUartState *s = opaque;
    return (s->uas & UAS_DR) ? 0 : 1;
}

static void leon_uart_receive(void *opaque, const uint8_t *buf, int size)
{
    LeonUartState *s = opaque;
    s->rcv = *buf;
    s->uas |= UAS_DR;
    leon_uart_check_irq(s);
}

static void leon_uart_event(void *opaque, int event)
{
#ifdef DEBUG_UART
    printf("Leon uart: event %x\n", event);
#endif
}

static uint32_t leon_uart_read_uad(struct LeonUartState *s)
{
    if (s->uas & UAS_DR) {
	s->uas &= ~UAS_DR;
	leon_uart_check_irq(s);
    }
    return s->rcv;
}

static void leon_uart_init (CharDriverState *chr,
			    struct LeonUartState *s, qemu_irq irq)
{
    s->chr = chr;
    s->uac = 0;
    s->irq = irq;
    s->uas = UAS_TS | UAS_TH;

    qemu_chr_add_handlers(s->chr, leon_uart_can_receive, leon_uart_receive,
                          leon_uart_event, s);
}

static void leon_timer_reload (struct LeonTimerState *s,
			       uint64_t start_time, uint32_t cnt)
{
    s->load_time = start_time;
    start_time += muldiv64((cnt + 1) * (s->scar + 1),
			   ticks_per_sec, CPU_CLK);
    qemu_mod_timer(s->timer, start_time);
    s->next_time = start_time;
}

static void leon_irq_timer(void *opaque)
{
    struct LeonTimerState *s = opaque;

    qemu_set_irq(s->irq, 1);
    if (s->ctr & TIMCTR_RL)
	leon_timer_reload (s, s->next_time, s->rld);
}

static uint32_t leon_timer_read_counter (struct LeonTimerState *s)
{
    uint32_t res;
    uint64_t t;

    if (s->ctr & TIMCTR_EN) {
	t = qemu_get_clock(vm_clock);
	if (t > s->next_time) {
	    /* Timer was not yet reloaded or it is now stopped.  */
	    return 0;
	}
	else
	    t -= s->load_time;
	res = muldiv64(t, CPU_CLK, ticks_per_sec * (s->scar + 1));
	res = s->rld + 1 - res;
    }
    else {
	res = s->stopped_count;
    }
    return res;
}

static void leon_write_timctr (struct LeonTimerState *s, uint32_t val)
{
    /* Handle LD + EN.  */
    if (val & TIMCTR_LD) {
	if (val & TIMCTR_EN)
	    leon_timer_reload (s, qemu_get_clock(vm_clock), s->rld);
	else {
	    s->stopped_count = s->rld;
	    qemu_del_timer (s->timer);
	}
    }
    else if ((val ^ s->ctr) & TIMCTR_EN) {
	if (val & TIMCTR_EN)
	    leon_timer_reload (s, qemu_get_clock(vm_clock), s->stopped_count);
	else {
	    s->stopped_count = leon_timer_read_counter (s);
	    qemu_del_timer (s->timer);
	}
    }
    /* Handle RL.  */
    s->ctr = val & (TIMCTR_EN | TIMCTR_RL);
}

static void leon_timer_init (struct LeonTimerState *s, qemu_irq irq)
{
    s->timer = qemu_new_timer(vm_clock, leon_irq_timer, s);
    s->irq = irq;
}

static uint32_t leon_io_readl(void *opaque, target_phys_addr_t addr)
{
    LeonIoState *s = opaque;
    uint32_t ret;

    switch (addr) {
    case MCFG1:
    case MCFG2:
    case MCFG3:
	ret = s->mcfg[(addr - MCFG1) >> 2];
	break;
    case CCR:
        ret = s->ccr;
        break;
    case ITMP:
	ret = s->intctl.itmp;
	break;
    case ITP:
	ret = s->intctl.itp;
	break;
    case ITF:
	ret = s->intctl.itf;
	break;
    case ITC:
	ret = 0;
	break;
    case WDG:
	ret = s->wdg;
	break;
    case SCAR:
	ret = s->scar;
	break;
    case UAD1:
	ret = leon_uart_read_uad (&s->uart1);
	break;
    case UAC1:
	ret = s->uart1.uac;
	break;
    case UASCA1:
	ret = s->uart1.uasca;
	break;
    case UAS1:
	ret = s->uart1.uas;
	break;

    case TIMR1:
	ret = s->timr1.rld;
	break;
    case TIMR2:
	ret = s->timr2.rld;
	break;
    case TIMCTR1:
	ret = s->timr1.ctr;
	break;
    case TIMCTR2:
	ret = s->timr2.ctr;
	break;
    case TIMC1:
	ret = leon_timer_read_counter (&s->timr1);
	break;
    case TIMC2:
	ret = leon_timer_read_counter (&s->timr2);
	break;

    case IODAT:
	ret = s->iodata;
	break;
    case IODIR:
	ret = s->iodir;
	break;
    case IOIT:
	ret = s->ioit;
	break;
    default:
	printf ("Leon: read unknown register 0x%04x\n", (int)addr);
	ret = 0;
	break;
	}

    DPRINTF("read reg 0x%02x = %x\n", (unsigned)addr, ret);

    return ret;
}

static void leon_io_writel(void *opaque, target_phys_addr_t addr,
			   uint32_t val)
{
    LeonIoState *s = opaque;

    DPRINTF("write reg 0x%02x = %x\n", (unsigned)addr, val);

    switch (addr) {
    case MCFG1:
    case MCFG2:
    case MCFG3:
	s->mcfg[(addr - MCFG1) >> 2] = val;
	break;
    case CCR:
        s->ccr = (val & CCR_MASK) | (s->ccr & ~CCR_MASK);
        break;
    case ITMP:
	s->intctl.itmp = val;
	break;
    case ITF:
	s->intctl.itf = val & 0xfffe;
	leon_check_irqs(&s->intctl);
	break;
    case WDG:
	s->wdg = val & 0x00ffffff;
	break;
    case SCAR:
	s->scar = val & 0x3ff;
	s->timr1.scar = s->scar;
	s->timr2.scar = s->scar;
	break;
    case UAC1:
	s->uart1.uac = val & 0x1ff;
	break;
    case UASCA1:
	s->uart1.uasca = val & 0x3ff;
	break;
    case UAD1:
        {
	    unsigned char c = val;
	    qemu_chr_write(s->uart1.chr, &c, 1);
	}
	break;

    case SCAC:
	break;
    case TIMR1:
	s->timr1.rld = val & 0x00ffffff;
	break;
    case TIMR2:
	s->timr2.rld = val & 0x00ffffff;
	break;
    case TIMC1:
	break;
    case TIMCTR1:
	leon_write_timctr (&s->timr1, val);
	break;
    case TIMCTR2:
	leon_write_timctr (&s->timr2, val);
	break;

    case IODAT:
	s->iodata = val & 0xffff;
	break;
    case IODIR:
	s->iodir = val & 0x3ffff;
	break;
    case IOIT:
	s->ioit = val;
	break;

    default:
	printf ("Leon: write unknown register 0x%04x=%x\n", (int)addr, val);
    }
}

static CPUReadMemoryFunc *leon_io_read[3] = {
    NULL,
    NULL,
    leon_io_readl
};

static CPUWriteMemoryFunc *leon_io_write[3] = {
    NULL,
    NULL,
    leon_io_writel,
};

static void at697_hw_init(ram_addr_t ram_size,
			  const char *boot_device,
			  const char *kernel_filename,
			  const char *kernel_cmdline,
			  const char *initrd_filename,
                          const char *cpu_model)
{
    CPUState *env;
    ram_addr_t ram_offset, prom_offset;
    int ret;
    char *filename;
    qemu_irq *cpu_irqs;
    int bios_size;
    int aligned_bios_size;
    int leon_io_memory;
    LeonIoState *s;

    /* init CPU */
    if (!cpu_model)
	cpu_model = "LEON2";

    env = cpu_init(cpu_model);
    if (!env) {
        fprintf(stderr, "qemu: Unable to find Sparc CPU definition\n");
        exit(1);
    }

    cpu_sparc_set_intctl(env, intctl_leon2);
    cpu_sparc_set_id(env, 0);

    qemu_register_reset(main_cpu_reset, env);

    s = qemu_mallocz(sizeof(struct LeonIoState));
    leon_intctl = &s->intctl;
    leon_intctl->env = env;
    s->ccr = CCR_INIT;

    cpu_irqs = qemu_allocate_irqs(leon_set_irq, leon_intctl, MAX_PILS);

    /* allocate RAM */
    if ((uint64_t)ram_size > (1UL << 30)) {
        fprintf(stderr,
                "qemu: Too much memory for this machine: %d, maximum 1G\n",
                (unsigned int)(ram_size / (1024 * 1024)));
        exit(1);
    }
    ram_offset = qemu_ram_alloc(ram_size);
    cpu_register_physical_memory(0x40000000, ram_size, ram_offset);

    /* load boot prom */
    if (bios_name == NULL)
        bios_name = PROM_FILENAME;
    filename = qemu_find_file(QEMU_FILE_TYPE_BIOS, bios_name);
    bios_size = get_image_size(filename);
    if (bios_size > 0) {
	aligned_bios_size =
	    (bios_size + TARGET_PAGE_SIZE - 1) & TARGET_PAGE_MASK;
	prom_offset = qemu_ram_alloc(aligned_bios_size);
	cpu_register_physical_memory(0x00000000, aligned_bios_size,
				     prom_offset /* | IO_MEM_ROM */);
        ret = load_image_targphys(filename, 0x00000000, bios_size);
	if (ret < 0 || ret > bios_size) {
	    fprintf(stderr, "qemu: could not load prom '%s'\n", filename);
	    exit(1);
	}
    }
    else if (kernel_filename == NULL) {
      fprintf(stderr,"Can't read bios image %s\n", filename);
      exit(1);
    }

    leon_io_memory = cpu_register_io_memory(leon_io_read, leon_io_write, s);
    cpu_register_physical_memory(0x80000000, 0x1000, leon_io_memory);

    leon_timer_init (&s->timr1, cpu_irqs[8]);
    leon_timer_init (&s->timr2, cpu_irqs[9]);

    if (serial_hds[0])
	leon_uart_init (serial_hds[0], &s->uart1, cpu_irqs[3]);
    if (serial_hds[1])
	leon_uart_init (serial_hds[1], &s->uart2, cpu_irqs[2]);

    /* Can directly load an application. */
    if (kernel_filename != NULL) {
	long kernel_size;
	uint64_t entry;

	kernel_size = load_elf(kernel_filename, 0, &entry, NULL, NULL);
	if (kernel_size < 0) {
	    fprintf(stderr, "qemu: could not load kernel '%s'\n",
		    kernel_filename);
	    exit(1);
	}
	if (bios_size <= 0) {
	    /* If there is no bios/monitor, start the application.  */
	    env->pc = entry;
	    env->npc = entry + 4;
	}
    }
}

QEMUMachine at697_machine = {
    .name = "at697",
    .desc = "Leon-2 Atmel 697",
    .init = at697_hw_init,
    .use_scsi = 0,
};

static void leon_machine_init(void)
{
    qemu_register_machine(&at697_machine);
}

machine_init(leon_machine_init);
