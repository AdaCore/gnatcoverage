/*
 * ARM RealView Baseboard System emulation.
 *
 * Copyright (c) 2006-2007 CodeSourcery.
 * Written by Paul Brook
 *
 * This code is licenced under the GPL.
 */

#include "sysbus.h"
#include "arm-misc.h"
#include "primecell.h"
#include "devices.h"
#include "pci.h"
#include "net.h"
#include "sysemu.h"
#include "boards.h"

/* Board init.  */

static struct arm_boot_info realview_binfo = {
    .loader_start = 0x0,
    .smp_loader_start = 0x80000000,
    .board_id = 0x33b,
};

static void realview_init(ram_addr_t ram_size,
                     const char *boot_device,
                     const char *kernel_filename, const char *kernel_cmdline,
                     const char *initrd_filename, const char *cpu_model)
{
    CPUState *env;
    ram_addr_t ram_offset;
    DeviceState *dev;
    qemu_irq *irqp;
    qemu_irq pic[64];
    PCIBus *pci_bus;
    NICInfo *nd;
    int n;
    int done_smc = 0;
    qemu_irq cpu_irq[4];
    int ncpu;

    if (!cpu_model)
        cpu_model = "arm926";
    /* FIXME: obey smp_cpus.  */
    if (strcmp(cpu_model, "arm11mpcore") == 0) {
        ncpu = 4;
    } else {
        ncpu = 1;
    }

    for (n = 0; n < ncpu; n++) {
        env = cpu_init(cpu_model);
        if (!env) {
            fprintf(stderr, "Unable to find CPU definition\n");
            exit(1);
        }
        irqp = arm_pic_init_cpu(env);
        cpu_irq[n] = irqp[ARM_PIC_CPU_IRQ];
        if (n > 0) {
            /* Set entry point for secondary CPUs.  This assumes we're using
               the init code from arm_boot.c.  Real hardware resets all CPUs
               the same.  */
            env->regs[15] = 0x80000000;
        }
    }

    ram_offset = qemu_ram_alloc(ram_size);
    /* ??? RAM should repeat to fill physical memory space.  */
    /* SDRAM at address zero.  */
    cpu_register_physical_memory(0, ram_size, ram_offset | IO_MEM_RAM);

    arm_sysctl_init(0x10000000, 0xc1400400);

    if (ncpu == 1) {
        /* ??? The documentation says GIC1 is nFIQ and either GIC2 or GIC3
           is nIRQ (there are inconsistencies).  However Linux 2.6.17 expects
           GIC1 to be nIRQ and ignores all the others, so do that for now.  */
        dev = sysbus_create_simple("realview_gic", 0x10040000, cpu_irq[0]);
    } else {
        dev = sysbus_create_varargs("realview_mpcore", -1,
                                    cpu_irq[0], cpu_irq[1], cpu_irq[2],
                                    cpu_irq[3], NULL);
    }
    for (n = 0; n < 64; n++) {
        pic[n] = qdev_get_gpio_in(dev, n);
    }

    sysbus_create_simple("pl050_keyboard", 0x10006000, pic[20]);
    sysbus_create_simple("pl050_mouse", 0x10007000, pic[21]);

    sysbus_create_simple("pl011", 0x10009000, pic[12]);
    sysbus_create_simple("pl011", 0x1000a000, pic[13]);
    sysbus_create_simple("pl011", 0x1000b000, pic[14]);
    sysbus_create_simple("pl011", 0x1000c000, pic[15]);

    /* DMA controller is optional, apparently.  */
    sysbus_create_simple("pl081", 0x10030000, pic[24]);

    sysbus_create_simple("sp804", 0x10011000, pic[4]);
    sysbus_create_simple("sp804", 0x10012000, pic[5]);

    sysbus_create_simple("pl110_versatile", 0x10020000, pic[23]);

    sysbus_create_varargs("pl181", 0x10005000, pic[17], pic[18], NULL);

    sysbus_create_simple("pl031", 0x10017000, pic[10]);

    dev = sysbus_create_varargs("realview_pci", 0x60000000,
                                pic[48], pic[49], pic[50], pic[51], NULL);
    pci_bus = (PCIBus *)qdev_get_child_bus(dev, "pci");
    if (usb_enabled) {
        usb_ohci_init_pci(pci_bus, 3, -1);
    }
    n = drive_get_max_bus(IF_SCSI);
    while (n >= 0) {
        pci_create_simple(pci_bus, -1, "lsi53c895a");
        n--;
    }
    for(n = 0; n < nb_nics; n++) {
        nd = &nd_table[n];

        if ((!nd->model && !done_smc) || strcmp(nd->model, "smc91c111") == 0) {
            smc91c111_init(nd, 0x4e000000, pic[28]);
            done_smc = 1;
        } else {
            pci_nic_init(nd, "rtl8139", NULL);
        }
    }

    /* Memory map for RealView Emulation Baseboard:  */
    /* 0x10000000 System registers.  */
    /*  0x10001000 System controller.  */
    /*  0x10002000 Two-Wire Serial Bus.  */
    /* 0x10003000 Reserved.  */
    /*  0x10004000 AACI.  */
    /*  0x10005000 MCI.  */
    /* 0x10006000 KMI0.  */
    /* 0x10007000 KMI1.  */
    /*  0x10008000 Character LCD.  */
    /* 0x10009000 UART0.  */
    /* 0x1000a000 UART1.  */
    /* 0x1000b000 UART2.  */
    /* 0x1000c000 UART3.  */
    /*  0x1000d000 SSPI.  */
    /*  0x1000e000 SCI.  */
    /* 0x1000f000 Reserved.  */
    /*  0x10010000 Watchdog.  */
    /* 0x10011000 Timer 0+1.  */
    /* 0x10012000 Timer 2+3.  */
    /*  0x10013000 GPIO 0.  */
    /*  0x10014000 GPIO 1.  */
    /*  0x10015000 GPIO 2.  */
    /* 0x10016000 Reserved.  */
    /* 0x10017000 RTC.  */
    /*  0x10018000 DMC.  */
    /*  0x10019000 PCI controller config.  */
    /*  0x10020000 CLCD.  */
    /* 0x10030000 DMA Controller.  */
    /* 0x10040000 GIC1.  */
    /* 0x10050000 GIC2.  */
    /* 0x10060000 GIC3.  */
    /* 0x10070000 GIC4.  */
    /*  0x10080000 SMC.  */
    /*  0x40000000 NOR flash.  */
    /*  0x44000000 DoC flash.  */
    /*  0x48000000 SRAM.  */
    /*  0x4c000000 Configuration flash.  */
    /* 0x4e000000 Ethernet.  */
    /*  0x4f000000 USB.  */
    /*  0x50000000 PISMO.  */
    /*  0x54000000 PISMO.  */
    /*  0x58000000 PISMO.  */
    /*  0x5c000000 PISMO.  */
    /* 0x60000000 PCI.  */
    /* 0x61000000 PCI Self Config.  */
    /* 0x62000000 PCI Config.  */
    /* 0x63000000 PCI IO.  */
    /* 0x64000000 PCI mem 0.  */
    /* 0x68000000 PCI mem 1.  */
    /* 0x6c000000 PCI mem 2.  */

    /* ??? Hack to map an additional page of ram for the secondary CPU
       startup code.  I guess this works on real hardware because the
       BootROM happens to be in ROM/flash or in memory that isn't clobbered
       until after Linux boots the secondary CPUs.  */
    ram_offset = qemu_ram_alloc(0x1000);
    cpu_register_physical_memory(0x80000000, 0x1000, ram_offset | IO_MEM_RAM);

    realview_binfo.ram_size = ram_size;
    realview_binfo.kernel_filename = kernel_filename;
    realview_binfo.kernel_cmdline = kernel_cmdline;
    realview_binfo.initrd_filename = initrd_filename;
    realview_binfo.nb_cpus = ncpu;
    arm_load_kernel(first_cpu, &realview_binfo);
}

QEMUMachine realview_machine = {
    .name = "realview",
    .desc = "ARM RealView Emulation Baseboard (ARM926EJ-S)",
    .init = realview_init,
    .use_scsi = 1,
};

static void realview_machine_init(void)
{
    qemu_register_machine(&realview_machine);
}

machine_init(realview_machine_init);
