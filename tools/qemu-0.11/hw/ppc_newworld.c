/*
 * QEMU PowerPC CHRP (currently NewWorld PowerMac) hardware System Emulator
 *
 * Copyright (c) 2004-2007 Fabrice Bellard
 * Copyright (c) 2007 Jocelyn Mayer
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
#include "ppc.h"
#include "ppc_mac.h"
#include "mac_dbdma.h"
#include "nvram.h"
#include "pc.h"
#include "pci.h"
#include "net.h"
#include "sysemu.h"
#include "boards.h"
#include "fw_cfg.h"
#include "escc.h"
#include "openpic.h"

#define MAX_IDE_BUS 2
#define VGA_BIOS_SIZE 65536
#define CFG_ADDR 0xf0000510

/* debug UniNorth */
//#define DEBUG_UNIN

#ifdef DEBUG_UNIN
#define UNIN_DPRINTF(fmt, ...)                                  \
    do { printf("UNIN: " fmt , ## __VA_ARGS__); } while (0)
#else
#define UNIN_DPRINTF(fmt, ...)
#endif

/* UniN device */
static void unin_writel (void *opaque, target_phys_addr_t addr, uint32_t value)
{
    UNIN_DPRINTF("writel addr " TARGET_FMT_plx " val %x\n", addr, value);
}

static uint32_t unin_readl (void *opaque, target_phys_addr_t addr)
{
    uint32_t value;

    value = 0;
    UNIN_DPRINTF("readl addr " TARGET_FMT_plx " val %x\n", addr, value);

    return value;
}

static CPUWriteMemoryFunc *unin_write[] = {
    &unin_writel,
    &unin_writel,
    &unin_writel,
};

static CPUReadMemoryFunc *unin_read[] = {
    &unin_readl,
    &unin_readl,
    &unin_readl,
};

static int fw_cfg_boot_set(void *opaque, const char *boot_device)
{
    fw_cfg_add_i16(opaque, FW_CFG_BOOT_DEVICE, boot_device[0]);
    return 0;
}

/* PowerPC Mac99 hardware initialisation */
static void ppc_core99_init (ram_addr_t ram_size,
                             const char *boot_device,
                             const char *kernel_filename,
                             const char *kernel_cmdline,
                             const char *initrd_filename,
                             const char *cpu_model)
{
    CPUState *env = NULL, *envs[MAX_CPUS];
    char *filename;
    qemu_irq *pic, **openpic_irqs;
    int unin_memory;
    int linux_boot, i;
    ram_addr_t ram_offset, bios_offset, vga_bios_offset;
    uint32_t kernel_base, kernel_size, initrd_base, initrd_size;
    PCIBus *pci_bus;
    MacIONVRAMState *nvr;
    int nvram_mem_index;
    int vga_bios_size, bios_size;
    qemu_irq *dummy_irq;
    int pic_mem_index, dbdma_mem_index, cuda_mem_index, escc_mem_index;
    int ppc_boot_device;
    int index;
    BlockDriverState *hd[MAX_IDE_BUS * MAX_IDE_DEVS];
    void *fw_cfg;
    void *dbdma;
    uint8_t *vga_bios_ptr;

    linux_boot = (kernel_filename != NULL);

    /* init CPUs */
    if (cpu_model == NULL)
        cpu_model = "G4";
    for (i = 0; i < smp_cpus; i++) {
        env = cpu_init(cpu_model);
        if (!env) {
            fprintf(stderr, "Unable to find PowerPC CPU definition\n");
            exit(1);
        }
        /* Set time-base frequency to 100 Mhz */
        cpu_ppc_tb_init(env, 100UL * 1000UL * 1000UL);
#if 0
        env->osi_call = vga_osi_call;
#endif
        qemu_register_reset(&cpu_ppc_reset, env);
        envs[i] = env;
    }

    /* allocate RAM */
    ram_offset = qemu_ram_alloc(ram_size);
    cpu_register_physical_memory(0, ram_size, ram_offset);

    /* allocate and load BIOS */
    bios_offset = qemu_ram_alloc(BIOS_SIZE);
    if (bios_name == NULL)
        bios_name = PROM_FILENAME;
    filename = qemu_find_file(QEMU_FILE_TYPE_BIOS, bios_name);
    cpu_register_physical_memory(PROM_ADDR, BIOS_SIZE, bios_offset | IO_MEM_ROM);

    /* Load OpenBIOS (ELF) */
    if (filename) {
        bios_size = load_elf(filename, 0, NULL, NULL, NULL);
        qemu_free(filename);
    } else {
        bios_size = -1;
    }
    if (bios_size < 0 || bios_size > BIOS_SIZE) {
        hw_error("qemu: could not load PowerPC bios '%s'\n", bios_name);
        exit(1);
    }

    /* allocate and load VGA BIOS */
    vga_bios_offset = qemu_ram_alloc(VGA_BIOS_SIZE);
    vga_bios_ptr = qemu_get_ram_ptr(vga_bios_offset);
    filename = qemu_find_file(QEMU_FILE_TYPE_BIOS, VGABIOS_FILENAME);
    if (filename) {
        vga_bios_size = load_image(filename, vga_bios_ptr + 8);
        qemu_free(filename);
    } else {
        vga_bios_size = -1;
    }
    if (vga_bios_size < 0) {
        /* if no bios is present, we can still work */
        fprintf(stderr, "qemu: warning: could not load VGA bios '%s'\n",
                VGABIOS_FILENAME);
        vga_bios_size = 0;
    } else {
        /* set a specific header (XXX: find real Apple format for NDRV
           drivers) */
        vga_bios_ptr[0] = 'N';
        vga_bios_ptr[1] = 'D';
        vga_bios_ptr[2] = 'R';
        vga_bios_ptr[3] = 'V';
        cpu_to_be32w((uint32_t *)(vga_bios_ptr + 4), vga_bios_size);
        vga_bios_size += 8;
    }

    if (linux_boot) {
        uint64_t lowaddr = 0;
        kernel_base = KERNEL_LOAD_ADDR;

        /* Now we can load the kernel. The first step tries to load the kernel
           supposing PhysAddr = 0x00000000. If that was wrong the kernel is
           loaded again, the new PhysAddr being computed from lowaddr. */
        kernel_size = load_elf(kernel_filename, kernel_base, NULL, &lowaddr, NULL);
        if (kernel_size > 0 && lowaddr != KERNEL_LOAD_ADDR) {
            kernel_size = load_elf(kernel_filename, (2 * kernel_base) - lowaddr,
                                   NULL, NULL, NULL);
        }
        if (kernel_size < 0)
            kernel_size = load_aout(kernel_filename, kernel_base,
                                    ram_size - kernel_base);
        if (kernel_size < 0)
            kernel_size = load_image_targphys(kernel_filename,
                                              kernel_base,
                                              ram_size - kernel_base);
        if (kernel_size < 0) {
            hw_error("qemu: could not load kernel '%s'\n", kernel_filename);
            exit(1);
        }
        /* load initrd */
        if (initrd_filename) {
            initrd_base = INITRD_LOAD_ADDR;
            initrd_size = load_image_targphys(initrd_filename, initrd_base,
                                              ram_size - initrd_base);
            if (initrd_size < 0) {
                hw_error("qemu: could not load initial ram disk '%s'\n",
                         initrd_filename);
                exit(1);
            }
        } else {
            initrd_base = 0;
            initrd_size = 0;
        }
        ppc_boot_device = 'm';
    } else {
        kernel_base = 0;
        kernel_size = 0;
        initrd_base = 0;
        initrd_size = 0;
        ppc_boot_device = '\0';
        /* We consider that NewWorld PowerMac never have any floppy drive
         * For now, OHW cannot boot from the network.
         */
        for (i = 0; boot_device[i] != '\0'; i++) {
            if (boot_device[i] >= 'c' && boot_device[i] <= 'f') {
                ppc_boot_device = boot_device[i];
                break;
            }
        }
        if (ppc_boot_device == '\0') {
            fprintf(stderr, "No valid boot device for Mac99 machine\n");
            exit(1);
        }
    }

    isa_mem_base = 0x80000000;

    /* Register 8 MB of ISA IO space */
    isa_mmio_init(0xf2000000, 0x00800000);

    /* UniN init */
    unin_memory = cpu_register_io_memory(unin_read, unin_write, NULL);
    cpu_register_physical_memory(0xf8000000, 0x00001000, unin_memory);

    openpic_irqs = qemu_mallocz(smp_cpus * sizeof(qemu_irq *));
    openpic_irqs[0] =
        qemu_mallocz(smp_cpus * sizeof(qemu_irq) * OPENPIC_OUTPUT_NB);
    for (i = 0; i < smp_cpus; i++) {
        /* Mac99 IRQ connection between OpenPIC outputs pins
         * and PowerPC input pins
         */
        switch (PPC_INPUT(env)) {
        case PPC_FLAGS_INPUT_6xx:
            openpic_irqs[i] = openpic_irqs[0] + (i * OPENPIC_OUTPUT_NB);
            openpic_irqs[i][OPENPIC_OUTPUT_INT] =
                ((qemu_irq *)env->irq_inputs)[PPC6xx_INPUT_INT];
            openpic_irqs[i][OPENPIC_OUTPUT_CINT] =
                ((qemu_irq *)env->irq_inputs)[PPC6xx_INPUT_INT];
            openpic_irqs[i][OPENPIC_OUTPUT_MCK] =
                ((qemu_irq *)env->irq_inputs)[PPC6xx_INPUT_MCP];
            /* Not connected ? */
            openpic_irqs[i][OPENPIC_OUTPUT_DEBUG] = NULL;
            /* Check this */
            openpic_irqs[i][OPENPIC_OUTPUT_RESET] =
                ((qemu_irq *)env->irq_inputs)[PPC6xx_INPUT_HRESET];
            break;
#if defined(TARGET_PPC64)
        case PPC_FLAGS_INPUT_970:
            openpic_irqs[i] = openpic_irqs[0] + (i * OPENPIC_OUTPUT_NB);
            openpic_irqs[i][OPENPIC_OUTPUT_INT] =
                ((qemu_irq *)env->irq_inputs)[PPC970_INPUT_INT];
            openpic_irqs[i][OPENPIC_OUTPUT_CINT] =
                ((qemu_irq *)env->irq_inputs)[PPC970_INPUT_INT];
            openpic_irqs[i][OPENPIC_OUTPUT_MCK] =
                ((qemu_irq *)env->irq_inputs)[PPC970_INPUT_MCP];
            /* Not connected ? */
            openpic_irqs[i][OPENPIC_OUTPUT_DEBUG] = NULL;
            /* Check this */
            openpic_irqs[i][OPENPIC_OUTPUT_RESET] =
                ((qemu_irq *)env->irq_inputs)[PPC970_INPUT_HRESET];
            break;
#endif /* defined(TARGET_PPC64) */
        default:
            hw_error("Bus model not supported on mac99 machine\n");
            exit(1);
        }
    }
    pic = openpic_init(NULL, &pic_mem_index, smp_cpus, openpic_irqs, NULL);
    pci_bus = pci_pmac_init(pic);
    /* init basic PC hardware */
    pci_vga_init(pci_bus, vga_bios_offset, vga_bios_size);

    /* XXX: suppress that */
    dummy_irq = i8259_init(NULL);

    escc_mem_index = escc_init(0x80013000, dummy_irq[4], dummy_irq[5],
                               serial_hds[0], serial_hds[1], ESCC_CLOCK, 4);

    for(i = 0; i < nb_nics; i++)
        pci_nic_init(&nd_table[i], "ne2k_pci", NULL);

    if (drive_get_max_bus(IF_IDE) >= MAX_IDE_BUS) {
        fprintf(stderr, "qemu: too many IDE bus\n");
        exit(1);
    }
    for(i = 0; i < MAX_IDE_BUS * MAX_IDE_DEVS; i++) {
        index = drive_get_index(IF_IDE, i / MAX_IDE_DEVS, i % MAX_IDE_DEVS);
        if (index != -1)
            hd[i] = drives_table[index].bdrv;
        else
            hd[i] = NULL;
    }
    dbdma = DBDMA_init(&dbdma_mem_index);
    pci_cmd646_ide_init(pci_bus, hd, 0);

    /* cuda also initialize ADB */
    cuda_init(&cuda_mem_index, pic[0x19]);

    adb_kbd_init(&adb_bus);
    adb_mouse_init(&adb_bus);


    macio_init(pci_bus, PCI_DEVICE_ID_APPLE_UNI_N_KEYL, 0, pic_mem_index,
               dbdma_mem_index, cuda_mem_index, NULL, 0, NULL,
               escc_mem_index);

    if (usb_enabled) {
        usb_ohci_init_pci(pci_bus, 3, -1);
    }

    if (graphic_depth != 15 && graphic_depth != 32 && graphic_depth != 8)
        graphic_depth = 15;

    /* The NewWorld NVRAM is not located in the MacIO device */
    nvr = macio_nvram_init(&nvram_mem_index, 0x2000, 1);
    pmac_format_nvram_partition(nvr, 0x2000);
    macio_nvram_map(nvr, 0xFFF04000);
    /* No PCI init: the BIOS will do it */

    fw_cfg = fw_cfg_init(0, 0, CFG_ADDR, CFG_ADDR + 2);
    fw_cfg_add_i32(fw_cfg, FW_CFG_ID, 1);
    fw_cfg_add_i64(fw_cfg, FW_CFG_RAM_SIZE, (uint64_t)ram_size);
    fw_cfg_add_i16(fw_cfg, FW_CFG_MACHINE_ID, ARCH_MAC99);
    fw_cfg_add_i32(fw_cfg, FW_CFG_KERNEL_ADDR, kernel_base);
    fw_cfg_add_i32(fw_cfg, FW_CFG_KERNEL_SIZE, kernel_size);
    if (kernel_cmdline) {
        fw_cfg_add_i32(fw_cfg, FW_CFG_KERNEL_CMDLINE, CMDLINE_ADDR);
        pstrcpy_targphys(CMDLINE_ADDR, TARGET_PAGE_SIZE, kernel_cmdline);
    } else {
        fw_cfg_add_i32(fw_cfg, FW_CFG_KERNEL_CMDLINE, 0);
    }
    fw_cfg_add_i32(fw_cfg, FW_CFG_INITRD_ADDR, initrd_base);
    fw_cfg_add_i32(fw_cfg, FW_CFG_INITRD_SIZE, initrd_size);
    fw_cfg_add_i16(fw_cfg, FW_CFG_BOOT_DEVICE, ppc_boot_device);
    qemu_register_boot_set(fw_cfg_boot_set, fw_cfg);
}

static QEMUMachine core99_machine = {
    .name = "mac99",
    .desc = "Mac99 based PowerMAC",
    .init = ppc_core99_init,
    .max_cpus = MAX_CPUS,
};

static void core99_machine_init(void)
{
    qemu_register_machine(&core99_machine);
}

machine_init(core99_machine_init);
