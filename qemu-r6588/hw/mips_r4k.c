/*
 * QEMU/MIPS pseudo-board
 *
 * emulates a simple machine with ISA-like bus.
 * ISA IO space mapped to the 0x14000000 (PHYS) and
 * ISA memory at the 0x10000000 (PHYS, 16Mb in size).
 * All peripherial devices are attached to this "bus" with
 * the standard PC ISA addresses.
*/
#include "hw.h"
#include "mips.h"
#include "pc.h"
#include "isa.h"
#include "net.h"
#include "sysemu.h"
#include "boards.h"
#include "flash.h"
#include "qemu-log.h"

#ifdef TARGET_WORDS_BIGENDIAN
#define BIOS_FILENAME "mips_bios.bin"
#else
#define BIOS_FILENAME "mipsel_bios.bin"
#endif

#define PHYS_TO_VIRT(x) ((x) | ~(target_ulong)0x7fffffff)

#define VIRT_TO_PHYS_ADDEND (-((int64_t)(int32_t)0x80000000))

#define MAX_IDE_BUS 2

static const int ide_iobase[2] = { 0x1f0, 0x170 };
static const int ide_iobase2[2] = { 0x3f6, 0x376 };
static const int ide_irq[2] = { 14, 15 };

static int serial_io[MAX_SERIAL_PORTS] = { 0x3f8, 0x2f8, 0x3e8, 0x2e8 };
static int serial_irq[MAX_SERIAL_PORTS] = { 4, 3, 4, 3 };

static PITState *pit; /* PIT i8254 */

/* i8254 PIT is attached to the IRQ0 at PIC i8259 */

static struct _loaderparams {
    int ram_size;
    const char *kernel_filename;
    const char *kernel_cmdline;
    const char *initrd_filename;
} loaderparams;

static void mips_qemu_writel (void *opaque, target_phys_addr_t addr,
			      uint32_t val)
{
    if ((addr & 0xffff) == 0 && val == 42)
        qemu_system_reset_request ();
    else if ((addr & 0xffff) == 4 && val == 42)
        qemu_system_shutdown_request ();
}

static uint32_t mips_qemu_readl (void *opaque, target_phys_addr_t addr)
{
    return 0;
}

static CPUWriteMemoryFunc *mips_qemu_write[] = {
    &mips_qemu_writel,
    &mips_qemu_writel,
    &mips_qemu_writel,
};

static CPUReadMemoryFunc *mips_qemu_read[] = {
    &mips_qemu_readl,
    &mips_qemu_readl,
    &mips_qemu_readl,
};

static int mips_qemu_iomemtype = 0;

static void load_kernel (CPUState *env)
{
    int64_t entry, kernel_low, kernel_high;
    long kernel_size, initrd_size;
    ram_addr_t initrd_offset;

    kernel_size = load_elf(loaderparams.kernel_filename, VIRT_TO_PHYS_ADDEND,
                           (uint64_t *)&entry, (uint64_t *)&kernel_low,
                           (uint64_t *)&kernel_high);
    if (kernel_size >= 0) {
        if ((entry & ~0x7fffffffULL) == 0x80000000)
            entry = (int32_t)entry;
        env->active_tc.PC = entry;
    } else {
        fprintf(stderr, "qemu: could not load kernel '%s'\n",
                loaderparams.kernel_filename);
        exit(1);
    }

    /* load initrd */
    initrd_size = 0;
    initrd_offset = 0;
    if (loaderparams.initrd_filename) {
        initrd_size = get_image_size (loaderparams.initrd_filename);
        if (initrd_size > 0) {
            initrd_offset = (kernel_high + ~TARGET_PAGE_MASK) & TARGET_PAGE_MASK;
            if (initrd_offset + initrd_size > ram_size) {
                fprintf(stderr,
                        "qemu: memory too small for initial ram disk '%s'\n",
                        loaderparams.initrd_filename);
                exit(1);
            }
            initrd_size = load_image(loaderparams.initrd_filename,
                                     phys_ram_base + initrd_offset);
        }
        if (initrd_size == (target_ulong) -1) {
            fprintf(stderr, "qemu: could not load initial ram disk '%s'\n",
                    loaderparams.initrd_filename);
            exit(1);
        }
    }

    /* Store command line.  */
    if (initrd_size > 0) {
        int ret;
        ret = sprintf((char *)(phys_ram_base + (16 << 20) - 256),
                      "rd_start=0x" TARGET_FMT_lx " rd_size=%li ",
                      PHYS_TO_VIRT((uint32_t)initrd_offset),
                      initrd_size);
        strcpy ((char *)(phys_ram_base + (16 << 20) - 256 + ret),
                loaderparams.kernel_cmdline);
    }
    else {
        strcpy ((char *)(phys_ram_base + (16 << 20) - 256),
                loaderparams.kernel_cmdline);
    }

    *(int32_t *)(phys_ram_base + (16 << 20) - 260) = tswap32 (0x12345678);
    *(int32_t *)(phys_ram_base + (16 << 20) - 264) = tswap32 (ram_size);
}

static void main_cpu_reset(void *opaque)
{
    CPUState *env = opaque;
    cpu_reset(env);

    if (loaderparams.kernel_filename)
        load_kernel (env);
}

static const int sector_len = 32 * 1024;
static
void mips_r4k_init (ram_addr_t ram_size, int vga_ram_size,
                    const char *boot_device,
                    const char *kernel_filename, const char *kernel_cmdline,
                    const char *initrd_filename, const char *cpu_model)
{
    char buf[1024];
    unsigned long bios_offset;
    int bios_size;
    CPUState *env;
    RTCState *rtc_state;
    int i;
    qemu_irq *i8259;
    int index;
    BlockDriverState *hd[MAX_IDE_BUS * MAX_IDE_DEVS];

    /* init CPUs */
    if (cpu_model == NULL) {
#ifdef TARGET_MIPS64
        cpu_model = "R4000";
#else
        cpu_model = "24Kf";
#endif
    }
    env = cpu_init(cpu_model);
    if (!env) {
        fprintf(stderr, "Unable to find CPU definition\n");
        exit(1);
    }
    qemu_register_reset(main_cpu_reset, env);

    /* allocate RAM */
    if (ram_size > (256 << 20)) {
        fprintf(stderr,
                "qemu: Too much memory for this machine: %d MB, maximum 256 MB\n",
                ((unsigned int)ram_size / (1 << 20)));
        exit(1);
    }
    cpu_register_physical_memory(0, ram_size, IO_MEM_RAM);

    if (!mips_qemu_iomemtype) {
        mips_qemu_iomemtype = cpu_register_io_memory(0, mips_qemu_read,
                                                     mips_qemu_write, NULL);
    }
    cpu_register_physical_memory(0x1fbf0000, 0x10000, mips_qemu_iomemtype);

    /* Try to load a BIOS image. If this fails, we continue regardless,
       but initialize the hardware ourselves. When a kernel gets
       preloaded we also initialize the hardware, since the BIOS wasn't
       run. */
    bios_offset = ram_size + vga_ram_size;
    if (bios_name == NULL)
        bios_name = BIOS_FILENAME;
    snprintf(buf, sizeof(buf), "%s/%s", bios_dir, bios_name);
    bios_size = load_image(buf, phys_ram_base + bios_offset);
    if ((bios_size > 0) && (bios_size <= BIOS_SIZE)) {
	cpu_register_physical_memory(0x1fc00000,
				     BIOS_SIZE, bios_offset | IO_MEM_ROM);
    } else if ((index = drive_get_index(IF_PFLASH, 0, 0)) > -1) {
        uint32_t mips_rom = 0x00400000;
        cpu_register_physical_memory(0x1fc00000, mips_rom,
	                     qemu_ram_alloc(mips_rom) | IO_MEM_ROM);
        if (!pflash_cfi01_register(0x1fc00000, qemu_ram_alloc(mips_rom),
            drives_table[index].bdrv, sector_len, mips_rom / sector_len,
            4, 0, 0, 0, 0)) {
            fprintf(stderr, "qemu: Error registering flash memory.\n");
	}
    }
    else {
	/* not fatal */
        fprintf(stderr, "qemu: Warning, could not load MIPS bios '%s'\n",
		buf);
    }

    if (kernel_filename) {
        loaderparams.ram_size = ram_size;
        loaderparams.kernel_filename = kernel_filename;
        loaderparams.kernel_cmdline = kernel_cmdline;
        loaderparams.initrd_filename = initrd_filename;
        load_kernel (env);
    }

    /* Init CPU internal devices */
    cpu_mips_irq_init_cpu(env);
    cpu_mips_clock_init(env);

    /* The PIC is attached to the MIPS CPU INT0 pin */
    i8259 = i8259_init(env->irq[2]);

    rtc_state = rtc_init(0x70, i8259[8], 2000);

    /* Register 64 KB of ISA IO space at 0x14000000 */
    isa_mmio_init(0x14000000, 0x00010000);
    isa_mem_base = 0x10000000;

    pit = pit_init(0x40, i8259[0]);

    for(i = 0; i < MAX_SERIAL_PORTS; i++) {
        if (serial_hds[i]) {
            serial_init(serial_io[i], i8259[serial_irq[i]], 115200,
                        serial_hds[i]);
        }
    }

    isa_vga_init(phys_ram_base + ram_size, ram_size,
                 vga_ram_size);

    if (nd_table[0].vlan)
        isa_ne2000_init(0x300, i8259[9], &nd_table[0]);

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

    for(i = 0; i < MAX_IDE_BUS; i++)
        isa_ide_init(ide_iobase[i], ide_iobase2[i], i8259[ide_irq[i]],
                     hd[MAX_IDE_DEVS * i],
		     hd[MAX_IDE_DEVS * i + 1]);

    i8042_init(i8259[1], i8259[12], 0x60);
}

QEMUMachine mips_machine = {
    .name = "mips",
    .desc = "mips r4k platform",
    .init = mips_r4k_init,
    .ram_require = VGA_RAM_SIZE + BIOS_SIZE,
    .nodisk_ok = 1,
};
