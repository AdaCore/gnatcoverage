/* vim:set shiftwidth=4 ts=4 et: */
/*
 * PXA255 Sharp Zaurus SL-6000 PDA platform
 *
 * Copyright (c) 2008 Dmitry Baryshkov
 *
 * Code based on spitz platform by Andrzej Zaborowski <balrog@zabor.org>
 * This code is licensed under the GNU GPL v2.
 */

#include "hw.h"
#include "pxa.h"
#include "arm-misc.h"
#include "sysemu.h"
#include "devices.h"
#include "sharpsl.h"
#include "pcmcia.h"
#include "block.h"
#include "boards.h"
#include "i2c.h"

#define TOSA_RAM    0x04000000
#define TOSA_ROM	0x00800000

#define TOSA_GPIO_nSD_DETECT	(9)
#define TOSA_GPIO_ON_RESET		(19)
#define TOSA_GPIO_CF_IRQ		(21)	/* CF slot0 Ready */
#define TOSA_GPIO_CF_CD			(13)
#define TOSA_GPIO_TC6393XB_INT  (15)
#define TOSA_GPIO_JC_CF_IRQ		(36)	/* CF slot1 Ready */

#define TOSA_SCOOP_GPIO_BASE	1
#define TOSA_GPIO_IR_POWERDWN	(TOSA_SCOOP_GPIO_BASE + 2)
#define TOSA_GPIO_SD_WP			(TOSA_SCOOP_GPIO_BASE + 3)
#define TOSA_GPIO_PWR_ON		(TOSA_SCOOP_GPIO_BASE + 4)

#define TOSA_SCOOP_JC_GPIO_BASE		1
#define TOSA_GPIO_BT_LED		(TOSA_SCOOP_JC_GPIO_BASE + 0)
#define TOSA_GPIO_NOTE_LED		(TOSA_SCOOP_JC_GPIO_BASE + 1)
#define TOSA_GPIO_CHRG_ERR_LED		(TOSA_SCOOP_JC_GPIO_BASE + 2)
#define TOSA_GPIO_TC6393XB_L3V_ON	(TOSA_SCOOP_JC_GPIO_BASE + 5)
#define TOSA_GPIO_WLAN_LED		(TOSA_SCOOP_JC_GPIO_BASE + 7)

#define	DAC_BASE	0x4e
#define DAC_CH1		0
#define DAC_CH2		1

static void tosa_microdrive_attach(struct pxa2xx_state_s *cpu)
{
    struct pcmcia_card_s *md;
    int index;
    BlockDriverState *bs;

    index = drive_get_index(IF_IDE, 0, 0);
    if (index == -1)
        return;
    bs = drives_table[index].bdrv;
    if (bdrv_is_inserted(bs) && !bdrv_is_removable(bs)) {
        md = dscm1xxxx_init(bs);
        pxa2xx_pcmcia_attach(cpu->pcmcia[0], md);
    }
}

static void tosa_out_switch(void *opaque, int line, int level)
{
    switch (line) {
        case 0:
            fprintf(stderr, "blue LED %s.\n", level ? "on" : "off");
            break;
        case 1:
            fprintf(stderr, "green LED %s.\n", level ? "on" : "off");
            break;
        case 2:
            fprintf(stderr, "amber LED %s.\n", level ? "on" : "off");
            break;
        case 3:
            fprintf(stderr, "wlan LED %s.\n", level ? "on" : "off");
            break;
        default:
            fprintf(stderr, "Uhandled out event: %d = %d\n", line, level);
            break;
    }
}


static void tosa_gpio_setup(struct pxa2xx_state_s *cpu,
                struct scoop_info_s *scp0,
                struct scoop_info_s *scp1,
                struct tc6393xb_s *tmio)
{
    qemu_irq *outsignals = qemu_allocate_irqs(tosa_out_switch, cpu, 4);
    /* MMC/SD host */
    pxa2xx_mmci_handlers(cpu->mmc,
                    scoop_gpio_in_get(scp0)[TOSA_GPIO_SD_WP],
                    qemu_irq_invert(pxa2xx_gpio_in_get(cpu->gpio)[TOSA_GPIO_nSD_DETECT]));

    /* Handle reset */
    pxa2xx_gpio_out_set(cpu->gpio, TOSA_GPIO_ON_RESET, cpu->reset);

    /* PCMCIA signals: card's IRQ and Card-Detect */
    pxa2xx_pcmcia_set_irq_cb(cpu->pcmcia[0],
                        pxa2xx_gpio_in_get(cpu->gpio)[TOSA_GPIO_CF_IRQ],
                        pxa2xx_gpio_in_get(cpu->gpio)[TOSA_GPIO_CF_CD]);

    pxa2xx_pcmcia_set_irq_cb(cpu->pcmcia[1],
                        pxa2xx_gpio_in_get(cpu->gpio)[TOSA_GPIO_JC_CF_IRQ],
                        NULL);

    scoop_gpio_out_set(scp1, TOSA_GPIO_BT_LED, outsignals[0]);
    scoop_gpio_out_set(scp1, TOSA_GPIO_NOTE_LED, outsignals[1]);
    scoop_gpio_out_set(scp1, TOSA_GPIO_CHRG_ERR_LED, outsignals[2]);
    scoop_gpio_out_set(scp1, TOSA_GPIO_WLAN_LED, outsignals[3]);

    scoop_gpio_out_set(scp1, TOSA_GPIO_TC6393XB_L3V_ON, tc6393xb_l3v_get(tmio));
}

static uint32_t tosa_ssp_read(void *opaque)
{
    return 0;
}

static void tosa_ssp_write(void *opaque, uint32_t value)
{
    fprintf(stderr, "TG: %d %02x\n", value >> 5, value & 0x1f);
}

struct tosa_dac_i2c {
    i2c_slave i2c;
    int len;
    char buf[3];
};

static int tosa_dac_send(i2c_slave *i2c, uint8_t data)
{
    struct tosa_dac_i2c *s = (struct tosa_dac_i2c *)i2c;
    s->buf[s->len] = data;
    if (s->len ++ > 2) {
#ifdef VERBOSE
        fprintf(stderr, "%s: message too long (%i bytes)\n", __FUNCTION__, s->len);
#endif
        return 1;
    }

    if (s->len == 2) {
        fprintf(stderr, "dac: channel %d value 0x%02x\n",
                s->buf[0], s->buf[1]);
    }

    return 0;
}

static void tosa_dac_event(i2c_slave *i2c, enum i2c_event event)
{
    struct tosa_dac_i2c *s = (struct tosa_dac_i2c *)i2c;
    s->len = 0;
    switch (event) {
    case I2C_START_SEND:
        break;
    case I2C_START_RECV:
        printf("%s: recv not supported!!!\n", __FUNCTION__);
        break;
    case I2C_FINISH:
#ifdef VERBOSE
        if (s->len < 2)
            printf("%s: message too short (%i bytes)\n", __FUNCTION__, s->len);
        if (s->len > 2)
            printf("%s: message too long\n", __FUNCTION__);
#endif
        break;
    default:
        break;
    }
}

static int tosa_dac_recv(i2c_slave *s)
{
    printf("%s: recv not supported!!!\n", __FUNCTION__);
    return -1;
}

static void tosa_tg_init(struct pxa2xx_state_s *cpu)
{
    struct i2c_bus *bus = pxa2xx_i2c_bus(cpu->i2c[0]);
    struct i2c_slave *dac = i2c_slave_init(bus, 0, sizeof(struct tosa_dac_i2c));
    dac->send = tosa_dac_send;
    dac->event = tosa_dac_event;
    dac->recv = tosa_dac_recv;
    i2c_set_slave_address(dac, DAC_BASE);
    pxa2xx_ssp_attach(cpu->ssp[1], tosa_ssp_read,
                    tosa_ssp_write, cpu);
}


static struct arm_boot_info tosa_binfo = {
    .loader_start = PXA2XX_SDRAM_BASE,
    .ram_size = 0x04000000,
};

static void tosa_init(ram_addr_t ram_size, int vga_ram_size,
                const char *boot_device,
                const char *kernel_filename, const char *kernel_cmdline,
                const char *initrd_filename, const char *cpu_model)
{
    struct pxa2xx_state_s *cpu;
    struct tc6393xb_s *tmio;
    struct scoop_info_s *scp0, *scp1;

    if (ram_size < (TOSA_RAM + TOSA_ROM + PXA2XX_INTERNAL_SIZE + TC6393XB_RAM)) {
        fprintf(stderr, "This platform requires %i bytes of memory\n",
                TOSA_RAM + TOSA_ROM + PXA2XX_INTERNAL_SIZE);
        exit(1);
    }

    if (!cpu_model)
        cpu_model = "pxa255";

    cpu = pxa255_init(tosa_binfo.ram_size);

    cpu_register_physical_memory(0, TOSA_ROM,
                    qemu_ram_alloc(TOSA_ROM) | IO_MEM_ROM);

    tmio = tc6393xb_init(0x10000000,
            pxa2xx_gpio_in_get(cpu->gpio)[TOSA_GPIO_TC6393XB_INT]);

    scp0 = scoop_init(cpu, 0, 0x08800000);
    scp1 = scoop_init(cpu, 1, 0x14800040);

    tosa_gpio_setup(cpu, scp0, scp1, tmio);

    tosa_microdrive_attach(cpu);

    tosa_tg_init(cpu);

    /* Setup initial (reset) machine state */
    cpu->env->regs[15] = tosa_binfo.loader_start;

    tosa_binfo.kernel_filename = kernel_filename;
    tosa_binfo.kernel_cmdline = kernel_cmdline;
    tosa_binfo.initrd_filename = initrd_filename;
    tosa_binfo.board_id = 0x208;
    arm_load_kernel(cpu->env, &tosa_binfo);
    sl_bootparam_write(SL_PXA_PARAM_BASE - PXA2XX_SDRAM_BASE);
}

QEMUMachine tosapda_machine = {
    .name = "tosa",
    .desc = "Tosa PDA (PXA255)",
    .init = tosa_init,
    .ram_require = TOSA_RAM + TOSA_ROM + PXA2XX_INTERNAL_SIZE + RAMSIZE_FIXED + TC6393XB_RAM,
};
