/*
 * SSD0303 OLED controller with OSRAM Pictiva 96x16 display.
 *
 * Copyright (c) 2006-2007 CodeSourcery.
 * Written by Paul Brook
 *
 * This code is licenced under the GPL.
 */

/* The controller can support a variety of different displays, but we only
   implement one.  Most of the commends relating to brightness and geometry
   setup are ignored. */
#include "i2c.h"
#include "console.h"

//#define DEBUG_SSD0303 1

#ifdef DEBUG_SSD0303
#define DPRINTF(fmt, ...) \
do { printf("ssd0303: " fmt , ## __VA_ARGS__); } while (0)
#define BADF(fmt, ...) \
do { fprintf(stderr, "ssd0303: error: " fmt , ## __VA_ARGS__); exit(1);} while (0)
#else
#define DPRINTF(fmt, ...) do {} while(0)
#define BADF(fmt, ...) \
do { fprintf(stderr, "ssd0303: error: " fmt , ## __VA_ARGS__);} while (0)
#endif

/* Scaling factor for pixels.  */
#define MAGNIFY 4

enum ssd0303_mode
{
    SSD0303_IDLE,
    SSD0303_DATA,
    SSD0303_CMD
};

enum ssd0303_cmd {
    SSD0303_CMD_NONE,
    SSD0303_CMD_SKIP1
};

typedef struct {
    i2c_slave i2c;
    DisplayState *ds;
    int row;
    int col;
    int start_line;
    int mirror;
    int flash;
    int enabled;
    int inverse;
    int redraw;
    enum ssd0303_mode mode;
    enum ssd0303_cmd cmd_state;
    uint8_t framebuffer[132*8];
} ssd0303_state;

static int ssd0303_recv(i2c_slave *i2c)
{
    BADF("Reads not implemented\n");
    return -1;
}

static int ssd0303_send(i2c_slave *i2c, uint8_t data)
{
    ssd0303_state *s = (ssd0303_state *)i2c;
    enum ssd0303_cmd old_cmd_state;
    switch (s->mode) {
    case SSD0303_IDLE:
        DPRINTF("byte 0x%02x\n", data);
        if (data == 0x80)
            s->mode = SSD0303_CMD;
        else if (data == 0x40)
            s->mode = SSD0303_DATA;
        else
            BADF("Unexpected byte 0x%x\n", data);
        break;
    case SSD0303_DATA:
        DPRINTF("data 0x%02x\n", data);
        if (s->col < 132) {
            s->framebuffer[s->col + s->row * 132] = data;
            s->col++;
            s->redraw = 1;
        }
        break;
    case SSD0303_CMD:
        old_cmd_state = s->cmd_state;
        s->cmd_state = SSD0303_CMD_NONE;
        switch (old_cmd_state) {
        case SSD0303_CMD_NONE:
            DPRINTF("cmd 0x%02x\n", data);
            s->mode = SSD0303_IDLE;
            switch (data) {
            case 0x00 ... 0x0f: /* Set lower colum address.  */
                s->col = (s->col & 0xf0) | (data & 0xf);
                break;
            case 0x10 ... 0x20: /* Set higher column address.  */
                s->col = (s->col & 0x0f) | ((data & 0xf) << 4);
                break;
            case 0x40 ... 0x7f: /* Set start line.  */
                s->start_line = 0;
                break;
            case 0x81: /* Set contrast (Ignored).  */
                s->cmd_state = SSD0303_CMD_SKIP1;
                break;
            case 0xa0: /* Mirror off.  */
                s->mirror = 0;
                break;
            case 0xa1: /* Mirror off.  */
                s->mirror = 1;
                break;
            case 0xa4: /* Entire display off.  */
                s->flash = 0;
                break;
            case 0xa5: /* Entire display on.  */
                s->flash = 1;
                break;
            case 0xa6: /* Inverse off.  */
                s->inverse = 0;
                break;
            case 0xa7: /* Inverse on.  */
                s->inverse = 1;
                break;
            case 0xa8: /* Set multipled ratio (Ignored).  */
                s->cmd_state = SSD0303_CMD_SKIP1;
                break;
            case 0xad: /* DC-DC power control.  */
                s->cmd_state = SSD0303_CMD_SKIP1;
                break;
            case 0xae: /* Display off.  */
                s->enabled = 0;
                break;
            case 0xaf: /* Display on.  */
                s->enabled = 1;
                break;
            case 0xb0 ... 0xbf: /* Set Page address.  */
                s->row = data & 7;
                break;
            case 0xc0 ... 0xc8: /* Set COM output direction (Ignored).  */
                break;
            case 0xd3: /* Set display offset (Ignored).  */
                s->cmd_state = SSD0303_CMD_SKIP1;
                break;
            case 0xd5: /* Set display clock (Ignored).  */
                s->cmd_state = SSD0303_CMD_SKIP1;
                break;
            case 0xd8: /* Set color and power mode (Ignored).  */
                s->cmd_state = SSD0303_CMD_SKIP1;
                break;
            case 0xd9: /* Set pre-charge period (Ignored).  */
                s->cmd_state = SSD0303_CMD_SKIP1;
                break;
            case 0xda: /* Set COM pin configuration (Ignored).  */
                s->cmd_state = SSD0303_CMD_SKIP1;
                break;
            case 0xdb: /* Set VCOM dselect level (Ignored).  */
                s->cmd_state = SSD0303_CMD_SKIP1;
                break;
            case 0xe3: /* no-op.  */
                break;
            default:
                BADF("Unknown command: 0x%x\n", data);
            }
            break;
        case SSD0303_CMD_SKIP1:
            DPRINTF("skip 0x%02x\n", data);
            break;
        }
        break;
    }
    return 0;
}

static void ssd0303_event(i2c_slave *i2c, enum i2c_event event)
{
    ssd0303_state *s = (ssd0303_state *)i2c;
    switch (event) {
    case I2C_FINISH:
        s->mode = SSD0303_IDLE;
        break;
    case I2C_START_RECV:
    case I2C_START_SEND:
    case I2C_NACK:
        /* Nothing to do.  */
        break;
    }
}

static void ssd0303_update_display(void *opaque)
{
    ssd0303_state *s = (ssd0303_state *)opaque;
    uint8_t *dest;
    uint8_t *src;
    int x;
    int y;
    int line;
    char *colors[2];
    char colortab[MAGNIFY * 8];
    int dest_width;
    uint8_t mask;

    if (!s->redraw)
        return;

    switch (ds_get_bits_per_pixel(s->ds)) {
    case 0:
        return;
    case 15:
        dest_width = 2;
        break;
    case 16:
        dest_width = 2;
        break;
    case 24:
        dest_width = 3;
        break;
    case 32:
        dest_width = 4;
        break;
    default:
        BADF("Bad color depth\n");
        return;
    }
    dest_width *= MAGNIFY;
    memset(colortab, 0xff, dest_width);
    memset(colortab + dest_width, 0, dest_width);
    if (s->flash) {
        colors[0] = colortab;
        colors[1] = colortab;
    } else if (s->inverse) {
        colors[0] = colortab;
        colors[1] = colortab + dest_width;
    } else {
        colors[0] = colortab + dest_width;
        colors[1] = colortab;
    }
    dest = ds_get_data(s->ds);
    for (y = 0; y < 16; y++) {
        line = (y + s->start_line) & 63;
        src = s->framebuffer + 132 * (line >> 3) + 36;
        mask = 1 << (line & 7);
        for (x = 0; x < 96; x++) {
            memcpy(dest, colors[(*src & mask) != 0], dest_width);
            dest += dest_width;
            src++;
        }
        for (x = 1; x < MAGNIFY; x++) {
            memcpy(dest, dest - dest_width * 96, dest_width * 96);
            dest += dest_width * 96;
        }
    }
    s->redraw = 0;
    dpy_update(s->ds, 0, 0, 96 * MAGNIFY, 16 * MAGNIFY);
}

static void ssd0303_invalidate_display(void * opaque)
{
    ssd0303_state *s = (ssd0303_state *)opaque;
    s->redraw = 1;
}

static void ssd0303_save(QEMUFile *f, void *opaque)
{
    ssd0303_state *s = (ssd0303_state *)opaque;

    qemu_put_be32(f, s->row);
    qemu_put_be32(f, s->col);
    qemu_put_be32(f, s->start_line);
    qemu_put_be32(f, s->mirror);
    qemu_put_be32(f, s->flash);
    qemu_put_be32(f, s->enabled);
    qemu_put_be32(f, s->inverse);
    qemu_put_be32(f, s->redraw);
    qemu_put_be32(f, s->mode);
    qemu_put_be32(f, s->cmd_state);
    qemu_put_buffer(f, s->framebuffer, sizeof(s->framebuffer));

    i2c_slave_save(f, &s->i2c);
}

static int ssd0303_load(QEMUFile *f, void *opaque, int version_id)
{
    ssd0303_state *s = (ssd0303_state *)opaque;

    if (version_id != 1)
        return -EINVAL;

    s->row = qemu_get_be32(f);
    s->col = qemu_get_be32(f);
    s->start_line = qemu_get_be32(f);
    s->mirror = qemu_get_be32(f);
    s->flash = qemu_get_be32(f);
    s->enabled = qemu_get_be32(f);
    s->inverse = qemu_get_be32(f);
    s->redraw = qemu_get_be32(f);
    s->mode = qemu_get_be32(f);
    s->cmd_state = qemu_get_be32(f);
    qemu_get_buffer(f, s->framebuffer, sizeof(s->framebuffer));

    i2c_slave_load(f, &s->i2c);

    return 0;
}

static void ssd0303_init(i2c_slave *i2c)
{
    ssd0303_state *s = FROM_I2C_SLAVE(ssd0303_state, i2c);

    s->ds = graphic_console_init(ssd0303_update_display,
                                 ssd0303_invalidate_display,
                                 NULL, NULL, s);
    qemu_console_resize(s->ds, 96 * MAGNIFY, 16 * MAGNIFY);
    register_savevm("ssd0303_oled", -1, 1, ssd0303_save, ssd0303_load, s);
}

static I2CSlaveInfo ssd0303_info = {
    .qdev.name = "ssd0303",
    .qdev.size = sizeof(ssd0303_state),
    .init = ssd0303_init,
    .event = ssd0303_event,
    .recv = ssd0303_recv,
    .send = ssd0303_send
};

static void ssd0303_register_devices(void)
{
    i2c_register_slave(&ssd0303_info);
}

device_init(ssd0303_register_devices)
