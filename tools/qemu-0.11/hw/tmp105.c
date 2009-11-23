/*
 * Texas Instruments TMP105 temperature sensor.
 *
 * Copyright (C) 2008 Nokia Corporation
 * Written by Andrzej Zaborowski <andrew@openedhand.com>
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
 * with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#include "hw.h"
#include "i2c.h"

typedef struct {
    i2c_slave i2c;
    int len;
    uint8_t buf[2];
    qemu_irq pin;

    uint8_t pointer;
    uint8_t config;
    int16_t temperature;
    int16_t limit[2];
    int faults;
    int alarm;
} TMP105State;

static void tmp105_interrupt_update(TMP105State *s)
{
    qemu_set_irq(s->pin, s->alarm ^ ((~s->config >> 2) & 1));	/* POL */
}

static void tmp105_alarm_update(TMP105State *s)
{
    if ((s->config >> 0) & 1) {					/* SD */
        if ((s->config >> 7) & 1)				/* OS */
            s->config &= ~(1 << 7);				/* OS */
        else
            return;
    }

    if ((s->config >> 1) & 1) {					/* TM */
        if (s->temperature >= s->limit[1])
            s->alarm = 1;
        else if (s->temperature < s->limit[0])
            s->alarm = 1;
    } else {
        if (s->temperature >= s->limit[1])
            s->alarm = 1;
        else if (s->temperature < s->limit[0])
            s->alarm = 0;
    }

    tmp105_interrupt_update(s);
}

/* Units are 0.001 centigrades relative to 0 C.  */
void tmp105_set(i2c_slave *i2c, int temp)
{
    TMP105State *s = (TMP105State *) i2c;

    if (temp >= 128000 || temp < -128000) {
        fprintf(stderr, "%s: values is out of range (%i.%03i C)\n",
                        __FUNCTION__, temp / 1000, temp % 1000);
        exit(-1);
    }

    s->temperature = ((int16_t) (temp * 0x800 / 128000)) << 4;

    tmp105_alarm_update(s);
}

static const int tmp105_faultq[4] = { 1, 2, 4, 6 };

static void tmp105_read(TMP105State *s)
{
    s->len = 0;

    if ((s->config >> 1) & 1) {					/* TM */
        s->alarm = 0;
        tmp105_interrupt_update(s);
    }

    switch (s->pointer & 3) {
    case 0:	/* Temperature */
        s->buf[s->len ++] = (((uint16_t) s->temperature) >> 8);
        s->buf[s->len ++] = (((uint16_t) s->temperature) >> 0) &
                (0xf0 << ((~s->config >> 5) & 3));		/* R */
        break;

    case 1:	/* Configuration */
        s->buf[s->len ++] = s->config;
        break;

    case 2:	/* T_LOW */
        s->buf[s->len ++] = ((uint16_t) s->limit[0]) >> 8;
        s->buf[s->len ++] = ((uint16_t) s->limit[0]) >> 0;
        break;

    case 3:	/* T_HIGH */
        s->buf[s->len ++] = ((uint16_t) s->limit[1]) >> 8;
        s->buf[s->len ++] = ((uint16_t) s->limit[1]) >> 0;
        break;
    }
}

static void tmp105_write(TMP105State *s)
{
    switch (s->pointer & 3) {
    case 0:	/* Temperature */
        break;

    case 1:	/* Configuration */
        if (s->buf[0] & ~s->config & (1 << 0))			/* SD */
            printf("%s: TMP105 shutdown\n", __FUNCTION__);
        s->config = s->buf[0];
        s->faults = tmp105_faultq[(s->config >> 3) & 3];	/* F */
        tmp105_alarm_update(s);
        break;

    case 2:	/* T_LOW */
    case 3:	/* T_HIGH */
        if (s->len >= 3)
            s->limit[s->pointer & 1] = (int16_t)
                    ((((uint16_t) s->buf[0]) << 8) | s->buf[1]);
        tmp105_alarm_update(s);
        break;
    }
}

static int tmp105_rx(i2c_slave *i2c)
{
    TMP105State *s = (TMP105State *) i2c;

    if (s->len < 2)
        return s->buf[s->len ++];
    else
        return 0xff;
}

static int tmp105_tx(i2c_slave *i2c, uint8_t data)
{
    TMP105State *s = (TMP105State *) i2c;

    if (!s->len ++)
        s->pointer = data;
    else {
        if (s->len <= 2)
            s->buf[s->len - 1] = data;
        tmp105_write(s);
    }

    return 0;
}

static void tmp105_event(i2c_slave *i2c, enum i2c_event event)
{
    TMP105State *s = (TMP105State *) i2c;

    if (event == I2C_START_RECV)
        tmp105_read(s);

    s->len = 0;
}

static void tmp105_save(QEMUFile *f, void *opaque)
{
    TMP105State *s = (TMP105State *) opaque;

    qemu_put_byte(f, s->len);
    qemu_put_8s(f, &s->buf[0]);
    qemu_put_8s(f, &s->buf[1]);

    qemu_put_8s(f, &s->pointer);
    qemu_put_8s(f, &s->config);
    qemu_put_sbe16s(f, &s->temperature);
    qemu_put_sbe16s(f, &s->limit[0]);
    qemu_put_sbe16s(f, &s->limit[1]);
    qemu_put_byte(f, s->alarm);
    s->faults = tmp105_faultq[(s->config >> 3) & 3];		/* F */

    i2c_slave_save(f, &s->i2c);
}

static int tmp105_load(QEMUFile *f, void *opaque, int version_id)
{
    TMP105State *s = (TMP105State *) opaque;

    s->len = qemu_get_byte(f);
    qemu_get_8s(f, &s->buf[0]);
    qemu_get_8s(f, &s->buf[1]);

    qemu_get_8s(f, &s->pointer);
    qemu_get_8s(f, &s->config);
    qemu_get_sbe16s(f, &s->temperature);
    qemu_get_sbe16s(f, &s->limit[0]);
    qemu_get_sbe16s(f, &s->limit[1]);
    s->alarm = qemu_get_byte(f);

    tmp105_interrupt_update(s);

    i2c_slave_load(f, &s->i2c);
    return 0;
}

static void tmp105_reset(i2c_slave *i2c)
{
    TMP105State *s = (TMP105State *) i2c;

    s->temperature = 0;
    s->pointer = 0;
    s->config = 0;
    s->faults = tmp105_faultq[(s->config >> 3) & 3];
    s->alarm = 0;

    tmp105_interrupt_update(s);
}

static void tmp105_init(i2c_slave *i2c)
{
    TMP105State *s = FROM_I2C_SLAVE(TMP105State, i2c);

    qdev_init_gpio_out(&i2c->qdev, &s->pin, 1);

    tmp105_reset(&s->i2c);

    register_savevm("TMP105", -1, 0, tmp105_save, tmp105_load, s);
}

static I2CSlaveInfo tmp105_info = {
    .qdev.name = "tmp105",
    .qdev.size = sizeof(TMP105State),
    .init = tmp105_init,
    .event = tmp105_event,
    .recv = tmp105_rx,
    .send = tmp105_tx
};

static void tmp105_register_devices(void)
{
    i2c_register_slave(&tmp105_info);
}

device_init(tmp105_register_devices)
