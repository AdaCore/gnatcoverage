/*
 * QEMU PCI bus manager
 *
 * Copyright (c) 2004 Fabrice Bellard
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
#include "pci.h"
#include "monitor.h"
#include "net.h"
#include "sysemu.h"

//#define DEBUG_PCI
#ifdef DEBUG_PCI
# define PCI_DPRINTF(format, ...)       printf(format, __VA_ARGS__)
#else
# define PCI_DPRINTF(format, ...)       do { } while (0)
#endif

struct PCIBus {
    BusState qbus;
    int bus_num;
    int devfn_min;
    pci_set_irq_fn set_irq;
    pci_map_irq_fn map_irq;
    uint32_t config_reg; /* XXX: suppress */
    /* low level pic */
    SetIRQFunc *low_set_irq;
    qemu_irq *irq_opaque;
    PCIDevice *devices[256];
    PCIDevice *parent_dev;
    PCIBus *next;
    /* The bus IRQ state is the logical OR of the connected devices.
       Keep a count of the number of devices with raised IRQs.  */
    int nirq;
    int *irq_count;
};

static void pcibus_dev_print(Monitor *mon, DeviceState *dev, int indent);

static struct BusInfo pci_bus_info = {
    .name       = "PCI",
    .size       = sizeof(PCIBus),
    .print_dev  = pcibus_dev_print,
    .props      = (Property[]) {
        {
            .name   = "devfn",
            .info   = &qdev_prop_uint32,
            .offset = offsetof(PCIDevice, devfn),
            .defval = (uint32_t[]) { -1 },
        },
        {/* end of list */}
    }
};

static void pci_update_mappings(PCIDevice *d);
static void pci_set_irq(void *opaque, int irq_num, int level);

target_phys_addr_t pci_mem_base;
static uint16_t pci_default_sub_vendor_id = PCI_SUBVENDOR_ID_REDHAT_QUMRANET;
static uint16_t pci_default_sub_device_id = PCI_SUBDEVICE_ID_QEMU;
static PCIBus *first_bus;

static void pcibus_save(QEMUFile *f, void *opaque)
{
    PCIBus *bus = (PCIBus *)opaque;
    int i;

    qemu_put_be32(f, bus->nirq);
    for (i = 0; i < bus->nirq; i++)
        qemu_put_be32(f, bus->irq_count[i]);
}

static int  pcibus_load(QEMUFile *f, void *opaque, int version_id)
{
    PCIBus *bus = (PCIBus *)opaque;
    int i, nirq;

    if (version_id != 1)
        return -EINVAL;

    nirq = qemu_get_be32(f);
    if (bus->nirq != nirq) {
        fprintf(stderr, "pcibus_load: nirq mismatch: src=%d dst=%d\n",
                nirq, bus->nirq);
        return -EINVAL;
    }

    for (i = 0; i < nirq; i++)
        bus->irq_count[i] = qemu_get_be32(f);

    return 0;
}

static void pci_bus_reset(void *opaque)
{
    PCIBus *bus = (PCIBus *)opaque;
    int i;

    for (i = 0; i < bus->nirq; i++) {
        bus->irq_count[i] = 0;
    }
    for (i = 0; i < 256; i++) {
        if (bus->devices[i])
            memset(bus->devices[i]->irq_state, 0,
                   sizeof(bus->devices[i]->irq_state));
    }
}

PCIBus *pci_register_bus(DeviceState *parent, const char *name,
                         pci_set_irq_fn set_irq, pci_map_irq_fn map_irq,
                         qemu_irq *pic, int devfn_min, int nirq)
{
    PCIBus *bus;
    static int nbus = 0;

    bus = FROM_QBUS(PCIBus, qbus_create(&pci_bus_info, parent, name));
    bus->set_irq = set_irq;
    bus->map_irq = map_irq;
    bus->irq_opaque = pic;
    bus->devfn_min = devfn_min;
    bus->nirq = nirq;
    bus->irq_count = qemu_mallocz(nirq * sizeof(bus->irq_count[0]));
    bus->next = first_bus;
    first_bus = bus;
    register_savevm("PCIBUS", nbus++, 1, pcibus_save, pcibus_load, bus);
    qemu_register_reset(pci_bus_reset, bus);
    return bus;
}

static PCIBus *pci_register_secondary_bus(PCIDevice *dev, pci_map_irq_fn map_irq)
{
    PCIBus *bus;

    bus = qemu_mallocz(sizeof(PCIBus));
    bus->map_irq = map_irq;
    bus->parent_dev = dev;
    bus->next = dev->bus->next;
    dev->bus->next = bus;
    return bus;
}

int pci_bus_num(PCIBus *s)
{
    return s->bus_num;
}

void pci_device_save(PCIDevice *s, QEMUFile *f)
{
    int i;

    qemu_put_be32(f, 2); /* PCI device version */
    qemu_put_buffer(f, s->config, 256);
    for (i = 0; i < 4; i++)
        qemu_put_be32(f, s->irq_state[i]);
}

int pci_device_load(PCIDevice *s, QEMUFile *f)
{
    uint8_t config[PCI_CONFIG_SPACE_SIZE];
    uint32_t version_id;
    int i;

    version_id = qemu_get_be32(f);
    if (version_id > 2)
        return -EINVAL;
    qemu_get_buffer(f, config, sizeof config);
    for (i = 0; i < sizeof config; ++i)
        if ((config[i] ^ s->config[i]) & s->cmask[i] & ~s->wmask[i])
            return -EINVAL;
    memcpy(s->config, config, sizeof config);

    pci_update_mappings(s);

    if (version_id >= 2)
        for (i = 0; i < 4; i ++)
            s->irq_state[i] = qemu_get_be32(f);
    return 0;
}

static int pci_set_default_subsystem_id(PCIDevice *pci_dev)
{
    uint16_t *id;

    id = (void*)(&pci_dev->config[PCI_SUBVENDOR_ID]);
    id[0] = cpu_to_le16(pci_default_sub_vendor_id);
    id[1] = cpu_to_le16(pci_default_sub_device_id);
    return 0;
}

/*
 * Parse [[<domain>:]<bus>:]<slot>, return -1 on error
 */
static int pci_parse_devaddr(const char *addr, int *domp, int *busp, unsigned *slotp)
{
    const char *p;
    char *e;
    unsigned long val;
    unsigned long dom = 0, bus = 0;
    unsigned slot = 0;

    p = addr;
    val = strtoul(p, &e, 16);
    if (e == p)
	return -1;
    if (*e == ':') {
	bus = val;
	p = e + 1;
	val = strtoul(p, &e, 16);
	if (e == p)
	    return -1;
	if (*e == ':') {
	    dom = bus;
	    bus = val;
	    p = e + 1;
	    val = strtoul(p, &e, 16);
	    if (e == p)
		return -1;
	}
    }

    if (dom > 0xffff || bus > 0xff || val > 0x1f)
	return -1;

    slot = val;

    if (*e)
	return -1;

    /* Note: QEMU doesn't implement domains other than 0 */
    if (dom != 0 || pci_find_bus(bus) == NULL)
	return -1;

    *domp = dom;
    *busp = bus;
    *slotp = slot;
    return 0;
}

int pci_read_devaddr(Monitor *mon, const char *addr, int *domp, int *busp,
                     unsigned *slotp)
{
    /* strip legacy tag */
    if (!strncmp(addr, "pci_addr=", 9)) {
        addr += 9;
    }
    if (pci_parse_devaddr(addr, domp, busp, slotp)) {
        monitor_printf(mon, "Invalid pci address\n");
        return -1;
    }
    return 0;
}

static PCIBus *pci_get_bus_devfn(int *devfnp, const char *devaddr)
{
    int dom, bus;
    unsigned slot;

    if (!devaddr) {
        *devfnp = -1;
        return pci_find_bus(0);
    }

    if (pci_parse_devaddr(devaddr, &dom, &bus, &slot) < 0) {
        return NULL;
    }

    *devfnp = slot << 3;
    return pci_find_bus(bus);
}

static void pci_init_cmask(PCIDevice *dev)
{
    pci_set_word(dev->cmask + PCI_VENDOR_ID, 0xffff);
    pci_set_word(dev->cmask + PCI_DEVICE_ID, 0xffff);
    dev->cmask[PCI_STATUS] = PCI_STATUS_CAP_LIST;
    dev->cmask[PCI_REVISION_ID] = 0xff;
    dev->cmask[PCI_CLASS_PROG] = 0xff;
    pci_set_word(dev->cmask + PCI_CLASS_DEVICE, 0xffff);
    dev->cmask[PCI_HEADER_TYPE] = 0xff;
    dev->cmask[PCI_CAPABILITY_LIST] = 0xff;
}

static void pci_init_wmask(PCIDevice *dev)
{
    int i;
    dev->wmask[PCI_CACHE_LINE_SIZE] = 0xff;
    dev->wmask[PCI_INTERRUPT_LINE] = 0xff;
    dev->wmask[PCI_COMMAND] = PCI_COMMAND_IO | PCI_COMMAND_MEMORY
                              | PCI_COMMAND_MASTER;
    for (i = PCI_CONFIG_HEADER_SIZE; i < PCI_CONFIG_SPACE_SIZE; ++i)
        dev->wmask[i] = 0xff;
}

/* -1 for devfn means auto assign */
static PCIDevice *do_pci_register_device(PCIDevice *pci_dev, PCIBus *bus,
                                         const char *name, int devfn,
                                         PCIConfigReadFunc *config_read,
                                         PCIConfigWriteFunc *config_write)
{
    if (devfn < 0) {
        for(devfn = bus->devfn_min ; devfn < 256; devfn += 8) {
            if (!bus->devices[devfn])
                goto found;
        }
        return NULL;
    found: ;
    } else if (bus->devices[devfn]) {
        return NULL;
    }
    pci_dev->bus = bus;
    pci_dev->devfn = devfn;
    pstrcpy(pci_dev->name, sizeof(pci_dev->name), name);
    memset(pci_dev->irq_state, 0, sizeof(pci_dev->irq_state));
    pci_set_default_subsystem_id(pci_dev);
    pci_init_cmask(pci_dev);
    pci_init_wmask(pci_dev);

    if (!config_read)
        config_read = pci_default_read_config;
    if (!config_write)
        config_write = pci_default_write_config;
    pci_dev->config_read = config_read;
    pci_dev->config_write = config_write;
    bus->devices[devfn] = pci_dev;
    pci_dev->irq = qemu_allocate_irqs(pci_set_irq, pci_dev, 4);
    return pci_dev;
}

PCIDevice *pci_register_device(PCIBus *bus, const char *name,
                               int instance_size, int devfn,
                               PCIConfigReadFunc *config_read,
                               PCIConfigWriteFunc *config_write)
{
    PCIDevice *pci_dev;

    pci_dev = qemu_mallocz(instance_size);
    pci_dev = do_pci_register_device(pci_dev, bus, name, devfn,
                                     config_read, config_write);
    return pci_dev;
}
static target_phys_addr_t pci_to_cpu_addr(target_phys_addr_t addr)
{
    return addr + pci_mem_base;
}

static void pci_unregister_io_regions(PCIDevice *pci_dev)
{
    PCIIORegion *r;
    int i;

    for(i = 0; i < PCI_NUM_REGIONS; i++) {
        r = &pci_dev->io_regions[i];
        if (!r->size || r->addr == -1)
            continue;
        if (r->type == PCI_ADDRESS_SPACE_IO) {
            isa_unassign_ioport(r->addr, r->size);
        } else {
            cpu_register_physical_memory(pci_to_cpu_addr(r->addr),
                                                     r->size,
                                                     IO_MEM_UNASSIGNED);
        }
    }
}

int pci_unregister_device(PCIDevice *pci_dev)
{
    int ret = 0;

    if (pci_dev->unregister)
        ret = pci_dev->unregister(pci_dev);
    if (ret)
        return ret;

    pci_unregister_io_regions(pci_dev);

    qemu_free_irqs(pci_dev->irq);
    pci_dev->bus->devices[pci_dev->devfn] = NULL;
    qdev_free(&pci_dev->qdev);
    return 0;
}

void pci_register_bar(PCIDevice *pci_dev, int region_num,
                            uint32_t size, int type,
                            PCIMapIORegionFunc *map_func)
{
    PCIIORegion *r;
    uint32_t addr;
    uint32_t wmask;

    if ((unsigned int)region_num >= PCI_NUM_REGIONS)
        return;

    if (size & (size-1)) {
        fprintf(stderr, "ERROR: PCI region size must be pow2 "
                    "type=0x%x, size=0x%x\n", type, size);
        exit(1);
    }

    r = &pci_dev->io_regions[region_num];
    r->addr = -1;
    r->size = size;
    r->type = type;
    r->map_func = map_func;

    wmask = ~(size - 1);
    if (region_num == PCI_ROM_SLOT) {
        addr = 0x30;
        /* ROM enable bit is writeable */
        wmask |= 1;
    } else {
        addr = 0x10 + region_num * 4;
    }
    *(uint32_t *)(pci_dev->config + addr) = cpu_to_le32(type);
    *(uint32_t *)(pci_dev->wmask + addr) = cpu_to_le32(wmask);
    *(uint32_t *)(pci_dev->cmask + addr) = 0xffffffff;
}

static void pci_update_mappings(PCIDevice *d)
{
    PCIIORegion *r;
    int cmd, i;
    uint32_t last_addr, new_addr, config_ofs;

    cmd = le16_to_cpu(*(uint16_t *)(d->config + PCI_COMMAND));
    for(i = 0; i < PCI_NUM_REGIONS; i++) {
        r = &d->io_regions[i];
        if (i == PCI_ROM_SLOT) {
            config_ofs = 0x30;
        } else {
            config_ofs = 0x10 + i * 4;
        }
        if (r->size != 0) {
            if (r->type & PCI_ADDRESS_SPACE_IO) {
                if (cmd & PCI_COMMAND_IO) {
                    new_addr = le32_to_cpu(*(uint32_t *)(d->config +
                                                         config_ofs));
                    new_addr = new_addr & ~(r->size - 1);
                    last_addr = new_addr + r->size - 1;
                    /* NOTE: we have only 64K ioports on PC */
                    if (last_addr <= new_addr || new_addr == 0 ||
                        last_addr >= 0x10000) {
                        new_addr = -1;
                    }
                } else {
                    new_addr = -1;
                }
            } else {
                if (cmd & PCI_COMMAND_MEMORY) {
                    new_addr = le32_to_cpu(*(uint32_t *)(d->config +
                                                         config_ofs));
                    /* the ROM slot has a specific enable bit */
                    if (i == PCI_ROM_SLOT && !(new_addr & 1))
                        goto no_mem_map;
                    new_addr = new_addr & ~(r->size - 1);
                    last_addr = new_addr + r->size - 1;
                    /* NOTE: we do not support wrapping */
                    /* XXX: as we cannot support really dynamic
                       mappings, we handle specific values as invalid
                       mappings. */
                    if (last_addr <= new_addr || new_addr == 0 ||
                        last_addr == -1) {
                        new_addr = -1;
                    }
                } else {
                no_mem_map:
                    new_addr = -1;
                }
            }
            /* now do the real mapping */
            if (new_addr != r->addr) {
                if (r->addr != -1) {
                    if (r->type & PCI_ADDRESS_SPACE_IO) {
                        int class;
                        /* NOTE: specific hack for IDE in PC case:
                           only one byte must be mapped. */
                        class = d->config[0x0a] | (d->config[0x0b] << 8);
                        if (class == 0x0101 && r->size == 4) {
                            isa_unassign_ioport(r->addr + 2, 1);
                        } else {
                            isa_unassign_ioport(r->addr, r->size);
                        }
                    } else {
                        cpu_register_physical_memory(pci_to_cpu_addr(r->addr),
                                                     r->size,
                                                     IO_MEM_UNASSIGNED);
                        qemu_unregister_coalesced_mmio(r->addr, r->size);
                    }
                }
                r->addr = new_addr;
                if (r->addr != -1) {
                    r->map_func(d, i, r->addr, r->size, r->type);
                }
            }
        }
    }
}

uint32_t pci_default_read_config(PCIDevice *d,
                                 uint32_t address, int len)
{
    uint32_t val;

    switch(len) {
    default:
    case 4:
	if (address <= 0xfc) {
	    val = le32_to_cpu(*(uint32_t *)(d->config + address));
	    break;
	}
	/* fall through */
    case 2:
        if (address <= 0xfe) {
	    val = le16_to_cpu(*(uint16_t *)(d->config + address));
	    break;
	}
	/* fall through */
    case 1:
        val = d->config[address];
        break;
    }
    return val;
}

void pci_default_write_config(PCIDevice *d, uint32_t addr, uint32_t val, int l)
{
    uint8_t orig[PCI_CONFIG_SPACE_SIZE];
    int i;

    /* not efficient, but simple */
    memcpy(orig, d->config, PCI_CONFIG_SPACE_SIZE);
    for(i = 0; i < l && addr < PCI_CONFIG_SPACE_SIZE; val >>= 8, ++i, ++addr) {
        uint8_t wmask = d->wmask[addr];
        d->config[addr] = (d->config[addr] & ~wmask) | (val & wmask);
    }
    if (memcmp(orig + PCI_BASE_ADDRESS_0, d->config + PCI_BASE_ADDRESS_0, 24)
        || ((orig[PCI_COMMAND] ^ d->config[PCI_COMMAND])
            & (PCI_COMMAND_MEMORY | PCI_COMMAND_IO)))
        pci_update_mappings(d);
}

void pci_data_write(void *opaque, uint32_t addr, uint32_t val, int len)
{
    PCIBus *s = opaque;
    PCIDevice *pci_dev;
    int config_addr, bus_num;

#if 0
    PCI_DPRINTF("pci_data_write: addr=%08x val=%08x len=%d\n",
                addr, val, len);
#endif
    bus_num = (addr >> 16) & 0xff;
    while (s && s->bus_num != bus_num)
        s = s->next;
    if (!s)
        return;
    pci_dev = s->devices[(addr >> 8) & 0xff];
    if (!pci_dev)
        return;
    config_addr = addr & 0xff;
    PCI_DPRINTF("pci_config_write: %s: addr=%02x val=%08x len=%d\n",
                pci_dev->name, config_addr, val, len);
    pci_dev->config_write(pci_dev, config_addr, val, len);
}

uint32_t pci_data_read(void *opaque, uint32_t addr, int len)
{
    PCIBus *s = opaque;
    PCIDevice *pci_dev;
    int config_addr, bus_num;
    uint32_t val;

    bus_num = (addr >> 16) & 0xff;
    while (s && s->bus_num != bus_num)
        s= s->next;
    if (!s)
        goto fail;
    pci_dev = s->devices[(addr >> 8) & 0xff];
    if (!pci_dev) {
    fail:
        switch(len) {
        case 1:
            val = 0xff;
            break;
        case 2:
            val = 0xffff;
            break;
        default:
        case 4:
            val = 0xffffffff;
            break;
        }
        goto the_end;
    }
    config_addr = addr & 0xff;
    val = pci_dev->config_read(pci_dev, config_addr, len);
    PCI_DPRINTF("pci_config_read: %s: addr=%02x val=%08x len=%d\n",
                pci_dev->name, config_addr, val, len);
 the_end:
#if 0
    PCI_DPRINTF("pci_data_read: addr=%08x val=%08x len=%d\n",
                addr, val, len);
#endif
    return val;
}

/***********************************************************/
/* generic PCI irq support */

/* 0 <= irq_num <= 3. level must be 0 or 1 */
static void pci_set_irq(void *opaque, int irq_num, int level)
{
    PCIDevice *pci_dev = (PCIDevice *)opaque;
    PCIBus *bus;
    int change;

    change = level - pci_dev->irq_state[irq_num];
    if (!change)
        return;

    pci_dev->irq_state[irq_num] = level;
    for (;;) {
        bus = pci_dev->bus;
        irq_num = bus->map_irq(pci_dev, irq_num);
        if (bus->set_irq)
            break;
        pci_dev = bus->parent_dev;
    }
    bus->irq_count[irq_num] += change;
    bus->set_irq(bus->irq_opaque, irq_num, bus->irq_count[irq_num] != 0);
}

/***********************************************************/
/* monitor info on PCI */

typedef struct {
    uint16_t class;
    const char *desc;
} pci_class_desc;

static const pci_class_desc pci_class_descriptions[] =
{
    { 0x0100, "SCSI controller"},
    { 0x0101, "IDE controller"},
    { 0x0102, "Floppy controller"},
    { 0x0103, "IPI controller"},
    { 0x0104, "RAID controller"},
    { 0x0106, "SATA controller"},
    { 0x0107, "SAS controller"},
    { 0x0180, "Storage controller"},
    { 0x0200, "Ethernet controller"},
    { 0x0201, "Token Ring controller"},
    { 0x0202, "FDDI controller"},
    { 0x0203, "ATM controller"},
    { 0x0280, "Network controller"},
    { 0x0300, "VGA controller"},
    { 0x0301, "XGA controller"},
    { 0x0302, "3D controller"},
    { 0x0380, "Display controller"},
    { 0x0400, "Video controller"},
    { 0x0401, "Audio controller"},
    { 0x0402, "Phone"},
    { 0x0480, "Multimedia controller"},
    { 0x0500, "RAM controller"},
    { 0x0501, "Flash controller"},
    { 0x0580, "Memory controller"},
    { 0x0600, "Host bridge"},
    { 0x0601, "ISA bridge"},
    { 0x0602, "EISA bridge"},
    { 0x0603, "MC bridge"},
    { 0x0604, "PCI bridge"},
    { 0x0605, "PCMCIA bridge"},
    { 0x0606, "NUBUS bridge"},
    { 0x0607, "CARDBUS bridge"},
    { 0x0608, "RACEWAY bridge"},
    { 0x0680, "Bridge"},
    { 0x0c03, "USB controller"},
    { 0, NULL}
};

static void pci_info_device(PCIDevice *d)
{
    Monitor *mon = cur_mon;
    int i, class;
    PCIIORegion *r;
    const pci_class_desc *desc;

    monitor_printf(mon, "  Bus %2d, device %3d, function %d:\n",
                   d->bus->bus_num, d->devfn >> 3, d->devfn & 7);
    class = le16_to_cpu(*((uint16_t *)(d->config + PCI_CLASS_DEVICE)));
    monitor_printf(mon, "    ");
    desc = pci_class_descriptions;
    while (desc->desc && class != desc->class)
        desc++;
    if (desc->desc) {
        monitor_printf(mon, "%s", desc->desc);
    } else {
        monitor_printf(mon, "Class %04x", class);
    }
    monitor_printf(mon, ": PCI device %04x:%04x\n",
           le16_to_cpu(*((uint16_t *)(d->config + PCI_VENDOR_ID))),
           le16_to_cpu(*((uint16_t *)(d->config + PCI_DEVICE_ID))));

    if (d->config[PCI_INTERRUPT_PIN] != 0) {
        monitor_printf(mon, "      IRQ %d.\n",
                       d->config[PCI_INTERRUPT_LINE]);
    }
    if (class == 0x0604) {
        monitor_printf(mon, "      BUS %d.\n", d->config[0x19]);
    }
    for(i = 0;i < PCI_NUM_REGIONS; i++) {
        r = &d->io_regions[i];
        if (r->size != 0) {
            monitor_printf(mon, "      BAR%d: ", i);
            if (r->type & PCI_ADDRESS_SPACE_IO) {
                monitor_printf(mon, "I/O at 0x%04x [0x%04x].\n",
                               r->addr, r->addr + r->size - 1);
            } else {
                monitor_printf(mon, "32 bit memory at 0x%08x [0x%08x].\n",
                               r->addr, r->addr + r->size - 1);
            }
        }
    }
    monitor_printf(mon, "      id \"%s\"\n", d->qdev.id ? d->qdev.id : "");
    if (class == 0x0604 && d->config[0x19] != 0) {
        pci_for_each_device(d->config[0x19], pci_info_device);
    }
}

void pci_for_each_device(int bus_num, void (*fn)(PCIDevice *d))
{
    PCIBus *bus = first_bus;
    PCIDevice *d;
    int devfn;

    while (bus && bus->bus_num != bus_num)
        bus = bus->next;
    if (bus) {
        for(devfn = 0; devfn < 256; devfn++) {
            d = bus->devices[devfn];
            if (d)
                fn(d);
        }
    }
}

void pci_info(Monitor *mon)
{
    pci_for_each_device(0, pci_info_device);
}

PCIDevice *pci_create(const char *name, const char *devaddr)
{
    PCIBus *bus;
    int devfn;
    DeviceState *dev;

    bus = pci_get_bus_devfn(&devfn, devaddr);
    if (!bus) {
        fprintf(stderr, "Invalid PCI device address %s for device %s\n",
                devaddr, name);
        exit(1);
    }

    dev = qdev_create(&bus->qbus, name);
    qdev_prop_set_uint32(dev, "devfn", devfn);
    return (PCIDevice *)dev;
}

static const char * const pci_nic_models[] = {
    "ne2k_pci",
    "i82551",
    "i82557b",
    "i82559er",
    "rtl8139",
    "e1000",
    "pcnet",
    "virtio",
    NULL
};

static const char * const pci_nic_names[] = {
    "ne2k_pci",
    "i82551",
    "i82557b",
    "i82559er",
    "rtl8139",
    "e1000",
    "pcnet",
    "virtio-net-pci",
    NULL
};

/* Initialize a PCI NIC.  */
PCIDevice *pci_nic_init(NICInfo *nd, const char *default_model,
                        const char *default_devaddr)
{
    const char *devaddr = nd->devaddr ? nd->devaddr : default_devaddr;
    PCIDevice *pci_dev;
    DeviceState *dev;
    int i;

    qemu_check_nic_model_list(nd, pci_nic_models, default_model);

    for (i = 0; pci_nic_models[i]; i++) {
        if (strcmp(nd->model, pci_nic_models[i]) == 0) {
            pci_dev = pci_create(pci_nic_names[i], devaddr);
            dev = &pci_dev->qdev;
            if (nd->id)
                dev->id = qemu_strdup(nd->id);
            dev->nd = nd;
            qdev_init(dev);
            nd->private = dev;
            return pci_dev;
        }
    }

    return NULL;
}

typedef struct {
    PCIDevice dev;
    PCIBus *bus;
} PCIBridge;

static void pci_bridge_write_config(PCIDevice *d,
                             uint32_t address, uint32_t val, int len)
{
    PCIBridge *s = (PCIBridge *)d;

    pci_default_write_config(d, address, val, len);
    s->bus->bus_num = d->config[PCI_SECONDARY_BUS];
}

PCIBus *pci_find_bus(int bus_num)
{
    PCIBus *bus = first_bus;

    while (bus && bus->bus_num != bus_num)
        bus = bus->next;

    return bus;
}

PCIDevice *pci_find_device(int bus_num, int slot, int function)
{
    PCIBus *bus = pci_find_bus(bus_num);

    if (!bus)
        return NULL;

    return bus->devices[PCI_DEVFN(slot, function)];
}

PCIBus *pci_bridge_init(PCIBus *bus, int devfn, uint16_t vid, uint16_t did,
                        pci_map_irq_fn map_irq, const char *name)
{
    PCIBridge *s;
    s = (PCIBridge *)pci_register_device(bus, name, sizeof(PCIBridge),
                                         devfn, NULL, pci_bridge_write_config);

    pci_config_set_vendor_id(s->dev.config, vid);
    pci_config_set_device_id(s->dev.config, did);

    s->dev.config[0x04] = 0x06; // command = bus master, pci mem
    s->dev.config[0x05] = 0x00;
    s->dev.config[0x06] = 0xa0; // status = fast back-to-back, 66MHz, no error
    s->dev.config[0x07] = 0x00; // status = fast devsel
    s->dev.config[0x08] = 0x00; // revision
    s->dev.config[0x09] = 0x00; // programming i/f
    pci_config_set_class(s->dev.config, PCI_CLASS_BRIDGE_PCI);
    s->dev.config[0x0D] = 0x10; // latency_timer
    s->dev.config[PCI_HEADER_TYPE] =
        PCI_HEADER_TYPE_MULTI_FUNCTION | PCI_HEADER_TYPE_BRIDGE; // header_type
    s->dev.config[0x1E] = 0xa0; // secondary status

    s->bus = pci_register_secondary_bus(&s->dev, map_irq);
    return s->bus;
}

static void pci_qdev_init(DeviceState *qdev, DeviceInfo *base)
{
    PCIDevice *pci_dev = (PCIDevice *)qdev;
    PCIDeviceInfo *info = container_of(base, PCIDeviceInfo, qdev);
    PCIBus *bus;
    int devfn;

    bus = FROM_QBUS(PCIBus, qdev_get_parent_bus(qdev));
    devfn = pci_dev->devfn;
    pci_dev = do_pci_register_device(pci_dev, bus, base->name, devfn,
                                     info->config_read, info->config_write);
    assert(pci_dev);
    info->init(pci_dev);
}

void pci_qdev_register(PCIDeviceInfo *info)
{
    info->qdev.init = pci_qdev_init;
    info->qdev.bus_info = &pci_bus_info;
    qdev_register(&info->qdev);
}

void pci_qdev_register_many(PCIDeviceInfo *info)
{
    while (info->qdev.name) {
        pci_qdev_register(info);
        info++;
    }
}

PCIDevice *pci_create_simple(PCIBus *bus, int devfn, const char *name)
{
    DeviceState *dev;

    dev = qdev_create(&bus->qbus, name);
    qdev_prop_set_uint32(dev, "devfn", devfn);
    qdev_init(dev);

    return (PCIDevice *)dev;
}

static int pci_find_space(PCIDevice *pdev, uint8_t size)
{
    int offset = PCI_CONFIG_HEADER_SIZE;
    int i;
    for (i = PCI_CONFIG_HEADER_SIZE; i < PCI_CONFIG_SPACE_SIZE; ++i)
        if (pdev->used[i])
            offset = i + 1;
        else if (i - offset + 1 == size)
            return offset;
    return 0;
}

static uint8_t pci_find_capability_list(PCIDevice *pdev, uint8_t cap_id,
                                        uint8_t *prev_p)
{
    uint8_t next, prev;

    if (!(pdev->config[PCI_STATUS] & PCI_STATUS_CAP_LIST))
        return 0;

    for (prev = PCI_CAPABILITY_LIST; (next = pdev->config[prev]);
         prev = next + PCI_CAP_LIST_NEXT)
        if (pdev->config[next + PCI_CAP_LIST_ID] == cap_id)
            break;

    if (prev_p)
        *prev_p = prev;
    return next;
}

/* Reserve space and add capability to the linked list in pci config space */
int pci_add_capability(PCIDevice *pdev, uint8_t cap_id, uint8_t size)
{
    uint8_t offset = pci_find_space(pdev, size);
    uint8_t *config = pdev->config + offset;
    if (!offset)
        return -ENOSPC;
    config[PCI_CAP_LIST_ID] = cap_id;
    config[PCI_CAP_LIST_NEXT] = pdev->config[PCI_CAPABILITY_LIST];
    pdev->config[PCI_CAPABILITY_LIST] = offset;
    pdev->config[PCI_STATUS] |= PCI_STATUS_CAP_LIST;
    memset(pdev->used + offset, 0xFF, size);
    /* Make capability read-only by default */
    memset(pdev->wmask + offset, 0, size);
    /* Check capability by default */
    memset(pdev->cmask + offset, 0xFF, size);
    return offset;
}

/* Unlink capability from the pci config space. */
void pci_del_capability(PCIDevice *pdev, uint8_t cap_id, uint8_t size)
{
    uint8_t prev, offset = pci_find_capability_list(pdev, cap_id, &prev);
    if (!offset)
        return;
    pdev->config[prev] = pdev->config[offset + PCI_CAP_LIST_NEXT];
    /* Make capability writeable again */
    memset(pdev->wmask + offset, 0xff, size);
    /* Clear cmask as device-specific registers can't be checked */
    memset(pdev->cmask + offset, 0, size);
    memset(pdev->used + offset, 0, size);

    if (!pdev->config[PCI_CAPABILITY_LIST])
        pdev->config[PCI_STATUS] &= ~PCI_STATUS_CAP_LIST;
}

/* Reserve space for capability at a known offset (to call after load). */
void pci_reserve_capability(PCIDevice *pdev, uint8_t offset, uint8_t size)
{
    memset(pdev->used + offset, 0xff, size);
}

uint8_t pci_find_capability(PCIDevice *pdev, uint8_t cap_id)
{
    return pci_find_capability_list(pdev, cap_id, NULL);
}

static void pcibus_dev_print(Monitor *mon, DeviceState *dev, int indent)
{
    PCIDevice *d = (PCIDevice *)dev;
    const pci_class_desc *desc;
    char ctxt[64];
    PCIIORegion *r;
    int i, class;

    class = le16_to_cpu(*((uint16_t *)(d->config + PCI_CLASS_DEVICE)));
    desc = pci_class_descriptions;
    while (desc->desc && class != desc->class)
        desc++;
    if (desc->desc) {
        snprintf(ctxt, sizeof(ctxt), "%s", desc->desc);
    } else {
        snprintf(ctxt, sizeof(ctxt), "Class %04x", class);
    }

    monitor_printf(mon, "%*sclass %s, addr %02x:%02x.%x, "
                   "pci id %04x:%04x (sub %04x:%04x)\n",
                   indent, "", ctxt,
                   d->bus->bus_num, d->devfn >> 3, d->devfn & 7,
                   le16_to_cpu(*((uint16_t *)(d->config + PCI_VENDOR_ID))),
                   le16_to_cpu(*((uint16_t *)(d->config + PCI_DEVICE_ID))),
                   le16_to_cpu(*((uint16_t *)(d->config + PCI_SUBSYSTEM_VENDOR_ID))),
                   le16_to_cpu(*((uint16_t *)(d->config + PCI_SUBSYSTEM_ID))));
    for (i = 0; i < PCI_NUM_REGIONS; i++) {
        r = &d->io_regions[i];
        if (!r->size)
            continue;
        monitor_printf(mon, "%*sbar %d: %s at 0x%x [0x%x]\n", indent, "",
                       i, r->type & PCI_ADDRESS_SPACE_IO ? "i/o" : "mem",
                       r->addr, r->addr + r->size - 1);
    }
}
