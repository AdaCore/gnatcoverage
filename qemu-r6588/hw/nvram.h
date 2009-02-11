#ifndef NVRAM_H
#define NVRAM_H

/* NVRAM helpers */
typedef uint32_t (*nvram_read_t)(void *private, uint32_t addr);
typedef void (*nvram_write_t)(void *private, uint32_t addr, uint32_t val);
typedef struct nvram_t {
    void *opaque;
    nvram_read_t read_fn;
    nvram_write_t write_fn;
} nvram_t;

void NVRAM_set_byte (nvram_t *nvram, uint32_t addr, uint8_t value);
uint8_t NVRAM_get_byte (nvram_t *nvram, uint32_t addr);
void NVRAM_set_word (nvram_t *nvram, uint32_t addr, uint16_t value);
uint16_t NVRAM_get_word (nvram_t *nvram, uint32_t addr);
void NVRAM_set_lword (nvram_t *nvram, uint32_t addr, uint32_t value);
uint32_t NVRAM_get_lword (nvram_t *nvram, uint32_t addr);
void NVRAM_set_string (nvram_t *nvram, uint32_t addr,
                       const char *str, uint32_t max);
int NVRAM_get_string (nvram_t *nvram, uint8_t *dst, uint16_t addr, int max);
void NVRAM_set_crc (nvram_t *nvram, uint32_t addr,
                    uint32_t start, uint32_t count);
int PPC_NVRAM_set_params (nvram_t *nvram, uint16_t NVRAM_size,
                          const char *arch,
                          uint32_t RAM_size, int boot_device,
                          uint32_t kernel_image, uint32_t kernel_size,
                          const char *cmdline,
                          uint32_t initrd_image, uint32_t initrd_size,
                          uint32_t NVRAM_image,
                          int width, int height, int depth);
typedef struct m48t59_t m48t59_t;

void m48t59_write (void *private, uint32_t addr, uint32_t val);
uint32_t m48t59_read (void *private, uint32_t addr);
void m48t59_toggle_lock (void *private, int lock);
m48t59_t *m48t59_init (qemu_irq IRQ, target_phys_addr_t mem_base,
                       uint32_t io_base, uint16_t size,
                       int type);
void m48t59_set_addr (void *opaque, uint32_t addr);

#endif /* !NVRAM_H */
