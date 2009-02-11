#ifndef SYSEMU_H
#define SYSEMU_H
/* Misc. things related to the system emulator.  */

/* vl.c */
extern const char *bios_name;
extern const char *bios_dir;

extern int vm_running;
extern const char *qemu_name;
extern uint8_t qemu_uuid[];
#define UUID_FMT "%02hhx%02hhx%02hhx%02hhx-%02hhx%02hhx-%02hhx%02hhx-%02hhx%02hhx-%02hhx%02hhx%02hhx%02hhx%02hhx%02hhx"

typedef struct vm_change_state_entry VMChangeStateEntry;
typedef void VMChangeStateHandler(void *opaque, int running, int reason);

VMChangeStateEntry *qemu_add_vm_change_state_handler(VMChangeStateHandler *cb,
                                                     void *opaque);
void qemu_del_vm_change_state_handler(VMChangeStateEntry *e);

void vm_start(void);
void vm_stop(int reason);

int64_t cpu_get_ticks(void);
void cpu_enable_ticks(void);
void cpu_disable_ticks(void);

void qemu_system_reset_request(void);
void qemu_system_shutdown_request(void);
void qemu_system_powerdown_request(void);
int qemu_shutdown_requested(void);
int qemu_reset_requested(void);
int qemu_powerdown_requested(void);
#if !defined(TARGET_SPARC) && !defined(TARGET_I386)
// Please implement a power failure function to signal the OS
#define qemu_system_powerdown() do{}while(0)
#else
void qemu_system_powerdown(void);
#endif
void qemu_system_reset(void);

void do_savevm(const char *name);
void do_loadvm(const char *name);
void do_delvm(const char *name);
void do_info_snapshots(void);

void qemu_announce_self(void);

void main_loop_wait(int timeout);

int qemu_savevm_state_begin(QEMUFile *f);
int qemu_savevm_state_iterate(QEMUFile *f);
int qemu_savevm_state_complete(QEMUFile *f);
int qemu_savevm_state(QEMUFile *f);
int qemu_loadvm_state(QEMUFile *f);

#ifdef _WIN32
/* Polling handling */

/* return TRUE if no sleep should be done afterwards */
typedef int PollingFunc(void *opaque);

int qemu_add_polling_cb(PollingFunc *func, void *opaque);
void qemu_del_polling_cb(PollingFunc *func, void *opaque);

/* Wait objects handling */
typedef void WaitObjectFunc(void *opaque);

int qemu_add_wait_object(HANDLE handle, WaitObjectFunc *func, void *opaque);
void qemu_del_wait_object(HANDLE handle, WaitObjectFunc *func, void *opaque);
#endif

/* TAP win32 */
int tap_win32_init(VLANState *vlan, const char *model,
                   const char *name, const char *ifname);

/* SLIRP */
void do_info_slirp(void);

extern int bios_size;
extern int cirrus_vga_enabled;
extern int std_vga_enabled;
extern int vmsvga_enabled;
extern int graphic_width;
extern int graphic_height;
extern int graphic_depth;
extern int nographic;
extern const char *keyboard_layout;
extern int win2k_install_hack;
extern int rtc_td_hack;
extern int alt_grab;
extern int usb_enabled;
extern int smp_cpus;
extern int cursor_hide;
extern int graphic_rotate;
extern int no_quit;
extern int semihosting_enabled;
extern int old_param;
extern const char *bootp_filename;

#ifdef USE_KQEMU
extern int kqemu_allowed;
#endif

#define MAX_OPTION_ROMS 16
extern const char *option_rom[MAX_OPTION_ROMS];
extern int nb_option_roms;

#if defined(TARGET_SPARC) || defined(TARGET_PPC)
#define MAX_PROM_ENVS 128
extern const char *prom_envs[MAX_PROM_ENVS];
extern unsigned int nb_prom_envs;
#endif

#if defined (TARGET_PPC)
#define BIOS_SIZE (1024 * 1024)
#elif defined (TARGET_SPARC64)
#define BIOS_SIZE ((512 + 32) * 1024)
#elif defined(TARGET_MIPS)
#define BIOS_SIZE (4 * 1024 * 1024)
#endif

typedef enum {
    IF_IDE, IF_SCSI, IF_FLOPPY, IF_PFLASH, IF_MTD, IF_SD, IF_VIRTIO
} BlockInterfaceType;

typedef enum {
    BLOCK_ERR_REPORT, BLOCK_ERR_IGNORE, BLOCK_ERR_STOP_ENOSPC,
    BLOCK_ERR_STOP_ANY
} BlockInterfaceErrorAction;

typedef struct DriveInfo {
    BlockDriverState *bdrv;
    BlockInterfaceType type;
    int bus;
    int unit;
    BlockInterfaceErrorAction onerror;
    char serial[21];
} DriveInfo;

#define MAX_IDE_DEVS	2
#define MAX_SCSI_DEVS	7
#define MAX_DRIVES 32

extern int nb_drives;
extern DriveInfo drives_table[MAX_DRIVES+1];

extern int drive_get_index(BlockInterfaceType type, int bus, int unit);
extern int drive_get_max_bus(BlockInterfaceType type);
extern const char *drive_get_serial(BlockDriverState *bdrv);
extern BlockInterfaceErrorAction drive_get_onerror(BlockDriverState *bdrv);

/* serial ports */

#define MAX_SERIAL_PORTS 4

extern CharDriverState *serial_hds[MAX_SERIAL_PORTS];

/* parallel ports */

#define MAX_PARALLEL_PORTS 3

extern CharDriverState *parallel_hds[MAX_PARALLEL_PORTS];

/* virtio consoles */

#define MAX_VIRTIO_CONSOLES 1

extern CharDriverState *virtcon_hds[MAX_VIRTIO_CONSOLES];

#define TFR(expr) do { if ((expr) != -1) break; } while (errno == EINTR)

#ifdef NEED_CPU_H
/* loader.c */
int get_image_size(const char *filename);
int load_image(const char *filename, uint8_t *addr); /* deprecated */
int load_image_targphys(const char *filename, target_phys_addr_t, int max_sz);
int load_elf(const char *filename, int64_t address_offset,
             uint64_t *pentry, uint64_t *lowaddr, uint64_t *highaddr);
int load_aout(const char *filename, target_phys_addr_t addr, int max_sz);
int load_uimage(const char *filename, target_ulong *ep, target_ulong *loadaddr,
                int *is_linux);

int fread_targphys(target_phys_addr_t dst_addr, size_t nbytes, FILE *f);
int fread_targphys_ok(target_phys_addr_t dst_addr, size_t nbytes, FILE *f);
int read_targphys(int fd, target_phys_addr_t dst_addr, size_t nbytes);
void pstrcpy_targphys(target_phys_addr_t dest, int buf_size,
                      const char *source);
#endif

#ifdef HAS_AUDIO
struct soundhw {
    const char *name;
    const char *descr;
    int enabled;
    int isa;
    union {
        int (*init_isa) (AudioState *s, qemu_irq *pic);
        int (*init_pci) (PCIBus *bus, AudioState *s);
    } init;
};

extern struct soundhw soundhw[];
#endif

void do_usb_add(const char *devname);
void do_usb_del(const char *devname);
void usb_info(void);

const char *get_opt_name(char *buf, int buf_size, const char *p);
const char *get_opt_value(char *buf, int buf_size, const char *p);
int get_param_value(char *buf, int buf_size,
                    const char *tag, const char *str);
int check_params(char *buf, int buf_size,
                 const char * const *params, const char *str);

#endif
