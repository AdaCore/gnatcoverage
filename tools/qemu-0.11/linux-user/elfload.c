/* This is the Linux kernel elf-loading code, ported into user space */
#include <sys/time.h>
#include <sys/param.h>

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "qemu.h"
#include "disas.h"

#ifdef _ARCH_PPC64
#undef ARCH_DLINFO
#undef ELF_PLATFORM
#undef ELF_HWCAP
#undef ELF_CLASS
#undef ELF_DATA
#undef ELF_ARCH
#endif

#define ELF_OSABI   ELFOSABI_SYSV

/* from personality.h */

/*
 * Flags for bug emulation.
 *
 * These occupy the top three bytes.
 */
enum {
	ADDR_NO_RANDOMIZE = 	0x0040000,	/* disable randomization of VA space */
	FDPIC_FUNCPTRS =	0x0080000,	/* userspace function ptrs point to descriptors
						 * (signal handling)
						 */
	MMAP_PAGE_ZERO =	0x0100000,
	ADDR_COMPAT_LAYOUT =	0x0200000,
	READ_IMPLIES_EXEC =	0x0400000,
	ADDR_LIMIT_32BIT =	0x0800000,
	SHORT_INODE =		0x1000000,
	WHOLE_SECONDS =		0x2000000,
	STICKY_TIMEOUTS	=	0x4000000,
	ADDR_LIMIT_3GB = 	0x8000000,
};

/*
 * Personality types.
 *
 * These go in the low byte.  Avoid using the top bit, it will
 * conflict with error returns.
 */
enum {
	PER_LINUX =		0x0000,
	PER_LINUX_32BIT =	0x0000 | ADDR_LIMIT_32BIT,
	PER_LINUX_FDPIC =	0x0000 | FDPIC_FUNCPTRS,
	PER_SVR4 =		0x0001 | STICKY_TIMEOUTS | MMAP_PAGE_ZERO,
	PER_SVR3 =		0x0002 | STICKY_TIMEOUTS | SHORT_INODE,
	PER_SCOSVR3 =		0x0003 | STICKY_TIMEOUTS |
					 WHOLE_SECONDS | SHORT_INODE,
	PER_OSR5 =		0x0003 | STICKY_TIMEOUTS | WHOLE_SECONDS,
	PER_WYSEV386 =		0x0004 | STICKY_TIMEOUTS | SHORT_INODE,
	PER_ISCR4 =		0x0005 | STICKY_TIMEOUTS,
	PER_BSD =		0x0006,
	PER_SUNOS =		0x0006 | STICKY_TIMEOUTS,
	PER_XENIX =		0x0007 | STICKY_TIMEOUTS | SHORT_INODE,
	PER_LINUX32 =		0x0008,
	PER_LINUX32_3GB =	0x0008 | ADDR_LIMIT_3GB,
	PER_IRIX32 =		0x0009 | STICKY_TIMEOUTS,/* IRIX5 32-bit */
	PER_IRIXN32 =		0x000a | STICKY_TIMEOUTS,/* IRIX6 new 32-bit */
	PER_IRIX64 =		0x000b | STICKY_TIMEOUTS,/* IRIX6 64-bit */
	PER_RISCOS =		0x000c,
	PER_SOLARIS =		0x000d | STICKY_TIMEOUTS,
	PER_UW7 =		0x000e | STICKY_TIMEOUTS | MMAP_PAGE_ZERO,
	PER_OSF4 =		0x000f,			 /* OSF/1 v4 */
	PER_HPUX =		0x0010,
	PER_MASK =		0x00ff,
};

/*
 * Return the base personality without flags.
 */
#define personality(pers)	(pers & PER_MASK)

/* this flag is uneffective under linux too, should be deleted */
#ifndef MAP_DENYWRITE
#define MAP_DENYWRITE 0
#endif

/* should probably go in elf.h */
#ifndef ELIBBAD
#define ELIBBAD 80
#endif

#ifdef TARGET_I386

#define ELF_PLATFORM get_elf_platform()

static const char *get_elf_platform(void)
{
    static char elf_platform[] = "i386";
    int family = (thread_env->cpuid_version >> 8) & 0xff;
    if (family > 6)
        family = 6;
    if (family >= 3)
        elf_platform[1] = '0' + family;
    return elf_platform;
}

#define ELF_HWCAP get_elf_hwcap()

static uint32_t get_elf_hwcap(void)
{
  return thread_env->cpuid_features;
}

#ifdef TARGET_X86_64
#define ELF_START_MMAP 0x2aaaaab000ULL
#define elf_check_arch(x) ( ((x) == ELF_ARCH) )

#define ELF_CLASS      ELFCLASS64
#define ELF_DATA       ELFDATA2LSB
#define ELF_ARCH       EM_X86_64

static inline void init_thread(struct target_pt_regs *regs, struct image_info *infop)
{
    regs->rax = 0;
    regs->rsp = infop->start_stack;
    regs->rip = infop->entry;
}

typedef target_ulong    target_elf_greg_t;
typedef uint32_t        target_uid_t;
typedef uint32_t        target_gid_t;
typedef int32_t         target_pid_t;

#define ELF_NREG    27
typedef target_elf_greg_t  target_elf_gregset_t[ELF_NREG];

/*
 * Note that ELF_NREG should be 29 as there should be place for
 * TRAPNO and ERR "registers" as well but linux doesn't dump
 * those.
 *
 * See linux kernel: arch/x86/include/asm/elf.h
 */
static void elf_core_copy_regs(target_elf_gregset_t *regs, const CPUState *env)
{
    (*regs)[0] = env->regs[15];
    (*regs)[1] = env->regs[14];
    (*regs)[2] = env->regs[13];
    (*regs)[3] = env->regs[12];
    (*regs)[4] = env->regs[R_EBP];
    (*regs)[5] = env->regs[R_EBX];
    (*regs)[6] = env->regs[11];
    (*regs)[7] = env->regs[10];
    (*regs)[8] = env->regs[9];
    (*regs)[9] = env->regs[8];
    (*regs)[10] = env->regs[R_EAX];
    (*regs)[11] = env->regs[R_ECX];
    (*regs)[12] = env->regs[R_EDX];
    (*regs)[13] = env->regs[R_ESI];
    (*regs)[14] = env->regs[R_EDI];
    (*regs)[15] = env->regs[R_EAX]; /* XXX */
    (*regs)[16] = env->eip;
    (*regs)[17] = env->segs[R_CS].selector & 0xffff;
    (*regs)[18] = env->eflags;
    (*regs)[19] = env->regs[R_ESP];
    (*regs)[20] = env->segs[R_SS].selector & 0xffff;
    (*regs)[21] = env->segs[R_FS].selector & 0xffff;
    (*regs)[22] = env->segs[R_GS].selector & 0xffff;
    (*regs)[23] = env->segs[R_DS].selector & 0xffff;
    (*regs)[24] = env->segs[R_ES].selector & 0xffff;
    (*regs)[25] = env->segs[R_FS].selector & 0xffff;
    (*regs)[26] = env->segs[R_GS].selector & 0xffff;
}

#else

#define ELF_START_MMAP 0x80000000

/*
 * This is used to ensure we don't load something for the wrong architecture.
 */
#define elf_check_arch(x) ( ((x) == EM_386) || ((x) == EM_486) )

/*
 * These are used to set parameters in the core dumps.
 */
#define ELF_CLASS	ELFCLASS32
#define ELF_DATA	ELFDATA2LSB
#define ELF_ARCH	EM_386

static inline void init_thread(struct target_pt_regs *regs, struct image_info *infop)
{
    regs->esp = infop->start_stack;
    regs->eip = infop->entry;

    /* SVR4/i386 ABI (pages 3-31, 3-32) says that when the program
       starts %edx contains a pointer to a function which might be
       registered using `atexit'.  This provides a mean for the
       dynamic linker to call DT_FINI functions for shared libraries
       that have been loaded before the code runs.

       A value of 0 tells we have no such handler.  */
    regs->edx = 0;
}

typedef target_ulong    target_elf_greg_t;
typedef uint16_t        target_uid_t;
typedef uint16_t        target_gid_t;
typedef int32_t         target_pid_t;

#define ELF_NREG    17
typedef target_elf_greg_t  target_elf_gregset_t[ELF_NREG];

/*
 * Note that ELF_NREG should be 19 as there should be place for
 * TRAPNO and ERR "registers" as well but linux doesn't dump
 * those.
 *
 * See linux kernel: arch/x86/include/asm/elf.h
 */
static void elf_core_copy_regs(target_elf_gregset_t *regs, const CPUState *env)
{
    (*regs)[0] = env->regs[R_EBX];
    (*regs)[1] = env->regs[R_ECX];
    (*regs)[2] = env->regs[R_EDX];
    (*regs)[3] = env->regs[R_ESI];
    (*regs)[4] = env->regs[R_EDI];
    (*regs)[5] = env->regs[R_EBP];
    (*regs)[6] = env->regs[R_EAX];
    (*regs)[7] = env->segs[R_DS].selector & 0xffff;
    (*regs)[8] = env->segs[R_ES].selector & 0xffff;
    (*regs)[9] = env->segs[R_FS].selector & 0xffff;
    (*regs)[10] = env->segs[R_GS].selector & 0xffff;
    (*regs)[11] = env->regs[R_EAX]; /* XXX */
    (*regs)[12] = env->eip;
    (*regs)[13] = env->segs[R_CS].selector & 0xffff;
    (*regs)[14] = env->eflags;
    (*regs)[15] = env->regs[R_ESP];
    (*regs)[16] = env->segs[R_SS].selector & 0xffff;
}
#endif

#define USE_ELF_CORE_DUMP
#define ELF_EXEC_PAGESIZE	4096

#endif

#ifdef TARGET_ARM

#define ELF_START_MMAP 0x80000000

#define elf_check_arch(x) ( (x) == EM_ARM )

#define ELF_CLASS	ELFCLASS32
#ifdef TARGET_WORDS_BIGENDIAN
#define ELF_DATA	ELFDATA2MSB
#else
#define ELF_DATA	ELFDATA2LSB
#endif
#define ELF_ARCH	EM_ARM

static inline void init_thread(struct target_pt_regs *regs, struct image_info *infop)
{
    abi_long stack = infop->start_stack;
    memset(regs, 0, sizeof(*regs));
    regs->ARM_cpsr = 0x10;
    if (infop->entry & 1)
      regs->ARM_cpsr |= CPSR_T;
    regs->ARM_pc = infop->entry & 0xfffffffe;
    regs->ARM_sp = infop->start_stack;
    /* FIXME - what to for failure of get_user()? */
    get_user_ual(regs->ARM_r2, stack + 8); /* envp */
    get_user_ual(regs->ARM_r1, stack + 4); /* envp */
    /* XXX: it seems that r0 is zeroed after ! */
    regs->ARM_r0 = 0;
    /* For uClinux PIC binaries.  */
    /* XXX: Linux does this only on ARM with no MMU (do we care ?) */
    regs->ARM_r10 = infop->start_data;
}

typedef uint32_t target_elf_greg_t;
typedef uint16_t target_uid_t;
typedef uint16_t target_gid_t;
typedef int32_t  target_pid_t;

#define ELF_NREG    18
typedef target_elf_greg_t  target_elf_gregset_t[ELF_NREG];

static void elf_core_copy_regs(target_elf_gregset_t *regs, const CPUState *env)
{
    (*regs)[0] = env->regs[0];
    (*regs)[1] = env->regs[1];
    (*regs)[2] = env->regs[2];
    (*regs)[3] = env->regs[3];
    (*regs)[4] = env->regs[4];
    (*regs)[5] = env->regs[5];
    (*regs)[6] = env->regs[6];
    (*regs)[7] = env->regs[7];
    (*regs)[8] = env->regs[8];
    (*regs)[9] = env->regs[9];
    (*regs)[10] = env->regs[10];
    (*regs)[11] = env->regs[11];
    (*regs)[12] = env->regs[12];
    (*regs)[13] = env->regs[13];
    (*regs)[14] = env->regs[14];
    (*regs)[15] = env->regs[15];

    (*regs)[16] = cpsr_read((CPUState *)env);
    (*regs)[17] = env->regs[0]; /* XXX */
}

#define USE_ELF_CORE_DUMP
#define ELF_EXEC_PAGESIZE	4096

enum
{
  ARM_HWCAP_ARM_SWP       = 1 << 0,
  ARM_HWCAP_ARM_HALF      = 1 << 1,
  ARM_HWCAP_ARM_THUMB     = 1 << 2,
  ARM_HWCAP_ARM_26BIT     = 1 << 3,
  ARM_HWCAP_ARM_FAST_MULT = 1 << 4,
  ARM_HWCAP_ARM_FPA       = 1 << 5,
  ARM_HWCAP_ARM_VFP       = 1 << 6,
  ARM_HWCAP_ARM_EDSP      = 1 << 7,
};

#define ELF_HWCAP (ARM_HWCAP_ARM_SWP | ARM_HWCAP_ARM_HALF              \
                    | ARM_HWCAP_ARM_THUMB | ARM_HWCAP_ARM_FAST_MULT     \
                    | ARM_HWCAP_ARM_FPA | ARM_HWCAP_ARM_VFP)

#endif

#ifdef TARGET_SPARC
#ifdef TARGET_SPARC64

#define ELF_START_MMAP 0x80000000

#ifndef TARGET_ABI32
#define elf_check_arch(x) ( (x) == EM_SPARCV9 || (x) == EM_SPARC32PLUS )
#else
#define elf_check_arch(x) ( (x) == EM_SPARC32PLUS || (x) == EM_SPARC )
#endif

#define ELF_CLASS   ELFCLASS64
#define ELF_DATA    ELFDATA2MSB
#define ELF_ARCH    EM_SPARCV9

#define STACK_BIAS		2047

static inline void init_thread(struct target_pt_regs *regs, struct image_info *infop)
{
#ifndef TARGET_ABI32
    regs->tstate = 0;
#endif
    regs->pc = infop->entry;
    regs->npc = regs->pc + 4;
    regs->y = 0;
#ifdef TARGET_ABI32
    regs->u_regs[14] = infop->start_stack - 16 * 4;
#else
    if (personality(infop->personality) == PER_LINUX32)
        regs->u_regs[14] = infop->start_stack - 16 * 4;
    else
        regs->u_regs[14] = infop->start_stack - 16 * 8 - STACK_BIAS;
#endif
}

#else
#define ELF_START_MMAP 0x80000000

#define elf_check_arch(x) ( (x) == EM_SPARC )

#define ELF_CLASS   ELFCLASS32
#define ELF_DATA    ELFDATA2MSB
#define ELF_ARCH    EM_SPARC

static inline void init_thread(struct target_pt_regs *regs, struct image_info *infop)
{
    regs->psr = 0;
    regs->pc = infop->entry;
    regs->npc = regs->pc + 4;
    regs->y = 0;
    regs->u_regs[14] = infop->start_stack - 16 * 4;
}

#endif
#endif

#ifdef TARGET_PPC

#define ELF_START_MMAP 0x80000000

#if defined(TARGET_PPC64) && !defined(TARGET_ABI32)

#define elf_check_arch(x) ( (x) == EM_PPC64 )

#define ELF_CLASS	ELFCLASS64

#else

#define elf_check_arch(x) ( (x) == EM_PPC )

#define ELF_CLASS	ELFCLASS32

#endif

#ifdef TARGET_WORDS_BIGENDIAN
#define ELF_DATA	ELFDATA2MSB
#else
#define ELF_DATA	ELFDATA2LSB
#endif
#define ELF_ARCH	EM_PPC

/* Feature masks for the Aux Vector Hardware Capabilities (AT_HWCAP).
   See arch/powerpc/include/asm/cputable.h.  */
enum {
    PPC_FEATURE_32 = 0x80000000,
    PPC_FEATURE_64 = 0x40000000,
    PPC_FEATURE_601_INSTR = 0x20000000,
    PPC_FEATURE_HAS_ALTIVEC = 0x10000000,
    PPC_FEATURE_HAS_FPU = 0x08000000,
    PPC_FEATURE_HAS_MMU = 0x04000000,
    PPC_FEATURE_HAS_4xxMAC = 0x02000000,
    PPC_FEATURE_UNIFIED_CACHE = 0x01000000,
    PPC_FEATURE_HAS_SPE = 0x00800000,
    PPC_FEATURE_HAS_EFP_SINGLE = 0x00400000,
    PPC_FEATURE_HAS_EFP_DOUBLE = 0x00200000,
    PPC_FEATURE_NO_TB = 0x00100000,
    PPC_FEATURE_POWER4 = 0x00080000,
    PPC_FEATURE_POWER5 = 0x00040000,
    PPC_FEATURE_POWER5_PLUS = 0x00020000,
    PPC_FEATURE_CELL = 0x00010000,
    PPC_FEATURE_BOOKE = 0x00008000,
    PPC_FEATURE_SMT = 0x00004000,
    PPC_FEATURE_ICACHE_SNOOP = 0x00002000,
    PPC_FEATURE_ARCH_2_05 = 0x00001000,
    PPC_FEATURE_PA6T = 0x00000800,
    PPC_FEATURE_HAS_DFP = 0x00000400,
    PPC_FEATURE_POWER6_EXT = 0x00000200,
    PPC_FEATURE_ARCH_2_06 = 0x00000100,
    PPC_FEATURE_HAS_VSX = 0x00000080,
    PPC_FEATURE_PSERIES_PERFMON_COMPAT = 0x00000040,

    PPC_FEATURE_TRUE_LE = 0x00000002,
    PPC_FEATURE_PPC_LE = 0x00000001,
};

#define ELF_HWCAP get_elf_hwcap()

static uint32_t get_elf_hwcap(void)
{
    CPUState *e = thread_env;
    uint32_t features = 0;

    /* We don't have to be terribly complete here; the high points are
       Altivec/FP/SPE support.  Anything else is just a bonus.  */
#define GET_FEATURE(flag, feature)              \
    do {if (e->insns_flags & flag) features |= feature; } while(0)
    GET_FEATURE(PPC_64B, PPC_FEATURE_64);
    GET_FEATURE(PPC_FLOAT, PPC_FEATURE_HAS_FPU);
    GET_FEATURE(PPC_ALTIVEC, PPC_FEATURE_HAS_ALTIVEC);
    GET_FEATURE(PPC_SPE, PPC_FEATURE_HAS_SPE);
    GET_FEATURE(PPC_SPE_SINGLE, PPC_FEATURE_HAS_EFP_SINGLE);
    GET_FEATURE(PPC_SPE_DOUBLE, PPC_FEATURE_HAS_EFP_DOUBLE);
    GET_FEATURE(PPC_BOOKE, PPC_FEATURE_BOOKE);
    GET_FEATURE(PPC_405_MAC, PPC_FEATURE_HAS_4xxMAC);
#undef GET_FEATURE

    return features;
}

/*
 * We need to put in some extra aux table entries to tell glibc what
 * the cache block size is, so it can use the dcbz instruction safely.
 */
#define AT_DCACHEBSIZE          19
#define AT_ICACHEBSIZE          20
#define AT_UCACHEBSIZE          21
/* A special ignored type value for PPC, for glibc compatibility.  */
#define AT_IGNOREPPC            22
/*
 * The requirements here are:
 * - keep the final alignment of sp (sp & 0xf)
 * - make sure the 32-bit value at the first 16 byte aligned position of
 *   AUXV is greater than 16 for glibc compatibility.
 *   AT_IGNOREPPC is used for that.
 * - for compatibility with glibc ARCH_DLINFO must always be defined on PPC,
 *   even if DLINFO_ARCH_ITEMS goes to zero or is undefined.
 */
#define DLINFO_ARCH_ITEMS       5
#define ARCH_DLINFO                                                     \
do {                                                                    \
        NEW_AUX_ENT(AT_DCACHEBSIZE, 0x20);                              \
        NEW_AUX_ENT(AT_ICACHEBSIZE, 0x20);                              \
        NEW_AUX_ENT(AT_UCACHEBSIZE, 0);                                 \
        /*                                                              \
         * Now handle glibc compatibility.                              \
         */                                                             \
	NEW_AUX_ENT(AT_IGNOREPPC, AT_IGNOREPPC);			\
	NEW_AUX_ENT(AT_IGNOREPPC, AT_IGNOREPPC);			\
 } while (0)

static inline void init_thread(struct target_pt_regs *_regs, struct image_info *infop)
{
    abi_ulong pos = infop->start_stack;
    abi_ulong tmp;
#if defined(TARGET_PPC64) && !defined(TARGET_ABI32)
    abi_ulong entry, toc;
#endif

    _regs->gpr[1] = infop->start_stack;
#if defined(TARGET_PPC64) && !defined(TARGET_ABI32)
    entry = ldq_raw(infop->entry) + infop->load_addr;
    toc = ldq_raw(infop->entry + 8) + infop->load_addr;
    _regs->gpr[2] = toc;
    infop->entry = entry;
#endif
    _regs->nip = infop->entry;
    /* Note that isn't exactly what regular kernel does
     * but this is what the ABI wants and is needed to allow
     * execution of PPC BSD programs.
     */
    /* FIXME - what to for failure of get_user()? */
    get_user_ual(_regs->gpr[3], pos);
    pos += sizeof(abi_ulong);
    _regs->gpr[4] = pos;
    for (tmp = 1; tmp != 0; pos += sizeof(abi_ulong))
        tmp = ldl(pos);
    _regs->gpr[5] = pos;
}

#define ELF_EXEC_PAGESIZE	4096

#endif

#ifdef TARGET_MIPS

#define ELF_START_MMAP 0x80000000

#define elf_check_arch(x) ( (x) == EM_MIPS )

#ifdef TARGET_MIPS64
#define ELF_CLASS   ELFCLASS64
#else
#define ELF_CLASS   ELFCLASS32
#endif
#ifdef TARGET_WORDS_BIGENDIAN
#define ELF_DATA	ELFDATA2MSB
#else
#define ELF_DATA	ELFDATA2LSB
#endif
#define ELF_ARCH    EM_MIPS

static inline void init_thread(struct target_pt_regs *regs, struct image_info *infop)
{
    regs->cp0_status = 2 << CP0St_KSU;
    regs->cp0_epc = infop->entry;
    regs->regs[29] = infop->start_stack;
}

#define ELF_EXEC_PAGESIZE        4096

#endif /* TARGET_MIPS */

#ifdef TARGET_MICROBLAZE

#define ELF_START_MMAP 0x80000000

#define elf_check_arch(x) ( (x) == EM_XILINX_MICROBLAZE )

#define ELF_CLASS   ELFCLASS32
#define ELF_DATA	ELFDATA2MSB
#define ELF_ARCH    EM_MIPS

static inline void init_thread(struct target_pt_regs *regs, struct image_info *infop)
{
    regs->pc = infop->entry;
    regs->r1 = infop->start_stack;

}

#define ELF_EXEC_PAGESIZE        4096

#endif /* TARGET_MICROBLAZE */

#ifdef TARGET_SH4

#define ELF_START_MMAP 0x80000000

#define elf_check_arch(x) ( (x) == EM_SH )

#define ELF_CLASS ELFCLASS32
#define ELF_DATA  ELFDATA2LSB
#define ELF_ARCH  EM_SH

static inline void init_thread(struct target_pt_regs *regs, struct image_info *infop)
{
  /* Check other registers XXXXX */
  regs->pc = infop->entry;
  regs->regs[15] = infop->start_stack;
}

#define ELF_EXEC_PAGESIZE        4096

#endif

#ifdef TARGET_CRIS

#define ELF_START_MMAP 0x80000000

#define elf_check_arch(x) ( (x) == EM_CRIS )

#define ELF_CLASS ELFCLASS32
#define ELF_DATA  ELFDATA2LSB
#define ELF_ARCH  EM_CRIS

static inline void init_thread(struct target_pt_regs *regs, struct image_info *infop)
{
  regs->erp = infop->entry;
}

#define ELF_EXEC_PAGESIZE        8192

#endif

#ifdef TARGET_M68K

#define ELF_START_MMAP 0x80000000

#define elf_check_arch(x) ( (x) == EM_68K )

#define ELF_CLASS	ELFCLASS32
#define ELF_DATA	ELFDATA2MSB
#define ELF_ARCH	EM_68K

/* ??? Does this need to do anything?
#define ELF_PLAT_INIT(_r) */

static inline void init_thread(struct target_pt_regs *regs, struct image_info *infop)
{
    regs->usp = infop->start_stack;
    regs->sr = 0;
    regs->pc = infop->entry;
}

#define ELF_EXEC_PAGESIZE	8192

#endif

#ifdef TARGET_ALPHA

#define ELF_START_MMAP (0x30000000000ULL)

#define elf_check_arch(x) ( (x) == ELF_ARCH )

#define ELF_CLASS      ELFCLASS64
#define ELF_DATA       ELFDATA2MSB
#define ELF_ARCH       EM_ALPHA

static inline void init_thread(struct target_pt_regs *regs, struct image_info *infop)
{
    regs->pc = infop->entry;
    regs->ps = 8;
    regs->usp = infop->start_stack;
    regs->unique = infop->start_data; /* ? */
    printf("Set unique value to " TARGET_FMT_lx " (" TARGET_FMT_lx ")\n",
           regs->unique, infop->start_data);
}

#define ELF_EXEC_PAGESIZE        8192

#endif /* TARGET_ALPHA */

#ifndef ELF_PLATFORM
#define ELF_PLATFORM (NULL)
#endif

#ifndef ELF_HWCAP
#define ELF_HWCAP 0
#endif

#ifdef TARGET_ABI32
#undef ELF_CLASS
#define ELF_CLASS ELFCLASS32
#undef bswaptls
#define bswaptls(ptr) bswap32s(ptr)
#endif

#include "elf.h"

struct exec
{
  unsigned int a_info;   /* Use macros N_MAGIC, etc for access */
  unsigned int a_text;   /* length of text, in bytes */
  unsigned int a_data;   /* length of data, in bytes */
  unsigned int a_bss;    /* length of uninitialized data area, in bytes */
  unsigned int a_syms;   /* length of symbol table data in file, in bytes */
  unsigned int a_entry;  /* start address */
  unsigned int a_trsize; /* length of relocation info for text, in bytes */
  unsigned int a_drsize; /* length of relocation info for data, in bytes */
};


#define N_MAGIC(exec) ((exec).a_info & 0xffff)
#define OMAGIC 0407
#define NMAGIC 0410
#define ZMAGIC 0413
#define QMAGIC 0314

/* max code+data+bss space allocated to elf interpreter */
#define INTERP_MAP_SIZE (32 * 1024 * 1024)

/* max code+data+bss+brk space allocated to ET_DYN executables */
#define ET_DYN_MAP_SIZE (128 * 1024 * 1024)

/* Necessary parameters */
#define TARGET_ELF_EXEC_PAGESIZE TARGET_PAGE_SIZE
#define TARGET_ELF_PAGESTART(_v) ((_v) & ~(unsigned long)(TARGET_ELF_EXEC_PAGESIZE-1))
#define TARGET_ELF_PAGEOFFSET(_v) ((_v) & (TARGET_ELF_EXEC_PAGESIZE-1))

#define INTERPRETER_NONE 0
#define INTERPRETER_AOUT 1
#define INTERPRETER_ELF 2

#define DLINFO_ITEMS 12

static inline void memcpy_fromfs(void * to, const void * from, unsigned long n)
{
	memcpy(to, from, n);
}

static int load_aout_interp(void * exptr, int interp_fd);

#ifdef BSWAP_NEEDED
static void bswap_ehdr(struct elfhdr *ehdr)
{
    bswap16s(&ehdr->e_type);			/* Object file type */
    bswap16s(&ehdr->e_machine);		/* Architecture */
    bswap32s(&ehdr->e_version);		/* Object file version */
    bswaptls(&ehdr->e_entry);		/* Entry point virtual address */
    bswaptls(&ehdr->e_phoff);		/* Program header table file offset */
    bswaptls(&ehdr->e_shoff);		/* Section header table file offset */
    bswap32s(&ehdr->e_flags);		/* Processor-specific flags */
    bswap16s(&ehdr->e_ehsize);		/* ELF header size in bytes */
    bswap16s(&ehdr->e_phentsize);		/* Program header table entry size */
    bswap16s(&ehdr->e_phnum);		/* Program header table entry count */
    bswap16s(&ehdr->e_shentsize);		/* Section header table entry size */
    bswap16s(&ehdr->e_shnum);		/* Section header table entry count */
    bswap16s(&ehdr->e_shstrndx);		/* Section header string table index */
}

static void bswap_phdr(struct elf_phdr *phdr)
{
    bswap32s(&phdr->p_type);			/* Segment type */
    bswaptls(&phdr->p_offset);		/* Segment file offset */
    bswaptls(&phdr->p_vaddr);		/* Segment virtual address */
    bswaptls(&phdr->p_paddr);		/* Segment physical address */
    bswaptls(&phdr->p_filesz);		/* Segment size in file */
    bswaptls(&phdr->p_memsz);		/* Segment size in memory */
    bswap32s(&phdr->p_flags);		/* Segment flags */
    bswaptls(&phdr->p_align);		/* Segment alignment */
}

static void bswap_shdr(struct elf_shdr *shdr)
{
    bswap32s(&shdr->sh_name);
    bswap32s(&shdr->sh_type);
    bswaptls(&shdr->sh_flags);
    bswaptls(&shdr->sh_addr);
    bswaptls(&shdr->sh_offset);
    bswaptls(&shdr->sh_size);
    bswap32s(&shdr->sh_link);
    bswap32s(&shdr->sh_info);
    bswaptls(&shdr->sh_addralign);
    bswaptls(&shdr->sh_entsize);
}

static void bswap_sym(struct elf_sym *sym)
{
    bswap32s(&sym->st_name);
    bswaptls(&sym->st_value);
    bswaptls(&sym->st_size);
    bswap16s(&sym->st_shndx);
}
#endif

#ifdef USE_ELF_CORE_DUMP
static int elf_core_dump(int, const CPUState *);

#ifdef BSWAP_NEEDED
static void bswap_note(struct elf_note *en)
{
    bswaptls(&en->n_namesz);
    bswaptls(&en->n_descsz);
    bswaptls(&en->n_type);
}
#endif /* BSWAP_NEEDED */

#endif /* USE_ELF_CORE_DUMP */

/*
 * 'copy_elf_strings()' copies argument/envelope strings from user
 * memory to free pages in kernel mem. These are in a format ready
 * to be put directly into the top of new user memory.
 *
 */
static abi_ulong copy_elf_strings(int argc,char ** argv, void **page,
                                  abi_ulong p)
{
    char *tmp, *tmp1, *pag = NULL;
    int len, offset = 0;

    if (!p) {
	return 0;       /* bullet-proofing */
    }
    while (argc-- > 0) {
        tmp = argv[argc];
        if (!tmp) {
	    fprintf(stderr, "VFS: argc is wrong");
	    exit(-1);
	}
        tmp1 = tmp;
	while (*tmp++);
	len = tmp - tmp1;
	if (p < len) {  /* this shouldn't happen - 128kB */
		return 0;
	}
	while (len) {
	    --p; --tmp; --len;
	    if (--offset < 0) {
		offset = p % TARGET_PAGE_SIZE;
                pag = (char *)page[p/TARGET_PAGE_SIZE];
                if (!pag) {
                    pag = (char *)malloc(TARGET_PAGE_SIZE);
                    memset(pag, 0, TARGET_PAGE_SIZE);
                    page[p/TARGET_PAGE_SIZE] = pag;
                    if (!pag)
                        return 0;
		}
	    }
	    if (len == 0 || offset == 0) {
	        *(pag + offset) = *tmp;
	    }
	    else {
	      int bytes_to_copy = (len > offset) ? offset : len;
	      tmp -= bytes_to_copy;
	      p -= bytes_to_copy;
	      offset -= bytes_to_copy;
	      len -= bytes_to_copy;
	      memcpy_fromfs(pag + offset, tmp, bytes_to_copy + 1);
	    }
	}
    }
    return p;
}

static abi_ulong setup_arg_pages(abi_ulong p, struct linux_binprm *bprm,
                                 struct image_info *info)
{
    abi_ulong stack_base, size, error;
    int i;

    /* Create enough stack to hold everything.  If we don't use
     * it for args, we'll use it for something else...
     */
    size = x86_stack_size;
    if (size < MAX_ARG_PAGES*TARGET_PAGE_SIZE)
        size = MAX_ARG_PAGES*TARGET_PAGE_SIZE;
    error = target_mmap(0,
                        size + qemu_host_page_size,
                        PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANONYMOUS,
                        -1, 0);
    if (error == -1) {
        perror("stk mmap");
        exit(-1);
    }
    /* we reserve one extra page at the top of the stack as guard */
    target_mprotect(error + size, qemu_host_page_size, PROT_NONE);

    stack_base = error + size - MAX_ARG_PAGES*TARGET_PAGE_SIZE;
    p += stack_base;

    for (i = 0 ; i < MAX_ARG_PAGES ; i++) {
	if (bprm->page[i]) {
	    info->rss++;
            /* FIXME - check return value of memcpy_to_target() for failure */
	    memcpy_to_target(stack_base, bprm->page[i], TARGET_PAGE_SIZE);
	    free(bprm->page[i]);
	}
        stack_base += TARGET_PAGE_SIZE;
    }
    return p;
}

static void set_brk(abi_ulong start, abi_ulong end)
{
	/* page-align the start and end addresses... */
        start = HOST_PAGE_ALIGN(start);
        end = HOST_PAGE_ALIGN(end);
        if (end <= start)
                return;
        if(target_mmap(start, end - start,
                       PROT_READ | PROT_WRITE | PROT_EXEC,
                       MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0) == -1) {
	    perror("cannot mmap brk");
	    exit(-1);
	}
}


/* We need to explicitly zero any fractional pages after the data
   section (i.e. bss).  This would contain the junk from the file that
   should not be in memory. */
static void padzero(abi_ulong elf_bss, abi_ulong last_bss)
{
        abi_ulong nbyte;

	if (elf_bss >= last_bss)
		return;

        /* XXX: this is really a hack : if the real host page size is
           smaller than the target page size, some pages after the end
           of the file may not be mapped. A better fix would be to
           patch target_mmap(), but it is more complicated as the file
           size must be known */
        if (qemu_real_host_page_size < qemu_host_page_size) {
            abi_ulong end_addr, end_addr1;
            end_addr1 = (elf_bss + qemu_real_host_page_size - 1) &
                ~(qemu_real_host_page_size - 1);
            end_addr = HOST_PAGE_ALIGN(elf_bss);
            if (end_addr1 < end_addr) {
                mmap((void *)g2h(end_addr1), end_addr - end_addr1,
                     PROT_READ|PROT_WRITE|PROT_EXEC,
                     MAP_FIXED|MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
            }
        }

        nbyte = elf_bss & (qemu_host_page_size-1);
        if (nbyte) {
	    nbyte = qemu_host_page_size - nbyte;
	    do {
                /* FIXME - what to do if put_user() fails? */
		put_user_u8(0, elf_bss);
                elf_bss++;
	    } while (--nbyte);
        }
}


static abi_ulong create_elf_tables(abi_ulong p, int argc, int envc,
                                   struct elfhdr * exec,
                                   abi_ulong load_addr,
                                   abi_ulong load_bias,
                                   abi_ulong interp_load_addr, int ibcs,
                                   struct image_info *info)
{
        abi_ulong sp;
        int size;
        abi_ulong u_platform;
        const char *k_platform;
        const int n = sizeof(elf_addr_t);

        sp = p;
        u_platform = 0;
        k_platform = ELF_PLATFORM;
        if (k_platform) {
            size_t len = strlen(k_platform) + 1;
            sp -= (len + n - 1) & ~(n - 1);
            u_platform = sp;
            /* FIXME - check return value of memcpy_to_target() for failure */
            memcpy_to_target(sp, k_platform, len);
        }
	/*
	 * Force 16 byte _final_ alignment here for generality.
	 */
        sp = sp &~ (abi_ulong)15;
        size = (DLINFO_ITEMS + 1) * 2;
        if (k_platform)
          size += 2;
#ifdef DLINFO_ARCH_ITEMS
	size += DLINFO_ARCH_ITEMS * 2;
#endif
        size += envc + argc + 2;
	size += (!ibcs ? 3 : 1);	/* argc itself */
        size *= n;
        if (size & 15)
            sp -= 16 - (size & 15);

        /* This is correct because Linux defines
         * elf_addr_t as Elf32_Off / Elf64_Off
         */
#define NEW_AUX_ENT(id, val) do {		\
            sp -= n; put_user_ual(val, sp);	\
            sp -= n; put_user_ual(id, sp);	\
          } while(0)

        NEW_AUX_ENT (AT_NULL, 0);

        /* There must be exactly DLINFO_ITEMS entries here.  */
        NEW_AUX_ENT(AT_PHDR, (abi_ulong)(load_addr + exec->e_phoff));
        NEW_AUX_ENT(AT_PHENT, (abi_ulong)(sizeof (struct elf_phdr)));
        NEW_AUX_ENT(AT_PHNUM, (abi_ulong)(exec->e_phnum));
        NEW_AUX_ENT(AT_PAGESZ, (abi_ulong)(TARGET_PAGE_SIZE));
        NEW_AUX_ENT(AT_BASE, (abi_ulong)(interp_load_addr));
        NEW_AUX_ENT(AT_FLAGS, (abi_ulong)0);
        NEW_AUX_ENT(AT_ENTRY, load_bias + exec->e_entry);
        NEW_AUX_ENT(AT_UID, (abi_ulong) getuid());
        NEW_AUX_ENT(AT_EUID, (abi_ulong) geteuid());
        NEW_AUX_ENT(AT_GID, (abi_ulong) getgid());
        NEW_AUX_ENT(AT_EGID, (abi_ulong) getegid());
        NEW_AUX_ENT(AT_HWCAP, (abi_ulong) ELF_HWCAP);
        NEW_AUX_ENT(AT_CLKTCK, (abi_ulong) sysconf(_SC_CLK_TCK));
        if (k_platform)
            NEW_AUX_ENT(AT_PLATFORM, u_platform);
#ifdef ARCH_DLINFO
	/*
	 * ARCH_DLINFO must come last so platform specific code can enforce
	 * special alignment requirements on the AUXV if necessary (eg. PPC).
	 */
        ARCH_DLINFO;
#endif
#undef NEW_AUX_ENT

        info->saved_auxv = sp;

        sp = loader_build_argptr(envc, argc, sp, p, !ibcs);
        return sp;
}


static abi_ulong load_elf_interp(struct elfhdr * interp_elf_ex,
                                 int interpreter_fd,
                                 abi_ulong *interp_load_addr)
{
	struct elf_phdr *elf_phdata  =  NULL;
	struct elf_phdr *eppnt;
	abi_ulong load_addr = 0;
	int load_addr_set = 0;
	int retval;
	abi_ulong last_bss, elf_bss;
	abi_ulong error;
	int i;

	elf_bss = 0;
	last_bss = 0;
	error = 0;

#ifdef BSWAP_NEEDED
        bswap_ehdr(interp_elf_ex);
#endif
	/* First of all, some simple consistency checks */
	if ((interp_elf_ex->e_type != ET_EXEC &&
             interp_elf_ex->e_type != ET_DYN) ||
	   !elf_check_arch(interp_elf_ex->e_machine)) {
		return ~((abi_ulong)0UL);
	}


	/* Now read in all of the header information */

	if (sizeof(struct elf_phdr) * interp_elf_ex->e_phnum > TARGET_PAGE_SIZE)
	    return ~(abi_ulong)0UL;

	elf_phdata =  (struct elf_phdr *)
		malloc(sizeof(struct elf_phdr) * interp_elf_ex->e_phnum);

	if (!elf_phdata)
	  return ~((abi_ulong)0UL);

	/*
	 * If the size of this structure has changed, then punt, since
	 * we will be doing the wrong thing.
	 */
	if (interp_elf_ex->e_phentsize != sizeof(struct elf_phdr)) {
	    free(elf_phdata);
	    return ~((abi_ulong)0UL);
        }

	retval = lseek(interpreter_fd, interp_elf_ex->e_phoff, SEEK_SET);
	if(retval >= 0) {
	    retval = read(interpreter_fd,
			   (char *) elf_phdata,
			   sizeof(struct elf_phdr) * interp_elf_ex->e_phnum);
	}
	if (retval < 0) {
		perror("load_elf_interp");
		exit(-1);
		free (elf_phdata);
		return retval;
 	}
#ifdef BSWAP_NEEDED
	eppnt = elf_phdata;
	for (i=0; i<interp_elf_ex->e_phnum; i++, eppnt++) {
            bswap_phdr(eppnt);
        }
#endif

        if (interp_elf_ex->e_type == ET_DYN) {
            /* in order to avoid hardcoding the interpreter load
               address in qemu, we allocate a big enough memory zone */
            error = target_mmap(0, INTERP_MAP_SIZE,
                                PROT_NONE, MAP_PRIVATE | MAP_ANON,
                                -1, 0);
            if (error == -1) {
                perror("mmap");
                exit(-1);
            }
            load_addr = error;
            load_addr_set = 1;
        }

	eppnt = elf_phdata;
	for(i=0; i<interp_elf_ex->e_phnum; i++, eppnt++)
	  if (eppnt->p_type == PT_LOAD) {
	    int elf_type = MAP_PRIVATE | MAP_DENYWRITE;
	    int elf_prot = 0;
	    abi_ulong vaddr = 0;
	    abi_ulong k;

	    if (eppnt->p_flags & PF_R) elf_prot =  PROT_READ;
	    if (eppnt->p_flags & PF_W) elf_prot |= PROT_WRITE;
	    if (eppnt->p_flags & PF_X) elf_prot |= PROT_EXEC;
	    if (interp_elf_ex->e_type == ET_EXEC || load_addr_set) {
	    	elf_type |= MAP_FIXED;
	    	vaddr = eppnt->p_vaddr;
	    }
	    error = target_mmap(load_addr+TARGET_ELF_PAGESTART(vaddr),
		 eppnt->p_filesz + TARGET_ELF_PAGEOFFSET(eppnt->p_vaddr),
		 elf_prot,
		 elf_type,
		 interpreter_fd,
		 eppnt->p_offset - TARGET_ELF_PAGEOFFSET(eppnt->p_vaddr));

	    if (error == -1) {
	      /* Real error */
	      close(interpreter_fd);
	      free(elf_phdata);
	      return ~((abi_ulong)0UL);
	    }

	    if (!load_addr_set && interp_elf_ex->e_type == ET_DYN) {
	      load_addr = error;
	      load_addr_set = 1;
	    }

	    /*
	     * Find the end of the file  mapping for this phdr, and keep
	     * track of the largest address we see for this.
	     */
	    k = load_addr + eppnt->p_vaddr + eppnt->p_filesz;
	    if (k > elf_bss) elf_bss = k;

	    /*
	     * Do the same thing for the memory mapping - between
	     * elf_bss and last_bss is the bss section.
	     */
	    k = load_addr + eppnt->p_memsz + eppnt->p_vaddr;
	    if (k > last_bss) last_bss = k;
	  }

	/* Now use mmap to map the library into memory. */

	close(interpreter_fd);

	/*
	 * Now fill out the bss section.  First pad the last page up
	 * to the page boundary, and then perform a mmap to make sure
	 * that there are zeromapped pages up to and including the last
	 * bss page.
	 */
	padzero(elf_bss, last_bss);
	elf_bss = TARGET_ELF_PAGESTART(elf_bss + qemu_host_page_size - 1); /* What we have mapped so far */

	/* Map the last of the bss segment */
	if (last_bss > elf_bss) {
            target_mmap(elf_bss, last_bss-elf_bss,
                        PROT_READ|PROT_WRITE|PROT_EXEC,
                        MAP_FIXED|MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
	}
	free(elf_phdata);

	*interp_load_addr = load_addr;
	return ((abi_ulong) interp_elf_ex->e_entry) + load_addr;
}

static int symfind(const void *s0, const void *s1)
{
    struct elf_sym *key = (struct elf_sym *)s0;
    struct elf_sym *sym = (struct elf_sym *)s1;
    int result = 0;
    if (key->st_value < sym->st_value) {
        result = -1;
    } else if (key->st_value >= sym->st_value + sym->st_size) {
        result = 1;
    }
    return result;
}

static const char *lookup_symbolxx(struct syminfo *s, target_ulong orig_addr)
{
#if ELF_CLASS == ELFCLASS32
    struct elf_sym *syms = s->disas_symtab.elf32;
#else
    struct elf_sym *syms = s->disas_symtab.elf64;
#endif

    // binary search
    struct elf_sym key;
    struct elf_sym *sym;

    key.st_value = orig_addr;

    sym = bsearch(&key, syms, s->disas_num_syms, sizeof(*syms), symfind);
    if (sym != NULL) {
        return s->disas_strtab + sym->st_name;
    }

    return "";
}

/* FIXME: This should use elf_ops.h  */
static int symcmp(const void *s0, const void *s1)
{
    struct elf_sym *sym0 = (struct elf_sym *)s0;
    struct elf_sym *sym1 = (struct elf_sym *)s1;
    return (sym0->st_value < sym1->st_value)
        ? -1
        : ((sym0->st_value > sym1->st_value) ? 1 : 0);
}

/* Best attempt to load symbols from this ELF object. */
static void load_symbols(struct elfhdr *hdr, int fd)
{
    unsigned int i, nsyms;
    struct elf_shdr sechdr, symtab, strtab;
    char *strings;
    struct syminfo *s;
    struct elf_sym *syms;

    lseek(fd, hdr->e_shoff, SEEK_SET);
    for (i = 0; i < hdr->e_shnum; i++) {
        if (read(fd, &sechdr, sizeof(sechdr)) != sizeof(sechdr))
            return;
#ifdef BSWAP_NEEDED
        bswap_shdr(&sechdr);
#endif
        if (sechdr.sh_type == SHT_SYMTAB) {
            symtab = sechdr;
            lseek(fd, hdr->e_shoff
                  + sizeof(sechdr) * sechdr.sh_link, SEEK_SET);
            if (read(fd, &strtab, sizeof(strtab))
                != sizeof(strtab))
                return;
#ifdef BSWAP_NEEDED
            bswap_shdr(&strtab);
#endif
            goto found;
        }
    }
    return; /* Shouldn't happen... */

 found:
    /* Now know where the strtab and symtab are.  Snarf them. */
    s = malloc(sizeof(*s));
    syms = malloc(symtab.sh_size);
    if (!syms)
        return;
    s->disas_strtab = strings = malloc(strtab.sh_size);
    if (!s->disas_strtab)
        return;

    lseek(fd, symtab.sh_offset, SEEK_SET);
    if (read(fd, syms, symtab.sh_size) != symtab.sh_size)
        return;

    nsyms = symtab.sh_size / sizeof(struct elf_sym);

    i = 0;
    while (i < nsyms) {
#ifdef BSWAP_NEEDED
        bswap_sym(syms + i);
#endif
        // Throw away entries which we do not need.
        if (syms[i].st_shndx == SHN_UNDEF ||
                syms[i].st_shndx >= SHN_LORESERVE ||
                ELF_ST_TYPE(syms[i].st_info) != STT_FUNC) {
            nsyms--;
            if (i < nsyms) {
                syms[i] = syms[nsyms];
            }
            continue;
        }
#if defined(TARGET_ARM) || defined (TARGET_MIPS)
        /* The bottom address bit marks a Thumb or MIPS16 symbol.  */
        syms[i].st_value &= ~(target_ulong)1;
#endif
        i++;
    }
    syms = realloc(syms, nsyms * sizeof(*syms));

    qsort(syms, nsyms, sizeof(*syms), symcmp);

    lseek(fd, strtab.sh_offset, SEEK_SET);
    if (read(fd, strings, strtab.sh_size) != strtab.sh_size)
        return;
    s->disas_num_syms = nsyms;
#if ELF_CLASS == ELFCLASS32
    s->disas_symtab.elf32 = syms;
    s->lookup_symbol = lookup_symbolxx;
#else
    s->disas_symtab.elf64 = syms;
    s->lookup_symbol = lookup_symbolxx;
#endif
    s->next = syminfos;
    syminfos = s;
}

int load_elf_binary(struct linux_binprm * bprm, struct target_pt_regs * regs,
                    struct image_info * info)
{
    struct elfhdr elf_ex;
    struct elfhdr interp_elf_ex;
    struct exec interp_ex;
    int interpreter_fd = -1; /* avoid warning */
    abi_ulong load_addr, load_bias;
    int load_addr_set = 0;
    unsigned int interpreter_type = INTERPRETER_NONE;
    unsigned char ibcs2_interpreter;
    int i;
    abi_ulong mapped_addr;
    struct elf_phdr * elf_ppnt;
    struct elf_phdr *elf_phdata;
    abi_ulong elf_bss, k, elf_brk;
    int retval;
    char * elf_interpreter;
    abi_ulong elf_entry, interp_load_addr = 0;
    int status;
    abi_ulong start_code, end_code, start_data, end_data;
    abi_ulong reloc_func_desc = 0;
    abi_ulong elf_stack;
    char passed_fileno[6];

    ibcs2_interpreter = 0;
    status = 0;
    load_addr = 0;
    load_bias = 0;
    elf_ex = *((struct elfhdr *) bprm->buf);          /* exec-header */
#ifdef BSWAP_NEEDED
    bswap_ehdr(&elf_ex);
#endif

    /* First of all, some simple consistency checks */
    if ((elf_ex.e_type != ET_EXEC && elf_ex.e_type != ET_DYN) ||
       				(! elf_check_arch(elf_ex.e_machine))) {
	    return -ENOEXEC;
    }

    bprm->p = copy_elf_strings(1, &bprm->filename, bprm->page, bprm->p);
    bprm->p = copy_elf_strings(bprm->envc,bprm->envp,bprm->page,bprm->p);
    bprm->p = copy_elf_strings(bprm->argc,bprm->argv,bprm->page,bprm->p);
    if (!bprm->p) {
        retval = -E2BIG;
    }

    /* Now read in all of the header information */
    elf_phdata = (struct elf_phdr *)malloc(elf_ex.e_phentsize*elf_ex.e_phnum);
    if (elf_phdata == NULL) {
	return -ENOMEM;
    }

    retval = lseek(bprm->fd, elf_ex.e_phoff, SEEK_SET);
    if(retval > 0) {
	retval = read(bprm->fd, (char *) elf_phdata,
				elf_ex.e_phentsize * elf_ex.e_phnum);
    }

    if (retval < 0) {
	perror("load_elf_binary");
	exit(-1);
	free (elf_phdata);
	return -errno;
    }

#ifdef BSWAP_NEEDED
    elf_ppnt = elf_phdata;
    for (i=0; i<elf_ex.e_phnum; i++, elf_ppnt++) {
        bswap_phdr(elf_ppnt);
    }
#endif
    elf_ppnt = elf_phdata;

    elf_bss = 0;
    elf_brk = 0;


    elf_stack = ~((abi_ulong)0UL);
    elf_interpreter = NULL;
    start_code = ~((abi_ulong)0UL);
    end_code = 0;
    start_data = 0;
    end_data = 0;
    interp_ex.a_info = 0;

    for(i=0;i < elf_ex.e_phnum; i++) {
	if (elf_ppnt->p_type == PT_INTERP) {
	    if ( elf_interpreter != NULL )
	    {
		free (elf_phdata);
		free(elf_interpreter);
		close(bprm->fd);
		return -EINVAL;
	    }

	    /* This is the program interpreter used for
	     * shared libraries - for now assume that this
	     * is an a.out format binary
	     */

	    elf_interpreter = (char *)malloc(elf_ppnt->p_filesz);

	    if (elf_interpreter == NULL) {
		free (elf_phdata);
		close(bprm->fd);
		return -ENOMEM;
	    }

	    retval = lseek(bprm->fd, elf_ppnt->p_offset, SEEK_SET);
	    if(retval >= 0) {
		retval = read(bprm->fd, elf_interpreter, elf_ppnt->p_filesz);
	    }
	    if(retval < 0) {
	 	perror("load_elf_binary2");
		exit(-1);
	    }

	    /* If the program interpreter is one of these two,
	       then assume an iBCS2 image. Otherwise assume
	       a native linux image. */

	    /* JRP - Need to add X86 lib dir stuff here... */

	    if (strcmp(elf_interpreter,"/usr/lib/libc.so.1") == 0 ||
		strcmp(elf_interpreter,"/usr/lib/ld.so.1") == 0) {
	      ibcs2_interpreter = 1;
	    }

#if 0
	    printf("Using ELF interpreter %s\n", elf_interpreter);
#endif
	    if (retval >= 0) {
		retval = open(path(elf_interpreter), O_RDONLY);
		if(retval >= 0) {
		    interpreter_fd = retval;
		}
		else {
		    perror(elf_interpreter);
		    exit(-1);
		    /* retval = -errno; */
		}
	    }

	    if (retval >= 0) {
		retval = lseek(interpreter_fd, 0, SEEK_SET);
		if(retval >= 0) {
		    retval = read(interpreter_fd,bprm->buf,128);
		}
	    }
	    if (retval >= 0) {
		interp_ex = *((struct exec *) bprm->buf); /* aout exec-header */
		interp_elf_ex=*((struct elfhdr *) bprm->buf); /* elf exec-header */
	    }
	    if (retval < 0) {
		perror("load_elf_binary3");
		exit(-1);
		free (elf_phdata);
		free(elf_interpreter);
		close(bprm->fd);
		return retval;
	    }
	}
	elf_ppnt++;
    }

    /* Some simple consistency checks for the interpreter */
    if (elf_interpreter){
	interpreter_type = INTERPRETER_ELF | INTERPRETER_AOUT;

	/* Now figure out which format our binary is */
	if ((N_MAGIC(interp_ex) != OMAGIC) && (N_MAGIC(interp_ex) != ZMAGIC) &&
	    	(N_MAGIC(interp_ex) != QMAGIC)) {
	  interpreter_type = INTERPRETER_ELF;
	}

	if (interp_elf_ex.e_ident[0] != 0x7f ||
            strncmp((char *)&interp_elf_ex.e_ident[1], "ELF",3) != 0) {
	    interpreter_type &= ~INTERPRETER_ELF;
	}

	if (!interpreter_type) {
	    free(elf_interpreter);
	    free(elf_phdata);
	    close(bprm->fd);
	    return -ELIBBAD;
	}
    }

    /* OK, we are done with that, now set up the arg stuff,
       and then start this sucker up */

    {
	char * passed_p;

	if (interpreter_type == INTERPRETER_AOUT) {
	    snprintf(passed_fileno, sizeof(passed_fileno), "%d", bprm->fd);
	    passed_p = passed_fileno;

	    if (elf_interpreter) {
		bprm->p = copy_elf_strings(1,&passed_p,bprm->page,bprm->p);
		bprm->argc++;
	    }
	}
	if (!bprm->p) {
	    if (elf_interpreter) {
	        free(elf_interpreter);
	    }
	    free (elf_phdata);
	    close(bprm->fd);
	    return -E2BIG;
	}
    }

    /* OK, This is the point of no return */
    info->end_data = 0;
    info->end_code = 0;
    info->start_mmap = (abi_ulong)ELF_START_MMAP;
    info->mmap = 0;
    elf_entry = (abi_ulong) elf_ex.e_entry;

    /* Do this so that we can load the interpreter, if need be.  We will
       change some of these later */
    info->rss = 0;
    bprm->p = setup_arg_pages(bprm->p, bprm, info);
    info->start_stack = bprm->p;

    /* Now we do a little grungy work by mmaping the ELF image into
     * the correct location in memory.  At this point, we assume that
     * the image should be loaded at fixed address, not at a variable
     * address.
     */

    for(i = 0, elf_ppnt = elf_phdata; i < elf_ex.e_phnum; i++, elf_ppnt++) {
        int elf_prot = 0;
        int elf_flags = 0;
        abi_ulong error;

	if (elf_ppnt->p_type != PT_LOAD)
            continue;

        if (elf_ppnt->p_flags & PF_R) elf_prot |= PROT_READ;
        if (elf_ppnt->p_flags & PF_W) elf_prot |= PROT_WRITE;
        if (elf_ppnt->p_flags & PF_X) elf_prot |= PROT_EXEC;
        elf_flags = MAP_PRIVATE | MAP_DENYWRITE;
        if (elf_ex.e_type == ET_EXEC || load_addr_set) {
            elf_flags |= MAP_FIXED;
        } else if (elf_ex.e_type == ET_DYN) {
            /* Try and get dynamic programs out of the way of the default mmap
               base, as well as whatever program they might try to exec.  This
               is because the brk will follow the loader, and is not movable.  */
            /* NOTE: for qemu, we do a big mmap to get enough space
               without hardcoding any address */
            error = target_mmap(0, ET_DYN_MAP_SIZE,
                                PROT_NONE, MAP_PRIVATE | MAP_ANON,
                                -1, 0);
            if (error == -1) {
                perror("mmap");
                exit(-1);
            }
            load_bias = TARGET_ELF_PAGESTART(error - elf_ppnt->p_vaddr);
        }

        error = target_mmap(TARGET_ELF_PAGESTART(load_bias + elf_ppnt->p_vaddr),
                            (elf_ppnt->p_filesz +
                             TARGET_ELF_PAGEOFFSET(elf_ppnt->p_vaddr)),
                            elf_prot,
                            (MAP_FIXED | MAP_PRIVATE | MAP_DENYWRITE),
                            bprm->fd,
                            (elf_ppnt->p_offset -
                             TARGET_ELF_PAGEOFFSET(elf_ppnt->p_vaddr)));
        if (error == -1) {
            perror("mmap");
            exit(-1);
        }

#ifdef LOW_ELF_STACK
        if (TARGET_ELF_PAGESTART(elf_ppnt->p_vaddr) < elf_stack)
            elf_stack = TARGET_ELF_PAGESTART(elf_ppnt->p_vaddr);
#endif

        if (!load_addr_set) {
            load_addr_set = 1;
            load_addr = elf_ppnt->p_vaddr - elf_ppnt->p_offset;
            if (elf_ex.e_type == ET_DYN) {
                load_bias += error -
                    TARGET_ELF_PAGESTART(load_bias + elf_ppnt->p_vaddr);
                load_addr += load_bias;
                reloc_func_desc = load_bias;
            }
        }
        k = elf_ppnt->p_vaddr;
        if (k < start_code)
            start_code = k;
        if (start_data < k)
            start_data = k;
        k = elf_ppnt->p_vaddr + elf_ppnt->p_filesz;
        if (k > elf_bss)
            elf_bss = k;
        if ((elf_ppnt->p_flags & PF_X) && end_code <  k)
            end_code = k;
        if (end_data < k)
            end_data = k;
        k = elf_ppnt->p_vaddr + elf_ppnt->p_memsz;
        if (k > elf_brk) elf_brk = k;
    }

    elf_entry += load_bias;
    elf_bss += load_bias;
    elf_brk += load_bias;
    start_code += load_bias;
    end_code += load_bias;
    start_data += load_bias;
    end_data += load_bias;

    if (elf_interpreter) {
	if (interpreter_type & 1) {
	    elf_entry = load_aout_interp(&interp_ex, interpreter_fd);
	}
	else if (interpreter_type & 2) {
	    elf_entry = load_elf_interp(&interp_elf_ex, interpreter_fd,
					    &interp_load_addr);
	}
        reloc_func_desc = interp_load_addr;

	close(interpreter_fd);
	free(elf_interpreter);

	if (elf_entry == ~((abi_ulong)0UL)) {
	    printf("Unable to load interpreter\n");
	    free(elf_phdata);
	    exit(-1);
	    return 0;
	}
    }

    free(elf_phdata);

    if (qemu_log_enabled())
	load_symbols(&elf_ex, bprm->fd);

    if (interpreter_type != INTERPRETER_AOUT) close(bprm->fd);
    info->personality = (ibcs2_interpreter ? PER_SVR4 : PER_LINUX);

#ifdef LOW_ELF_STACK
    info->start_stack = bprm->p = elf_stack - 4;
#endif
    bprm->p = create_elf_tables(bprm->p,
		    bprm->argc,
		    bprm->envc,
                    &elf_ex,
                    load_addr, load_bias,
		    interp_load_addr,
		    (interpreter_type == INTERPRETER_AOUT ? 0 : 1),
		    info);
    info->load_addr = reloc_func_desc;
    info->start_brk = info->brk = elf_brk;
    info->end_code = end_code;
    info->start_code = start_code;
    info->start_data = start_data;
    info->end_data = end_data;
    info->start_stack = bprm->p;

    /* Calling set_brk effectively mmaps the pages that we need for the bss and break
       sections */
    set_brk(elf_bss, elf_brk);

    padzero(elf_bss, elf_brk);

#if 0
    printf("(start_brk) %x\n" , info->start_brk);
    printf("(end_code) %x\n" , info->end_code);
    printf("(start_code) %x\n" , info->start_code);
    printf("(end_data) %x\n" , info->end_data);
    printf("(start_stack) %x\n" , info->start_stack);
    printf("(brk) %x\n" , info->brk);
#endif

    if ( info->personality == PER_SVR4 )
    {
	    /* Why this, you ask???  Well SVr4 maps page 0 as read-only,
	       and some applications "depend" upon this behavior.
	       Since we do not have the power to recompile these, we
	       emulate the SVr4 behavior.  Sigh.  */
	    mapped_addr = target_mmap(0, qemu_host_page_size, PROT_READ | PROT_EXEC,
                                      MAP_FIXED | MAP_PRIVATE, -1, 0);
    }

    info->entry = elf_entry;

#ifdef USE_ELF_CORE_DUMP
    bprm->core_dump = &elf_core_dump;
#endif

    return 0;
}

#ifdef USE_ELF_CORE_DUMP

/*
 * Definitions to generate Intel SVR4-like core files.
 * These mostly have the same names as the SVR4 types with "target_elf_"
 * tacked on the front to prevent clashes with linux definitions,
 * and the typedef forms have been avoided.  This is mostly like
 * the SVR4 structure, but more Linuxy, with things that Linux does
 * not support and which gdb doesn't really use excluded.
 *
 * Fields we don't dump (their contents is zero) in linux-user qemu
 * are marked with XXX.
 *
 * Core dump code is copied from linux kernel (fs/binfmt_elf.c).
 *
 * Porting ELF coredump for target is (quite) simple process.  First you
 * define ELF_USE_CORE_DUMP in target ELF code (where init_thread() for
 * the target resides):
 *
 * #define USE_ELF_CORE_DUMP
 *
 * Next you define type of register set used for dumping.  ELF specification
 * says that it needs to be array of elf_greg_t that has size of ELF_NREG.
 *
 * typedef <target_regtype> target_elf_greg_t;
 * #define ELF_NREG <number of registers>
 * typedef taret_elf_greg_t target_elf_gregset_t[ELF_NREG];
 *
 * Then define following types to match target types.  Actual types can
 * be found from linux kernel (arch/<ARCH>/include/asm/posix_types.h):
 *
 * typedef <target_uid_type> target_uid_t;
 * typedef <target_gid_type> target_gid_t;
 * typedef <target_pid_type> target_pid_t;
 *
 * Last step is to implement target specific function that copies registers
 * from given cpu into just specified register set.  Prototype is:
 *
 * static void elf_core_copy_regs(taret_elf_gregset_t *regs,
 *                                const CPUState *env);
 *
 * Parameters:
 *     regs - copy register values into here (allocated and zeroed by caller)
 *     env - copy registers from here
 *
 * Example for ARM target is provided in this file.
 */

/* An ELF note in memory */
struct memelfnote {
    const char *name;
    size_t     namesz;
    size_t     namesz_rounded;
    int        type;
    size_t     datasz;
    void       *data;
    size_t     notesz;
};

struct target_elf_siginfo {
    int  si_signo; /* signal number */
    int  si_code;  /* extra code */
    int  si_errno; /* errno */
};

struct target_elf_prstatus {
    struct target_elf_siginfo pr_info;      /* Info associated with signal */
    short              pr_cursig;    /* Current signal */
    target_ulong       pr_sigpend;   /* XXX */
    target_ulong       pr_sighold;   /* XXX */
    target_pid_t       pr_pid;
    target_pid_t       pr_ppid;
    target_pid_t       pr_pgrp;
    target_pid_t       pr_sid;
    struct target_timeval pr_utime;  /* XXX User time */
    struct target_timeval pr_stime;  /* XXX System time */
    struct target_timeval pr_cutime; /* XXX Cumulative user time */
    struct target_timeval pr_cstime; /* XXX Cumulative system time */
    target_elf_gregset_t      pr_reg;       /* GP registers */
    int                pr_fpvalid;   /* XXX */
};

#define ELF_PRARGSZ     (80) /* Number of chars for args */

struct target_elf_prpsinfo {
    char         pr_state;       /* numeric process state */
    char         pr_sname;       /* char for pr_state */
    char         pr_zomb;        /* zombie */
    char         pr_nice;        /* nice val */
    target_ulong pr_flag;        /* flags */
    target_uid_t pr_uid;
    target_gid_t pr_gid;
    target_pid_t pr_pid, pr_ppid, pr_pgrp, pr_sid;
    /* Lots missing */
    char    pr_fname[16];           /* filename of executable */
    char    pr_psargs[ELF_PRARGSZ]; /* initial part of arg list */
};

/* Here is the structure in which status of each thread is captured. */
struct elf_thread_status {
    TAILQ_ENTRY(elf_thread_status)  ets_link;
    struct target_elf_prstatus prstatus;   /* NT_PRSTATUS */
#if 0
    elf_fpregset_t fpu;             /* NT_PRFPREG */
    struct task_struct *thread;
    elf_fpxregset_t xfpu;           /* ELF_CORE_XFPREG_TYPE */
#endif
    struct memelfnote notes[1];
    int num_notes;
};

struct elf_note_info {
    struct memelfnote   *notes;
    struct target_elf_prstatus *prstatus;  /* NT_PRSTATUS */
    struct target_elf_prpsinfo *psinfo;    /* NT_PRPSINFO */

    TAILQ_HEAD(thread_list_head, elf_thread_status) thread_list;
#if 0
    /*
     * Current version of ELF coredump doesn't support
     * dumping fp regs etc.
     */
    elf_fpregset_t *fpu;
    elf_fpxregset_t *xfpu;
    int thread_status_size;
#endif
    int notes_size;
    int numnote;
};

struct vm_area_struct {
    abi_ulong   vma_start;  /* start vaddr of memory region */
    abi_ulong   vma_end;    /* end vaddr of memory region */
    abi_ulong   vma_flags;  /* protection etc. flags for the region */
    TAILQ_ENTRY(vm_area_struct) vma_link;
};

struct mm_struct {
    TAILQ_HEAD(, vm_area_struct) mm_mmap;
    int mm_count;           /* number of mappings */
};

static struct mm_struct *vma_init(void);
static void vma_delete(struct mm_struct *);
static int vma_add_mapping(struct mm_struct *, abi_ulong,
    abi_ulong, abi_ulong);
static int vma_get_mapping_count(const struct mm_struct *);
static struct vm_area_struct *vma_first(const struct mm_struct *);
static struct vm_area_struct *vma_next(struct vm_area_struct *);
static abi_ulong vma_dump_size(const struct vm_area_struct *);
static int vma_walker(void *priv, unsigned long start, unsigned long end,
    unsigned long flags);

static void fill_elf_header(struct elfhdr *, int, uint16_t, uint32_t);
static void fill_note(struct memelfnote *, const char *, int,
    unsigned int, void *);
static void fill_prstatus(struct target_elf_prstatus *, const TaskState *, int);
static int fill_psinfo(struct target_elf_prpsinfo *, const TaskState *);
static void fill_auxv_note(struct memelfnote *, const TaskState *);
static void fill_elf_note_phdr(struct elf_phdr *, int, off_t);
static size_t note_size(const struct memelfnote *);
static void free_note_info(struct elf_note_info *);
static int fill_note_info(struct elf_note_info *, long, const CPUState *);
static void fill_thread_info(struct elf_note_info *, const CPUState *);
static int core_dump_filename(const TaskState *, char *, size_t);

static int dump_write(int, const void *, size_t);
static int write_note(struct memelfnote *, int);
static int write_note_info(struct elf_note_info *, int);

#ifdef BSWAP_NEEDED
static void bswap_prstatus(struct target_elf_prstatus *);
static void bswap_psinfo(struct target_elf_prpsinfo *);

static void bswap_prstatus(struct target_elf_prstatus *prstatus)
{
    prstatus->pr_info.si_signo = tswapl(prstatus->pr_info.si_signo);
    prstatus->pr_info.si_code = tswapl(prstatus->pr_info.si_code);
    prstatus->pr_info.si_errno = tswapl(prstatus->pr_info.si_errno);
    prstatus->pr_cursig = tswap16(prstatus->pr_cursig);
    prstatus->pr_sigpend = tswapl(prstatus->pr_sigpend);
    prstatus->pr_sighold = tswapl(prstatus->pr_sighold);
    prstatus->pr_pid = tswap32(prstatus->pr_pid);
    prstatus->pr_ppid = tswap32(prstatus->pr_ppid);
    prstatus->pr_pgrp = tswap32(prstatus->pr_pgrp);
    prstatus->pr_sid = tswap32(prstatus->pr_sid);
    /* cpu times are not filled, so we skip them */
    /* regs should be in correct format already */
    prstatus->pr_fpvalid = tswap32(prstatus->pr_fpvalid);
}

static void bswap_psinfo(struct target_elf_prpsinfo *psinfo)
{
    psinfo->pr_flag = tswapl(psinfo->pr_flag);
    psinfo->pr_uid = tswap16(psinfo->pr_uid);
    psinfo->pr_gid = tswap16(psinfo->pr_gid);
    psinfo->pr_pid = tswap32(psinfo->pr_pid);
    psinfo->pr_ppid = tswap32(psinfo->pr_ppid);
    psinfo->pr_pgrp = tswap32(psinfo->pr_pgrp);
    psinfo->pr_sid = tswap32(psinfo->pr_sid);
}
#endif /* BSWAP_NEEDED */

/*
 * Minimal support for linux memory regions.  These are needed
 * when we are finding out what memory exactly belongs to
 * emulated process.  No locks needed here, as long as
 * thread that received the signal is stopped.
 */

static struct mm_struct *vma_init(void)
{
    struct mm_struct *mm;

    if ((mm = qemu_malloc(sizeof (*mm))) == NULL)
        return (NULL);

    mm->mm_count = 0;
    TAILQ_INIT(&mm->mm_mmap);

    return (mm);
}

static void vma_delete(struct mm_struct *mm)
{
    struct vm_area_struct *vma;

    while ((vma = vma_first(mm)) != NULL) {
        TAILQ_REMOVE(&mm->mm_mmap, vma, vma_link);
        qemu_free(vma);
    }
    qemu_free(mm);
}

static int vma_add_mapping(struct mm_struct *mm, abi_ulong start,
    abi_ulong end, abi_ulong flags)
{
    struct vm_area_struct *vma;

    if ((vma = qemu_mallocz(sizeof (*vma))) == NULL)
        return (-1);

    vma->vma_start = start;
    vma->vma_end = end;
    vma->vma_flags = flags;

    TAILQ_INSERT_TAIL(&mm->mm_mmap, vma, vma_link);
    mm->mm_count++;

    return (0);
}

static struct vm_area_struct *vma_first(const struct mm_struct *mm)
{
    return (TAILQ_FIRST(&mm->mm_mmap));
}

static struct vm_area_struct *vma_next(struct vm_area_struct *vma)
{
    return (TAILQ_NEXT(vma, vma_link));
}

static int vma_get_mapping_count(const struct mm_struct *mm)
{
    return (mm->mm_count);
}

/*
 * Calculate file (dump) size of given memory region.
 */
static abi_ulong vma_dump_size(const struct vm_area_struct *vma)
{
    /* if we cannot even read the first page, skip it */
    if (!access_ok(VERIFY_READ, vma->vma_start, TARGET_PAGE_SIZE))
        return (0);

    /*
     * Usually we don't dump executable pages as they contain
     * non-writable code that debugger can read directly from
     * target library etc.  However, thread stacks are marked
     * also executable so we read in first page of given region
     * and check whether it contains elf header.  If there is
     * no elf header, we dump it.
     */
    if (vma->vma_flags & PROT_EXEC) {
        char page[TARGET_PAGE_SIZE];

        copy_from_user(page, vma->vma_start, sizeof (page));
        if ((page[EI_MAG0] == ELFMAG0) &&
            (page[EI_MAG1] == ELFMAG1) &&
            (page[EI_MAG2] == ELFMAG2) &&
            (page[EI_MAG3] == ELFMAG3)) {
            /*
             * Mappings are possibly from ELF binary.  Don't dump
             * them.
             */
            return (0);
        }
    }

    return (vma->vma_end - vma->vma_start);
}

static int vma_walker(void *priv, unsigned long start, unsigned long end,
    unsigned long flags)
{
    struct mm_struct *mm = (struct mm_struct *)priv;

    /*
     * Don't dump anything that qemu has reserved for internal use.
     */
    if (flags & PAGE_RESERVED)
        return (0);

    vma_add_mapping(mm, start, end, flags);
    return (0);
}

static void fill_note(struct memelfnote *note, const char *name, int type,
    unsigned int sz, void *data)
{
    unsigned int namesz;

    namesz = strlen(name) + 1;
    note->name = name;
    note->namesz = namesz;
    note->namesz_rounded = roundup(namesz, sizeof (int32_t));
    note->type = type;
    note->datasz = roundup(sz, sizeof (int32_t));;
    note->data = data;

    /*
     * We calculate rounded up note size here as specified by
     * ELF document.
     */
    note->notesz = sizeof (struct elf_note) +
        note->namesz_rounded + note->datasz;
}

static void fill_elf_header(struct elfhdr *elf, int segs, uint16_t machine,
    uint32_t flags)
{
    (void) memset(elf, 0, sizeof(*elf));

    (void) memcpy(elf->e_ident, ELFMAG, SELFMAG);
    elf->e_ident[EI_CLASS] = ELF_CLASS;
    elf->e_ident[EI_DATA] = ELF_DATA;
    elf->e_ident[EI_VERSION] = EV_CURRENT;
    elf->e_ident[EI_OSABI] = ELF_OSABI;

    elf->e_type = ET_CORE;
    elf->e_machine = machine;
    elf->e_version = EV_CURRENT;
    elf->e_phoff = sizeof(struct elfhdr);
    elf->e_flags = flags;
    elf->e_ehsize = sizeof(struct elfhdr);
    elf->e_phentsize = sizeof(struct elf_phdr);
    elf->e_phnum = segs;

#ifdef BSWAP_NEEDED
    bswap_ehdr(elf);
#endif
}

static void fill_elf_note_phdr(struct elf_phdr *phdr, int sz, off_t offset)
{
    phdr->p_type = PT_NOTE;
    phdr->p_offset = offset;
    phdr->p_vaddr = 0;
    phdr->p_paddr = 0;
    phdr->p_filesz = sz;
    phdr->p_memsz = 0;
    phdr->p_flags = 0;
    phdr->p_align = 0;

#ifdef BSWAP_NEEDED
    bswap_phdr(phdr);
#endif
}

static size_t note_size(const struct memelfnote *note)
{
    return (note->notesz);
}

static void fill_prstatus(struct target_elf_prstatus *prstatus,
    const TaskState *ts, int signr)
{
    (void) memset(prstatus, 0, sizeof (*prstatus));
    prstatus->pr_info.si_signo = prstatus->pr_cursig = signr;
    prstatus->pr_pid = ts->ts_tid;
    prstatus->pr_ppid = getppid();
    prstatus->pr_pgrp = getpgrp();
    prstatus->pr_sid = getsid(0);

#ifdef BSWAP_NEEDED
    bswap_prstatus(prstatus);
#endif
}

static int fill_psinfo(struct target_elf_prpsinfo *psinfo, const TaskState *ts)
{
    char *filename, *base_filename;
    unsigned int i, len;

    (void) memset(psinfo, 0, sizeof (*psinfo));

    len = ts->info->arg_end - ts->info->arg_start;
    if (len >= ELF_PRARGSZ)
        len = ELF_PRARGSZ - 1;
    if (copy_from_user(&psinfo->pr_psargs, ts->info->arg_start, len))
        return -EFAULT;
    for (i = 0; i < len; i++)
        if (psinfo->pr_psargs[i] == 0)
            psinfo->pr_psargs[i] = ' ';
    psinfo->pr_psargs[len] = 0;

    psinfo->pr_pid = getpid();
    psinfo->pr_ppid = getppid();
    psinfo->pr_pgrp = getpgrp();
    psinfo->pr_sid = getsid(0);
    psinfo->pr_uid = getuid();
    psinfo->pr_gid = getgid();

    filename = strdup(ts->bprm->filename);
    base_filename = strdup(basename(filename));
    (void) strncpy(psinfo->pr_fname, base_filename,
        sizeof(psinfo->pr_fname));
    free(base_filename);
    free(filename);

#ifdef BSWAP_NEEDED
    bswap_psinfo(psinfo);
#endif
    return (0);
}

static void fill_auxv_note(struct memelfnote *note, const TaskState *ts)
{
    elf_addr_t auxv = (elf_addr_t)ts->info->saved_auxv;
    elf_addr_t orig_auxv = auxv;
    abi_ulong val;
    void *ptr;
    int i, len;

    /*
     * Auxiliary vector is stored in target process stack.  It contains
     * {type, value} pairs that we need to dump into note.  This is not
     * strictly necessary but we do it here for sake of completeness.
     */

    /* find out lenght of the vector, AT_NULL is terminator */
    i = len = 0;
    do {
        get_user_ual(val, auxv);
        i += 2;
        auxv += 2 * sizeof (elf_addr_t);
    } while (val != AT_NULL);
    len = i * sizeof (elf_addr_t);

    /* read in whole auxv vector and copy it to memelfnote */
    ptr = lock_user(VERIFY_READ, orig_auxv, len, 0);
    if (ptr != NULL) {
        fill_note(note, "CORE", NT_AUXV, len, ptr);
        unlock_user(ptr, auxv, len);
    }
}

/*
 * Constructs name of coredump file.  We have following convention
 * for the name:
 *     qemu_<basename-of-target-binary>_<date>-<time>_<pid>.core
 *
 * Returns 0 in case of success, -1 otherwise (errno is set).
 */
static int core_dump_filename(const TaskState *ts, char *buf,
    size_t bufsize)
{
    char timestamp[64];
    char *filename = NULL;
    char *base_filename = NULL;
    struct timeval tv;
    struct tm tm;

    assert(bufsize >= PATH_MAX);

    if (gettimeofday(&tv, NULL) < 0) {
        (void) fprintf(stderr, "unable to get current timestamp: %s",
            strerror(errno));
        return (-1);
    }

    filename = strdup(ts->bprm->filename);
    base_filename = strdup(basename(filename));
    (void) strftime(timestamp, sizeof (timestamp), "%Y%m%d-%H%M%S",
        localtime_r(&tv.tv_sec, &tm));
    (void) snprintf(buf, bufsize, "qemu_%s_%s_%d.core",
        base_filename, timestamp, (int)getpid());
    free(base_filename);
    free(filename);

    return (0);
}

static int dump_write(int fd, const void *ptr, size_t size)
{
    const char *bufp = (const char *)ptr;
    ssize_t bytes_written, bytes_left;
    struct rlimit dumpsize;
    off_t pos;

    bytes_written = 0;
    getrlimit(RLIMIT_CORE, &dumpsize);
    if ((pos = lseek(fd, 0, SEEK_CUR))==-1) {
        if (errno == ESPIPE) { /* not a seekable stream */
            bytes_left = size;
        } else {
            return pos;
        }
    } else {
        if (dumpsize.rlim_cur <= pos) {
            return -1;
        } else if (dumpsize.rlim_cur == RLIM_INFINITY) {
            bytes_left = size;
        } else {
            size_t limit_left=dumpsize.rlim_cur - pos;
            bytes_left = limit_left >= size ? size : limit_left ;
        }
    }

    /*
     * In normal conditions, single write(2) should do but
     * in case of socket etc. this mechanism is more portable.
     */
    do {
        bytes_written = write(fd, bufp, bytes_left);
        if (bytes_written < 0) {
            if (errno == EINTR)
                continue;
            return (-1);
        } else if (bytes_written == 0) { /* eof */
            return (-1);
        }
        bufp += bytes_written;
        bytes_left -= bytes_written;
    } while (bytes_left > 0);

    return (0);
}

static int write_note(struct memelfnote *men, int fd)
{
    struct elf_note en;

    en.n_namesz = men->namesz;
    en.n_type = men->type;
    en.n_descsz = men->datasz;

#ifdef BSWAP_NEEDED
    bswap_note(&en);
#endif

    if (dump_write(fd, &en, sizeof(en)) != 0)
        return (-1);
    if (dump_write(fd, men->name, men->namesz_rounded) != 0)
        return (-1);
    if (dump_write(fd, men->data, men->datasz) != 0)
        return (-1);

    return (0);
}

static void fill_thread_info(struct elf_note_info *info, const CPUState *env)
{
    TaskState *ts = (TaskState *)env->opaque;
    struct elf_thread_status *ets;

    ets = qemu_mallocz(sizeof (*ets));
    ets->num_notes = 1; /* only prstatus is dumped */
    fill_prstatus(&ets->prstatus, ts, 0);
    elf_core_copy_regs(&ets->prstatus.pr_reg, env);
    fill_note(&ets->notes[0], "CORE", NT_PRSTATUS, sizeof (ets->prstatus),
        &ets->prstatus);

    TAILQ_INSERT_TAIL(&info->thread_list, ets, ets_link);

    info->notes_size += note_size(&ets->notes[0]);
}

static int fill_note_info(struct elf_note_info *info,
    long signr, const CPUState *env)
{
#define NUMNOTES 3
    CPUState *cpu = NULL;
    TaskState *ts = (TaskState *)env->opaque;
    int i;

    (void) memset(info, 0, sizeof (*info));

    TAILQ_INIT(&info->thread_list);

    info->notes = qemu_mallocz(NUMNOTES * sizeof (struct memelfnote));
    if (info->notes == NULL)
        return (-ENOMEM);
    info->prstatus = qemu_mallocz(sizeof (*info->prstatus));
    if (info->prstatus == NULL)
        return (-ENOMEM);
    info->psinfo = qemu_mallocz(sizeof (*info->psinfo));
    if (info->prstatus == NULL)
        return (-ENOMEM);

    /*
     * First fill in status (and registers) of current thread
     * including process info & aux vector.
     */
    fill_prstatus(info->prstatus, ts, signr);
    elf_core_copy_regs(&info->prstatus->pr_reg, env);
    fill_note(&info->notes[0], "CORE", NT_PRSTATUS,
        sizeof (*info->prstatus), info->prstatus);
    fill_psinfo(info->psinfo, ts);
    fill_note(&info->notes[1], "CORE", NT_PRPSINFO,
        sizeof (*info->psinfo), info->psinfo);
    fill_auxv_note(&info->notes[2], ts);
    info->numnote = 3;

    info->notes_size = 0;
    for (i = 0; i < info->numnote; i++)
        info->notes_size += note_size(&info->notes[i]);

    /* read and fill status of all threads */
    cpu_list_lock();
    for (cpu = first_cpu; cpu != NULL; cpu = cpu->next_cpu) {
        if (cpu == thread_env)
            continue;
        fill_thread_info(info, cpu);
    }
    cpu_list_unlock();

    return (0);
}

static void free_note_info(struct elf_note_info *info)
{
    struct elf_thread_status *ets;

    while (!TAILQ_EMPTY(&info->thread_list)) {
        ets = TAILQ_FIRST(&info->thread_list);
        TAILQ_REMOVE(&info->thread_list, ets, ets_link);
        qemu_free(ets);
    }

    qemu_free(info->prstatus);
    qemu_free(info->psinfo);
    qemu_free(info->notes);
}

static int write_note_info(struct elf_note_info *info, int fd)
{
    struct elf_thread_status *ets;
    int i, error = 0;

    /* write prstatus, psinfo and auxv for current thread */
    for (i = 0; i < info->numnote; i++)
        if ((error = write_note(&info->notes[i], fd)) != 0)
            return (error);

    /* write prstatus for each thread */
    for (ets = info->thread_list.tqh_first; ets != NULL;
        ets = ets->ets_link.tqe_next) {
        if ((error = write_note(&ets->notes[0], fd)) != 0)
            return (error);
    }

    return (0);
}

/*
 * Write out ELF coredump.
 *
 * See documentation of ELF object file format in:
 * http://www.caldera.com/developers/devspecs/gabi41.pdf
 *
 * Coredump format in linux is following:
 *
 * 0   +----------------------+         \
 *     | ELF header           | ET_CORE  |
 *     +----------------------+          |
 *     | ELF program headers  |          |--- headers
 *     | - NOTE section       |          |
 *     | - PT_LOAD sections   |          |
 *     +----------------------+         /
 *     | NOTEs:               |
 *     | - NT_PRSTATUS        |
 *     | - NT_PRSINFO         |
 *     | - NT_AUXV            |
 *     +----------------------+ <-- aligned to target page
 *     | Process memory dump  |
 *     :                      :
 *     .                      .
 *     :                      :
 *     |                      |
 *     +----------------------+
 *
 * NT_PRSTATUS -> struct elf_prstatus (per thread)
 * NT_PRSINFO  -> struct elf_prpsinfo
 * NT_AUXV is array of { type, value } pairs (see fill_auxv_note()).
 *
 * Format follows System V format as close as possible.  Current
 * version limitations are as follows:
 *     - no floating point registers are dumped
 *
 * Function returns 0 in case of success, negative errno otherwise.
 *
 * TODO: make this work also during runtime: it should be
 * possible to force coredump from running process and then
 * continue processing.  For example qemu could set up SIGUSR2
 * handler (provided that target process haven't registered
 * handler for that) that does the dump when signal is received.
 */
static int elf_core_dump(int signr, const CPUState *env)
{
    const TaskState *ts = (const TaskState *)env->opaque;
    struct vm_area_struct *vma = NULL;
    char corefile[PATH_MAX];
    struct elf_note_info info;
    struct elfhdr elf;
    struct elf_phdr phdr;
    struct rlimit dumpsize;
    struct mm_struct *mm = NULL;
    off_t offset = 0, data_offset = 0;
    int segs = 0;
    int fd = -1;

    errno = 0;
    getrlimit(RLIMIT_CORE, &dumpsize);
    if (dumpsize.rlim_cur == 0)
       return 0;

    if (core_dump_filename(ts, corefile, sizeof (corefile)) < 0)
        return (-errno);

    if ((fd = open(corefile, O_WRONLY | O_CREAT,
        S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH)) < 0)
        return (-errno);

    /*
     * Walk through target process memory mappings and
     * set up structure containing this information.  After
     * this point vma_xxx functions can be used.
     */
    if ((mm = vma_init()) == NULL)
        goto out;

    walk_memory_regions(mm, vma_walker);
    segs = vma_get_mapping_count(mm);

    /*
     * Construct valid coredump ELF header.  We also
     * add one more segment for notes.
     */
    fill_elf_header(&elf, segs + 1, ELF_MACHINE, 0);
    if (dump_write(fd, &elf, sizeof (elf)) != 0)
        goto out;

    /* fill in in-memory version of notes */
    if (fill_note_info(&info, signr, env) < 0)
        goto out;

    offset += sizeof (elf);                             /* elf header */
    offset += (segs + 1) * sizeof (struct elf_phdr);    /* program headers */

    /* write out notes program header */
    fill_elf_note_phdr(&phdr, info.notes_size, offset);

    offset += info.notes_size;
    if (dump_write(fd, &phdr, sizeof (phdr)) != 0)
        goto out;

    /*
     * ELF specification wants data to start at page boundary so
     * we align it here.
     */
    offset = roundup(offset, ELF_EXEC_PAGESIZE);

    /*
     * Write program headers for memory regions mapped in
     * the target process.
     */
    for (vma = vma_first(mm); vma != NULL; vma = vma_next(vma)) {
        (void) memset(&phdr, 0, sizeof (phdr));

        phdr.p_type = PT_LOAD;
        phdr.p_offset = offset;
        phdr.p_vaddr = vma->vma_start;
        phdr.p_paddr = 0;
        phdr.p_filesz = vma_dump_size(vma);
        offset += phdr.p_filesz;
        phdr.p_memsz = vma->vma_end - vma->vma_start;
        phdr.p_flags = vma->vma_flags & PROT_READ ? PF_R : 0;
        if (vma->vma_flags & PROT_WRITE)
            phdr.p_flags |= PF_W;
        if (vma->vma_flags & PROT_EXEC)
            phdr.p_flags |= PF_X;
        phdr.p_align = ELF_EXEC_PAGESIZE;

        dump_write(fd, &phdr, sizeof (phdr));
    }

    /*
     * Next we write notes just after program headers.  No
     * alignment needed here.
     */
    if (write_note_info(&info, fd) < 0)
        goto out;

    /* align data to page boundary */
    data_offset = lseek(fd, 0, SEEK_CUR);
    data_offset = TARGET_PAGE_ALIGN(data_offset);
    if (lseek(fd, data_offset, SEEK_SET) != data_offset)
        goto out;

    /*
     * Finally we can dump process memory into corefile as well.
     */
    for (vma = vma_first(mm); vma != NULL; vma = vma_next(vma)) {
        abi_ulong addr;
        abi_ulong end;

        end = vma->vma_start + vma_dump_size(vma);

        for (addr = vma->vma_start; addr < end;
            addr += TARGET_PAGE_SIZE) {
            char page[TARGET_PAGE_SIZE];
            int error;

            /*
             *  Read in page from target process memory and
             *  write it to coredump file.
             */
            error = copy_from_user(page, addr, sizeof (page));
            if (error != 0) {
                (void) fprintf(stderr, "unable to dump " TARGET_FMT_lx "\n",
                    addr);
                errno = -error;
                goto out;
            }
            if (dump_write(fd, page, TARGET_PAGE_SIZE) < 0)
                goto out;
        }
    }

out:
    free_note_info(&info);
    if (mm != NULL)
        vma_delete(mm);
    (void) close(fd);

    if (errno != 0)
        return (-errno);
    return (0);
}

#endif /* USE_ELF_CORE_DUMP */

static int load_aout_interp(void * exptr, int interp_fd)
{
    printf("a.out interpreter not yet supported\n");
    return(0);
}

void do_init_thread(struct target_pt_regs *regs, struct image_info *infop)
{
    init_thread(regs, infop);
}
