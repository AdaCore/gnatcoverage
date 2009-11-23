/*
 *  MicroBlaze virtual CPU header
 *
 *  Copyright (c) 2009 Edgar E. Iglesias
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */
#ifndef CPU_MICROBLAZE_H
#define CPU_MICROBLAZE_H

#define TARGET_LONG_BITS 32

#define CPUState struct CPUMBState

#include "cpu-defs.h"
struct CPUMBState;
#if !defined(CONFIG_USER_ONLY)
#include "mmu.h"
#endif

#define TARGET_HAS_ICE 1

#define ELF_MACHINE	EM_XILINX_MICROBLAZE

#define EXCP_NMI        1
#define EXCP_MMU        2
#define EXCP_IRQ        3
#define EXCP_BREAK      4
#define EXCP_HW_BREAK   5

/* Register aliases. R0 - R15 */
#define R_SP     1
#define SR_PC    0
#define SR_MSR   1
#define SR_EAR   3
#define SR_ESR   5
#define SR_FSR   7
#define SR_BTR   0xb
#define SR_EDR   0xd

/* MSR flags.  */
#define MSR_BE  (1<<0) /* 0x001 */
#define MSR_IE  (1<<1) /* 0x002 */
#define MSR_C   (1<<2) /* 0x004 */
#define MSR_BIP (1<<3) /* 0x008 */
#define MSR_FSL (1<<4) /* 0x010 */
#define MSR_ICE (1<<5) /* 0x020 */
#define MSR_DZ  (1<<6) /* 0x040 */
#define MSR_DCE (1<<7) /* 0x080 */
#define MSR_EE  (1<<8) /* 0x100 */
#define MSR_EIP (1<<9) /* 0x200 */
#define MSR_CC  (1<<31)

/* Machine State Register (MSR) Fields */
#define MSR_UM (1<<11) /* User Mode */
#define MSR_UMS        (1<<12) /* User Mode Save */
#define MSR_VM (1<<13) /* Virtual Mode */
#define MSR_VMS        (1<<14) /* Virtual Mode Save */

#define MSR_KERNEL      MSR_EE|MSR_VM
//#define MSR_USER     MSR_KERNEL|MSR_UM|MSR_IE
#define MSR_KERNEL_VMS  MSR_EE|MSR_VMS
//#define MSR_USER_VMS MSR_KERNEL_VMS|MSR_UMS|MSR_IE

/* Exception State Register (ESR) Fields */
#define          ESR_DIZ       (1<<11) /* Zone Protection */
#define          ESR_S         (1<<10) /* Store instruction */



/* Version reg.  */
/* Basic PVR mask */
#define PVR0_PVR_FULL_MASK              0x80000000
#define PVR0_USE_BARREL_MASK            0x40000000
#define PVR0_USE_DIV_MASK               0x20000000
#define PVR0_USE_HW_MUL_MASK            0x10000000
#define PVR0_USE_FPU_MASK               0x08000000
#define PVR0_USE_EXC_MASK               0x04000000
#define PVR0_USE_ICACHE_MASK            0x02000000
#define PVR0_USE_DCACHE_MASK            0x01000000
#define PVR0_USE_MMU                    0x00800000      /* new */
#define PVR0_VERSION_MASK               0x0000FF00
#define PVR0_USER1_MASK                 0x000000FF

/* User 2 PVR mask */
#define PVR1_USER2_MASK                 0xFFFFFFFF

/* Configuration PVR masks */
#define PVR2_D_OPB_MASK                 0x80000000
#define PVR2_D_LMB_MASK                 0x40000000
#define PVR2_I_OPB_MASK                 0x20000000
#define PVR2_I_LMB_MASK                 0x10000000
#define PVR2_INTERRUPT_IS_EDGE_MASK     0x08000000
#define PVR2_EDGE_IS_POSITIVE_MASK      0x04000000
#define PVR2_D_PLB_MASK                 0x02000000      /* new */
#define PVR2_I_PLB_MASK                 0x01000000      /* new */
#define PVR2_INTERCONNECT               0x00800000      /* new */
#define PVR2_USE_EXTEND_FSL             0x00080000      /* new */
#define PVR2_USE_FSL_EXC                0x00040000      /* new */
#define PVR2_USE_MSR_INSTR              0x00020000
#define PVR2_USE_PCMP_INSTR             0x00010000
#define PVR2_AREA_OPTIMISED             0x00008000
#define PVR2_USE_BARREL_MASK            0x00004000
#define PVR2_USE_DIV_MASK               0x00002000
#define PVR2_USE_HW_MUL_MASK            0x00001000
#define PVR2_USE_FPU_MASK               0x00000800
#define PVR2_USE_MUL64_MASK             0x00000400
#define PVR2_USE_FPU2_MASK              0x00000200      /* new */
#define PVR2_USE_IPLBEXC                0x00000100
#define PVR2_USE_DPLBEXC                0x00000080
#define PVR2_OPCODE_0x0_ILL_MASK        0x00000040
#define PVR2_UNALIGNED_EXC_MASK         0x00000020
#define PVR2_ILL_OPCODE_EXC_MASK        0x00000010
#define PVR2_IOPB_BUS_EXC_MASK          0x00000008
#define PVR2_DOPB_BUS_EXC_MASK          0x00000004
#define PVR2_DIV_ZERO_EXC_MASK          0x00000002
#define PVR2_FPU_EXC_MASK               0x00000001

/* Debug and exception PVR masks */
#define PVR3_DEBUG_ENABLED_MASK         0x80000000
#define PVR3_NUMBER_OF_PC_BRK_MASK      0x1E000000
#define PVR3_NUMBER_OF_RD_ADDR_BRK_MASK 0x00380000
#define PVR3_NUMBER_OF_WR_ADDR_BRK_MASK 0x0000E000
#define PVR3_FSL_LINKS_MASK             0x00000380

/* ICache config PVR masks */
#define PVR4_USE_ICACHE_MASK            0x80000000
#define PVR4_ICACHE_ADDR_TAG_BITS_MASK  0x7C000000
#define PVR4_ICACHE_USE_FSL_MASK        0x02000000
#define PVR4_ICACHE_ALLOW_WR_MASK       0x01000000
#define PVR4_ICACHE_LINE_LEN_MASK       0x00E00000
#define PVR4_ICACHE_BYTE_SIZE_MASK      0x001F0000

/* DCache config PVR masks */
#define PVR5_USE_DCACHE_MASK            0x80000000
#define PVR5_DCACHE_ADDR_TAG_BITS_MASK  0x7C000000
#define PVR5_DCACHE_USE_FSL_MASK        0x02000000
#define PVR5_DCACHE_ALLOW_WR_MASK       0x01000000
#define PVR5_DCACHE_LINE_LEN_MASK       0x00E00000
#define PVR5_DCACHE_BYTE_SIZE_MASK      0x001F0000

/* ICache base address PVR mask */
#define PVR6_ICACHE_BASEADDR_MASK       0xFFFFFFFF

/* ICache high address PVR mask */
#define PVR7_ICACHE_HIGHADDR_MASK       0xFFFFFFFF

/* DCache base address PVR mask */
#define PVR8_DCACHE_BASEADDR_MASK       0xFFFFFFFF

/* DCache high address PVR mask */
#define PVR9_DCACHE_HIGHADDR_MASK       0xFFFFFFFF

/* Target family PVR mask */
#define PVR10_TARGET_FAMILY_MASK        0xFF000000

/* MMU descrtiption */
#define PVR11_USE_MMU                   0xC0000000
#define PVR11_MMU_ITLB_SIZE             0x38000000
#define PVR11_MMU_DTLB_SIZE             0x07000000
#define PVR11_MMU_TLB_ACCESS            0x00C00000
#define PVR11_MMU_ZONES                 0x003C0000
/* MSR Reset value PVR mask */
#define PVR11_MSR_RESET_VALUE_MASK      0x000007FF



/* CPU flags.  */

/* Condition codes.  */
#define CC_GE  5
#define CC_GT  4
#define CC_LE  3
#define CC_LT  2
#define CC_NE  1
#define CC_EQ  0

#define NB_MMU_MODES    3
typedef struct CPUMBState {
    uint32_t debug;
    uint32_t btaken;
    uint32_t btarget;
    uint32_t bimm;

    uint32_t imm;
    uint32_t regs[33];
    uint32_t sregs[24];

    /* Internal flags.  */
#define IMM_FLAG	4	
#define DRTI_FLAG	(1 << 16)
#define DRTE_FLAG	(1 << 17)
#define DRTB_FLAG	(1 << 18)
#define D_FLAG		(1 << 19)  /* Bit in ESR.  */
/* TB dependant CPUState.  */
#define IFLAGS_TB_MASK	(D_FLAG | IMM_FLAG | DRTI_FLAG | DRTE_FLAG | DRTB_FLAG)
    uint32_t iflags;

    struct {
        uint32_t regs[16];
    } pvr;

#if !defined(CONFIG_USER_ONLY)
    /* Unified MMU.  */
    struct microblaze_mmu mmu;
#endif

    CPU_COMMON
} CPUMBState;

CPUState *cpu_mb_init(const char *cpu_model);
int cpu_mb_exec(CPUState *s);
void cpu_mb_close(CPUState *s);
void do_interrupt(CPUState *env);
/* you can call this signal handler from your SIGBUS and SIGSEGV
   signal handlers to inform the virtual CPU of exceptions. non zero
   is returned if the signal was handled by the virtual CPU.  */
int cpu_mb_signal_handler(int host_signum, void *pinfo,
                          void *puc);

enum {
    CC_OP_DYNAMIC, /* Use env->cc_op  */
    CC_OP_FLAGS,
    CC_OP_CMP,
};

/* FIXME: MB uses variable pages down to 1K but linux only uses 4k.  */
#define TARGET_PAGE_BITS 12
#define MMAP_SHIFT TARGET_PAGE_BITS

#define cpu_init cpu_mb_init
#define cpu_exec cpu_mb_exec
#define cpu_gen_code cpu_mb_gen_code
#define cpu_signal_handler cpu_mb_signal_handler

#define CPU_SAVE_VERSION 1

/* MMU modes definitions */
#define MMU_MODE0_SUFFIX _nommu
#define MMU_MODE1_SUFFIX _kernel
#define MMU_MODE2_SUFFIX _user
#define MMU_NOMMU_IDX   0
#define MMU_KERNEL_IDX  1
#define MMU_USER_IDX    2
/* See NB_MMU_MODES further up the file.  */

static inline int cpu_mmu_index (CPUState *env)
{
        /* Are we in nommu mode?.  */
        if (!(env->sregs[SR_MSR] & MSR_VM))
            return MMU_NOMMU_IDX;

	if (env->sregs[SR_MSR] & MSR_UM)
            return MMU_USER_IDX;
        return MMU_KERNEL_IDX;
}

int cpu_mb_handle_mmu_fault(CPUState *env, target_ulong address, int rw,
                            int mmu_idx, int is_softmmu);

#if defined(CONFIG_USER_ONLY)
static inline void cpu_clone_regs(CPUState *env, target_ulong newsp)
{
    if (newsp)
        env->regs[R_SP] = newsp;
    env->regs[3] = 0;
}
#endif

static inline void cpu_set_tls(CPUState *env, target_ulong newtls)
{
}

static inline int cpu_interrupts_enabled(CPUState *env)
{
    return env->sregs[SR_MSR] & MSR_IE;
}

#include "cpu-all.h"
#include "exec-all.h"

static inline void cpu_pc_from_tb(CPUState *env, TranslationBlock *tb)
{
    env->sregs[SR_PC] = tb->pc;
}

static inline target_ulong cpu_get_pc(CPUState *env)
{
    return env->sregs[SR_PC];
}

static inline void cpu_get_tb_cpu_state(CPUState *env, target_ulong *pc,
                                        target_ulong *cs_base, int *flags)
{
    *pc = env->sregs[SR_PC];
    *cs_base = 0;
    *flags = env->iflags & IFLAGS_TB_MASK;
}
#endif
