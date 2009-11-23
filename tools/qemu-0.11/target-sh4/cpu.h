/*
 *  SH4 emulation
 *
 *  Copyright (c) 2005 Samuel Tardieu
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */
#ifndef _CPU_SH4_H
#define _CPU_SH4_H

#include "config.h"

#define TARGET_LONG_BITS 32
#define TARGET_HAS_ICE 1

#define ELF_MACHINE	EM_SH

/* CPU Subtypes */
#define SH_CPU_SH7750  (1 << 0)
#define SH_CPU_SH7750S (1 << 1)
#define SH_CPU_SH7750R (1 << 2)
#define SH_CPU_SH7751  (1 << 3)
#define SH_CPU_SH7751R (1 << 4)
#define SH_CPU_SH7785  (1 << 5)
#define SH_CPU_SH7750_ALL (SH_CPU_SH7750 | SH_CPU_SH7750S | SH_CPU_SH7750R)
#define SH_CPU_SH7751_ALL (SH_CPU_SH7751 | SH_CPU_SH7751R)

#define CPUState struct CPUSH4State

#include "cpu-defs.h"

#include "softfloat.h"

#define TARGET_PAGE_BITS 12	/* 4k XXXXX */

#define SR_MD (1 << 30)
#define SR_RB (1 << 29)
#define SR_BL (1 << 28)
#define SR_FD (1 << 15)
#define SR_M  (1 << 9)
#define SR_Q  (1 << 8)
#define SR_I3 (1 << 7)
#define SR_I2 (1 << 6)
#define SR_I1 (1 << 5)
#define SR_I0 (1 << 4)
#define SR_S  (1 << 1)
#define SR_T  (1 << 0)

#define FPSCR_FR (1 << 21)
#define FPSCR_SZ (1 << 20)
#define FPSCR_PR (1 << 19)
#define FPSCR_DN (1 << 18)
#define DELAY_SLOT             (1 << 0)
#define DELAY_SLOT_CONDITIONAL (1 << 1)
#define DELAY_SLOT_TRUE        (1 << 2)
#define DELAY_SLOT_CLEARME     (1 << 3)
/* The dynamic value of the DELAY_SLOT_TRUE flag determines whether the jump
 * after the delay slot should be taken or not. It is calculated from SR_T.
 *
 * It is unclear if it is permitted to modify the SR_T flag in a delay slot.
 * The use of DELAY_SLOT_TRUE flag makes us accept such SR_T modification.
 */

/* XXXXX The structure could be made more compact */
typedef struct tlb_t {
    uint8_t asid;		/* address space identifier */
    uint32_t vpn;		/* virtual page number */
    uint8_t v;			/* validity */
    uint32_t ppn;		/* physical page number */
    uint8_t sz;			/* page size */
    uint32_t size;		/* cached page size in bytes */
    uint8_t sh;			/* share status */
    uint8_t c;			/* cacheability */
    uint8_t pr;			/* protection key */
    uint8_t d;			/* dirty */
    uint8_t wt;			/* write through */
    uint8_t sa;			/* space attribute (PCMCIA) */
    uint8_t tc;			/* timing control */
} tlb_t;

#define UTLB_SIZE 64
#define ITLB_SIZE 4

#define NB_MMU_MODES 2

enum sh_features {
    SH_FEATURE_SH4A = 1,
    SH_FEATURE_BCR3_AND_BCR4 = 2,
};

typedef struct memory_content {
    uint32_t address;
    uint32_t value;
    struct memory_content *next;
} memory_content;

typedef struct CPUSH4State {
    int id;			/* CPU model */

    uint32_t flags;		/* general execution flags */
    uint32_t gregs[24];		/* general registers */
    float32 fregs[32];		/* floating point registers */
    uint32_t sr;		/* status register */
    uint32_t ssr;		/* saved status register */
    uint32_t spc;		/* saved program counter */
    uint32_t gbr;		/* global base register */
    uint32_t vbr;		/* vector base register */
    uint32_t sgr;		/* saved global register 15 */
    uint32_t dbr;		/* debug base register */
    uint32_t pc;		/* program counter */
    uint32_t delayed_pc;	/* target of delayed jump */
    uint32_t mach;		/* multiply and accumulate high */
    uint32_t macl;		/* multiply and accumulate low */
    uint32_t pr;		/* procedure register */
    uint32_t fpscr;		/* floating point status/control register */
    uint32_t fpul;		/* floating point communication register */

    /* float point status register */
    float_status fp_status;

    /* The features that we should emulate. See sh_features above.  */
    uint32_t features;

    /* Those belong to the specific unit (SH7750) but are handled here */
    uint32_t mmucr;		/* MMU control register */
    uint32_t pteh;		/* page table entry high register */
    uint32_t ptel;		/* page table entry low register */
    uint32_t ptea;		/* page table entry assistance register */
    uint32_t ttb;		/* tranlation table base register */
    uint32_t tea;		/* TLB exception address register */
    uint32_t tra;		/* TRAPA exception register */
    uint32_t expevt;		/* exception event register */
    uint32_t intevt;		/* interrupt event register */

    uint32_t pvr;		/* Processor Version Register */
    uint32_t prr;		/* Processor Revision Register */
    uint32_t cvr;		/* Cache Version Register */

    uint32_t ldst;

     CPU_COMMON tlb_t utlb[UTLB_SIZE];	/* unified translation table */
    tlb_t itlb[ITLB_SIZE];	/* instruction translation table */
    void *intc_handle;
    int intr_at_halt;		/* SR_BL ignored during sleep */
    memory_content *movcal_backup;
    memory_content **movcal_backup_tail;
} CPUSH4State;

CPUSH4State *cpu_sh4_init(const char *cpu_model);
int cpu_sh4_exec(CPUSH4State * s);
int cpu_sh4_signal_handler(int host_signum, void *pinfo,
                           void *puc);
int cpu_sh4_handle_mmu_fault(CPUSH4State * env, target_ulong address, int rw,
			     int mmu_idx, int is_softmmu);
void do_interrupt(CPUSH4State * env);

void sh4_cpu_list(FILE *f, int (*cpu_fprintf)(FILE *f, const char *fmt, ...));
void cpu_sh4_write_mmaped_utlb_addr(CPUSH4State *s, target_phys_addr_t addr,
				    uint32_t mem_value);

int cpu_sh4_is_cached(CPUSH4State * env, target_ulong addr);

static inline void cpu_set_tls(CPUSH4State *env, target_ulong newtls)
{
  env->gbr = newtls;
}

void cpu_load_tlb(CPUSH4State * env);

#include "softfloat.h"

#define cpu_init cpu_sh4_init
#define cpu_exec cpu_sh4_exec
#define cpu_gen_code cpu_sh4_gen_code
#define cpu_signal_handler cpu_sh4_signal_handler
#define cpu_list sh4_cpu_list

/* MMU modes definitions */
#define MMU_MODE0_SUFFIX _kernel
#define MMU_MODE1_SUFFIX _user
#define MMU_USER_IDX 1
static inline int cpu_mmu_index (CPUState *env)
{
    return (env->sr & SR_MD) == 0 ? 1 : 0;
}

#if defined(CONFIG_USER_ONLY)
static inline void cpu_clone_regs(CPUState *env, target_ulong newsp)
{
    if (newsp)
        env->gregs[15] = newsp;
    env->gregs[0] = 0;
}
#endif

#include "cpu-all.h"
#include "exec-all.h"

/* Memory access type */
enum {
    /* Privilege */
    ACCESS_PRIV = 0x01,
    /* Direction */
    ACCESS_WRITE = 0x02,
    /* Type of instruction */
    ACCESS_CODE = 0x10,
    ACCESS_INT = 0x20
};

/* MMU control register */
#define MMUCR    0x1F000010
#define MMUCR_AT (1<<0)
#define MMUCR_SV (1<<8)
#define MMUCR_URC_BITS (6)
#define MMUCR_URC_OFFSET (10)
#define MMUCR_URC_SIZE (1 << MMUCR_URC_BITS)
#define MMUCR_URC_MASK (((MMUCR_URC_SIZE) - 1) << MMUCR_URC_OFFSET)
static inline int cpu_mmucr_urc (uint32_t mmucr)
{
    return ((mmucr & MMUCR_URC_MASK) >> MMUCR_URC_OFFSET);
}

/* PTEH : Page Translation Entry High register */
#define PTEH_ASID_BITS (8)
#define PTEH_ASID_SIZE (1 << PTEH_ASID_BITS)
#define PTEH_ASID_MASK (PTEH_ASID_SIZE - 1)
#define cpu_pteh_asid(pteh) ((pteh) & PTEH_ASID_MASK)
#define PTEH_VPN_BITS (22)
#define PTEH_VPN_OFFSET (10)
#define PTEH_VPN_SIZE (1 << PTEH_VPN_BITS)
#define PTEH_VPN_MASK (((PTEH_VPN_SIZE) - 1) << PTEH_VPN_OFFSET)
static inline int cpu_pteh_vpn (uint32_t pteh)
{
    return ((pteh & PTEH_VPN_MASK) >> PTEH_VPN_OFFSET);
}

/* PTEL : Page Translation Entry Low register */
#define PTEL_V        (1 << 8)
#define cpu_ptel_v(ptel) (((ptel) & PTEL_V) >> 8)
#define PTEL_C        (1 << 3)
#define cpu_ptel_c(ptel) (((ptel) & PTEL_C) >> 3)
#define PTEL_D        (1 << 2)
#define cpu_ptel_d(ptel) (((ptel) & PTEL_D) >> 2)
#define PTEL_SH       (1 << 1)
#define cpu_ptel_sh(ptel)(((ptel) & PTEL_SH) >> 1)
#define PTEL_WT       (1 << 0)
#define cpu_ptel_wt(ptel) ((ptel) & PTEL_WT)

#define PTEL_SZ_HIGH_OFFSET  (7)
#define PTEL_SZ_HIGH  (1 << PTEL_SZ_HIGH_OFFSET)
#define PTEL_SZ_LOW_OFFSET   (4)
#define PTEL_SZ_LOW   (1 << PTEL_SZ_LOW_OFFSET)
static inline int cpu_ptel_sz (uint32_t ptel)
{
    int sz;
    sz = (ptel & PTEL_SZ_HIGH) >> PTEL_SZ_HIGH_OFFSET;
    sz <<= 1;
    sz |= (ptel & PTEL_SZ_LOW) >> PTEL_SZ_LOW_OFFSET;
    return sz;
}

#define PTEL_PPN_BITS (19)
#define PTEL_PPN_OFFSET (10)
#define PTEL_PPN_SIZE (1 << PTEL_PPN_BITS)
#define PTEL_PPN_MASK (((PTEL_PPN_SIZE) - 1) << PTEL_PPN_OFFSET)
static inline int cpu_ptel_ppn (uint32_t ptel)
{
    return ((ptel & PTEL_PPN_MASK) >> PTEL_PPN_OFFSET);
}

#define PTEL_PR_BITS   (2)
#define PTEL_PR_OFFSET (5)
#define PTEL_PR_SIZE (1 << PTEL_PR_BITS)
#define PTEL_PR_MASK (((PTEL_PR_SIZE) - 1) << PTEL_PR_OFFSET)
static inline int cpu_ptel_pr (uint32_t ptel)
{
    return ((ptel & PTEL_PR_MASK) >> PTEL_PR_OFFSET);
}

/* PTEA : Page Translation Entry Assistance register */
#define PTEA_SA_BITS (3)
#define PTEA_SA_SIZE (1 << PTEA_SA_BITS)
#define PTEA_SA_MASK (PTEA_SA_SIZE - 1)
#define cpu_ptea_sa(ptea) ((ptea) & PTEA_SA_MASK)
#define PTEA_TC        (1 << 3)
#define cpu_ptea_tc(ptea) (((ptea) & PTEA_TC) >> 3)

static inline void cpu_pc_from_tb(CPUState *env, TranslationBlock *tb)
{
    env->pc = tb->pc;
    env->flags = tb->flags;
}

#define TB_FLAG_PENDING_MOVCA  (1 << 4)

static inline void cpu_get_tb_cpu_state(CPUState *env, target_ulong *pc,
                                        target_ulong *cs_base, int *flags)
{
    *pc = env->pc;
    *cs_base = 0;
    *flags = (env->flags & (DELAY_SLOT | DELAY_SLOT_CONDITIONAL
                    | DELAY_SLOT_TRUE | DELAY_SLOT_CLEARME))   /* Bits  0- 3 */
            | (env->fpscr & (FPSCR_FR | FPSCR_SZ | FPSCR_PR))  /* Bits 19-21 */
            | (env->sr & (SR_MD | SR_RB))                      /* Bits 29-30 */
            | (env->sr & SR_FD)                                /* Bit 15 */
            | (env->movcal_backup ? TB_FLAG_PENDING_MOVCA : 0); /* Bit 4 */
}

#endif				/* _CPU_SH4_H */
