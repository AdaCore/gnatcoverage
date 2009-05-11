/*
 *  PowerPC emulation for qemu: main translation routines.
 *
 *  Copyright (c) 2003-2007 Jocelyn Mayer
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
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston MA  02110-1301 USA
 */
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include "cpu.h"
#include "exec-all.h"
#include "disas.h"
#include "tcg-op.h"
#include "qemu-common.h"

#include "helper.h"
#define GEN_HELPER 1
#include "helper.h"

#define CPU_SINGLE_STEP 0x1
#define CPU_BRANCH_STEP 0x2
#define GDBSTUB_SINGLE_STEP 0x4

/* Include definitions for instructions classes and implementations flags */
//#define DO_SINGLE_STEP
//#define PPC_DEBUG_DISAS
//#define DO_PPC_STATISTICS

#ifdef PPC_DEBUG_DISAS
#  define LOG_DISAS(...) qemu_log_mask(CPU_LOG_TB_IN_ASM, ## __VA_ARGS__)
#else
#  define LOG_DISAS(...) do { } while (0)
#endif
/*****************************************************************************/
/* Code translation helpers                                                  */

/* global register indexes */
static TCGv_ptr cpu_env;
static char cpu_reg_names[10*3 + 22*4 /* GPR */
#if !defined(TARGET_PPC64)
    + 10*4 + 22*5 /* SPE GPRh */
#endif
    + 10*4 + 22*5 /* FPR */
    + 2*(10*6 + 22*7) /* AVRh, AVRl */
    + 8*5 /* CRF */];
static TCGv cpu_gpr[32];
#if !defined(TARGET_PPC64)
static TCGv cpu_gprh[32];
#endif
static TCGv_i64 cpu_fpr[32];
static TCGv_i64 cpu_avrh[32], cpu_avrl[32];
static TCGv_i32 cpu_crf[8];
static TCGv cpu_nip;
static TCGv cpu_msr;
static TCGv cpu_ctr;
static TCGv cpu_lr;
static TCGv cpu_xer;
static TCGv cpu_reserve;
static TCGv_i32 cpu_fpscr;
static TCGv_i32 cpu_access_type;

#include "gen-icount.h"

void ppc_translate_init(void)
{
    int i;
    char* p;
    static int done_init = 0;

    if (done_init)
        return;

    cpu_env = tcg_global_reg_new_ptr(TCG_AREG0, "env");

    p = cpu_reg_names;

    for (i = 0; i < 8; i++) {
        sprintf(p, "crf%d", i);
        cpu_crf[i] = tcg_global_mem_new_i32(TCG_AREG0,
                                            offsetof(CPUState, crf[i]), p);
        p += 5;
    }

    for (i = 0; i < 32; i++) {
        sprintf(p, "r%d", i);
        cpu_gpr[i] = tcg_global_mem_new(TCG_AREG0,
                                        offsetof(CPUState, gpr[i]), p);
        p += (i < 10) ? 3 : 4;
#if !defined(TARGET_PPC64)
        sprintf(p, "r%dH", i);
        cpu_gprh[i] = tcg_global_mem_new_i32(TCG_AREG0,
                                             offsetof(CPUState, gprh[i]), p);
        p += (i < 10) ? 4 : 5;
#endif

        sprintf(p, "fp%d", i);
        cpu_fpr[i] = tcg_global_mem_new_i64(TCG_AREG0,
                                            offsetof(CPUState, fpr[i]), p);
        p += (i < 10) ? 4 : 5;

        sprintf(p, "avr%dH", i);
#ifdef WORDS_BIGENDIAN
        cpu_avrh[i] = tcg_global_mem_new_i64(TCG_AREG0,
                                             offsetof(CPUState, avr[i].u64[0]), p);
#else
        cpu_avrh[i] = tcg_global_mem_new_i64(TCG_AREG0,
                                             offsetof(CPUState, avr[i].u64[1]), p);
#endif
        p += (i < 10) ? 6 : 7;

        sprintf(p, "avr%dL", i);
#ifdef WORDS_BIGENDIAN
        cpu_avrl[i] = tcg_global_mem_new_i64(TCG_AREG0,
                                             offsetof(CPUState, avr[i].u64[1]), p);
#else
        cpu_avrl[i] = tcg_global_mem_new_i64(TCG_AREG0,
                                             offsetof(CPUState, avr[i].u64[0]), p);
#endif
        p += (i < 10) ? 6 : 7;
    }

    cpu_nip = tcg_global_mem_new(TCG_AREG0,
                                 offsetof(CPUState, nip), "nip");

    cpu_msr = tcg_global_mem_new(TCG_AREG0,
                                 offsetof(CPUState, msr), "msr");

    cpu_ctr = tcg_global_mem_new(TCG_AREG0,
                                 offsetof(CPUState, ctr), "ctr");

    cpu_lr = tcg_global_mem_new(TCG_AREG0,
                                offsetof(CPUState, lr), "lr");

    cpu_xer = tcg_global_mem_new(TCG_AREG0,
                                 offsetof(CPUState, xer), "xer");

    cpu_reserve = tcg_global_mem_new(TCG_AREG0,
                                     offsetof(CPUState, reserve), "reserve");

    cpu_fpscr = tcg_global_mem_new_i32(TCG_AREG0,
                                       offsetof(CPUState, fpscr), "fpscr");

    cpu_access_type = tcg_global_mem_new_i32(TCG_AREG0,
                                             offsetof(CPUState, access_type), "access_type");

    /* register helpers */
#define GEN_HELPER 2
#include "helper.h"

    done_init = 1;
}

/* internal defines */
typedef struct DisasContext {
    struct TranslationBlock *tb;
    target_ulong nip;
    uint32_t opcode;
    uint32_t exception;
    /* Routine used to access memory */
    int mem_idx;
    int access_type;
    /* Translation flags */
    int le_mode;
#if defined(TARGET_PPC64)
    int sf_mode;
#endif
    int fpu_enabled;
    int altivec_enabled;
    int spe_enabled;
    ppc_spr_t *spr_cb; /* Needed to check rights for mfspr/mtspr */
    int singlestep_enabled;
} DisasContext;

struct opc_handler_t {
    /* invalid bits */
    uint32_t inval;
    /* instruction type */
    uint64_t type;
    /* handler */
    void (*handler)(DisasContext *ctx);
#if defined(DO_PPC_STATISTICS) || defined(PPC_DUMP_CPU)
    const char *oname;
#endif
#if defined(DO_PPC_STATISTICS)
    uint64_t count;
#endif
};

static always_inline void gen_reset_fpstatus (void)
{
#ifdef CONFIG_SOFTFLOAT
    gen_helper_reset_fpstatus();
#endif
}

static always_inline void gen_compute_fprf (TCGv_i64 arg, int set_fprf, int set_rc)
{
    TCGv_i32 t0 = tcg_temp_new_i32();

    if (set_fprf != 0) {
        /* This case might be optimized later */
        tcg_gen_movi_i32(t0, 1);
        gen_helper_compute_fprf(t0, arg, t0);
        if (unlikely(set_rc)) {
            tcg_gen_mov_i32(cpu_crf[1], t0);
        }
        gen_helper_float_check_status();
    } else if (unlikely(set_rc)) {
        /* We always need to compute fpcc */
        tcg_gen_movi_i32(t0, 0);
        gen_helper_compute_fprf(t0, arg, t0);
        tcg_gen_mov_i32(cpu_crf[1], t0);
    }

    tcg_temp_free_i32(t0);
}

static always_inline void gen_set_access_type (DisasContext *ctx, int access_type)
{
    if (ctx->access_type != access_type) {
        tcg_gen_movi_i32(cpu_access_type, access_type);
        ctx->access_type = access_type;
    }
}

static always_inline void gen_update_nip (DisasContext *ctx, target_ulong nip)
{
#if defined(TARGET_PPC64)
    if (ctx->sf_mode)
        tcg_gen_movi_tl(cpu_nip, nip);
    else
#endif
        tcg_gen_movi_tl(cpu_nip, (uint32_t)nip);
}

static always_inline void gen_exception_err (DisasContext *ctx, uint32_t excp, uint32_t error)
{
    TCGv_i32 t0, t1;
    if (ctx->exception == POWERPC_EXCP_NONE) {
        gen_update_nip(ctx, ctx->nip);
    }
    t0 = tcg_const_i32(excp);
    t1 = tcg_const_i32(error);
    gen_helper_raise_exception_err(t0, t1);
    tcg_temp_free_i32(t0);
    tcg_temp_free_i32(t1);
    ctx->exception = (excp);
}

static always_inline void gen_exception (DisasContext *ctx, uint32_t excp)
{
    TCGv_i32 t0;
    if (ctx->exception == POWERPC_EXCP_NONE) {
        gen_update_nip(ctx, ctx->nip);
    }
    t0 = tcg_const_i32(excp);
    gen_helper_raise_exception(t0);
    tcg_temp_free_i32(t0);
    ctx->exception = (excp);
}

static always_inline void gen_debug_exception (DisasContext *ctx)
{
    TCGv_i32 t0;
    gen_update_nip(ctx, ctx->nip);
    t0 = tcg_const_i32(EXCP_DEBUG);
    gen_helper_raise_exception(t0);
    tcg_temp_free_i32(t0);
}

static always_inline void gen_inval_exception (DisasContext *ctx, uint32_t error)
{
    gen_exception_err(ctx, POWERPC_EXCP_PROGRAM, POWERPC_EXCP_INVAL | error);
}

/* Stop translation */
static always_inline void gen_stop_exception (DisasContext *ctx)
{
    gen_update_nip(ctx, ctx->nip);
    ctx->exception = POWERPC_EXCP_STOP;
}

/* No need to update nip here, as execution flow will change */
static always_inline void gen_sync_exception (DisasContext *ctx)
{
    ctx->exception = POWERPC_EXCP_SYNC;
}

#define GEN_HANDLER(name, opc1, opc2, opc3, inval, type)                      \
static void gen_##name (DisasContext *ctx);                                   \
GEN_OPCODE(name, opc1, opc2, opc3, inval, type);                              \
static void gen_##name (DisasContext *ctx)

#define GEN_HANDLER2(name, onam, opc1, opc2, opc3, inval, type)               \
static void gen_##name (DisasContext *ctx);                                   \
GEN_OPCODE2(name, onam, opc1, opc2, opc3, inval, type);                       \
static void gen_##name (DisasContext *ctx)

typedef struct opcode_t {
    unsigned char opc1, opc2, opc3;
#if HOST_LONG_BITS == 64 /* Explicitly align to 64 bits */
    unsigned char pad[5];
#else
    unsigned char pad[1];
#endif
    opc_handler_t handler;
    const char *oname;
} opcode_t;

/*****************************************************************************/
/***                           Instruction decoding                        ***/
#define EXTRACT_HELPER(name, shift, nb)                                       \
static always_inline uint32_t name (uint32_t opcode)                          \
{                                                                             \
    return (opcode >> (shift)) & ((1 << (nb)) - 1);                           \
}

#define EXTRACT_SHELPER(name, shift, nb)                                      \
static always_inline int32_t name (uint32_t opcode)                           \
{                                                                             \
    return (int16_t)((opcode >> (shift)) & ((1 << (nb)) - 1));                \
}

/* Opcode part 1 */
EXTRACT_HELPER(opc1, 26, 6);
/* Opcode part 2 */
EXTRACT_HELPER(opc2, 1, 5);
/* Opcode part 3 */
EXTRACT_HELPER(opc3, 6, 5);
/* Update Cr0 flags */
EXTRACT_HELPER(Rc, 0, 1);
/* Destination */
EXTRACT_HELPER(rD, 21, 5);
/* Source */
EXTRACT_HELPER(rS, 21, 5);
/* First operand */
EXTRACT_HELPER(rA, 16, 5);
/* Second operand */
EXTRACT_HELPER(rB, 11, 5);
/* Third operand */
EXTRACT_HELPER(rC, 6, 5);
/***                               Get CRn                                 ***/
EXTRACT_HELPER(crfD, 23, 3);
EXTRACT_HELPER(crfS, 18, 3);
EXTRACT_HELPER(crbD, 21, 5);
EXTRACT_HELPER(crbA, 16, 5);
EXTRACT_HELPER(crbB, 11, 5);
/* SPR / TBL */
EXTRACT_HELPER(_SPR, 11, 10);
static always_inline uint32_t SPR (uint32_t opcode)
{
    uint32_t sprn = _SPR(opcode);

    return ((sprn >> 5) & 0x1F) | ((sprn & 0x1F) << 5);
}
/***                              Get constants                            ***/
EXTRACT_HELPER(IMM, 12, 8);
/* 16 bits signed immediate value */
EXTRACT_SHELPER(SIMM, 0, 16);
/* 16 bits unsigned immediate value */
EXTRACT_HELPER(UIMM, 0, 16);
/* 5 bits signed immediate value */
EXTRACT_HELPER(SIMM5, 16, 5);
/* 5 bits signed immediate value */
EXTRACT_HELPER(UIMM5, 16, 5);
/* Bit count */
EXTRACT_HELPER(NB, 11, 5);
/* Shift count */
EXTRACT_HELPER(SH, 11, 5);
/* Vector shift count */
EXTRACT_HELPER(VSH, 6, 4);
/* Mask start */
EXTRACT_HELPER(MB, 6, 5);
/* Mask end */
EXTRACT_HELPER(ME, 1, 5);
/* Trap operand */
EXTRACT_HELPER(TO, 21, 5);

EXTRACT_HELPER(CRM, 12, 8);
EXTRACT_HELPER(FM, 17, 8);
EXTRACT_HELPER(SR, 16, 4);
EXTRACT_HELPER(FPIMM, 12, 4);

/***                            Jump target decoding                       ***/
/* Displacement */
EXTRACT_SHELPER(d, 0, 16);
/* Immediate address */
static always_inline target_ulong LI (uint32_t opcode)
{
    return (opcode >> 0) & 0x03FFFFFC;
}

static always_inline uint32_t BD (uint32_t opcode)
{
    return (opcode >> 0) & 0xFFFC;
}

EXTRACT_HELPER(BO, 21, 5);
EXTRACT_HELPER(BI, 16, 5);
/* Absolute/relative address */
EXTRACT_HELPER(AA, 1, 1);
/* Link */
EXTRACT_HELPER(LK, 0, 1);

/* Create a mask between <start> and <end> bits */
static always_inline target_ulong MASK (uint32_t start, uint32_t end)
{
    target_ulong ret;

#if defined(TARGET_PPC64)
    if (likely(start == 0)) {
        ret = UINT64_MAX << (63 - end);
    } else if (likely(end == 63)) {
        ret = UINT64_MAX >> start;
    }
#else
    if (likely(start == 0)) {
        ret = UINT32_MAX << (31  - end);
    } else if (likely(end == 31)) {
        ret = UINT32_MAX >> start;
    }
#endif
    else {
        ret = (((target_ulong)(-1ULL)) >> (start)) ^
            (((target_ulong)(-1ULL) >> (end)) >> 1);
        if (unlikely(start > end))
            return ~ret;
    }

    return ret;
}

/*****************************************************************************/
/* PowerPC Instructions types definitions                                    */
enum {
    PPC_NONE           = 0x0000000000000000ULL,
    /* PowerPC base instructions set                                         */
    PPC_INSNS_BASE     = 0x0000000000000001ULL,
    /*   integer operations instructions                                     */
#define PPC_INTEGER PPC_INSNS_BASE
    /*   flow control instructions                                           */
#define PPC_FLOW    PPC_INSNS_BASE
    /*   virtual memory instructions                                         */
#define PPC_MEM     PPC_INSNS_BASE
    /*   ld/st with reservation instructions                                 */
#define PPC_RES     PPC_INSNS_BASE
    /*   spr/msr access instructions                                         */
#define PPC_MISC    PPC_INSNS_BASE
    /* Deprecated instruction sets                                           */
    /*   Original POWER instruction set                                      */
    PPC_POWER          = 0x0000000000000002ULL,
    /*   POWER2 instruction set extension                                    */
    PPC_POWER2         = 0x0000000000000004ULL,
    /*   Power RTC support                                                   */
    PPC_POWER_RTC      = 0x0000000000000008ULL,
    /*   Power-to-PowerPC bridge (601)                                       */
    PPC_POWER_BR       = 0x0000000000000010ULL,
    /* 64 bits PowerPC instruction set                                       */
    PPC_64B            = 0x0000000000000020ULL,
    /*   New 64 bits extensions (PowerPC 2.0x)                               */
    PPC_64BX           = 0x0000000000000040ULL,
    /*   64 bits hypervisor extensions                                       */
    PPC_64H            = 0x0000000000000080ULL,
    /*   New wait instruction (PowerPC 2.0x)                                 */
    PPC_WAIT           = 0x0000000000000100ULL,
    /*   Time base mftb instruction                                          */
    PPC_MFTB           = 0x0000000000000200ULL,

    /* Fixed-point unit extensions                                           */
    /*   PowerPC 602 specific                                                */
    PPC_602_SPEC       = 0x0000000000000400ULL,
    /*   isel instruction                                                    */
    PPC_ISEL           = 0x0000000000000800ULL,
    /*   popcntb instruction                                                 */
    PPC_POPCNTB        = 0x0000000000001000ULL,
    /*   string load / store                                                 */
    PPC_STRING         = 0x0000000000002000ULL,

    /* Floating-point unit extensions                                        */
    /*   Optional floating point instructions                                */
    PPC_FLOAT          = 0x0000000000010000ULL,
    /* New floating-point extensions (PowerPC 2.0x)                          */
    PPC_FLOAT_EXT      = 0x0000000000020000ULL,
    PPC_FLOAT_FSQRT    = 0x0000000000040000ULL,
    PPC_FLOAT_FRES     = 0x0000000000080000ULL,
    PPC_FLOAT_FRSQRTE  = 0x0000000000100000ULL,
    PPC_FLOAT_FRSQRTES = 0x0000000000200000ULL,
    PPC_FLOAT_FSEL     = 0x0000000000400000ULL,
    PPC_FLOAT_STFIWX   = 0x0000000000800000ULL,

    /* Vector/SIMD extensions                                                */
    /*   Altivec support                                                     */
    PPC_ALTIVEC        = 0x0000000001000000ULL,
    /*   PowerPC 2.03 SPE extension                                          */
    PPC_SPE            = 0x0000000002000000ULL,
    /*   PowerPC 2.03 SPE single-precision floating-point extension          */
    PPC_SPE_SINGLE     = 0x0000000004000000ULL,
    /*   PowerPC 2.03 SPE double-precision floating-point extension          */
    PPC_SPE_DOUBLE     = 0x0000000008000000ULL,

    /* Optional memory control instructions                                  */
    PPC_MEM_TLBIA      = 0x0000000010000000ULL,
    PPC_MEM_TLBIE      = 0x0000000020000000ULL,
    PPC_MEM_TLBSYNC    = 0x0000000040000000ULL,
    /*   sync instruction                                                    */
    PPC_MEM_SYNC       = 0x0000000080000000ULL,
    /*   eieio instruction                                                   */
    PPC_MEM_EIEIO      = 0x0000000100000000ULL,

    /* Cache control instructions                                            */
    PPC_CACHE          = 0x0000000200000000ULL,
    /*   icbi instruction                                                    */
    PPC_CACHE_ICBI     = 0x0000000400000000ULL,
    /*   dcbz instruction with fixed cache line size                         */
    PPC_CACHE_DCBZ     = 0x0000000800000000ULL,
    /*   dcbz instruction with tunable cache line size                       */
    PPC_CACHE_DCBZT    = 0x0000001000000000ULL,
    /*   dcba instruction                                                    */
    PPC_CACHE_DCBA     = 0x0000002000000000ULL,
    /*   Freescale cache locking instructions                                */
    PPC_CACHE_LOCK     = 0x0000004000000000ULL,

    /* MMU related extensions                                                */
    /*   external control instructions                                       */
    PPC_EXTERN         = 0x0000010000000000ULL,
    /*   segment register access instructions                                */
    PPC_SEGMENT        = 0x0000020000000000ULL,
    /*   PowerPC 6xx TLB management instructions                             */
    PPC_6xx_TLB        = 0x0000040000000000ULL,
    /* PowerPC 74xx TLB management instructions                              */
    PPC_74xx_TLB       = 0x0000080000000000ULL,
    /*   PowerPC 40x TLB management instructions                             */
    PPC_40x_TLB        = 0x0000100000000000ULL,
    /*   segment register access instructions for PowerPC 64 "bridge"        */
    PPC_SEGMENT_64B    = 0x0000200000000000ULL,
    /*   SLB management                                                      */
    PPC_SLBI           = 0x0000400000000000ULL,

    /* Embedded PowerPC dedicated instructions                               */
    PPC_WRTEE          = 0x0001000000000000ULL,
    /* PowerPC 40x exception model                                           */
    PPC_40x_EXCP       = 0x0002000000000000ULL,
    /* PowerPC 405 Mac instructions                                          */
    PPC_405_MAC        = 0x0004000000000000ULL,
    /* PowerPC 440 specific instructions                                     */
    PPC_440_SPEC       = 0x0008000000000000ULL,
    /* BookE (embedded) PowerPC specification                                */
    PPC_BOOKE          = 0x0010000000000000ULL,
    /* mfapidi instruction                                                   */
    PPC_MFAPIDI        = 0x0020000000000000ULL,
    /* tlbiva instruction                                                    */
    PPC_TLBIVA         = 0x0040000000000000ULL,
    /* tlbivax instruction                                                   */
    PPC_TLBIVAX        = 0x0080000000000000ULL,
    /* PowerPC 4xx dedicated instructions                                    */
    PPC_4xx_COMMON     = 0x0100000000000000ULL,
    /* PowerPC 40x ibct instructions                                         */
    PPC_40x_ICBT       = 0x0200000000000000ULL,
    /* rfmci is not implemented in all BookE PowerPC                         */
    PPC_RFMCI          = 0x0400000000000000ULL,
    /* rfdi instruction                                                      */
    PPC_RFDI           = 0x0800000000000000ULL,
    /* DCR accesses                                                          */
    PPC_DCR            = 0x1000000000000000ULL,
    /* DCR extended accesse                                                  */
    PPC_DCRX           = 0x2000000000000000ULL,
    /* user-mode DCR access, implemented in PowerPC 460                      */
    PPC_DCRUX          = 0x4000000000000000ULL,
};

/*****************************************************************************/
/* PowerPC instructions table                                                */
#if HOST_LONG_BITS == 64
#define OPC_ALIGN 8
#else
#define OPC_ALIGN 4
#endif
#if defined(__APPLE__)
#define OPCODES_SECTION                                                       \
    __attribute__ ((section("__TEXT,__opcodes"), unused, aligned (OPC_ALIGN) ))
#else
#define OPCODES_SECTION                                                       \
    __attribute__ ((section(".opcodes"), unused, aligned (OPC_ALIGN) ))
#endif

#if defined(DO_PPC_STATISTICS)
#define GEN_OPCODE(name, op1, op2, op3, invl, _typ)                           \
OPCODES_SECTION opcode_t opc_##name = {                                       \
    .opc1 = op1,                                                              \
    .opc2 = op2,                                                              \
    .opc3 = op3,                                                              \
    .pad  = { 0, },                                                           \
    .handler = {                                                              \
        .inval   = invl,                                                      \
        .type = _typ,                                                         \
        .handler = &gen_##name,                                               \
        .oname = stringify(name),                                             \
    },                                                                        \
    .oname = stringify(name),                                                 \
}
#define GEN_OPCODE2(name, onam, op1, op2, op3, invl, _typ)                    \
OPCODES_SECTION opcode_t opc_##name = {                                       \
    .opc1 = op1,                                                              \
    .opc2 = op2,                                                              \
    .opc3 = op3,                                                              \
    .pad  = { 0, },                                                           \
    .handler = {                                                              \
        .inval   = invl,                                                      \
        .type = _typ,                                                         \
        .handler = &gen_##name,                                               \
        .oname = onam,                                                        \
    },                                                                        \
    .oname = onam,                                                            \
}
#else
#define GEN_OPCODE(name, op1, op2, op3, invl, _typ)                           \
OPCODES_SECTION opcode_t opc_##name = {                                       \
    .opc1 = op1,                                                              \
    .opc2 = op2,                                                              \
    .opc3 = op3,                                                              \
    .pad  = { 0, },                                                           \
    .handler = {                                                              \
        .inval   = invl,                                                      \
        .type = _typ,                                                         \
        .handler = &gen_##name,                                               \
    },                                                                        \
    .oname = stringify(name),                                                 \
}
#define GEN_OPCODE2(name, onam, op1, op2, op3, invl, _typ)                    \
OPCODES_SECTION opcode_t opc_##name = {                                       \
    .opc1 = op1,                                                              \
    .opc2 = op2,                                                              \
    .opc3 = op3,                                                              \
    .pad  = { 0, },                                                           \
    .handler = {                                                              \
        .inval   = invl,                                                      \
        .type = _typ,                                                         \
        .handler = &gen_##name,                                               \
    },                                                                        \
    .oname = onam,                                                            \
}
#endif

#define GEN_OPCODE_MARK(name)                                                 \
OPCODES_SECTION opcode_t opc_##name = {                                       \
    .opc1 = 0xFF,                                                             \
    .opc2 = 0xFF,                                                             \
    .opc3 = 0xFF,                                                             \
    .pad  = { 0, },                                                           \
    .handler = {                                                              \
        .inval   = 0x00000000,                                                \
        .type = 0x00,                                                         \
        .handler = NULL,                                                      \
    },                                                                        \
    .oname = stringify(name),                                                 \
}

/* SPR load/store helpers */
static always_inline void gen_load_spr(TCGv t, int reg)
{
    tcg_gen_ld_tl(t, cpu_env, offsetof(CPUState, spr[reg]));
}

static always_inline void gen_store_spr(int reg, TCGv t)
{
    tcg_gen_st_tl(t, cpu_env, offsetof(CPUState, spr[reg]));
}

/* Start opcode list */
GEN_OPCODE_MARK(start);

/* Invalid instruction */
GEN_HANDLER(invalid, 0x00, 0x00, 0x00, 0xFFFFFFFF, PPC_NONE)
{
    gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
}

static opc_handler_t invalid_handler = {
    .inval   = 0xFFFFFFFF,
    .type    = PPC_NONE,
    .handler = gen_invalid,
};

/***                           Integer comparison                          ***/

static always_inline void gen_op_cmp(TCGv arg0, TCGv arg1, int s, int crf)
{
    int l1, l2, l3;

    tcg_gen_trunc_tl_i32(cpu_crf[crf], cpu_xer);
    tcg_gen_shri_i32(cpu_crf[crf], cpu_crf[crf], XER_SO);
    tcg_gen_andi_i32(cpu_crf[crf], cpu_crf[crf], 1);

    l1 = gen_new_label();
    l2 = gen_new_label();
    l3 = gen_new_label();
    if (s) {
        tcg_gen_brcond_tl(TCG_COND_LT, arg0, arg1, l1);
        tcg_gen_brcond_tl(TCG_COND_GT, arg0, arg1, l2);
    } else {
        tcg_gen_brcond_tl(TCG_COND_LTU, arg0, arg1, l1);
        tcg_gen_brcond_tl(TCG_COND_GTU, arg0, arg1, l2);
    }
    tcg_gen_ori_i32(cpu_crf[crf], cpu_crf[crf], 1 << CRF_EQ);
    tcg_gen_br(l3);
    gen_set_label(l1);
    tcg_gen_ori_i32(cpu_crf[crf], cpu_crf[crf], 1 << CRF_LT);
    tcg_gen_br(l3);
    gen_set_label(l2);
    tcg_gen_ori_i32(cpu_crf[crf], cpu_crf[crf], 1 << CRF_GT);
    gen_set_label(l3);
}

static always_inline void gen_op_cmpi(TCGv arg0, target_ulong arg1, int s, int crf)
{
    TCGv t0 = tcg_const_local_tl(arg1);
    gen_op_cmp(arg0, t0, s, crf);
    tcg_temp_free(t0);
}

#if defined(TARGET_PPC64)
static always_inline void gen_op_cmp32(TCGv arg0, TCGv arg1, int s, int crf)
{
    TCGv t0, t1;
    t0 = tcg_temp_local_new();
    t1 = tcg_temp_local_new();
    if (s) {
        tcg_gen_ext32s_tl(t0, arg0);
        tcg_gen_ext32s_tl(t1, arg1);
    } else {
        tcg_gen_ext32u_tl(t0, arg0);
        tcg_gen_ext32u_tl(t1, arg1);
    }
    gen_op_cmp(t0, t1, s, crf);
    tcg_temp_free(t1);
    tcg_temp_free(t0);
}

static always_inline void gen_op_cmpi32(TCGv arg0, target_ulong arg1, int s, int crf)
{
    TCGv t0 = tcg_const_local_tl(arg1);
    gen_op_cmp32(arg0, t0, s, crf);
    tcg_temp_free(t0);
}
#endif

static always_inline void gen_set_Rc0 (DisasContext *ctx, TCGv reg)
{
#if defined(TARGET_PPC64)
    if (!(ctx->sf_mode))
        gen_op_cmpi32(reg, 0, 1, 0);
    else
#endif
        gen_op_cmpi(reg, 0, 1, 0);
}

/* cmp */
GEN_HANDLER(cmp, 0x1F, 0x00, 0x00, 0x00400000, PPC_INTEGER)
{
#if defined(TARGET_PPC64)
    if (!(ctx->sf_mode && (ctx->opcode & 0x00200000)))
        gen_op_cmp32(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)],
                     1, crfD(ctx->opcode));
    else
#endif
        gen_op_cmp(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)],
                   1, crfD(ctx->opcode));
}

/* cmpi */
GEN_HANDLER(cmpi, 0x0B, 0xFF, 0xFF, 0x00400000, PPC_INTEGER)
{
#if defined(TARGET_PPC64)
    if (!(ctx->sf_mode && (ctx->opcode & 0x00200000)))
        gen_op_cmpi32(cpu_gpr[rA(ctx->opcode)], SIMM(ctx->opcode),
                      1, crfD(ctx->opcode));
    else
#endif
        gen_op_cmpi(cpu_gpr[rA(ctx->opcode)], SIMM(ctx->opcode),
                    1, crfD(ctx->opcode));
}

/* cmpl */
GEN_HANDLER(cmpl, 0x1F, 0x00, 0x01, 0x00400000, PPC_INTEGER)
{
#if defined(TARGET_PPC64)
    if (!(ctx->sf_mode && (ctx->opcode & 0x00200000)))
        gen_op_cmp32(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)],
                     0, crfD(ctx->opcode));
    else
#endif
        gen_op_cmp(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)],
                   0, crfD(ctx->opcode));
}

/* cmpli */
GEN_HANDLER(cmpli, 0x0A, 0xFF, 0xFF, 0x00400000, PPC_INTEGER)
{
#if defined(TARGET_PPC64)
    if (!(ctx->sf_mode && (ctx->opcode & 0x00200000)))
        gen_op_cmpi32(cpu_gpr[rA(ctx->opcode)], UIMM(ctx->opcode),
                      0, crfD(ctx->opcode));
    else
#endif
        gen_op_cmpi(cpu_gpr[rA(ctx->opcode)], UIMM(ctx->opcode),
                    0, crfD(ctx->opcode));
}

/* isel (PowerPC 2.03 specification) */
GEN_HANDLER(isel, 0x1F, 0x0F, 0xFF, 0x00000001, PPC_ISEL)
{
    int l1, l2;
    uint32_t bi = rC(ctx->opcode);
    uint32_t mask;
    TCGv_i32 t0;

    l1 = gen_new_label();
    l2 = gen_new_label();

    mask = 1 << (3 - (bi & 0x03));
    t0 = tcg_temp_new_i32();
    tcg_gen_andi_i32(t0, cpu_crf[bi >> 2], mask);
    tcg_gen_brcondi_i32(TCG_COND_EQ, t0, 0, l1);
    if (rA(ctx->opcode) == 0)
        tcg_gen_movi_tl(cpu_gpr[rD(ctx->opcode)], 0);
    else
        tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
    gen_set_label(l2);
    tcg_temp_free_i32(t0);
}

/***                           Integer arithmetic                          ***/

static always_inline void gen_op_arith_compute_ov(DisasContext *ctx, TCGv arg0, TCGv arg1, TCGv arg2, int sub)
{
    int l1;
    TCGv t0;

    l1 = gen_new_label();
    /* Start with XER OV disabled, the most likely case */
    tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_OV));
    t0 = tcg_temp_local_new();
    tcg_gen_xor_tl(t0, arg0, arg1);
#if defined(TARGET_PPC64)
    if (!ctx->sf_mode)
        tcg_gen_ext32s_tl(t0, t0);
#endif
    if (sub)
        tcg_gen_brcondi_tl(TCG_COND_LT, t0, 0, l1);
    else
        tcg_gen_brcondi_tl(TCG_COND_GE, t0, 0, l1);
    tcg_gen_xor_tl(t0, arg1, arg2);
#if defined(TARGET_PPC64)
    if (!ctx->sf_mode)
        tcg_gen_ext32s_tl(t0, t0);
#endif
    if (sub)
        tcg_gen_brcondi_tl(TCG_COND_GE, t0, 0, l1);
    else
        tcg_gen_brcondi_tl(TCG_COND_LT, t0, 0, l1);
    tcg_gen_ori_tl(cpu_xer, cpu_xer, (1 << XER_OV) | (1 << XER_SO));
    gen_set_label(l1);
    tcg_temp_free(t0);
}

static always_inline void gen_op_arith_compute_ca(DisasContext *ctx, TCGv arg1, TCGv arg2, int sub)
{
    int l1 = gen_new_label();

#if defined(TARGET_PPC64)
    if (!(ctx->sf_mode)) {
        TCGv t0, t1;
        t0 = tcg_temp_new();
        t1 = tcg_temp_new();

        tcg_gen_ext32u_tl(t0, arg1);
        tcg_gen_ext32u_tl(t1, arg2);
        if (sub) {
            tcg_gen_brcond_tl(TCG_COND_GTU, t0, t1, l1);
        } else {
            tcg_gen_brcond_tl(TCG_COND_GEU, t0, t1, l1);
        }
        tcg_gen_ori_tl(cpu_xer, cpu_xer, 1 << XER_CA);
        gen_set_label(l1);
        tcg_temp_free(t0);
        tcg_temp_free(t1);
    } else
#endif
    {
        if (sub) {
            tcg_gen_brcond_tl(TCG_COND_GTU, arg1, arg2, l1);
        } else {
            tcg_gen_brcond_tl(TCG_COND_GEU, arg1, arg2, l1);
        }
        tcg_gen_ori_tl(cpu_xer, cpu_xer, 1 << XER_CA);
        gen_set_label(l1);
    }
}

/* Common add function */
static always_inline void gen_op_arith_add(DisasContext *ctx, TCGv ret, TCGv arg1, TCGv arg2,
                                           int add_ca, int compute_ca, int compute_ov)
{
    TCGv t0, t1;

    if ((!compute_ca && !compute_ov) ||
        (!TCGV_EQUAL(ret,arg1) && !TCGV_EQUAL(ret, arg2)))  {
        t0 = ret;
    } else {
        t0 = tcg_temp_local_new();
    }

    if (add_ca) {
        t1 = tcg_temp_local_new();
        tcg_gen_andi_tl(t1, cpu_xer, (1 << XER_CA));
        tcg_gen_shri_tl(t1, t1, XER_CA);
    }

    if (compute_ca && compute_ov) {
        /* Start with XER CA and OV disabled, the most likely case */
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~((1 << XER_CA) | (1 << XER_OV)));
    } else if (compute_ca) {
        /* Start with XER CA disabled, the most likely case */
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_CA));
    } else if (compute_ov) {
        /* Start with XER OV disabled, the most likely case */
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_OV));
    }

    tcg_gen_add_tl(t0, arg1, arg2);

    if (compute_ca) {
        gen_op_arith_compute_ca(ctx, t0, arg1, 0);
    }
    if (add_ca) {
        tcg_gen_add_tl(t0, t0, t1);
        gen_op_arith_compute_ca(ctx, t0, t1, 0);
        tcg_temp_free(t1);
    }
    if (compute_ov) {
        gen_op_arith_compute_ov(ctx, t0, arg1, arg2, 0);
    }

    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, t0);

    if (!TCGV_EQUAL(t0, ret)) {
        tcg_gen_mov_tl(ret, t0);
        tcg_temp_free(t0);
    }
}
/* Add functions with two operands */
#define GEN_INT_ARITH_ADD(name, opc3, add_ca, compute_ca, compute_ov)         \
GEN_HANDLER(name, 0x1F, 0x0A, opc3, 0x00000000, PPC_INTEGER)                  \
{                                                                             \
    gen_op_arith_add(ctx, cpu_gpr[rD(ctx->opcode)],                           \
                     cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)],      \
                     add_ca, compute_ca, compute_ov);                         \
}
/* Add functions with one operand and one immediate */
#define GEN_INT_ARITH_ADD_CONST(name, opc3, const_val,                        \
                                add_ca, compute_ca, compute_ov)               \
GEN_HANDLER(name, 0x1F, 0x0A, opc3, 0x0000F800, PPC_INTEGER)                  \
{                                                                             \
    TCGv t0 = tcg_const_local_tl(const_val);                                  \
    gen_op_arith_add(ctx, cpu_gpr[rD(ctx->opcode)],                           \
                     cpu_gpr[rA(ctx->opcode)], t0,                            \
                     add_ca, compute_ca, compute_ov);                         \
    tcg_temp_free(t0);                                                        \
}

/* add  add.  addo  addo. */
GEN_INT_ARITH_ADD(add, 0x08, 0, 0, 0)
GEN_INT_ARITH_ADD(addo, 0x18, 0, 0, 1)
/* addc  addc.  addco  addco. */
GEN_INT_ARITH_ADD(addc, 0x00, 0, 1, 0)
GEN_INT_ARITH_ADD(addco, 0x10, 0, 1, 1)
/* adde  adde.  addeo  addeo. */
GEN_INT_ARITH_ADD(adde, 0x04, 1, 1, 0)
GEN_INT_ARITH_ADD(addeo, 0x14, 1, 1, 1)
/* addme  addme.  addmeo  addmeo.  */
GEN_INT_ARITH_ADD_CONST(addme, 0x07, -1LL, 1, 1, 0)
GEN_INT_ARITH_ADD_CONST(addmeo, 0x17, -1LL, 1, 1, 1)
/* addze  addze.  addzeo  addzeo.*/
GEN_INT_ARITH_ADD_CONST(addze, 0x06, 0, 1, 1, 0)
GEN_INT_ARITH_ADD_CONST(addzeo, 0x16, 0, 1, 1, 1)
/* addi */
GEN_HANDLER(addi, 0x0E, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    target_long simm = SIMM(ctx->opcode);

    if (rA(ctx->opcode) == 0) {
        /* li case */
        tcg_gen_movi_tl(cpu_gpr[rD(ctx->opcode)], simm);
    } else {
        tcg_gen_addi_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], simm);
    }
}
/* addic  addic.*/
static always_inline void gen_op_addic (DisasContext *ctx, TCGv ret, TCGv arg1,
                                        int compute_Rc0)
{
    target_long simm = SIMM(ctx->opcode);

    /* Start with XER CA and OV disabled, the most likely case */
    tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_CA));

    if (likely(simm != 0)) {
        TCGv t0 = tcg_temp_local_new();
        tcg_gen_addi_tl(t0, arg1, simm);
        gen_op_arith_compute_ca(ctx, t0, arg1, 0);
        tcg_gen_mov_tl(ret, t0);
        tcg_temp_free(t0);
    } else {
        tcg_gen_mov_tl(ret, arg1);
    }
    if (compute_Rc0) {
        gen_set_Rc0(ctx, ret);
    }
}
GEN_HANDLER(addic, 0x0C, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    gen_op_addic(ctx, cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], 0);
}
GEN_HANDLER2(addic_, "addic.", 0x0D, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    gen_op_addic(ctx, cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], 1);
}
/* addis */
GEN_HANDLER(addis, 0x0F, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    target_long simm = SIMM(ctx->opcode);

    if (rA(ctx->opcode) == 0) {
        /* lis case */
        tcg_gen_movi_tl(cpu_gpr[rD(ctx->opcode)], simm << 16);
    } else {
        tcg_gen_addi_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], simm << 16);
    }
}

static always_inline void gen_op_arith_divw (DisasContext *ctx, TCGv ret, TCGv arg1, TCGv arg2,
                                             int sign, int compute_ov)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    TCGv_i32 t0 = tcg_temp_local_new_i32();
    TCGv_i32 t1 = tcg_temp_local_new_i32();

    tcg_gen_trunc_tl_i32(t0, arg1);
    tcg_gen_trunc_tl_i32(t1, arg2);
    tcg_gen_brcondi_i32(TCG_COND_EQ, t1, 0, l1);
    if (sign) {
        int l3 = gen_new_label();
        tcg_gen_brcondi_i32(TCG_COND_NE, t1, -1, l3);
        tcg_gen_brcondi_i32(TCG_COND_EQ, t0, INT32_MIN, l1);
        gen_set_label(l3);
        tcg_gen_div_i32(t0, t0, t1);
    } else {
        tcg_gen_divu_i32(t0, t0, t1);
    }
    if (compute_ov) {
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_OV));
    }
    tcg_gen_br(l2);
    gen_set_label(l1);
    if (sign) {
        tcg_gen_sari_i32(t0, t0, 31);
    } else {
        tcg_gen_movi_i32(t0, 0);
    }
    if (compute_ov) {
        tcg_gen_ori_tl(cpu_xer, cpu_xer, (1 << XER_OV) | (1 << XER_SO));
    }
    gen_set_label(l2);
    tcg_gen_extu_i32_tl(ret, t0);
    tcg_temp_free_i32(t0);
    tcg_temp_free_i32(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, ret);
}
/* Div functions */
#define GEN_INT_ARITH_DIVW(name, opc3, sign, compute_ov)                      \
GEN_HANDLER(name, 0x1F, 0x0B, opc3, 0x00000000, PPC_INTEGER)                  \
{                                                                             \
    gen_op_arith_divw(ctx, cpu_gpr[rD(ctx->opcode)],                          \
                     cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)],      \
                     sign, compute_ov);                                       \
}
/* divwu  divwu.  divwuo  divwuo.   */
GEN_INT_ARITH_DIVW(divwu, 0x0E, 0, 0);
GEN_INT_ARITH_DIVW(divwuo, 0x1E, 0, 1);
/* divw  divw.  divwo  divwo.   */
GEN_INT_ARITH_DIVW(divw, 0x0F, 1, 0);
GEN_INT_ARITH_DIVW(divwo, 0x1F, 1, 1);
#if defined(TARGET_PPC64)
static always_inline void gen_op_arith_divd (DisasContext *ctx, TCGv ret, TCGv arg1, TCGv arg2,
                                             int sign, int compute_ov)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();

    tcg_gen_brcondi_i64(TCG_COND_EQ, arg2, 0, l1);
    if (sign) {
        int l3 = gen_new_label();
        tcg_gen_brcondi_i64(TCG_COND_NE, arg2, -1, l3);
        tcg_gen_brcondi_i64(TCG_COND_EQ, arg1, INT64_MIN, l1);
        gen_set_label(l3);
        tcg_gen_div_i64(ret, arg1, arg2);
    } else {
        tcg_gen_divu_i64(ret, arg1, arg2);
    }
    if (compute_ov) {
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_OV));
    }
    tcg_gen_br(l2);
    gen_set_label(l1);
    if (sign) {
        tcg_gen_sari_i64(ret, arg1, 63);
    } else {
        tcg_gen_movi_i64(ret, 0);
    }
    if (compute_ov) {
        tcg_gen_ori_tl(cpu_xer, cpu_xer, (1 << XER_OV) | (1 << XER_SO));
    }
    gen_set_label(l2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, ret);
}
#define GEN_INT_ARITH_DIVD(name, opc3, sign, compute_ov)                      \
GEN_HANDLER(name, 0x1F, 0x09, opc3, 0x00000000, PPC_64B)                      \
{                                                                             \
    gen_op_arith_divd(ctx, cpu_gpr[rD(ctx->opcode)],                          \
                      cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)],     \
                      sign, compute_ov);                                      \
}
/* divwu  divwu.  divwuo  divwuo.   */
GEN_INT_ARITH_DIVD(divdu, 0x0E, 0, 0);
GEN_INT_ARITH_DIVD(divduo, 0x1E, 0, 1);
/* divw  divw.  divwo  divwo.   */
GEN_INT_ARITH_DIVD(divd, 0x0F, 1, 0);
GEN_INT_ARITH_DIVD(divdo, 0x1F, 1, 1);
#endif

/* mulhw  mulhw. */
GEN_HANDLER(mulhw, 0x1F, 0x0B, 0x02, 0x00000400, PPC_INTEGER)
{
    TCGv_i64 t0, t1;

    t0 = tcg_temp_new_i64();
    t1 = tcg_temp_new_i64();
#if defined(TARGET_PPC64)
    tcg_gen_ext32s_tl(t0, cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_ext32s_tl(t1, cpu_gpr[rB(ctx->opcode)]);
    tcg_gen_mul_i64(t0, t0, t1);
    tcg_gen_shri_i64(cpu_gpr[rD(ctx->opcode)], t0, 32);
#else
    tcg_gen_ext_tl_i64(t0, cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_ext_tl_i64(t1, cpu_gpr[rB(ctx->opcode)]);
    tcg_gen_mul_i64(t0, t0, t1);
    tcg_gen_shri_i64(t0, t0, 32);
    tcg_gen_trunc_i64_tl(cpu_gpr[rD(ctx->opcode)], t0);
#endif
    tcg_temp_free_i64(t0);
    tcg_temp_free_i64(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}
/* mulhwu  mulhwu.  */
GEN_HANDLER(mulhwu, 0x1F, 0x0B, 0x00, 0x00000400, PPC_INTEGER)
{
    TCGv_i64 t0, t1;

    t0 = tcg_temp_new_i64();
    t1 = tcg_temp_new_i64();
#if defined(TARGET_PPC64)
    tcg_gen_ext32u_i64(t0, cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_ext32u_i64(t1, cpu_gpr[rB(ctx->opcode)]);
    tcg_gen_mul_i64(t0, t0, t1);
    tcg_gen_shri_i64(cpu_gpr[rD(ctx->opcode)], t0, 32);
#else
    tcg_gen_extu_tl_i64(t0, cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_extu_tl_i64(t1, cpu_gpr[rB(ctx->opcode)]);
    tcg_gen_mul_i64(t0, t0, t1);
    tcg_gen_shri_i64(t0, t0, 32);
    tcg_gen_trunc_i64_tl(cpu_gpr[rD(ctx->opcode)], t0);
#endif
    tcg_temp_free_i64(t0);
    tcg_temp_free_i64(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}
/* mullw  mullw. */
GEN_HANDLER(mullw, 0x1F, 0x0B, 0x07, 0x00000000, PPC_INTEGER)
{
    tcg_gen_mul_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)],
                   cpu_gpr[rB(ctx->opcode)]);
    tcg_gen_ext32s_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)]);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}
/* mullwo  mullwo. */
GEN_HANDLER(mullwo, 0x1F, 0x0B, 0x17, 0x00000000, PPC_INTEGER)
{
    int l1;
    TCGv_i64 t0, t1;

    t0 = tcg_temp_new_i64();
    t1 = tcg_temp_new_i64();
    l1 = gen_new_label();
    /* Start with XER OV disabled, the most likely case */
    tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_OV));
#if defined(TARGET_PPC64)
    tcg_gen_ext32s_i64(t0, cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_ext32s_i64(t1, cpu_gpr[rB(ctx->opcode)]);
#else
    tcg_gen_ext_tl_i64(t0, cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_ext_tl_i64(t1, cpu_gpr[rB(ctx->opcode)]);
#endif
    tcg_gen_mul_i64(t0, t0, t1);
#if defined(TARGET_PPC64)
    tcg_gen_ext32s_i64(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_gen_brcond_i64(TCG_COND_EQ, t0, cpu_gpr[rD(ctx->opcode)], l1);
#else
    tcg_gen_trunc_i64_tl(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_gen_ext32s_i64(t1, t0);
    tcg_gen_brcond_i64(TCG_COND_EQ, t0, t1, l1);
#endif
    tcg_gen_ori_tl(cpu_xer, cpu_xer, (1 << XER_OV) | (1 << XER_SO));
    gen_set_label(l1);
    tcg_temp_free_i64(t0);
    tcg_temp_free_i64(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}
/* mulli */
GEN_HANDLER(mulli, 0x07, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    tcg_gen_muli_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)],
                    SIMM(ctx->opcode));
}
#if defined(TARGET_PPC64)
#define GEN_INT_ARITH_MUL_HELPER(name, opc3)                                  \
GEN_HANDLER(name, 0x1F, 0x09, opc3, 0x00000000, PPC_64B)                      \
{                                                                             \
    gen_helper_##name (cpu_gpr[rD(ctx->opcode)],                              \
                       cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);   \
    if (unlikely(Rc(ctx->opcode) != 0))                                       \
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);                           \
}
/* mulhd  mulhd. */
GEN_INT_ARITH_MUL_HELPER(mulhdu, 0x00);
/* mulhdu  mulhdu. */
GEN_INT_ARITH_MUL_HELPER(mulhd, 0x02);
/* mulld  mulld. */
GEN_HANDLER(mulld, 0x1F, 0x09, 0x07, 0x00000000, PPC_64B)
{
    tcg_gen_mul_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)],
                   cpu_gpr[rB(ctx->opcode)]);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}
/* mulldo  mulldo. */
GEN_INT_ARITH_MUL_HELPER(mulldo, 0x17);
#endif

/* neg neg. nego nego. */
static always_inline void gen_op_arith_neg (DisasContext *ctx, TCGv ret, TCGv arg1, int ov_check)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    TCGv t0 = tcg_temp_local_new();
#if defined(TARGET_PPC64)
    if (ctx->sf_mode) {
        tcg_gen_mov_tl(t0, arg1);
        tcg_gen_brcondi_tl(TCG_COND_EQ, t0, INT64_MIN, l1);
    } else
#endif
    {
        tcg_gen_ext32s_tl(t0, arg1);
        tcg_gen_brcondi_tl(TCG_COND_EQ, t0, INT32_MIN, l1);
    }
    tcg_gen_neg_tl(ret, arg1);
    if (ov_check) {
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_OV));
    }
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_mov_tl(ret, t0);
    if (ov_check) {
        tcg_gen_ori_tl(cpu_xer, cpu_xer, (1 << XER_OV) | (1 << XER_SO));
    }
    gen_set_label(l2);
    tcg_temp_free(t0);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, ret);
}
GEN_HANDLER(neg, 0x1F, 0x08, 0x03, 0x0000F800, PPC_INTEGER)
{
    gen_op_arith_neg(ctx, cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], 0);
}
GEN_HANDLER(nego, 0x1F, 0x08, 0x13, 0x0000F800, PPC_INTEGER)
{
    gen_op_arith_neg(ctx, cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], 1);
}

/* Common subf function */
static always_inline void gen_op_arith_subf(DisasContext *ctx, TCGv ret, TCGv arg1, TCGv arg2,
                                            int add_ca, int compute_ca, int compute_ov)
{
    TCGv t0, t1;

    if ((!compute_ca && !compute_ov) ||
        (!TCGV_EQUAL(ret, arg1) && !TCGV_EQUAL(ret, arg2)))  {
        t0 = ret;
    } else {
        t0 = tcg_temp_local_new();
    }

    if (add_ca) {
        t1 = tcg_temp_local_new();
        tcg_gen_andi_tl(t1, cpu_xer, (1 << XER_CA));
        tcg_gen_shri_tl(t1, t1, XER_CA);
    }

    if (compute_ca && compute_ov) {
        /* Start with XER CA and OV disabled, the most likely case */
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~((1 << XER_CA) | (1 << XER_OV)));
    } else if (compute_ca) {
        /* Start with XER CA disabled, the most likely case */
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_CA));
    } else if (compute_ov) {
        /* Start with XER OV disabled, the most likely case */
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_OV));
    }

    if (add_ca) {
        tcg_gen_not_tl(t0, arg1);
        tcg_gen_add_tl(t0, t0, arg2);
        gen_op_arith_compute_ca(ctx, t0, arg2, 0);
        tcg_gen_add_tl(t0, t0, t1);
        gen_op_arith_compute_ca(ctx, t0, t1, 0);
        tcg_temp_free(t1);
    } else {
        tcg_gen_sub_tl(t0, arg2, arg1);
        if (compute_ca) {
            gen_op_arith_compute_ca(ctx, t0, arg2, 1);
        }
    }
    if (compute_ov) {
        gen_op_arith_compute_ov(ctx, t0, arg1, arg2, 1);
    }

    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, t0);

    if (!TCGV_EQUAL(t0, ret)) {
        tcg_gen_mov_tl(ret, t0);
        tcg_temp_free(t0);
    }
}
/* Sub functions with Two operands functions */
#define GEN_INT_ARITH_SUBF(name, opc3, add_ca, compute_ca, compute_ov)        \
GEN_HANDLER(name, 0x1F, 0x08, opc3, 0x00000000, PPC_INTEGER)                  \
{                                                                             \
    gen_op_arith_subf(ctx, cpu_gpr[rD(ctx->opcode)],                          \
                      cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)],     \
                      add_ca, compute_ca, compute_ov);                        \
}
/* Sub functions with one operand and one immediate */
#define GEN_INT_ARITH_SUBF_CONST(name, opc3, const_val,                       \
                                add_ca, compute_ca, compute_ov)               \
GEN_HANDLER(name, 0x1F, 0x08, opc3, 0x0000F800, PPC_INTEGER)                  \
{                                                                             \
    TCGv t0 = tcg_const_local_tl(const_val);                                  \
    gen_op_arith_subf(ctx, cpu_gpr[rD(ctx->opcode)],                          \
                      cpu_gpr[rA(ctx->opcode)], t0,                           \
                      add_ca, compute_ca, compute_ov);                        \
    tcg_temp_free(t0);                                                        \
}
/* subf  subf.  subfo  subfo. */
GEN_INT_ARITH_SUBF(subf, 0x01, 0, 0, 0)
GEN_INT_ARITH_SUBF(subfo, 0x11, 0, 0, 1)
/* subfc  subfc.  subfco  subfco. */
GEN_INT_ARITH_SUBF(subfc, 0x00, 0, 1, 0)
GEN_INT_ARITH_SUBF(subfco, 0x10, 0, 1, 1)
/* subfe  subfe.  subfeo  subfo. */
GEN_INT_ARITH_SUBF(subfe, 0x04, 1, 1, 0)
GEN_INT_ARITH_SUBF(subfeo, 0x14, 1, 1, 1)
/* subfme  subfme.  subfmeo  subfmeo.  */
GEN_INT_ARITH_SUBF_CONST(subfme, 0x07, -1LL, 1, 1, 0)
GEN_INT_ARITH_SUBF_CONST(subfmeo, 0x17, -1LL, 1, 1, 1)
/* subfze  subfze.  subfzeo  subfzeo.*/
GEN_INT_ARITH_SUBF_CONST(subfze, 0x06, 0, 1, 1, 0)
GEN_INT_ARITH_SUBF_CONST(subfzeo, 0x16, 0, 1, 1, 1)
/* subfic */
GEN_HANDLER(subfic, 0x08, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    /* Start with XER CA and OV disabled, the most likely case */
    tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_CA));
    TCGv t0 = tcg_temp_local_new();
    TCGv t1 = tcg_const_local_tl(SIMM(ctx->opcode));
    tcg_gen_sub_tl(t0, t1, cpu_gpr[rA(ctx->opcode)]);
    gen_op_arith_compute_ca(ctx, t0, t1, 1);
    tcg_temp_free(t1);
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
}

/***                            Integer logical                            ***/
#define GEN_LOGICAL2(name, tcg_op, opc, type)                                 \
GEN_HANDLER(name, 0x1F, 0x1C, opc, 0x00000000, type)                          \
{                                                                             \
    tcg_op(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)],                \
       cpu_gpr[rB(ctx->opcode)]);                                             \
    if (unlikely(Rc(ctx->opcode) != 0))                                       \
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);                           \
}

#define GEN_LOGICAL1(name, tcg_op, opc, type)                                 \
GEN_HANDLER(name, 0x1F, 0x1A, opc, 0x00000000, type)                          \
{                                                                             \
    tcg_op(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);               \
    if (unlikely(Rc(ctx->opcode) != 0))                                       \
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);                           \
}

/* and & and. */
GEN_LOGICAL2(and, tcg_gen_and_tl, 0x00, PPC_INTEGER);
/* andc & andc. */
GEN_LOGICAL2(andc, tcg_gen_andc_tl, 0x01, PPC_INTEGER);
/* andi. */
GEN_HANDLER2(andi_, "andi.", 0x1C, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    tcg_gen_andi_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], UIMM(ctx->opcode));
    gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
/* andis. */
GEN_HANDLER2(andis_, "andis.", 0x1D, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    tcg_gen_andi_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], UIMM(ctx->opcode) << 16);
    gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
/* cntlzw */
GEN_HANDLER(cntlzw, 0x1F, 0x1A, 0x00, 0x00000000, PPC_INTEGER)
{
    gen_helper_cntlzw(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
/* eqv & eqv. */
GEN_LOGICAL2(eqv, tcg_gen_eqv_tl, 0x08, PPC_INTEGER);
/* extsb & extsb. */
GEN_LOGICAL1(extsb, tcg_gen_ext8s_tl, 0x1D, PPC_INTEGER);
/* extsh & extsh. */
GEN_LOGICAL1(extsh, tcg_gen_ext16s_tl, 0x1C, PPC_INTEGER);
/* nand & nand. */
GEN_LOGICAL2(nand, tcg_gen_nand_tl, 0x0E, PPC_INTEGER);
/* nor & nor. */
GEN_LOGICAL2(nor, tcg_gen_nor_tl, 0x03, PPC_INTEGER);
/* or & or. */
GEN_HANDLER(or, 0x1F, 0x1C, 0x0D, 0x00000000, PPC_INTEGER)
{
    int rs, ra, rb;

    rs = rS(ctx->opcode);
    ra = rA(ctx->opcode);
    rb = rB(ctx->opcode);
    /* Optimisation for mr. ri case */
    if (rs != ra || rs != rb) {
        if (rs != rb)
            tcg_gen_or_tl(cpu_gpr[ra], cpu_gpr[rs], cpu_gpr[rb]);
        else
            tcg_gen_mov_tl(cpu_gpr[ra], cpu_gpr[rs]);
        if (unlikely(Rc(ctx->opcode) != 0))
            gen_set_Rc0(ctx, cpu_gpr[ra]);
    } else if (unlikely(Rc(ctx->opcode) != 0)) {
        gen_set_Rc0(ctx, cpu_gpr[rs]);
#if defined(TARGET_PPC64)
    } else {
        int prio = 0;

        switch (rs) {
        case 1:
            /* Set process priority to low */
            prio = 2;
            break;
        case 6:
            /* Set process priority to medium-low */
            prio = 3;
            break;
        case 2:
            /* Set process priority to normal */
            prio = 4;
            break;
#if !defined(CONFIG_USER_ONLY)
        case 31:
            if (ctx->mem_idx > 0) {
                /* Set process priority to very low */
                prio = 1;
            }
            break;
        case 5:
            if (ctx->mem_idx > 0) {
                /* Set process priority to medium-hight */
                prio = 5;
            }
            break;
        case 3:
            if (ctx->mem_idx > 0) {
                /* Set process priority to high */
                prio = 6;
            }
            break;
        case 7:
            if (ctx->mem_idx > 1) {
                /* Set process priority to very high */
                prio = 7;
            }
            break;
#endif
        default:
            /* nop */
            break;
        }
        if (prio) {
            TCGv t0 = tcg_temp_new();
            gen_load_spr(t0, SPR_PPR);
            tcg_gen_andi_tl(t0, t0, ~0x001C000000000000ULL);
            tcg_gen_ori_tl(t0, t0, ((uint64_t)prio) << 50);
            gen_store_spr(SPR_PPR, t0);
            tcg_temp_free(t0);
        }
#endif
    }
}
/* orc & orc. */
GEN_LOGICAL2(orc, tcg_gen_orc_tl, 0x0C, PPC_INTEGER);
/* xor & xor. */
GEN_HANDLER(xor, 0x1F, 0x1C, 0x09, 0x00000000, PPC_INTEGER)
{
    /* Optimisation for "set to zero" case */
    if (rS(ctx->opcode) != rB(ctx->opcode))
        tcg_gen_xor_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
    else
        tcg_gen_movi_tl(cpu_gpr[rA(ctx->opcode)], 0);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
/* ori */
GEN_HANDLER(ori, 0x18, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    target_ulong uimm = UIMM(ctx->opcode);

    if (rS(ctx->opcode) == rA(ctx->opcode) && uimm == 0) {
        /* NOP */
        /* XXX: should handle special NOPs for POWER series */
        return;
    }
    tcg_gen_ori_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], uimm);
}
/* oris */
GEN_HANDLER(oris, 0x19, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    target_ulong uimm = UIMM(ctx->opcode);

    if (rS(ctx->opcode) == rA(ctx->opcode) && uimm == 0) {
        /* NOP */
        return;
    }
    tcg_gen_ori_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], uimm << 16);
}
/* xori */
GEN_HANDLER(xori, 0x1A, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    target_ulong uimm = UIMM(ctx->opcode);

    if (rS(ctx->opcode) == rA(ctx->opcode) && uimm == 0) {
        /* NOP */
        return;
    }
    tcg_gen_xori_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], uimm);
}
/* xoris */
GEN_HANDLER(xoris, 0x1B, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    target_ulong uimm = UIMM(ctx->opcode);

    if (rS(ctx->opcode) == rA(ctx->opcode) && uimm == 0) {
        /* NOP */
        return;
    }
    tcg_gen_xori_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], uimm << 16);
}
/* popcntb : PowerPC 2.03 specification */
GEN_HANDLER(popcntb, 0x1F, 0x03, 0x03, 0x0000F801, PPC_POPCNTB)
{
#if defined(TARGET_PPC64)
    if (ctx->sf_mode)
        gen_helper_popcntb_64(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
    else
#endif
        gen_helper_popcntb(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
}

#if defined(TARGET_PPC64)
/* extsw & extsw. */
GEN_LOGICAL1(extsw, tcg_gen_ext32s_tl, 0x1E, PPC_64B);
/* cntlzd */
GEN_HANDLER(cntlzd, 0x1F, 0x1A, 0x01, 0x00000000, PPC_64B)
{
    gen_helper_cntlzd(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
#endif

/***                             Integer rotate                            ***/
/* rlwimi & rlwimi. */
GEN_HANDLER(rlwimi, 0x14, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    uint32_t mb, me, sh;

    mb = MB(ctx->opcode);
    me = ME(ctx->opcode);
    sh = SH(ctx->opcode);
    if (likely(sh == 0 && mb == 0 && me == 31)) {
        tcg_gen_ext32u_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
    } else {
        target_ulong mask;
        TCGv t1;
        TCGv t0 = tcg_temp_new();
#if defined(TARGET_PPC64)
        TCGv_i32 t2 = tcg_temp_new_i32();
        tcg_gen_trunc_i64_i32(t2, cpu_gpr[rS(ctx->opcode)]);
        tcg_gen_rotli_i32(t2, t2, sh);
        tcg_gen_extu_i32_i64(t0, t2);
        tcg_temp_free_i32(t2);
#else
        tcg_gen_rotli_i32(t0, cpu_gpr[rS(ctx->opcode)], sh);
#endif
#if defined(TARGET_PPC64)
        mb += 32;
        me += 32;
#endif
        mask = MASK(mb, me);
        t1 = tcg_temp_new();
        tcg_gen_andi_tl(t0, t0, mask);
        tcg_gen_andi_tl(t1, cpu_gpr[rA(ctx->opcode)], ~mask);
        tcg_gen_or_tl(cpu_gpr[rA(ctx->opcode)], t0, t1);
        tcg_temp_free(t0);
        tcg_temp_free(t1);
    }
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
/* rlwinm & rlwinm. */
GEN_HANDLER(rlwinm, 0x15, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    uint32_t mb, me, sh;

    sh = SH(ctx->opcode);
    mb = MB(ctx->opcode);
    me = ME(ctx->opcode);

    if (likely(mb == 0 && me == (31 - sh))) {
        if (likely(sh == 0)) {
            tcg_gen_ext32u_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
        } else {
            TCGv t0 = tcg_temp_new();
            tcg_gen_ext32u_tl(t0, cpu_gpr[rS(ctx->opcode)]);
            tcg_gen_shli_tl(t0, t0, sh);
            tcg_gen_ext32u_tl(cpu_gpr[rA(ctx->opcode)], t0);
            tcg_temp_free(t0);
        }
    } else if (likely(sh != 0 && me == 31 && sh == (32 - mb))) {
        TCGv t0 = tcg_temp_new();
        tcg_gen_ext32u_tl(t0, cpu_gpr[rS(ctx->opcode)]);
        tcg_gen_shri_tl(t0, t0, mb);
        tcg_gen_ext32u_tl(cpu_gpr[rA(ctx->opcode)], t0);
        tcg_temp_free(t0);
    } else {
        TCGv t0 = tcg_temp_new();
#if defined(TARGET_PPC64)
        TCGv_i32 t1 = tcg_temp_new_i32();
        tcg_gen_trunc_i64_i32(t1, cpu_gpr[rS(ctx->opcode)]);
        tcg_gen_rotli_i32(t1, t1, sh);
        tcg_gen_extu_i32_i64(t0, t1);
        tcg_temp_free_i32(t1);
#else
        tcg_gen_rotli_i32(t0, cpu_gpr[rS(ctx->opcode)], sh);
#endif
#if defined(TARGET_PPC64)
        mb += 32;
        me += 32;
#endif
        tcg_gen_andi_tl(cpu_gpr[rA(ctx->opcode)], t0, MASK(mb, me));
        tcg_temp_free(t0);
    }
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
/* rlwnm & rlwnm. */
GEN_HANDLER(rlwnm, 0x17, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    uint32_t mb, me;
    TCGv t0;
#if defined(TARGET_PPC64)
    TCGv_i32 t1, t2;
#endif

    mb = MB(ctx->opcode);
    me = ME(ctx->opcode);
    t0 = tcg_temp_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x1f);
#if defined(TARGET_PPC64)
    t1 = tcg_temp_new_i32();
    t2 = tcg_temp_new_i32();
    tcg_gen_trunc_i64_i32(t1, cpu_gpr[rS(ctx->opcode)]);
    tcg_gen_trunc_i64_i32(t2, t0);
    tcg_gen_rotl_i32(t1, t1, t2);
    tcg_gen_extu_i32_i64(t0, t1);
    tcg_temp_free_i32(t1);
    tcg_temp_free_i32(t2);
#else
    tcg_gen_rotl_i32(t0, cpu_gpr[rS(ctx->opcode)], t0);
#endif
    if (unlikely(mb != 0 || me != 31)) {
#if defined(TARGET_PPC64)
        mb += 32;
        me += 32;
#endif
        tcg_gen_andi_tl(cpu_gpr[rA(ctx->opcode)], t0, MASK(mb, me));
    } else {
        tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], t0);
    }
    tcg_temp_free(t0);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

#if defined(TARGET_PPC64)
#define GEN_PPC64_R2(name, opc1, opc2)                                        \
GEN_HANDLER2(name##0, stringify(name), opc1, opc2, 0xFF, 0x00000000, PPC_64B) \
{                                                                             \
    gen_##name(ctx, 0);                                                       \
}                                                                             \
GEN_HANDLER2(name##1, stringify(name), opc1, opc2 | 0x10, 0xFF, 0x00000000,   \
             PPC_64B)                                                         \
{                                                                             \
    gen_##name(ctx, 1);                                                       \
}
#define GEN_PPC64_R4(name, opc1, opc2)                                        \
GEN_HANDLER2(name##0, stringify(name), opc1, opc2, 0xFF, 0x00000000, PPC_64B) \
{                                                                             \
    gen_##name(ctx, 0, 0);                                                    \
}                                                                             \
GEN_HANDLER2(name##1, stringify(name), opc1, opc2 | 0x01, 0xFF, 0x00000000,   \
             PPC_64B)                                                         \
{                                                                             \
    gen_##name(ctx, 0, 1);                                                    \
}                                                                             \
GEN_HANDLER2(name##2, stringify(name), opc1, opc2 | 0x10, 0xFF, 0x00000000,   \
             PPC_64B)                                                         \
{                                                                             \
    gen_##name(ctx, 1, 0);                                                    \
}                                                                             \
GEN_HANDLER2(name##3, stringify(name), opc1, opc2 | 0x11, 0xFF, 0x00000000,   \
             PPC_64B)                                                         \
{                                                                             \
    gen_##name(ctx, 1, 1);                                                    \
}

static always_inline void gen_rldinm (DisasContext *ctx, uint32_t mb,
                                      uint32_t me, uint32_t sh)
{
    if (likely(sh != 0 && mb == 0 && me == (63 - sh))) {
        tcg_gen_shli_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], sh);
    } else if (likely(sh != 0 && me == 63 && sh == (64 - mb))) {
        tcg_gen_shri_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], mb);
    } else {
        TCGv t0 = tcg_temp_new();
        tcg_gen_rotli_tl(t0, cpu_gpr[rS(ctx->opcode)], sh);
        if (likely(mb == 0 && me == 63)) {
            tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], t0);
        } else {
            tcg_gen_andi_tl(cpu_gpr[rA(ctx->opcode)], t0, MASK(mb, me));
        }
        tcg_temp_free(t0);
    }
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
/* rldicl - rldicl. */
static always_inline void gen_rldicl (DisasContext *ctx, int mbn, int shn)
{
    uint32_t sh, mb;

    sh = SH(ctx->opcode) | (shn << 5);
    mb = MB(ctx->opcode) | (mbn << 5);
    gen_rldinm(ctx, mb, 63, sh);
}
GEN_PPC64_R4(rldicl, 0x1E, 0x00);
/* rldicr - rldicr. */
static always_inline void gen_rldicr (DisasContext *ctx, int men, int shn)
{
    uint32_t sh, me;

    sh = SH(ctx->opcode) | (shn << 5);
    me = MB(ctx->opcode) | (men << 5);
    gen_rldinm(ctx, 0, me, sh);
}
GEN_PPC64_R4(rldicr, 0x1E, 0x02);
/* rldic - rldic. */
static always_inline void gen_rldic (DisasContext *ctx, int mbn, int shn)
{
    uint32_t sh, mb;

    sh = SH(ctx->opcode) | (shn << 5);
    mb = MB(ctx->opcode) | (mbn << 5);
    gen_rldinm(ctx, mb, 63 - sh, sh);
}
GEN_PPC64_R4(rldic, 0x1E, 0x04);

static always_inline void gen_rldnm (DisasContext *ctx, uint32_t mb,
                                     uint32_t me)
{
    TCGv t0;

    mb = MB(ctx->opcode);
    me = ME(ctx->opcode);
    t0 = tcg_temp_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x3f);
    tcg_gen_rotl_tl(t0, cpu_gpr[rS(ctx->opcode)], t0);
    if (unlikely(mb != 0 || me != 63)) {
        tcg_gen_andi_tl(cpu_gpr[rA(ctx->opcode)], t0, MASK(mb, me));
    } else {
        tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], t0);
    }
    tcg_temp_free(t0);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* rldcl - rldcl. */
static always_inline void gen_rldcl (DisasContext *ctx, int mbn)
{
    uint32_t mb;

    mb = MB(ctx->opcode) | (mbn << 5);
    gen_rldnm(ctx, mb, 63);
}
GEN_PPC64_R2(rldcl, 0x1E, 0x08);
/* rldcr - rldcr. */
static always_inline void gen_rldcr (DisasContext *ctx, int men)
{
    uint32_t me;

    me = MB(ctx->opcode) | (men << 5);
    gen_rldnm(ctx, 0, me);
}
GEN_PPC64_R2(rldcr, 0x1E, 0x09);
/* rldimi - rldimi. */
static always_inline void gen_rldimi (DisasContext *ctx, int mbn, int shn)
{
    uint32_t sh, mb, me;

    sh = SH(ctx->opcode) | (shn << 5);
    mb = MB(ctx->opcode) | (mbn << 5);
    me = 63 - sh;
    if (unlikely(sh == 0 && mb == 0)) {
        tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
    } else {
        TCGv t0, t1;
        target_ulong mask;

        t0 = tcg_temp_new();
        tcg_gen_rotli_tl(t0, cpu_gpr[rS(ctx->opcode)], sh);
        t1 = tcg_temp_new();
        mask = MASK(mb, me);
        tcg_gen_andi_tl(t0, t0, mask);
        tcg_gen_andi_tl(t1, cpu_gpr[rA(ctx->opcode)], ~mask);
        tcg_gen_or_tl(cpu_gpr[rA(ctx->opcode)], t0, t1);
        tcg_temp_free(t0);
        tcg_temp_free(t1);
    }
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
GEN_PPC64_R4(rldimi, 0x1E, 0x06);
#endif

/***                             Integer shift                             ***/
/* slw & slw. */
GEN_HANDLER(slw, 0x1F, 0x18, 0x00, 0x00000000, PPC_INTEGER)
{
    TCGv t0;
    int l1, l2;
    l1 = gen_new_label();
    l2 = gen_new_label();

    t0 = tcg_temp_local_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x3f);
    tcg_gen_brcondi_tl(TCG_COND_LT, t0, 0x20, l1);
    tcg_gen_movi_tl(cpu_gpr[rA(ctx->opcode)], 0);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_shl_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], t0);
    tcg_gen_ext32u_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    gen_set_label(l2);
    tcg_temp_free(t0);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
/* sraw & sraw. */
GEN_HANDLER(sraw, 0x1F, 0x18, 0x18, 0x00000000, PPC_INTEGER)
{
    gen_helper_sraw(cpu_gpr[rA(ctx->opcode)],
                    cpu_gpr[rS(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
/* srawi & srawi. */
GEN_HANDLER(srawi, 0x1F, 0x18, 0x19, 0x00000000, PPC_INTEGER)
{
    int sh = SH(ctx->opcode);
    if (sh != 0) {
        int l1, l2;
        TCGv t0;
        l1 = gen_new_label();
        l2 = gen_new_label();
        t0 = tcg_temp_local_new();
        tcg_gen_ext32s_tl(t0, cpu_gpr[rS(ctx->opcode)]);
        tcg_gen_brcondi_tl(TCG_COND_GE, t0, 0, l1);
        tcg_gen_andi_tl(t0, cpu_gpr[rS(ctx->opcode)], (1ULL << sh) - 1);
        tcg_gen_brcondi_tl(TCG_COND_EQ, t0, 0, l1);
        tcg_gen_ori_tl(cpu_xer, cpu_xer, 1 << XER_CA);
        tcg_gen_br(l2);
        gen_set_label(l1);
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_CA));
        gen_set_label(l2);
        tcg_gen_ext32s_tl(t0, cpu_gpr[rS(ctx->opcode)]);
        tcg_gen_sari_tl(cpu_gpr[rA(ctx->opcode)], t0, sh);
        tcg_temp_free(t0);
    } else {
        tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_CA));
    }
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
/* srw & srw. */
GEN_HANDLER(srw, 0x1F, 0x18, 0x10, 0x00000000, PPC_INTEGER)
{
    TCGv t0, t1;
    int l1, l2;
    l1 = gen_new_label();
    l2 = gen_new_label();

    t0 = tcg_temp_local_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x3f);
    tcg_gen_brcondi_tl(TCG_COND_LT, t0, 0x20, l1);
    tcg_gen_movi_tl(cpu_gpr[rA(ctx->opcode)], 0);
    tcg_gen_br(l2);
    gen_set_label(l1);
    t1 = tcg_temp_new();
    tcg_gen_ext32u_tl(t1, cpu_gpr[rS(ctx->opcode)]);
    tcg_gen_shr_tl(cpu_gpr[rA(ctx->opcode)], t1, t0);
    tcg_temp_free(t1);
    gen_set_label(l2);
    tcg_temp_free(t0);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
#if defined(TARGET_PPC64)
/* sld & sld. */
GEN_HANDLER(sld, 0x1F, 0x1B, 0x00, 0x00000000, PPC_64B)
{
    TCGv t0;
    int l1, l2;
    l1 = gen_new_label();
    l2 = gen_new_label();

    t0 = tcg_temp_local_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x7f);
    tcg_gen_brcondi_tl(TCG_COND_LT, t0, 0x40, l1);
    tcg_gen_movi_tl(cpu_gpr[rA(ctx->opcode)], 0);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_shl_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], t0);
    gen_set_label(l2);
    tcg_temp_free(t0);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
/* srad & srad. */
GEN_HANDLER(srad, 0x1F, 0x1A, 0x18, 0x00000000, PPC_64B)
{
    gen_helper_srad(cpu_gpr[rA(ctx->opcode)],
                    cpu_gpr[rS(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
/* sradi & sradi. */
static always_inline void gen_sradi (DisasContext *ctx, int n)
{
    int sh = SH(ctx->opcode) + (n << 5);
    if (sh != 0) {
        int l1, l2;
        TCGv t0;
        l1 = gen_new_label();
        l2 = gen_new_label();
        t0 = tcg_temp_local_new();
        tcg_gen_brcondi_tl(TCG_COND_GE, cpu_gpr[rS(ctx->opcode)], 0, l1);
        tcg_gen_andi_tl(t0, cpu_gpr[rS(ctx->opcode)], (1ULL << sh) - 1);
        tcg_gen_brcondi_tl(TCG_COND_EQ, t0, 0, l1);
        tcg_gen_ori_tl(cpu_xer, cpu_xer, 1 << XER_CA);
        tcg_gen_br(l2);
        gen_set_label(l1);
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_CA));
        gen_set_label(l2);
        tcg_temp_free(t0);
        tcg_gen_sari_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], sh);
    } else {
        tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
        tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_CA));
    }
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
GEN_HANDLER2(sradi0, "sradi", 0x1F, 0x1A, 0x19, 0x00000000, PPC_64B)
{
    gen_sradi(ctx, 0);
}
GEN_HANDLER2(sradi1, "sradi", 0x1F, 0x1B, 0x19, 0x00000000, PPC_64B)
{
    gen_sradi(ctx, 1);
}
/* srd & srd. */
GEN_HANDLER(srd, 0x1F, 0x1B, 0x10, 0x00000000, PPC_64B)
{
    TCGv t0;
    int l1, l2;
    l1 = gen_new_label();
    l2 = gen_new_label();

    t0 = tcg_temp_local_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x7f);
    tcg_gen_brcondi_tl(TCG_COND_LT, t0, 0x40, l1);
    tcg_gen_movi_tl(cpu_gpr[rA(ctx->opcode)], 0);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_shr_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], t0);
    gen_set_label(l2);
    tcg_temp_free(t0);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}
#endif

/***                       Floating-Point arithmetic                       ***/
#define _GEN_FLOAT_ACB(name, op, op1, op2, isfloat, set_fprf, type)           \
GEN_HANDLER(f##name, op1, op2, 0xFF, 0x00000000, type)                        \
{                                                                             \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    /* NIP cannot be restored if the memory exception comes from an helper */ \
    gen_update_nip(ctx, ctx->nip - 4);                                        \
    gen_reset_fpstatus();                                                     \
    gen_helper_f##op(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rA(ctx->opcode)],      \
                     cpu_fpr[rC(ctx->opcode)], cpu_fpr[rB(ctx->opcode)]);     \
    if (isfloat) {                                                            \
        gen_helper_frsp(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rD(ctx->opcode)]);  \
    }                                                                         \
    gen_compute_fprf(cpu_fpr[rD(ctx->opcode)], set_fprf,                      \
                     Rc(ctx->opcode) != 0);                                   \
}

#define GEN_FLOAT_ACB(name, op2, set_fprf, type)                              \
_GEN_FLOAT_ACB(name, name, 0x3F, op2, 0, set_fprf, type);                     \
_GEN_FLOAT_ACB(name##s, name, 0x3B, op2, 1, set_fprf, type);

#define _GEN_FLOAT_AB(name, op, op1, op2, inval, isfloat, set_fprf, type)     \
GEN_HANDLER(f##name, op1, op2, 0xFF, inval, type)                             \
{                                                                             \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    /* NIP cannot be restored if the memory exception comes from an helper */ \
    gen_update_nip(ctx, ctx->nip - 4);                                        \
    gen_reset_fpstatus();                                                     \
    gen_helper_f##op(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rA(ctx->opcode)],      \
                     cpu_fpr[rB(ctx->opcode)]);                               \
    if (isfloat) {                                                            \
        gen_helper_frsp(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rD(ctx->opcode)]);  \
    }                                                                         \
    gen_compute_fprf(cpu_fpr[rD(ctx->opcode)],                                \
                     set_fprf, Rc(ctx->opcode) != 0);                         \
}
#define GEN_FLOAT_AB(name, op2, inval, set_fprf, type)                        \
_GEN_FLOAT_AB(name, name, 0x3F, op2, inval, 0, set_fprf, type);               \
_GEN_FLOAT_AB(name##s, name, 0x3B, op2, inval, 1, set_fprf, type);

#define _GEN_FLOAT_AC(name, op, op1, op2, inval, isfloat, set_fprf, type)     \
GEN_HANDLER(f##name, op1, op2, 0xFF, inval, type)                             \
{                                                                             \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    /* NIP cannot be restored if the memory exception comes from an helper */ \
    gen_update_nip(ctx, ctx->nip - 4);                                        \
    gen_reset_fpstatus();                                                     \
    gen_helper_f##op(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rA(ctx->opcode)],      \
                       cpu_fpr[rC(ctx->opcode)]);                             \
    if (isfloat) {                                                            \
        gen_helper_frsp(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rD(ctx->opcode)]);  \
    }                                                                         \
    gen_compute_fprf(cpu_fpr[rD(ctx->opcode)],                                \
                     set_fprf, Rc(ctx->opcode) != 0);                         \
}
#define GEN_FLOAT_AC(name, op2, inval, set_fprf, type)                        \
_GEN_FLOAT_AC(name, name, 0x3F, op2, inval, 0, set_fprf, type);               \
_GEN_FLOAT_AC(name##s, name, 0x3B, op2, inval, 1, set_fprf, type);

#define GEN_FLOAT_B(name, op2, op3, set_fprf, type)                           \
GEN_HANDLER(f##name, 0x3F, op2, op3, 0x001F0000, type)                        \
{                                                                             \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    /* NIP cannot be restored if the memory exception comes from an helper */ \
    gen_update_nip(ctx, ctx->nip - 4);                                        \
    gen_reset_fpstatus();                                                     \
    gen_helper_f##name(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rB(ctx->opcode)]);   \
    gen_compute_fprf(cpu_fpr[rD(ctx->opcode)],                                \
                     set_fprf, Rc(ctx->opcode) != 0);                         \
}

#define GEN_FLOAT_BS(name, op1, op2, set_fprf, type)                          \
GEN_HANDLER(f##name, op1, op2, 0xFF, 0x001F07C0, type)                        \
{                                                                             \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    /* NIP cannot be restored if the memory exception comes from an helper */ \
    gen_update_nip(ctx, ctx->nip - 4);                                        \
    gen_reset_fpstatus();                                                     \
    gen_helper_f##name(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rB(ctx->opcode)]);   \
    gen_compute_fprf(cpu_fpr[rD(ctx->opcode)],                                \
                     set_fprf, Rc(ctx->opcode) != 0);                         \
}

/* fadd - fadds */
GEN_FLOAT_AB(add, 0x15, 0x000007C0, 1, PPC_FLOAT);
/* fdiv - fdivs */
GEN_FLOAT_AB(div, 0x12, 0x000007C0, 1, PPC_FLOAT);
/* fmul - fmuls */
GEN_FLOAT_AC(mul, 0x19, 0x0000F800, 1, PPC_FLOAT);

/* fre */
GEN_FLOAT_BS(re, 0x3F, 0x18, 1, PPC_FLOAT_EXT);

/* fres */
GEN_FLOAT_BS(res, 0x3B, 0x18, 1, PPC_FLOAT_FRES);

/* frsqrte */
GEN_FLOAT_BS(rsqrte, 0x3F, 0x1A, 1, PPC_FLOAT_FRSQRTE);

/* frsqrtes */
GEN_HANDLER(frsqrtes, 0x3B, 0x1A, 0xFF, 0x001F07C0, PPC_FLOAT_FRSQRTES)
{
    if (unlikely(!ctx->fpu_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_FPU);
        return;
    }
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    gen_reset_fpstatus();
    gen_helper_frsqrte(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rB(ctx->opcode)]);
    gen_helper_frsp(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rD(ctx->opcode)]);
    gen_compute_fprf(cpu_fpr[rD(ctx->opcode)], 1, Rc(ctx->opcode) != 0);
}

/* fsel */
_GEN_FLOAT_ACB(sel, sel, 0x3F, 0x17, 0, 0, PPC_FLOAT_FSEL);
/* fsub - fsubs */
GEN_FLOAT_AB(sub, 0x14, 0x000007C0, 1, PPC_FLOAT);
/* Optional: */
/* fsqrt */
GEN_HANDLER(fsqrt, 0x3F, 0x16, 0xFF, 0x001F07C0, PPC_FLOAT_FSQRT)
{
    if (unlikely(!ctx->fpu_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_FPU);
        return;
    }
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    gen_reset_fpstatus();
    gen_helper_fsqrt(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rB(ctx->opcode)]);
    gen_compute_fprf(cpu_fpr[rD(ctx->opcode)], 1, Rc(ctx->opcode) != 0);
}

GEN_HANDLER(fsqrts, 0x3B, 0x16, 0xFF, 0x001F07C0, PPC_FLOAT_FSQRT)
{
    if (unlikely(!ctx->fpu_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_FPU);
        return;
    }
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    gen_reset_fpstatus();
    gen_helper_fsqrt(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rB(ctx->opcode)]);
    gen_helper_frsp(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rD(ctx->opcode)]);
    gen_compute_fprf(cpu_fpr[rD(ctx->opcode)], 1, Rc(ctx->opcode) != 0);
}

/***                     Floating-Point multiply-and-add                   ***/
/* fmadd - fmadds */
GEN_FLOAT_ACB(madd, 0x1D, 1, PPC_FLOAT);
/* fmsub - fmsubs */
GEN_FLOAT_ACB(msub, 0x1C, 1, PPC_FLOAT);
/* fnmadd - fnmadds */
GEN_FLOAT_ACB(nmadd, 0x1F, 1, PPC_FLOAT);
/* fnmsub - fnmsubs */
GEN_FLOAT_ACB(nmsub, 0x1E, 1, PPC_FLOAT);

/***                     Floating-Point round & convert                    ***/
/* fctiw */
GEN_FLOAT_B(ctiw, 0x0E, 0x00, 0, PPC_FLOAT);
/* fctiwz */
GEN_FLOAT_B(ctiwz, 0x0F, 0x00, 0, PPC_FLOAT);
/* frsp */
GEN_FLOAT_B(rsp, 0x0C, 0x00, 1, PPC_FLOAT);
#if defined(TARGET_PPC64)
/* fcfid */
GEN_FLOAT_B(cfid, 0x0E, 0x1A, 1, PPC_64B);
/* fctid */
GEN_FLOAT_B(ctid, 0x0E, 0x19, 0, PPC_64B);
/* fctidz */
GEN_FLOAT_B(ctidz, 0x0F, 0x19, 0, PPC_64B);
#endif

/* frin */
GEN_FLOAT_B(rin, 0x08, 0x0C, 1, PPC_FLOAT_EXT);
/* friz */
GEN_FLOAT_B(riz, 0x08, 0x0D, 1, PPC_FLOAT_EXT);
/* frip */
GEN_FLOAT_B(rip, 0x08, 0x0E, 1, PPC_FLOAT_EXT);
/* frim */
GEN_FLOAT_B(rim, 0x08, 0x0F, 1, PPC_FLOAT_EXT);

/***                         Floating-Point compare                        ***/
/* fcmpo */
GEN_HANDLER(fcmpo, 0x3F, 0x00, 0x01, 0x00600001, PPC_FLOAT)
{
    TCGv_i32 crf;
    if (unlikely(!ctx->fpu_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_FPU);
        return;
    }
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    gen_reset_fpstatus();
    crf = tcg_const_i32(crfD(ctx->opcode));
    gen_helper_fcmpo(cpu_fpr[rA(ctx->opcode)], cpu_fpr[rB(ctx->opcode)], crf);
    tcg_temp_free_i32(crf);
    gen_helper_float_check_status();
}

/* fcmpu */
GEN_HANDLER(fcmpu, 0x3F, 0x00, 0x00, 0x00600001, PPC_FLOAT)
{
    TCGv_i32 crf;
    if (unlikely(!ctx->fpu_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_FPU);
        return;
    }
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    gen_reset_fpstatus();
    crf = tcg_const_i32(crfD(ctx->opcode));
    gen_helper_fcmpu(cpu_fpr[rA(ctx->opcode)], cpu_fpr[rB(ctx->opcode)], crf);
    tcg_temp_free_i32(crf);
    gen_helper_float_check_status();
}

/***                         Floating-point move                           ***/
/* fabs */
/* XXX: beware that fabs never checks for NaNs nor update FPSCR */
GEN_FLOAT_B(abs, 0x08, 0x08, 0, PPC_FLOAT);

/* fmr  - fmr. */
/* XXX: beware that fmr never checks for NaNs nor update FPSCR */
GEN_HANDLER(fmr, 0x3F, 0x08, 0x02, 0x001F0000, PPC_FLOAT)
{
    if (unlikely(!ctx->fpu_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_FPU);
        return;
    }
    tcg_gen_mov_i64(cpu_fpr[rD(ctx->opcode)], cpu_fpr[rB(ctx->opcode)]);
    gen_compute_fprf(cpu_fpr[rD(ctx->opcode)], 0, Rc(ctx->opcode) != 0);
}

/* fnabs */
/* XXX: beware that fnabs never checks for NaNs nor update FPSCR */
GEN_FLOAT_B(nabs, 0x08, 0x04, 0, PPC_FLOAT);
/* fneg */
/* XXX: beware that fneg never checks for NaNs nor update FPSCR */
GEN_FLOAT_B(neg, 0x08, 0x01, 0, PPC_FLOAT);

/***                  Floating-Point status & ctrl register                ***/
/* mcrfs */
GEN_HANDLER(mcrfs, 0x3F, 0x00, 0x02, 0x0063F801, PPC_FLOAT)
{
    int bfa;

    if (unlikely(!ctx->fpu_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_FPU);
        return;
    }
    bfa = 4 * (7 - crfS(ctx->opcode));
    tcg_gen_shri_i32(cpu_crf[crfD(ctx->opcode)], cpu_fpscr, bfa);
    tcg_gen_andi_i32(cpu_crf[crfD(ctx->opcode)], cpu_crf[crfD(ctx->opcode)], 0xf);
    tcg_gen_andi_i32(cpu_fpscr, cpu_fpscr, ~(0xF << bfa));
}

/* mffs */
GEN_HANDLER(mffs, 0x3F, 0x07, 0x12, 0x001FF800, PPC_FLOAT)
{
    if (unlikely(!ctx->fpu_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_FPU);
        return;
    }
    gen_reset_fpstatus();
    tcg_gen_extu_i32_i64(cpu_fpr[rD(ctx->opcode)], cpu_fpscr);
    gen_compute_fprf(cpu_fpr[rD(ctx->opcode)], 0, Rc(ctx->opcode) != 0);
}

/* mtfsb0 */
GEN_HANDLER(mtfsb0, 0x3F, 0x06, 0x02, 0x001FF800, PPC_FLOAT)
{
    uint8_t crb;

    if (unlikely(!ctx->fpu_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_FPU);
        return;
    }
    crb = 31 - crbD(ctx->opcode);
    gen_reset_fpstatus();
    if (likely(crb != FPSCR_FEX && crb != FPSCR_VX)) {
        TCGv_i32 t0;
        /* NIP cannot be restored if the memory exception comes from an helper */
        gen_update_nip(ctx, ctx->nip - 4);
        t0 = tcg_const_i32(crb);
        gen_helper_fpscr_clrbit(t0);
        tcg_temp_free_i32(t0);
    }
    if (unlikely(Rc(ctx->opcode) != 0)) {
        tcg_gen_shri_i32(cpu_crf[1], cpu_fpscr, FPSCR_OX);
    }
}

/* mtfsb1 */
GEN_HANDLER(mtfsb1, 0x3F, 0x06, 0x01, 0x001FF800, PPC_FLOAT)
{
    uint8_t crb;

    if (unlikely(!ctx->fpu_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_FPU);
        return;
    }
    crb = 31 - crbD(ctx->opcode);
    gen_reset_fpstatus();
    /* XXX: we pretend we can only do IEEE floating-point computations */
    if (likely(crb != FPSCR_FEX && crb != FPSCR_VX && crb != FPSCR_NI)) {
        TCGv_i32 t0;
        /* NIP cannot be restored if the memory exception comes from an helper */
        gen_update_nip(ctx, ctx->nip - 4);
        t0 = tcg_const_i32(crb);
        gen_helper_fpscr_setbit(t0);
        tcg_temp_free_i32(t0);
    }
    if (unlikely(Rc(ctx->opcode) != 0)) {
        tcg_gen_shri_i32(cpu_crf[1], cpu_fpscr, FPSCR_OX);
    }
    /* We can raise a differed exception */
    gen_helper_float_check_status();
}

/* mtfsf */
GEN_HANDLER(mtfsf, 0x3F, 0x07, 0x16, 0x02010000, PPC_FLOAT)
{
    TCGv_i32 t0;

    if (unlikely(!ctx->fpu_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_FPU);
        return;
    }
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    gen_reset_fpstatus();
    t0 = tcg_const_i32(FM(ctx->opcode));
    gen_helper_store_fpscr(cpu_fpr[rB(ctx->opcode)], t0);
    tcg_temp_free_i32(t0);
    if (unlikely(Rc(ctx->opcode) != 0)) {
        tcg_gen_shri_i32(cpu_crf[1], cpu_fpscr, FPSCR_OX);
    }
    /* We can raise a differed exception */
    gen_helper_float_check_status();
}

/* mtfsfi */
GEN_HANDLER(mtfsfi, 0x3F, 0x06, 0x04, 0x006f0800, PPC_FLOAT)
{
    int bf, sh;
    TCGv_i64 t0;
    TCGv_i32 t1;

    if (unlikely(!ctx->fpu_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_FPU);
        return;
    }
    bf = crbD(ctx->opcode) >> 2;
    sh = 7 - bf;
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    gen_reset_fpstatus();
    t0 = tcg_const_i64(FPIMM(ctx->opcode) << (4 * sh));
    t1 = tcg_const_i32(1 << sh);
    gen_helper_store_fpscr(t0, t1);
    tcg_temp_free_i64(t0);
    tcg_temp_free_i32(t1);
    if (unlikely(Rc(ctx->opcode) != 0)) {
        tcg_gen_shri_i32(cpu_crf[1], cpu_fpscr, FPSCR_OX);
    }
    /* We can raise a differed exception */
    gen_helper_float_check_status();
}

/***                           Addressing modes                            ***/
/* Register indirect with immediate index : EA = (rA|0) + SIMM */
static always_inline void gen_addr_imm_index (DisasContext *ctx, TCGv EA, target_long maskl)
{
    target_long simm = SIMM(ctx->opcode);

    simm &= ~maskl;
    if (rA(ctx->opcode) == 0) {
#if defined(TARGET_PPC64)
        if (!ctx->sf_mode) {
            tcg_gen_movi_tl(EA, (uint32_t)simm);
        } else
#endif
        tcg_gen_movi_tl(EA, simm);
    } else if (likely(simm != 0)) {
        tcg_gen_addi_tl(EA, cpu_gpr[rA(ctx->opcode)], simm);
#if defined(TARGET_PPC64)
        if (!ctx->sf_mode) {
            tcg_gen_ext32u_tl(EA, EA);
        }
#endif
    } else {
#if defined(TARGET_PPC64)
        if (!ctx->sf_mode) {
            tcg_gen_ext32u_tl(EA, cpu_gpr[rA(ctx->opcode)]);
        } else
#endif
        tcg_gen_mov_tl(EA, cpu_gpr[rA(ctx->opcode)]);
    }
}

static always_inline void gen_addr_reg_index (DisasContext *ctx, TCGv EA)
{
    if (rA(ctx->opcode) == 0) {
#if defined(TARGET_PPC64)
        if (!ctx->sf_mode) {
            tcg_gen_ext32u_tl(EA, cpu_gpr[rB(ctx->opcode)]);
        } else
#endif
        tcg_gen_mov_tl(EA, cpu_gpr[rB(ctx->opcode)]);
    } else {
        tcg_gen_add_tl(EA, cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
#if defined(TARGET_PPC64)
        if (!ctx->sf_mode) {
            tcg_gen_ext32u_tl(EA, EA);
        }
#endif
    }
}

static always_inline void gen_addr_register (DisasContext *ctx, TCGv EA)
{
    if (rA(ctx->opcode) == 0) {
        tcg_gen_movi_tl(EA, 0);
    } else {
#if defined(TARGET_PPC64)
        if (!ctx->sf_mode) {
            tcg_gen_ext32u_tl(EA, cpu_gpr[rA(ctx->opcode)]);
        } else
#endif
            tcg_gen_mov_tl(EA, cpu_gpr[rA(ctx->opcode)]);
    }
}

static always_inline void gen_addr_add (DisasContext *ctx, TCGv ret, TCGv arg1, target_long val)
{
    tcg_gen_addi_tl(ret, arg1, val);
#if defined(TARGET_PPC64)
    if (!ctx->sf_mode) {
        tcg_gen_ext32u_tl(ret, ret);
    }
#endif
}

static always_inline void gen_check_align (DisasContext *ctx, TCGv EA, int mask)
{
    int l1 = gen_new_label();
    TCGv t0 = tcg_temp_new();
    TCGv_i32 t1, t2;
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    tcg_gen_andi_tl(t0, EA, mask);
    tcg_gen_brcondi_tl(TCG_COND_EQ, t0, 0, l1);
    t1 = tcg_const_i32(POWERPC_EXCP_ALIGN);
    t2 = tcg_const_i32(0);
    gen_helper_raise_exception_err(t1, t2);
    tcg_temp_free_i32(t1);
    tcg_temp_free_i32(t2);
    gen_set_label(l1);
    tcg_temp_free(t0);
}

/***                             Integer load                              ***/
static always_inline void gen_qemu_ld8u(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    tcg_gen_qemu_ld8u(arg1, arg2, ctx->mem_idx);
}

static always_inline void gen_qemu_ld8s(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    tcg_gen_qemu_ld8s(arg1, arg2, ctx->mem_idx);
}

static always_inline void gen_qemu_ld16u(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    tcg_gen_qemu_ld16u(arg1, arg2, ctx->mem_idx);
    if (unlikely(ctx->le_mode)) {
#if defined(TARGET_PPC64)
        TCGv_i32 t0 = tcg_temp_new_i32();
        tcg_gen_trunc_tl_i32(t0, arg1);
        tcg_gen_bswap16_i32(t0, t0);
        tcg_gen_extu_i32_tl(arg1, t0);
        tcg_temp_free_i32(t0);
#else
        tcg_gen_bswap16_i32(arg1, arg1);
#endif
    }
}

static always_inline void gen_qemu_ld16s(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    if (unlikely(ctx->le_mode)) {
#if defined(TARGET_PPC64)
        TCGv_i32 t0;
        tcg_gen_qemu_ld16u(arg1, arg2, ctx->mem_idx);
        t0 = tcg_temp_new_i32();
        tcg_gen_trunc_tl_i32(t0, arg1);
        tcg_gen_bswap16_i32(t0, t0);
        tcg_gen_extu_i32_tl(arg1, t0);
        tcg_gen_ext16s_tl(arg1, arg1);
        tcg_temp_free_i32(t0);
#else
        tcg_gen_qemu_ld16u(arg1, arg2, ctx->mem_idx);
        tcg_gen_bswap16_i32(arg1, arg1);
        tcg_gen_ext16s_i32(arg1, arg1);
#endif
    } else {
        tcg_gen_qemu_ld16s(arg1, arg2, ctx->mem_idx);
    }
}

static always_inline void gen_qemu_ld32u(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    tcg_gen_qemu_ld32u(arg1, arg2, ctx->mem_idx);
    if (unlikely(ctx->le_mode)) {
#if defined(TARGET_PPC64)
        TCGv_i32 t0 = tcg_temp_new_i32();
        tcg_gen_trunc_tl_i32(t0, arg1);
        tcg_gen_bswap_i32(t0, t0);
        tcg_gen_extu_i32_tl(arg1, t0);
        tcg_temp_free_i32(t0);
#else
        tcg_gen_bswap_i32(arg1, arg1);
#endif
    }
}

#if defined(TARGET_PPC64)
static always_inline void gen_qemu_ld32s(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    if (unlikely(ctx->mem_idx)) {
        TCGv_i32 t0;
        tcg_gen_qemu_ld32u(arg1, arg2, ctx->mem_idx);
        t0 = tcg_temp_new_i32();
        tcg_gen_trunc_tl_i32(t0, arg1);
        tcg_gen_bswap_i32(t0, t0);
        tcg_gen_ext_i32_tl(arg1, t0);
        tcg_temp_free_i32(t0);
    } else
        tcg_gen_qemu_ld32s(arg1, arg2, ctx->mem_idx);
}
#endif

static always_inline void gen_qemu_ld64(DisasContext *ctx, TCGv_i64 arg1, TCGv arg2)
{
    tcg_gen_qemu_ld64(arg1, arg2, ctx->mem_idx);
    if (unlikely(ctx->le_mode)) {
        tcg_gen_bswap_i64(arg1, arg1);
    }
}

static always_inline void gen_qemu_st8(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    tcg_gen_qemu_st8(arg1, arg2, ctx->mem_idx);
}

static always_inline void gen_qemu_st16(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    if (unlikely(ctx->le_mode)) {
#if defined(TARGET_PPC64)
        TCGv_i32 t0;
        TCGv t1;
        t0 = tcg_temp_new_i32();
        tcg_gen_trunc_tl_i32(t0, arg1);
        tcg_gen_ext16u_i32(t0, t0);
        tcg_gen_bswap16_i32(t0, t0);
        t1 = tcg_temp_new();
        tcg_gen_extu_i32_tl(t1, t0);
        tcg_temp_free_i32(t0);
        tcg_gen_qemu_st16(t1, arg2, ctx->mem_idx);
        tcg_temp_free(t1);
#else
        TCGv t0 = tcg_temp_new();
        tcg_gen_ext16u_tl(t0, arg1);
        tcg_gen_bswap16_i32(t0, t0);
        tcg_gen_qemu_st16(t0, arg2, ctx->mem_idx);
        tcg_temp_free(t0);
#endif
    } else {
        tcg_gen_qemu_st16(arg1, arg2, ctx->mem_idx);
    }
}

static always_inline void gen_qemu_st32(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    if (unlikely(ctx->le_mode)) {
#if defined(TARGET_PPC64)
        TCGv_i32 t0;
        TCGv t1;
        t0 = tcg_temp_new_i32();
        tcg_gen_trunc_tl_i32(t0, arg1);
        tcg_gen_bswap_i32(t0, t0);
        t1 = tcg_temp_new();
        tcg_gen_extu_i32_tl(t1, t0);
        tcg_temp_free_i32(t0);
        tcg_gen_qemu_st32(t1, arg2, ctx->mem_idx);
        tcg_temp_free(t1);
#else
        TCGv t0 = tcg_temp_new_i32();
        tcg_gen_bswap_i32(t0, arg1);
        tcg_gen_qemu_st32(t0, arg2, ctx->mem_idx);
        tcg_temp_free(t0);
#endif
    } else {
        tcg_gen_qemu_st32(arg1, arg2, ctx->mem_idx);
    }
}

static always_inline void gen_qemu_st64(DisasContext *ctx, TCGv_i64 arg1, TCGv arg2)
{
    if (unlikely(ctx->le_mode)) {
        TCGv_i64 t0 = tcg_temp_new_i64();
        tcg_gen_bswap_i64(t0, arg1);
        tcg_gen_qemu_st64(t0, arg2, ctx->mem_idx);
        tcg_temp_free_i64(t0);
    } else
        tcg_gen_qemu_st64(arg1, arg2, ctx->mem_idx);
}

#define GEN_LD(name, ldop, opc, type)                                         \
GEN_HANDLER(name, opc, 0xFF, 0xFF, 0x00000000, type)                          \
{                                                                             \
    TCGv EA;                                                                  \
    gen_set_access_type(ctx, ACCESS_INT);                                     \
    EA = tcg_temp_new();                                                      \
    gen_addr_imm_index(ctx, EA, 0);                                           \
    gen_qemu_##ldop(ctx, cpu_gpr[rD(ctx->opcode)], EA);                       \
    tcg_temp_free(EA);                                                        \
}

#define GEN_LDU(name, ldop, opc, type)                                        \
GEN_HANDLER(name##u, opc, 0xFF, 0xFF, 0x00000000, type)                       \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(rA(ctx->opcode) == 0 ||                                      \
                 rA(ctx->opcode) == rD(ctx->opcode))) {                       \
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);                   \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_INT);                                     \
    EA = tcg_temp_new();                                                      \
    if (type == PPC_64B)                                                      \
        gen_addr_imm_index(ctx, EA, 0x03);                                    \
    else                                                                      \
        gen_addr_imm_index(ctx, EA, 0);                                       \
    gen_qemu_##ldop(ctx, cpu_gpr[rD(ctx->opcode)], EA);                       \
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], EA);                             \
    tcg_temp_free(EA);                                                        \
}

#define GEN_LDUX(name, ldop, opc2, opc3, type)                                \
GEN_HANDLER(name##ux, 0x1F, opc2, opc3, 0x00000001, type)                     \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(rA(ctx->opcode) == 0 ||                                      \
                 rA(ctx->opcode) == rD(ctx->opcode))) {                       \
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);                   \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_INT);                                     \
    EA = tcg_temp_new();                                                      \
    gen_addr_reg_index(ctx, EA);                                              \
    gen_qemu_##ldop(ctx, cpu_gpr[rD(ctx->opcode)], EA);                       \
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], EA);                             \
    tcg_temp_free(EA);                                                        \
}

#define GEN_LDX(name, ldop, opc2, opc3, type)                                 \
GEN_HANDLER(name##x, 0x1F, opc2, opc3, 0x00000001, type)                      \
{                                                                             \
    TCGv EA;                                                                  \
    gen_set_access_type(ctx, ACCESS_INT);                                     \
    EA = tcg_temp_new();                                                      \
    gen_addr_reg_index(ctx, EA);                                              \
    gen_qemu_##ldop(ctx, cpu_gpr[rD(ctx->opcode)], EA);                       \
    tcg_temp_free(EA);                                                        \
}

#define GEN_LDS(name, ldop, op, type)                                         \
GEN_LD(name, ldop, op | 0x20, type);                                          \
GEN_LDU(name, ldop, op | 0x21, type);                                         \
GEN_LDUX(name, ldop, 0x17, op | 0x01, type);                                  \
GEN_LDX(name, ldop, 0x17, op | 0x00, type)

/* lbz lbzu lbzux lbzx */
GEN_LDS(lbz, ld8u, 0x02, PPC_INTEGER);
/* lha lhau lhaux lhax */
GEN_LDS(lha, ld16s, 0x0A, PPC_INTEGER);
/* lhz lhzu lhzux lhzx */
GEN_LDS(lhz, ld16u, 0x08, PPC_INTEGER);
/* lwz lwzu lwzux lwzx */
GEN_LDS(lwz, ld32u, 0x00, PPC_INTEGER);
#if defined(TARGET_PPC64)
/* lwaux */
GEN_LDUX(lwa, ld32s, 0x15, 0x0B, PPC_64B);
/* lwax */
GEN_LDX(lwa, ld32s, 0x15, 0x0A, PPC_64B);
/* ldux */
GEN_LDUX(ld, ld64, 0x15, 0x01, PPC_64B);
/* ldx */
GEN_LDX(ld, ld64, 0x15, 0x00, PPC_64B);
GEN_HANDLER(ld, 0x3A, 0xFF, 0xFF, 0x00000000, PPC_64B)
{
    TCGv EA;
    if (Rc(ctx->opcode)) {
        if (unlikely(rA(ctx->opcode) == 0 ||
                     rA(ctx->opcode) == rD(ctx->opcode))) {
            gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
            return;
        }
    }
    gen_set_access_type(ctx, ACCESS_INT);
    EA = tcg_temp_new();
    gen_addr_imm_index(ctx, EA, 0x03);
    if (ctx->opcode & 0x02) {
        /* lwa (lwau is undefined) */
        gen_qemu_ld32s(ctx, cpu_gpr[rD(ctx->opcode)], EA);
    } else {
        /* ld - ldu */
        gen_qemu_ld64(ctx, cpu_gpr[rD(ctx->opcode)], EA);
    }
    if (Rc(ctx->opcode))
        tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], EA);
    tcg_temp_free(EA);
}
/* lq */
GEN_HANDLER(lq, 0x38, 0xFF, 0xFF, 0x00000000, PPC_64BX)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    int ra, rd;
    TCGv EA;

    /* Restore CPU state */
    if (unlikely(ctx->mem_idx == 0)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    ra = rA(ctx->opcode);
    rd = rD(ctx->opcode);
    if (unlikely((rd & 1) || rd == ra)) {
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
        return;
    }
    if (unlikely(ctx->le_mode)) {
        /* Little-endian mode is not handled */
        gen_exception_err(ctx, POWERPC_EXCP_ALIGN, POWERPC_EXCP_ALIGN_LE);
        return;
    }
    gen_set_access_type(ctx, ACCESS_INT);
    EA = tcg_temp_new();
    gen_addr_imm_index(ctx, EA, 0x0F);
    gen_qemu_ld64(ctx, cpu_gpr[rd], EA);
    gen_addr_add(ctx, EA, EA, 8);
    gen_qemu_ld64(ctx, cpu_gpr[rd+1], EA);
    tcg_temp_free(EA);
#endif
}
#endif

/***                              Integer store                            ***/
#define GEN_ST(name, stop, opc, type)                                         \
GEN_HANDLER(name, opc, 0xFF, 0xFF, 0x00000000, type)                          \
{                                                                             \
    TCGv EA;                                                                  \
    gen_set_access_type(ctx, ACCESS_INT);                                     \
    EA = tcg_temp_new();                                                      \
    gen_addr_imm_index(ctx, EA, 0);                                           \
    gen_qemu_##stop(ctx, cpu_gpr[rS(ctx->opcode)], EA);                       \
    tcg_temp_free(EA);                                                        \
}

#define GEN_STU(name, stop, opc, type)                                        \
GEN_HANDLER(stop##u, opc, 0xFF, 0xFF, 0x00000000, type)                       \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(rA(ctx->opcode) == 0)) {                                     \
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);                   \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_INT);                                     \
    EA = tcg_temp_new();                                                      \
    if (type == PPC_64B)                                                      \
        gen_addr_imm_index(ctx, EA, 0x03);                                    \
    else                                                                      \
        gen_addr_imm_index(ctx, EA, 0);                                       \
    gen_qemu_##stop(ctx, cpu_gpr[rS(ctx->opcode)], EA);                       \
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], EA);                             \
    tcg_temp_free(EA);                                                        \
}

#define GEN_STUX(name, stop, opc2, opc3, type)                                \
GEN_HANDLER(name##ux, 0x1F, opc2, opc3, 0x00000001, type)                     \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(rA(ctx->opcode) == 0)) {                                     \
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);                   \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_INT);                                     \
    EA = tcg_temp_new();                                                      \
    gen_addr_reg_index(ctx, EA);                                              \
    gen_qemu_##stop(ctx, cpu_gpr[rS(ctx->opcode)], EA);                       \
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], EA);                             \
    tcg_temp_free(EA);                                                        \
}

#define GEN_STX(name, stop, opc2, opc3, type)                                 \
GEN_HANDLER(name##x, 0x1F, opc2, opc3, 0x00000001, type)                      \
{                                                                             \
    TCGv EA;                                                                  \
    gen_set_access_type(ctx, ACCESS_INT);                                     \
    EA = tcg_temp_new();                                                      \
    gen_addr_reg_index(ctx, EA);                                              \
    gen_qemu_##stop(ctx, cpu_gpr[rS(ctx->opcode)], EA);                       \
    tcg_temp_free(EA);                                                        \
}

#define GEN_STS(name, stop, op, type)                                         \
GEN_ST(name, stop, op | 0x20, type);                                          \
GEN_STU(name, stop, op | 0x21, type);                                         \
GEN_STUX(name, stop, 0x17, op | 0x01, type);                                  \
GEN_STX(name, stop, 0x17, op | 0x00, type)

/* stb stbu stbux stbx */
GEN_STS(stb, st8, 0x06, PPC_INTEGER);
/* sth sthu sthux sthx */
GEN_STS(sth, st16, 0x0C, PPC_INTEGER);
/* stw stwu stwux stwx */
GEN_STS(stw, st32, 0x04, PPC_INTEGER);
#if defined(TARGET_PPC64)
GEN_STUX(std, st64, 0x15, 0x05, PPC_64B);
GEN_STX(std, st64, 0x15, 0x04, PPC_64B);
GEN_HANDLER(std, 0x3E, 0xFF, 0xFF, 0x00000000, PPC_64B)
{
    int rs;
    TCGv EA;

    rs = rS(ctx->opcode);
    if ((ctx->opcode & 0x3) == 0x2) {
#if defined(CONFIG_USER_ONLY)
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
        /* stq */
        if (unlikely(ctx->mem_idx == 0)) {
            gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
            return;
        }
        if (unlikely(rs & 1)) {
            gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
            return;
        }
        if (unlikely(ctx->le_mode)) {
            /* Little-endian mode is not handled */
            gen_exception_err(ctx, POWERPC_EXCP_ALIGN, POWERPC_EXCP_ALIGN_LE);
            return;
        }
        gen_set_access_type(ctx, ACCESS_INT);
        EA = tcg_temp_new();
        gen_addr_imm_index(ctx, EA, 0x03);
        gen_qemu_st64(ctx, cpu_gpr[rs], EA);
        gen_addr_add(ctx, EA, EA, 8);
        gen_qemu_st64(ctx, cpu_gpr[rs+1], EA);
        tcg_temp_free(EA);
#endif
    } else {
        /* std / stdu */
        if (Rc(ctx->opcode)) {
            if (unlikely(rA(ctx->opcode) == 0)) {
                gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
                return;
            }
        }
        gen_set_access_type(ctx, ACCESS_INT);
        EA = tcg_temp_new();
        gen_addr_imm_index(ctx, EA, 0x03);
        gen_qemu_st64(ctx, cpu_gpr[rs], EA);
        if (Rc(ctx->opcode))
            tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], EA);
        tcg_temp_free(EA);
    }
}
#endif
/***                Integer load and store with byte reverse               ***/
/* lhbrx */
static void always_inline gen_qemu_ld16ur(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    tcg_gen_qemu_ld16u(arg1, arg2, ctx->mem_idx);
    if (likely(!ctx->le_mode)) {
#if defined(TARGET_PPC64)
        TCGv_i32 t0 = tcg_temp_new_i32();
        tcg_gen_trunc_tl_i32(t0, arg1);
        tcg_gen_bswap16_i32(t0, t0);
        tcg_gen_extu_i32_tl(arg1, t0);
        tcg_temp_free_i32(t0);
#else
        tcg_gen_bswap16_i32(arg1, arg1);
#endif
    }
}
GEN_LDX(lhbr, ld16ur, 0x16, 0x18, PPC_INTEGER);

/* lwbrx */
static void always_inline gen_qemu_ld32ur(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    tcg_gen_qemu_ld32u(arg1, arg2, ctx->mem_idx);
    if (likely(!ctx->le_mode)) {
#if defined(TARGET_PPC64)
        TCGv_i32 t0 = tcg_temp_new_i32();
        tcg_gen_trunc_tl_i32(t0, arg1);
        tcg_gen_bswap_i32(t0, t0);
        tcg_gen_extu_i32_tl(arg1, t0);
        tcg_temp_free_i32(t0);
#else
        tcg_gen_bswap_i32(arg1, arg1);
#endif
    }
}
GEN_LDX(lwbr, ld32ur, 0x16, 0x10, PPC_INTEGER);

/* sthbrx */
static void always_inline gen_qemu_st16r(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    if (likely(!ctx->le_mode)) {
#if defined(TARGET_PPC64)
        TCGv_i32 t0;
        TCGv t1;
        t0 = tcg_temp_new_i32();
        tcg_gen_trunc_tl_i32(t0, arg1);
        tcg_gen_ext16u_i32(t0, t0);
        tcg_gen_bswap16_i32(t0, t0);
        t1 = tcg_temp_new();
        tcg_gen_extu_i32_tl(t1, t0);
        tcg_temp_free_i32(t0);
        tcg_gen_qemu_st16(t1, arg2, ctx->mem_idx);
        tcg_temp_free(t1);
#else
        TCGv t0 = tcg_temp_new();
        tcg_gen_ext16u_tl(t0, arg1);
        tcg_gen_bswap16_i32(t0, t0);
        tcg_gen_qemu_st16(t0, arg2, ctx->mem_idx);
        tcg_temp_free(t0);
#endif
    } else {
        tcg_gen_qemu_st16(arg1, arg2, ctx->mem_idx);
    }
}
GEN_STX(sthbr, st16r, 0x16, 0x1C, PPC_INTEGER);

/* stwbrx */
static void always_inline gen_qemu_st32r(DisasContext *ctx, TCGv arg1, TCGv arg2)
{
    if (likely(!ctx->le_mode)) {
#if defined(TARGET_PPC64)
        TCGv_i32 t0;
        TCGv t1;
        t0 = tcg_temp_new_i32();
        tcg_gen_trunc_tl_i32(t0, arg1);
        tcg_gen_bswap_i32(t0, t0);
        t1 = tcg_temp_new();
        tcg_gen_extu_i32_tl(t1, t0);
        tcg_temp_free_i32(t0);
        tcg_gen_qemu_st32(t1, arg2, ctx->mem_idx);
        tcg_temp_free(t1);
#else
        TCGv t0 = tcg_temp_new_i32();
        tcg_gen_bswap_i32(t0, arg1);
        tcg_gen_qemu_st32(t0, arg2, ctx->mem_idx);
        tcg_temp_free(t0);
#endif
    } else {
        tcg_gen_qemu_st32(arg1, arg2, ctx->mem_idx);
    }
}
GEN_STX(stwbr, st32r, 0x16, 0x14, PPC_INTEGER);

/***                    Integer load and store multiple                    ***/
/* lmw */
GEN_HANDLER(lmw, 0x2E, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    TCGv t0;
    TCGv_i32 t1;
    gen_set_access_type(ctx, ACCESS_INT);
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    t0 = tcg_temp_new();
    t1 = tcg_const_i32(rD(ctx->opcode));
    gen_addr_imm_index(ctx, t0, 0);
    gen_helper_lmw(t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free_i32(t1);
}

/* stmw */
GEN_HANDLER(stmw, 0x2F, 0xFF, 0xFF, 0x00000000, PPC_INTEGER)
{
    TCGv t0;
    TCGv_i32 t1;
    gen_set_access_type(ctx, ACCESS_INT);
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    t0 = tcg_temp_new();
    t1 = tcg_const_i32(rS(ctx->opcode));
    gen_addr_imm_index(ctx, t0, 0);
    gen_helper_stmw(t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free_i32(t1);
}

/***                    Integer load and store strings                     ***/
/* lswi */
/* PowerPC32 specification says we must generate an exception if
 * rA is in the range of registers to be loaded.
 * In an other hand, IBM says this is valid, but rA won't be loaded.
 * For now, I'll follow the spec...
 */
GEN_HANDLER(lswi, 0x1F, 0x15, 0x12, 0x00000001, PPC_STRING)
{
    TCGv t0;
    TCGv_i32 t1, t2;
    int nb = NB(ctx->opcode);
    int start = rD(ctx->opcode);
    int ra = rA(ctx->opcode);
    int nr;

    if (nb == 0)
        nb = 32;
    nr = nb / 4;
    if (unlikely(((start + nr) > 32  &&
                  start <= ra && (start + nr - 32) > ra) ||
                 ((start + nr) <= 32 && start <= ra && (start + nr) > ra))) {
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_LSWX);
        return;
    }
    gen_set_access_type(ctx, ACCESS_INT);
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    t0 = tcg_temp_new();
    gen_addr_register(ctx, t0);
    t1 = tcg_const_i32(nb);
    t2 = tcg_const_i32(start);
    gen_helper_lsw(t0, t1, t2);
    tcg_temp_free(t0);
    tcg_temp_free_i32(t1);
    tcg_temp_free_i32(t2);
}

/* lswx */
GEN_HANDLER(lswx, 0x1F, 0x15, 0x10, 0x00000001, PPC_STRING)
{
    TCGv t0;
    TCGv_i32 t1, t2, t3;
    gen_set_access_type(ctx, ACCESS_INT);
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    t1 = tcg_const_i32(rD(ctx->opcode));
    t2 = tcg_const_i32(rA(ctx->opcode));
    t3 = tcg_const_i32(rB(ctx->opcode));
    gen_helper_lswx(t0, t1, t2, t3);
    tcg_temp_free(t0);
    tcg_temp_free_i32(t1);
    tcg_temp_free_i32(t2);
    tcg_temp_free_i32(t3);
}

/* stswi */
GEN_HANDLER(stswi, 0x1F, 0x15, 0x16, 0x00000001, PPC_STRING)
{
    TCGv t0;
    TCGv_i32 t1, t2;
    int nb = NB(ctx->opcode);
    gen_set_access_type(ctx, ACCESS_INT);
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    t0 = tcg_temp_new();
    gen_addr_register(ctx, t0);
    if (nb == 0)
        nb = 32;
    t1 = tcg_const_i32(nb);
    t2 = tcg_const_i32(rS(ctx->opcode));
    gen_helper_stsw(t0, t1, t2);
    tcg_temp_free(t0);
    tcg_temp_free_i32(t1);
    tcg_temp_free_i32(t2);
}

/* stswx */
GEN_HANDLER(stswx, 0x1F, 0x15, 0x14, 0x00000001, PPC_STRING)
{
    TCGv t0;
    TCGv_i32 t1, t2;
    gen_set_access_type(ctx, ACCESS_INT);
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    t1 = tcg_temp_new_i32();
    tcg_gen_trunc_tl_i32(t1, cpu_xer);
    tcg_gen_andi_i32(t1, t1, 0x7F);
    t2 = tcg_const_i32(rS(ctx->opcode));
    gen_helper_stsw(t0, t1, t2);
    tcg_temp_free(t0);
    tcg_temp_free_i32(t1);
    tcg_temp_free_i32(t2);
}

/***                        Memory synchronisation                         ***/
/* eieio */
GEN_HANDLER(eieio, 0x1F, 0x16, 0x1A, 0x03FFF801, PPC_MEM_EIEIO)
{
}

/* isync */
GEN_HANDLER(isync, 0x13, 0x16, 0x04, 0x03FFF801, PPC_MEM)
{
    gen_stop_exception(ctx);
}

/* lwarx */
GEN_HANDLER(lwarx, 0x1F, 0x14, 0x00, 0x00000001, PPC_RES)
{
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_RES);
    t0 = tcg_temp_local_new();
    gen_addr_reg_index(ctx, t0);
    gen_check_align(ctx, t0, 0x03);
    gen_qemu_ld32u(ctx, cpu_gpr[rD(ctx->opcode)], t0);
    tcg_gen_mov_tl(cpu_reserve, t0);
    tcg_temp_free(t0);
}

/* stwcx. */
GEN_HANDLER2(stwcx_, "stwcx.", 0x1F, 0x16, 0x04, 0x00000000, PPC_RES)
{
    int l1;
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_RES);
    t0 = tcg_temp_local_new();
    gen_addr_reg_index(ctx, t0);
    gen_check_align(ctx, t0, 0x03);
    tcg_gen_trunc_tl_i32(cpu_crf[0], cpu_xer);
    tcg_gen_shri_i32(cpu_crf[0], cpu_crf[0], XER_SO);
    tcg_gen_andi_i32(cpu_crf[0], cpu_crf[0], 1);
    l1 = gen_new_label();
    tcg_gen_brcond_tl(TCG_COND_NE, t0, cpu_reserve, l1);
    tcg_gen_ori_i32(cpu_crf[0], cpu_crf[0], 1 << CRF_EQ);
    gen_qemu_st32(ctx, cpu_gpr[rS(ctx->opcode)], t0);
    gen_set_label(l1);
    tcg_gen_movi_tl(cpu_reserve, -1);
    tcg_temp_free(t0);
}

#if defined(TARGET_PPC64)
/* ldarx */
GEN_HANDLER(ldarx, 0x1F, 0x14, 0x02, 0x00000001, PPC_64B)
{
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_RES);
    t0 = tcg_temp_local_new();
    gen_addr_reg_index(ctx, t0);
    gen_check_align(ctx, t0, 0x07);
    gen_qemu_ld64(ctx, cpu_gpr[rD(ctx->opcode)], t0);
    tcg_gen_mov_tl(cpu_reserve, t0);
    tcg_temp_free(t0);
}

/* stdcx. */
GEN_HANDLER2(stdcx_, "stdcx.", 0x1F, 0x16, 0x06, 0x00000000, PPC_64B)
{
    int l1;
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_RES);
    t0 = tcg_temp_local_new();
    gen_addr_reg_index(ctx, t0);
    gen_check_align(ctx, t0, 0x07);
    tcg_gen_trunc_tl_i32(cpu_crf[0], cpu_xer);
    tcg_gen_shri_i32(cpu_crf[0], cpu_crf[0], XER_SO);
    tcg_gen_andi_i32(cpu_crf[0], cpu_crf[0], 1);
    l1 = gen_new_label();
    tcg_gen_brcond_tl(TCG_COND_NE, t0, cpu_reserve, l1);
    tcg_gen_ori_i32(cpu_crf[0], cpu_crf[0], 1 << CRF_EQ);
    gen_qemu_st64(ctx, cpu_gpr[rS(ctx->opcode)], t0);
    gen_set_label(l1);
    tcg_gen_movi_tl(cpu_reserve, -1);
    tcg_temp_free(t0);
}
#endif /* defined(TARGET_PPC64) */

/* sync */
GEN_HANDLER(sync, 0x1F, 0x16, 0x12, 0x039FF801, PPC_MEM_SYNC)
{
}

/* wait */
GEN_HANDLER(wait, 0x1F, 0x1E, 0x01, 0x03FFF801, PPC_WAIT)
{
    TCGv_i32 t0 = tcg_temp_new_i32();
    tcg_gen_st_i32(t0, cpu_env, offsetof(CPUState, halted));
    tcg_temp_free_i32(t0);
    /* Stop translation, as the CPU is supposed to sleep from now */
    gen_exception_err(ctx, EXCP_HLT, 1);
}

/***                         Floating-point load                           ***/
#define GEN_LDF(name, ldop, opc, type)                                        \
GEN_HANDLER(name, opc, 0xFF, 0xFF, 0x00000000, type)                          \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_FLOAT);                                   \
    EA = tcg_temp_new();                                                      \
    gen_addr_imm_index(ctx, EA, 0);                                           \
    gen_qemu_##ldop(ctx, cpu_fpr[rD(ctx->opcode)], EA);                       \
    tcg_temp_free(EA);                                                        \
}

#define GEN_LDUF(name, ldop, opc, type)                                       \
GEN_HANDLER(name##u, opc, 0xFF, 0xFF, 0x00000000, type)                       \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    if (unlikely(rA(ctx->opcode) == 0)) {                                     \
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);                   \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_FLOAT);                                   \
    EA = tcg_temp_new();                                                      \
    gen_addr_imm_index(ctx, EA, 0);                                           \
    gen_qemu_##ldop(ctx, cpu_fpr[rD(ctx->opcode)], EA);                       \
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], EA);                             \
    tcg_temp_free(EA);                                                        \
}

#define GEN_LDUXF(name, ldop, opc, type)                                      \
GEN_HANDLER(name##ux, 0x1F, 0x17, opc, 0x00000001, type)                      \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    if (unlikely(rA(ctx->opcode) == 0)) {                                     \
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);                   \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_FLOAT);                                   \
    EA = tcg_temp_new();                                                      \
    gen_addr_reg_index(ctx, EA);                                              \
    gen_qemu_##ldop(ctx, cpu_fpr[rD(ctx->opcode)], EA);                       \
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], EA);                             \
    tcg_temp_free(EA);                                                        \
}

#define GEN_LDXF(name, ldop, opc2, opc3, type)                                \
GEN_HANDLER(name##x, 0x1F, opc2, opc3, 0x00000001, type)                      \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_FLOAT);                                   \
    EA = tcg_temp_new();                                                      \
    gen_addr_reg_index(ctx, EA);                                              \
    gen_qemu_##ldop(ctx, cpu_fpr[rD(ctx->opcode)], EA);                       \
    tcg_temp_free(EA);                                                        \
}

#define GEN_LDFS(name, ldop, op, type)                                        \
GEN_LDF(name, ldop, op | 0x20, type);                                         \
GEN_LDUF(name, ldop, op | 0x21, type);                                        \
GEN_LDUXF(name, ldop, op | 0x01, type);                                       \
GEN_LDXF(name, ldop, 0x17, op | 0x00, type)

static always_inline void gen_qemu_ld32fs(DisasContext *ctx, TCGv_i64 arg1, TCGv arg2)
{
    TCGv t0 = tcg_temp_new();
    TCGv_i32 t1 = tcg_temp_new_i32();
    gen_qemu_ld32u(ctx, t0, arg2);
    tcg_gen_trunc_tl_i32(t1, t0);
    tcg_temp_free(t0);
    gen_helper_float32_to_float64(arg1, t1);
    tcg_temp_free_i32(t1);
}

 /* lfd lfdu lfdux lfdx */
GEN_LDFS(lfd, ld64, 0x12, PPC_FLOAT);
 /* lfs lfsu lfsux lfsx */
GEN_LDFS(lfs, ld32fs, 0x10, PPC_FLOAT);

/***                         Floating-point store                          ***/
#define GEN_STF(name, stop, opc, type)                                        \
GEN_HANDLER(name, opc, 0xFF, 0xFF, 0x00000000, type)                          \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_FLOAT);                                   \
    EA = tcg_temp_new();                                                      \
    gen_addr_imm_index(ctx, EA, 0);                                           \
    gen_qemu_##stop(ctx, cpu_fpr[rS(ctx->opcode)], EA);                       \
    tcg_temp_free(EA);                                                        \
}

#define GEN_STUF(name, stop, opc, type)                                       \
GEN_HANDLER(name##u, opc, 0xFF, 0xFF, 0x00000000, type)                       \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    if (unlikely(rA(ctx->opcode) == 0)) {                                     \
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);                   \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_FLOAT);                                   \
    EA = tcg_temp_new();                                                      \
    gen_addr_imm_index(ctx, EA, 0);                                           \
    gen_qemu_##stop(ctx, cpu_fpr[rS(ctx->opcode)], EA);                       \
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], EA);                             \
    tcg_temp_free(EA);                                                        \
}

#define GEN_STUXF(name, stop, opc, type)                                      \
GEN_HANDLER(name##ux, 0x1F, 0x17, opc, 0x00000001, type)                      \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    if (unlikely(rA(ctx->opcode) == 0)) {                                     \
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);                   \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_FLOAT);                                   \
    EA = tcg_temp_new();                                                      \
    gen_addr_reg_index(ctx, EA);                                              \
    gen_qemu_##stop(ctx, cpu_fpr[rS(ctx->opcode)], EA);                       \
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], EA);                             \
    tcg_temp_free(EA);                                                        \
}

#define GEN_STXF(name, stop, opc2, opc3, type)                                \
GEN_HANDLER(name##x, 0x1F, opc2, opc3, 0x00000001, type)                      \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(!ctx->fpu_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_FPU);                                 \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_FLOAT);                                   \
    EA = tcg_temp_new();                                                      \
    gen_addr_reg_index(ctx, EA);                                              \
    gen_qemu_##stop(ctx, cpu_fpr[rS(ctx->opcode)], EA);                       \
    tcg_temp_free(EA);                                                        \
}

#define GEN_STFS(name, stop, op, type)                                        \
GEN_STF(name, stop, op | 0x20, type);                                         \
GEN_STUF(name, stop, op | 0x21, type);                                        \
GEN_STUXF(name, stop, op | 0x01, type);                                       \
GEN_STXF(name, stop, 0x17, op | 0x00, type)

static always_inline void gen_qemu_st32fs(DisasContext *ctx, TCGv_i64 arg1, TCGv arg2)
{
    TCGv_i32 t0 = tcg_temp_new_i32();
    TCGv t1 = tcg_temp_new();
    gen_helper_float64_to_float32(t0, arg1);
    tcg_gen_extu_i32_tl(t1, t0);
    tcg_temp_free_i32(t0);
    gen_qemu_st32(ctx, t1, arg2);
    tcg_temp_free(t1);
}

/* stfd stfdu stfdux stfdx */
GEN_STFS(stfd, st64, 0x16, PPC_FLOAT);
/* stfs stfsu stfsux stfsx */
GEN_STFS(stfs, st32fs, 0x14, PPC_FLOAT);

/* Optional: */
static always_inline void gen_qemu_st32fiw(DisasContext *ctx, TCGv_i64 arg1, TCGv arg2)
{
    TCGv t0 = tcg_temp_new();
    tcg_gen_trunc_i64_tl(t0, arg1),
    gen_qemu_st32(ctx, t0, arg2);
    tcg_temp_free(t0);
}
/* stfiwx */
GEN_STXF(stfiw, st32fiw, 0x17, 0x1E, PPC_FLOAT_STFIWX);

/***                                Branch                                 ***/
static always_inline void gen_goto_tb (DisasContext *ctx, int n,
                                       target_ulong dest)
{
    TranslationBlock *tb;
    tb = ctx->tb;
#if defined(TARGET_PPC64)
    if (!ctx->sf_mode)
        dest = (uint32_t) dest;
#endif
    if ((tb->pc & TARGET_PAGE_MASK) == (dest & TARGET_PAGE_MASK) &&
        likely(!ctx->singlestep_enabled)) {
        tcg_gen_goto_tb(n);
        tcg_gen_movi_tl(cpu_nip, dest & ~3);
        tcg_gen_exit_tb((long)tb + n);
    } else {
        tcg_gen_movi_tl(cpu_nip, dest & ~3);
        if (unlikely(ctx->singlestep_enabled)) {
            if ((ctx->singlestep_enabled &
                (CPU_BRANCH_STEP | CPU_SINGLE_STEP)) &&
                ctx->exception == POWERPC_EXCP_BRANCH) {
                target_ulong tmp = ctx->nip;
                ctx->nip = dest;
                gen_exception(ctx, POWERPC_EXCP_TRACE);
                ctx->nip = tmp;
            }
            if (ctx->singlestep_enabled & GDBSTUB_SINGLE_STEP) {
                gen_debug_exception(ctx);
            }
        }
        tcg_gen_exit_tb(0);
    }
}

static always_inline void gen_setlr (DisasContext *ctx, target_ulong nip)
{
#if defined(TARGET_PPC64)
    if (ctx->sf_mode == 0)
        tcg_gen_movi_tl(cpu_lr, (uint32_t)nip);
    else
#endif
        tcg_gen_movi_tl(cpu_lr, nip);
}

/* b ba bl bla */
GEN_HANDLER(b, 0x12, 0xFF, 0xFF, 0x00000000, PPC_FLOW)
{
    target_ulong li, target;

    ctx->exception = POWERPC_EXCP_BRANCH;
    /* sign extend LI */
#if defined(TARGET_PPC64)
    if (ctx->sf_mode)
        li = ((int64_t)LI(ctx->opcode) << 38) >> 38;
    else
#endif
        li = ((int32_t)LI(ctx->opcode) << 6) >> 6;
    if (likely(AA(ctx->opcode) == 0))
        target = ctx->nip + li - 4;
    else
        target = li;
    if (LK(ctx->opcode))
        gen_setlr(ctx, ctx->nip);
    gen_goto_tb(ctx, 0, target);
}

#define BCOND_IM  0
#define BCOND_LR  1
#define BCOND_CTR 2

static always_inline void gen_bcond (DisasContext *ctx, int type)
{
    uint32_t bo = BO(ctx->opcode);
    int l1 = gen_new_label();
    TCGv target;

    ctx->exception = POWERPC_EXCP_BRANCH;
    if (type == BCOND_LR || type == BCOND_CTR) {
        target = tcg_temp_local_new();
        if (type == BCOND_CTR)
            tcg_gen_mov_tl(target, cpu_ctr);
        else
            tcg_gen_mov_tl(target, cpu_lr);
    }
    if (LK(ctx->opcode))
        gen_setlr(ctx, ctx->nip);
    l1 = gen_new_label();
    if ((bo & 0x4) == 0) {
        /* Decrement and test CTR */
        TCGv temp = tcg_temp_new();
        if (unlikely(type == BCOND_CTR)) {
            gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
            return;
        }
        tcg_gen_subi_tl(cpu_ctr, cpu_ctr, 1);
#if defined(TARGET_PPC64)
        if (!ctx->sf_mode)
            tcg_gen_ext32u_tl(temp, cpu_ctr);
        else
#endif
            tcg_gen_mov_tl(temp, cpu_ctr);
        if (bo & 0x2) {
            tcg_gen_brcondi_tl(TCG_COND_NE, temp, 0, l1);
        } else {
            tcg_gen_brcondi_tl(TCG_COND_EQ, temp, 0, l1);
        }
        tcg_temp_free(temp);
    }
    if ((bo & 0x10) == 0) {
        /* Test CR */
        uint32_t bi = BI(ctx->opcode);
        uint32_t mask = 1 << (3 - (bi & 0x03));
        TCGv_i32 temp = tcg_temp_new_i32();

        if (bo & 0x8) {
            tcg_gen_andi_i32(temp, cpu_crf[bi >> 2], mask);
            tcg_gen_brcondi_i32(TCG_COND_EQ, temp, 0, l1);
        } else {
            tcg_gen_andi_i32(temp, cpu_crf[bi >> 2], mask);
            tcg_gen_brcondi_i32(TCG_COND_NE, temp, 0, l1);
        }
        tcg_temp_free_i32(temp);
    }
    if (type == BCOND_IM) {
        target_ulong li = (target_long)((int16_t)(BD(ctx->opcode)));
        if (likely(AA(ctx->opcode) == 0)) {
            gen_goto_tb(ctx, 0, ctx->nip + li - 4);
        } else {
            gen_goto_tb(ctx, 0, li);
        }
        gen_set_label(l1);
        gen_goto_tb(ctx, 1, ctx->nip);
    } else {
#if defined(TARGET_PPC64)
        if (!(ctx->sf_mode))
            tcg_gen_andi_tl(cpu_nip, target, (uint32_t)~3);
        else
#endif
            tcg_gen_andi_tl(cpu_nip, target, ~3);
        tcg_gen_exit_tb(0);
        gen_set_label(l1);
#if defined(TARGET_PPC64)
        if (!(ctx->sf_mode))
            tcg_gen_movi_tl(cpu_nip, (uint32_t)ctx->nip);
        else
#endif
            tcg_gen_movi_tl(cpu_nip, ctx->nip);
        tcg_gen_exit_tb(0);
    }
}

GEN_HANDLER(bc, 0x10, 0xFF, 0xFF, 0x00000000, PPC_FLOW)
{
    gen_bcond(ctx, BCOND_IM);
}

GEN_HANDLER(bcctr, 0x13, 0x10, 0x10, 0x00000000, PPC_FLOW)
{
    gen_bcond(ctx, BCOND_CTR);
}

GEN_HANDLER(bclr, 0x13, 0x10, 0x00, 0x00000000, PPC_FLOW)
{
    gen_bcond(ctx, BCOND_LR);
}

/***                      Condition register logical                       ***/
#define GEN_CRLOGIC(name, tcg_op, opc)                                        \
GEN_HANDLER(name, 0x13, 0x01, opc, 0x00000001, PPC_INTEGER)                   \
{                                                                             \
    uint8_t bitmask;                                                          \
    int sh;                                                                   \
    TCGv_i32 t0, t1;                                                          \
    sh = (crbD(ctx->opcode) & 0x03) - (crbA(ctx->opcode) & 0x03);             \
    t0 = tcg_temp_new_i32();                                                  \
    if (sh > 0)                                                               \
        tcg_gen_shri_i32(t0, cpu_crf[crbA(ctx->opcode) >> 2], sh);            \
    else if (sh < 0)                                                          \
        tcg_gen_shli_i32(t0, cpu_crf[crbA(ctx->opcode) >> 2], -sh);           \
    else                                                                      \
        tcg_gen_mov_i32(t0, cpu_crf[crbA(ctx->opcode) >> 2]);                 \
    t1 = tcg_temp_new_i32();                                                  \
    sh = (crbD(ctx->opcode) & 0x03) - (crbB(ctx->opcode) & 0x03);             \
    if (sh > 0)                                                               \
        tcg_gen_shri_i32(t1, cpu_crf[crbB(ctx->opcode) >> 2], sh);            \
    else if (sh < 0)                                                          \
        tcg_gen_shli_i32(t1, cpu_crf[crbB(ctx->opcode) >> 2], -sh);           \
    else                                                                      \
        tcg_gen_mov_i32(t1, cpu_crf[crbB(ctx->opcode) >> 2]);                 \
    tcg_op(t0, t0, t1);                                                       \
    bitmask = 1 << (3 - (crbD(ctx->opcode) & 0x03));                          \
    tcg_gen_andi_i32(t0, t0, bitmask);                                        \
    tcg_gen_andi_i32(t1, cpu_crf[crbD(ctx->opcode) >> 2], ~bitmask);          \
    tcg_gen_or_i32(cpu_crf[crbD(ctx->opcode) >> 2], t0, t1);                  \
    tcg_temp_free_i32(t0);                                                    \
    tcg_temp_free_i32(t1);                                                    \
}

/* crand */
GEN_CRLOGIC(crand, tcg_gen_and_i32, 0x08);
/* crandc */
GEN_CRLOGIC(crandc, tcg_gen_andc_i32, 0x04);
/* creqv */
GEN_CRLOGIC(creqv, tcg_gen_eqv_i32, 0x09);
/* crnand */
GEN_CRLOGIC(crnand, tcg_gen_nand_i32, 0x07);
/* crnor */
GEN_CRLOGIC(crnor, tcg_gen_nor_i32, 0x01);
/* cror */
GEN_CRLOGIC(cror, tcg_gen_or_i32, 0x0E);
/* crorc */
GEN_CRLOGIC(crorc, tcg_gen_orc_i32, 0x0D);
/* crxor */
GEN_CRLOGIC(crxor, tcg_gen_xor_i32, 0x06);
/* mcrf */
GEN_HANDLER(mcrf, 0x13, 0x00, 0xFF, 0x00000001, PPC_INTEGER)
{
    tcg_gen_mov_i32(cpu_crf[crfD(ctx->opcode)], cpu_crf[crfS(ctx->opcode)]);
}

/***                           System linkage                              ***/
/* rfi (mem_idx only) */
GEN_HANDLER(rfi, 0x13, 0x12, 0x01, 0x03FF8001, PPC_FLOW)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    /* Restore CPU state */
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_helper_rfi();
    gen_sync_exception(ctx);
#endif
}

#if defined(TARGET_PPC64)
GEN_HANDLER(rfid, 0x13, 0x12, 0x00, 0x03FF8001, PPC_64B)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    /* Restore CPU state */
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_helper_rfid();
    gen_sync_exception(ctx);
#endif
}

GEN_HANDLER(hrfid, 0x13, 0x12, 0x08, 0x03FF8001, PPC_64H)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    /* Restore CPU state */
    if (unlikely(ctx->mem_idx <= 1)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_helper_hrfid();
    gen_sync_exception(ctx);
#endif
}
#endif

/* sc */
#if defined(CONFIG_USER_ONLY)
#define POWERPC_SYSCALL POWERPC_EXCP_SYSCALL_USER
#else
#define POWERPC_SYSCALL POWERPC_EXCP_SYSCALL
#endif
GEN_HANDLER(sc, 0x11, 0xFF, 0xFF, 0x03FFF01D, PPC_FLOW)
{
    uint32_t lev;

    lev = (ctx->opcode >> 5) & 0x7F;
    gen_exception_err(ctx, POWERPC_SYSCALL, lev);
}

/***                                Trap                                   ***/
/* tw */
GEN_HANDLER(tw, 0x1F, 0x04, 0x00, 0x00000001, PPC_FLOW)
{
    TCGv_i32 t0 = tcg_const_i32(TO(ctx->opcode));
    /* Update the nip since this might generate a trap exception */
    gen_update_nip(ctx, ctx->nip);
    gen_helper_tw(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)], t0);
    tcg_temp_free_i32(t0);
}

/* twi */
GEN_HANDLER(twi, 0x03, 0xFF, 0xFF, 0x00000000, PPC_FLOW)
{
    TCGv t0 = tcg_const_tl(SIMM(ctx->opcode));
    TCGv_i32 t1 = tcg_const_i32(TO(ctx->opcode));
    /* Update the nip since this might generate a trap exception */
    gen_update_nip(ctx, ctx->nip);
    gen_helper_tw(cpu_gpr[rA(ctx->opcode)], t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free_i32(t1);
}

#if defined(TARGET_PPC64)
/* td */
GEN_HANDLER(td, 0x1F, 0x04, 0x02, 0x00000001, PPC_64B)
{
    TCGv_i32 t0 = tcg_const_i32(TO(ctx->opcode));
    /* Update the nip since this might generate a trap exception */
    gen_update_nip(ctx, ctx->nip);
    gen_helper_td(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)], t0);
    tcg_temp_free_i32(t0);
}

/* tdi */
GEN_HANDLER(tdi, 0x02, 0xFF, 0xFF, 0x00000000, PPC_64B)
{
    TCGv t0 = tcg_const_tl(SIMM(ctx->opcode));
    TCGv_i32 t1 = tcg_const_i32(TO(ctx->opcode));
    /* Update the nip since this might generate a trap exception */
    gen_update_nip(ctx, ctx->nip);
    gen_helper_td(cpu_gpr[rA(ctx->opcode)], t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free_i32(t1);
}
#endif

/***                          Processor control                            ***/
/* mcrxr */
GEN_HANDLER(mcrxr, 0x1F, 0x00, 0x10, 0x007FF801, PPC_MISC)
{
    tcg_gen_trunc_tl_i32(cpu_crf[crfD(ctx->opcode)], cpu_xer);
    tcg_gen_shri_i32(cpu_crf[crfD(ctx->opcode)], cpu_crf[crfD(ctx->opcode)], XER_CA);
    tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_SO | 1 << XER_OV | 1 << XER_CA));
}

/* mfcr */
GEN_HANDLER(mfcr, 0x1F, 0x13, 0x00, 0x00000801, PPC_MISC)
{
    uint32_t crm, crn;

    if (likely(ctx->opcode & 0x00100000)) {
        crm = CRM(ctx->opcode);
        if (likely((crm ^ (crm - 1)) == 0)) {
            crn = ffs(crm);
            tcg_gen_extu_i32_tl(cpu_gpr[rD(ctx->opcode)], cpu_crf[7 - crn]);
        }
    } else {
        gen_helper_load_cr(cpu_gpr[rD(ctx->opcode)]);
    }
}

/* mfmsr */
GEN_HANDLER(mfmsr, 0x1F, 0x13, 0x02, 0x001FF801, PPC_MISC)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], cpu_msr);
#endif
}

#if 1
#define SPR_NOACCESS ((void *)(-1UL))
#else
static void spr_noaccess (void *opaque, int sprn)
{
    sprn = ((sprn >> 5) & 0x1F) | ((sprn & 0x1F) << 5);
    printf("ERROR: try to access SPR %d !\n", sprn);
}
#define SPR_NOACCESS (&spr_noaccess)
#endif

/* mfspr */
static always_inline void gen_op_mfspr (DisasContext *ctx)
{
    void (*read_cb)(void *opaque, int gprn, int sprn);
    uint32_t sprn = SPR(ctx->opcode);

#if !defined(CONFIG_USER_ONLY)
    if (ctx->mem_idx == 2)
        read_cb = ctx->spr_cb[sprn].hea_read;
    else if (ctx->mem_idx)
        read_cb = ctx->spr_cb[sprn].oea_read;
    else
#endif
        read_cb = ctx->spr_cb[sprn].uea_read;
    if (likely(read_cb != NULL)) {
        if (likely(read_cb != SPR_NOACCESS)) {
            (*read_cb)(ctx, rD(ctx->opcode), sprn);
        } else {
            /* Privilege exception */
            /* This is a hack to avoid warnings when running Linux:
             * this OS breaks the PowerPC virtualisation model,
             * allowing userland application to read the PVR
             */
            if (sprn != SPR_PVR) {
                qemu_log("Trying to read privileged spr %d %03x at "
                            ADDRX "\n", sprn, sprn, ctx->nip);
                printf("Trying to read privileged spr %d %03x at " ADDRX "\n",
                       sprn, sprn, ctx->nip);
            }
            gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        }
    } else {
        /* Not defined */
        qemu_log("Trying to read invalid spr %d %03x at "
                    ADDRX "\n", sprn, sprn, ctx->nip);
        printf("Trying to read invalid spr %d %03x at " ADDRX "\n",
               sprn, sprn, ctx->nip);
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_SPR);
    }
}

GEN_HANDLER(mfspr, 0x1F, 0x13, 0x0A, 0x00000001, PPC_MISC)
{
    gen_op_mfspr(ctx);
}

/* mftb */
GEN_HANDLER(mftb, 0x1F, 0x13, 0x0B, 0x00000001, PPC_MFTB)
{
    gen_op_mfspr(ctx);
}

/* mtcrf */
GEN_HANDLER(mtcrf, 0x1F, 0x10, 0x04, 0x00000801, PPC_MISC)
{
    uint32_t crm, crn;

    crm = CRM(ctx->opcode);
    if (likely((ctx->opcode & 0x00100000) || (crm ^ (crm - 1)) == 0)) {
        TCGv_i32 temp = tcg_temp_new_i32();
        crn = ffs(crm);
        tcg_gen_trunc_tl_i32(temp, cpu_gpr[rS(ctx->opcode)]);
        tcg_gen_shri_i32(cpu_crf[7 - crn], temp, crn * 4);
        tcg_gen_andi_i32(cpu_crf[7 - crn], cpu_crf[7 - crn], 0xf);
        tcg_temp_free_i32(temp);
    } else {
        TCGv_i32 temp = tcg_const_i32(crm);
        gen_helper_store_cr(cpu_gpr[rS(ctx->opcode)], temp);
        tcg_temp_free_i32(temp);
    }
}

/* mtmsr */
#if defined(TARGET_PPC64)
GEN_HANDLER(mtmsrd, 0x1F, 0x12, 0x05, 0x001EF801, PPC_64B)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    if (ctx->opcode & 0x00010000) {
        /* Special form that does not need any synchronisation */
        TCGv t0 = tcg_temp_new();
        tcg_gen_andi_tl(t0, cpu_gpr[rS(ctx->opcode)], (1 << MSR_RI) | (1 << MSR_EE));
        tcg_gen_andi_tl(cpu_msr, cpu_msr, ~((1 << MSR_RI) | (1 << MSR_EE)));
        tcg_gen_or_tl(cpu_msr, cpu_msr, t0);
        tcg_temp_free(t0);
    } else {
        /* XXX: we need to update nip before the store
         *      if we enter power saving mode, we will exit the loop
         *      directly from ppc_store_msr
         */
        gen_update_nip(ctx, ctx->nip);
        gen_helper_store_msr(cpu_gpr[rS(ctx->opcode)]);
        /* Must stop the translation as machine state (may have) changed */
        /* Note that mtmsr is not always defined as context-synchronizing */
        gen_stop_exception(ctx);
    }
#endif
}
#endif

GEN_HANDLER(mtmsr, 0x1F, 0x12, 0x04, 0x001FF801, PPC_MISC)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    if (ctx->opcode & 0x00010000) {
        /* Special form that does not need any synchronisation */
        TCGv t0 = tcg_temp_new();
        tcg_gen_andi_tl(t0, cpu_gpr[rS(ctx->opcode)], (1 << MSR_RI) | (1 << MSR_EE));
        tcg_gen_andi_tl(cpu_msr, cpu_msr, ~((1 << MSR_RI) | (1 << MSR_EE)));
        tcg_gen_or_tl(cpu_msr, cpu_msr, t0);
        tcg_temp_free(t0);
    } else {
        /* XXX: we need to update nip before the store
         *      if we enter power saving mode, we will exit the loop
         *      directly from ppc_store_msr
         */
        gen_update_nip(ctx, ctx->nip);
#if defined(TARGET_PPC64)
        if (!ctx->sf_mode) {
            TCGv t0 = tcg_temp_new();
            TCGv t1 = tcg_temp_new();
            tcg_gen_andi_tl(t0, cpu_msr, 0xFFFFFFFF00000000ULL);
            tcg_gen_ext32u_tl(t1, cpu_gpr[rS(ctx->opcode)]);
            tcg_gen_or_tl(t0, t0, t1);
            tcg_temp_free(t1);
            gen_helper_store_msr(t0);
            tcg_temp_free(t0);
        } else
#endif
            gen_helper_store_msr(cpu_gpr[rS(ctx->opcode)]);
        /* Must stop the translation as machine state (may have) changed */
        /* Note that mtmsr is not always defined as context-synchronizing */
        gen_stop_exception(ctx);
    }
#endif
}

/* mtspr */
GEN_HANDLER(mtspr, 0x1F, 0x13, 0x0E, 0x00000001, PPC_MISC)
{
    void (*write_cb)(void *opaque, int sprn, int gprn);
    uint32_t sprn = SPR(ctx->opcode);

#if !defined(CONFIG_USER_ONLY)
    if (ctx->mem_idx == 2)
        write_cb = ctx->spr_cb[sprn].hea_write;
    else if (ctx->mem_idx)
        write_cb = ctx->spr_cb[sprn].oea_write;
    else
#endif
        write_cb = ctx->spr_cb[sprn].uea_write;
    if (likely(write_cb != NULL)) {
        if (likely(write_cb != SPR_NOACCESS)) {
            (*write_cb)(ctx, sprn, rS(ctx->opcode));
        } else {
            /* Privilege exception */
            qemu_log("Trying to write privileged spr %d %03x at "
                        ADDRX "\n", sprn, sprn, ctx->nip);
            printf("Trying to write privileged spr %d %03x at " ADDRX "\n",
                   sprn, sprn, ctx->nip);
            gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        }
    } else {
        /* Not defined */
        qemu_log("Trying to write invalid spr %d %03x at "
                    ADDRX "\n", sprn, sprn, ctx->nip);
        printf("Trying to write invalid spr %d %03x at " ADDRX "\n",
               sprn, sprn, ctx->nip);
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_SPR);
    }
}

/***                         Cache management                              ***/
/* dcbf */
GEN_HANDLER(dcbf, 0x1F, 0x16, 0x02, 0x03C00001, PPC_CACHE)
{
    /* XXX: specification says this is treated as a load by the MMU */
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_CACHE);
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_qemu_ld8u(ctx, t0, t0);
    tcg_temp_free(t0);
}

/* dcbi (Supervisor only) */
GEN_HANDLER(dcbi, 0x1F, 0x16, 0x0E, 0x03E00001, PPC_CACHE)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    TCGv EA, val;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    EA = tcg_temp_new();
    gen_set_access_type(ctx, ACCESS_CACHE);
    gen_addr_reg_index(ctx, EA);
    val = tcg_temp_new();
    /* XXX: specification says this should be treated as a store by the MMU */
    gen_qemu_ld8u(ctx, val, EA);
    gen_qemu_st8(ctx, val, EA);
    tcg_temp_free(val);
    tcg_temp_free(EA);
#endif
}

/* dcdst */
GEN_HANDLER(dcbst, 0x1F, 0x16, 0x01, 0x03E00001, PPC_CACHE)
{
    /* XXX: specification say this is treated as a load by the MMU */
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_CACHE);
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_qemu_ld8u(ctx, t0, t0);
    tcg_temp_free(t0);
}

/* dcbt */
GEN_HANDLER(dcbt, 0x1F, 0x16, 0x08, 0x02000001, PPC_CACHE)
{
    /* interpreted as no-op */
    /* XXX: specification say this is treated as a load by the MMU
     *      but does not generate any exception
     */
}

/* dcbtst */
GEN_HANDLER(dcbtst, 0x1F, 0x16, 0x07, 0x02000001, PPC_CACHE)
{
    /* interpreted as no-op */
    /* XXX: specification say this is treated as a load by the MMU
     *      but does not generate any exception
     */
}

/* dcbz */
GEN_HANDLER(dcbz, 0x1F, 0x16, 0x1F, 0x03E00001, PPC_CACHE_DCBZ)
{
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_CACHE);
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_helper_dcbz(t0);
    tcg_temp_free(t0);
}

GEN_HANDLER2(dcbz_970, "dcbz", 0x1F, 0x16, 0x1F, 0x03C00001, PPC_CACHE_DCBZT)
{
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_CACHE);
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    if (ctx->opcode & 0x00200000)
        gen_helper_dcbz(t0);
    else
        gen_helper_dcbz_970(t0);
    tcg_temp_free(t0);
}

/* dst / dstt */
GEN_HANDLER(dst, 0x1F, 0x16, 0x0A, 0x01800001, PPC_ALTIVEC)
{
    if (rA(ctx->opcode) == 0) {
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_LSWX);
    } else {
        /* interpreted as no-op */
    }
}

/* dstst /dststt */
GEN_HANDLER(dstst, 0x1F, 0x16, 0x0B, 0x02000001, PPC_ALTIVEC)
{
    if (rA(ctx->opcode) == 0) {
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_LSWX);
    } else {
        /* interpreted as no-op */
    }

}

/* dss / dssall */
GEN_HANDLER(dss, 0x1F, 0x16, 0x19, 0x019FF801, PPC_ALTIVEC)
{
    /* interpreted as no-op */
}

/* icbi */
GEN_HANDLER(icbi, 0x1F, 0x16, 0x1E, 0x03E00001, PPC_CACHE_ICBI)
{
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_CACHE);
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_helper_icbi(t0);
    tcg_temp_free(t0);
}

/* Optional: */
/* dcba */
GEN_HANDLER(dcba, 0x1F, 0x16, 0x17, 0x03E00001, PPC_CACHE_DCBA)
{
    /* interpreted as no-op */
    /* XXX: specification say this is treated as a store by the MMU
     *      but does not generate any exception
     */
}

/***                    Segment register manipulation                      ***/
/* Supervisor only: */
/* mfsr */
GEN_HANDLER(mfsr, 0x1F, 0x13, 0x12, 0x0010F801, PPC_SEGMENT)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    t0 = tcg_const_tl(SR(ctx->opcode));
    gen_helper_load_sr(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
#endif
}

/* mfsrin */
GEN_HANDLER(mfsrin, 0x1F, 0x13, 0x14, 0x001F0001, PPC_SEGMENT)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    t0 = tcg_temp_new();
    tcg_gen_shri_tl(t0, cpu_gpr[rB(ctx->opcode)], 28);
    tcg_gen_andi_tl(t0, t0, 0xF);
    gen_helper_load_sr(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
#endif
}

/* mtsr */
GEN_HANDLER(mtsr, 0x1F, 0x12, 0x06, 0x0010F801, PPC_SEGMENT)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    t0 = tcg_const_tl(SR(ctx->opcode));
    gen_helper_store_sr(t0, cpu_gpr[rS(ctx->opcode)]);
    tcg_temp_free(t0);
#endif
}

/* mtsrin */
GEN_HANDLER(mtsrin, 0x1F, 0x12, 0x07, 0x001F0001, PPC_SEGMENT)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    t0 = tcg_temp_new();
    tcg_gen_shri_tl(t0, cpu_gpr[rB(ctx->opcode)], 28);
    tcg_gen_andi_tl(t0, t0, 0xF);
    gen_helper_store_sr(t0, cpu_gpr[rD(ctx->opcode)]);
    tcg_temp_free(t0);
#endif
}

#if defined(TARGET_PPC64)
/* Specific implementation for PowerPC 64 "bridge" emulation using SLB */
/* mfsr */
GEN_HANDLER2(mfsr_64b, "mfsr", 0x1F, 0x13, 0x12, 0x0010F801, PPC_SEGMENT_64B)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    t0 = tcg_const_tl(SR(ctx->opcode));
    gen_helper_load_slb(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
#endif
}

/* mfsrin */
GEN_HANDLER2(mfsrin_64b, "mfsrin", 0x1F, 0x13, 0x14, 0x001F0001,
             PPC_SEGMENT_64B)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    t0 = tcg_temp_new();
    tcg_gen_shri_tl(t0, cpu_gpr[rB(ctx->opcode)], 28);
    tcg_gen_andi_tl(t0, t0, 0xF);
    gen_helper_load_slb(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
#endif
}

/* mtsr */
GEN_HANDLER2(mtsr_64b, "mtsr", 0x1F, 0x12, 0x06, 0x0010F801, PPC_SEGMENT_64B)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    t0 = tcg_const_tl(SR(ctx->opcode));
    gen_helper_store_slb(t0, cpu_gpr[rS(ctx->opcode)]);
    tcg_temp_free(t0);
#endif
}

/* mtsrin */
GEN_HANDLER2(mtsrin_64b, "mtsrin", 0x1F, 0x12, 0x07, 0x001F0001,
             PPC_SEGMENT_64B)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    t0 = tcg_temp_new();
    tcg_gen_shri_tl(t0, cpu_gpr[rB(ctx->opcode)], 28);
    tcg_gen_andi_tl(t0, t0, 0xF);
    gen_helper_store_slb(t0, cpu_gpr[rS(ctx->opcode)]);
    tcg_temp_free(t0);
#endif
}
#endif /* defined(TARGET_PPC64) */

/***                      Lookaside buffer management                      ***/
/* Optional & mem_idx only: */
/* tlbia */
GEN_HANDLER(tlbia, 0x1F, 0x12, 0x0B, 0x03FFFC01, PPC_MEM_TLBIA)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_helper_tlbia();
#endif
}

/* tlbie */
GEN_HANDLER(tlbie, 0x1F, 0x12, 0x09, 0x03FF0001, PPC_MEM_TLBIE)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
#if defined(TARGET_PPC64)
    if (!ctx->sf_mode) {
        TCGv t0 = tcg_temp_new();
        tcg_gen_ext32u_tl(t0, cpu_gpr[rB(ctx->opcode)]);
        gen_helper_tlbie(t0);
        tcg_temp_free(t0);
    } else
#endif
        gen_helper_tlbie(cpu_gpr[rB(ctx->opcode)]);
#endif
}

/* tlbsync */
GEN_HANDLER(tlbsync, 0x1F, 0x16, 0x11, 0x03FFF801, PPC_MEM_TLBSYNC)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    /* This has no effect: it should ensure that all previous
     * tlbie have completed
     */
    gen_stop_exception(ctx);
#endif
}

#if defined(TARGET_PPC64)
/* slbia */
GEN_HANDLER(slbia, 0x1F, 0x12, 0x0F, 0x03FFFC01, PPC_SLBI)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_helper_slbia();
#endif
}

/* slbie */
GEN_HANDLER(slbie, 0x1F, 0x12, 0x0D, 0x03FF0001, PPC_SLBI)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_helper_slbie(cpu_gpr[rB(ctx->opcode)]);
#endif
}
#endif

/***                              External control                         ***/
/* Optional: */
/* eciwx */
GEN_HANDLER(eciwx, 0x1F, 0x16, 0x0D, 0x00000001, PPC_EXTERN)
{
    TCGv t0;
    /* Should check EAR[E] ! */
    gen_set_access_type(ctx, ACCESS_EXT);
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_check_align(ctx, t0, 0x03);
    gen_qemu_ld32u(ctx, cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
}

/* ecowx */
GEN_HANDLER(ecowx, 0x1F, 0x16, 0x09, 0x00000001, PPC_EXTERN)
{
    TCGv t0;
    /* Should check EAR[E] ! */
    gen_set_access_type(ctx, ACCESS_EXT);
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_check_align(ctx, t0, 0x03);
    gen_qemu_st32(ctx, cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
}

/* PowerPC 601 specific instructions */
/* abs - abs. */
GEN_HANDLER(abs, 0x1F, 0x08, 0x0B, 0x0000F800, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    tcg_gen_brcondi_tl(TCG_COND_GE, cpu_gpr[rA(ctx->opcode)], 0, l1);
    tcg_gen_neg_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    gen_set_label(l2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* abso - abso. */
GEN_HANDLER(abso, 0x1F, 0x08, 0x1B, 0x0000F800, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    int l3 = gen_new_label();
    /* Start with XER OV disabled, the most likely case */
    tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_OV));
    tcg_gen_brcondi_tl(TCG_COND_GE, cpu_gpr[rA(ctx->opcode)], 0, l2);
    tcg_gen_brcondi_tl(TCG_COND_NE, cpu_gpr[rA(ctx->opcode)], 0x80000000, l1);
    tcg_gen_ori_tl(cpu_xer, cpu_xer, (1 << XER_OV) | (1 << XER_SO));
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_neg_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_br(l3);
    gen_set_label(l2);
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    gen_set_label(l3);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* clcs */
GEN_HANDLER(clcs, 0x1F, 0x10, 0x13, 0x0000F800, PPC_POWER_BR)
{
    TCGv_i32 t0 = tcg_const_i32(rA(ctx->opcode));
    gen_helper_clcs(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free_i32(t0);
    /* Rc=1 sets CR0 to an undefined state */
}

/* div - div. */
GEN_HANDLER(div, 0x1F, 0x0B, 0x0A, 0x00000000, PPC_POWER_BR)
{
    gen_helper_div(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* divo - divo. */
GEN_HANDLER(divo, 0x1F, 0x0B, 0x1A, 0x00000000, PPC_POWER_BR)
{
    gen_helper_divo(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* divs - divs. */
GEN_HANDLER(divs, 0x1F, 0x0B, 0x0B, 0x00000000, PPC_POWER_BR)
{
    gen_helper_divs(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* divso - divso. */
GEN_HANDLER(divso, 0x1F, 0x0B, 0x1B, 0x00000000, PPC_POWER_BR)
{
    gen_helper_divso(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* doz - doz. */
GEN_HANDLER(doz, 0x1F, 0x08, 0x08, 0x00000000, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    tcg_gen_brcond_tl(TCG_COND_GE, cpu_gpr[rB(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], l1);
    tcg_gen_sub_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rB(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_movi_tl(cpu_gpr[rD(ctx->opcode)], 0);
    gen_set_label(l2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* dozo - dozo. */
GEN_HANDLER(dozo, 0x1F, 0x08, 0x18, 0x00000000, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    TCGv t2 = tcg_temp_new();
    /* Start with XER OV disabled, the most likely case */
    tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_OV));
    tcg_gen_brcond_tl(TCG_COND_GE, cpu_gpr[rB(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], l1);
    tcg_gen_sub_tl(t0, cpu_gpr[rB(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_xor_tl(t1, cpu_gpr[rB(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_xor_tl(t2, cpu_gpr[rA(ctx->opcode)], t0);
    tcg_gen_andc_tl(t1, t1, t2);
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_gen_brcondi_tl(TCG_COND_GE, t1, 0, l2);
    tcg_gen_ori_tl(cpu_xer, cpu_xer, (1 << XER_OV) | (1 << XER_SO));
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_movi_tl(cpu_gpr[rD(ctx->opcode)], 0);
    gen_set_label(l2);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    tcg_temp_free(t2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* dozi */
GEN_HANDLER(dozi, 0x09, 0xFF, 0xFF, 0x00000000, PPC_POWER_BR)
{
    target_long simm = SIMM(ctx->opcode);
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    tcg_gen_brcondi_tl(TCG_COND_LT, cpu_gpr[rA(ctx->opcode)], simm, l1);
    tcg_gen_subfi_tl(cpu_gpr[rD(ctx->opcode)], simm, cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_movi_tl(cpu_gpr[rD(ctx->opcode)], 0);
    gen_set_label(l2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* lscbx - lscbx. */
GEN_HANDLER(lscbx, 0x1F, 0x15, 0x08, 0x00000000, PPC_POWER_BR)
{
    TCGv t0 = tcg_temp_new();
    TCGv_i32 t1 = tcg_const_i32(rD(ctx->opcode));
    TCGv_i32 t2 = tcg_const_i32(rA(ctx->opcode));
    TCGv_i32 t3 = tcg_const_i32(rB(ctx->opcode));

    gen_addr_reg_index(ctx, t0);
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    gen_helper_lscbx(t0, t0, t1, t2, t3);
    tcg_temp_free_i32(t1);
    tcg_temp_free_i32(t2);
    tcg_temp_free_i32(t3);
    tcg_gen_andi_tl(cpu_xer, cpu_xer, ~0x7F);
    tcg_gen_or_tl(cpu_xer, cpu_xer, t0);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, t0);
    tcg_temp_free(t0);
}

/* maskg - maskg. */
GEN_HANDLER(maskg, 0x1F, 0x1D, 0x00, 0x00000000, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    TCGv t2 = tcg_temp_new();
    TCGv t3 = tcg_temp_new();
    tcg_gen_movi_tl(t3, 0xFFFFFFFF);
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_andi_tl(t1, cpu_gpr[rS(ctx->opcode)], 0x1F);
    tcg_gen_addi_tl(t2, t0, 1);
    tcg_gen_shr_tl(t2, t3, t2);
    tcg_gen_shr_tl(t3, t3, t1);
    tcg_gen_xor_tl(cpu_gpr[rA(ctx->opcode)], t2, t3);
    tcg_gen_brcond_tl(TCG_COND_GE, t0, t1, l1);
    tcg_gen_neg_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    gen_set_label(l1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    tcg_temp_free(t2);
    tcg_temp_free(t3);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* maskir - maskir. */
GEN_HANDLER(maskir, 0x1F, 0x1D, 0x10, 0x00000000, PPC_POWER_BR)
{
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_and_tl(t0, cpu_gpr[rS(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
    tcg_gen_andc_tl(t1, cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
    tcg_gen_or_tl(cpu_gpr[rA(ctx->opcode)], t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* mul - mul. */
GEN_HANDLER(mul, 0x1F, 0x0B, 0x03, 0x00000000, PPC_POWER_BR)
{
    TCGv_i64 t0 = tcg_temp_new_i64();
    TCGv_i64 t1 = tcg_temp_new_i64();
    TCGv t2 = tcg_temp_new();
    tcg_gen_extu_tl_i64(t0, cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_extu_tl_i64(t1, cpu_gpr[rB(ctx->opcode)]);
    tcg_gen_mul_i64(t0, t0, t1);
    tcg_gen_trunc_i64_tl(t2, t0);
    gen_store_spr(SPR_MQ, t2);
    tcg_gen_shri_i64(t1, t0, 32);
    tcg_gen_trunc_i64_tl(cpu_gpr[rD(ctx->opcode)], t1);
    tcg_temp_free_i64(t0);
    tcg_temp_free_i64(t1);
    tcg_temp_free(t2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* mulo - mulo. */
GEN_HANDLER(mulo, 0x1F, 0x0B, 0x13, 0x00000000, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    TCGv_i64 t0 = tcg_temp_new_i64();
    TCGv_i64 t1 = tcg_temp_new_i64();
    TCGv t2 = tcg_temp_new();
    /* Start with XER OV disabled, the most likely case */
    tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_OV));
    tcg_gen_extu_tl_i64(t0, cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_extu_tl_i64(t1, cpu_gpr[rB(ctx->opcode)]);
    tcg_gen_mul_i64(t0, t0, t1);
    tcg_gen_trunc_i64_tl(t2, t0);
    gen_store_spr(SPR_MQ, t2);
    tcg_gen_shri_i64(t1, t0, 32);
    tcg_gen_trunc_i64_tl(cpu_gpr[rD(ctx->opcode)], t1);
    tcg_gen_ext32s_i64(t1, t0);
    tcg_gen_brcond_i64(TCG_COND_EQ, t0, t1, l1);
    tcg_gen_ori_tl(cpu_xer, cpu_xer, (1 << XER_OV) | (1 << XER_SO));
    gen_set_label(l1);
    tcg_temp_free_i64(t0);
    tcg_temp_free_i64(t1);
    tcg_temp_free(t2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* nabs - nabs. */
GEN_HANDLER(nabs, 0x1F, 0x08, 0x0F, 0x00000000, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    tcg_gen_brcondi_tl(TCG_COND_GT, cpu_gpr[rA(ctx->opcode)], 0, l1);
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_neg_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    gen_set_label(l2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* nabso - nabso. */
GEN_HANDLER(nabso, 0x1F, 0x08, 0x1F, 0x00000000, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    tcg_gen_brcondi_tl(TCG_COND_GT, cpu_gpr[rA(ctx->opcode)], 0, l1);
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_neg_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    gen_set_label(l2);
    /* nabs never overflows */
    tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_OV));
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rD(ctx->opcode)]);
}

/* rlmi - rlmi. */
GEN_HANDLER(rlmi, 0x16, 0xFF, 0xFF, 0x00000000, PPC_POWER_BR)
{
    uint32_t mb = MB(ctx->opcode);
    uint32_t me = ME(ctx->opcode);
    TCGv t0 = tcg_temp_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_rotl_tl(t0, cpu_gpr[rS(ctx->opcode)], t0);
    tcg_gen_andi_tl(t0, t0, MASK(mb, me));
    tcg_gen_andi_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], ~MASK(mb, me));
    tcg_gen_or_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], t0);
    tcg_temp_free(t0);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* rrib - rrib. */
GEN_HANDLER(rrib, 0x1F, 0x19, 0x10, 0x00000000, PPC_POWER_BR)
{
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_movi_tl(t1, 0x80000000);
    tcg_gen_shr_tl(t1, t1, t0);
    tcg_gen_shr_tl(t0, cpu_gpr[rS(ctx->opcode)], t0);
    tcg_gen_and_tl(t0, t0, t1);
    tcg_gen_andc_tl(t1, cpu_gpr[rA(ctx->opcode)], t1);
    tcg_gen_or_tl(cpu_gpr[rA(ctx->opcode)], t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* sle - sle. */
GEN_HANDLER(sle, 0x1F, 0x19, 0x04, 0x00000000, PPC_POWER_BR)
{
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_andi_tl(t1, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_shl_tl(t0, cpu_gpr[rS(ctx->opcode)], t1);
    tcg_gen_subfi_tl(t1, 32, t1);
    tcg_gen_shr_tl(t1, cpu_gpr[rS(ctx->opcode)], t1);
    tcg_gen_or_tl(t1, t0, t1);
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], t0);
    gen_store_spr(SPR_MQ, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* sleq - sleq. */
GEN_HANDLER(sleq, 0x1F, 0x19, 0x06, 0x00000000, PPC_POWER_BR)
{
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    TCGv t2 = tcg_temp_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_movi_tl(t2, 0xFFFFFFFF);
    tcg_gen_shl_tl(t2, t2, t0);
    tcg_gen_rotl_tl(t0, cpu_gpr[rS(ctx->opcode)], t0);
    gen_load_spr(t1, SPR_MQ);
    gen_store_spr(SPR_MQ, t0);
    tcg_gen_and_tl(t0, t0, t2);
    tcg_gen_andc_tl(t1, t1, t2);
    tcg_gen_or_tl(cpu_gpr[rA(ctx->opcode)], t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    tcg_temp_free(t2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* sliq - sliq. */
GEN_HANDLER(sliq, 0x1F, 0x18, 0x05, 0x00000000, PPC_POWER_BR)
{
    int sh = SH(ctx->opcode);
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_shli_tl(t0, cpu_gpr[rS(ctx->opcode)], sh);
    tcg_gen_shri_tl(t1, cpu_gpr[rS(ctx->opcode)], 32 - sh);
    tcg_gen_or_tl(t1, t0, t1);
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], t0);
    gen_store_spr(SPR_MQ, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* slliq - slliq. */
GEN_HANDLER(slliq, 0x1F, 0x18, 0x07, 0x00000000, PPC_POWER_BR)
{
    int sh = SH(ctx->opcode);
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_rotli_tl(t0, cpu_gpr[rS(ctx->opcode)], sh);
    gen_load_spr(t1, SPR_MQ);
    gen_store_spr(SPR_MQ, t0);
    tcg_gen_andi_tl(t0, t0,  (0xFFFFFFFFU << sh));
    tcg_gen_andi_tl(t1, t1, ~(0xFFFFFFFFU << sh));
    tcg_gen_or_tl(cpu_gpr[rA(ctx->opcode)], t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* sllq - sllq. */
GEN_HANDLER(sllq, 0x1F, 0x18, 0x06, 0x00000000, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    TCGv t0 = tcg_temp_local_new();
    TCGv t1 = tcg_temp_local_new();
    TCGv t2 = tcg_temp_local_new();
    tcg_gen_andi_tl(t2, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_movi_tl(t1, 0xFFFFFFFF);
    tcg_gen_shl_tl(t1, t1, t2);
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x20);
    tcg_gen_brcondi_tl(TCG_COND_EQ, t0, 0, l1);
    gen_load_spr(t0, SPR_MQ);
    tcg_gen_and_tl(cpu_gpr[rA(ctx->opcode)], t0, t1);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_shl_tl(t0, cpu_gpr[rS(ctx->opcode)], t2);
    gen_load_spr(t2, SPR_MQ);
    tcg_gen_andc_tl(t1, t2, t1);
    tcg_gen_or_tl(cpu_gpr[rA(ctx->opcode)], t0, t1);
    gen_set_label(l2);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    tcg_temp_free(t2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* slq - slq. */
GEN_HANDLER(slq, 0x1F, 0x18, 0x04, 0x00000000, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_andi_tl(t1, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_shl_tl(t0, cpu_gpr[rS(ctx->opcode)], t1);
    tcg_gen_subfi_tl(t1, 32, t1);
    tcg_gen_shr_tl(t1, cpu_gpr[rS(ctx->opcode)], t1);
    tcg_gen_or_tl(t1, t0, t1);
    gen_store_spr(SPR_MQ, t1);
    tcg_gen_andi_tl(t1, cpu_gpr[rB(ctx->opcode)], 0x20);
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], t0);
    tcg_gen_brcondi_tl(TCG_COND_EQ, t1, 0, l1);
    tcg_gen_movi_tl(cpu_gpr[rA(ctx->opcode)], 0);
    gen_set_label(l1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* sraiq - sraiq. */
GEN_HANDLER(sraiq, 0x1F, 0x18, 0x1D, 0x00000000, PPC_POWER_BR)
{
    int sh = SH(ctx->opcode);
    int l1 = gen_new_label();
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_shri_tl(t0, cpu_gpr[rS(ctx->opcode)], sh);
    tcg_gen_shli_tl(t1, cpu_gpr[rS(ctx->opcode)], 32 - sh);
    tcg_gen_or_tl(t0, t0, t1);
    gen_store_spr(SPR_MQ, t0);
    tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_CA));
    tcg_gen_brcondi_tl(TCG_COND_EQ, t1, 0, l1);
    tcg_gen_brcondi_tl(TCG_COND_GE, cpu_gpr[rS(ctx->opcode)], 0, l1);
    tcg_gen_ori_tl(cpu_xer, cpu_xer, (1 << XER_CA));
    gen_set_label(l1);
    tcg_gen_sari_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], sh);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* sraq - sraq. */
GEN_HANDLER(sraq, 0x1F, 0x18, 0x1C, 0x00000000, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_local_new();
    TCGv t2 = tcg_temp_local_new();
    tcg_gen_andi_tl(t2, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_shr_tl(t0, cpu_gpr[rS(ctx->opcode)], t2);
    tcg_gen_sar_tl(t1, cpu_gpr[rS(ctx->opcode)], t2);
    tcg_gen_subfi_tl(t2, 32, t2);
    tcg_gen_shl_tl(t2, cpu_gpr[rS(ctx->opcode)], t2);
    tcg_gen_or_tl(t0, t0, t2);
    gen_store_spr(SPR_MQ, t0);
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x20);
    tcg_gen_brcondi_tl(TCG_COND_EQ, t2, 0, l1);
    tcg_gen_mov_tl(t2, cpu_gpr[rS(ctx->opcode)]);
    tcg_gen_sari_tl(t1, cpu_gpr[rS(ctx->opcode)], 31);
    gen_set_label(l1);
    tcg_temp_free(t0);
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], t1);
    tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_CA));
    tcg_gen_brcondi_tl(TCG_COND_GE, t1, 0, l2);
    tcg_gen_brcondi_tl(TCG_COND_EQ, t2, 0, l2);
    tcg_gen_ori_tl(cpu_xer, cpu_xer, (1 << XER_CA));
    gen_set_label(l2);
    tcg_temp_free(t1);
    tcg_temp_free(t2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* sre - sre. */
GEN_HANDLER(sre, 0x1F, 0x19, 0x14, 0x00000000, PPC_POWER_BR)
{
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_andi_tl(t1, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_shr_tl(t0, cpu_gpr[rS(ctx->opcode)], t1);
    tcg_gen_subfi_tl(t1, 32, t1);
    tcg_gen_shl_tl(t1, cpu_gpr[rS(ctx->opcode)], t1);
    tcg_gen_or_tl(t1, t0, t1);
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], t0);
    gen_store_spr(SPR_MQ, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* srea - srea. */
GEN_HANDLER(srea, 0x1F, 0x19, 0x1C, 0x00000000, PPC_POWER_BR)
{
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_andi_tl(t1, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_rotr_tl(t0, cpu_gpr[rS(ctx->opcode)], t1);
    gen_store_spr(SPR_MQ, t0);
    tcg_gen_sar_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)], t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* sreq */
GEN_HANDLER(sreq, 0x1F, 0x19, 0x16, 0x00000000, PPC_POWER_BR)
{
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    TCGv t2 = tcg_temp_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_movi_tl(t1, 0xFFFFFFFF);
    tcg_gen_shr_tl(t1, t1, t0);
    tcg_gen_rotr_tl(t0, cpu_gpr[rS(ctx->opcode)], t0);
    gen_load_spr(t2, SPR_MQ);
    gen_store_spr(SPR_MQ, t0);
    tcg_gen_and_tl(t0, t0, t1);
    tcg_gen_andc_tl(t2, t2, t1);
    tcg_gen_or_tl(cpu_gpr[rA(ctx->opcode)], t0, t2);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    tcg_temp_free(t2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* sriq */
GEN_HANDLER(sriq, 0x1F, 0x18, 0x15, 0x00000000, PPC_POWER_BR)
{
    int sh = SH(ctx->opcode);
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_shri_tl(t0, cpu_gpr[rS(ctx->opcode)], sh);
    tcg_gen_shli_tl(t1, cpu_gpr[rS(ctx->opcode)], 32 - sh);
    tcg_gen_or_tl(t1, t0, t1);
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], t0);
    gen_store_spr(SPR_MQ, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* srliq */
GEN_HANDLER(srliq, 0x1F, 0x18, 0x17, 0x00000000, PPC_POWER_BR)
{
    int sh = SH(ctx->opcode);
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_rotri_tl(t0, cpu_gpr[rS(ctx->opcode)], sh);
    gen_load_spr(t1, SPR_MQ);
    gen_store_spr(SPR_MQ, t0);
    tcg_gen_andi_tl(t0, t0,  (0xFFFFFFFFU >> sh));
    tcg_gen_andi_tl(t1, t1, ~(0xFFFFFFFFU >> sh));
    tcg_gen_or_tl(cpu_gpr[rA(ctx->opcode)], t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* srlq */
GEN_HANDLER(srlq, 0x1F, 0x18, 0x16, 0x00000000, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    TCGv t0 = tcg_temp_local_new();
    TCGv t1 = tcg_temp_local_new();
    TCGv t2 = tcg_temp_local_new();
    tcg_gen_andi_tl(t2, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_movi_tl(t1, 0xFFFFFFFF);
    tcg_gen_shr_tl(t2, t1, t2);
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x20);
    tcg_gen_brcondi_tl(TCG_COND_EQ, t0, 0, l1);
    gen_load_spr(t0, SPR_MQ);
    tcg_gen_and_tl(cpu_gpr[rA(ctx->opcode)], t0, t2);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_shr_tl(t0, cpu_gpr[rS(ctx->opcode)], t2);
    tcg_gen_and_tl(t0, t0, t2);
    gen_load_spr(t1, SPR_MQ);
    tcg_gen_andc_tl(t1, t1, t2);
    tcg_gen_or_tl(cpu_gpr[rA(ctx->opcode)], t0, t1);
    gen_set_label(l2);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    tcg_temp_free(t2);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* srq */
GEN_HANDLER(srq, 0x1F, 0x18, 0x14, 0x00000000, PPC_POWER_BR)
{
    int l1 = gen_new_label();
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_andi_tl(t1, cpu_gpr[rB(ctx->opcode)], 0x1F);
    tcg_gen_shr_tl(t0, cpu_gpr[rS(ctx->opcode)], t1);
    tcg_gen_subfi_tl(t1, 32, t1);
    tcg_gen_shl_tl(t1, cpu_gpr[rS(ctx->opcode)], t1);
    tcg_gen_or_tl(t1, t0, t1);
    gen_store_spr(SPR_MQ, t1);
    tcg_gen_andi_tl(t1, cpu_gpr[rB(ctx->opcode)], 0x20);
    tcg_gen_mov_tl(cpu_gpr[rA(ctx->opcode)], t0);
    tcg_gen_brcondi_tl(TCG_COND_EQ, t0, 0, l1);
    tcg_gen_movi_tl(cpu_gpr[rA(ctx->opcode)], 0);
    gen_set_label(l1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc(ctx->opcode) != 0))
        gen_set_Rc0(ctx, cpu_gpr[rA(ctx->opcode)]);
}

/* PowerPC 602 specific instructions */
/* dsa  */
GEN_HANDLER(dsa, 0x1F, 0x14, 0x13, 0x03FFF801, PPC_602_SPEC)
{
    /* XXX: TODO */
    gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
}

/* esa */
GEN_HANDLER(esa, 0x1F, 0x14, 0x12, 0x03FFF801, PPC_602_SPEC)
{
    /* XXX: TODO */
    gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
}

/* mfrom */
GEN_HANDLER(mfrom, 0x1F, 0x09, 0x08, 0x03E0F801, PPC_602_SPEC)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_helper_602_mfrom(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
#endif
}

/* 602 - 603 - G2 TLB management */
/* tlbld */
GEN_HANDLER2(tlbld_6xx, "tlbld", 0x1F, 0x12, 0x1E, 0x03FF0001, PPC_6xx_TLB)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_helper_6xx_tlbd(cpu_gpr[rB(ctx->opcode)]);
#endif
}

/* tlbli */
GEN_HANDLER2(tlbli_6xx, "tlbli", 0x1F, 0x12, 0x1F, 0x03FF0001, PPC_6xx_TLB)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_helper_6xx_tlbi(cpu_gpr[rB(ctx->opcode)]);
#endif
}

/* 74xx TLB management */
/* tlbld */
GEN_HANDLER2(tlbld_74xx, "tlbld", 0x1F, 0x12, 0x1E, 0x03FF0001, PPC_74xx_TLB)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_helper_74xx_tlbd(cpu_gpr[rB(ctx->opcode)]);
#endif
}

/* tlbli */
GEN_HANDLER2(tlbli_74xx, "tlbli", 0x1F, 0x12, 0x1F, 0x03FF0001, PPC_74xx_TLB)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_helper_74xx_tlbi(cpu_gpr[rB(ctx->opcode)]);
#endif
}

/* POWER instructions not in PowerPC 601 */
/* clf */
GEN_HANDLER(clf, 0x1F, 0x16, 0x03, 0x03E00000, PPC_POWER)
{
    /* Cache line flush: implemented as no-op */
}

/* cli */
GEN_HANDLER(cli, 0x1F, 0x16, 0x0F, 0x03E00000, PPC_POWER)
{
    /* Cache line invalidate: privileged and treated as no-op */
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
#endif
}

/* dclst */
GEN_HANDLER(dclst, 0x1F, 0x16, 0x13, 0x03E00000, PPC_POWER)
{
    /* Data cache line store: treated as no-op */
}

GEN_HANDLER(mfsri, 0x1F, 0x13, 0x13, 0x00000001, PPC_POWER)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    int ra = rA(ctx->opcode);
    int rd = rD(ctx->opcode);
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    tcg_gen_shri_tl(t0, t0, 28);
    tcg_gen_andi_tl(t0, t0, 0xF);
    gen_helper_load_sr(cpu_gpr[rd], t0);
    tcg_temp_free(t0);
    if (ra != 0 && ra != rd)
        tcg_gen_mov_tl(cpu_gpr[ra], cpu_gpr[rd]);
#endif
}

GEN_HANDLER(rac, 0x1F, 0x12, 0x19, 0x00000001, PPC_POWER)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_helper_rac(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
#endif
}

GEN_HANDLER(rfsvc, 0x13, 0x12, 0x02, 0x03FFF0001, PPC_POWER)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_helper_rfsvc();
    gen_sync_exception(ctx);
#endif
}

/* svc is not implemented for now */

/* POWER2 specific instructions */
/* Quad manipulation (load/store two floats at a time) */

/* lfq */
GEN_HANDLER(lfq, 0x38, 0xFF, 0xFF, 0x00000003, PPC_POWER2)
{
    int rd = rD(ctx->opcode);
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_FLOAT);
    t0 = tcg_temp_new();
    gen_addr_imm_index(ctx, t0, 0);
    gen_qemu_ld64(ctx, cpu_fpr[rd], t0);
    gen_addr_add(ctx, t0, t0, 8);
    gen_qemu_ld64(ctx, cpu_fpr[(rd + 1) % 32], t0);
    tcg_temp_free(t0);
}

/* lfqu */
GEN_HANDLER(lfqu, 0x39, 0xFF, 0xFF, 0x00000003, PPC_POWER2)
{
    int ra = rA(ctx->opcode);
    int rd = rD(ctx->opcode);
    TCGv t0, t1;
    gen_set_access_type(ctx, ACCESS_FLOAT);
    t0 = tcg_temp_new();
    t1 = tcg_temp_new();
    gen_addr_imm_index(ctx, t0, 0);
    gen_qemu_ld64(ctx, cpu_fpr[rd], t0);
    gen_addr_add(ctx, t1, t0, 8);
    gen_qemu_ld64(ctx, cpu_fpr[(rd + 1) % 32], t1);
    if (ra != 0)
        tcg_gen_mov_tl(cpu_gpr[ra], t0);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
}

/* lfqux */
GEN_HANDLER(lfqux, 0x1F, 0x17, 0x19, 0x00000001, PPC_POWER2)
{
    int ra = rA(ctx->opcode);
    int rd = rD(ctx->opcode);
    gen_set_access_type(ctx, ACCESS_FLOAT);
    TCGv t0, t1;
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_qemu_ld64(ctx, cpu_fpr[rd], t0);
    t1 = tcg_temp_new();
    gen_addr_add(ctx, t1, t0, 8);
    gen_qemu_ld64(ctx, cpu_fpr[(rd + 1) % 32], t1);
    tcg_temp_free(t1);
    if (ra != 0)
        tcg_gen_mov_tl(cpu_gpr[ra], t0);
    tcg_temp_free(t0);
}

/* lfqx */
GEN_HANDLER(lfqx, 0x1F, 0x17, 0x18, 0x00000001, PPC_POWER2)
{
    int rd = rD(ctx->opcode);
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_FLOAT);
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_qemu_ld64(ctx, cpu_fpr[rd], t0);
    gen_addr_add(ctx, t0, t0, 8);
    gen_qemu_ld64(ctx, cpu_fpr[(rd + 1) % 32], t0);
    tcg_temp_free(t0);
}

/* stfq */
GEN_HANDLER(stfq, 0x3C, 0xFF, 0xFF, 0x00000003, PPC_POWER2)
{
    int rd = rD(ctx->opcode);
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_FLOAT);
    t0 = tcg_temp_new();
    gen_addr_imm_index(ctx, t0, 0);
    gen_qemu_st64(ctx, cpu_fpr[rd], t0);
    gen_addr_add(ctx, t0, t0, 8);
    gen_qemu_st64(ctx, cpu_fpr[(rd + 1) % 32], t0);
    tcg_temp_free(t0);
}

/* stfqu */
GEN_HANDLER(stfqu, 0x3D, 0xFF, 0xFF, 0x00000003, PPC_POWER2)
{
    int ra = rA(ctx->opcode);
    int rd = rD(ctx->opcode);
    TCGv t0, t1;
    gen_set_access_type(ctx, ACCESS_FLOAT);
    t0 = tcg_temp_new();
    gen_addr_imm_index(ctx, t0, 0);
    gen_qemu_st64(ctx, cpu_fpr[rd], t0);
    t1 = tcg_temp_new();
    gen_addr_add(ctx, t1, t0, 8);
    gen_qemu_st64(ctx, cpu_fpr[(rd + 1) % 32], t1);
    tcg_temp_free(t1);
    if (ra != 0)
        tcg_gen_mov_tl(cpu_gpr[ra], t0);
    tcg_temp_free(t0);
}

/* stfqux */
GEN_HANDLER(stfqux, 0x1F, 0x17, 0x1D, 0x00000001, PPC_POWER2)
{
    int ra = rA(ctx->opcode);
    int rd = rD(ctx->opcode);
    TCGv t0, t1;
    gen_set_access_type(ctx, ACCESS_FLOAT);
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_qemu_st64(ctx, cpu_fpr[rd], t0);
    t1 = tcg_temp_new();
    gen_addr_add(ctx, t1, t0, 8);
    gen_qemu_st64(ctx, cpu_fpr[(rd + 1) % 32], t1);
    tcg_temp_free(t1);
    if (ra != 0)
        tcg_gen_mov_tl(cpu_gpr[ra], t0);
    tcg_temp_free(t0);
}

/* stfqx */
GEN_HANDLER(stfqx, 0x1F, 0x17, 0x1C, 0x00000001, PPC_POWER2)
{
    int rd = rD(ctx->opcode);
    TCGv t0;
    gen_set_access_type(ctx, ACCESS_FLOAT);
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_qemu_st64(ctx, cpu_fpr[rd], t0);
    gen_addr_add(ctx, t0, t0, 8);
    gen_qemu_st64(ctx, cpu_fpr[(rd + 1) % 32], t0);
    tcg_temp_free(t0);
}

/* BookE specific instructions */
/* XXX: not implemented on 440 ? */
GEN_HANDLER(mfapidi, 0x1F, 0x13, 0x08, 0x0000F801, PPC_MFAPIDI)
{
    /* XXX: TODO */
    gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
}

/* XXX: not implemented on 440 ? */
GEN_HANDLER(tlbiva, 0x1F, 0x12, 0x18, 0x03FFF801, PPC_TLBIVA)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_helper_tlbie(cpu_gpr[rB(ctx->opcode)]);
    tcg_temp_free(t0);
#endif
}

/* All 405 MAC instructions are translated here */
static always_inline void gen_405_mulladd_insn (DisasContext *ctx,
                                                int opc2, int opc3,
                                                int ra, int rb, int rt, int Rc)
{
    TCGv t0, t1;

    t0 = tcg_temp_local_new();
    t1 = tcg_temp_local_new();

    switch (opc3 & 0x0D) {
    case 0x05:
        /* macchw    - macchw.    - macchwo   - macchwo.   */
        /* macchws   - macchws.   - macchwso  - macchwso.  */
        /* nmacchw   - nmacchw.   - nmacchwo  - nmacchwo.  */
        /* nmacchws  - nmacchws.  - nmacchwso - nmacchwso. */
        /* mulchw - mulchw. */
        tcg_gen_ext16s_tl(t0, cpu_gpr[ra]);
        tcg_gen_sari_tl(t1, cpu_gpr[rb], 16);
        tcg_gen_ext16s_tl(t1, t1);
        break;
    case 0x04:
        /* macchwu   - macchwu.   - macchwuo  - macchwuo.  */
        /* macchwsu  - macchwsu.  - macchwsuo - macchwsuo. */
        /* mulchwu - mulchwu. */
        tcg_gen_ext16u_tl(t0, cpu_gpr[ra]);
        tcg_gen_shri_tl(t1, cpu_gpr[rb], 16);
        tcg_gen_ext16u_tl(t1, t1);
        break;
    case 0x01:
        /* machhw    - machhw.    - machhwo   - machhwo.   */
        /* machhws   - machhws.   - machhwso  - machhwso.  */
        /* nmachhw   - nmachhw.   - nmachhwo  - nmachhwo.  */
        /* nmachhws  - nmachhws.  - nmachhwso - nmachhwso. */
        /* mulhhw - mulhhw. */
        tcg_gen_sari_tl(t0, cpu_gpr[ra], 16);
        tcg_gen_ext16s_tl(t0, t0);
        tcg_gen_sari_tl(t1, cpu_gpr[rb], 16);
        tcg_gen_ext16s_tl(t1, t1);
        break;
    case 0x00:
        /* machhwu   - machhwu.   - machhwuo  - machhwuo.  */
        /* machhwsu  - machhwsu.  - machhwsuo - machhwsuo. */
        /* mulhhwu - mulhhwu. */
        tcg_gen_shri_tl(t0, cpu_gpr[ra], 16);
        tcg_gen_ext16u_tl(t0, t0);
        tcg_gen_shri_tl(t1, cpu_gpr[rb], 16);
        tcg_gen_ext16u_tl(t1, t1);
        break;
    case 0x0D:
        /* maclhw    - maclhw.    - maclhwo   - maclhwo.   */
        /* maclhws   - maclhws.   - maclhwso  - maclhwso.  */
        /* nmaclhw   - nmaclhw.   - nmaclhwo  - nmaclhwo.  */
        /* nmaclhws  - nmaclhws.  - nmaclhwso - nmaclhwso. */
        /* mullhw - mullhw. */
        tcg_gen_ext16s_tl(t0, cpu_gpr[ra]);
        tcg_gen_ext16s_tl(t1, cpu_gpr[rb]);
        break;
    case 0x0C:
        /* maclhwu   - maclhwu.   - maclhwuo  - maclhwuo.  */
        /* maclhwsu  - maclhwsu.  - maclhwsuo - maclhwsuo. */
        /* mullhwu - mullhwu. */
        tcg_gen_ext16u_tl(t0, cpu_gpr[ra]);
        tcg_gen_ext16u_tl(t1, cpu_gpr[rb]);
        break;
    }
    if (opc2 & 0x04) {
        /* (n)multiply-and-accumulate (0x0C / 0x0E) */
        tcg_gen_mul_tl(t1, t0, t1);
        if (opc2 & 0x02) {
            /* nmultiply-and-accumulate (0x0E) */
            tcg_gen_sub_tl(t0, cpu_gpr[rt], t1);
        } else {
            /* multiply-and-accumulate (0x0C) */
            tcg_gen_add_tl(t0, cpu_gpr[rt], t1);
        }

        if (opc3 & 0x12) {
            /* Check overflow and/or saturate */
            int l1 = gen_new_label();

            if (opc3 & 0x10) {
                /* Start with XER OV disabled, the most likely case */
                tcg_gen_andi_tl(cpu_xer, cpu_xer, ~(1 << XER_OV));
            }
            if (opc3 & 0x01) {
                /* Signed */
                tcg_gen_xor_tl(t1, cpu_gpr[rt], t1);
                tcg_gen_brcondi_tl(TCG_COND_GE, t1, 0, l1);
                tcg_gen_xor_tl(t1, cpu_gpr[rt], t0);
                tcg_gen_brcondi_tl(TCG_COND_LT, t1, 0, l1);
                if (opc3 & 0x02) {
                    /* Saturate */
                    tcg_gen_sari_tl(t0, cpu_gpr[rt], 31);
                    tcg_gen_xori_tl(t0, t0, 0x7fffffff);
                }
            } else {
                /* Unsigned */
                tcg_gen_brcond_tl(TCG_COND_GEU, t0, t1, l1);
                if (opc3 & 0x02) {
                    /* Saturate */
                    tcg_gen_movi_tl(t0, UINT32_MAX);
                }
            }
            if (opc3 & 0x10) {
                /* Check overflow */
                tcg_gen_ori_tl(cpu_xer, cpu_xer, (1 << XER_OV) | (1 << XER_SO));
            }
            gen_set_label(l1);
            tcg_gen_mov_tl(cpu_gpr[rt], t0);
        }
    } else {
        tcg_gen_mul_tl(cpu_gpr[rt], t0, t1);
    }
    tcg_temp_free(t0);
    tcg_temp_free(t1);
    if (unlikely(Rc) != 0) {
        /* Update Rc0 */
        gen_set_Rc0(ctx, cpu_gpr[rt]);
    }
}

#define GEN_MAC_HANDLER(name, opc2, opc3)                                     \
GEN_HANDLER(name, 0x04, opc2, opc3, 0x00000000, PPC_405_MAC)                  \
{                                                                             \
    gen_405_mulladd_insn(ctx, opc2, opc3, rA(ctx->opcode), rB(ctx->opcode),   \
                         rD(ctx->opcode), Rc(ctx->opcode));                   \
}

/* macchw    - macchw.    */
GEN_MAC_HANDLER(macchw, 0x0C, 0x05);
/* macchwo   - macchwo.   */
GEN_MAC_HANDLER(macchwo, 0x0C, 0x15);
/* macchws   - macchws.   */
GEN_MAC_HANDLER(macchws, 0x0C, 0x07);
/* macchwso  - macchwso.  */
GEN_MAC_HANDLER(macchwso, 0x0C, 0x17);
/* macchwsu  - macchwsu.  */
GEN_MAC_HANDLER(macchwsu, 0x0C, 0x06);
/* macchwsuo - macchwsuo. */
GEN_MAC_HANDLER(macchwsuo, 0x0C, 0x16);
/* macchwu   - macchwu.   */
GEN_MAC_HANDLER(macchwu, 0x0C, 0x04);
/* macchwuo  - macchwuo.  */
GEN_MAC_HANDLER(macchwuo, 0x0C, 0x14);
/* machhw    - machhw.    */
GEN_MAC_HANDLER(machhw, 0x0C, 0x01);
/* machhwo   - machhwo.   */
GEN_MAC_HANDLER(machhwo, 0x0C, 0x11);
/* machhws   - machhws.   */
GEN_MAC_HANDLER(machhws, 0x0C, 0x03);
/* machhwso  - machhwso.  */
GEN_MAC_HANDLER(machhwso, 0x0C, 0x13);
/* machhwsu  - machhwsu.  */
GEN_MAC_HANDLER(machhwsu, 0x0C, 0x02);
/* machhwsuo - machhwsuo. */
GEN_MAC_HANDLER(machhwsuo, 0x0C, 0x12);
/* machhwu   - machhwu.   */
GEN_MAC_HANDLER(machhwu, 0x0C, 0x00);
/* machhwuo  - machhwuo.  */
GEN_MAC_HANDLER(machhwuo, 0x0C, 0x10);
/* maclhw    - maclhw.    */
GEN_MAC_HANDLER(maclhw, 0x0C, 0x0D);
/* maclhwo   - maclhwo.   */
GEN_MAC_HANDLER(maclhwo, 0x0C, 0x1D);
/* maclhws   - maclhws.   */
GEN_MAC_HANDLER(maclhws, 0x0C, 0x0F);
/* maclhwso  - maclhwso.  */
GEN_MAC_HANDLER(maclhwso, 0x0C, 0x1F);
/* maclhwu   - maclhwu.   */
GEN_MAC_HANDLER(maclhwu, 0x0C, 0x0C);
/* maclhwuo  - maclhwuo.  */
GEN_MAC_HANDLER(maclhwuo, 0x0C, 0x1C);
/* maclhwsu  - maclhwsu.  */
GEN_MAC_HANDLER(maclhwsu, 0x0C, 0x0E);
/* maclhwsuo - maclhwsuo. */
GEN_MAC_HANDLER(maclhwsuo, 0x0C, 0x1E);
/* nmacchw   - nmacchw.   */
GEN_MAC_HANDLER(nmacchw, 0x0E, 0x05);
/* nmacchwo  - nmacchwo.  */
GEN_MAC_HANDLER(nmacchwo, 0x0E, 0x15);
/* nmacchws  - nmacchws.  */
GEN_MAC_HANDLER(nmacchws, 0x0E, 0x07);
/* nmacchwso - nmacchwso. */
GEN_MAC_HANDLER(nmacchwso, 0x0E, 0x17);
/* nmachhw   - nmachhw.   */
GEN_MAC_HANDLER(nmachhw, 0x0E, 0x01);
/* nmachhwo  - nmachhwo.  */
GEN_MAC_HANDLER(nmachhwo, 0x0E, 0x11);
/* nmachhws  - nmachhws.  */
GEN_MAC_HANDLER(nmachhws, 0x0E, 0x03);
/* nmachhwso - nmachhwso. */
GEN_MAC_HANDLER(nmachhwso, 0x0E, 0x13);
/* nmaclhw   - nmaclhw.   */
GEN_MAC_HANDLER(nmaclhw, 0x0E, 0x0D);
/* nmaclhwo  - nmaclhwo.  */
GEN_MAC_HANDLER(nmaclhwo, 0x0E, 0x1D);
/* nmaclhws  - nmaclhws.  */
GEN_MAC_HANDLER(nmaclhws, 0x0E, 0x0F);
/* nmaclhwso - nmaclhwso. */
GEN_MAC_HANDLER(nmaclhwso, 0x0E, 0x1F);

/* mulchw  - mulchw.  */
GEN_MAC_HANDLER(mulchw, 0x08, 0x05);
/* mulchwu - mulchwu. */
GEN_MAC_HANDLER(mulchwu, 0x08, 0x04);
/* mulhhw  - mulhhw.  */
GEN_MAC_HANDLER(mulhhw, 0x08, 0x01);
/* mulhhwu - mulhhwu. */
GEN_MAC_HANDLER(mulhhwu, 0x08, 0x00);
/* mullhw  - mullhw.  */
GEN_MAC_HANDLER(mullhw, 0x08, 0x0D);
/* mullhwu - mullhwu. */
GEN_MAC_HANDLER(mullhwu, 0x08, 0x0C);

/* mfdcr */
GEN_HANDLER(mfdcr, 0x1F, 0x03, 0x0A, 0x00000001, PPC_DCR)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    TCGv dcrn;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    dcrn = tcg_const_tl(SPR(ctx->opcode));
    gen_helper_load_dcr(cpu_gpr[rD(ctx->opcode)], dcrn);
    tcg_temp_free(dcrn);
#endif
}

/* mtdcr */
GEN_HANDLER(mtdcr, 0x1F, 0x03, 0x0E, 0x00000001, PPC_DCR)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    TCGv dcrn;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    dcrn = tcg_const_tl(SPR(ctx->opcode));
    gen_helper_store_dcr(dcrn, cpu_gpr[rS(ctx->opcode)]);
    tcg_temp_free(dcrn);
#endif
}

/* mfdcrx */
/* XXX: not implemented on 440 ? */
GEN_HANDLER(mfdcrx, 0x1F, 0x03, 0x08, 0x00000000, PPC_DCRX)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    gen_helper_load_dcr(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    /* Note: Rc update flag set leads to undefined state of Rc0 */
#endif
}

/* mtdcrx */
/* XXX: not implemented on 440 ? */
GEN_HANDLER(mtdcrx, 0x1F, 0x03, 0x0C, 0x00000000, PPC_DCRX)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_REG);
        return;
    }
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    gen_helper_store_dcr(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
    /* Note: Rc update flag set leads to undefined state of Rc0 */
#endif
}

/* mfdcrux (PPC 460) : user-mode access to DCR */
GEN_HANDLER(mfdcrux, 0x1F, 0x03, 0x09, 0x00000000, PPC_DCRUX)
{
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    gen_helper_load_dcr(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
    /* Note: Rc update flag set leads to undefined state of Rc0 */
}

/* mtdcrux (PPC 460) : user-mode access to DCR */
GEN_HANDLER(mtdcrux, 0x1F, 0x03, 0x0D, 0x00000000, PPC_DCRUX)
{
    /* NIP cannot be restored if the memory exception comes from an helper */
    gen_update_nip(ctx, ctx->nip - 4);
    gen_helper_store_dcr(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
    /* Note: Rc update flag set leads to undefined state of Rc0 */
}

/* dccci */
GEN_HANDLER(dccci, 0x1F, 0x06, 0x0E, 0x03E00001, PPC_4xx_COMMON)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    /* interpreted as no-op */
#endif
}

/* dcread */
GEN_HANDLER(dcread, 0x1F, 0x06, 0x0F, 0x00000001, PPC_4xx_COMMON)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    TCGv EA, val;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    gen_set_access_type(ctx, ACCESS_CACHE);
    EA = tcg_temp_new();
    gen_addr_reg_index(ctx, EA);
    val = tcg_temp_new();
    gen_qemu_ld32u(ctx, val, EA);
    tcg_temp_free(val);
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], EA);
    tcg_temp_free(EA);
#endif
}

/* icbt */
GEN_HANDLER2(icbt_40x, "icbt", 0x1F, 0x06, 0x08, 0x03E00001, PPC_40x_ICBT)
{
    /* interpreted as no-op */
    /* XXX: specification say this is treated as a load by the MMU
     *      but does not generate any exception
     */
}

/* iccci */
GEN_HANDLER(iccci, 0x1F, 0x06, 0x1E, 0x00000001, PPC_4xx_COMMON)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    /* interpreted as no-op */
#endif
}

/* icread */
GEN_HANDLER(icread, 0x1F, 0x06, 0x1F, 0x03E00001, PPC_4xx_COMMON)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    /* interpreted as no-op */
#endif
}

/* rfci (mem_idx only) */
GEN_HANDLER2(rfci_40x, "rfci", 0x13, 0x13, 0x01, 0x03FF8001, PPC_40x_EXCP)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    /* Restore CPU state */
    gen_helper_40x_rfci();
    gen_sync_exception(ctx);
#endif
}

GEN_HANDLER(rfci, 0x13, 0x13, 0x01, 0x03FF8001, PPC_BOOKE)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    /* Restore CPU state */
    gen_helper_rfci();
    gen_sync_exception(ctx);
#endif
}

/* BookE specific */
/* XXX: not implemented on 440 ? */
GEN_HANDLER(rfdi, 0x13, 0x07, 0x01, 0x03FF8001, PPC_RFDI)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    /* Restore CPU state */
    gen_helper_rfdi();
    gen_sync_exception(ctx);
#endif
}

/* XXX: not implemented on 440 ? */
GEN_HANDLER(rfmci, 0x13, 0x06, 0x01, 0x03FF8001, PPC_RFMCI)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    /* Restore CPU state */
    gen_helper_rfmci();
    gen_sync_exception(ctx);
#endif
}

/* TLB management - PowerPC 405 implementation */
/* tlbre */
GEN_HANDLER2(tlbre_40x, "tlbre", 0x1F, 0x12, 0x1D, 0x00000001, PPC_40x_TLB)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    switch (rB(ctx->opcode)) {
    case 0:
        gen_helper_4xx_tlbre_hi(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
        break;
    case 1:
        gen_helper_4xx_tlbre_lo(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
        break;
    default:
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
        break;
    }
#endif
}

/* tlbsx - tlbsx. */
GEN_HANDLER2(tlbsx_40x, "tlbsx", 0x1F, 0x12, 0x1C, 0x00000000, PPC_40x_TLB)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_helper_4xx_tlbsx(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
    if (Rc(ctx->opcode)) {
        int l1 = gen_new_label();
        tcg_gen_trunc_tl_i32(cpu_crf[0], cpu_xer);
        tcg_gen_shri_i32(cpu_crf[0], cpu_crf[0], XER_SO);
        tcg_gen_andi_i32(cpu_crf[0], cpu_crf[0], 1);
        tcg_gen_brcondi_tl(TCG_COND_EQ, cpu_gpr[rD(ctx->opcode)], -1, l1);
        tcg_gen_ori_i32(cpu_crf[0], cpu_crf[0], 0x02);
        gen_set_label(l1);
    }
#endif
}

/* tlbwe */
GEN_HANDLER2(tlbwe_40x, "tlbwe", 0x1F, 0x12, 0x1E, 0x00000001, PPC_40x_TLB)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    switch (rB(ctx->opcode)) {
    case 0:
        gen_helper_4xx_tlbwe_hi(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
        break;
    case 1:
        gen_helper_4xx_tlbwe_lo(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
        break;
    default:
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
        break;
    }
#endif
}

/* TLB management - PowerPC 440 implementation */
/* tlbre */
GEN_HANDLER2(tlbre_440, "tlbre", 0x1F, 0x12, 0x1D, 0x00000001, PPC_BOOKE)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    switch (rB(ctx->opcode)) {
    case 0:
    case 1:
    case 2:
        {
            TCGv_i32 t0 = tcg_const_i32(rB(ctx->opcode));
            gen_helper_440_tlbwe(t0, cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
            tcg_temp_free_i32(t0);
        }
        break;
    default:
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
        break;
    }
#endif
}

/* tlbsx - tlbsx. */
GEN_HANDLER2(tlbsx_440, "tlbsx", 0x1F, 0x12, 0x1C, 0x00000000, PPC_BOOKE)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    t0 = tcg_temp_new();
    gen_addr_reg_index(ctx, t0);
    gen_helper_440_tlbsx(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
    if (Rc(ctx->opcode)) {
        int l1 = gen_new_label();
        tcg_gen_trunc_tl_i32(cpu_crf[0], cpu_xer);
        tcg_gen_shri_i32(cpu_crf[0], cpu_crf[0], XER_SO);
        tcg_gen_andi_i32(cpu_crf[0], cpu_crf[0], 1);
        tcg_gen_brcondi_tl(TCG_COND_EQ, cpu_gpr[rD(ctx->opcode)], -1, l1);
        tcg_gen_ori_i32(cpu_crf[0], cpu_crf[0], 0x02);
        gen_set_label(l1);
    }
#endif
}

/* tlbwe */
GEN_HANDLER2(tlbwe_440, "tlbwe", 0x1F, 0x12, 0x1E, 0x00000001, PPC_BOOKE)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    switch (rB(ctx->opcode)) {
    case 0:
    case 1:
    case 2:
        {
            TCGv_i32 t0 = tcg_const_i32(rB(ctx->opcode));
            gen_helper_440_tlbwe(t0, cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)]);
            tcg_temp_free_i32(t0);
        }
        break;
    default:
        gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
        break;
    }
#endif
}

/* wrtee */
GEN_HANDLER(wrtee, 0x1F, 0x03, 0x04, 0x000FFC01, PPC_WRTEE)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    TCGv t0;
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    t0 = tcg_temp_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rD(ctx->opcode)], (1 << MSR_EE));
    tcg_gen_andi_tl(cpu_msr, cpu_msr, ~(1 << MSR_EE));
    tcg_gen_or_tl(cpu_msr, cpu_msr, t0);
    tcg_temp_free(t0);
    /* Stop translation to have a chance to raise an exception
     * if we just set msr_ee to 1
     */
    gen_stop_exception(ctx);
#endif
}

/* wrteei */
GEN_HANDLER(wrteei, 0x1F, 0x03, 0x05, 0x000EFC01, PPC_WRTEE)
{
#if defined(CONFIG_USER_ONLY)
    gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
#else
    if (unlikely(!ctx->mem_idx)) {
        gen_inval_exception(ctx, POWERPC_EXCP_PRIV_OPC);
        return;
    }
    if (ctx->opcode & 0x00010000) {
        tcg_gen_ori_tl(cpu_msr, cpu_msr, (1 << MSR_EE));
        /* Stop translation to have a chance to raise an exception */
        gen_stop_exception(ctx);
    } else {
        tcg_gen_andi_tl(cpu_msr, cpu_msr, ~(1 << MSR_EE));
    }
#endif
}

/* PowerPC 440 specific instructions */
/* dlmzb */
GEN_HANDLER(dlmzb, 0x1F, 0x0E, 0x02, 0x00000000, PPC_440_SPEC)
{
    TCGv_i32 t0 = tcg_const_i32(Rc(ctx->opcode));
    gen_helper_dlmzb(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rS(ctx->opcode)],
                     cpu_gpr[rB(ctx->opcode)], t0);
    tcg_temp_free_i32(t0);
}

/* mbar replaces eieio on 440 */
GEN_HANDLER(mbar, 0x1F, 0x16, 0x1a, 0x001FF801, PPC_BOOKE)
{
    /* interpreted as no-op */
}

/* msync replaces sync on 440 */
GEN_HANDLER(msync, 0x1F, 0x16, 0x12, 0x03FFF801, PPC_BOOKE)
{
    /* interpreted as no-op */
}

/* icbt */
GEN_HANDLER2(icbt_440, "icbt", 0x1F, 0x16, 0x00, 0x03E00001, PPC_BOOKE)
{
    /* interpreted as no-op */
    /* XXX: specification say this is treated as a load by the MMU
     *      but does not generate any exception
     */
}

/***                      Altivec vector extension                         ***/
/* Altivec registers moves */

static always_inline TCGv_ptr gen_avr_ptr(int reg)
{
    TCGv_ptr r = tcg_temp_new_ptr();
    tcg_gen_addi_ptr(r, cpu_env, offsetof(CPUPPCState, avr[reg]));
    return r;
}

#define GEN_VR_LDX(name, opc2, opc3)                                          \
GEN_HANDLER(name, 0x1F, opc2, opc3, 0x00000001, PPC_ALTIVEC)                  \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(!ctx->altivec_enabled)) {                                    \
        gen_exception(ctx, POWERPC_EXCP_VPU);                                 \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_INT);                                     \
    EA = tcg_temp_new();                                                      \
    gen_addr_reg_index(ctx, EA);                                              \
    tcg_gen_andi_tl(EA, EA, ~0xf);                                            \
    if (ctx->le_mode) {                                                       \
        gen_qemu_ld64(ctx, cpu_avrl[rD(ctx->opcode)], EA);                    \
        tcg_gen_addi_tl(EA, EA, 8);                                           \
        gen_qemu_ld64(ctx, cpu_avrh[rD(ctx->opcode)], EA);                    \
    } else {                                                                  \
        gen_qemu_ld64(ctx, cpu_avrh[rD(ctx->opcode)], EA);                    \
        tcg_gen_addi_tl(EA, EA, 8);                                           \
        gen_qemu_ld64(ctx, cpu_avrl[rD(ctx->opcode)], EA);                    \
    }                                                                         \
    tcg_temp_free(EA);                                                        \
}

#define GEN_VR_STX(name, opc2, opc3)                                          \
GEN_HANDLER(st##name, 0x1F, opc2, opc3, 0x00000001, PPC_ALTIVEC)              \
{                                                                             \
    TCGv EA;                                                                  \
    if (unlikely(!ctx->altivec_enabled)) {                                    \
        gen_exception(ctx, POWERPC_EXCP_VPU);                                 \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_INT);                                     \
    EA = tcg_temp_new();                                                      \
    gen_addr_reg_index(ctx, EA);                                              \
    tcg_gen_andi_tl(EA, EA, ~0xf);                                            \
    if (ctx->le_mode) {                                                       \
        gen_qemu_st64(ctx, cpu_avrl[rD(ctx->opcode)], EA);                    \
        tcg_gen_addi_tl(EA, EA, 8);                                           \
        gen_qemu_st64(ctx, cpu_avrh[rD(ctx->opcode)], EA);                    \
    } else {                                                                  \
        gen_qemu_st64(ctx, cpu_avrh[rD(ctx->opcode)], EA);                    \
        tcg_gen_addi_tl(EA, EA, 8);                                           \
        gen_qemu_st64(ctx, cpu_avrl[rD(ctx->opcode)], EA);                    \
    }                                                                         \
    tcg_temp_free(EA);                                                        \
}

#define GEN_VR_LVE(name, opc2, opc3)                                    \
    GEN_HANDLER(lve##name, 0x1F, opc2, opc3, 0x00000001, PPC_ALTIVEC)   \
    {                                                                   \
        TCGv EA;                                                        \
        TCGv_ptr rs;                                                    \
        if (unlikely(!ctx->altivec_enabled)) {                          \
            gen_exception(ctx, POWERPC_EXCP_VPU);                       \
            return;                                                     \
        }                                                               \
        gen_set_access_type(ctx, ACCESS_INT);                           \
        EA = tcg_temp_new();                                            \
        gen_addr_reg_index(ctx, EA);                                    \
        rs = gen_avr_ptr(rS(ctx->opcode));                              \
        gen_helper_lve##name (rs, EA);                                  \
        tcg_temp_free(EA);                                              \
        tcg_temp_free_ptr(rs);                                          \
    }

#define GEN_VR_STVE(name, opc2, opc3)                                   \
    GEN_HANDLER(stve##name, 0x1F, opc2, opc3, 0x00000001, PPC_ALTIVEC)  \
    {                                                                   \
        TCGv EA;                                                        \
        TCGv_ptr rs;                                                    \
        if (unlikely(!ctx->altivec_enabled)) {                          \
            gen_exception(ctx, POWERPC_EXCP_VPU);                       \
            return;                                                     \
        }                                                               \
        gen_set_access_type(ctx, ACCESS_INT);                           \
        EA = tcg_temp_new();                                            \
        gen_addr_reg_index(ctx, EA);                                    \
        rs = gen_avr_ptr(rS(ctx->opcode));                              \
        gen_helper_stve##name (rs, EA);                                 \
        tcg_temp_free(EA);                                              \
        tcg_temp_free_ptr(rs);                                          \
    }

GEN_VR_LDX(lvx, 0x07, 0x03);
/* As we don't emulate the cache, lvxl is stricly equivalent to lvx */
GEN_VR_LDX(lvxl, 0x07, 0x0B);

GEN_VR_LVE(bx, 0x07, 0x00);
GEN_VR_LVE(hx, 0x07, 0x01);
GEN_VR_LVE(wx, 0x07, 0x02);

GEN_VR_STX(svx, 0x07, 0x07);
/* As we don't emulate the cache, stvxl is stricly equivalent to stvx */
GEN_VR_STX(svxl, 0x07, 0x0F);

GEN_VR_STVE(bx, 0x07, 0x04);
GEN_VR_STVE(hx, 0x07, 0x05);
GEN_VR_STVE(wx, 0x07, 0x06);

GEN_HANDLER(lvsl, 0x1f, 0x06, 0x00, 0x00000001, PPC_ALTIVEC)
{
    TCGv_ptr rd;
    TCGv EA;
    if (unlikely(!ctx->altivec_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_VPU);
        return;
    }
    EA = tcg_temp_new();
    gen_addr_reg_index(ctx, EA);
    rd = gen_avr_ptr(rD(ctx->opcode));
    gen_helper_lvsl(rd, EA);
    tcg_temp_free(EA);
    tcg_temp_free_ptr(rd);
}

GEN_HANDLER(lvsr, 0x1f, 0x06, 0x01, 0x00000001, PPC_ALTIVEC)
{
    TCGv_ptr rd;
    TCGv EA;
    if (unlikely(!ctx->altivec_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_VPU);
        return;
    }
    EA = tcg_temp_new();
    gen_addr_reg_index(ctx, EA);
    rd = gen_avr_ptr(rD(ctx->opcode));
    gen_helper_lvsr(rd, EA);
    tcg_temp_free(EA);
    tcg_temp_free_ptr(rd);
}

GEN_HANDLER(mfvscr, 0x04, 0x2, 0x18, 0x001ff800, PPC_ALTIVEC)
{
    TCGv_i32 t;
    if (unlikely(!ctx->altivec_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_VPU);
        return;
    }
    tcg_gen_movi_i64(cpu_avrh[rD(ctx->opcode)], 0);
    t = tcg_temp_new_i32();
    tcg_gen_ld_i32(t, cpu_env, offsetof(CPUState, vscr));
    tcg_gen_extu_i32_i64(cpu_avrl[rD(ctx->opcode)], t);
    tcg_temp_free_i32(t);
}

GEN_HANDLER(mtvscr, 0x04, 0x2, 0x19, 0x03ff0000, PPC_ALTIVEC)
{
    TCGv_ptr p;
    if (unlikely(!ctx->altivec_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_VPU);
        return;
    }
    p = gen_avr_ptr(rD(ctx->opcode));
    gen_helper_mtvscr(p);
    tcg_temp_free_ptr(p);
}

/* Logical operations */
#define GEN_VX_LOGICAL(name, tcg_op, opc2, opc3)                        \
GEN_HANDLER(name, 0x04, opc2, opc3, 0x00000000, PPC_ALTIVEC)            \
{                                                                       \
    if (unlikely(!ctx->altivec_enabled)) {                              \
        gen_exception(ctx, POWERPC_EXCP_VPU);                           \
        return;                                                         \
    }                                                                   \
    tcg_op(cpu_avrh[rD(ctx->opcode)], cpu_avrh[rA(ctx->opcode)], cpu_avrh[rB(ctx->opcode)]); \
    tcg_op(cpu_avrl[rD(ctx->opcode)], cpu_avrl[rA(ctx->opcode)], cpu_avrl[rB(ctx->opcode)]); \
}

GEN_VX_LOGICAL(vand, tcg_gen_and_i64, 2, 16);
GEN_VX_LOGICAL(vandc, tcg_gen_andc_i64, 2, 17);
GEN_VX_LOGICAL(vor, tcg_gen_or_i64, 2, 18);
GEN_VX_LOGICAL(vxor, tcg_gen_xor_i64, 2, 19);
GEN_VX_LOGICAL(vnor, tcg_gen_nor_i64, 2, 20);

#define GEN_VXFORM(name, opc2, opc3)                                    \
GEN_HANDLER(name, 0x04, opc2, opc3, 0x00000000, PPC_ALTIVEC)            \
{                                                                       \
    TCGv_ptr ra, rb, rd;                                                \
    if (unlikely(!ctx->altivec_enabled)) {                              \
        gen_exception(ctx, POWERPC_EXCP_VPU);                           \
        return;                                                         \
    }                                                                   \
    ra = gen_avr_ptr(rA(ctx->opcode));                                  \
    rb = gen_avr_ptr(rB(ctx->opcode));                                  \
    rd = gen_avr_ptr(rD(ctx->opcode));                                  \
    gen_helper_##name (rd, ra, rb);                                     \
    tcg_temp_free_ptr(ra);                                              \
    tcg_temp_free_ptr(rb);                                              \
    tcg_temp_free_ptr(rd);                                              \
}

GEN_VXFORM(vaddubm, 0, 0);
GEN_VXFORM(vadduhm, 0, 1);
GEN_VXFORM(vadduwm, 0, 2);
GEN_VXFORM(vsububm, 0, 16);
GEN_VXFORM(vsubuhm, 0, 17);
GEN_VXFORM(vsubuwm, 0, 18);
GEN_VXFORM(vmaxub, 1, 0);
GEN_VXFORM(vmaxuh, 1, 1);
GEN_VXFORM(vmaxuw, 1, 2);
GEN_VXFORM(vmaxsb, 1, 4);
GEN_VXFORM(vmaxsh, 1, 5);
GEN_VXFORM(vmaxsw, 1, 6);
GEN_VXFORM(vminub, 1, 8);
GEN_VXFORM(vminuh, 1, 9);
GEN_VXFORM(vminuw, 1, 10);
GEN_VXFORM(vminsb, 1, 12);
GEN_VXFORM(vminsh, 1, 13);
GEN_VXFORM(vminsw, 1, 14);
GEN_VXFORM(vavgub, 1, 16);
GEN_VXFORM(vavguh, 1, 17);
GEN_VXFORM(vavguw, 1, 18);
GEN_VXFORM(vavgsb, 1, 20);
GEN_VXFORM(vavgsh, 1, 21);
GEN_VXFORM(vavgsw, 1, 22);
GEN_VXFORM(vmrghb, 6, 0);
GEN_VXFORM(vmrghh, 6, 1);
GEN_VXFORM(vmrghw, 6, 2);
GEN_VXFORM(vmrglb, 6, 4);
GEN_VXFORM(vmrglh, 6, 5);
GEN_VXFORM(vmrglw, 6, 6);
GEN_VXFORM(vmuloub, 4, 0);
GEN_VXFORM(vmulouh, 4, 1);
GEN_VXFORM(vmulosb, 4, 4);
GEN_VXFORM(vmulosh, 4, 5);
GEN_VXFORM(vmuleub, 4, 8);
GEN_VXFORM(vmuleuh, 4, 9);
GEN_VXFORM(vmulesb, 4, 12);
GEN_VXFORM(vmulesh, 4, 13);
GEN_VXFORM(vslb, 2, 4);
GEN_VXFORM(vslh, 2, 5);
GEN_VXFORM(vslw, 2, 6);
GEN_VXFORM(vsrb, 2, 8);
GEN_VXFORM(vsrh, 2, 9);
GEN_VXFORM(vsrw, 2, 10);
GEN_VXFORM(vsrab, 2, 12);
GEN_VXFORM(vsrah, 2, 13);
GEN_VXFORM(vsraw, 2, 14);
GEN_VXFORM(vslo, 6, 16);
GEN_VXFORM(vsro, 6, 17);
GEN_VXFORM(vaddcuw, 0, 6);
GEN_VXFORM(vsubcuw, 0, 22);
GEN_VXFORM(vaddubs, 0, 8);
GEN_VXFORM(vadduhs, 0, 9);
GEN_VXFORM(vadduws, 0, 10);
GEN_VXFORM(vaddsbs, 0, 12);
GEN_VXFORM(vaddshs, 0, 13);
GEN_VXFORM(vaddsws, 0, 14);
GEN_VXFORM(vsububs, 0, 24);
GEN_VXFORM(vsubuhs, 0, 25);
GEN_VXFORM(vsubuws, 0, 26);
GEN_VXFORM(vsubsbs, 0, 28);
GEN_VXFORM(vsubshs, 0, 29);
GEN_VXFORM(vsubsws, 0, 30);
GEN_VXFORM(vrlb, 2, 0);
GEN_VXFORM(vrlh, 2, 1);
GEN_VXFORM(vrlw, 2, 2);
GEN_VXFORM(vsl, 2, 7);
GEN_VXFORM(vsr, 2, 11);
GEN_VXFORM(vpkuhum, 7, 0);
GEN_VXFORM(vpkuwum, 7, 1);
GEN_VXFORM(vpkuhus, 7, 2);
GEN_VXFORM(vpkuwus, 7, 3);
GEN_VXFORM(vpkshus, 7, 4);
GEN_VXFORM(vpkswus, 7, 5);
GEN_VXFORM(vpkshss, 7, 6);
GEN_VXFORM(vpkswss, 7, 7);
GEN_VXFORM(vpkpx, 7, 12);
GEN_VXFORM(vsum4ubs, 4, 24);
GEN_VXFORM(vsum4sbs, 4, 28);
GEN_VXFORM(vsum4shs, 4, 25);
GEN_VXFORM(vsum2sws, 4, 26);
GEN_VXFORM(vsumsws, 4, 30);
GEN_VXFORM(vaddfp, 5, 0);
GEN_VXFORM(vsubfp, 5, 1);
GEN_VXFORM(vmaxfp, 5, 16);
GEN_VXFORM(vminfp, 5, 17);

#define GEN_VXRFORM1(opname, name, str, opc2, opc3)                     \
    GEN_HANDLER2(name, str, 0x4, opc2, opc3, 0x00000000, PPC_ALTIVEC)   \
    {                                                                   \
        TCGv_ptr ra, rb, rd;                                            \
        if (unlikely(!ctx->altivec_enabled)) {                          \
            gen_exception(ctx, POWERPC_EXCP_VPU);                       \
            return;                                                     \
        }                                                               \
        ra = gen_avr_ptr(rA(ctx->opcode));                              \
        rb = gen_avr_ptr(rB(ctx->opcode));                              \
        rd = gen_avr_ptr(rD(ctx->opcode));                              \
        gen_helper_##opname (rd, ra, rb);                               \
        tcg_temp_free_ptr(ra);                                          \
        tcg_temp_free_ptr(rb);                                          \
        tcg_temp_free_ptr(rd);                                          \
    }

#define GEN_VXRFORM(name, opc2, opc3)                                \
    GEN_VXRFORM1(name, name, #name, opc2, opc3)                      \
    GEN_VXRFORM1(name##_dot, name##_, #name ".", opc2, (opc3 | (0x1 << 4)))

GEN_VXRFORM(vcmpequb, 3, 0)
GEN_VXRFORM(vcmpequh, 3, 1)
GEN_VXRFORM(vcmpequw, 3, 2)
GEN_VXRFORM(vcmpgtsb, 3, 12)
GEN_VXRFORM(vcmpgtsh, 3, 13)
GEN_VXRFORM(vcmpgtsw, 3, 14)
GEN_VXRFORM(vcmpgtub, 3, 8)
GEN_VXRFORM(vcmpgtuh, 3, 9)
GEN_VXRFORM(vcmpgtuw, 3, 10)
GEN_VXRFORM(vcmpeqfp, 3, 3)
GEN_VXRFORM(vcmpgefp, 3, 7)
GEN_VXRFORM(vcmpgtfp, 3, 11)
GEN_VXRFORM(vcmpbfp, 3, 15)

#define GEN_VXFORM_SIMM(name, opc2, opc3)                               \
    GEN_HANDLER(name, 0x04, opc2, opc3, 0x00000000, PPC_ALTIVEC)        \
    {                                                                   \
        TCGv_ptr rd;                                                    \
        TCGv_i32 simm;                                                  \
        if (unlikely(!ctx->altivec_enabled)) {                          \
            gen_exception(ctx, POWERPC_EXCP_VPU);                       \
            return;                                                     \
        }                                                               \
        simm = tcg_const_i32(SIMM5(ctx->opcode));                       \
        rd = gen_avr_ptr(rD(ctx->opcode));                              \
        gen_helper_##name (rd, simm);                                   \
        tcg_temp_free_i32(simm);                                        \
        tcg_temp_free_ptr(rd);                                          \
    }

GEN_VXFORM_SIMM(vspltisb, 6, 12);
GEN_VXFORM_SIMM(vspltish, 6, 13);
GEN_VXFORM_SIMM(vspltisw, 6, 14);

#define GEN_VXFORM_NOA(name, opc2, opc3)                                \
    GEN_HANDLER(name, 0x04, opc2, opc3, 0x001f0000, PPC_ALTIVEC)        \
    {                                                                   \
        TCGv_ptr rb, rd;                                                \
        if (unlikely(!ctx->altivec_enabled)) {                          \
            gen_exception(ctx, POWERPC_EXCP_VPU);                       \
            return;                                                     \
        }                                                               \
        rb = gen_avr_ptr(rB(ctx->opcode));                              \
        rd = gen_avr_ptr(rD(ctx->opcode));                              \
        gen_helper_##name (rd, rb);                                     \
        tcg_temp_free_ptr(rb);                                          \
        tcg_temp_free_ptr(rd);                                         \
    }

GEN_VXFORM_NOA(vupkhsb, 7, 8);
GEN_VXFORM_NOA(vupkhsh, 7, 9);
GEN_VXFORM_NOA(vupklsb, 7, 10);
GEN_VXFORM_NOA(vupklsh, 7, 11);
GEN_VXFORM_NOA(vupkhpx, 7, 13);
GEN_VXFORM_NOA(vupklpx, 7, 15);
GEN_VXFORM_NOA(vrefp, 5, 4);
GEN_VXFORM_NOA(vrsqrtefp, 5, 5);
GEN_VXFORM_NOA(vlogefp, 5, 7);
GEN_VXFORM_NOA(vrfim, 5, 8);
GEN_VXFORM_NOA(vrfin, 5, 9);
GEN_VXFORM_NOA(vrfip, 5, 10);
GEN_VXFORM_NOA(vrfiz, 5, 11);

#define GEN_VXFORM_SIMM(name, opc2, opc3)                               \
    GEN_HANDLER(name, 0x04, opc2, opc3, 0x00000000, PPC_ALTIVEC)        \
    {                                                                   \
        TCGv_ptr rd;                                                    \
        TCGv_i32 simm;                                                  \
        if (unlikely(!ctx->altivec_enabled)) {                          \
            gen_exception(ctx, POWERPC_EXCP_VPU);                       \
            return;                                                     \
        }                                                               \
        simm = tcg_const_i32(SIMM5(ctx->opcode));                       \
        rd = gen_avr_ptr(rD(ctx->opcode));                              \
        gen_helper_##name (rd, simm);                                   \
        tcg_temp_free_i32(simm);                                        \
        tcg_temp_free_ptr(rd);                                          \
    }

#define GEN_VXFORM_UIMM(name, opc2, opc3)                               \
    GEN_HANDLER(name, 0x04, opc2, opc3, 0x00000000, PPC_ALTIVEC)        \
    {                                                                   \
        TCGv_ptr rb, rd;                                                \
        TCGv_i32 uimm;                                                  \
        if (unlikely(!ctx->altivec_enabled)) {                          \
            gen_exception(ctx, POWERPC_EXCP_VPU);                       \
            return;                                                     \
        }                                                               \
        uimm = tcg_const_i32(UIMM5(ctx->opcode));                       \
        rb = gen_avr_ptr(rB(ctx->opcode));                              \
        rd = gen_avr_ptr(rD(ctx->opcode));                              \
        gen_helper_##name (rd, rb, uimm);                               \
        tcg_temp_free_i32(uimm);                                        \
        tcg_temp_free_ptr(rb);                                          \
        tcg_temp_free_ptr(rd);                                          \
    }

GEN_VXFORM_UIMM(vspltb, 6, 8);
GEN_VXFORM_UIMM(vsplth, 6, 9);
GEN_VXFORM_UIMM(vspltw, 6, 10);
GEN_VXFORM_UIMM(vcfux, 5, 12);
GEN_VXFORM_UIMM(vcfsx, 5, 13);
GEN_VXFORM_UIMM(vctuxs, 5, 14);
GEN_VXFORM_UIMM(vctsxs, 5, 15);

GEN_HANDLER(vsldoi, 0x04, 0x16, 0xFF, 0x00000400, PPC_ALTIVEC)
{
    TCGv_ptr ra, rb, rd;
    TCGv_i32 sh;
    if (unlikely(!ctx->altivec_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_VPU);
        return;
    }
    ra = gen_avr_ptr(rA(ctx->opcode));
    rb = gen_avr_ptr(rB(ctx->opcode));
    rd = gen_avr_ptr(rD(ctx->opcode));
    sh = tcg_const_i32(VSH(ctx->opcode));
    gen_helper_vsldoi (rd, ra, rb, sh);
    tcg_temp_free_ptr(ra);
    tcg_temp_free_ptr(rb);
    tcg_temp_free_ptr(rd);
    tcg_temp_free_i32(sh);
}

#define GEN_VAFORM_PAIRED(name0, name1, opc2)                           \
    GEN_HANDLER(name0##_##name1, 0x04, opc2, 0xFF, 0x00000000, PPC_ALTIVEC) \
    {                                                                   \
        TCGv_ptr ra, rb, rc, rd;                                        \
        if (unlikely(!ctx->altivec_enabled)) {                          \
            gen_exception(ctx, POWERPC_EXCP_VPU);                       \
            return;                                                     \
        }                                                               \
        ra = gen_avr_ptr(rA(ctx->opcode));                              \
        rb = gen_avr_ptr(rB(ctx->opcode));                              \
        rc = gen_avr_ptr(rC(ctx->opcode));                              \
        rd = gen_avr_ptr(rD(ctx->opcode));                              \
        if (Rc(ctx->opcode)) {                                          \
            gen_helper_##name1 (rd, ra, rb, rc);                        \
        } else {                                                        \
            gen_helper_##name0 (rd, ra, rb, rc);                        \
        }                                                               \
        tcg_temp_free_ptr(ra);                                          \
        tcg_temp_free_ptr(rb);                                          \
        tcg_temp_free_ptr(rc);                                          \
        tcg_temp_free_ptr(rd);                                          \
    }

GEN_VAFORM_PAIRED(vmhaddshs, vmhraddshs, 16)

GEN_HANDLER(vmladduhm, 0x04, 0x11, 0xFF, 0x00000000, PPC_ALTIVEC)
{
    TCGv_ptr ra, rb, rc, rd;
    if (unlikely(!ctx->altivec_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_VPU);
        return;
    }
    ra = gen_avr_ptr(rA(ctx->opcode));
    rb = gen_avr_ptr(rB(ctx->opcode));
    rc = gen_avr_ptr(rC(ctx->opcode));
    rd = gen_avr_ptr(rD(ctx->opcode));
    gen_helper_vmladduhm(rd, ra, rb, rc);
    tcg_temp_free_ptr(ra);
    tcg_temp_free_ptr(rb);
    tcg_temp_free_ptr(rc);
    tcg_temp_free_ptr(rd);
}

GEN_VAFORM_PAIRED(vmsumubm, vmsummbm, 18)
GEN_VAFORM_PAIRED(vmsumuhm, vmsumuhs, 19)
GEN_VAFORM_PAIRED(vmsumshm, vmsumshs, 20)
GEN_VAFORM_PAIRED(vsel, vperm, 21)
GEN_VAFORM_PAIRED(vmaddfp, vnmsubfp, 23)

/***                           SPE extension                               ***/
/* Register moves */

static always_inline void gen_load_gpr64(TCGv_i64 t, int reg) {
#if defined(TARGET_PPC64)
    tcg_gen_mov_i64(t, cpu_gpr[reg]);
#else
    tcg_gen_concat_i32_i64(t, cpu_gpr[reg], cpu_gprh[reg]);
#endif
}

static always_inline void gen_store_gpr64(int reg, TCGv_i64 t) {
#if defined(TARGET_PPC64)
    tcg_gen_mov_i64(cpu_gpr[reg], t);
#else
    TCGv_i64 tmp = tcg_temp_new_i64();
    tcg_gen_trunc_i64_i32(cpu_gpr[reg], t);
    tcg_gen_shri_i64(tmp, t, 32);
    tcg_gen_trunc_i64_i32(cpu_gprh[reg], tmp);
    tcg_temp_free_i64(tmp);
#endif
}

#define GEN_SPE(name0, name1, opc2, opc3, inval, type)                        \
GEN_HANDLER(name0##_##name1, 0x04, opc2, opc3, inval, type)                   \
{                                                                             \
    if (Rc(ctx->opcode))                                                      \
        gen_##name1(ctx);                                                     \
    else                                                                      \
        gen_##name0(ctx);                                                     \
}

/* Handler for undefined SPE opcodes */
static always_inline void gen_speundef (DisasContext *ctx)
{
    gen_inval_exception(ctx, POWERPC_EXCP_INVAL_INVAL);
}

/* SPE logic */
#if defined(TARGET_PPC64)
#define GEN_SPEOP_LOGIC2(name, tcg_op)                                        \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    tcg_op(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)],                \
           cpu_gpr[rB(ctx->opcode)]);                                         \
}
#else
#define GEN_SPEOP_LOGIC2(name, tcg_op)                                        \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    tcg_op(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)],                \
           cpu_gpr[rB(ctx->opcode)]);                                         \
    tcg_op(cpu_gprh[rD(ctx->opcode)], cpu_gprh[rA(ctx->opcode)],              \
           cpu_gprh[rB(ctx->opcode)]);                                        \
}
#endif

GEN_SPEOP_LOGIC2(evand, tcg_gen_and_tl);
GEN_SPEOP_LOGIC2(evandc, tcg_gen_andc_tl);
GEN_SPEOP_LOGIC2(evxor, tcg_gen_xor_tl);
GEN_SPEOP_LOGIC2(evor, tcg_gen_or_tl);
GEN_SPEOP_LOGIC2(evnor, tcg_gen_nor_tl);
GEN_SPEOP_LOGIC2(eveqv, tcg_gen_eqv_tl);
GEN_SPEOP_LOGIC2(evorc, tcg_gen_orc_tl);
GEN_SPEOP_LOGIC2(evnand, tcg_gen_nand_tl);

/* SPE logic immediate */
#if defined(TARGET_PPC64)
#define GEN_SPEOP_TCG_LOGIC_IMM2(name, tcg_opi)                               \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    TCGv_i32 t0 = tcg_temp_local_new_i32();                                   \
    TCGv_i32 t1 = tcg_temp_local_new_i32();                                   \
    TCGv_i64 t2 = tcg_temp_local_new_i64();                                   \
    tcg_gen_trunc_i64_i32(t0, cpu_gpr[rA(ctx->opcode)]);                      \
    tcg_opi(t0, t0, rB(ctx->opcode));                                         \
    tcg_gen_shri_i64(t2, cpu_gpr[rA(ctx->opcode)], 32);                       \
    tcg_gen_trunc_i64_i32(t1, t2);                                            \
    tcg_temp_free_i64(t2);                                                    \
    tcg_opi(t1, t1, rB(ctx->opcode));                                         \
    tcg_gen_concat_i32_i64(cpu_gpr[rD(ctx->opcode)], t0, t1);                 \
    tcg_temp_free_i32(t0);                                                    \
    tcg_temp_free_i32(t1);                                                    \
}
#else
#define GEN_SPEOP_TCG_LOGIC_IMM2(name, tcg_opi)                               \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    tcg_opi(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)],               \
            rB(ctx->opcode));                                                 \
    tcg_opi(cpu_gprh[rD(ctx->opcode)], cpu_gprh[rA(ctx->opcode)],             \
            rB(ctx->opcode));                                                 \
}
#endif
GEN_SPEOP_TCG_LOGIC_IMM2(evslwi, tcg_gen_shli_i32);
GEN_SPEOP_TCG_LOGIC_IMM2(evsrwiu, tcg_gen_shri_i32);
GEN_SPEOP_TCG_LOGIC_IMM2(evsrwis, tcg_gen_sari_i32);
GEN_SPEOP_TCG_LOGIC_IMM2(evrlwi, tcg_gen_rotli_i32);

/* SPE arithmetic */
#if defined(TARGET_PPC64)
#define GEN_SPEOP_ARITH1(name, tcg_op)                                        \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    TCGv_i32 t0 = tcg_temp_local_new_i32();                                   \
    TCGv_i32 t1 = tcg_temp_local_new_i32();                                   \
    TCGv_i64 t2 = tcg_temp_local_new_i64();                                   \
    tcg_gen_trunc_i64_i32(t0, cpu_gpr[rA(ctx->opcode)]);                      \
    tcg_op(t0, t0);                                                           \
    tcg_gen_shri_i64(t2, cpu_gpr[rA(ctx->opcode)], 32);                       \
    tcg_gen_trunc_i64_i32(t1, t2);                                            \
    tcg_temp_free_i64(t2);                                                    \
    tcg_op(t1, t1);                                                           \
    tcg_gen_concat_i32_i64(cpu_gpr[rD(ctx->opcode)], t0, t1);                 \
    tcg_temp_free_i32(t0);                                                    \
    tcg_temp_free_i32(t1);                                                    \
}
#else
#define GEN_SPEOP_ARITH1(name, tcg_op)                                        \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    tcg_op(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);               \
    tcg_op(cpu_gprh[rD(ctx->opcode)], cpu_gprh[rA(ctx->opcode)]);             \
}
#endif

static always_inline void gen_op_evabs (TCGv_i32 ret, TCGv_i32 arg1)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();

    tcg_gen_brcondi_i32(TCG_COND_GE, arg1, 0, l1);
    tcg_gen_neg_i32(ret, arg1);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_mov_i32(ret, arg1);
    gen_set_label(l2);
}
GEN_SPEOP_ARITH1(evabs, gen_op_evabs);
GEN_SPEOP_ARITH1(evneg, tcg_gen_neg_i32);
GEN_SPEOP_ARITH1(evextsb, tcg_gen_ext8s_i32);
GEN_SPEOP_ARITH1(evextsh, tcg_gen_ext16s_i32);
static always_inline void gen_op_evrndw (TCGv_i32 ret, TCGv_i32 arg1)
{
    tcg_gen_addi_i32(ret, arg1, 0x8000);
    tcg_gen_ext16u_i32(ret, ret);
}
GEN_SPEOP_ARITH1(evrndw, gen_op_evrndw);
GEN_SPEOP_ARITH1(evcntlsw, gen_helper_cntlsw32);
GEN_SPEOP_ARITH1(evcntlzw, gen_helper_cntlzw32);

#if defined(TARGET_PPC64)
#define GEN_SPEOP_ARITH2(name, tcg_op)                                        \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    TCGv_i32 t0 = tcg_temp_local_new_i32();                                   \
    TCGv_i32 t1 = tcg_temp_local_new_i32();                                   \
    TCGv_i32 t2 = tcg_temp_local_new_i32();                                   \
    TCGv_i64 t3 = tcg_temp_local_new_i64();                                   \
    tcg_gen_trunc_i64_i32(t0, cpu_gpr[rA(ctx->opcode)]);                      \
    tcg_gen_trunc_i64_i32(t2, cpu_gpr[rB(ctx->opcode)]);                      \
    tcg_op(t0, t0, t2);                                                       \
    tcg_gen_shri_i64(t3, cpu_gpr[rA(ctx->opcode)], 32);                       \
    tcg_gen_trunc_i64_i32(t1, t3);                                            \
    tcg_gen_shri_i64(t3, cpu_gpr[rB(ctx->opcode)], 32);                       \
    tcg_gen_trunc_i64_i32(t2, t3);                                            \
    tcg_temp_free_i64(t3);                                                    \
    tcg_op(t1, t1, t2);                                                       \
    tcg_temp_free_i32(t2);                                                    \
    tcg_gen_concat_i32_i64(cpu_gpr[rD(ctx->opcode)], t0, t1);                 \
    tcg_temp_free_i32(t0);                                                    \
    tcg_temp_free_i32(t1);                                                    \
}
#else
#define GEN_SPEOP_ARITH2(name, tcg_op)                                        \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    tcg_op(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)],                \
           cpu_gpr[rB(ctx->opcode)]);                                         \
    tcg_op(cpu_gprh[rD(ctx->opcode)], cpu_gprh[rA(ctx->opcode)],              \
           cpu_gprh[rB(ctx->opcode)]);                                        \
}
#endif

static always_inline void gen_op_evsrwu (TCGv_i32 ret, TCGv_i32 arg1, TCGv_i32 arg2)
{
    TCGv_i32 t0;
    int l1, l2;

    l1 = gen_new_label();
    l2 = gen_new_label();
    t0 = tcg_temp_local_new_i32();
    /* No error here: 6 bits are used */
    tcg_gen_andi_i32(t0, arg2, 0x3F);
    tcg_gen_brcondi_i32(TCG_COND_GE, t0, 32, l1);
    tcg_gen_shr_i32(ret, arg1, t0);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_movi_i32(ret, 0);
    tcg_gen_br(l2);
    tcg_temp_free_i32(t0);
}
GEN_SPEOP_ARITH2(evsrwu, gen_op_evsrwu);
static always_inline void gen_op_evsrws (TCGv_i32 ret, TCGv_i32 arg1, TCGv_i32 arg2)
{
    TCGv_i32 t0;
    int l1, l2;

    l1 = gen_new_label();
    l2 = gen_new_label();
    t0 = tcg_temp_local_new_i32();
    /* No error here: 6 bits are used */
    tcg_gen_andi_i32(t0, arg2, 0x3F);
    tcg_gen_brcondi_i32(TCG_COND_GE, t0, 32, l1);
    tcg_gen_sar_i32(ret, arg1, t0);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_movi_i32(ret, 0);
    tcg_gen_br(l2);
    tcg_temp_free_i32(t0);
}
GEN_SPEOP_ARITH2(evsrws, gen_op_evsrws);
static always_inline void gen_op_evslw (TCGv_i32 ret, TCGv_i32 arg1, TCGv_i32 arg2)
{
    TCGv_i32 t0;
    int l1, l2;

    l1 = gen_new_label();
    l2 = gen_new_label();
    t0 = tcg_temp_local_new_i32();
    /* No error here: 6 bits are used */
    tcg_gen_andi_i32(t0, arg2, 0x3F);
    tcg_gen_brcondi_i32(TCG_COND_GE, t0, 32, l1);
    tcg_gen_shl_i32(ret, arg1, t0);
    tcg_gen_br(l2);
    gen_set_label(l1);
    tcg_gen_movi_i32(ret, 0);
    tcg_gen_br(l2);
    tcg_temp_free_i32(t0);
}
GEN_SPEOP_ARITH2(evslw, gen_op_evslw);
static always_inline void gen_op_evrlw (TCGv_i32 ret, TCGv_i32 arg1, TCGv_i32 arg2)
{
    TCGv_i32 t0 = tcg_temp_new_i32();
    tcg_gen_andi_i32(t0, arg2, 0x1F);
    tcg_gen_rotl_i32(ret, arg1, t0);
    tcg_temp_free_i32(t0);
}
GEN_SPEOP_ARITH2(evrlw, gen_op_evrlw);
static always_inline void gen_evmergehi (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
#if defined(TARGET_PPC64)
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_shri_tl(t0, cpu_gpr[rB(ctx->opcode)], 32);
    tcg_gen_andi_tl(t1, cpu_gpr[rA(ctx->opcode)], 0xFFFFFFFF0000000ULL);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
#else
    tcg_gen_mov_i32(cpu_gpr[rD(ctx->opcode)], cpu_gprh[rB(ctx->opcode)]);
    tcg_gen_mov_i32(cpu_gprh[rD(ctx->opcode)], cpu_gprh[rA(ctx->opcode)]);
#endif
}
GEN_SPEOP_ARITH2(evaddw, tcg_gen_add_i32);
static always_inline void gen_op_evsubf (TCGv_i32 ret, TCGv_i32 arg1, TCGv_i32 arg2)
{
    tcg_gen_sub_i32(ret, arg2, arg1);
}
GEN_SPEOP_ARITH2(evsubfw, gen_op_evsubf);

/* SPE arithmetic immediate */
#if defined(TARGET_PPC64)
#define GEN_SPEOP_ARITH_IMM2(name, tcg_op)                                    \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    TCGv_i32 t0 = tcg_temp_local_new_i32();                                   \
    TCGv_i32 t1 = tcg_temp_local_new_i32();                                   \
    TCGv_i64 t2 = tcg_temp_local_new_i64();                                   \
    tcg_gen_trunc_i64_i32(t0, cpu_gpr[rB(ctx->opcode)]);                      \
    tcg_op(t0, t0, rA(ctx->opcode));                                          \
    tcg_gen_shri_i64(t2, cpu_gpr[rB(ctx->opcode)], 32);                       \
    tcg_gen_trunc_i64_i32(t1, t2);                                            \
    tcg_temp_free_i64(t2);                                                    \
    tcg_op(t1, t1, rA(ctx->opcode));                                          \
    tcg_gen_concat_i32_i64(cpu_gpr[rD(ctx->opcode)], t0, t1);                 \
    tcg_temp_free_i32(t0);                                                    \
    tcg_temp_free_i32(t1);                                                    \
}
#else
#define GEN_SPEOP_ARITH_IMM2(name, tcg_op)                                    \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    tcg_op(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rB(ctx->opcode)],                \
           rA(ctx->opcode));                                                  \
    tcg_op(cpu_gprh[rD(ctx->opcode)], cpu_gprh[rB(ctx->opcode)],              \
           rA(ctx->opcode));                                                  \
}
#endif
GEN_SPEOP_ARITH_IMM2(evaddiw, tcg_gen_addi_i32);
GEN_SPEOP_ARITH_IMM2(evsubifw, tcg_gen_subi_i32);

/* SPE comparison */
#if defined(TARGET_PPC64)
#define GEN_SPEOP_COMP(name, tcg_cond)                                        \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    int l1 = gen_new_label();                                                 \
    int l2 = gen_new_label();                                                 \
    int l3 = gen_new_label();                                                 \
    int l4 = gen_new_label();                                                 \
    TCGv_i32 t0 = tcg_temp_local_new_i32();                                   \
    TCGv_i32 t1 = tcg_temp_local_new_i32();                                   \
    TCGv_i64 t2 = tcg_temp_local_new_i64();                                   \
    tcg_gen_trunc_i64_i32(t0, cpu_gpr[rA(ctx->opcode)]);                      \
    tcg_gen_trunc_i64_i32(t1, cpu_gpr[rB(ctx->opcode)]);                      \
    tcg_gen_brcond_i32(tcg_cond, t0, t1, l1);                                 \
    tcg_gen_movi_i32(cpu_crf[crfD(ctx->opcode)], 0);                          \
    tcg_gen_br(l2);                                                           \
    gen_set_label(l1);                                                        \
    tcg_gen_movi_i32(cpu_crf[crfD(ctx->opcode)],                              \
                     CRF_CL | CRF_CH_OR_CL | CRF_CH_AND_CL);                  \
    gen_set_label(l2);                                                        \
    tcg_gen_shri_i64(t2, cpu_gpr[rA(ctx->opcode)], 32);                       \
    tcg_gen_trunc_i64_i32(t0, t2);                                            \
    tcg_gen_shri_i64(t2, cpu_gpr[rB(ctx->opcode)], 32);                       \
    tcg_gen_trunc_i64_i32(t1, t2);                                            \
    tcg_temp_free_i64(t2);                                                    \
    tcg_gen_brcond_i32(tcg_cond, t0, t1, l3);                                 \
    tcg_gen_andi_i32(cpu_crf[crfD(ctx->opcode)], cpu_crf[crfD(ctx->opcode)],  \
                     ~(CRF_CH | CRF_CH_AND_CL));                              \
    tcg_gen_br(l4);                                                           \
    gen_set_label(l3);                                                        \
    tcg_gen_ori_i32(cpu_crf[crfD(ctx->opcode)], cpu_crf[crfD(ctx->opcode)],   \
                    CRF_CH | CRF_CH_OR_CL);                                   \
    gen_set_label(l4);                                                        \
    tcg_temp_free_i32(t0);                                                    \
    tcg_temp_free_i32(t1);                                                    \
}
#else
#define GEN_SPEOP_COMP(name, tcg_cond)                                        \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    int l1 = gen_new_label();                                                 \
    int l2 = gen_new_label();                                                 \
    int l3 = gen_new_label();                                                 \
    int l4 = gen_new_label();                                                 \
                                                                              \
    tcg_gen_brcond_i32(tcg_cond, cpu_gpr[rA(ctx->opcode)],                    \
                       cpu_gpr[rB(ctx->opcode)], l1);                         \
    tcg_gen_movi_tl(cpu_crf[crfD(ctx->opcode)], 0);                           \
    tcg_gen_br(l2);                                                           \
    gen_set_label(l1);                                                        \
    tcg_gen_movi_i32(cpu_crf[crfD(ctx->opcode)],                              \
                     CRF_CL | CRF_CH_OR_CL | CRF_CH_AND_CL);                  \
    gen_set_label(l2);                                                        \
    tcg_gen_brcond_i32(tcg_cond, cpu_gprh[rA(ctx->opcode)],                   \
                       cpu_gprh[rB(ctx->opcode)], l3);                        \
    tcg_gen_andi_i32(cpu_crf[crfD(ctx->opcode)], cpu_crf[crfD(ctx->opcode)],  \
                     ~(CRF_CH | CRF_CH_AND_CL));                              \
    tcg_gen_br(l4);                                                           \
    gen_set_label(l3);                                                        \
    tcg_gen_ori_i32(cpu_crf[crfD(ctx->opcode)], cpu_crf[crfD(ctx->opcode)],   \
                    CRF_CH | CRF_CH_OR_CL);                                   \
    gen_set_label(l4);                                                        \
}
#endif
GEN_SPEOP_COMP(evcmpgtu, TCG_COND_GTU);
GEN_SPEOP_COMP(evcmpgts, TCG_COND_GT);
GEN_SPEOP_COMP(evcmpltu, TCG_COND_LTU);
GEN_SPEOP_COMP(evcmplts, TCG_COND_LT);
GEN_SPEOP_COMP(evcmpeq, TCG_COND_EQ);

/* SPE misc */
static always_inline void gen_brinc (DisasContext *ctx)
{
    /* Note: brinc is usable even if SPE is disabled */
    gen_helper_brinc(cpu_gpr[rD(ctx->opcode)],
                     cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
}
static always_inline void gen_evmergelo (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
#if defined(TARGET_PPC64)
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x00000000FFFFFFFFLL);
    tcg_gen_shli_tl(t1, cpu_gpr[rA(ctx->opcode)], 32);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
#else
    tcg_gen_mov_i32(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
    tcg_gen_mov_i32(cpu_gprh[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
#endif
}
static always_inline void gen_evmergehilo (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
#if defined(TARGET_PPC64)
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_andi_tl(t0, cpu_gpr[rB(ctx->opcode)], 0x00000000FFFFFFFFLL);
    tcg_gen_andi_tl(t1, cpu_gpr[rA(ctx->opcode)], 0xFFFFFFFF0000000ULL);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
#else
    tcg_gen_mov_i32(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
    tcg_gen_mov_i32(cpu_gprh[rD(ctx->opcode)], cpu_gprh[rA(ctx->opcode)]);
#endif
}
static always_inline void gen_evmergelohi (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
#if defined(TARGET_PPC64)
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    tcg_gen_shri_tl(t0, cpu_gpr[rB(ctx->opcode)], 32);
    tcg_gen_shli_tl(t1, cpu_gpr[rA(ctx->opcode)], 32);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], t0, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
#else
    tcg_gen_mov_i32(cpu_gpr[rD(ctx->opcode)], cpu_gprh[rB(ctx->opcode)]);
    tcg_gen_mov_i32(cpu_gprh[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
#endif
}
static always_inline void gen_evsplati (DisasContext *ctx)
{
    uint64_t imm = ((int32_t)(rA(ctx->opcode) << 11)) >> 27;

#if defined(TARGET_PPC64)
    tcg_gen_movi_tl(cpu_gpr[rD(ctx->opcode)], (imm << 32) | imm);
#else
    tcg_gen_movi_i32(cpu_gpr[rD(ctx->opcode)], imm);
    tcg_gen_movi_i32(cpu_gprh[rD(ctx->opcode)], imm);
#endif
}
static always_inline void gen_evsplatfi (DisasContext *ctx)
{
    uint64_t imm = rA(ctx->opcode) << 11;

#if defined(TARGET_PPC64)
    tcg_gen_movi_tl(cpu_gpr[rD(ctx->opcode)], (imm << 32) | imm);
#else
    tcg_gen_movi_i32(cpu_gpr[rD(ctx->opcode)], imm);
    tcg_gen_movi_i32(cpu_gprh[rD(ctx->opcode)], imm);
#endif
}

static always_inline void gen_evsel (DisasContext *ctx)
{
    int l1 = gen_new_label();
    int l2 = gen_new_label();
    int l3 = gen_new_label();
    int l4 = gen_new_label();
    TCGv_i32 t0 = tcg_temp_local_new_i32();
#if defined(TARGET_PPC64)
    TCGv t1 = tcg_temp_local_new();
    TCGv t2 = tcg_temp_local_new();
#endif
    tcg_gen_andi_i32(t0, cpu_crf[ctx->opcode & 0x07], 1 << 3);
    tcg_gen_brcondi_i32(TCG_COND_EQ, t0, 0, l1);
#if defined(TARGET_PPC64)
    tcg_gen_andi_tl(t1, cpu_gpr[rA(ctx->opcode)], 0xFFFFFFFF00000000ULL);
#else
    tcg_gen_mov_tl(cpu_gprh[rD(ctx->opcode)], cpu_gprh[rA(ctx->opcode)]);
#endif
    tcg_gen_br(l2);
    gen_set_label(l1);
#if defined(TARGET_PPC64)
    tcg_gen_andi_tl(t1, cpu_gpr[rB(ctx->opcode)], 0xFFFFFFFF00000000ULL);
#else
    tcg_gen_mov_tl(cpu_gprh[rD(ctx->opcode)], cpu_gprh[rB(ctx->opcode)]);
#endif
    gen_set_label(l2);
    tcg_gen_andi_i32(t0, cpu_crf[ctx->opcode & 0x07], 1 << 2);
    tcg_gen_brcondi_i32(TCG_COND_EQ, t0, 0, l3);
#if defined(TARGET_PPC64)
    tcg_gen_andi_tl(t2, cpu_gpr[rA(ctx->opcode)], 0x00000000FFFFFFFFULL);
#else
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)]);
#endif
    tcg_gen_br(l4);
    gen_set_label(l3);
#if defined(TARGET_PPC64)
    tcg_gen_andi_tl(t2, cpu_gpr[rB(ctx->opcode)], 0x00000000FFFFFFFFULL);
#else
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);
#endif
    gen_set_label(l4);
    tcg_temp_free_i32(t0);
#if defined(TARGET_PPC64)
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], t1, t2);
    tcg_temp_free(t1);
    tcg_temp_free(t2);
#endif
}
GEN_HANDLER2(evsel0, "evsel", 0x04, 0x1c, 0x09, 0x00000000, PPC_SPE)
{
    gen_evsel(ctx);
}
GEN_HANDLER2(evsel1, "evsel", 0x04, 0x1d, 0x09, 0x00000000, PPC_SPE)
{
    gen_evsel(ctx);
}
GEN_HANDLER2(evsel2, "evsel", 0x04, 0x1e, 0x09, 0x00000000, PPC_SPE)
{
    gen_evsel(ctx);
}
GEN_HANDLER2(evsel3, "evsel", 0x04, 0x1f, 0x09, 0x00000000, PPC_SPE)
{
    gen_evsel(ctx);
}

GEN_SPE(evaddw,         speundef,      0x00, 0x08, 0x00000000, PPC_SPE); ////
GEN_SPE(evaddiw,        speundef,      0x01, 0x08, 0x00000000, PPC_SPE);
GEN_SPE(evsubfw,        speundef,      0x02, 0x08, 0x00000000, PPC_SPE); ////
GEN_SPE(evsubifw,       speundef,      0x03, 0x08, 0x00000000, PPC_SPE);
GEN_SPE(evabs,          evneg,         0x04, 0x08, 0x0000F800, PPC_SPE); ////
GEN_SPE(evextsb,        evextsh,       0x05, 0x08, 0x0000F800, PPC_SPE); ////
GEN_SPE(evrndw,         evcntlzw,      0x06, 0x08, 0x0000F800, PPC_SPE); ////
GEN_SPE(evcntlsw,       brinc,         0x07, 0x08, 0x00000000, PPC_SPE); //
GEN_SPE(speundef,       evand,         0x08, 0x08, 0x00000000, PPC_SPE); ////
GEN_SPE(evandc,         speundef,      0x09, 0x08, 0x00000000, PPC_SPE); ////
GEN_SPE(evxor,          evor,          0x0B, 0x08, 0x00000000, PPC_SPE); ////
GEN_SPE(evnor,          eveqv,         0x0C, 0x08, 0x00000000, PPC_SPE); ////
GEN_SPE(speundef,       evorc,         0x0D, 0x08, 0x00000000, PPC_SPE); ////
GEN_SPE(evnand,         speundef,      0x0F, 0x08, 0x00000000, PPC_SPE); ////
GEN_SPE(evsrwu,         evsrws,        0x10, 0x08, 0x00000000, PPC_SPE); ////
GEN_SPE(evsrwiu,        evsrwis,       0x11, 0x08, 0x00000000, PPC_SPE);
GEN_SPE(evslw,          speundef,      0x12, 0x08, 0x00000000, PPC_SPE); ////
GEN_SPE(evslwi,         speundef,      0x13, 0x08, 0x00000000, PPC_SPE);
GEN_SPE(evrlw,          evsplati,      0x14, 0x08, 0x00000000, PPC_SPE); //
GEN_SPE(evrlwi,         evsplatfi,     0x15, 0x08, 0x00000000, PPC_SPE);
GEN_SPE(evmergehi,      evmergelo,     0x16, 0x08, 0x00000000, PPC_SPE); ////
GEN_SPE(evmergehilo,    evmergelohi,   0x17, 0x08, 0x00000000, PPC_SPE); ////
GEN_SPE(evcmpgtu,       evcmpgts,      0x18, 0x08, 0x00600000, PPC_SPE); ////
GEN_SPE(evcmpltu,       evcmplts,      0x19, 0x08, 0x00600000, PPC_SPE); ////
GEN_SPE(evcmpeq,        speundef,      0x1A, 0x08, 0x00600000, PPC_SPE); ////

/* SPE load and stores */
static always_inline void gen_addr_spe_imm_index (DisasContext *ctx, TCGv EA, int sh)
{
    target_ulong uimm = rB(ctx->opcode);

    if (rA(ctx->opcode) == 0) {
        tcg_gen_movi_tl(EA, uimm << sh);
    } else {
        tcg_gen_addi_tl(EA, cpu_gpr[rA(ctx->opcode)], uimm << sh);
#if defined(TARGET_PPC64)
        if (!ctx->sf_mode) {
            tcg_gen_ext32u_tl(EA, EA);
        }
#endif
    }
}

static always_inline void gen_op_evldd(DisasContext *ctx, TCGv addr)
{
#if defined(TARGET_PPC64)
    gen_qemu_ld64(ctx, cpu_gpr[rD(ctx->opcode)], addr);
#else
    TCGv_i64 t0 = tcg_temp_new_i64();
    gen_qemu_ld64(ctx, t0, addr);
    tcg_gen_trunc_i64_i32(cpu_gpr[rD(ctx->opcode)], t0);
    tcg_gen_shri_i64(t0, t0, 32);
    tcg_gen_trunc_i64_i32(cpu_gprh[rD(ctx->opcode)], t0);
    tcg_temp_free_i64(t0);
#endif
}

static always_inline void gen_op_evldw(DisasContext *ctx, TCGv addr)
{
#if defined(TARGET_PPC64)
    TCGv t0 = tcg_temp_new();
    gen_qemu_ld32u(ctx, t0, addr);
    tcg_gen_shli_tl(cpu_gpr[rD(ctx->opcode)], t0, 32);
    gen_addr_add(ctx, addr, addr, 4);
    gen_qemu_ld32u(ctx, t0, addr);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
#else
    gen_qemu_ld32u(ctx, cpu_gprh[rD(ctx->opcode)], addr);
    gen_addr_add(ctx, addr, addr, 4);
    gen_qemu_ld32u(ctx, cpu_gpr[rD(ctx->opcode)], addr);
#endif
}

static always_inline void gen_op_evldh(DisasContext *ctx, TCGv addr)
{
    TCGv t0 = tcg_temp_new();
#if defined(TARGET_PPC64)
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(cpu_gpr[rD(ctx->opcode)], t0, 48);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(t0, t0, 32);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(t0, t0, 16);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
#else
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(cpu_gprh[rD(ctx->opcode)], t0, 16);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_or_tl(cpu_gprh[rD(ctx->opcode)], cpu_gprh[rD(ctx->opcode)], t0);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(cpu_gprh[rD(ctx->opcode)], t0, 16);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
#endif
    tcg_temp_free(t0);
}

static always_inline void gen_op_evlhhesplat(DisasContext *ctx, TCGv addr)
{
    TCGv t0 = tcg_temp_new();
    gen_qemu_ld16u(ctx, t0, addr);
#if defined(TARGET_PPC64)
    tcg_gen_shli_tl(cpu_gpr[rD(ctx->opcode)], t0, 48);
    tcg_gen_shli_tl(t0, t0, 16);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
#else
    tcg_gen_shli_tl(t0, t0, 16);
    tcg_gen_mov_tl(cpu_gprh[rD(ctx->opcode)], t0);
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], t0);
#endif
    tcg_temp_free(t0);
}

static always_inline void gen_op_evlhhousplat(DisasContext *ctx, TCGv addr)
{
    TCGv t0 = tcg_temp_new();
    gen_qemu_ld16u(ctx, t0, addr);
#if defined(TARGET_PPC64)
    tcg_gen_shli_tl(cpu_gpr[rD(ctx->opcode)], t0, 32);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
#else
    tcg_gen_mov_tl(cpu_gprh[rD(ctx->opcode)], t0);
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], t0);
#endif
    tcg_temp_free(t0);
}

static always_inline void gen_op_evlhhossplat(DisasContext *ctx, TCGv addr)
{
    TCGv t0 = tcg_temp_new();
    gen_qemu_ld16s(ctx, t0, addr);
#if defined(TARGET_PPC64)
    tcg_gen_shli_tl(cpu_gpr[rD(ctx->opcode)], t0, 32);
    tcg_gen_ext32u_tl(t0, t0);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
#else
    tcg_gen_mov_tl(cpu_gprh[rD(ctx->opcode)], t0);
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], t0);
#endif
    tcg_temp_free(t0);
}

static always_inline void gen_op_evlwhe(DisasContext *ctx, TCGv addr)
{
    TCGv t0 = tcg_temp_new();
#if defined(TARGET_PPC64)
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(cpu_gpr[rD(ctx->opcode)], t0, 48);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(t0, t0, 16);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
#else
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(cpu_gprh[rD(ctx->opcode)], t0, 16);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(cpu_gpr[rD(ctx->opcode)], t0, 16);
#endif
    tcg_temp_free(t0);
}

static always_inline void gen_op_evlwhou(DisasContext *ctx, TCGv addr)
{
#if defined(TARGET_PPC64)
    TCGv t0 = tcg_temp_new();
    gen_qemu_ld16u(ctx, cpu_gpr[rD(ctx->opcode)], addr);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(t0, t0, 32);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
#else
    gen_qemu_ld16u(ctx, cpu_gprh[rD(ctx->opcode)], addr);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16u(ctx, cpu_gpr[rD(ctx->opcode)], addr);
#endif
}

static always_inline void gen_op_evlwhos(DisasContext *ctx, TCGv addr)
{
#if defined(TARGET_PPC64)
    TCGv t0 = tcg_temp_new();
    gen_qemu_ld16s(ctx, t0, addr);
    tcg_gen_ext32u_tl(cpu_gpr[rD(ctx->opcode)], t0);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16s(ctx, t0, addr);
    tcg_gen_shli_tl(t0, t0, 32);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
    tcg_temp_free(t0);
#else
    gen_qemu_ld16s(ctx, cpu_gprh[rD(ctx->opcode)], addr);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16s(ctx, cpu_gpr[rD(ctx->opcode)], addr);
#endif
}

static always_inline void gen_op_evlwwsplat(DisasContext *ctx, TCGv addr)
{
    TCGv t0 = tcg_temp_new();
    gen_qemu_ld32u(ctx, t0, addr);
#if defined(TARGET_PPC64)
    tcg_gen_shli_tl(cpu_gpr[rD(ctx->opcode)], t0, 32);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
#else
    tcg_gen_mov_tl(cpu_gprh[rD(ctx->opcode)], t0);
    tcg_gen_mov_tl(cpu_gpr[rD(ctx->opcode)], t0);
#endif
    tcg_temp_free(t0);
}

static always_inline void gen_op_evlwhsplat(DisasContext *ctx, TCGv addr)
{
    TCGv t0 = tcg_temp_new();
#if defined(TARGET_PPC64)
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(cpu_gpr[rD(ctx->opcode)], t0, 48);
    tcg_gen_shli_tl(t0, t0, 32);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
    tcg_gen_shli_tl(t0, t0, 16);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t0);
#else
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(cpu_gprh[rD(ctx->opcode)], t0, 16);
    tcg_gen_or_tl(cpu_gprh[rD(ctx->opcode)], cpu_gprh[rD(ctx->opcode)], t0);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_ld16u(ctx, t0, addr);
    tcg_gen_shli_tl(cpu_gpr[rD(ctx->opcode)], t0, 16);
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gprh[rD(ctx->opcode)], t0);
#endif
    tcg_temp_free(t0);
}

static always_inline void gen_op_evstdd(DisasContext *ctx, TCGv addr)
{
#if defined(TARGET_PPC64)
    gen_qemu_st64(ctx, cpu_gpr[rS(ctx->opcode)], addr);
#else
    TCGv_i64 t0 = tcg_temp_new_i64();
    tcg_gen_concat_i32_i64(t0, cpu_gpr[rS(ctx->opcode)], cpu_gprh[rS(ctx->opcode)]);
    gen_qemu_st64(ctx, t0, addr);
    tcg_temp_free_i64(t0);
#endif
}

static always_inline void gen_op_evstdw(DisasContext *ctx, TCGv addr)
{
#if defined(TARGET_PPC64)
    TCGv t0 = tcg_temp_new();
    tcg_gen_shri_tl(t0, cpu_gpr[rS(ctx->opcode)], 32);
    gen_qemu_st32(ctx, t0, addr);
    tcg_temp_free(t0);
#else
    gen_qemu_st32(ctx, cpu_gprh[rS(ctx->opcode)], addr);
#endif
    gen_addr_add(ctx, addr, addr, 4);
    gen_qemu_st32(ctx, cpu_gpr[rS(ctx->opcode)], addr);
}

static always_inline void gen_op_evstdh(DisasContext *ctx, TCGv addr)
{
    TCGv t0 = tcg_temp_new();
#if defined(TARGET_PPC64)
    tcg_gen_shri_tl(t0, cpu_gpr[rS(ctx->opcode)], 48);
#else
    tcg_gen_shri_tl(t0, cpu_gprh[rS(ctx->opcode)], 16);
#endif
    gen_qemu_st16(ctx, t0, addr);
    gen_addr_add(ctx, addr, addr, 2);
#if defined(TARGET_PPC64)
    tcg_gen_shri_tl(t0, cpu_gpr[rS(ctx->opcode)], 32);
    gen_qemu_st16(ctx, t0, addr);
#else
    gen_qemu_st16(ctx, cpu_gprh[rS(ctx->opcode)], addr);
#endif
    gen_addr_add(ctx, addr, addr, 2);
    tcg_gen_shri_tl(t0, cpu_gpr[rS(ctx->opcode)], 16);
    gen_qemu_st16(ctx, t0, addr);
    tcg_temp_free(t0);
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_st16(ctx, cpu_gpr[rS(ctx->opcode)], addr);
}

static always_inline void gen_op_evstwhe(DisasContext *ctx, TCGv addr)
{
    TCGv t0 = tcg_temp_new();
#if defined(TARGET_PPC64)
    tcg_gen_shri_tl(t0, cpu_gpr[rS(ctx->opcode)], 48);
#else
    tcg_gen_shri_tl(t0, cpu_gprh[rS(ctx->opcode)], 16);
#endif
    gen_qemu_st16(ctx, t0, addr);
    gen_addr_add(ctx, addr, addr, 2);
    tcg_gen_shri_tl(t0, cpu_gpr[rS(ctx->opcode)], 16);
    gen_qemu_st16(ctx, t0, addr);
    tcg_temp_free(t0);
}

static always_inline void gen_op_evstwho(DisasContext *ctx, TCGv addr)
{
#if defined(TARGET_PPC64)
    TCGv t0 = tcg_temp_new();
    tcg_gen_shri_tl(t0, cpu_gpr[rS(ctx->opcode)], 32);
    gen_qemu_st16(ctx, t0, addr);
    tcg_temp_free(t0);
#else
    gen_qemu_st16(ctx, cpu_gprh[rS(ctx->opcode)], addr);
#endif
    gen_addr_add(ctx, addr, addr, 2);
    gen_qemu_st16(ctx, cpu_gpr[rS(ctx->opcode)], addr);
}

static always_inline void gen_op_evstwwe(DisasContext *ctx, TCGv addr)
{
#if defined(TARGET_PPC64)
    TCGv t0 = tcg_temp_new();
    tcg_gen_shri_tl(t0, cpu_gpr[rS(ctx->opcode)], 32);
    gen_qemu_st32(ctx, t0, addr);
    tcg_temp_free(t0);
#else
    gen_qemu_st32(ctx, cpu_gprh[rS(ctx->opcode)], addr);
#endif
}

static always_inline void gen_op_evstwwo(DisasContext *ctx, TCGv addr)
{
    gen_qemu_st32(ctx, cpu_gpr[rS(ctx->opcode)], addr);
}

#define GEN_SPEOP_LDST(name, opc2, sh)                                        \
GEN_HANDLER(name, 0x04, opc2, 0x0C, 0x00000000, PPC_SPE)                      \
{                                                                             \
    TCGv t0;                                                                  \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    gen_set_access_type(ctx, ACCESS_INT);                                     \
    t0 = tcg_temp_new();                                                      \
    if (Rc(ctx->opcode)) {                                                    \
        gen_addr_spe_imm_index(ctx, t0, sh);                                  \
    } else {                                                                  \
        gen_addr_reg_index(ctx, t0);                                          \
    }                                                                         \
    gen_op_##name(ctx, t0);                                                   \
    tcg_temp_free(t0);                                                        \
}

GEN_SPEOP_LDST(evldd, 0x00, 3);
GEN_SPEOP_LDST(evldw, 0x01, 3);
GEN_SPEOP_LDST(evldh, 0x02, 3);
GEN_SPEOP_LDST(evlhhesplat, 0x04, 1);
GEN_SPEOP_LDST(evlhhousplat, 0x06, 1);
GEN_SPEOP_LDST(evlhhossplat, 0x07, 1);
GEN_SPEOP_LDST(evlwhe, 0x08, 2);
GEN_SPEOP_LDST(evlwhou, 0x0A, 2);
GEN_SPEOP_LDST(evlwhos, 0x0B, 2);
GEN_SPEOP_LDST(evlwwsplat, 0x0C, 2);
GEN_SPEOP_LDST(evlwhsplat, 0x0E, 2);

GEN_SPEOP_LDST(evstdd, 0x10, 3);
GEN_SPEOP_LDST(evstdw, 0x11, 3);
GEN_SPEOP_LDST(evstdh, 0x12, 3);
GEN_SPEOP_LDST(evstwhe, 0x18, 2);
GEN_SPEOP_LDST(evstwho, 0x1A, 2);
GEN_SPEOP_LDST(evstwwe, 0x1C, 2);
GEN_SPEOP_LDST(evstwwo, 0x1E, 2);

/* Multiply and add - TODO */
#if 0
GEN_SPE(speundef,       evmhessf,      0x01, 0x10, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhossf,      0x03, 0x10, 0x00000000, PPC_SPE);
GEN_SPE(evmheumi,       evmhesmi,      0x04, 0x10, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhesmf,      0x05, 0x10, 0x00000000, PPC_SPE);
GEN_SPE(evmhoumi,       evmhosmi,      0x06, 0x10, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhosmf,      0x07, 0x10, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhessfa,     0x11, 0x10, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhossfa,     0x13, 0x10, 0x00000000, PPC_SPE);
GEN_SPE(evmheumia,      evmhesmia,     0x14, 0x10, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhesmfa,     0x15, 0x10, 0x00000000, PPC_SPE);
GEN_SPE(evmhoumia,      evmhosmia,     0x16, 0x10, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhosmfa,     0x17, 0x10, 0x00000000, PPC_SPE);

GEN_SPE(speundef,       evmwhssf,      0x03, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(evmwlumi,       speundef,      0x04, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(evmwhumi,       evmwhsmi,      0x06, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmwhsmf,      0x07, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmwssf,       0x09, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(evmwumi,        evmwsmi,       0x0C, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmwsmf,       0x0D, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmwhssfa,     0x13, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(evmwlumia,      speundef,      0x14, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(evmwhumia,      evmwhsmia,     0x16, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmwhsmfa,     0x17, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmwssfa,      0x19, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(evmwumia,       evmwsmia,      0x1C, 0x11, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmwsmfa,      0x1D, 0x11, 0x00000000, PPC_SPE);

GEN_SPE(evadduiaaw,     evaddsiaaw,    0x00, 0x13, 0x0000F800, PPC_SPE);
GEN_SPE(evsubfusiaaw,   evsubfssiaaw,  0x01, 0x13, 0x0000F800, PPC_SPE);
GEN_SPE(evaddumiaaw,    evaddsmiaaw,   0x04, 0x13, 0x0000F800, PPC_SPE);
GEN_SPE(evsubfumiaaw,   evsubfsmiaaw,  0x05, 0x13, 0x0000F800, PPC_SPE);
GEN_SPE(evdivws,        evdivwu,       0x06, 0x13, 0x00000000, PPC_SPE);
GEN_SPE(evmra,          speundef,      0x07, 0x13, 0x0000F800, PPC_SPE);

GEN_SPE(evmheusiaaw,    evmhessiaaw,   0x00, 0x14, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhessfaaw,   0x01, 0x14, 0x00000000, PPC_SPE);
GEN_SPE(evmhousiaaw,    evmhossiaaw,   0x02, 0x14, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhossfaaw,   0x03, 0x14, 0x00000000, PPC_SPE);
GEN_SPE(evmheumiaaw,    evmhesmiaaw,   0x04, 0x14, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhesmfaaw,   0x05, 0x14, 0x00000000, PPC_SPE);
GEN_SPE(evmhoumiaaw,    evmhosmiaaw,   0x06, 0x14, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhosmfaaw,   0x07, 0x14, 0x00000000, PPC_SPE);
GEN_SPE(evmhegumiaa,    evmhegsmiaa,   0x14, 0x14, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhegsmfaa,   0x15, 0x14, 0x00000000, PPC_SPE);
GEN_SPE(evmhogumiaa,    evmhogsmiaa,   0x16, 0x14, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhogsmfaa,   0x17, 0x14, 0x00000000, PPC_SPE);

GEN_SPE(evmwlusiaaw,    evmwlssiaaw,   0x00, 0x15, 0x00000000, PPC_SPE);
GEN_SPE(evmwlumiaaw,    evmwlsmiaaw,   0x04, 0x15, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmwssfaa,     0x09, 0x15, 0x00000000, PPC_SPE);
GEN_SPE(evmwumiaa,      evmwsmiaa,     0x0C, 0x15, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmwsmfaa,     0x0D, 0x15, 0x00000000, PPC_SPE);

GEN_SPE(evmheusianw,    evmhessianw,   0x00, 0x16, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhessfanw,   0x01, 0x16, 0x00000000, PPC_SPE);
GEN_SPE(evmhousianw,    evmhossianw,   0x02, 0x16, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhossfanw,   0x03, 0x16, 0x00000000, PPC_SPE);
GEN_SPE(evmheumianw,    evmhesmianw,   0x04, 0x16, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhesmfanw,   0x05, 0x16, 0x00000000, PPC_SPE);
GEN_SPE(evmhoumianw,    evmhosmianw,   0x06, 0x16, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhosmfanw,   0x07, 0x16, 0x00000000, PPC_SPE);
GEN_SPE(evmhegumian,    evmhegsmian,   0x14, 0x16, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhegsmfan,   0x15, 0x16, 0x00000000, PPC_SPE);
GEN_SPE(evmhigumian,    evmhigsmian,   0x16, 0x16, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmhogsmfan,   0x17, 0x16, 0x00000000, PPC_SPE);

GEN_SPE(evmwlusianw,    evmwlssianw,   0x00, 0x17, 0x00000000, PPC_SPE);
GEN_SPE(evmwlumianw,    evmwlsmianw,   0x04, 0x17, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmwssfan,     0x09, 0x17, 0x00000000, PPC_SPE);
GEN_SPE(evmwumian,      evmwsmian,     0x0C, 0x17, 0x00000000, PPC_SPE);
GEN_SPE(speundef,       evmwsmfan,     0x0D, 0x17, 0x00000000, PPC_SPE);
#endif

/***                      SPE floating-point extension                     ***/
#if defined(TARGET_PPC64)
#define GEN_SPEFPUOP_CONV_32_32(name)                                         \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    TCGv_i32 t0;                                                              \
    TCGv t1;                                                                  \
    t0 = tcg_temp_new_i32();                                                  \
    tcg_gen_trunc_tl_i32(t0, cpu_gpr[rB(ctx->opcode)]);                       \
    gen_helper_##name(t0, t0);                                                \
    t1 = tcg_temp_new();                                                      \
    tcg_gen_extu_i32_tl(t1, t0);                                              \
    tcg_temp_free_i32(t0);                                                    \
    tcg_gen_andi_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)],       \
                    0xFFFFFFFF00000000ULL);                                   \
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t1);    \
    tcg_temp_free(t1);                                                        \
}
#define GEN_SPEFPUOP_CONV_32_64(name)                                         \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    TCGv_i32 t0;                                                              \
    TCGv t1;                                                                  \
    t0 = tcg_temp_new_i32();                                                  \
    gen_helper_##name(t0, cpu_gpr[rB(ctx->opcode)]);                          \
    t1 = tcg_temp_new();                                                      \
    tcg_gen_extu_i32_tl(t1, t0);                                              \
    tcg_temp_free_i32(t0);                                                    \
    tcg_gen_andi_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)],       \
                    0xFFFFFFFF00000000ULL);                                   \
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t1);    \
    tcg_temp_free(t1);                                                        \
}
#define GEN_SPEFPUOP_CONV_64_32(name)                                         \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    TCGv_i32 t0 = tcg_temp_new_i32();                                         \
    tcg_gen_trunc_tl_i32(t0, cpu_gpr[rB(ctx->opcode)]);                       \
    gen_helper_##name(cpu_gpr[rD(ctx->opcode)], t0);                          \
    tcg_temp_free_i32(t0);                                                    \
}
#define GEN_SPEFPUOP_CONV_64_64(name)                                         \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    gen_helper_##name(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);    \
}
#define GEN_SPEFPUOP_ARITH2_32_32(name)                                       \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    TCGv_i32 t0, t1;                                                          \
    TCGv_i64 t2;                                                              \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    t0 = tcg_temp_new_i32();                                                  \
    t1 = tcg_temp_new_i32();                                                  \
    tcg_gen_trunc_tl_i32(t0, cpu_gpr[rA(ctx->opcode)]);                       \
    tcg_gen_trunc_tl_i32(t1, cpu_gpr[rB(ctx->opcode)]);                       \
    gen_helper_##name(t0, t0, t1);                                            \
    tcg_temp_free_i32(t1);                                                    \
    t2 = tcg_temp_new();                                                      \
    tcg_gen_extu_i32_tl(t2, t0);                                              \
    tcg_temp_free_i32(t0);                                                    \
    tcg_gen_andi_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)],       \
                    0xFFFFFFFF00000000ULL);                                   \
    tcg_gen_or_tl(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rD(ctx->opcode)], t2);    \
    tcg_temp_free(t2);                                                        \
}
#define GEN_SPEFPUOP_ARITH2_64_64(name)                                       \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    gen_helper_##name(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rA(ctx->opcode)],     \
                      cpu_gpr[rB(ctx->opcode)]);                              \
}
#define GEN_SPEFPUOP_COMP_32(name)                                            \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    TCGv_i32 t0, t1;                                                          \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    t0 = tcg_temp_new_i32();                                                  \
    t1 = tcg_temp_new_i32();                                                  \
    tcg_gen_trunc_tl_i32(t0, cpu_gpr[rA(ctx->opcode)]);                       \
    tcg_gen_trunc_tl_i32(t1, cpu_gpr[rB(ctx->opcode)]);                       \
    gen_helper_##name(cpu_crf[crfD(ctx->opcode)], t0, t1);                    \
    tcg_temp_free_i32(t0);                                                    \
    tcg_temp_free_i32(t1);                                                    \
}
#define GEN_SPEFPUOP_COMP_64(name)                                            \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    gen_helper_##name(cpu_crf[crfD(ctx->opcode)],                             \
                      cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);    \
}
#else
#define GEN_SPEFPUOP_CONV_32_32(name)                                         \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    gen_helper_##name(cpu_gpr[rD(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);    \
}
#define GEN_SPEFPUOP_CONV_32_64(name)                                         \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    TCGv_i64 t0 = tcg_temp_new_i64();                                         \
    gen_load_gpr64(t0, rB(ctx->opcode));                                      \
    gen_helper_##name(cpu_gpr[rD(ctx->opcode)], t0);                          \
    tcg_temp_free_i64(t0);                                                    \
}
#define GEN_SPEFPUOP_CONV_64_32(name)                                         \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    TCGv_i64 t0 = tcg_temp_new_i64();                                         \
    gen_helper_##name(t0, cpu_gpr[rB(ctx->opcode)]);                          \
    gen_store_gpr64(rD(ctx->opcode), t0);                                     \
    tcg_temp_free_i64(t0);                                                    \
}
#define GEN_SPEFPUOP_CONV_64_64(name)                                         \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    TCGv_i64 t0 = tcg_temp_new_i64();                                         \
    gen_load_gpr64(t0, rB(ctx->opcode));                                      \
    gen_helper_##name(t0, t0);                                                \
    gen_store_gpr64(rD(ctx->opcode), t0);                                     \
    tcg_temp_free_i64(t0);                                                    \
}
#define GEN_SPEFPUOP_ARITH2_32_32(name)                                       \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    gen_helper_##name(cpu_gpr[rD(ctx->opcode)],                               \
                      cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);    \
}
#define GEN_SPEFPUOP_ARITH2_64_64(name)                                       \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    TCGv_i64 t0, t1;                                                          \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    t0 = tcg_temp_new_i64();                                                  \
    t1 = tcg_temp_new_i64();                                                  \
    gen_load_gpr64(t0, rA(ctx->opcode));                                      \
    gen_load_gpr64(t1, rB(ctx->opcode));                                      \
    gen_helper_##name(t0, t0, t1);                                            \
    gen_store_gpr64(rD(ctx->opcode), t0);                                     \
    tcg_temp_free_i64(t0);                                                    \
    tcg_temp_free_i64(t1);                                                    \
}
#define GEN_SPEFPUOP_COMP_32(name)                                            \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    gen_helper_##name(cpu_crf[crfD(ctx->opcode)],                             \
                      cpu_gpr[rA(ctx->opcode)], cpu_gpr[rB(ctx->opcode)]);    \
}
#define GEN_SPEFPUOP_COMP_64(name)                                            \
static always_inline void gen_##name (DisasContext *ctx)                      \
{                                                                             \
    TCGv_i64 t0, t1;                                                          \
    if (unlikely(!ctx->spe_enabled)) {                                        \
        gen_exception(ctx, POWERPC_EXCP_APU);                                 \
        return;                                                               \
    }                                                                         \
    t0 = tcg_temp_new_i64();                                                  \
    t1 = tcg_temp_new_i64();                                                  \
    gen_load_gpr64(t0, rA(ctx->opcode));                                      \
    gen_load_gpr64(t1, rB(ctx->opcode));                                      \
    gen_helper_##name(cpu_crf[crfD(ctx->opcode)], t0, t1);                    \
    tcg_temp_free_i64(t0);                                                    \
    tcg_temp_free_i64(t1);                                                    \
}
#endif

/* Single precision floating-point vectors operations */
/* Arithmetic */
GEN_SPEFPUOP_ARITH2_64_64(evfsadd);
GEN_SPEFPUOP_ARITH2_64_64(evfssub);
GEN_SPEFPUOP_ARITH2_64_64(evfsmul);
GEN_SPEFPUOP_ARITH2_64_64(evfsdiv);
static always_inline void gen_evfsabs (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
#if defined(TARGET_PPC64)
    tcg_gen_andi_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], ~0x8000000080000000LL);
#else
    tcg_gen_andi_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], ~0x80000000);
    tcg_gen_andi_tl(cpu_gprh[rA(ctx->opcode)], cpu_gprh[rA(ctx->opcode)], ~0x80000000);
#endif
}
static always_inline void gen_evfsnabs (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
#if defined(TARGET_PPC64)
    tcg_gen_ori_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], 0x8000000080000000LL);
#else
    tcg_gen_ori_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], 0x80000000);
    tcg_gen_ori_tl(cpu_gprh[rA(ctx->opcode)], cpu_gprh[rA(ctx->opcode)], 0x80000000);
#endif
}
static always_inline void gen_evfsneg (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
#if defined(TARGET_PPC64)
    tcg_gen_xori_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], 0x8000000080000000LL);
#else
    tcg_gen_xori_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], 0x80000000);
    tcg_gen_xori_tl(cpu_gprh[rA(ctx->opcode)], cpu_gprh[rA(ctx->opcode)], 0x80000000);
#endif
}

/* Conversion */
GEN_SPEFPUOP_CONV_64_64(evfscfui);
GEN_SPEFPUOP_CONV_64_64(evfscfsi);
GEN_SPEFPUOP_CONV_64_64(evfscfuf);
GEN_SPEFPUOP_CONV_64_64(evfscfsf);
GEN_SPEFPUOP_CONV_64_64(evfsctui);
GEN_SPEFPUOP_CONV_64_64(evfsctsi);
GEN_SPEFPUOP_CONV_64_64(evfsctuf);
GEN_SPEFPUOP_CONV_64_64(evfsctsf);
GEN_SPEFPUOP_CONV_64_64(evfsctuiz);
GEN_SPEFPUOP_CONV_64_64(evfsctsiz);

/* Comparison */
GEN_SPEFPUOP_COMP_64(evfscmpgt);
GEN_SPEFPUOP_COMP_64(evfscmplt);
GEN_SPEFPUOP_COMP_64(evfscmpeq);
GEN_SPEFPUOP_COMP_64(evfststgt);
GEN_SPEFPUOP_COMP_64(evfststlt);
GEN_SPEFPUOP_COMP_64(evfststeq);

/* Opcodes definitions */
GEN_SPE(evfsadd,        evfssub,       0x00, 0x0A, 0x00000000, PPC_SPE_SINGLE); //
GEN_SPE(evfsabs,        evfsnabs,      0x02, 0x0A, 0x0000F800, PPC_SPE_SINGLE); //
GEN_SPE(evfsneg,        speundef,      0x03, 0x0A, 0x0000F800, PPC_SPE_SINGLE); //
GEN_SPE(evfsmul,        evfsdiv,       0x04, 0x0A, 0x00000000, PPC_SPE_SINGLE); //
GEN_SPE(evfscmpgt,      evfscmplt,     0x06, 0x0A, 0x00600000, PPC_SPE_SINGLE); //
GEN_SPE(evfscmpeq,      speundef,      0x07, 0x0A, 0x00600000, PPC_SPE_SINGLE); //
GEN_SPE(evfscfui,       evfscfsi,      0x08, 0x0A, 0x00180000, PPC_SPE_SINGLE); //
GEN_SPE(evfscfuf,       evfscfsf,      0x09, 0x0A, 0x00180000, PPC_SPE_SINGLE); //
GEN_SPE(evfsctui,       evfsctsi,      0x0A, 0x0A, 0x00180000, PPC_SPE_SINGLE); //
GEN_SPE(evfsctuf,       evfsctsf,      0x0B, 0x0A, 0x00180000, PPC_SPE_SINGLE); //
GEN_SPE(evfsctuiz,      speundef,      0x0C, 0x0A, 0x00180000, PPC_SPE_SINGLE); //
GEN_SPE(evfsctsiz,      speundef,      0x0D, 0x0A, 0x00180000, PPC_SPE_SINGLE); //
GEN_SPE(evfststgt,      evfststlt,     0x0E, 0x0A, 0x00600000, PPC_SPE_SINGLE); //
GEN_SPE(evfststeq,      speundef,      0x0F, 0x0A, 0x00600000, PPC_SPE_SINGLE); //

/* Single precision floating-point operations */
/* Arithmetic */
GEN_SPEFPUOP_ARITH2_32_32(efsadd);
GEN_SPEFPUOP_ARITH2_32_32(efssub);
GEN_SPEFPUOP_ARITH2_32_32(efsmul);
GEN_SPEFPUOP_ARITH2_32_32(efsdiv);
static always_inline void gen_efsabs (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
    tcg_gen_andi_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], (target_long)~0x80000000LL);
}
static always_inline void gen_efsnabs (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
    tcg_gen_ori_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], 0x80000000);
}
static always_inline void gen_efsneg (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
    tcg_gen_xori_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], 0x80000000);
}

/* Conversion */
GEN_SPEFPUOP_CONV_32_32(efscfui);
GEN_SPEFPUOP_CONV_32_32(efscfsi);
GEN_SPEFPUOP_CONV_32_32(efscfuf);
GEN_SPEFPUOP_CONV_32_32(efscfsf);
GEN_SPEFPUOP_CONV_32_32(efsctui);
GEN_SPEFPUOP_CONV_32_32(efsctsi);
GEN_SPEFPUOP_CONV_32_32(efsctuf);
GEN_SPEFPUOP_CONV_32_32(efsctsf);
GEN_SPEFPUOP_CONV_32_32(efsctuiz);
GEN_SPEFPUOP_CONV_32_32(efsctsiz);
GEN_SPEFPUOP_CONV_32_64(efscfd);

/* Comparison */
GEN_SPEFPUOP_COMP_32(efscmpgt);
GEN_SPEFPUOP_COMP_32(efscmplt);
GEN_SPEFPUOP_COMP_32(efscmpeq);
GEN_SPEFPUOP_COMP_32(efststgt);
GEN_SPEFPUOP_COMP_32(efststlt);
GEN_SPEFPUOP_COMP_32(efststeq);

/* Opcodes definitions */
GEN_SPE(efsadd,         efssub,        0x00, 0x0B, 0x00000000, PPC_SPE_SINGLE); //
GEN_SPE(efsabs,         efsnabs,       0x02, 0x0B, 0x0000F800, PPC_SPE_SINGLE); //
GEN_SPE(efsneg,         speundef,      0x03, 0x0B, 0x0000F800, PPC_SPE_SINGLE); //
GEN_SPE(efsmul,         efsdiv,        0x04, 0x0B, 0x00000000, PPC_SPE_SINGLE); //
GEN_SPE(efscmpgt,       efscmplt,      0x06, 0x0B, 0x00600000, PPC_SPE_SINGLE); //
GEN_SPE(efscmpeq,       efscfd,        0x07, 0x0B, 0x00600000, PPC_SPE_SINGLE); //
GEN_SPE(efscfui,        efscfsi,       0x08, 0x0B, 0x00180000, PPC_SPE_SINGLE); //
GEN_SPE(efscfuf,        efscfsf,       0x09, 0x0B, 0x00180000, PPC_SPE_SINGLE); //
GEN_SPE(efsctui,        efsctsi,       0x0A, 0x0B, 0x00180000, PPC_SPE_SINGLE); //
GEN_SPE(efsctuf,        efsctsf,       0x0B, 0x0B, 0x00180000, PPC_SPE_SINGLE); //
GEN_SPE(efsctuiz,       speundef,      0x0C, 0x0B, 0x00180000, PPC_SPE_SINGLE); //
GEN_SPE(efsctsiz,       speundef,      0x0D, 0x0B, 0x00180000, PPC_SPE_SINGLE); //
GEN_SPE(efststgt,       efststlt,      0x0E, 0x0B, 0x00600000, PPC_SPE_SINGLE); //
GEN_SPE(efststeq,       speundef,      0x0F, 0x0B, 0x00600000, PPC_SPE_SINGLE); //

/* Double precision floating-point operations */
/* Arithmetic */
GEN_SPEFPUOP_ARITH2_64_64(efdadd);
GEN_SPEFPUOP_ARITH2_64_64(efdsub);
GEN_SPEFPUOP_ARITH2_64_64(efdmul);
GEN_SPEFPUOP_ARITH2_64_64(efddiv);
static always_inline void gen_efdabs (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
#if defined(TARGET_PPC64)
    tcg_gen_andi_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], ~0x8000000000000000LL);
#else
    tcg_gen_andi_tl(cpu_gprh[rA(ctx->opcode)], cpu_gprh[rA(ctx->opcode)], ~0x80000000);
#endif
}
static always_inline void gen_efdnabs (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
#if defined(TARGET_PPC64)
    tcg_gen_ori_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], 0x8000000000000000LL);
#else
    tcg_gen_ori_tl(cpu_gprh[rA(ctx->opcode)], cpu_gprh[rA(ctx->opcode)], 0x80000000);
#endif
}
static always_inline void gen_efdneg (DisasContext *ctx)
{
    if (unlikely(!ctx->spe_enabled)) {
        gen_exception(ctx, POWERPC_EXCP_APU);
        return;
    }
#if defined(TARGET_PPC64)
    tcg_gen_xori_tl(cpu_gpr[rA(ctx->opcode)], cpu_gpr[rA(ctx->opcode)], 0x8000000000000000LL);
#else
    tcg_gen_xori_tl(cpu_gprh[rA(ctx->opcode)], cpu_gprh[rA(ctx->opcode)], 0x80000000);
#endif
}

/* Conversion */
GEN_SPEFPUOP_CONV_64_32(efdcfui);
GEN_SPEFPUOP_CONV_64_32(efdcfsi);
GEN_SPEFPUOP_CONV_64_32(efdcfuf);
GEN_SPEFPUOP_CONV_64_32(efdcfsf);
GEN_SPEFPUOP_CONV_32_64(efdctui);
GEN_SPEFPUOP_CONV_32_64(efdctsi);
GEN_SPEFPUOP_CONV_32_64(efdctuf);
GEN_SPEFPUOP_CONV_32_64(efdctsf);
GEN_SPEFPUOP_CONV_32_64(efdctuiz);
GEN_SPEFPUOP_CONV_32_64(efdctsiz);
GEN_SPEFPUOP_CONV_64_32(efdcfs);
GEN_SPEFPUOP_CONV_64_64(efdcfuid);
GEN_SPEFPUOP_CONV_64_64(efdcfsid);
GEN_SPEFPUOP_CONV_64_64(efdctuidz);
GEN_SPEFPUOP_CONV_64_64(efdctsidz);

/* Comparison */
GEN_SPEFPUOP_COMP_64(efdcmpgt);
GEN_SPEFPUOP_COMP_64(efdcmplt);
GEN_SPEFPUOP_COMP_64(efdcmpeq);
GEN_SPEFPUOP_COMP_64(efdtstgt);
GEN_SPEFPUOP_COMP_64(efdtstlt);
GEN_SPEFPUOP_COMP_64(efdtsteq);

/* Opcodes definitions */
GEN_SPE(efdadd,         efdsub,        0x10, 0x0B, 0x00000000, PPC_SPE_DOUBLE); //
GEN_SPE(efdcfuid,       efdcfsid,      0x11, 0x0B, 0x00180000, PPC_SPE_DOUBLE); //
GEN_SPE(efdabs,         efdnabs,       0x12, 0x0B, 0x0000F800, PPC_SPE_DOUBLE); //
GEN_SPE(efdneg,         speundef,      0x13, 0x0B, 0x0000F800, PPC_SPE_DOUBLE); //
GEN_SPE(efdmul,         efddiv,        0x14, 0x0B, 0x00000000, PPC_SPE_DOUBLE); //
GEN_SPE(efdctuidz,      efdctsidz,     0x15, 0x0B, 0x00180000, PPC_SPE_DOUBLE); //
GEN_SPE(efdcmpgt,       efdcmplt,      0x16, 0x0B, 0x00600000, PPC_SPE_DOUBLE); //
GEN_SPE(efdcmpeq,       efdcfs,        0x17, 0x0B, 0x00600000, PPC_SPE_DOUBLE); //
GEN_SPE(efdcfui,        efdcfsi,       0x18, 0x0B, 0x00180000, PPC_SPE_DOUBLE); //
GEN_SPE(efdcfuf,        efdcfsf,       0x19, 0x0B, 0x00180000, PPC_SPE_DOUBLE); //
GEN_SPE(efdctui,        efdctsi,       0x1A, 0x0B, 0x00180000, PPC_SPE_DOUBLE); //
GEN_SPE(efdctuf,        efdctsf,       0x1B, 0x0B, 0x00180000, PPC_SPE_DOUBLE); //
GEN_SPE(efdctuiz,       speundef,      0x1C, 0x0B, 0x00180000, PPC_SPE_DOUBLE); //
GEN_SPE(efdctsiz,       speundef,      0x1D, 0x0B, 0x00180000, PPC_SPE_DOUBLE); //
GEN_SPE(efdtstgt,       efdtstlt,      0x1E, 0x0B, 0x00600000, PPC_SPE_DOUBLE); //
GEN_SPE(efdtsteq,       speundef,      0x1F, 0x0B, 0x00600000, PPC_SPE_DOUBLE); //

/* End opcode list */
GEN_OPCODE_MARK(end);

#include "translate_init.c"
#include "helper_regs.h"

/*****************************************************************************/
/* Misc PowerPC helpers */
void cpu_dump_state (CPUState *env, FILE *f,
                     int (*cpu_fprintf)(FILE *f, const char *fmt, ...),
                     int flags)
{
#define RGPL  4
#define RFPL  4

    int i;

    cpu_fprintf(f, "NIP " ADDRX "   LR " ADDRX " CTR " ADDRX " XER %08x\n",
                env->nip, env->lr, env->ctr, env->xer);
    cpu_fprintf(f, "MSR " ADDRX " HID0 " ADDRX "  HF " ADDRX " idx %d\n",
                env->msr, env->spr[SPR_HID0], env->hflags, env->mmu_idx);
#if !defined(NO_TIMER_DUMP)
    cpu_fprintf(f, "TB %08x %08x "
#if !defined(CONFIG_USER_ONLY)
                "DECR %08x"
#endif
                "\n",
                cpu_ppc_load_tbu(env), cpu_ppc_load_tbl(env)
#if !defined(CONFIG_USER_ONLY)
                , cpu_ppc_load_decr(env)
#endif
                );
#endif
    for (i = 0; i < 32; i++) {
        if ((i & (RGPL - 1)) == 0)
            cpu_fprintf(f, "GPR%02d", i);
        cpu_fprintf(f, " " REGX, ppc_dump_gpr(env, i));
        if ((i & (RGPL - 1)) == (RGPL - 1))
            cpu_fprintf(f, "\n");
    }
    cpu_fprintf(f, "CR ");
    for (i = 0; i < 8; i++)
        cpu_fprintf(f, "%01x", env->crf[i]);
    cpu_fprintf(f, "  [");
    for (i = 0; i < 8; i++) {
        char a = '-';
        if (env->crf[i] & 0x08)
            a = 'L';
        else if (env->crf[i] & 0x04)
            a = 'G';
        else if (env->crf[i] & 0x02)
            a = 'E';
        cpu_fprintf(f, " %c%c", a, env->crf[i] & 0x01 ? 'O' : ' ');
    }
    cpu_fprintf(f, " ]             RES " ADDRX "\n", env->reserve);
    for (i = 0; i < 32; i++) {
        if ((i & (RFPL - 1)) == 0)
            cpu_fprintf(f, "FPR%02d", i);
        cpu_fprintf(f, " %016" PRIx64, *((uint64_t *)&env->fpr[i]));
        if ((i & (RFPL - 1)) == (RFPL - 1))
            cpu_fprintf(f, "\n");
    }
    cpu_fprintf(f, "FPSCR %08x\n", env->fpscr);
#if !defined(CONFIG_USER_ONLY)
    cpu_fprintf(f, "SRR0 " ADDRX " SRR1 " ADDRX " SDR1 " ADDRX "\n",
                env->spr[SPR_SRR0], env->spr[SPR_SRR1], env->sdr1);
#endif

#undef RGPL
#undef RFPL
}

void cpu_dump_statistics (CPUState *env, FILE*f,
                          int (*cpu_fprintf)(FILE *f, const char *fmt, ...),
                          int flags)
{
#if defined(DO_PPC_STATISTICS)
    opc_handler_t **t1, **t2, **t3, *handler;
    int op1, op2, op3;

    t1 = env->opcodes;
    for (op1 = 0; op1 < 64; op1++) {
        handler = t1[op1];
        if (is_indirect_opcode(handler)) {
            t2 = ind_table(handler);
            for (op2 = 0; op2 < 32; op2++) {
                handler = t2[op2];
                if (is_indirect_opcode(handler)) {
                    t3 = ind_table(handler);
                    for (op3 = 0; op3 < 32; op3++) {
                        handler = t3[op3];
                        if (handler->count == 0)
                            continue;
                        cpu_fprintf(f, "%02x %02x %02x (%02x %04d) %16s: "
                                    "%016llx %lld\n",
                                    op1, op2, op3, op1, (op3 << 5) | op2,
                                    handler->oname,
                                    handler->count, handler->count);
                    }
                } else {
                    if (handler->count == 0)
                        continue;
                    cpu_fprintf(f, "%02x %02x    (%02x %04d) %16s: "
                                "%016llx %lld\n",
                                op1, op2, op1, op2, handler->oname,
                                handler->count, handler->count);
                }
            }
        } else {
            if (handler->count == 0)
                continue;
            cpu_fprintf(f, "%02x       (%02x     ) %16s: %016llx %lld\n",
                        op1, op1, handler->oname,
                        handler->count, handler->count);
        }
    }
#endif
}

/*****************************************************************************/
static always_inline void gen_intermediate_code_internal (CPUState *env,
                                                          TranslationBlock *tb,
                                                          int search_pc)
{
    DisasContext ctx, *ctxp = &ctx;
    opc_handler_t **table, *handler;
    target_ulong pc_start;
    uint16_t *gen_opc_end;
    CPUBreakpoint *bp;
    int j, lj = -1;
    int num_insns;
    int max_insns;

    pc_start = tb->pc;
    gen_opc_end = gen_opc_buf + OPC_MAX_SIZE;
    ctx.nip = pc_start;
    ctx.tb = tb;
    ctx.exception = POWERPC_EXCP_NONE;
    ctx.spr_cb = env->spr_cb;
    ctx.mem_idx = env->mmu_idx;
    ctx.access_type = -1;
    ctx.le_mode = env->hflags & (1 << MSR_LE) ? 1 : 0;
#if defined(TARGET_PPC64)
    ctx.sf_mode = msr_sf;
#endif
    ctx.fpu_enabled = msr_fp;
    if ((env->flags & POWERPC_FLAG_SPE) && msr_spe)
        ctx.spe_enabled = msr_spe;
    else
        ctx.spe_enabled = 0;
    if ((env->flags & POWERPC_FLAG_VRE) && msr_vr)
        ctx.altivec_enabled = msr_vr;
    else
        ctx.altivec_enabled = 0;
    if ((env->flags & POWERPC_FLAG_SE) && msr_se)
        ctx.singlestep_enabled = CPU_SINGLE_STEP;
    else
        ctx.singlestep_enabled = 0;
    if ((env->flags & POWERPC_FLAG_BE) && msr_be)
        ctx.singlestep_enabled |= CPU_BRANCH_STEP;
    if (unlikely(env->singlestep_enabled))
        ctx.singlestep_enabled |= GDBSTUB_SINGLE_STEP;
#if defined (DO_SINGLE_STEP) && 0
    /* Single step trace mode */
    msr_se = 1;
#endif
    num_insns = 0;
    max_insns = tb->cflags & CF_COUNT_MASK;
    if (max_insns == 0)
        max_insns = CF_COUNT_MASK;

    gen_icount_start();
    /* Set env in case of segfault during code fetch */
    while (ctx.exception == POWERPC_EXCP_NONE && gen_opc_ptr < gen_opc_end) {
        if (unlikely(!TAILQ_EMPTY(&env->breakpoints))) {
            TAILQ_FOREACH(bp, &env->breakpoints, entry) {
                if (bp->pc == ctx.nip) {
                    gen_debug_exception(ctxp);
                    break;
                }
            }
        }
        if (unlikely(search_pc)) {
            j = gen_opc_ptr - gen_opc_buf;
            if (lj < j) {
                lj++;
                while (lj < j)
                    gen_opc_instr_start[lj++] = 0;
                gen_opc_pc[lj] = ctx.nip;
                gen_opc_instr_start[lj] = 1;
                gen_opc_icount[lj] = num_insns;
            }
        }
        LOG_DISAS("----------------\n");
        LOG_DISAS("nip=" ADDRX " super=%d ir=%d\n",
                  ctx.nip, ctx.mem_idx, (int)msr_ir);
        if (num_insns + 1 == max_insns && (tb->cflags & CF_LAST_IO))
            gen_io_start();
        if (unlikely(ctx.le_mode)) {
            ctx.opcode = bswap32(ldl_code(ctx.nip));
        } else {
            ctx.opcode = ldl_code(ctx.nip);
        }
        LOG_DISAS("translate opcode %08x (%02x %02x %02x) (%s)\n",
                    ctx.opcode, opc1(ctx.opcode), opc2(ctx.opcode),
                    opc3(ctx.opcode), little_endian ? "little" : "big");
        ctx.nip += 4;
        table = env->opcodes;
        num_insns++;
        handler = table[opc1(ctx.opcode)];
        if (is_indirect_opcode(handler)) {
            table = ind_table(handler);
            handler = table[opc2(ctx.opcode)];
            if (is_indirect_opcode(handler)) {
                table = ind_table(handler);
                handler = table[opc3(ctx.opcode)];
            }
        }
        /* Is opcode *REALLY* valid ? */
        if (unlikely(handler->handler == &gen_invalid)) {
            if (qemu_log_enabled()) {
                qemu_log("invalid/unsupported opcode: "
                          "%02x - %02x - %02x (%08x) " ADDRX " %d\n",
                          opc1(ctx.opcode), opc2(ctx.opcode),
                          opc3(ctx.opcode), ctx.opcode, ctx.nip - 4, (int)msr_ir);
            } else {
                printf("invalid/unsupported opcode: "
                       "%02x - %02x - %02x (%08x) " ADDRX " %d\n",
                       opc1(ctx.opcode), opc2(ctx.opcode),
                       opc3(ctx.opcode), ctx.opcode, ctx.nip - 4, (int)msr_ir);
            }
        } else {
            if (unlikely((ctx.opcode & handler->inval) != 0)) {
                if (qemu_log_enabled()) {
                    qemu_log("invalid bits: %08x for opcode: "
                              "%02x - %02x - %02x (%08x) " ADDRX "\n",
                              ctx.opcode & handler->inval, opc1(ctx.opcode),
                              opc2(ctx.opcode), opc3(ctx.opcode),
                              ctx.opcode, ctx.nip - 4);
                } else {
                    printf("invalid bits: %08x for opcode: "
                           "%02x - %02x - %02x (%08x) " ADDRX "\n",
                           ctx.opcode & handler->inval, opc1(ctx.opcode),
                           opc2(ctx.opcode), opc3(ctx.opcode),
                           ctx.opcode, ctx.nip - 4);
                }
                gen_inval_exception(ctxp, POWERPC_EXCP_INVAL_INVAL);
                break;
            }
        }
        (*(handler->handler))(&ctx);
#if defined(DO_PPC_STATISTICS)
        handler->count++;
#endif
        /* Check trace mode exceptions */
        if (unlikely(ctx.singlestep_enabled & CPU_SINGLE_STEP &&
                     (ctx.nip <= 0x100 || ctx.nip > 0xF00) &&
                     ctx.exception != POWERPC_SYSCALL &&
                     ctx.exception != POWERPC_EXCP_TRAP &&
                     ctx.exception != POWERPC_EXCP_BRANCH)) {
            gen_exception(ctxp, POWERPC_EXCP_TRACE);
        } else if (unlikely(((ctx.nip & (TARGET_PAGE_SIZE - 1)) == 0) ||
                            (env->singlestep_enabled) ||
                            num_insns >= max_insns)) {
            /* if we reach a page boundary or are single stepping, stop
             * generation
             */
            break;
        }
#if defined (DO_SINGLE_STEP)
        break;
#endif
    }
    if (tb->cflags & CF_LAST_IO)
        gen_io_end();
    if (ctx.exception == POWERPC_EXCP_NONE) {
        gen_goto_tb(&ctx, 0, ctx.nip);
    } else if (ctx.exception != POWERPC_EXCP_BRANCH) {
        if (unlikely(env->singlestep_enabled)) {
            gen_debug_exception(ctxp);
        }
        /* Generate the return instruction */
        tcg_gen_exit_tb(0);
    }
    gen_icount_end(tb, num_insns);
    *gen_opc_ptr = INDEX_op_end;
    if (unlikely(search_pc)) {
        j = gen_opc_ptr - gen_opc_buf;
        lj++;
        while (lj <= j)
            gen_opc_instr_start[lj++] = 0;
    } else {
        tb->size = ctx.nip - pc_start;
        tb->icount = num_insns;
    }
#if defined(DEBUG_DISAS)
    qemu_log_mask(CPU_LOG_TB_CPU, "---------------- excp: %04x\n", ctx.exception);
    log_cpu_state_mask(CPU_LOG_TB_CPU, env, 0);
    if (qemu_loglevel_mask(CPU_LOG_TB_IN_ASM)) {
        int flags;
        flags = env->bfd_mach;
        flags |= ctx.le_mode << 16;
        qemu_log("IN: %s\n", lookup_symbol(pc_start));
        log_target_disas(pc_start, ctx.nip - pc_start, flags);
        qemu_log("\n");
    }
#endif
}

void gen_intermediate_code (CPUState *env, struct TranslationBlock *tb)
{
    gen_intermediate_code_internal(env, tb, 0);
}

void gen_intermediate_code_pc (CPUState *env, struct TranslationBlock *tb)
{
    gen_intermediate_code_internal(env, tb, 1);
}

void gen_pc_load(CPUState *env, TranslationBlock *tb,
                unsigned long searched_pc, int pc_pos, void *puc)
{
    env->nip = gen_opc_pc[pc_pos];
}
