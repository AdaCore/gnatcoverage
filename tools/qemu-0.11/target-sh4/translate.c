/*
 *  SH4 translation
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
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#define DEBUG_DISAS
#define SH4_DEBUG_DISAS
//#define SH4_SINGLE_STEP

#include "cpu.h"
#include "exec-all.h"
#include "disas.h"
#include "tcg-op.h"
#include "qemu-common.h"

#include "helper.h"
#define GEN_HELPER 1
#include "helper.h"

typedef struct DisasContext {
    struct TranslationBlock *tb;
    target_ulong pc;
    uint32_t sr;
    uint32_t fpscr;
    uint16_t opcode;
    uint32_t flags;
    int bstate;
    int memidx;
    uint32_t delayed_pc;
    int singlestep_enabled;
    uint32_t features;
    int has_movcal;
} DisasContext;

#if defined(CONFIG_USER_ONLY)
#define IS_USER(ctx) 1
#else
#define IS_USER(ctx) (!(ctx->sr & SR_MD))
#endif

enum {
    BS_NONE     = 0, /* We go out of the TB without reaching a branch or an
                      * exception condition
                      */
    BS_STOP     = 1, /* We want to stop translation for any reason */
    BS_BRANCH   = 2, /* We reached a branch condition     */
    BS_EXCP     = 3, /* We reached an exception condition */
};

/* global register indexes */
static TCGv_ptr cpu_env;
static TCGv cpu_gregs[24];
static TCGv cpu_pc, cpu_sr, cpu_ssr, cpu_spc, cpu_gbr;
static TCGv cpu_vbr, cpu_sgr, cpu_dbr, cpu_mach, cpu_macl;
static TCGv cpu_pr, cpu_fpscr, cpu_fpul, cpu_ldst;
static TCGv cpu_fregs[32];

/* internal register indexes */
static TCGv cpu_flags, cpu_delayed_pc;

#include "gen-icount.h"

static void sh4_translate_init(void)
{
    int i;
    static int done_init = 0;
    static const char * const gregnames[24] = {
        "R0_BANK0", "R1_BANK0", "R2_BANK0", "R3_BANK0",
        "R4_BANK0", "R5_BANK0", "R6_BANK0", "R7_BANK0",
        "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15",
        "R0_BANK1", "R1_BANK1", "R2_BANK1", "R3_BANK1",
        "R4_BANK1", "R5_BANK1", "R6_BANK1", "R7_BANK1"
    };
    static const char * const fregnames[32] = {
         "FPR0_BANK0",  "FPR1_BANK0",  "FPR2_BANK0",  "FPR3_BANK0",
         "FPR4_BANK0",  "FPR5_BANK0",  "FPR6_BANK0",  "FPR7_BANK0",
         "FPR8_BANK0",  "FPR9_BANK0", "FPR10_BANK0", "FPR11_BANK0",
        "FPR12_BANK0", "FPR13_BANK0", "FPR14_BANK0", "FPR15_BANK0",
         "FPR0_BANK1",  "FPR1_BANK1",  "FPR2_BANK1",  "FPR3_BANK1",
         "FPR4_BANK1",  "FPR5_BANK1",  "FPR6_BANK1",  "FPR7_BANK1",
         "FPR8_BANK1",  "FPR9_BANK1", "FPR10_BANK1", "FPR11_BANK1",
        "FPR12_BANK1", "FPR13_BANK1", "FPR14_BANK1", "FPR15_BANK1",
    };

    if (done_init)
        return;

    cpu_env = tcg_global_reg_new_ptr(TCG_AREG0, "env");

    for (i = 0; i < 24; i++)
        cpu_gregs[i] = tcg_global_mem_new_i32(TCG_AREG0,
                                              offsetof(CPUState, gregs[i]),
                                              gregnames[i]);

    cpu_pc = tcg_global_mem_new_i32(TCG_AREG0,
                                    offsetof(CPUState, pc), "PC");
    cpu_sr = tcg_global_mem_new_i32(TCG_AREG0,
                                    offsetof(CPUState, sr), "SR");
    cpu_ssr = tcg_global_mem_new_i32(TCG_AREG0,
                                     offsetof(CPUState, ssr), "SSR");
    cpu_spc = tcg_global_mem_new_i32(TCG_AREG0,
                                     offsetof(CPUState, spc), "SPC");
    cpu_gbr = tcg_global_mem_new_i32(TCG_AREG0,
                                     offsetof(CPUState, gbr), "GBR");
    cpu_vbr = tcg_global_mem_new_i32(TCG_AREG0,
                                     offsetof(CPUState, vbr), "VBR");
    cpu_sgr = tcg_global_mem_new_i32(TCG_AREG0,
                                     offsetof(CPUState, sgr), "SGR");
    cpu_dbr = tcg_global_mem_new_i32(TCG_AREG0,
                                     offsetof(CPUState, dbr), "DBR");
    cpu_mach = tcg_global_mem_new_i32(TCG_AREG0,
                                      offsetof(CPUState, mach), "MACH");
    cpu_macl = tcg_global_mem_new_i32(TCG_AREG0,
                                      offsetof(CPUState, macl), "MACL");
    cpu_pr = tcg_global_mem_new_i32(TCG_AREG0,
                                    offsetof(CPUState, pr), "PR");
    cpu_fpscr = tcg_global_mem_new_i32(TCG_AREG0,
                                       offsetof(CPUState, fpscr), "FPSCR");
    cpu_fpul = tcg_global_mem_new_i32(TCG_AREG0,
                                      offsetof(CPUState, fpul), "FPUL");

    cpu_flags = tcg_global_mem_new_i32(TCG_AREG0,
				       offsetof(CPUState, flags), "_flags_");
    cpu_delayed_pc = tcg_global_mem_new_i32(TCG_AREG0,
					    offsetof(CPUState, delayed_pc),
					    "_delayed_pc_");
    cpu_ldst = tcg_global_mem_new_i32(TCG_AREG0,
				      offsetof(CPUState, ldst), "_ldst_");

    for (i = 0; i < 32; i++)
        cpu_fregs[i] = tcg_global_mem_new_i32(TCG_AREG0,
                                              offsetof(CPUState, fregs[i]),
                                              fregnames[i]);

    /* register helpers */
#define GEN_HELPER 2
#include "helper.h"

    done_init = 1;
}

void cpu_dump_state(CPUState * env, FILE * f,
		    int (*cpu_fprintf) (FILE * f, const char *fmt, ...),
		    int flags)
{
    int i;
    cpu_fprintf(f, "pc=0x%08x sr=0x%08x pr=0x%08x fpscr=0x%08x\n",
		env->pc, env->sr, env->pr, env->fpscr);
    cpu_fprintf(f, "spc=0x%08x ssr=0x%08x gbr=0x%08x vbr=0x%08x\n",
		env->spc, env->ssr, env->gbr, env->vbr);
    cpu_fprintf(f, "sgr=0x%08x dbr=0x%08x delayed_pc=0x%08x fpul=0x%08x\n",
		env->sgr, env->dbr, env->delayed_pc, env->fpul);
    for (i = 0; i < 24; i += 4) {
	cpu_fprintf(f, "r%d=0x%08x r%d=0x%08x r%d=0x%08x r%d=0x%08x\n",
		    i, env->gregs[i], i + 1, env->gregs[i + 1],
		    i + 2, env->gregs[i + 2], i + 3, env->gregs[i + 3]);
    }
    if (env->flags & DELAY_SLOT) {
	cpu_fprintf(f, "in delay slot (delayed_pc=0x%08x)\n",
		    env->delayed_pc);
    } else if (env->flags & DELAY_SLOT_CONDITIONAL) {
	cpu_fprintf(f, "in conditional delay slot (delayed_pc=0x%08x)\n",
		    env->delayed_pc);
    }
}

static void cpu_sh4_reset(CPUSH4State * env)
{
    if (qemu_loglevel_mask(CPU_LOG_RESET)) {
        qemu_log("CPU Reset (CPU %d)\n", env->cpu_index);
        log_cpu_state(env, 0);
    }

#if defined(CONFIG_USER_ONLY)
    env->sr = 0;
#else
    env->sr = SR_MD | SR_RB | SR_BL | SR_I3 | SR_I2 | SR_I1 | SR_I0;
#endif
    env->vbr = 0;
    env->pc = 0xA0000000;
#if defined(CONFIG_USER_ONLY)
    env->fpscr = FPSCR_PR; /* value for userspace according to the kernel */
    set_float_rounding_mode(float_round_nearest_even, &env->fp_status); /* ?! */
#else
    env->fpscr = 0x00040001; /* CPU reset value according to SH4 manual */
    set_float_rounding_mode(float_round_to_zero, &env->fp_status);
#endif
    env->mmucr = 0;
}

typedef struct {
    const char *name;
    int id;
    uint32_t pvr;
    uint32_t prr;
    uint32_t cvr;
    uint32_t features;
} sh4_def_t;

static sh4_def_t sh4_defs[] = {
    {
	.name = "SH7750R",
	.id = SH_CPU_SH7750R,
	.pvr = 0x00050000,
	.prr = 0x00000100,
	.cvr = 0x00110000,
	.features = SH_FEATURE_BCR3_AND_BCR4,
    }, {
	.name = "SH7751R",
	.id = SH_CPU_SH7751R,
	.pvr = 0x04050005,
	.prr = 0x00000113,
	.cvr = 0x00110000,	/* Neutered caches, should be 0x20480000 */
	.features = SH_FEATURE_BCR3_AND_BCR4,
    }, {
	.name = "SH7785",
	.id = SH_CPU_SH7785,
	.pvr = 0x10300700,
	.prr = 0x00000200,
	.cvr = 0x71440211,
	.features = SH_FEATURE_SH4A,
     },
};

static const sh4_def_t *cpu_sh4_find_by_name(const char *name)
{
    int i;

    if (strcasecmp(name, "any") == 0)
	return &sh4_defs[0];

    for (i = 0; i < ARRAY_SIZE(sh4_defs); i++)
	if (strcasecmp(name, sh4_defs[i].name) == 0)
	    return &sh4_defs[i];

    return NULL;
}

void sh4_cpu_list(FILE *f, int (*cpu_fprintf)(FILE *f, const char *fmt, ...))
{
    int i;

    for (i = 0; i < ARRAY_SIZE(sh4_defs); i++)
	(*cpu_fprintf)(f, "%s\n", sh4_defs[i].name);
}

static void cpu_sh4_register(CPUSH4State *env, const sh4_def_t *def)
{
    env->pvr = def->pvr;
    env->prr = def->prr;
    env->cvr = def->cvr;
    env->id = def->id;
}

CPUSH4State *cpu_sh4_init(const char *cpu_model)
{
    CPUSH4State *env;
    const sh4_def_t *def;

    def = cpu_sh4_find_by_name(cpu_model);
    if (!def)
	return NULL;
    env = qemu_mallocz(sizeof(CPUSH4State));
    env->features = def->features;
    cpu_exec_init(env);
    env->movcal_backup_tail = &(env->movcal_backup);
    sh4_translate_init();
    env->cpu_model_str = cpu_model;
    cpu_sh4_reset(env);
    cpu_sh4_register(env, def);
    tlb_flush(env, 1);
    qemu_init_vcpu(env);
    return env;
}

static void gen_goto_tb(DisasContext * ctx, int n, target_ulong dest)
{
    TranslationBlock *tb;
    tb = ctx->tb;

    if ((tb->pc & TARGET_PAGE_MASK) == (dest & TARGET_PAGE_MASK) &&
	!ctx->singlestep_enabled) {
	/* Use a direct jump if in same page and singlestep not enabled */
        tcg_gen_goto_tb(n);
        tcg_gen_movi_i32(cpu_pc, dest);
        tcg_gen_exit_tb((long) tb + n);
    } else {
        tcg_gen_movi_i32(cpu_pc, dest);
        if (ctx->singlestep_enabled)
            gen_helper_debug();
        tcg_gen_exit_tb(0);
    }
}

static void gen_jump(DisasContext * ctx)
{
    if (ctx->delayed_pc == (uint32_t) - 1) {
	/* Target is not statically known, it comes necessarily from a
	   delayed jump as immediate jump are conditinal jumps */
	tcg_gen_mov_i32(cpu_pc, cpu_delayed_pc);
	if (ctx->singlestep_enabled)
	    gen_helper_debug();
	tcg_gen_exit_tb(0);
    } else {
	gen_goto_tb(ctx, 0, ctx->delayed_pc);
    }
}

static inline void gen_branch_slot(uint32_t delayed_pc, int t)
{
    TCGv sr;
    int label = gen_new_label();
    tcg_gen_movi_i32(cpu_delayed_pc, delayed_pc);
    sr = tcg_temp_new();
    tcg_gen_andi_i32(sr, cpu_sr, SR_T);
    tcg_gen_brcondi_i32(TCG_COND_NE, sr, t ? SR_T : 0, label);
    tcg_gen_ori_i32(cpu_flags, cpu_flags, DELAY_SLOT_TRUE);
    gen_set_label(label);
}

/* Immediate conditional jump (bt or bf) */
static void gen_conditional_jump(DisasContext * ctx,
				 target_ulong ift, target_ulong ifnott)
{
    int l1;
    TCGv sr;

    l1 = gen_new_label();
    sr = tcg_temp_new();
    tcg_gen_andi_i32(sr, cpu_sr, SR_T);
    tcg_gen_brcondi_i32(TCG_COND_EQ, sr, SR_T, l1);
    gen_goto_tb(ctx, 0, ifnott);
    gen_set_label(l1);
    gen_goto_tb(ctx, 1, ift);
}

/* Delayed conditional jump (bt or bf) */
static void gen_delayed_conditional_jump(DisasContext * ctx)
{
    int l1;
    TCGv ds;

    l1 = gen_new_label();
    ds = tcg_temp_new();
    tcg_gen_andi_i32(ds, cpu_flags, DELAY_SLOT_TRUE);
    tcg_gen_brcondi_i32(TCG_COND_EQ, ds, DELAY_SLOT_TRUE, l1);
    gen_goto_tb(ctx, 1, ctx->pc + 2);
    gen_set_label(l1);
    tcg_gen_andi_i32(cpu_flags, cpu_flags, ~DELAY_SLOT_TRUE);
    gen_jump(ctx);
}

static inline void gen_set_t(void)
{
    tcg_gen_ori_i32(cpu_sr, cpu_sr, SR_T);
}

static inline void gen_clr_t(void)
{
    tcg_gen_andi_i32(cpu_sr, cpu_sr, ~SR_T);
}

static inline void gen_cmp(int cond, TCGv t0, TCGv t1)
{
    int label1 = gen_new_label();
    int label2 = gen_new_label();
    tcg_gen_brcond_i32(cond, t1, t0, label1);
    gen_clr_t();
    tcg_gen_br(label2);
    gen_set_label(label1);
    gen_set_t();
    gen_set_label(label2);
}

static inline void gen_cmp_imm(int cond, TCGv t0, int32_t imm)
{
    int label1 = gen_new_label();
    int label2 = gen_new_label();
    tcg_gen_brcondi_i32(cond, t0, imm, label1);
    gen_clr_t();
    tcg_gen_br(label2);
    gen_set_label(label1);
    gen_set_t();
    gen_set_label(label2);
}

static inline void gen_store_flags(uint32_t flags)
{
    tcg_gen_andi_i32(cpu_flags, cpu_flags, DELAY_SLOT_TRUE);
    tcg_gen_ori_i32(cpu_flags, cpu_flags, flags);
}

static inline void gen_copy_bit_i32(TCGv t0, int p0, TCGv t1, int p1)
{
    TCGv tmp = tcg_temp_new();

    p0 &= 0x1f;
    p1 &= 0x1f;

    tcg_gen_andi_i32(tmp, t1, (1 << p1));
    tcg_gen_andi_i32(t0, t0, ~(1 << p0));
    if (p0 < p1)
        tcg_gen_shri_i32(tmp, tmp, p1 - p0);
    else if (p0 > p1)
        tcg_gen_shli_i32(tmp, tmp, p0 - p1);
    tcg_gen_or_i32(t0, t0, tmp);

    tcg_temp_free(tmp);
}

static inline void gen_load_fpr64(TCGv_i64 t, int reg)
{
    tcg_gen_concat_i32_i64(t, cpu_fregs[reg + 1], cpu_fregs[reg]);
}

static inline void gen_store_fpr64 (TCGv_i64 t, int reg)
{
    TCGv_i32 tmp = tcg_temp_new_i32();
    tcg_gen_trunc_i64_i32(tmp, t);
    tcg_gen_mov_i32(cpu_fregs[reg + 1], tmp);
    tcg_gen_shri_i64(t, t, 32);
    tcg_gen_trunc_i64_i32(tmp, t);
    tcg_gen_mov_i32(cpu_fregs[reg], tmp);
    tcg_temp_free_i32(tmp);
}

#define B3_0 (ctx->opcode & 0xf)
#define B6_4 ((ctx->opcode >> 4) & 0x7)
#define B7_4 ((ctx->opcode >> 4) & 0xf)
#define B7_0 (ctx->opcode & 0xff)
#define B7_0s ((int32_t) (int8_t) (ctx->opcode & 0xff))
#define B11_0s (ctx->opcode & 0x800 ? 0xfffff000 | (ctx->opcode & 0xfff) : \
  (ctx->opcode & 0xfff))
#define B11_8 ((ctx->opcode >> 8) & 0xf)
#define B15_12 ((ctx->opcode >> 12) & 0xf)

#define REG(x) ((x) < 8 && (ctx->sr & (SR_MD | SR_RB)) == (SR_MD | SR_RB) ? \
		(cpu_gregs[x + 16]) : (cpu_gregs[x]))

#define ALTREG(x) ((x) < 8 && (ctx->sr & (SR_MD | SR_RB)) != (SR_MD | SR_RB) \
		? (cpu_gregs[x + 16]) : (cpu_gregs[x]))

#define FREG(x) (ctx->fpscr & FPSCR_FR ? (x) ^ 0x10 : (x))
#define XHACK(x) ((((x) & 1 ) << 4) | ((x) & 0xe))
#define XREG(x) (ctx->fpscr & FPSCR_FR ? XHACK(x) ^ 0x10 : XHACK(x))
#define DREG(x) FREG(x) /* Assumes lsb of (x) is always 0 */

#define CHECK_NOT_DELAY_SLOT \
  if (ctx->flags & (DELAY_SLOT | DELAY_SLOT_CONDITIONAL))     \
  {                                                           \
      tcg_gen_movi_i32(cpu_pc, ctx->pc-2);                    \
      gen_helper_raise_slot_illegal_instruction();            \
      ctx->bstate = BS_EXCP;                                  \
      return;                                                 \
  }

#define CHECK_PRIVILEGED                                      \
  if (IS_USER(ctx)) {                                         \
      tcg_gen_movi_i32(cpu_pc, ctx->pc);                      \
      gen_helper_raise_illegal_instruction();                 \
      ctx->bstate = BS_EXCP;                                  \
      return;                                                 \
  }

#define CHECK_FPU_ENABLED                                       \
  if (ctx->flags & SR_FD) {                                     \
      if (ctx->flags & (DELAY_SLOT | DELAY_SLOT_CONDITIONAL)) { \
          tcg_gen_movi_i32(cpu_pc, ctx->pc-2);                  \
          gen_helper_raise_slot_fpu_disable();                  \
      } else {                                                  \
          tcg_gen_movi_i32(cpu_pc, ctx->pc);                    \
          gen_helper_raise_fpu_disable();                       \
      }                                                         \
      ctx->bstate = BS_EXCP;                                    \
      return;                                                   \
  }

static void _decode_opc(DisasContext * ctx)
{
    /* This code tries to make movcal emulation sufficiently
       accurate for Linux purposes.  This instruction writes
       memory, and prior to that, always allocates a cache line.
       It is used in two contexts:
       - in memcpy, where data is copied in blocks, the first write
       of to a block uses movca.l for performance.
       - in arch/sh/mm/cache-sh4.c, movcal.l + ocbi combination is used
       to flush the cache. Here, the data written by movcal.l is never
       written to memory, and the data written is just bogus.

       To simulate this, we simulate movcal.l, we store the value to memory,
       but we also remember the previous content. If we see ocbi, we check
       if movcal.l for that address was done previously. If so, the write should
       not have hit the memory, so we restore the previous content.
       When we see an instruction that is neither movca.l
       nor ocbi, the previous content is discarded.

       To optimize, we only try to flush stores when we're at the start of
       TB, or if we already saw movca.l in this TB and did not flush stores
       yet.  */
    if (ctx->has_movcal)
	{
	  int opcode = ctx->opcode & 0xf0ff;
	  if (opcode != 0x0093 /* ocbi */
	      && opcode != 0x00c3 /* movca.l */)
	      {
		  gen_helper_discard_movcal_backup ();
		  ctx->has_movcal = 0;
	      }
	}

#if 0
    fprintf(stderr, "Translating opcode 0x%04x\n", ctx->opcode);
#endif

    switch (ctx->opcode) {
    case 0x0019:		/* div0u */
	tcg_gen_andi_i32(cpu_sr, cpu_sr, ~(SR_M | SR_Q | SR_T));
	return;
    case 0x000b:		/* rts */
	CHECK_NOT_DELAY_SLOT
	tcg_gen_mov_i32(cpu_delayed_pc, cpu_pr);
	ctx->flags |= DELAY_SLOT;
	ctx->delayed_pc = (uint32_t) - 1;
	return;
    case 0x0028:		/* clrmac */
	tcg_gen_movi_i32(cpu_mach, 0);
	tcg_gen_movi_i32(cpu_macl, 0);
	return;
    case 0x0048:		/* clrs */
	tcg_gen_andi_i32(cpu_sr, cpu_sr, ~SR_S);
	return;
    case 0x0008:		/* clrt */
	gen_clr_t();
	return;
    case 0x0038:		/* ldtlb */
	CHECK_PRIVILEGED
	gen_helper_ldtlb();
	return;
    case 0x002b:		/* rte */
	CHECK_PRIVILEGED
	CHECK_NOT_DELAY_SLOT
	tcg_gen_mov_i32(cpu_sr, cpu_ssr);
	tcg_gen_mov_i32(cpu_delayed_pc, cpu_spc);
	ctx->flags |= DELAY_SLOT;
	ctx->delayed_pc = (uint32_t) - 1;
	return;
    case 0x0058:		/* sets */
	tcg_gen_ori_i32(cpu_sr, cpu_sr, SR_S);
	return;
    case 0x0018:		/* sett */
	gen_set_t();
	return;
    case 0xfbfd:		/* frchg */
	tcg_gen_xori_i32(cpu_fpscr, cpu_fpscr, FPSCR_FR);
	ctx->bstate = BS_STOP;
	return;
    case 0xf3fd:		/* fschg */
	tcg_gen_xori_i32(cpu_fpscr, cpu_fpscr, FPSCR_SZ);
	ctx->bstate = BS_STOP;
	return;
    case 0x0009:		/* nop */
	return;
    case 0x001b:		/* sleep */
	CHECK_PRIVILEGED
	gen_helper_sleep(tcg_const_i32(ctx->pc + 2));
	return;
    }

    switch (ctx->opcode & 0xf000) {
    case 0x1000:		/* mov.l Rm,@(disp,Rn) */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_addi_i32(addr, REG(B11_8), B3_0 * 4);
	    tcg_gen_qemu_st32(REG(B7_4), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0x5000:		/* mov.l @(disp,Rm),Rn */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_addi_i32(addr, REG(B7_4), B3_0 * 4);
	    tcg_gen_qemu_ld32s(REG(B11_8), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0xe000:		/* mov #imm,Rn */
	tcg_gen_movi_i32(REG(B11_8), B7_0s);
	return;
    case 0x9000:		/* mov.w @(disp,PC),Rn */
	{
	    TCGv addr = tcg_const_i32(ctx->pc + 4 + B7_0 * 2);
	    tcg_gen_qemu_ld16s(REG(B11_8), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0xd000:		/* mov.l @(disp,PC),Rn */
	{
	    TCGv addr = tcg_const_i32((ctx->pc + 4 + B7_0 * 4) & ~3);
	    tcg_gen_qemu_ld32s(REG(B11_8), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0x7000:		/* add #imm,Rn */
	tcg_gen_addi_i32(REG(B11_8), REG(B11_8), B7_0s);
	return;
    case 0xa000:		/* bra disp */
	CHECK_NOT_DELAY_SLOT
	ctx->delayed_pc = ctx->pc + 4 + B11_0s * 2;
	tcg_gen_movi_i32(cpu_delayed_pc, ctx->delayed_pc);
	ctx->flags |= DELAY_SLOT;
	return;
    case 0xb000:		/* bsr disp */
	CHECK_NOT_DELAY_SLOT
	tcg_gen_movi_i32(cpu_pr, ctx->pc + 4);
	ctx->delayed_pc = ctx->pc + 4 + B11_0s * 2;
	tcg_gen_movi_i32(cpu_delayed_pc, ctx->delayed_pc);
	ctx->flags |= DELAY_SLOT;
	return;
    }

    switch (ctx->opcode & 0xf00f) {
    case 0x6003:		/* mov Rm,Rn */
	tcg_gen_mov_i32(REG(B11_8), REG(B7_4));
	return;
    case 0x2000:		/* mov.b Rm,@Rn */
	tcg_gen_qemu_st8(REG(B7_4), REG(B11_8), ctx->memidx);
	return;
    case 0x2001:		/* mov.w Rm,@Rn */
	tcg_gen_qemu_st16(REG(B7_4), REG(B11_8), ctx->memidx);
	return;
    case 0x2002:		/* mov.l Rm,@Rn */
	tcg_gen_qemu_st32(REG(B7_4), REG(B11_8), ctx->memidx);
	return;
    case 0x6000:		/* mov.b @Rm,Rn */
	tcg_gen_qemu_ld8s(REG(B11_8), REG(B7_4), ctx->memidx);
	return;
    case 0x6001:		/* mov.w @Rm,Rn */
	tcg_gen_qemu_ld16s(REG(B11_8), REG(B7_4), ctx->memidx);
	return;
    case 0x6002:		/* mov.l @Rm,Rn */
	tcg_gen_qemu_ld32s(REG(B11_8), REG(B7_4), ctx->memidx);
	return;
    case 0x2004:		/* mov.b Rm,@-Rn */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_subi_i32(addr, REG(B11_8), 1);
	    tcg_gen_qemu_st8(REG(B7_4), addr, ctx->memidx);	/* might cause re-execution */
	    tcg_gen_subi_i32(REG(B11_8), REG(B11_8), 1);	/* modify register status */
	    tcg_temp_free(addr);
	}
	return;
    case 0x2005:		/* mov.w Rm,@-Rn */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_subi_i32(addr, REG(B11_8), 2);
	    tcg_gen_qemu_st16(REG(B7_4), addr, ctx->memidx);
	    tcg_gen_subi_i32(REG(B11_8), REG(B11_8), 2);
	    tcg_temp_free(addr);
	}
	return;
    case 0x2006:		/* mov.l Rm,@-Rn */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_subi_i32(addr, REG(B11_8), 4);
	    tcg_gen_qemu_st32(REG(B7_4), addr, ctx->memidx);
	    tcg_gen_subi_i32(REG(B11_8), REG(B11_8), 4);
	}
	return;
    case 0x6004:		/* mov.b @Rm+,Rn */
	tcg_gen_qemu_ld8s(REG(B11_8), REG(B7_4), ctx->memidx);
	if ( B11_8 != B7_4 )
		tcg_gen_addi_i32(REG(B7_4), REG(B7_4), 1);
	return;
    case 0x6005:		/* mov.w @Rm+,Rn */
	tcg_gen_qemu_ld16s(REG(B11_8), REG(B7_4), ctx->memidx);
	if ( B11_8 != B7_4 )
		tcg_gen_addi_i32(REG(B7_4), REG(B7_4), 2);
	return;
    case 0x6006:		/* mov.l @Rm+,Rn */
	tcg_gen_qemu_ld32s(REG(B11_8), REG(B7_4), ctx->memidx);
	if ( B11_8 != B7_4 )
		tcg_gen_addi_i32(REG(B7_4), REG(B7_4), 4);
	return;
    case 0x0004:		/* mov.b Rm,@(R0,Rn) */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_add_i32(addr, REG(B11_8), REG(0));
	    tcg_gen_qemu_st8(REG(B7_4), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0x0005:		/* mov.w Rm,@(R0,Rn) */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_add_i32(addr, REG(B11_8), REG(0));
	    tcg_gen_qemu_st16(REG(B7_4), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0x0006:		/* mov.l Rm,@(R0,Rn) */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_add_i32(addr, REG(B11_8), REG(0));
	    tcg_gen_qemu_st32(REG(B7_4), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0x000c:		/* mov.b @(R0,Rm),Rn */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_add_i32(addr, REG(B7_4), REG(0));
	    tcg_gen_qemu_ld8s(REG(B11_8), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0x000d:		/* mov.w @(R0,Rm),Rn */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_add_i32(addr, REG(B7_4), REG(0));
	    tcg_gen_qemu_ld16s(REG(B11_8), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0x000e:		/* mov.l @(R0,Rm),Rn */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_add_i32(addr, REG(B7_4), REG(0));
	    tcg_gen_qemu_ld32s(REG(B11_8), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0x6008:		/* swap.b Rm,Rn */
	{
	    TCGv highw, high, low;
	    highw = tcg_temp_new();
	    tcg_gen_andi_i32(highw, REG(B7_4), 0xffff0000);
	    high = tcg_temp_new();
	    tcg_gen_ext8u_i32(high, REG(B7_4));
	    tcg_gen_shli_i32(high, high, 8);
	    low = tcg_temp_new();
	    tcg_gen_shri_i32(low, REG(B7_4), 8);
	    tcg_gen_ext8u_i32(low, low);
	    tcg_gen_or_i32(REG(B11_8), high, low);
	    tcg_gen_or_i32(REG(B11_8), REG(B11_8), highw);
	    tcg_temp_free(low);
	    tcg_temp_free(high);
	}
	return;
    case 0x6009:		/* swap.w Rm,Rn */
	{
	    TCGv high, low;
	    high = tcg_temp_new();
	    tcg_gen_ext16u_i32(high, REG(B7_4));
	    tcg_gen_shli_i32(high, high, 16);
	    low = tcg_temp_new();
	    tcg_gen_shri_i32(low, REG(B7_4), 16);
	    tcg_gen_ext16u_i32(low, low);
	    tcg_gen_or_i32(REG(B11_8), high, low);
	    tcg_temp_free(low);
	    tcg_temp_free(high);
	}
	return;
    case 0x200d:		/* xtrct Rm,Rn */
	{
	    TCGv high, low;
	    high = tcg_temp_new();
	    tcg_gen_ext16u_i32(high, REG(B7_4));
	    tcg_gen_shli_i32(high, high, 16);
	    low = tcg_temp_new();
	    tcg_gen_shri_i32(low, REG(B11_8), 16);
	    tcg_gen_ext16u_i32(low, low);
	    tcg_gen_or_i32(REG(B11_8), high, low);
	    tcg_temp_free(low);
	    tcg_temp_free(high);
	}
	return;
    case 0x300c:		/* add Rm,Rn */
	tcg_gen_add_i32(REG(B11_8), REG(B11_8), REG(B7_4));
	return;
    case 0x300e:		/* addc Rm,Rn */
	gen_helper_addc(REG(B11_8), REG(B7_4), REG(B11_8));
	return;
    case 0x300f:		/* addv Rm,Rn */
	gen_helper_addv(REG(B11_8), REG(B7_4), REG(B11_8));
	return;
    case 0x2009:		/* and Rm,Rn */
	tcg_gen_and_i32(REG(B11_8), REG(B11_8), REG(B7_4));
	return;
    case 0x3000:		/* cmp/eq Rm,Rn */
	gen_cmp(TCG_COND_EQ, REG(B7_4), REG(B11_8));
	return;
    case 0x3003:		/* cmp/ge Rm,Rn */
	gen_cmp(TCG_COND_GE, REG(B7_4), REG(B11_8));
	return;
    case 0x3007:		/* cmp/gt Rm,Rn */
	gen_cmp(TCG_COND_GT, REG(B7_4), REG(B11_8));
	return;
    case 0x3006:		/* cmp/hi Rm,Rn */
	gen_cmp(TCG_COND_GTU, REG(B7_4), REG(B11_8));
	return;
    case 0x3002:		/* cmp/hs Rm,Rn */
	gen_cmp(TCG_COND_GEU, REG(B7_4), REG(B11_8));
	return;
    case 0x200c:		/* cmp/str Rm,Rn */
	{
	    int label1 = gen_new_label();
	    int label2 = gen_new_label();
	    TCGv cmp1 = tcg_temp_local_new();
	    TCGv cmp2 = tcg_temp_local_new();
	    tcg_gen_xor_i32(cmp1, REG(B7_4), REG(B11_8));
	    tcg_gen_andi_i32(cmp2, cmp1, 0xff000000);
	    tcg_gen_brcondi_i32(TCG_COND_EQ, cmp2, 0, label1);
	    tcg_gen_andi_i32(cmp2, cmp1, 0x00ff0000);
	    tcg_gen_brcondi_i32(TCG_COND_EQ, cmp2, 0, label1);
	    tcg_gen_andi_i32(cmp2, cmp1, 0x0000ff00);
	    tcg_gen_brcondi_i32(TCG_COND_EQ, cmp2, 0, label1);
	    tcg_gen_andi_i32(cmp2, cmp1, 0x000000ff);
	    tcg_gen_brcondi_i32(TCG_COND_EQ, cmp2, 0, label1);
	    tcg_gen_andi_i32(cpu_sr, cpu_sr, ~SR_T);
	    tcg_gen_br(label2);
	    gen_set_label(label1);
	    tcg_gen_ori_i32(cpu_sr, cpu_sr, SR_T);
	    gen_set_label(label2);
	    tcg_temp_free(cmp2);
	    tcg_temp_free(cmp1);
	}
	return;
    case 0x2007:		/* div0s Rm,Rn */
	{
	    gen_copy_bit_i32(cpu_sr, 8, REG(B11_8), 31);	/* SR_Q */
	    gen_copy_bit_i32(cpu_sr, 9, REG(B7_4), 31);		/* SR_M */
	    TCGv val = tcg_temp_new();
	    tcg_gen_xor_i32(val, REG(B7_4), REG(B11_8));
	    gen_copy_bit_i32(cpu_sr, 0, val, 31);		/* SR_T */
	    tcg_temp_free(val);
	}
	return;
    case 0x3004:		/* div1 Rm,Rn */
	gen_helper_div1(REG(B11_8), REG(B7_4), REG(B11_8));
	return;
    case 0x300d:		/* dmuls.l Rm,Rn */
	{
	    TCGv_i64 tmp1 = tcg_temp_new_i64();
	    TCGv_i64 tmp2 = tcg_temp_new_i64();

	    tcg_gen_ext_i32_i64(tmp1, REG(B7_4));
	    tcg_gen_ext_i32_i64(tmp2, REG(B11_8));
	    tcg_gen_mul_i64(tmp1, tmp1, tmp2);
	    tcg_gen_trunc_i64_i32(cpu_macl, tmp1);
	    tcg_gen_shri_i64(tmp1, tmp1, 32);
	    tcg_gen_trunc_i64_i32(cpu_mach, tmp1);

	    tcg_temp_free_i64(tmp2);
	    tcg_temp_free_i64(tmp1);
	}
	return;
    case 0x3005:		/* dmulu.l Rm,Rn */
	{
	    TCGv_i64 tmp1 = tcg_temp_new_i64();
	    TCGv_i64 tmp2 = tcg_temp_new_i64();

	    tcg_gen_extu_i32_i64(tmp1, REG(B7_4));
	    tcg_gen_extu_i32_i64(tmp2, REG(B11_8));
	    tcg_gen_mul_i64(tmp1, tmp1, tmp2);
	    tcg_gen_trunc_i64_i32(cpu_macl, tmp1);
	    tcg_gen_shri_i64(tmp1, tmp1, 32);
	    tcg_gen_trunc_i64_i32(cpu_mach, tmp1);

	    tcg_temp_free_i64(tmp2);
	    tcg_temp_free_i64(tmp1);
	}
	return;
    case 0x600e:		/* exts.b Rm,Rn */
	tcg_gen_ext8s_i32(REG(B11_8), REG(B7_4));
	return;
    case 0x600f:		/* exts.w Rm,Rn */
	tcg_gen_ext16s_i32(REG(B11_8), REG(B7_4));
	return;
    case 0x600c:		/* extu.b Rm,Rn */
	tcg_gen_ext8u_i32(REG(B11_8), REG(B7_4));
	return;
    case 0x600d:		/* extu.w Rm,Rn */
	tcg_gen_ext16u_i32(REG(B11_8), REG(B7_4));
	return;
    case 0x000f:		/* mac.l @Rm+,@Rn+ */
	{
	    TCGv arg0, arg1;
	    arg0 = tcg_temp_new();
	    tcg_gen_qemu_ld32s(arg0, REG(B7_4), ctx->memidx);
	    arg1 = tcg_temp_new();
	    tcg_gen_qemu_ld32s(arg1, REG(B11_8), ctx->memidx);
	    gen_helper_macl(arg0, arg1);
	    tcg_temp_free(arg1);
	    tcg_temp_free(arg0);
	    tcg_gen_addi_i32(REG(B7_4), REG(B7_4), 4);
	    tcg_gen_addi_i32(REG(B11_8), REG(B11_8), 4);
	}
	return;
    case 0x400f:		/* mac.w @Rm+,@Rn+ */
	{
	    TCGv arg0, arg1;
	    arg0 = tcg_temp_new();
	    tcg_gen_qemu_ld32s(arg0, REG(B7_4), ctx->memidx);
	    arg1 = tcg_temp_new();
	    tcg_gen_qemu_ld32s(arg1, REG(B11_8), ctx->memidx);
	    gen_helper_macw(arg0, arg1);
	    tcg_temp_free(arg1);
	    tcg_temp_free(arg0);
	    tcg_gen_addi_i32(REG(B11_8), REG(B11_8), 2);
	    tcg_gen_addi_i32(REG(B7_4), REG(B7_4), 2);
	}
	return;
    case 0x0007:		/* mul.l Rm,Rn */
	tcg_gen_mul_i32(cpu_macl, REG(B7_4), REG(B11_8));
	return;
    case 0x200f:		/* muls.w Rm,Rn */
	{
	    TCGv arg0, arg1;
	    arg0 = tcg_temp_new();
	    tcg_gen_ext16s_i32(arg0, REG(B7_4));
	    arg1 = tcg_temp_new();
	    tcg_gen_ext16s_i32(arg1, REG(B11_8));
	    tcg_gen_mul_i32(cpu_macl, arg0, arg1);
	    tcg_temp_free(arg1);
	    tcg_temp_free(arg0);
	}
	return;
    case 0x200e:		/* mulu.w Rm,Rn */
	{
	    TCGv arg0, arg1;
	    arg0 = tcg_temp_new();
	    tcg_gen_ext16u_i32(arg0, REG(B7_4));
	    arg1 = tcg_temp_new();
	    tcg_gen_ext16u_i32(arg1, REG(B11_8));
	    tcg_gen_mul_i32(cpu_macl, arg0, arg1);
	    tcg_temp_free(arg1);
	    tcg_temp_free(arg0);
	}
	return;
    case 0x600b:		/* neg Rm,Rn */
	tcg_gen_neg_i32(REG(B11_8), REG(B7_4));
	return;
    case 0x600a:		/* negc Rm,Rn */
	gen_helper_negc(REG(B11_8), REG(B7_4));
	return;
    case 0x6007:		/* not Rm,Rn */
	tcg_gen_not_i32(REG(B11_8), REG(B7_4));
	return;
    case 0x200b:		/* or Rm,Rn */
	tcg_gen_or_i32(REG(B11_8), REG(B11_8), REG(B7_4));
	return;
    case 0x400c:		/* shad Rm,Rn */
	{
	    int label1 = gen_new_label();
	    int label2 = gen_new_label();
	    int label3 = gen_new_label();
	    int label4 = gen_new_label();
	    TCGv shift = tcg_temp_local_new();
	    tcg_gen_brcondi_i32(TCG_COND_LT, REG(B7_4), 0, label1);
	    /* Rm positive, shift to the left */
	    tcg_gen_andi_i32(shift, REG(B7_4), 0x1f);
	    tcg_gen_shl_i32(REG(B11_8), REG(B11_8), shift);
	    tcg_gen_br(label4);
	    /* Rm negative, shift to the right */
	    gen_set_label(label1);
	    tcg_gen_andi_i32(shift, REG(B7_4), 0x1f);
	    tcg_gen_brcondi_i32(TCG_COND_EQ, shift, 0, label2);
	    tcg_gen_not_i32(shift, REG(B7_4));
	    tcg_gen_andi_i32(shift, shift, 0x1f);
	    tcg_gen_addi_i32(shift, shift, 1);
	    tcg_gen_sar_i32(REG(B11_8), REG(B11_8), shift);
	    tcg_gen_br(label4);
	    /* Rm = -32 */
	    gen_set_label(label2);
	    tcg_gen_brcondi_i32(TCG_COND_LT, REG(B11_8), 0, label3);
	    tcg_gen_movi_i32(REG(B11_8), 0);
	    tcg_gen_br(label4);
	    gen_set_label(label3);
	    tcg_gen_movi_i32(REG(B11_8), 0xffffffff);
	    gen_set_label(label4);
	    tcg_temp_free(shift);
	}
	return;
    case 0x400d:		/* shld Rm,Rn */
	{
	    int label1 = gen_new_label();
	    int label2 = gen_new_label();
	    int label3 = gen_new_label();
	    TCGv shift = tcg_temp_local_new();
	    tcg_gen_brcondi_i32(TCG_COND_LT, REG(B7_4), 0, label1);
	    /* Rm positive, shift to the left */
	    tcg_gen_andi_i32(shift, REG(B7_4), 0x1f);
	    tcg_gen_shl_i32(REG(B11_8), REG(B11_8), shift);
	    tcg_gen_br(label3);
	    /* Rm negative, shift to the right */
	    gen_set_label(label1);
	    tcg_gen_andi_i32(shift, REG(B7_4), 0x1f);
	    tcg_gen_brcondi_i32(TCG_COND_EQ, shift, 0, label2);
	    tcg_gen_not_i32(shift, REG(B7_4));
	    tcg_gen_andi_i32(shift, shift, 0x1f);
	    tcg_gen_addi_i32(shift, shift, 1);
	    tcg_gen_shr_i32(REG(B11_8), REG(B11_8), shift);
	    tcg_gen_br(label3);
	    /* Rm = -32 */
	    gen_set_label(label2);
	    tcg_gen_movi_i32(REG(B11_8), 0);
	    gen_set_label(label3);
	    tcg_temp_free(shift);
	}
	return;
    case 0x3008:		/* sub Rm,Rn */
	tcg_gen_sub_i32(REG(B11_8), REG(B11_8), REG(B7_4));
	return;
    case 0x300a:		/* subc Rm,Rn */
	gen_helper_subc(REG(B11_8), REG(B7_4), REG(B11_8));
	return;
    case 0x300b:		/* subv Rm,Rn */
	gen_helper_subv(REG(B11_8), REG(B7_4), REG(B11_8));
	return;
    case 0x2008:		/* tst Rm,Rn */
	{
	    TCGv val = tcg_temp_new();
	    tcg_gen_and_i32(val, REG(B7_4), REG(B11_8));
	    gen_cmp_imm(TCG_COND_EQ, val, 0);
	    tcg_temp_free(val);
	}
	return;
    case 0x200a:		/* xor Rm,Rn */
	tcg_gen_xor_i32(REG(B11_8), REG(B11_8), REG(B7_4));
	return;
    case 0xf00c: /* fmov {F,D,X}Rm,{F,D,X}Rn - FPSCR: Nothing */
	CHECK_FPU_ENABLED
	if (ctx->fpscr & FPSCR_SZ) {
	    TCGv_i64 fp = tcg_temp_new_i64();
	    gen_load_fpr64(fp, XREG(B7_4));
	    gen_store_fpr64(fp, XREG(B11_8));
	    tcg_temp_free_i64(fp);
	} else {
	    tcg_gen_mov_i32(cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B7_4)]);
	}
	return;
    case 0xf00a: /* fmov {F,D,X}Rm,@Rn - FPSCR: Nothing */
	CHECK_FPU_ENABLED
	if (ctx->fpscr & FPSCR_SZ) {
	    TCGv addr_hi = tcg_temp_new();
	    int fr = XREG(B7_4);
	    tcg_gen_addi_i32(addr_hi, REG(B11_8), 4);
	    tcg_gen_qemu_st32(cpu_fregs[fr  ], REG(B11_8), ctx->memidx);
	    tcg_gen_qemu_st32(cpu_fregs[fr+1], addr_hi,	   ctx->memidx);
	    tcg_temp_free(addr_hi);
	} else {
	    tcg_gen_qemu_st32(cpu_fregs[FREG(B7_4)], REG(B11_8), ctx->memidx);
	}
	return;
    case 0xf008: /* fmov @Rm,{F,D,X}Rn - FPSCR: Nothing */
	CHECK_FPU_ENABLED
	if (ctx->fpscr & FPSCR_SZ) {
	    TCGv addr_hi = tcg_temp_new();
	    int fr = XREG(B11_8);
	    tcg_gen_addi_i32(addr_hi, REG(B7_4), 4);
	    tcg_gen_qemu_ld32u(cpu_fregs[fr  ], REG(B7_4), ctx->memidx);
	    tcg_gen_qemu_ld32u(cpu_fregs[fr+1], addr_hi,   ctx->memidx);
	    tcg_temp_free(addr_hi);
	} else {
	    tcg_gen_qemu_ld32u(cpu_fregs[FREG(B11_8)], REG(B7_4), ctx->memidx);
	}
	return;
    case 0xf009: /* fmov @Rm+,{F,D,X}Rn - FPSCR: Nothing */
	CHECK_FPU_ENABLED
	if (ctx->fpscr & FPSCR_SZ) {
	    TCGv addr_hi = tcg_temp_new();
	    int fr = XREG(B11_8);
	    tcg_gen_addi_i32(addr_hi, REG(B7_4), 4);
	    tcg_gen_qemu_ld32u(cpu_fregs[fr  ], REG(B7_4), ctx->memidx);
	    tcg_gen_qemu_ld32u(cpu_fregs[fr+1], addr_hi,   ctx->memidx);
	    tcg_gen_addi_i32(REG(B7_4), REG(B7_4), 8);
	    tcg_temp_free(addr_hi);
	} else {
	    tcg_gen_qemu_ld32u(cpu_fregs[FREG(B11_8)], REG(B7_4), ctx->memidx);
	    tcg_gen_addi_i32(REG(B7_4), REG(B7_4), 4);
	}
	return;
    case 0xf00b: /* fmov {F,D,X}Rm,@-Rn - FPSCR: Nothing */
	CHECK_FPU_ENABLED
	if (ctx->fpscr & FPSCR_SZ) {
	    TCGv addr = tcg_temp_new_i32();
	    int fr = XREG(B7_4);
	    tcg_gen_subi_i32(addr, REG(B11_8), 4);
	    tcg_gen_qemu_st32(cpu_fregs[fr+1], addr, ctx->memidx);
	    tcg_gen_subi_i32(addr, REG(B11_8), 8);
	    tcg_gen_qemu_st32(cpu_fregs[fr  ], addr, ctx->memidx);
	    tcg_gen_mov_i32(REG(B11_8), addr);
	    tcg_temp_free(addr);
	} else {
	    TCGv addr;
	    addr = tcg_temp_new_i32();
	    tcg_gen_subi_i32(addr, REG(B11_8), 4);
	    tcg_gen_qemu_st32(cpu_fregs[FREG(B7_4)], addr, ctx->memidx);
	    tcg_temp_free(addr);
	    tcg_gen_subi_i32(REG(B11_8), REG(B11_8), 4);
	}
	return;
    case 0xf006: /* fmov @(R0,Rm),{F,D,X}Rm - FPSCR: Nothing */
	CHECK_FPU_ENABLED
	{
	    TCGv addr = tcg_temp_new_i32();
	    tcg_gen_add_i32(addr, REG(B7_4), REG(0));
	    if (ctx->fpscr & FPSCR_SZ) {
		int fr = XREG(B11_8);
		tcg_gen_qemu_ld32u(cpu_fregs[fr	 ], addr, ctx->memidx);
		tcg_gen_addi_i32(addr, addr, 4);
		tcg_gen_qemu_ld32u(cpu_fregs[fr+1], addr, ctx->memidx);
	    } else {
		tcg_gen_qemu_ld32u(cpu_fregs[FREG(B11_8)], addr, ctx->memidx);
	    }
	    tcg_temp_free(addr);
	}
	return;
    case 0xf007: /* fmov {F,D,X}Rn,@(R0,Rn) - FPSCR: Nothing */
	CHECK_FPU_ENABLED
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_add_i32(addr, REG(B11_8), REG(0));
	    if (ctx->fpscr & FPSCR_SZ) {
		int fr = XREG(B7_4);
		tcg_gen_qemu_ld32u(cpu_fregs[fr	 ], addr, ctx->memidx);
		tcg_gen_addi_i32(addr, addr, 4);
		tcg_gen_qemu_ld32u(cpu_fregs[fr+1], addr, ctx->memidx);
	    } else {
		tcg_gen_qemu_st32(cpu_fregs[FREG(B7_4)], addr, ctx->memidx);
	    }
	    tcg_temp_free(addr);
	}
	return;
    case 0xf000: /* fadd Rm,Rn - FPSCR: R[PR,Enable.O/U/I]/W[Cause,Flag] */
    case 0xf001: /* fsub Rm,Rn - FPSCR: R[PR,Enable.O/U/I]/W[Cause,Flag] */
    case 0xf002: /* fmul Rm,Rn - FPSCR: R[PR,Enable.O/U/I]/W[Cause,Flag] */
    case 0xf003: /* fdiv Rm,Rn - FPSCR: R[PR,Enable.O/U/I]/W[Cause,Flag] */
    case 0xf004: /* fcmp/eq Rm,Rn - FPSCR: R[PR,Enable.V]/W[Cause,Flag] */
    case 0xf005: /* fcmp/gt Rm,Rn - FPSCR: R[PR,Enable.V]/W[Cause,Flag] */
	{
	    CHECK_FPU_ENABLED
	    if (ctx->fpscr & FPSCR_PR) {
                TCGv_i64 fp0, fp1;

		if (ctx->opcode & 0x0110)
		    break; /* illegal instruction */
		fp0 = tcg_temp_new_i64();
		fp1 = tcg_temp_new_i64();
		gen_load_fpr64(fp0, DREG(B11_8));
		gen_load_fpr64(fp1, DREG(B7_4));
                switch (ctx->opcode & 0xf00f) {
                case 0xf000:		/* fadd Rm,Rn */
                    gen_helper_fadd_DT(fp0, fp0, fp1);
                    break;
                case 0xf001:		/* fsub Rm,Rn */
                    gen_helper_fsub_DT(fp0, fp0, fp1);
                    break;
                case 0xf002:		/* fmul Rm,Rn */
                    gen_helper_fmul_DT(fp0, fp0, fp1);
                    break;
                case 0xf003:		/* fdiv Rm,Rn */
                    gen_helper_fdiv_DT(fp0, fp0, fp1);
                    break;
                case 0xf004:		/* fcmp/eq Rm,Rn */
                    gen_helper_fcmp_eq_DT(fp0, fp1);
                    return;
                case 0xf005:		/* fcmp/gt Rm,Rn */
                    gen_helper_fcmp_gt_DT(fp0, fp1);
                    return;
                }
		gen_store_fpr64(fp0, DREG(B11_8));
                tcg_temp_free_i64(fp0);
                tcg_temp_free_i64(fp1);
	    } else {
                switch (ctx->opcode & 0xf00f) {
                case 0xf000:		/* fadd Rm,Rn */
                    gen_helper_fadd_FT(cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B7_4)]);
                    break;
                case 0xf001:		/* fsub Rm,Rn */
                    gen_helper_fsub_FT(cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B7_4)]);
                    break;
                case 0xf002:		/* fmul Rm,Rn */
                    gen_helper_fmul_FT(cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B7_4)]);
                    break;
                case 0xf003:		/* fdiv Rm,Rn */
                    gen_helper_fdiv_FT(cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B7_4)]);
                    break;
                case 0xf004:		/* fcmp/eq Rm,Rn */
                    gen_helper_fcmp_eq_FT(cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B7_4)]);
                    return;
                case 0xf005:		/* fcmp/gt Rm,Rn */
                    gen_helper_fcmp_gt_FT(cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B7_4)]);
                    return;
                }
	    }
	}
	return;
    case 0xf00e: /* fmac FR0,RM,Rn */
        {
            CHECK_FPU_ENABLED
            if (ctx->fpscr & FPSCR_PR) {
                break; /* illegal instruction */
            } else {
                gen_helper_fmac_FT(cpu_fregs[FREG(B11_8)],
                                   cpu_fregs[FREG(0)], cpu_fregs[FREG(B7_4)], cpu_fregs[FREG(B11_8)]);
                return;
            }
        }
    }

    switch (ctx->opcode & 0xff00) {
    case 0xc900:		/* and #imm,R0 */
	tcg_gen_andi_i32(REG(0), REG(0), B7_0);
	return;
    case 0xcd00:		/* and.b #imm,@(R0,GBR) */
	{
	    TCGv addr, val;
	    addr = tcg_temp_new();
	    tcg_gen_add_i32(addr, REG(0), cpu_gbr);
	    val = tcg_temp_new();
	    tcg_gen_qemu_ld8u(val, addr, ctx->memidx);
	    tcg_gen_andi_i32(val, val, B7_0);
	    tcg_gen_qemu_st8(val, addr, ctx->memidx);
	    tcg_temp_free(val);
	    tcg_temp_free(addr);
	}
	return;
    case 0x8b00:		/* bf label */
	CHECK_NOT_DELAY_SLOT
	    gen_conditional_jump(ctx, ctx->pc + 2,
				 ctx->pc + 4 + B7_0s * 2);
	ctx->bstate = BS_BRANCH;
	return;
    case 0x8f00:		/* bf/s label */
	CHECK_NOT_DELAY_SLOT
	gen_branch_slot(ctx->delayed_pc = ctx->pc + 4 + B7_0s * 2, 0);
	ctx->flags |= DELAY_SLOT_CONDITIONAL;
	return;
    case 0x8900:		/* bt label */
	CHECK_NOT_DELAY_SLOT
	    gen_conditional_jump(ctx, ctx->pc + 4 + B7_0s * 2,
				 ctx->pc + 2);
	ctx->bstate = BS_BRANCH;
	return;
    case 0x8d00:		/* bt/s label */
	CHECK_NOT_DELAY_SLOT
	gen_branch_slot(ctx->delayed_pc = ctx->pc + 4 + B7_0s * 2, 1);
	ctx->flags |= DELAY_SLOT_CONDITIONAL;
	return;
    case 0x8800:		/* cmp/eq #imm,R0 */
	gen_cmp_imm(TCG_COND_EQ, REG(0), B7_0s);
	return;
    case 0xc400:		/* mov.b @(disp,GBR),R0 */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_addi_i32(addr, cpu_gbr, B7_0);
	    tcg_gen_qemu_ld8s(REG(0), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0xc500:		/* mov.w @(disp,GBR),R0 */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_addi_i32(addr, cpu_gbr, B7_0 * 2);
	    tcg_gen_qemu_ld16s(REG(0), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0xc600:		/* mov.l @(disp,GBR),R0 */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_addi_i32(addr, cpu_gbr, B7_0 * 4);
	    tcg_gen_qemu_ld32s(REG(0), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0xc000:		/* mov.b R0,@(disp,GBR) */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_addi_i32(addr, cpu_gbr, B7_0);
	    tcg_gen_qemu_st8(REG(0), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0xc100:		/* mov.w R0,@(disp,GBR) */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_addi_i32(addr, cpu_gbr, B7_0 * 2);
	    tcg_gen_qemu_st16(REG(0), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0xc200:		/* mov.l R0,@(disp,GBR) */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_addi_i32(addr, cpu_gbr, B7_0 * 4);
	    tcg_gen_qemu_st32(REG(0), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0x8000:		/* mov.b R0,@(disp,Rn) */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_addi_i32(addr, REG(B7_4), B3_0);
	    tcg_gen_qemu_st8(REG(0), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0x8100:		/* mov.w R0,@(disp,Rn) */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_addi_i32(addr, REG(B7_4), B3_0 * 2);
	    tcg_gen_qemu_st16(REG(0), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0x8400:		/* mov.b @(disp,Rn),R0 */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_addi_i32(addr, REG(B7_4), B3_0);
	    tcg_gen_qemu_ld8s(REG(0), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0x8500:		/* mov.w @(disp,Rn),R0 */
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_addi_i32(addr, REG(B7_4), B3_0 * 2);
	    tcg_gen_qemu_ld16s(REG(0), addr, ctx->memidx);
	    tcg_temp_free(addr);
	}
	return;
    case 0xc700:		/* mova @(disp,PC),R0 */
	tcg_gen_movi_i32(REG(0), ((ctx->pc & 0xfffffffc) + 4 + B7_0 * 4) & ~3);
	return;
    case 0xcb00:		/* or #imm,R0 */
	tcg_gen_ori_i32(REG(0), REG(0), B7_0);
	return;
    case 0xcf00:		/* or.b #imm,@(R0,GBR) */
	{
	    TCGv addr, val;
	    addr = tcg_temp_new();
	    tcg_gen_add_i32(addr, REG(0), cpu_gbr);
	    val = tcg_temp_new();
	    tcg_gen_qemu_ld8u(val, addr, ctx->memidx);
	    tcg_gen_ori_i32(val, val, B7_0);
	    tcg_gen_qemu_st8(val, addr, ctx->memidx);
	    tcg_temp_free(val);
	    tcg_temp_free(addr);
	}
	return;
    case 0xc300:		/* trapa #imm */
	{
	    TCGv imm;
	    CHECK_NOT_DELAY_SLOT
	    tcg_gen_movi_i32(cpu_pc, ctx->pc);
	    imm = tcg_const_i32(B7_0);
	    gen_helper_trapa(imm);
	    tcg_temp_free(imm);
	    ctx->bstate = BS_BRANCH;
	}
	return;
    case 0xc800:		/* tst #imm,R0 */
	{
	    TCGv val = tcg_temp_new();
	    tcg_gen_andi_i32(val, REG(0), B7_0);
	    gen_cmp_imm(TCG_COND_EQ, val, 0);
	    tcg_temp_free(val);
	}
	return;
    case 0xcc00:		/* tst.b #imm,@(R0,GBR) */
	{
	    TCGv val = tcg_temp_new();
	    tcg_gen_add_i32(val, REG(0), cpu_gbr);
	    tcg_gen_qemu_ld8u(val, val, ctx->memidx);
	    tcg_gen_andi_i32(val, val, B7_0);
	    gen_cmp_imm(TCG_COND_EQ, val, 0);
	    tcg_temp_free(val);
	}
	return;
    case 0xca00:		/* xor #imm,R0 */
	tcg_gen_xori_i32(REG(0), REG(0), B7_0);
	return;
    case 0xce00:		/* xor.b #imm,@(R0,GBR) */
	{
	    TCGv addr, val;
	    addr = tcg_temp_new();
	    tcg_gen_add_i32(addr, REG(0), cpu_gbr);
	    val = tcg_temp_new();
	    tcg_gen_qemu_ld8u(val, addr, ctx->memidx);
	    tcg_gen_xori_i32(val, val, B7_0);
	    tcg_gen_qemu_st8(val, addr, ctx->memidx);
	    tcg_temp_free(val);
	    tcg_temp_free(addr);
	}
	return;
    }

    switch (ctx->opcode & 0xf08f) {
    case 0x408e:		/* ldc Rm,Rn_BANK */
	CHECK_PRIVILEGED
	tcg_gen_mov_i32(ALTREG(B6_4), REG(B11_8));
	return;
    case 0x4087:		/* ldc.l @Rm+,Rn_BANK */
	CHECK_PRIVILEGED
	tcg_gen_qemu_ld32s(ALTREG(B6_4), REG(B11_8), ctx->memidx);
	tcg_gen_addi_i32(REG(B11_8), REG(B11_8), 4);
	return;
    case 0x0082:		/* stc Rm_BANK,Rn */
	CHECK_PRIVILEGED
	tcg_gen_mov_i32(REG(B11_8), ALTREG(B6_4));
	return;
    case 0x4083:		/* stc.l Rm_BANK,@-Rn */
	CHECK_PRIVILEGED
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_subi_i32(addr, REG(B11_8), 4);
	    tcg_gen_qemu_st32(ALTREG(B6_4), addr, ctx->memidx);
	    tcg_temp_free(addr);
	    tcg_gen_subi_i32(REG(B11_8), REG(B11_8), 4);
	}
	return;
    }

    switch (ctx->opcode & 0xf0ff) {
    case 0x0023:		/* braf Rn */
	CHECK_NOT_DELAY_SLOT
	tcg_gen_addi_i32(cpu_delayed_pc, REG(B11_8), ctx->pc + 4);
	ctx->flags |= DELAY_SLOT;
	ctx->delayed_pc = (uint32_t) - 1;
	return;
    case 0x0003:		/* bsrf Rn */
	CHECK_NOT_DELAY_SLOT
	tcg_gen_movi_i32(cpu_pr, ctx->pc + 4);
	tcg_gen_add_i32(cpu_delayed_pc, REG(B11_8), cpu_pr);
	ctx->flags |= DELAY_SLOT;
	ctx->delayed_pc = (uint32_t) - 1;
	return;
    case 0x4015:		/* cmp/pl Rn */
	gen_cmp_imm(TCG_COND_GT, REG(B11_8), 0);
	return;
    case 0x4011:		/* cmp/pz Rn */
	gen_cmp_imm(TCG_COND_GE, REG(B11_8), 0);
	return;
    case 0x4010:		/* dt Rn */
	tcg_gen_subi_i32(REG(B11_8), REG(B11_8), 1);
	gen_cmp_imm(TCG_COND_EQ, REG(B11_8), 0);
	return;
    case 0x402b:		/* jmp @Rn */
	CHECK_NOT_DELAY_SLOT
	tcg_gen_mov_i32(cpu_delayed_pc, REG(B11_8));
	ctx->flags |= DELAY_SLOT;
	ctx->delayed_pc = (uint32_t) - 1;
	return;
    case 0x400b:		/* jsr @Rn */
	CHECK_NOT_DELAY_SLOT
	tcg_gen_movi_i32(cpu_pr, ctx->pc + 4);
	tcg_gen_mov_i32(cpu_delayed_pc, REG(B11_8));
	ctx->flags |= DELAY_SLOT;
	ctx->delayed_pc = (uint32_t) - 1;
	return;
    case 0x400e:		/* ldc Rm,SR */
	CHECK_PRIVILEGED
	tcg_gen_andi_i32(cpu_sr, REG(B11_8), 0x700083f3);
	ctx->bstate = BS_STOP;
	return;
    case 0x4007:		/* ldc.l @Rm+,SR */
	CHECK_PRIVILEGED
	{
	    TCGv val = tcg_temp_new();
	    tcg_gen_qemu_ld32s(val, REG(B11_8), ctx->memidx);
	    tcg_gen_andi_i32(cpu_sr, val, 0x700083f3);
	    tcg_temp_free(val);
	    tcg_gen_addi_i32(REG(B11_8), REG(B11_8), 4);
	    ctx->bstate = BS_STOP;
	}
	return;
    case 0x0002:		/* stc SR,Rn */
	CHECK_PRIVILEGED
	tcg_gen_mov_i32(REG(B11_8), cpu_sr);
	return;
    case 0x4003:		/* stc SR,@-Rn */
	CHECK_PRIVILEGED
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_subi_i32(addr, REG(B11_8), 4);
	    tcg_gen_qemu_st32(cpu_sr, addr, ctx->memidx);
	    tcg_temp_free(addr);
	    tcg_gen_subi_i32(REG(B11_8), REG(B11_8), 4);
	}
	return;
#define LDST(reg,ldnum,ldpnum,stnum,stpnum,prechk)		\
  case ldnum:							\
    prechk    							\
    tcg_gen_mov_i32 (cpu_##reg, REG(B11_8));			\
    return;							\
  case ldpnum:							\
    prechk    							\
    tcg_gen_qemu_ld32s (cpu_##reg, REG(B11_8), ctx->memidx);	\
    tcg_gen_addi_i32(REG(B11_8), REG(B11_8), 4);		\
    return;							\
  case stnum:							\
    prechk    							\
    tcg_gen_mov_i32 (REG(B11_8), cpu_##reg);			\
    return;							\
  case stpnum:							\
    prechk    							\
    {								\
	TCGv addr = tcg_temp_new();			\
	tcg_gen_subi_i32(addr, REG(B11_8), 4);			\
	tcg_gen_qemu_st32 (cpu_##reg, addr, ctx->memidx);	\
	tcg_temp_free(addr);					\
	tcg_gen_subi_i32(REG(B11_8), REG(B11_8), 4);		\
    }								\
    return;
	LDST(gbr,  0x401e, 0x4017, 0x0012, 0x4013, {})
	LDST(vbr,  0x402e, 0x4027, 0x0022, 0x4023, CHECK_PRIVILEGED)
	LDST(ssr,  0x403e, 0x4037, 0x0032, 0x4033, CHECK_PRIVILEGED)
	LDST(spc,  0x404e, 0x4047, 0x0042, 0x4043, CHECK_PRIVILEGED)
	LDST(dbr,  0x40fa, 0x40f6, 0x00fa, 0x40f2, CHECK_PRIVILEGED)
	LDST(mach, 0x400a, 0x4006, 0x000a, 0x4002, {})
	LDST(macl, 0x401a, 0x4016, 0x001a, 0x4012, {})
	LDST(pr,   0x402a, 0x4026, 0x002a, 0x4022, {})
	LDST(fpul, 0x405a, 0x4056, 0x005a, 0x4052, {CHECK_FPU_ENABLED})
    case 0x406a:		/* lds Rm,FPSCR */
	CHECK_FPU_ENABLED
	gen_helper_ld_fpscr(REG(B11_8));
	ctx->bstate = BS_STOP;
	return;
    case 0x4066:		/* lds.l @Rm+,FPSCR */
	CHECK_FPU_ENABLED
	{
	    TCGv addr = tcg_temp_new();
	    tcg_gen_qemu_ld32s(addr, REG(B11_8), ctx->memidx);
	    tcg_gen_addi_i32(REG(B11_8), REG(B11_8), 4);
	    gen_helper_ld_fpscr(addr);
	    tcg_temp_free(addr);
	    ctx->bstate = BS_STOP;
	}
	return;
    case 0x006a:		/* sts FPSCR,Rn */
	CHECK_FPU_ENABLED
	tcg_gen_andi_i32(REG(B11_8), cpu_fpscr, 0x003fffff);
	return;
    case 0x4062:		/* sts FPSCR,@-Rn */
	CHECK_FPU_ENABLED
	{
	    TCGv addr, val;
	    val = tcg_temp_new();
	    tcg_gen_andi_i32(val, cpu_fpscr, 0x003fffff);
	    addr = tcg_temp_new();
	    tcg_gen_subi_i32(addr, REG(B11_8), 4);
	    tcg_gen_qemu_st32(val, addr, ctx->memidx);
	    tcg_temp_free(addr);
	    tcg_temp_free(val);
	    tcg_gen_subi_i32(REG(B11_8), REG(B11_8), 4);
	}
	return;
    case 0x00c3:		/* movca.l R0,@Rm */
        {
            TCGv val = tcg_temp_new();
            tcg_gen_qemu_ld32u(val, REG(B11_8), ctx->memidx);
            gen_helper_movcal (REG(B11_8), val);            
            tcg_gen_qemu_st32(REG(0), REG(B11_8), ctx->memidx);
        }
        ctx->has_movcal = 1;
	return;
    case 0x40a9:
	/* MOVUA.L @Rm,R0 (Rm) -> R0
	   Load non-boundary-aligned data */
	tcg_gen_qemu_ld32u(REG(0), REG(B11_8), ctx->memidx);
	return;
    case 0x40e9:
	/* MOVUA.L @Rm+,R0   (Rm) -> R0, Rm + 4 -> Rm
	   Load non-boundary-aligned data */
	tcg_gen_qemu_ld32u(REG(0), REG(B11_8), ctx->memidx);
	tcg_gen_addi_i32(REG(B11_8), REG(B11_8), 4);
	return;
    case 0x0029:		/* movt Rn */
	tcg_gen_andi_i32(REG(B11_8), cpu_sr, SR_T);
	return;
    case 0x0073:
        /* MOVCO.L
	       LDST -> T
               If (T == 1) R0 -> (Rn)
               0 -> LDST
        */
        if (ctx->features & SH_FEATURE_SH4A) {
	    int label = gen_new_label();
	    gen_clr_t();
	    tcg_gen_or_i32(cpu_sr, cpu_sr, cpu_ldst);
	    tcg_gen_brcondi_i32(TCG_COND_EQ, cpu_ldst, 0, label);
	    tcg_gen_qemu_st32(REG(0), REG(B11_8), ctx->memidx);
	    gen_set_label(label);
	    tcg_gen_movi_i32(cpu_ldst, 0);
	    return;
	} else
	    break;
    case 0x0063:
        /* MOVLI.L @Rm,R0
               1 -> LDST
               (Rm) -> R0
               When interrupt/exception
               occurred 0 -> LDST
        */
	if (ctx->features & SH_FEATURE_SH4A) {
	    tcg_gen_movi_i32(cpu_ldst, 0);
	    tcg_gen_qemu_ld32s(REG(0), REG(B11_8), ctx->memidx);
	    tcg_gen_movi_i32(cpu_ldst, 1);
	    return;
	} else
	    break;
    case 0x0093:		/* ocbi @Rn */
	{
	    gen_helper_ocbi (REG(B11_8));
	}
	return;
    case 0x00a3:		/* ocbp @Rn */
	{
	    TCGv dummy = tcg_temp_new();
	    tcg_gen_qemu_ld32s(dummy, REG(B11_8), ctx->memidx);
	    tcg_temp_free(dummy);
	}
	return;
    case 0x00b3:		/* ocbwb @Rn */
	{
	    TCGv dummy = tcg_temp_new();
	    tcg_gen_qemu_ld32s(dummy, REG(B11_8), ctx->memidx);
	    tcg_temp_free(dummy);
	}
	return;
    case 0x0083:		/* pref @Rn */
	return;
    case 0x00d3:		/* prefi @Rn */
	if (ctx->features & SH_FEATURE_SH4A)
	    return;
	else
	    break;
    case 0x00e3:		/* icbi @Rn */
	if (ctx->features & SH_FEATURE_SH4A)
	    return;
	else
	    break;
    case 0x00ab:		/* synco */
	if (ctx->features & SH_FEATURE_SH4A)
	    return;
	else
	    break;
    case 0x4024:		/* rotcl Rn */
	{
	    TCGv tmp = tcg_temp_new();
	    tcg_gen_mov_i32(tmp, cpu_sr);
	    gen_copy_bit_i32(cpu_sr, 0, REG(B11_8), 31);
	    tcg_gen_shli_i32(REG(B11_8), REG(B11_8), 1);
	    gen_copy_bit_i32(REG(B11_8), 0, tmp, 0);
	    tcg_temp_free(tmp);
	}
	return;
    case 0x4025:		/* rotcr Rn */
	{
	    TCGv tmp = tcg_temp_new();
	    tcg_gen_mov_i32(tmp, cpu_sr);
	    gen_copy_bit_i32(cpu_sr, 0, REG(B11_8), 0);
	    tcg_gen_shri_i32(REG(B11_8), REG(B11_8), 1);
	    gen_copy_bit_i32(REG(B11_8), 31, tmp, 0);
	    tcg_temp_free(tmp);
	}
	return;
    case 0x4004:		/* rotl Rn */
	gen_copy_bit_i32(cpu_sr, 0, REG(B11_8), 31);
	tcg_gen_shli_i32(REG(B11_8), REG(B11_8), 1);
	gen_copy_bit_i32(REG(B11_8), 0, cpu_sr, 0);
	return;
    case 0x4005:		/* rotr Rn */
	gen_copy_bit_i32(cpu_sr, 0, REG(B11_8), 0);
	tcg_gen_shri_i32(REG(B11_8), REG(B11_8), 1);
	gen_copy_bit_i32(REG(B11_8), 31, cpu_sr, 0);
	return;
    case 0x4000:		/* shll Rn */
    case 0x4020:		/* shal Rn */
	gen_copy_bit_i32(cpu_sr, 0, REG(B11_8), 31);
	tcg_gen_shli_i32(REG(B11_8), REG(B11_8), 1);
	return;
    case 0x4021:		/* shar Rn */
	gen_copy_bit_i32(cpu_sr, 0, REG(B11_8), 0);
	tcg_gen_sari_i32(REG(B11_8), REG(B11_8), 1);
	return;
    case 0x4001:		/* shlr Rn */
	gen_copy_bit_i32(cpu_sr, 0, REG(B11_8), 0);
	tcg_gen_shri_i32(REG(B11_8), REG(B11_8), 1);
	return;
    case 0x4008:		/* shll2 Rn */
	tcg_gen_shli_i32(REG(B11_8), REG(B11_8), 2);
	return;
    case 0x4018:		/* shll8 Rn */
	tcg_gen_shli_i32(REG(B11_8), REG(B11_8), 8);
	return;
    case 0x4028:		/* shll16 Rn */
	tcg_gen_shli_i32(REG(B11_8), REG(B11_8), 16);
	return;
    case 0x4009:		/* shlr2 Rn */
	tcg_gen_shri_i32(REG(B11_8), REG(B11_8), 2);
	return;
    case 0x4019:		/* shlr8 Rn */
	tcg_gen_shri_i32(REG(B11_8), REG(B11_8), 8);
	return;
    case 0x4029:		/* shlr16 Rn */
	tcg_gen_shri_i32(REG(B11_8), REG(B11_8), 16);
	return;
    case 0x401b:		/* tas.b @Rn */
	{
	    TCGv addr, val;
	    addr = tcg_temp_local_new();
	    tcg_gen_mov_i32(addr, REG(B11_8));
	    val = tcg_temp_local_new();
	    tcg_gen_qemu_ld8u(val, addr, ctx->memidx);
	    gen_cmp_imm(TCG_COND_EQ, val, 0);
	    tcg_gen_ori_i32(val, val, 0x80);
	    tcg_gen_qemu_st8(val, addr, ctx->memidx);
	    tcg_temp_free(val);
	    tcg_temp_free(addr);
	}
	return;
    case 0xf00d: /* fsts FPUL,FRn - FPSCR: Nothing */
	CHECK_FPU_ENABLED
	tcg_gen_mov_i32(cpu_fregs[FREG(B11_8)], cpu_fpul);
	return;
    case 0xf01d: /* flds FRm,FPUL - FPSCR: Nothing */
	CHECK_FPU_ENABLED
	tcg_gen_mov_i32(cpu_fpul, cpu_fregs[FREG(B11_8)]);
	return;
    case 0xf02d: /* float FPUL,FRn/DRn - FPSCR: R[PR,Enable.I]/W[Cause,Flag] */
	CHECK_FPU_ENABLED
	if (ctx->fpscr & FPSCR_PR) {
	    TCGv_i64 fp;
	    if (ctx->opcode & 0x0100)
		break; /* illegal instruction */
	    fp = tcg_temp_new_i64();
	    gen_helper_float_DT(fp, cpu_fpul);
	    gen_store_fpr64(fp, DREG(B11_8));
	    tcg_temp_free_i64(fp);
	}
	else {
	    gen_helper_float_FT(cpu_fregs[FREG(B11_8)], cpu_fpul);
	}
	return;
    case 0xf03d: /* ftrc FRm/DRm,FPUL - FPSCR: R[PR,Enable.V]/W[Cause,Flag] */
	CHECK_FPU_ENABLED
	if (ctx->fpscr & FPSCR_PR) {
	    TCGv_i64 fp;
	    if (ctx->opcode & 0x0100)
		break; /* illegal instruction */
	    fp = tcg_temp_new_i64();
	    gen_load_fpr64(fp, DREG(B11_8));
	    gen_helper_ftrc_DT(cpu_fpul, fp);
	    tcg_temp_free_i64(fp);
	}
	else {
	    gen_helper_ftrc_FT(cpu_fpul, cpu_fregs[FREG(B11_8)]);
	}
	return;
    case 0xf04d: /* fneg FRn/DRn - FPSCR: Nothing */
	CHECK_FPU_ENABLED
	{
	    gen_helper_fneg_T(cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B11_8)]);
	}
	return;
    case 0xf05d: /* fabs FRn/DRn */
	CHECK_FPU_ENABLED
	if (ctx->fpscr & FPSCR_PR) {
	    if (ctx->opcode & 0x0100)
		break; /* illegal instruction */
	    TCGv_i64 fp = tcg_temp_new_i64();
	    gen_load_fpr64(fp, DREG(B11_8));
	    gen_helper_fabs_DT(fp, fp);
	    gen_store_fpr64(fp, DREG(B11_8));
	    tcg_temp_free_i64(fp);
	} else {
	    gen_helper_fabs_FT(cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B11_8)]);
	}
	return;
    case 0xf06d: /* fsqrt FRn */
	CHECK_FPU_ENABLED
	if (ctx->fpscr & FPSCR_PR) {
	    if (ctx->opcode & 0x0100)
		break; /* illegal instruction */
	    TCGv_i64 fp = tcg_temp_new_i64();
	    gen_load_fpr64(fp, DREG(B11_8));
	    gen_helper_fsqrt_DT(fp, fp);
	    gen_store_fpr64(fp, DREG(B11_8));
	    tcg_temp_free_i64(fp);
	} else {
	    gen_helper_fsqrt_FT(cpu_fregs[FREG(B11_8)], cpu_fregs[FREG(B11_8)]);
	}
	return;
    case 0xf07d: /* fsrra FRn */
	CHECK_FPU_ENABLED
	break;
    case 0xf08d: /* fldi0 FRn - FPSCR: R[PR] */
	CHECK_FPU_ENABLED
	if (!(ctx->fpscr & FPSCR_PR)) {
	    tcg_gen_movi_i32(cpu_fregs[FREG(B11_8)], 0);
	}
	return;
    case 0xf09d: /* fldi1 FRn - FPSCR: R[PR] */
	CHECK_FPU_ENABLED
	if (!(ctx->fpscr & FPSCR_PR)) {
	    tcg_gen_movi_i32(cpu_fregs[FREG(B11_8)], 0x3f800000);
	}
	return;
    case 0xf0ad: /* fcnvsd FPUL,DRn */
	CHECK_FPU_ENABLED
	{
	    TCGv_i64 fp = tcg_temp_new_i64();
	    gen_helper_fcnvsd_FT_DT(fp, cpu_fpul);
	    gen_store_fpr64(fp, DREG(B11_8));
	    tcg_temp_free_i64(fp);
	}
	return;
    case 0xf0bd: /* fcnvds DRn,FPUL */
	CHECK_FPU_ENABLED
	{
	    TCGv_i64 fp = tcg_temp_new_i64();
	    gen_load_fpr64(fp, DREG(B11_8));
	    gen_helper_fcnvds_DT_FT(cpu_fpul, fp);
	    tcg_temp_free_i64(fp);
	}
	return;
    }
#if 0
    fprintf(stderr, "unknown instruction 0x%04x at pc 0x%08x\n",
	    ctx->opcode, ctx->pc);
    fflush(stderr);
#endif
    gen_helper_raise_illegal_instruction();
    ctx->bstate = BS_EXCP;
}

static void decode_opc(DisasContext * ctx)
{
    uint32_t old_flags = ctx->flags;

    _decode_opc(ctx);

    if (old_flags & (DELAY_SLOT | DELAY_SLOT_CONDITIONAL)) {
        if (ctx->flags & DELAY_SLOT_CLEARME) {
            gen_store_flags(0);
        } else {
	    /* go out of the delay slot */
	    uint32_t new_flags = ctx->flags;
	    new_flags &= ~(DELAY_SLOT | DELAY_SLOT_CONDITIONAL);
	    gen_store_flags(new_flags);
        }
        ctx->flags = 0;
        ctx->bstate = BS_BRANCH;
        if (old_flags & DELAY_SLOT_CONDITIONAL) {
	    gen_delayed_conditional_jump(ctx);
        } else if (old_flags & DELAY_SLOT) {
            gen_jump(ctx);
	}

    }

    /* go into a delay slot */
    if (ctx->flags & (DELAY_SLOT | DELAY_SLOT_CONDITIONAL))
        gen_store_flags(ctx->flags);
}

static inline void
gen_intermediate_code_internal(CPUState * env, TranslationBlock * tb,
                               int search_pc)
{
    DisasContext ctx;
    target_ulong pc_start;
    static uint16_t *gen_opc_end;
    CPUBreakpoint *bp;
    int i, ii;
    int num_insns;
    int max_insns;

    pc_start = tb->pc;
    gen_opc_end = gen_opc_buf + OPC_MAX_SIZE;
    ctx.pc = pc_start;
    ctx.flags = (uint32_t)tb->flags;
    ctx.bstate = BS_NONE;
    ctx.sr = env->sr;
    ctx.fpscr = env->fpscr;
    ctx.memidx = (env->sr & SR_MD) ? 1 : 0;
    /* We don't know if the delayed pc came from a dynamic or static branch,
       so assume it is a dynamic branch.  */
    ctx.delayed_pc = -1; /* use delayed pc from env pointer */
    ctx.tb = tb;
    ctx.singlestep_enabled = env->singlestep_enabled;
    ctx.features = env->features;
    ctx.has_movcal = (tb->flags & TB_FLAG_PENDING_MOVCA);

#ifdef DEBUG_DISAS
    qemu_log_mask(CPU_LOG_TB_CPU,
                 "------------------------------------------------\n");
    log_cpu_state_mask(CPU_LOG_TB_CPU, env, 0);
#endif

    ii = -1;
    num_insns = 0;
    max_insns = tb->cflags & CF_COUNT_MASK;
    if (max_insns == 0)
        max_insns = CF_COUNT_MASK;
    gen_icount_start();
    while (ctx.bstate == BS_NONE && gen_opc_ptr < gen_opc_end) {
        if (unlikely(!TAILQ_EMPTY(&env->breakpoints))) {
            TAILQ_FOREACH(bp, &env->breakpoints, entry) {
                if (ctx.pc == bp->pc) {
		    /* We have hit a breakpoint - make sure PC is up-to-date */
		    tcg_gen_movi_i32(cpu_pc, ctx.pc);
		    gen_helper_debug();
		    ctx.bstate = BS_EXCP;
		    break;
		}
	    }
	}
        if (search_pc) {
            i = gen_opc_ptr - gen_opc_buf;
            if (ii < i) {
                ii++;
                while (ii < i)
                    gen_opc_instr_start[ii++] = 0;
            }
            gen_opc_pc[ii] = ctx.pc;
            gen_opc_hflags[ii] = ctx.flags;
            gen_opc_instr_start[ii] = 1;
            gen_opc_icount[ii] = num_insns;
        }
        if (num_insns + 1 == max_insns && (tb->cflags & CF_LAST_IO))
            gen_io_start();
#if 0
	fprintf(stderr, "Loading opcode at address 0x%08x\n", ctx.pc);
	fflush(stderr);
#endif
	ctx.opcode = lduw_code(ctx.pc);
	decode_opc(&ctx);
        num_insns++;
	ctx.pc += 2;
	if ((ctx.pc & (TARGET_PAGE_SIZE - 1)) == 0)
	    break;
	if (env->singlestep_enabled)
	    break;
        if (num_insns >= max_insns)
            break;
        if (singlestep)
            break;
    }
    if (tb->cflags & CF_LAST_IO)
        gen_io_end();
    if (env->singlestep_enabled) {
        tcg_gen_movi_i32(cpu_pc, ctx.pc);
        gen_helper_debug();
    } else {
	switch (ctx.bstate) {
        case BS_STOP:
            /* gen_op_interrupt_restart(); */
            /* fall through */
        case BS_NONE:
            if (ctx.flags) {
                gen_store_flags(ctx.flags | DELAY_SLOT_CLEARME);
	    }
            gen_goto_tb(&ctx, 0, ctx.pc);
            break;
        case BS_EXCP:
            /* gen_op_interrupt_restart(); */
            tcg_gen_exit_tb(0);
            break;
        case BS_BRANCH:
        default:
            break;
	}
    }

    gen_icount_end(tb, num_insns);
    *gen_opc_ptr = INDEX_op_end;
    if (search_pc) {
        i = gen_opc_ptr - gen_opc_buf;
        ii++;
        while (ii <= i)
            gen_opc_instr_start[ii++] = 0;
    } else {
        tb->size = ctx.pc - pc_start;
        tb->icount = num_insns;
    }

#ifdef DEBUG_DISAS
#ifdef SH4_DEBUG_DISAS
    qemu_log_mask(CPU_LOG_TB_IN_ASM, "\n");
#endif
    if (qemu_loglevel_mask(CPU_LOG_TB_IN_ASM)) {
	qemu_log("IN:\n");	/* , lookup_symbol(pc_start)); */
	log_target_disas(pc_start, ctx.pc - pc_start, 0);
	qemu_log("\n");
    }
#endif
}

void gen_intermediate_code(CPUState * env, struct TranslationBlock *tb)
{
    gen_intermediate_code_internal(env, tb, 0);
}

void gen_intermediate_code_pc(CPUState * env, struct TranslationBlock *tb)
{
    gen_intermediate_code_internal(env, tb, 1);
}

void gen_pc_load(CPUState *env, TranslationBlock *tb,
                unsigned long searched_pc, int pc_pos, void *puc)
{
    env->pc = gen_opc_pc[pc_pos];
    env->flags = gen_opc_hflags[pc_pos];
}
