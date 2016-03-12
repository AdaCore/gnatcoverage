
/*--------------------------------------------------------------------*/
/*--- Coverage: Execution traces for GNATcoverage.      cov_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   Copyright (C) 2013-2016, AdaCore

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_tool_basics.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_libcprint.h"  /* VG_(printf) */
#include "pub_tool_libcassert.h" /* tl_assert */
#include "pub_tool_machine.h"    /* VG_(fnptr_to_fnentry) */
#include "pub_tool_options.h"    /* command line options */
#include "pub_tool_libcbase.h"   /* VG_(strlen) */

#include "libvex_basictypes.h"
#include "libvex.h"

#include "cov_traces.h"

/* filenames */
static UChar *clo_cov_exec_file=NULL;

//#define DEBUG
static VG_REGPARM(2) void per_branch(struct trace_entry *te, Word taken)
{
    if (tracefile_history_for_te(te) && (te->op & 0x3) != 0) {
        /* If we save history for this "te" and there's already a branch state
         * is already set, create a new "te".
         */
        te = new_trace_entry(te->pc, te->size);
        te->op |= TRACE_OP_HIST_SET;
    }

    te->op |= TRACE_OP_BLOCK | (taken ? TRACE_OP_BR0 : TRACE_OP_BR1);

#ifdef DEBUG
    VG_(printf)("Branch (0x%08x:%u) cond: %d\n", (unsigned int)te->pc,
                (unsigned int)te->size, (int)taken);
#endif
}

/* From libvex pre 3.11.  */

#if VEX_HOST_WORDSIZE == 8
   static inline void* ULong_to_Ptr ( ULong n ) {
      return (void*)n;
   }
#elif VEX_HOST_WORDSIZE == 4
   static inline void* ULong_to_Ptr ( ULong n ) {
      UInt w = (UInt)n;
      return (void*)w;
   }
#endif

/* From Callgrind */

static Addr IRConst2Addr(IRConst* con)
{
    Addr addr;

    if (sizeof(Addr) == 4) {
        tl_assert(con->tag == Ico_U32);
        addr = con->Ico.U32;
    } else if (sizeof(Addr) == 8) {
        tl_assert(con->tag == Ico_U64);
        addr = con->Ico.U64;
    } else {
        VG_(tool_panic)("Coverage: invalid Addr type");
    }

    return addr;
}

static IRSB* cov_instrument (VgCallbackClosure* closure,
                             IRSB*              sbIn,
                             VexGuestLayout*    layout,
                             VexGuestExtents*   vge,
                             IRType             gWordTy,
                             IRType             hWordTy)
{
    Int                  i;
    IRStmt*              st;
    IRSB                *sbOut;
    Addr64               origAddr;
    IRDirty             *di;
    IRExpr             **argv, *arg1, *arg2;
    Int                  regparms;
    uint16_t             te_size     = 0;
    struct trace_entry  *te;
    Addr                 instrAddr, jumpDst;
    UInt                 instrLen;
    Bool                 toNextInstr = False;
    Bool                 has_branch = False;


#ifdef DEBUG
    VG_(printf)("Coverage: BEFORE\n");
    for (i = 0; i <  sbIn->stmts_used; i++) {
        st = sbIn->stmts[i];
        if (st) {
            ppIRStmt(st);
            VG_(printf)("\n");
        }
    }
#endif

    /* Set up SB */
    sbOut = deepCopyIRSBExceptStmts(sbIn);

    /* Find first IMark */
    for (i = 0; (i < sbIn->stmts_used) && (sbIn->stmts[i]->tag != Ist_IMark);
         i++) {
        continue;
    }

    /* Get the first statement */
    tl_assert(sbIn->stmts_used > 0);
    st = sbIn->stmts[i];

    /* Double check we are at a Mark statement */
    tl_assert(Ist_IMark == st->tag);

    /* Save address of the first instruction */
    origAddr = st->Ist.IMark.addr;

    /* Find last IMark */
    for (; i <  sbIn->stmts_used; i++) {
        if (sbIn->stmts[i] != NULL && sbIn->stmts[i]->tag == Ist_IMark) {
            st = sbIn->stmts[i];
        }
    }

    /* Double check we are at a Mark statement */
    tl_assert(Ist_IMark == st->tag);

    /* Compute BB size */
    /* META: instruction mark.  Marks the start of the statements
       that represent a single machine instruction (the end of
       those statements is marked by the next IMark or the end of
       the IRSB).  Contains the address and length of the
       instruction.

       It also contains a delta value.  The delta must be
       subtracted from a guest program counter value before
       attempting to establish, by comparison with the address
       and length values, whether or not that program counter
       value refers to this instruction.  For x86, amd64, ppc32,
       ppc64 and arm, the delta value is zero.  For Thumb
       instructions, the delta value is one.  This is because, on
       Thumb, guest PC values (guest_R15T) are encoded using the
       top 31 bits of the instruction address and a 1 in the lsb;
       hence they appear to be (numerically) 1 past the start of
       the instruction they refer to.  IOW, guest_R15T on ARM
       holds a standard ARM interworking address.

       ppIRStmt output: ------ IMark(<addr>, <len>, <delta>) ------,
       eg. ------ IMark(0x4000792, 5, 0) ------,
    */

    /* I don't know how to get the size of the super block, so I compute
     * it from the address of the last instruction (i.e. the last
     * IMark) and its size.
     */
    te_size = st->Ist.IMark.addr + st->Ist.IMark.len
        - st->Ist.IMark.delta - origAddr;


    /* Save addr and len of the last IMark for branch inversion
     * heuristic.
     */
    instrAddr = (Addr)ULong_to_Ptr(st->Ist.IMark.addr);
    instrLen  = st->Ist.IMark.len;

#ifdef DEBUG
    VG_(printf)("\nStart SB addr: %llx size:%d...\n", origAddr, te_size);
#endif

    /* Get trace entry for this BB */
    te = get_trace_entry(origAddr, te_size);
    tl_assert(te != NULL);

    for (i = 0; i <  sbIn->stmts_used; i++) {
        st = sbIn->stmts[i];
        if (!st) {
            continue;
        }

        switch (st->tag) {
        case Ist_Exit: {
            Bool guest_exit, inverted = False;

            /* Add instrumentation only if this is a branch in guest code */
            guest_exit = (st->Ist.Exit.jk == Ijk_Boring) ||
                (st->Ist.Exit.jk == Ijk_Call) ||
                (st->Ist.Exit.jk == Ijk_Ret);

            if (guest_exit) {

                if (has_branch) {
#ifdef DEBUG
                    int index;
                    VG_(printf)("Coverage: Not a basic block\n");
                    for (index = 0; index <  sbIn->stmts_used; index++) {
                        st = sbIn->stmts[index];
                        if (st) {
                            ppIRStmt(st);
                            VG_(printf)("\n");
                        }
                    }
#endif

                    VG_(tool_panic)("Coverage: More than one branch in this"
                                    " block (not a basic block)");
                }

                /* FIXME: This case has to be fixed as coverage analysis
                 * needs basic block, we just ignore it for now.
                 */
                /* has_branch = True; */

                /* Stuff to widen the guard expression to a host word, so
                 * we can pass it to the branch coverage function easily.
                 */
                IRType   tyW    = hWordTy;
                IROp     widen  = tyW==Ity_I32  ? Iop_1Uto32  : Iop_1Uto64;
                IROp     opXOR  = tyW==Ity_I32  ? Iop_Xor32   : Iop_Xor64;
                IRTemp   guard1 = newIRTemp(sbOut->tyenv, Ity_I1);
                IRTemp   guardW = newIRTemp(sbOut->tyenv, tyW);
                IRTemp   guard  = newIRTemp(sbOut->tyenv, tyW);
                IRExpr*  one    = tyW==Ity_I32 ? IRExpr_Const(IRConst_U32(1))
                    : IRExpr_Const(IRConst_U64(1));

                /* Widen the guard expression. */
                addStmtToIRSB(sbOut,
                              IRStmt_WrTmp(guard1, st->Ist.Exit.guard ));
                addStmtToIRSB(sbOut,
                              IRStmt_WrTmp(guardW,
                                           IRExpr_Unop(widen,
                                                       IRExpr_RdTmp(guard1))));


                /* VEX code generation sometimes inverts conditional branches.
                 * Coverage has to correct inversions. The heuristic is the
                 * following:
                 *  Inversion is assumed if the branch jumps to the address of
                 *  the next guest instruction in memory.
                 */

                /* We assume that there is only one jump in each SB */
                jumpDst = IRConst2Addr(st->Ist.Exit.dst);
                toNextInstr = (jumpDst == instrAddr + instrLen);

                /* if the last instructions of BB conditionally jumps to next
                 * instruction (= first instruction of next BB in memory), this
                 * is inverted by VEX.
                 */
                inverted = toNextInstr;

                /* If the exit is inverted, invert the sense of the guard. */
                addStmtToIRSB(
                    sbOut,
                    IRStmt_WrTmp(
                        guard,
                        inverted ? IRExpr_Binop(opXOR, IRExpr_RdTmp(guardW), one)
                                 : IRExpr_RdTmp(guardW)));

                regparms = 2;
                arg1 = mkIRExpr_HWord( (HWord)te);
                arg2 = IRExpr_RdTmp(guard);
                argv = mkIRExprVec_2(arg1, arg2);

                di = unsafeIRDirty_0_N(regparms, "per_branch",
                                       VG_(fnptr_to_fnentry)(&per_branch),
                                       argv);

                /* Insert our call */
                addStmtToIRSB(sbOut, IRStmt_Dirty(di));
            }
            break;
        }

        default:
            break;
        }

        /* Always keep the original statement */
        addStmtToIRSB(sbOut, st);

    }

    /* If the block is instrumented we will at least execute it */
    te->op |= TRACE_OP_BLOCK;

#ifdef DEBUG
    VG_(printf)("Coverage: AFTER\n");
    for (i = 0; i < sbOut->stmts_used; i++) {
        st = sbOut->stmts[i];
        if (st) {
            ppIRStmt(st);
            VG_(printf)("\n");
        }
    }
    VG_(printf)("Coverage: END\n");
#endif
    return sbOut;
}

/*--------------------------------------------------------------------*/
/*--- Setup                                                        ---*/
/*--------------------------------------------------------------------*/

static void cov_post_clo_init(void)
{
    /* Try a closer approximation of basic blocks  */
    /* This is the same as the command line option */
    /* --vex-guest-chase-thresh=0                  */
    VG_(clo_vex_control).guest_chase_thresh = 0;

    trace_init(clo_cov_exec_file);
}

static Bool cov_process_cmd_line_option(Char* arg)
{
   if VG_STR_CLO (arg, "--cov-exec-file", clo_cov_exec_file) {}
   else {
      return False;
   }

   return True;
}

static void cov_print_usage(void)
{
   VG_(printf)(
"   --cov-exec-file=<file>    filename for execution trace file (and options)\n"
   );
}

static void cov_print_debug_usage(void)
{
   VG_(printf)("    (none)\n");
}

static void cov_fini(Int exitcode)
{
    trace_cleanup();
}

static void cov_pre_clo_init(void)
{
    VG_(details_name)            ("Coverage");
    VG_(details_version)         ("1.0.0w");
    VG_(details_description)     ("Execution traces for GNATcoverage");
    VG_(details_copyright_author)("Copyright (C) 2012, AdaCore");
    VG_(details_bug_reports_to)  ("report@adacore.com");

    VG_(details_avg_translation_sizeB) ( 275 );

    VG_(basic_tool_funcs)        (cov_post_clo_init,
                                  cov_instrument,
                                  cov_fini);

    VG_(needs_command_line_options)(cov_process_cmd_line_option,
                                    cov_print_usage,
                                    cov_print_debug_usage);
    /* No needs, no core events to track */
}

VG_DETERMINE_INTERFACE_VERSION(cov_pre_clo_init)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
