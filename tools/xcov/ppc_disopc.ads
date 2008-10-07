------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------
pragma Restrictions (No_Elaboration_Code);

with Interfaces; use Interfaces;
with Ppc_Descs; use Ppc_Descs;

package Ppc_Disopc is
   
   type Ppc_Fields_Arr is array (0 .. 5) of Ppc_Fields;

   type Ppc_Insn_Descr is record
      Name : String (1 .. 7);
      Insn : Unsigned_32;
      Mask : Unsigned_32;
      Fields : Ppc_Fields_Arr;
   end record;

   Ppc_Insns : constant array (Natural range <>) of Ppc_Insn_Descr :=
     (
      ("twi    ",
       3 * S_OPC,
       16#fc000000#,
       (F_TO, F_A, F_SIMM, others => F_Eof)),
      ("mulli  ",
       7 * S_OPC,
       16#fc000000#,
       (F_D, F_A, F_SIMM, others => F_Eof)),
      ("subfic ",
       8 * S_OPC,
       16#fc000000#,
       (F_D, F_A, F_SIMM, others => F_Eof)),
      ("cmpli  ",
       10 * S_OPC,
       16#fc400000#,
       (F_L, F_CRFD, F_A, F_UIMM, others => F_Eof)),
      ("cmpi   ",
       11 * S_OPC,
       16#fc400000#,
       (F_L, F_CRFD, F_A, F_SIMM, others => F_Eof)),
      ("addic  ",
       12 * S_OPC,
       16#fc000000#,
       (F_D, F_A, F_SIMM, others => F_Eof)),
      ("addic. ",
       13 * S_OPC,
       16#fc000000#,
       (F_D, F_A, F_SIMM, others => F_Eof)),
      ("li     ",  -- Simplified mnemonic
       14 * S_OPC,
       16#fc1f0000#,
       (F_D, F_SIMM, others => F_Eof)),
      ("addi   ",
       14 * S_OPC,
       16#fc000000#,
       (F_D, F_A, F_SIMM, others => F_Eof)),
      ("lis    ",  -- Simplified mnemonic
       15 * S_OPC,
       16#fc1f0000#,
       (F_D, F_SIMM, others => F_Eof)),
      ("addis  ",
       15 * S_OPC,
       16#fc000000#,
       (F_D, F_A, F_SIMM, others => F_Eof)),
      ("blt    ",  -- Simplified mnemonic
       16 * S_OPC + 12 * S_BO + 0 * S_BI + 0 * S_LK,
       16#ffc30000#,
       (F_AA, F_LK, F_BR_HINT, F_CRFS, F_BD, others => F_Eof)),
      ("bgt    ",  -- Simplified mnemonic
       16 * S_OPC + 12 * S_BO + 1 * S_BI + 0 * S_LK,
       16#ffc30000#,
       (F_AA, F_LK, F_BR_HINT, F_CRFS, F_BD, others => F_Eof)),
      ("beq    ",  -- Simplified mnemonic
       16 * S_OPC + 12 * S_BO + 2 * S_BI + 0 * S_LK,
       16#ffc30000#,
       (F_AA, F_LK, F_BR_HINT, F_CRFS, F_BD, others => F_Eof)),
      ("bge    ",  -- Simplified mnemonic
       16 * S_OPC + 4 * S_BO + 0 * S_BI + 0 * S_LK,
       16#ffc30000#,
       (F_AA, F_LK, F_BR_HINT, F_CRFS, F_BD, others => F_Eof)),
      ("ble    ",  -- Simplified mnemonic
       16 * S_OPC + 4 * S_BO + 1 * S_BI + 0 * S_LK,
       16#ffc30000#,
       (F_AA, F_LK, F_BR_HINT, F_CRFS, F_BD, others => F_Eof)),
      ("bne    ",  -- Simplified mnemonic
       16 * S_OPC + 4 * S_BO + 2 * S_BI + 0 * S_LK,
       16#ffc30000#,
       (F_AA, F_LK, F_BR_HINT, F_CRFS, F_BD, others => F_Eof)),
      ("bc     ",
       16 * S_OPC,
       16#fc000000#,
       (F_AA, F_LK, F_BO, F_BI, F_BD, others => F_Eof)),
      ("sc     ",
       17 * S_OPC + 1 * S_XO,
       16#ffffffff#,
       (others => F_Eof)),
      ("b      ",
       18 * S_OPC,
       16#fc000000#,
       (F_AA, F_LK, F_LI, others => F_Eof)),
      ("mcrf   ",
       19 * S_OPC,
       16#fc63ffff#,
       (F_CRFD, F_CRFS, others => F_Eof)),
      ("blr    ",  -- Simplified mnemonic
       19 * S_OPC + 16 * S_XO + 20 * S_BO + 0 * S_BI + 0 * S_LK,
       16#ffffffff#,
       (others => F_Eof)),
      ("bclr   ",
       19 * S_OPC + 16 * S_XO,
       16#fc00fffe#,
       (F_LK, F_BO, F_BI, others => F_Eof)),
      ("crnor  ",
       19 * S_OPC + 33 * S_XO,
       16#fc0007ff#,
       (F_CRBD, F_CRBA, F_CRBB, others => F_Eof)),
      ("rfi    ",
       19 * S_OPC + 50 * S_XO,
       16#ffffffff#,
       (others => F_Eof)),
      ("crandc ",
       19 * S_OPC + 129 * S_XO,
       16#fc0007ff#,
       (F_CRBD, F_CRBA, F_CRBB, others => F_Eof)),
      ("isync  ",
       19 * S_OPC + 150 * S_XO,
       16#ffffffff#,
       (others => F_Eof)),
      ("crxor  ",
       19 * S_OPC + 193 * S_XO,
       16#fc0007ff#,
       (F_CRBD, F_CRBA, F_CRBB, others => F_Eof)),
      ("crnand ",
       19 * S_OPC + 225 * S_XO,
       16#fc0007ff#,
       (F_CRBD, F_CRBA, F_CRBB, others => F_Eof)),
      ("crand  ",
       19 * S_OPC + 257 * S_XO,
       16#fc0007ff#,
       (F_CRBD, F_CRBA, F_CRBB, others => F_Eof)),
      ("creqv  ",
       19 * S_OPC + 289 * S_XO,
       16#fc0007ff#,
       (F_CRBD, F_CRBA, F_CRBB, others => F_Eof)),
      ("crorc  ",
       19 * S_OPC + 417 * S_XO,
       16#fc0007ff#,
       (F_CRBD, F_CRBA, F_CRBB, others => F_Eof)),
      ("cror   ",
       19 * S_OPC + 449 * S_XO,
       16#fc0007ff#,
       (F_CRBD, F_CRBA, F_CRBB, others => F_Eof)),
      ("bctrl  ",  -- Simplified mnemonic
       19 * S_OPC + 528 * S_XO + 20 * S_BO + 0 * S_BI + 1 * S_LK,
       16#ffffffff#,
       (others => F_Eof)),
      ("bcctr  ",
       19 * S_OPC + 528 * S_XO,
       16#fc00fffe#,
       (F_LK, F_BO, F_BI, others => F_Eof)),
      ("rlwimi ",
       20 * S_OPC,
       16#fc000000#,
       (F_RC, F_A, F_S, F_SH, F_MB, F_ME, others => F_Eof)),
      ("clrlwi ",  -- Simplified mnemonic
       21 * S_OPC + 31 * S_XO,
       16#fc00f83e#,
       (F_RC, F_A, F_S, F_MB, others => F_Eof)),
      ("rlwinm ",
       21 * S_OPC,
       16#fc000000#,
       (F_RC, F_A, F_S, F_SH, F_MB, F_ME, others => F_Eof)),
      ("rlwnm  ",
       23 * S_OPC,
       16#fc000000#,
       (F_RC, F_A, F_S, F_B, F_MB, F_ME, others => F_Eof)),
      ("ori    ",
       24 * S_OPC,
       16#fc000000#,
       (F_A, F_S, F_UIMM, others => F_Eof)),
      ("oris   ",
       25 * S_OPC,
       16#fc000000#,
       (F_A, F_S, F_UIMM, others => F_Eof)),
      ("xori   ",
       26 * S_OPC,
       16#fc000000#,
       (F_A, F_S, F_UIMM, others => F_Eof)),
      ("xoris  ",
       27 * S_OPC,
       16#fc000000#,
       (F_A, F_S, F_UIMM, others => F_Eof)),
      ("andi.  ",
       28 * S_OPC,
       16#fc000000#,
       (F_A, F_S, F_UIMM, others => F_Eof)),
      ("andis. ",
       29 * S_OPC,
       16#fc000000#,
       (F_A, F_S, F_UIMM, others => F_Eof)),
      ("cmp    ",
       31 * S_OPC,
       16#fc4007ff#,
       (F_L, F_CRFD, F_A, F_B, others => F_Eof)),
      ("tw     ",
       31 * S_OPC + 4 * S_XO,
       16#fc0007ff#,
       (F_TO, F_A, F_B, others => F_Eof)),
      ("subfc  ",
       31 * S_OPC + 8 * S_XO,
       16#fc0003fe#,
       (F_OE, F_RC, F_D, F_A, F_B, others => F_Eof)),
      ("addc   ",
       31 * S_OPC + 10 * S_XO,
       16#fc0003fe#,
       (F_OE, F_RC, F_D, F_A, F_B, others => F_Eof)),
      ("mulhwu ",
       31 * S_OPC + 11 * S_XO,
       16#fc0007fe#,
       (F_RC, F_D, F_A, F_B, others => F_Eof)),
      ("mfcr   ",
       31 * S_OPC + 19 * S_XO,
       16#fc1fffff#,
       (F_D, others => F_Eof)),
      ("lwarx  ",
       31 * S_OPC + 20 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("lwzx   ",
       31 * S_OPC + 23 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("slw    ",
       31 * S_OPC + 24 * S_XO,
       16#fc0007fe#,
       (F_RC, F_A, F_S, F_B, others => F_Eof)),
      ("cntlzw ",
       31 * S_OPC + 26 * S_XO,
       16#fc00fffe#,
       (F_RC, F_A, F_S, others => F_Eof)),
      ("and    ",
       31 * S_OPC + 28 * S_XO,
       16#fc0007fe#,
       (F_RC, F_A, F_S, F_B, others => F_Eof)),
      ("cmpl   ",
       31 * S_OPC + 32 * S_XO,
       16#fc4007ff#,
       (F_L, F_CRFD, F_A, F_B, others => F_Eof)),
      ("subf   ",
       31 * S_OPC + 40 * S_XO,
       16#fc0003fe#,
       (F_OE, F_RC, F_D, F_A, F_B, others => F_Eof)),
      ("dcbst  ",
       31 * S_OPC + 54 * S_XO,
       16#ffe007ff#,
       (F_A, F_B, others => F_Eof)),
      ("lwzux  ",
       31 * S_OPC + 55 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("andc   ",
       31 * S_OPC + 60 * S_XO,
       16#fc0007fe#,
       (F_RC, F_A, F_S, F_B, others => F_Eof)),
      ("mulhw  ",
       31 * S_OPC + 75 * S_XO,
       16#fc0007fe#,
       (F_RC, F_D, F_A, F_B, others => F_Eof)),
      ("mfmsr  ",
       31 * S_OPC + 83 * S_XO,
       16#fc1fffff#,
       (F_D, others => F_Eof)),
      ("dcbf   ",
       31 * S_OPC + 86 * S_XO,
       16#ffe007ff#,
       (F_A, F_B, others => F_Eof)),
      ("lbzx   ",
       31 * S_OPC + 87 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("neg    ",
       31 * S_OPC + 104 * S_XO,
       16#fc00fbfe#,
       (F_OE, F_RC, F_D, F_A, others => F_Eof)),
      ("lbzux  ",
       31 * S_OPC + 119 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("nor    ",
       31 * S_OPC + 124 * S_XO,
       16#fc0007fe#,
       (F_RC, F_A, F_S, F_B, others => F_Eof)),
      ("subfe  ",
       31 * S_OPC + 136 * S_XO,
       16#fc0003fe#,
       (F_OE, F_RC, F_D, F_A, F_B, others => F_Eof)),
      ("adde   ",
       31 * S_OPC + 138 * S_XO,
       16#fc0003fe#,
       (F_OE, F_RC, F_D, F_A, F_B, others => F_Eof)),
      ("mtcrf  ",
       31 * S_OPC + 144 * S_XO,
       16#fc100fff#,
       (F_CRM, F_S, others => F_Eof)),
      ("mtmsr  ",
       31 * S_OPC + 146 * S_XO,
       16#fc1fffff#,
       (F_S, others => F_Eof)),
      ("stwcx. ",
       31 * S_OPC + 150 * S_XO + 1 * S_RC,
       16#fc0007ff#,
       (F_A, F_S, F_B, others => F_Eof)),
      ("stwx   ",
       31 * S_OPC + 151 * S_XO,
       16#fc0007ff#,
       (F_A, F_S, F_B, others => F_Eof)),
      ("stwux  ",
       31 * S_OPC + 183 * S_XO,
       16#fc0007ff#,
       (F_A, F_S, F_B, others => F_Eof)),
      ("subfze ",
       31 * S_OPC + 200 * S_XO,
       16#fc00fbfe#,
       (F_OE, F_RC, F_D, F_A, others => F_Eof)),
      ("addze  ",
       31 * S_OPC + 202 * S_XO,
       16#fc00fbfe#,
       (F_OE, F_RC, F_D, F_A, others => F_Eof)),
      ("mtsr   ",
       31 * S_OPC + 210 * S_XO,
       16#fc10ffff#,
       (F_SR, F_S, others => F_Eof)),
      ("stbx   ",
       31 * S_OPC + 215 * S_XO,
       16#fc0007ff#,
       (F_S, F_A, F_B, others => F_Eof)),
      ("subfme ",
       31 * S_OPC + 232 * S_XO,
       16#fc00fbfe#,
       (F_OE, F_RC, F_D, F_A, others => F_Eof)),
      ("addme  ",
       31 * S_OPC + 234 * S_XO,
       16#fc00fbfe#,
       (F_OE, F_RC, F_D, F_A, others => F_Eof)),
      ("mullw  ",
       31 * S_OPC + 235 * S_XO,
       16#fc0003fe#,
       (F_OE, F_RC, F_D, F_A, F_B, others => F_Eof)),
      ("mtsrin ",
       31 * S_OPC + 242 * S_XO,
       16#fc1f07ff#,
       (F_S, F_B, others => F_Eof)),
      ("dcbtst ",
       31 * S_OPC + 246 * S_XO,
       16#ffe007ff#,
       (F_A, F_B, others => F_Eof)),
      ("stbux  ",
       31 * S_OPC + 247 * S_XO,
       16#fc0007ff#,
       (F_S, F_A, F_B, others => F_Eof)),
      ("add    ",
       31 * S_OPC + 266 * S_XO,
       16#fc0003fe#,
       (F_OE, F_RC, F_D, F_A, F_B, others => F_Eof)),
      ("dcbt   ",
       31 * S_OPC + 278 * S_XO,
       16#ffe007ff#,
       (F_A, F_B, others => F_Eof)),
      ("lhzx   ",
       31 * S_OPC + 279 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("eqv    ",
       31 * S_OPC + 284 * S_XO,
       16#fc0007fe#,
       (F_RC, F_A, F_S, F_B, others => F_Eof)),
      ("tlbie  ",
       31 * S_OPC + 306 * S_XO,
       16#ffff07ff#,
       (F_B, others => F_Eof)),
      ("eciwx  ",
       31 * S_OPC + 310 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("lhzux  ",
       31 * S_OPC + 311 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("xor    ",
       31 * S_OPC + 316 * S_XO,
       16#fc0007fe#,
       (F_RC, F_A, F_S, F_B, others => F_Eof)),
      ("mflr   ",  -- Simplified mnemonic
       31 * S_OPC + 339 * S_XO + 256 * S_SPR,
       16#fc1fffff#,
       (F_D, others => F_Eof)),
      ("mfctr  ",  -- Simplified mnemonic
       31 * S_OPC + 339 * S_XO + 288 * S_SPR,
       16#fc1fffff#,
       (F_D, others => F_Eof)),
      ("mfspr  ",
       31 * S_OPC + 339 * S_XO,
       16#fc0007ff#,
       (F_D, F_SPR, others => F_Eof)),
      ("lhax   ",
       31 * S_OPC + 343 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("tlbia  ",
       31 * S_OPC + 370 * S_XO,
       16#ffffffff#,
       (others => F_Eof)),
      ("mftb   ",
       31 * S_OPC + 371 * S_XO,
       16#fc0007ff#,
       (F_D, F_TBR, others => F_Eof)),
      ("lhaux  ",
       31 * S_OPC + 375 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("sthx   ",
       31 * S_OPC + 407 * S_XO,
       16#fc0007ff#,
       (F_S, F_A, F_B, others => F_Eof)),
      ("orc    ",
       31 * S_OPC + 412 * S_XO,
       16#fc0007fe#,
       (F_RC, F_A, F_S, F_B, others => F_Eof)),
      ("ecowx  ",
       31 * S_OPC + 438 * S_XO,
       16#fc0007ff#,
       (F_S, F_A, F_B, others => F_Eof)),
      ("sthux  ",
       31 * S_OPC + 439 * S_XO,
       16#fc0007ff#,
       (F_S, F_A, F_B, others => F_Eof)),
      ("or     ",
       31 * S_OPC + 444 * S_XO,
       16#fc0007fe#,
       (F_RC, F_A, F_S, F_B, others => F_Eof)),
      ("divwu  ",
       31 * S_OPC + 459 * S_XO,
       16#fc0003fe#,
       (F_OE, F_RC, F_D, F_A, F_B, others => F_Eof)),
      ("mtlr   ",  -- Simplified mnemonic
       31 * S_OPC + 467 * S_XO + 256 * S_SPR,
       16#fc1fffff#,
       (F_S, others => F_Eof)),
      ("mtctr  ",  -- Simplified mnemonic
       31 * S_OPC + 467 * S_XO + 288 * S_SPR,
       16#fc1fffff#,
       (F_S, others => F_Eof)),
      ("mtspr  ",
       31 * S_OPC + 467 * S_XO,
       16#fc0007ff#,
       (F_SPR, F_S, others => F_Eof)),
      ("dcbi   ",
       31 * S_OPC + 470 * S_XO,
       16#ffe007ff#,
       (F_A, F_B, others => F_Eof)),
      ("nandx  ",
       31 * S_OPC + 476 * S_XO,
       16#fc0007fe#,
       (F_RC, F_S, F_A, F_B, others => F_Eof)),
      ("divw   ",
       31 * S_OPC + 491 * S_XO,
       16#fc0003fe#,
       (F_OE, F_RC, F_D, F_A, F_B, others => F_Eof)),
      ("mcrxr  ",
       31 * S_OPC + 512 * S_XO,
       16#fc7fffff#,
       (F_CRFD, others => F_Eof)),
      ("lswx   ",
       31 * S_OPC + 533 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("lwbrx  ",
       31 * S_OPC + 534 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("lfsx   ",
       31 * S_OPC + 535 * S_XO,
       16#fc0007ff#,
       (F_FD, F_A, F_B, others => F_Eof)),
      ("srw    ",
       31 * S_OPC + 536 * S_XO,
       16#fc0007fe#,
       (F_RC, F_A, F_S, F_B, others => F_Eof)),
      ("tlbsync",
       31 * S_OPC + 566 * S_XO,
       16#ffffffff#,
       (others => F_Eof)),
      ("lfsux  ",
       31 * S_OPC + 567 * S_XO,
       16#fc0007ff#,
       (F_FD, F_A, F_B, others => F_Eof)),
      ("mfsr   ",
       31 * S_OPC + 595 * S_XO,
       16#fc10ffff#,
       (F_D, F_SR, others => F_Eof)),
      ("lswi   ",
       31 * S_OPC + 597 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_NB, others => F_Eof)),
      ("sync   ",
       31 * S_OPC + 598 * S_XO,
       16#ffffffff#,
       (others => F_Eof)),
      ("lfdx   ",
       31 * S_OPC + 599 * S_XO,
       16#fc0007ff#,
       (F_FD, F_A, F_B, others => F_Eof)),
      ("lfdux  ",
       31 * S_OPC + 631 * S_XO,
       16#fc0007ff#,
       (F_FD, F_A, F_B, others => F_Eof)),
      ("mfsrin ",
       31 * S_OPC + 659 * S_XO,
       16#fc1f07ff#,
       (F_D, F_B, others => F_Eof)),
      ("stswx  ",
       31 * S_OPC + 661 * S_XO,
       16#fc0007ff#,
       (F_S, F_A, F_B, others => F_Eof)),
      ("stwbrx ",
       31 * S_OPC + 662 * S_XO,
       16#fc0007ff#,
       (F_S, F_A, F_B, others => F_Eof)),
      ("stfsx  ",
       31 * S_OPC + 663 * S_XO,
       16#fc0007ff#,
       (F_FS, F_A, F_B, others => F_Eof)),
      ("stfsux ",
       31 * S_OPC + 695 * S_XO,
       16#fc0007ff#,
       (F_FS, F_A, F_B, others => F_Eof)),
      ("stswi  ",
       31 * S_OPC + 725 * S_XO,
       16#fc0007ff#,
       (F_S, F_A, F_NB, others => F_Eof)),
      ("stfdx  ",
       31 * S_OPC + 727 * S_XO,
       16#fc0007ff#,
       (F_FS, F_A, F_B, others => F_Eof)),
      ("dcba   ",
       31 * S_OPC + 758 * S_XO,
       16#ffe007ff#,
       (F_A, F_B, others => F_Eof)),
      ("stfdux ",
       31 * S_OPC + 759 * S_XO,
       16#fc0007ff#,
       (F_FS, F_A, F_B, others => F_Eof)),
      ("lhbrx  ",
       31 * S_OPC + 790 * S_XO,
       16#fc0007ff#,
       (F_D, F_A, F_B, others => F_Eof)),
      ("sraw   ",
       31 * S_OPC + 792 * S_XO,
       16#fc0007fe#,
       (F_RC, F_A, F_S, F_B, others => F_Eof)),
      ("srawi  ",
       31 * S_OPC + 824 * S_XO,
       16#fc0007fe#,
       (F_RC, F_A, F_S, F_SH, others => F_Eof)),
      ("eieio  ",
       31 * S_OPC + 854 * S_XO,
       16#ffffffff#,
       (others => F_Eof)),
      ("sthbrx ",
       31 * S_OPC + 918 * S_XO,
       16#fc0007ff#,
       (F_S, F_A, F_B, others => F_Eof)),
      ("extsh  ",
       31 * S_OPC + 922 * S_XO,
       16#fc00fffe#,
       (F_RC, F_A, F_S, others => F_Eof)),
      ("extsb  ",
       31 * S_OPC + 954 * S_XO,
       16#fc00ffff#,
       (F_A, F_S, others => F_Eof)),
      ("icbi   ",
       31 * S_OPC + 982 * S_XO,
       16#ffe007ff#,
       (F_A, F_B, others => F_Eof)),
      ("stfiwx ",
       31 * S_OPC + 983 * S_XO,
       16#fc0007ff#,
       (F_FS, F_A, F_B, others => F_Eof)),
      ("dcbz   ",
       31 * S_OPC + 1014 * S_XO,
       16#ffe007ff#,
       (F_A, F_B, others => F_Eof)),
      ("lwz    ",
       32 * S_OPC,
       16#f8000000#,
       (F_U, F_D, F_DISP, F_A, others => F_Eof)),
      ("lbz    ",
       34 * S_OPC,
       16#f8000000#,
       (F_U, F_D, F_DISP, F_A, others => F_Eof)),
      ("stw    ",
       36 * S_OPC,
       16#f8000000#,
       (F_U, F_S, F_DISP, F_A, others => F_Eof)),
      ("stb    ",
       38 * S_OPC,
       16#f8000000#,
       (F_U, F_S, F_DISP, F_A, others => F_Eof)),
      ("lhz    ",
       40 * S_OPC,
       16#f8000000#,
       (F_U, F_D, F_DISP, F_A, others => F_Eof)),
      ("lha    ",
       42 * S_OPC,
       16#f8000000#,
       (F_U, F_D, F_DISP, F_A, others => F_Eof)),
      ("sth    ",
       44 * S_OPC,
       16#f8000000#,
       (F_U, F_S, F_DISP, F_A, others => F_Eof)),
      ("lmw    ",
       46 * S_OPC,
       16#fc000000#,
       (F_D, F_DISP, F_A, others => F_Eof)),
      ("stmw   ",
       47 * S_OPC,
       16#fc000000#,
       (F_S, F_DISP, F_A, others => F_Eof)),
      ("lfs    ",
       48 * S_OPC,
       16#f8000000#,
       (F_U, F_FD, F_DISP, F_A, others => F_Eof)),
      ("lfd    ",
       50 * S_OPC,
       16#f8000000#,
       (F_U, F_FD, F_DISP, F_A, others => F_Eof)),
      ("stfs   ",
       52 * S_OPC,
       16#f8000000#,
       (F_U, F_FS, F_DISP, F_A, others => F_Eof)),
      ("stfd   ",
       54 * S_OPC,
       16#f8000000#,
       (F_U, F_FS, F_DISP, F_A, others => F_Eof)),
      ("fdivs  ",
       59 * S_OPC + 18 * S_XO,
       16#fc0007fe#,
       (F_RC, F_FD, F_FA, F_FB, others => F_Eof)),
      ("fsubs  ",
       59 * S_OPC + 20 * S_XO,
       16#fc0007fe#,
       (F_RC, F_FD, F_FA, F_FB, others => F_Eof)),
      ("fadds  ",
       59 * S_OPC + 21 * S_XO,
       16#fc0007fe#,
       (F_RC, F_FD, F_FA, F_FB, others => F_Eof)),
      ("fsqrts ",
       59 * S_OPC + 22 * S_XO,
       16#fc1f07fe#,
       (F_RC, F_FD, F_FB, others => F_Eof)),
      ("fres   ",
       59 * S_OPC + 24 * S_XO,
       16#fc1f07fe#,
       (F_RC, F_FD, F_FB, others => F_Eof)),
      ("fmuls  ",
       59 * S_OPC + 25 * S_XO,
       16#fc00f83e#,
       (F_RC, F_FD, F_FA, F_FC, others => F_Eof)),
      ("fmsubs ",
       59 * S_OPC + 28 * S_XO,
       16#fc00003e#,
       (F_RC, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      ("fmadds ",
       59 * S_OPC + 29 * S_XO,
       16#fc00003e#,
       (F_RC, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      ("fnmsubs",
       59 * S_OPC + 30 * S_XO,
       16#fc00003e#,
       (F_RC, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      ("fnmadds",
       59 * S_OPC + 31 * S_XO,
       16#fc00003e#,
       (F_RC, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      ("fcmpu  ",
       63 * S_OPC,
       16#fc6007ff#,
       (F_CRFD, F_FA, F_FB, others => F_Eof)),
      ("frsp   ",
       63 * S_OPC + 12 * S_XO,
       16#fc1f07fe#,
       (F_RC, F_FD, F_FB, others => F_Eof)),
      ("fctiw  ",
       63 * S_OPC + 14 * S_XO,
       16#fc1f07fe#,
       (F_RC, F_FD, F_FB, others => F_Eof)),
      ("fctiwz ",
       63 * S_OPC + 15 * S_XO,
       16#fc1f07fe#,
       (F_RC, F_FD, F_FB, others => F_Eof)),
      ("fdiv   ",
       63 * S_OPC + 18 * S_XO,
       16#fc0007fe#,
       (F_RC, F_FD, F_FA, F_FB, others => F_Eof)),
      ("fsub   ",
       63 * S_OPC + 20 * S_XO,
       16#fc0007fe#,
       (F_RC, F_FD, F_FA, F_FB, others => F_Eof)),
      ("fadd   ",
       63 * S_OPC + 21 * S_XO,
       16#fc0007fe#,
       (F_RC, F_FD, F_FA, F_FB, others => F_Eof)),
      ("fsqrt  ",
       63 * S_OPC + 22 * S_XO,
       16#fc1f07fe#,
       (F_RC, F_FD, F_FB, others => F_Eof)),
      ("fsel   ",
       63 * S_OPC + 23 * S_XO,
       16#fc00003e#,
       (F_RC, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      ("fmul   ",
       63 * S_OPC + 25 * S_XO,
       16#fc00f83e#,
       (F_RC, F_FD, F_FA, F_FC, others => F_Eof)),
      ("frsqrte",
       63 * S_OPC + 26 * S_XO,
       16#fc1f07fe#,
       (F_RC, F_FD, F_FB, others => F_Eof)),
      ("fmsub  ",
       63 * S_OPC + 28 * S_XO,
       16#fc00003e#,
       (F_RC, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      ("fmadd  ",
       63 * S_OPC + 29 * S_XO,
       16#fc00003e#,
       (F_RC, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      ("fnmsub ",
       63 * S_OPC + 30 * S_XO,
       16#fc00003e#,
       (F_RC, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      ("fnmadd ",
       63 * S_OPC + 31 * S_XO,
       16#fc00003e#,
       (F_RC, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      ("fcmpo  ",
       63 * S_OPC + 32 * S_XO,
       16#fc6007ff#,
       (F_CRFD, F_FA, F_FB, others => F_Eof)),
      ("mtfsb1 ",
       63 * S_OPC + 38 * S_XO,
       16#fc1ffffe#,
       (F_RC, F_CRBD, others => F_Eof)),
      ("fneg   ",
       63 * S_OPC + 40 * S_XO,
       16#fc1f07fe#,
       (F_RC, F_FD, F_FB, others => F_Eof)),
      ("mcrfs  ",
       63 * S_OPC + 64 * S_XO,
       16#fc63ffff#,
       (F_CRFD, F_CRFS, others => F_Eof)),
      ("mtfsb0 ",
       63 * S_OPC + 70 * S_XO,
       16#fc1ffffe#,
       (F_RC, F_CRBD, others => F_Eof)),
      ("fmr    ",
       63 * S_OPC + 72 * S_XO,
       16#fc1f07fe#,
       (F_RC, F_FD, F_FB, others => F_Eof)),
      ("mtfsfi ",
       63 * S_OPC + 134 * S_XO,
       16#fc7f0ffe#,
       (F_RC, F_CRFD, F_IMM, others => F_Eof)),
      ("fnabs  ",
       63 * S_OPC + 136 * S_XO,
       16#fc1f07fe#,
       (F_RC, F_FD, F_FB, others => F_Eof)),
      ("fabs   ",
       63 * S_OPC + 264 * S_XO,
       16#fc1f07fe#,
       (F_RC, F_FD, F_FB, others => F_Eof)),
      ("mffs   ",
       63 * S_OPC + 583 * S_XO,
       16#fc1ffffe#,
       (F_RC, F_D, others => F_Eof)),
      ("mtfsf  ",
       63 * S_OPC + 711 * S_XO,
       16#fe0107fe#,
       (F_RC, F_FM, F_FB, others => F_Eof))
     );

   Ppc_Opc_Index : constant array (0 .. 64) of Integer :=
     (
      0 =>  0,
      1 =>  0,
      2 =>  0,
      3 =>  0,
      4 =>  1,
      5 =>  1,
      6 =>  1,
      7 =>  1,
      8 =>  2,
      9 =>  3,
      10 =>  3,
      11 =>  4,
      12 =>  5,
      13 =>  6,
      14 =>  7,
      15 =>  9,
      16 =>  11,
      17 =>  18,
      18 =>  19,
      19 =>  20,
      20 =>  35,
      21 =>  36,
      22 =>  38,
      23 =>  38,
      24 =>  39,
      25 =>  40,
      26 =>  41,
      27 =>  42,
      28 =>  43,
      29 =>  44,
      30 =>  45,
      31 =>  45,
      32 =>  143,
      33 =>  143,
      34 =>  144,
      35 =>  144,
      36 =>  145,
      37 =>  145,
      38 =>  146,
      39 =>  146,
      40 =>  147,
      41 =>  147,
      42 =>  148,
      43 =>  148,
      44 =>  149,
      45 =>  149,
      46 =>  150,
      47 =>  151,
      48 =>  152,
      49 =>  152,
      50 =>  153,
      51 =>  153,
      52 =>  154,
      53 =>  154,
      54 =>  155,
      55 =>  156,
      56 =>  156,
      57 =>  156,
      58 =>  156,
      59 =>  156,
      60 =>  166,
      61 =>  166,
      62 =>  166,
      63 =>  166,
      64 =>  192
     );

end Ppc_Disopc;
