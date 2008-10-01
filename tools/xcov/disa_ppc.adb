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
with System; use System;
--with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with System.Storage_Elements;
with Hex_Images; use Hex_Images;

package body Disa_Ppc is
   function Get_Insn_Length (Addr : System.Address) return Positive
   is
      pragma Unreferenced (Addr);
   begin
      return 4;
   end Get_Insn_Length;

   function Read_Byte (Addr : Address) return Unsigned_8
   is
      type Unsigned_8_Acc is access all Unsigned_8;
      function To_Unsigned_8_Acc is new Ada.Unchecked_Conversion
        (Address, Unsigned_8_Acc);
   begin
      return To_Unsigned_8_Acc (Addr).all;
   end Read_Byte;

   function Get_Insn (Addr : Address) return Unsigned_32
   is
      use System.Storage_Elements;
      B0, B1, B2, B3 : Unsigned_8;
   begin
      B0 := Read_Byte (Addr + 0);
      B1 := Read_Byte (Addr + 1);
      B2 := Read_Byte (Addr + 2);
      B3 := Read_Byte (Addr + 3);
      return Shift_Left (Unsigned_32 (B0), 24)
        or Shift_Left (Unsigned_32 (B1), 16)
        or Shift_Left (Unsigned_32 (B2), 8)
        or Shift_Left (Unsigned_32 (B3), 0);
   end Get_Insn;

   subtype S is String;

   type Ppc_Insns_Descs is array (Natural range <>) of Ppc_Insn_Descr;

   Ppc_Insns : constant Ppc_Insns_Descs :=
     (
      (new S'("twi"),
       2#000011_00000_00000_0000000000000000#,
       (F_TO, F_A, F_SIMM, others => F_Eof)),
      (new S'("mulli"),
       2#000111_00000_00000_0000000000000000#,
       (F_D, F_A, F_SIMM, others => F_Eof)),
      (new S'("subfic"),
       2#001000_00000_00000_0000000000000000#,
       (F_D, F_A, F_SIMM, others => F_Eof)),
      (new S'("cmpli"),
       2#001010_00000_00000_0000000000000000#,
       (F_L, F_CrfD, F_A, F_UIMM, others => F_Eof)),
      (new S'("cmpi"),
       2#001011_00000_00000_0000000000000000#,
       (F_L, F_CrfD, F_A, F_SIMM, others => F_Eof)),
      (new S'("addic"),
       2#001100_00000_00000_0000000000000000#,
       (F_D, F_A, F_SIMM, others => F_Eof)),
      (new S'("addic."),
       2#001101_00000_00000_0000000000000000#,
       (F_D, F_A, F_SIMM, others => F_Eof)),
      (new S'("li"), -- Simplified mnemonic
       2#001110_00000_00000_0000000000000000#,
       (F_D, F_SIMM, others => F_Eof)),
      (new S'("addi"),
       2#001110_00000_00000_0000000000000000#,
       (F_D, F_A, F_SIMM, others => F_Eof)),
      (new S'("lis"), -- Simplified mnemonic
       2#001111_00000_00000_0000000000000000#,
       (F_D, F_SIMM, others => F_Eof)),
      (new S'("addis"),
       2#001111_00000_00000_0000000000000000#,
       (F_D, F_A, F_SIMM, others => F_Eof)),
      (new S'("blt"), -- Simplified mnemonic
       2#010000_01100_00000_0000000000000000#,
       (F_AA, F_LK, F_Br_Hint, F_CrfS, F_BD, others => F_Eof)),
      (new S'("bgt"), -- Simplified mnemonic
       2#010000_01100_00001_0000000000000000#,
       (F_AA, F_LK, F_Br_Hint, F_CrfS, F_BD, others => F_Eof)),
      (new S'("beq"), -- Simplified mnemonic
       2#010000_01100_00010_0000000000000000#,
       (F_AA, F_LK, F_Br_Hint, F_CrfS, F_BD, others => F_Eof)),
      (new S'("bge"), -- Simplified mnemonic
       2#010000_00100_00000_0000000000000000#,
       (F_AA, F_LK, F_Br_Hint, F_CrfS, F_BD, others => F_Eof)),
      (new S'("ble"), -- Simplified mnemonic
       2#010000_00100_00001_0000000000000000#,
       (F_AA, F_LK, F_Br_Hint, F_CrfS, F_BD, others => F_Eof)),
      (new S'("bne"), -- Simplified mnemonic
       2#010000_00100_00010_0000000000000000#,
       (F_AA, F_LK, F_Br_Hint, F_CrfS, F_BD, others => F_Eof)),
      (new S'("bc"),
       2#010000_00000_00000_0000000000000000#,
       (F_AA, F_LK, F_BO, F_BI, F_BD, others => F_Eof)),
      (new S'("sc"),
       2#010001_00000_00000_00000000000000_10#,
       (others => F_Eof)),
      (new S'("b"),
       2#010010_00000_00000_0000000000000000#,
       (F_AA, F_LK, F_Li, Others => F_Eof)),
      (new S'("mcrf"),
       2#010011_00000_00000_00000_00000000000#,
       (F_Crfd, F_Crfs, others => F_Eof)),
      (new S'("blr"), -- Simpliflied mnemonic
       2#010011_10100_00000_00000_0000010000_0#,
       (others => F_Eof)),
      (new S'("bclr"),
       2#010011_00000_00000_00000_0000010000_0#,
       (F_LK, F_BO, F_BI, others => F_Eof)),
      (new S'("crnor"),
       2#010011_00000_00000_00000_0000100001_0#,
       (F_CrbD, F_CrbA, F_CrbB, others => F_Eof)),
      (new S'("rfi"),
       2#010011_00000_00000_00000_0000110010_0#,
       (others => F_Eof)),
      (new S'("crandc"),
       2#010011_00000_00000_00000_0010000001_0#,
       (F_CrbD, F_CrbA, F_CrbB, others => F_Eof)),
      (new S'("isync"),
       2#010011_00000_00000_00000_0010010110_0#,
       (others => F_Eof)),
      (new S'("crxor"),
       2#010011_00000_00000_00000_0011000001_0#,
       (F_CrbD, F_CrbA, F_CrbB, others => F_Eof)),
      (new S'("crnand"),
       2#010011_00000_00000_00000_0011100001_0#,
       (F_CrbD, F_CrbA, F_CrbB, others => F_Eof)),
      (new S'("crand"),
       2#010011_00000_00000_00000_0100000001_0#,
       (F_CrbD, F_CrbA, F_CrbB, others => F_Eof)),
      (new S'("creqv"),
       2#010011_00000_00000_00000_0100100001_0#,
       (F_CrbD, F_CrbA, F_CrbB, others => F_Eof)),
      (new S'("crorc"),
       2#010011_00000_00000_00000_0110100001_0#,
       (F_CrbD, F_CrbA, F_CrbB, others => F_Eof)),
      (new S'("cror"),
       2#010011_00000_00000_00000_0111000001_0#,
       (F_CrbD, F_CrbA, F_CrbB, others => F_Eof)),
      (new S'("bcctr"),
       2#010011_00000_00000_00000_1000010000_0#,
       (F_LK, F_BO, F_BI, others => F_Eof)),
      (new S'("rlwimi"),
       2#010100_00000_00000_00000_0000000000_0#,
       (F_Rc, F_A, F_S, F_SH, F_MB, F_ME, others => F_Eof)),
      (new S'("clrlwi"), -- Simplified mnemonic
       2#010101_00000_00000_00000_00000_11111_0#,
       (F_Rc, F_A, F_S, F_MB, others => F_Eof)),
      (new S'("rlwinm"),
       2#010101_00000_00000_00000_00000_00000_0#,
       (F_Rc, F_A, F_S, F_SH, F_MB, F_ME, others => F_Eof)),
      (new S'("rlwnm"),
       2#010111_00000_00000_00000_0000000000_0#,
       (F_Rc, F_A, F_S, F_B, F_MB, F_ME, others => F_Eof)),
      (new S'("ori"),
       2#011000_00000_00000_0000000000000000#,
       (F_A, F_S, F_UIMM, others => F_Eof)),
      (new S'("oris"),
       2#011001_00000_00000_0000000000000000#,
       (F_A, F_S, F_UIMM, others => F_Eof)),
      (new S'("xori"),
       2#011010_00000_00000_0000000000000000#,
       (F_A, F_S, F_UIMM, others => F_Eof)),
      (new S'("xoris"),
       2#011011_00000_00000_0000000000000000#,
       (F_A, F_S, F_UIMM, others => F_Eof)),
      (new S'("andi."),
       2#011100_00000_00000_0000000000000000#,
       (F_A, F_S, F_UIMM, others => F_Eof)),
      (new S'("andis."),
       2#011101_00000_00000_0000000000000000#,
       (F_A, F_S, F_UIMM, others => F_Eof)),
      (new S'("cmp"),
       2#011111_00000_00000_0000000000000000#,
       (F_L, F_CrfD, F_A, F_B, others => F_Eof)),
      (new S'("tw"),
       2#011111_00000_00000_00000_0000000100_0#,
       (F_TO, F_A, F_B, others => F_Eof)),
      (new S'("subfc"),
       2#011111_00000_00000_00000_0_000001000_0#,
       (F_OE, F_Rc, F_D, F_A, F_B, others => F_Eof)),
      (new S'("addc"),
       2#011111_00000_00000_00000_0_000001010_0#,
       (F_Oe, F_Rc, F_D, F_A, F_B, others => F_Eof)),
      (new S'("mulhwu"),
       2#011111_00000_00000_00000_0_000001011_0#,
       (F_Rc, F_D, F_A, F_B, others => F_Eof)),
      (new S'("mfcr"),
       2#011111_00000_00000_00000_0_000010011_0#,
       (F_D, others => F_Eof)),
      (new S'("lwarx"),
       2#011111_00000_00000_00000_0_000010100_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("lwzx"),
       2#011111_00000_00000_00000_0_000010111_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("slw"),
       2#011111_00000_00000_00000_0_000011000_0#,
       (F_Rc, F_A, F_S, F_B, others => F_Eof)),
      (new S'("cntlzw"),
       2#011111_00000_00000_00000_0_000011010_0#,
       (F_Rc, F_A, F_S, others => F_Eof)),
      (new S'("and"),
       2#011111_00000_00000_00000_0_000011100_0#,
       (F_Rc, F_A, F_S, F_B, others => F_Eof)),
      (new S'("cmpl"),
       2#011111_00000_00000_00000_0_000100000_0#,
       (F_L, F_CrfD, F_A, F_B, others => F_Eof)),
      (new S'("subf"),
       2#011111_00000_00000_00000_0_000101000_0#,
       (F_Oe, F_Rc, F_D, F_A, F_B, others => F_Eof)),
      (new S'("dcbst"),
       2#011111_00000_00000_00000_0_000110110_0#,
       (F_A, F_B, others => F_Eof)),
      (new S'("lwzux"),
       2#011111_00000_00000_00000_0_000110111_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("andc"),
       2#011111_00000_00000_00000_0_000111100_0#,
       (F_Rc, F_A, F_S, F_B, others => F_Eof)),
      (new S'("mulhw"),
       2#011111_00000_00000_00000_0_001001011_0#,
       (F_Rc, F_D, F_A, F_B, others => F_Eof)),
      (new S'("mfmsr"),
       2#011111_00000_00000_00000_0_001010011_0#,
       (F_D, others => F_Eof)),
      (new S'("dcbf"),
       2#011111_00000_00000_00000_0_001010110_0#,
       (F_A, F_B, others => F_Eof)),
      (new S'("lbzx"),
       2#011111_00000_00000_00000_0_001010111_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("neg"),
       2#011111_00000_00000_00000_0_001101000_0#,
       (F_Oe, F_Rc, F_D, F_A, others => F_Eof)),
      (new S'("lbzux"),
       2#011111_00000_00000_00000_0_001110111_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("nor"),
       2#011111_00000_00000_00000_0_001111100_0#,
       (F_Rc, F_A, F_S, F_B, others => F_Eof)),
      (new S'("subfe"),
       2#011111_00000_00000_00000_0_010001000_0#,
       (F_Oe, F_Rc, F_D, F_A, F_B, others => F_Eof)),
      (new S'("adde"),
       2#011111_00000_00000_00000_0_010001010_0#,
       (F_Oe, F_Rc, F_D, F_A, F_B, others => F_Eof)),
      (new S'("mtcrf"),
       2#011111_00000_0000000000_0010010000_0#,
       (F_CRM, F_S, others => F_Eof)),
      (new S'("mtmsr"),
       2#011111_00000_00000_00000_0010010010_0#,
       (F_S, others => F_Eof)),

      (new S'("stwcx."),
       2#011111_00000_00000_00000_0010010110_1#,
       (F_A, F_S, F_B, others => F_Eof)),
      (new S'("stwx"),
       2#011111_00000_00000_00000_0010010111_0#,
       (F_A, F_S, F_B, others => F_Eof)),
      (new S'("stwux"),
       2#011111_00000_00000_00000_0010110111_0#,
       (F_A, F_S, F_B, others => F_Eof)),
      (new S'("subfze"),
       2#011111_00000_00000_00000_0_011001000_0#,
       (F_Oe, F_Rc, F_D, F_A, others => F_Eof)),
      (new S'("addze"),
       2#011111_00000_00000_00000_0_011001010_0#,
       (F_Oe, F_Rc, F_D, F_A, others => F_Eof)),
      (new S'("mtsr"),
       2#011111_00000_00000_00000_0011010010_0#,
       (F_SR, F_S, others => F_Eof)),
      (new S'("stbx"),
       2#011111_00000_00000_00000_0011010111_0#,
       (F_S, F_A, F_B, others => F_Eof)),
      (new S'("subfme"),
       2#011111_00000_00000_00000_0_011101000_0#,
       (F_Oe, F_Rc, F_D, F_A, others => F_Eof)),
      (new S'("addme"),
       2#011111_00000_00000_00000_0_011101010_0#,
       (F_Oe, F_Rc, F_D, F_A, others => F_Eof)),
      (new S'("mullw"),
       2#011111_00000_00000_00000_0_011101011_0#,
       (F_Oe, F_Rc, F_D, F_A, F_B, others => F_Eof)),
      (new S'("mtsrin"),
       2#011111_00000_00000_00000_0011110010_0#,
       (F_S, F_B, others => F_Eof)),
      (new S'("dcbtst"),
       2#011111_00000_00000_00000_0011110110_0#,
       (F_A, F_B, others => F_Eof)),
      (new S'("stbux"),
       2#011111_00000_00000_00000_0011110111_0#,
       (F_S, F_A, F_B, others => F_Eof)),
      (new S'("add"),
       2#011111_00000_00000_00000_0_100001010_0#,
       (F_Oe, F_Rc, F_D, F_A, F_B, others => F_Eof)),
      (new S'("dcbt"),
       2#011111_00000_00000_00000_0100010110_0#,
       (F_A, F_B, others => F_Eof)),
      (new S'("lhzx"),
       2#011111_00000_00000_00000_0100010111_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("eqv"),
       2#011111_00000_00000_00000_0100011100_0#,
       (F_Rc, F_A, F_S, F_B, others => F_Eof)),
      (new S'("tlbie"),
       2#011111_00000_00000_00000_0100110010_0#,
       (F_B, others => F_Eof)),
      (new S'("eciwx"),
       2#011111_00000_00000_00000_0100110110_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("lhzux"),
       2#011111_00000_00000_00000_0100110111_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("xor"),
       2#011111_00000_00000_00000_0100111100_0#,
       (F_Rc, F_A, F_S, F_B, others => F_Eof)),
      (new S'("mflr"),  --  Simplified mnemonic.
       2#011111_00000_01000_00000_0101010011_0#,
       (F_D, others => F_Eof)),
      (new S'("mfctr"),  --  Simplified mnemonic.
       2#011111_00000_01001_00000_0101010011_0#,
       (F_D, others => F_Eof)),
      (new S'("mfspr"),
       2#011111_00000_00000_00000_0101010011_0#,
       (F_D, F_Spr, others => F_Eof)),
      (new S'("lhax"),
       2#011111_00000_00000_00000_0101010111_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("tlbia"),
       2#011111_00000_00000_00000_0101110010_0#,
       (others => F_Eof)),
      (new S'("mftb"),
       2#011111_00000_00000_00000_0101110011_0#,
       (F_D, F_Tbr, others => F_Eof)),
      (new S'("lhaux"),
       2#011111_00000_00000_00000_0101110111_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("sthx"),
       2#011111_00000_00000_00000_0110010111_0#,
       (F_S, F_A, F_B, others => F_Eof)),
      (new S'("orc"),
       2#011111_00000_00000_00000_0110011100_0#,
       (F_Rc, F_A, F_S, F_B, others => F_Eof)),
      (new S'("ecowx"),
       2#011111_00000_00000_00000_0110110110_0#,
       (F_S, F_A, F_B, others => F_Eof)),
      (new S'("sthux"),
       2#011111_00000_00000_00000_0110110111_0#,
       (F_S, F_A, F_B, others => F_Eof)),
      (new S'("or"),
       2#011111_00000_00000_00000_0110111100_0#,
       (F_Rc, F_A, F_S, F_B, others => F_Eof)),
      (new S'("divwu"),
       2#011111_00000_00000_00000_0111001011_0#,
       (F_Oe, F_Rc, F_D, F_A, F_B, others => F_Eof)),
      (new S'("mtlr"), -- Simplified mnemonic
       2#011111_00000_01000_00000_0111010011_0#,
       (F_S, others => F_Eof)),
      (new S'("mtctr"), -- Simplified mnemonic
       2#011111_00000_01001_00000_0111010011_0#,
       (F_S, others => F_Eof)),
      (new S'("mtspr"),
       2#011111_00000_00000_00000_0111010011_0#,
       (F_Spr, F_S, others => F_Eof)),

      (new S'("dcbi"),
       2#011111_00000_00000_00000_0111010110_0#,
       (F_A, F_B, others => F_Eof)),
      (new S'("nandx"),
       2#011111_00000_00000_00000_0111011100_0#,
       (F_Rc, F_S, F_A, F_B, others => F_Eof)),
      (new S'("divw"),
       2#011111_00000_00000_00000_0111101011_0#,
       (F_Oe, F_Rc, F_D, F_A, F_B, others => F_Eof)),
      (new S'("mcrxr"),
       2#011111_00000_00000_00000_1000000000_0#,
       (F_CrfD, others => F_Eof)),
      (new S'("lswx"),
       2#011111_00000_00000_00000_1000010101_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("lwbrx"),
       2#011111_00000_00000_00000_1000010110_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("lfsx"),
       2#011111_00000_00000_00000_1000010111_0#,
       (F_FD, F_A, F_B, others => F_Eof)),
      (new S'("srw"),
       2#011111_00000_00000_00000_1000011000_0#,
       (F_Rc, F_A, F_S, F_B, others => F_Eof)),
      (new S'("tlbsync"),
       2#011111_00000_00000_00000_1000110110_0#,
       (others => F_Eof)),
      (new S'("lfsux"),
       2#011111_00000_00000_00000_1000110111_0#,
       (F_FD, F_A, F_B, others => F_Eof)),
      (new S'("mfsr"),
       2#011111_00000_00000_00000_1001010011_0#,
       (F_D, F_SR, others => F_Eof)),
      (new S'("lswi"),
       2#011111_00000_00000_00000_1001010101_0#,
       (F_D, F_A, F_NB, others => F_Eof)),
      (new S'("sync"),
       2#011111_00000_00000_00000_1001010110_0#,
       (others => F_Eof)),
      (new S'("lfdx"),
       2#011111_00000_00000_00000_1001010111_0#,
       (F_FD, F_A, F_B, others => F_Eof)),
      (new S'("lfdux"),
       2#011111_00000_00000_00000_1001110111_0#,
       (F_FD, F_A, F_B, others => F_Eof)),
      (new S'("mfsrin"),
       2#011111_00000_00000_00000_1010010011_0#,
       (F_D, F_B, others => F_Eof)),
      (new S'("stswx"),
       2#011111_00000_00000_00000_1010010101_0#,
       (F_S, F_A, F_B, others => F_Eof)),
      (new S'("stwbrx"),
       2#011111_00000_00000_00000_1010010110_0#,
       (F_S, F_A, F_B, others => F_Eof)),
      (new S'("stfsx"),
       2#011111_00000_00000_00000_1010010111_0#,
       (F_FS, F_A, F_B, others => F_Eof)),
      (new S'("stfsux"),
       2#011111_00000_00000_00000_1010110111_0#,
       (F_FS, F_A, F_B, others => F_Eof)),
      (new S'("stswi"),
       2#011111_00000_00000_00000_1011010101_0#,
       (F_S, F_A, F_NB, others => F_Eof)),
      (new S'("stfdx"),
       2#011111_00000_00000_00000_1011010111_0#,
       (F_FS, F_A, F_B, others => F_Eof)),
      (new S'("dcba"),
       2#011111_00000_00000_00000_1011110110_0#,
       (F_A, F_B, others => F_Eof)),
      (new S'("stfdux"),
       2#011111_00000_00000_00000_1011110111_0#,
       (F_FS, F_A, F_B, others => F_Eof)),
      (new S'("lhbrx"),
       2#011111_00000_00000_00000_1100010110_0#,
       (F_D, F_A, F_B, others => F_Eof)),
      (new S'("sraw"),
       2#011111_00000_00000_00000_1100011000_0#,
       (F_Rc, F_A, F_S, F_B, others => F_Eof)),
      (new S'("srawi"),
       2#011111_00000_00000_00000_1100111000_0#,
       (F_Rc, F_A, F_S, F_SH, others => F_Eof)),
      (new S'("eieio"),
       2#011111_00000_00000_00000_1101010110_0#,
       (others => F_Eof)),
      (new S'("sthbrx"),
       2#011111_00000_00000_00000_1110010110_0#,
       (F_S, F_A, F_B, others => F_Eof)),
      (new S'("extsh"),
       2#011111_00000_00000_00000_1110011010_0#,
       (F_Rc, F_A, F_S, others => F_Eof)),
      (new S'("extsb"),
       2#011111_00000_00000_00000_1110111010_0#,
       (F_A, F_S, others => F_Eof)),
      (new S'("icbi"),
       2#011111_00000_00000_00000_1111010110_0#,
       (F_A, F_B, others => F_Eof)),
      (new S'("stfiwx"),
       2#011111_00000_00000_00000_1111010111_0#,
       (F_FS, F_A, F_B, others => F_Eof)),

      (new S'("dcbz"),
       2#011111_00000_00000_00000_1111110110_0#,
       (F_A, F_B, others => F_Eof)),
      (new S'("lwz"),
       2#100000_00000_00000_00000_0000000000_0#,
       (F_U, F_D, F_Disp, F_A, others => F_Eof)),
      (new S'("lbz"),
       2#100010_00000_00000_00000_0000000000_0#,
       (F_U, F_D, F_Disp, F_A, others => F_Eof)),
      (new S'("stw"),
       2#100100_00000_00000_00000_0000000000_0#,
       (F_U, F_S, F_Disp, F_A, others => F_Eof)),
      (new S'("stb"),
       2#100110_00000_00000_00000_0000000000_0#,
       (F_U, F_S, F_Disp, F_A, others => F_Eof)),
      (new S'("lhz"),
       2#101000_00000_00000_00000_0000000000_0#,
       (F_U, F_D, F_Disp, F_A, others => F_Eof)),
      (new S'("lha"),
       2#101010_00000_00000_00000_0000000000_0#,
       (F_U, F_D, F_Disp, F_A, others => F_Eof)),
      (new S'("sth"),
       2#101100_00000_00000_00000_0000000000_0#,
       (F_U, F_S, F_Disp, F_A, others => F_Eof)),
      (new S'("lmw"),
       2#101110_00000_00000_00000_0000000000_0#,
       (F_D, F_Disp, F_A, others => F_Eof)),
      (new S'("stmw"),
       2#101111_00000_00000_00000_0000000000_0#,
       (F_S, F_Disp, F_A, others => F_Eof)),
      (new S'("lfs"),
       2#110000_00000_00000_00000_0000000000_0#,
       (F_U, F_FD, F_Disp, F_A, others => F_Eof)),
      (new S'("lfd"),
       2#110010_00000_00000_00000_0000000000_0#,
       (F_U, F_FD, F_Disp, F_A, others => F_Eof)),
      (new S'("stfs"),
       2#110100_00000_00000_00000_0000000000_0#,
       (F_U, F_FS, F_Disp, F_A, others => F_Eof)),
      (new S'("stfd"),
       2#110110_00000_00000_00000_0000000000_0#,
       (F_U, F_FS, F_Disp, F_A, others => F_Eof)),

      (new S'("fdivs"),
       2#111011_00000_00000_00000_00000_10010_0#,
       (F_Rc, F_FD, F_FA, F_FB, others => F_Eof)),
      (new S'("fsubs"),
       2#111011_00000_00000_00000_00000_10100_0#,
       (F_Rc, F_FD, F_FA, F_FB, others => F_Eof)),
      (new S'("fadds"),
       2#111011_00000_00000_00000_00000_10101_0#,
       (F_Rc, F_FD, F_FA, F_FB, others => F_Eof)),
      (new S'("fsqrts"),
       2#111011_00000_00000_00000_00000_10110_0#,
       (F_Rc, F_FD, F_FB, others => F_Eof)),
      (new S'("fres"),
       2#111011_00000_00000_00000_00000_11000_0#,
       (F_Rc, F_FD, F_FB, others => F_Eof)),
      (new S'("fmuls"),
       2#111011_00000_00000_00000_00000_11001_0#,
       (F_Rc, F_FD, F_FA, F_FC, others => F_Eof)),
      (new S'("fmsubs"),
       2#111011_00000_00000_00000_00000_11100_0#,
       (F_Rc, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      (new S'("fmadds"),
       2#111011_00000_00000_00000_00000_11101_0#,
       (F_Rc, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),


      (new S'("fnmsubs"),
       2#111011_00000_00000_00000_00000_11110_0#,
       (F_Rc, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      (new S'("fnmadds"),
       2#111011_00000_00000_00000_00000_11111_0#,
       (F_Rc, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      (new S'("fcmpu"),
       2#111111_00000_00000_00000_0000000000_0#,
       (F_CrfD, F_FA, F_FB, others => F_Eof)),
      (new S'("frsp"),
       2#111111_00000_00000_00000_0000001100_0#,
       (F_Rc, F_FD, F_FB, others => F_Eof)),
      (new S'("fctiw"),
       2#111111_00000_00000_00000_0000001110_0#,
       (F_Rc, F_FD, F_FB, others => F_Eof)),
      (new S'("fctiwz"),
       2#111111_00000_00000_00000_0000001111_0#,
       (F_Rc, F_FD, F_FB, others => F_Eof)),
      (new S'("fdiv"),
       2#111111_00000_00000_00000_00000_10010_0#,
       (F_Rc, F_FD, F_FA, F_FB, others => F_Eof)),
      (new S'("fsub"),
       2#111111_00000_00000_00000_00000_10100_0#,
       (F_Rc, F_FD, F_FA, F_FB, others => F_Eof)),
      (new S'("fadd"),
       2#111111_00000_00000_00000_00000_10101_0#,
       (F_Rc, F_FD, F_FA, F_FB, others => F_Eof)),
      (new S'("fsqrt"),
       2#111111_00000_00000_00000_00000_10110_0#,
       (F_Rc, F_FD, F_FB, others => F_Eof)),
      (new S'("fsel"),
       2#111111_00000_00000_00000_00000_10111_0#,
       (F_Rc, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      (new S'("fmul"),
       2#111111_00000_00000_00000_00000_11001_0#,
       (F_Rc, F_FD, F_FA, F_FC, others => F_Eof)),
      (new S'("frsqrte"),
       2#111111_00000_00000_00000_00000_11010_0#,
       (F_Rc, F_FD, F_FB, others => F_Eof)),
      (new S'("fmsub"),
       2#111111_00000_00000_00000_00000_11100_0#,
       (F_Rc, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      (new S'("fmadd"),
       2#111111_00000_00000_00000_00000_11101_0#,
       (F_Rc, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      (new S'("fnmsub"),
       2#111111_00000_00000_00000_00000_11110_0#,
       (F_Rc, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      (new S'("fnmadd"),
       2#111111_00000_00000_00000_00000_11111_0#,
       (F_Rc, F_FD, F_FA, F_FB, F_FC, others => F_Eof)),
      (new S'("fcmpo"),
       2#111111_00000_00000_00000_0000100000_0#,
       (F_CrfD, F_FA, F_FB, others => F_Eof)),
      (new S'("mtfsb1"),
       2#111111_00000_00000_00000_0000100110_0#,
       (F_Rc, F_CrbD , others => F_Eof)),
      (new S'("fneg"),
       2#111111_00000_00000_00000_0000101000_0#,
       (F_Rc, F_FD, F_FB, others => F_Eof)),
      (new S'("mcrfs"),
       2#111111_00000_00000_00000_0001000000_0#,
       (F_CrfD, F_crfS, others => F_Eof)),
      (new S'("mtfsb0"),
       2#111111_00000_00000_00000_0001000110_0#,
       (F_Rc, F_CrbD, others => F_Eof)),
      (new S'("fmr"),
       2#111111_00000_00000_00000_0001001000_0#,
       (F_Rc, F_FD, F_FB, others => F_Eof)),
      (new S'("mtfsfi"),
       2#111111_00000_00000_00000_0010000110_0#,
       (F_Rc, F_CrfD, F_IMM, others => F_Eof)),
      (new S'("fnabs"),
       2#111111_00000_00000_00000_0010001000_0#,
       (F_Rc, F_FD, F_FB, others => F_Eof)),
      (new S'("fabs"),
       2#111111_00000_00000_00000_0100001000_0#,
       (F_Rc, F_FD, F_FB, others => F_Eof)),
      (new S'("mffs"),
       2#111111_00000_00000_00000_1001000111_0#,
       (F_Rc, F_D, others => F_Eof)),
      (new S'("mtfsf"),
       2#111111_00000_00000_00000_1011000111_0#,
       (F_Rc, F_FM, F_FB, others => F_Eof))
     );

   subtype Bit_Number is Natural range 0 .. 31;

   type Field_Type is record
      First, Last : Bit_Number;
   end record;

   --  Use the PowerPC Big-endian convention.
   --  Ie bit 0 is 2**31.
   function Field (F : Field_Type) return Unsigned_32
   is
      pragma Assert (not (F.First = 0 and F.Last = 31));
      Nbr_Bits : constant Bit_Number := F.Last - F.First + 1;
   begin
      pragma Assert (F.First <= F.Last);
      return ((2 ** Nbr_Bits) - 1) * (2 ** (31 - F.Last));
   end Field;

   type Fields_Mask_Type is array (Ppc_Fields'First .. Ppc_Fields'Pred (F_Eof))
     of Field_Type;
   Fields_Mask : constant Fields_Mask_Type :=
     (F_TO | F_D | F_FD | F_BO | F_S | F_FS | F_CrbD => (6, 10),
      F_A | F_FA | F_BI | F_CrbA => (11, 15),
      F_B | F_FB | F_Crbb | F_Sh | F_Nb => (16, 20),
      F_C | F_FC | F_MB => (21, 25),
      F_ME => (26, 30),
      F_CrfD => (6, 8),
      F_Crfs => (11, 13),
      F_L => (10, 10),
      F_Bd => (16, 29),
      F_Aa => (30, 30),
      F_Lk => (31, 31),
      F_Fm => (7, 14),
      F_Imm => (16, 19),
      F_Crm => (12, 19),
      F_Li => (6, 29),
      F_Rc => (31, 31),
      F_Spr | F_Tbr => (11, 20),
      F_OE => (21, 21),
      F_Sr => (12, 15),
      F_Br_Hint => (10, 10),
      F_U => (5, 5),
      F_SIMM | F_UIMM | F_Disp => (16, 31));

   type Insns_Masks_Type is array (Ppc_Insns'Range) of Unsigned_32;
   Insns_Mask : Insns_Masks_Type;

   procedure Gen_Masks
   is
      --M_Opc : constant Unsigned_32 := Field ((0, 5));
      Mask : Unsigned_32;
      --Prev : Unsigned_32;
      F : Ppc_Fields;
   begin
      --Prev := 0;
      for I in Ppc_Insns'Range loop
         declare
            Insn : Ppc_Insn_Descr renames Ppc_Insns (I);
         begin
            --  Be sure the insns are ordered.
            --pragma Assert (Insn.Insn > Prev,
            --              "insn " & Insn.Name.all & " is not ordered");
            --Prev := Insn.Insn;

            Mask := 0;
            for J in Insn.Fields'Range loop
               F := Insn.Fields (J);
               exit when F = F_Eof;
               Mask := Mask or Field (Fields_Mask (F));
            end loop;
            Insns_Mask (I) := not Mask;

            pragma Assert ((Insn.Insn and Mask) = 0,
                           "Bad insn for " & Insn.Name.all);
         end;
      end loop;
   end Gen_Masks;

   procedure Disassemble_Insn (Addr : System.Address;
                               Pc : Traces.Pc_Type;
                               Line : out String;
                               Line_Pos : out Natural;
                               Insn_Len : out Natural;
                               Proc_Cb : Symbol_Proc_Type)
   is
      W : Unsigned_32;

      --  Add CHAR to the line.
      procedure Add (C : Character);
      pragma Inline (Add);

      procedure Add (C : Character) is
      begin
         if Line_Pos <= Line'Last then
            Line (Line_Pos) := C;
            Line_Pos := Line_Pos + 1;
         end if;
      end Add;

      --  Add STR to the line.
      procedure Add (Str : String) is
      begin
         if Line_Pos + Str'Length <= Line'Last then
            Line (Line_Pos .. Line_Pos + Str'Length - 1) := Str;
            Line_Pos := Line_Pos + Str'Length;
         else
            for I in Str'Range loop
               Add (Str (I));
            end loop;
         end if;
      end Add;

      procedure Add_Ht is
      begin
         Add (' ');
         while Line_Pos - Line'First < 7 loop
            Add (' ');
         end loop;
      end Add_HT;

      subtype Natural_0_9 is Natural range 0 .. 9;
      procedure Add_Digit (N : Natural_0_9) is
      begin
         Add (Character'Val (Character'Pos ('0') + N));
      end Add_Digit;

      procedure Add_Num2 (Reg : Unsigned_32)
      is
         H, L : Unsigned_32;
      begin
         L := Reg mod 10;
         H := Reg / 10;
         if H /= 0 then
            Add_Digit (Natural (H));
         end if;
         Add_Digit (Natural (L));
      end Add_Num2;


      function Get_Field (F : Field_Type) return Unsigned_32
      is
         Len : constant Natural := F.Last - F.First + 1;
      begin
         return Shift_Right (Shift_Left (W, F.First), 32 - Len);
      end Get_Field;

      procedure Add_Simm (Val : Unsigned_32)
      is
         V : constant Unsigned_16 := Unsigned_16 (Val);
      begin
         if (V and 16#8000#) /= 0 then
            Add ("-0x");
            Add (Hex_Image (-V));
         else
            Add ("0x");
            Add (Hex_Image (V));
         end if;
      end Add_Simm;

      procedure Add_Uimm
      is
         V : constant Unsigned_16 := Unsigned_16 (Get_Field ((16, 31)));
      begin
         Add ("0x");
         Add (Hex_Image (V));
      end Add_Uimm;

      pragma Unreferenced (Add_Uimm);

      Insn_Index : Integer := -1;
   begin
      W := Get_Insn (Addr);
      Insn_Len := 4;
      Line_Pos := Line'First;

      --  Find insn.
      for I in Ppc_Insns'Range loop
         if (W and Insns_Mask (I)) = Ppc_Insns (I).Insn then
            Insn_Index := I;
            exit;
         end if;
      end loop;

      if Insn_Index > 0 then
         declare
            Insn : Ppc_Insn_Descr renames Ppc_Insns (Insn_Index);
            Has_Ht : Boolean := False;
            Is_First : Boolean := True;
            F : Ppc_Fields;
            Val : Unsigned_32;
         begin
            Add (Insn.Name.all);
            for I in Insn.Fields'Range loop
               F := Insn.Fields (I);
               exit when F = F_Eof;
               if F not in Ppc_Mnemo_Fields then
                  if not Has_Ht then
                     Add_Ht;
                     Has_Ht := True;
                     Is_First := False;
                  elsif Is_First then
                     Is_First := False;
                  else
                     Add (',');
                  end if;
               else
                  pragma Assert (Is_First);
                  null;
               end if;

               Val := Get_Field (Fields_Mask (F));

               case F is
                  when F_OE =>
                     if Val /= 0 then
                        Add ('o');
                     end if;
                  when F_Rc =>
                     if Val /= 0 then
                        Add ('.');
                     end if;
                  when F_AA =>
                     if Val /= 0 then
                        Add ('a');
                     end if;
                  when F_LK =>
                     if Val /= 0 then
                        Add ('l');
                     end if;
                  when F_Br_Hint =>
                     if Val = 0 then
                        Add ('-');
                     else
                        Add ('+');
                     end if;
                  when F_L =>
                     if Val = 0 then
                        Add ('w');
                     else
                        Add ('d');
                     end if;
                  when F_U =>
                     if Val /= 0 then
                        Add ('u');
                     end if;
                  when F_A | F_D | F_S | F_B =>
                     Add ('r');
                     Add_Num2 (Val);
                  when F_FA | F_FD | F_FS | F_FB | F_FC =>
                     Add ('f');
                     Add_Num2 (Val);
                  when F_Disp =>
                     pragma Assert (Insn.Fields (I + 1) = F_A);
                     pragma Assert (Insn.Fields (I + 2) = F_Eof);
                     Add_Simm (Val);
                     Add ("(r");
                     Add_Num2 (Get_Field (Fields_Mask (F_A)));
                     Add (')');
                     exit;
                  when F_Simm =>
                     Add_Simm (Val);
                  when F_Li =>
                     declare
                        Target : Unsigned_32;
                     begin
                        --  Sign extend
                        if (Val and 16#800000#) /= 0 then
                           Val := Val or 16#Ff000000#;
                        end if;
                        Target := Shift_Left (Val, 2);
                        --  Test AA field.
                        if (W and 2) = 0 then
                           Target := Target + Pc;
                        end if;
                        Add ("0x");
                        Add (Hex_Image (Target));
                        if Proc_Cb /= null then
                           Proc_Cb.all (Target, Line, Line_Pos);
                        end if;
                     end;
                  when F_BD =>
                     declare
                        Target : Unsigned_32;
                     begin
                        --  Sign extend
                        if (Val and 16#8000#) /= 0 then
                           Val := Val or 16#Ffff0000#;
                        end if;
                        Target := Shift_Left (Val, 2);
                        --  Test AA field.
                        if (W and 2) = 0 then
                           Target := Target + Pc;
                        end if;
                        Add ("0x");
                        Add (Hex_Image (Target));
                        if Proc_Cb /= null then
                           Proc_Cb.all (Target, Line, Line_Pos);
                        end if;
                     end;
                  when F_CrfD | F_CrfS =>
                     Add ("cr");
                     Add_Num2 (Val);
                  when F_BI | F_BO | F_SH | F_MB | F_ME =>
                     Add_Num2 (Val);
                  when others =>
                     Add (Hex_Image (Val));
               end case;
            end loop;
         end;
      else
         Add (".long ");
         Add (Hex_Image (W));
      end if;
   end Disassemble_Insn;

begin
   Gen_Masks;
end Disa_Ppc;

