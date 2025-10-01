------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with Disa_Common;

package body Disa_AArch64 is

   function To_Insn (Insn_Bin : Binary_Content) return Unsigned_32
   is (Disa_Common.ELF_To_U32
         (Slice (Insn_Bin, Insn_Bin.First, Insn_Bin.First + 3)));

   type Imm_Fun_Type is
     access function
       (Insn : Unsigned_32; Ignored_Sign_Extend : Boolean) return Unsigned_32;

   function Get_Imm26
     (Insn : Unsigned_32; Ignored_Sign_Extend : Boolean) return Unsigned_32
   is (Shift_Right_Arithmetic (Shift_Left (Insn, 6), 6));

   function Get_Imm14
     (Insn : Unsigned_32; Ignored_Sign_Extend : Boolean) return Unsigned_32
   is (Shift_Right_Arithmetic (Shift_Left (Insn, 13), 18));

   function Get_Imm19
     (Insn : Unsigned_32; Ignored_Sign_Extend : Boolean) return Unsigned_32
   is (Shift_Right_Arithmetic (Shift_Left (Insn, 8), 13));

   function Get_Target
     (Imm_Fun : Imm_Fun_Type; Insn : Unsigned_32; Pc : Unsigned_64)
      return Unsigned_64
   is (Pc + Unsigned_64 (Shift_Left (Imm_Fun (Insn, True), 2)));

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (Object : in out AArch64_Disassembler) is
   begin
      Object.Handle := Dis_Opcodes.Create_AArch64_Disassembler;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Object : in out AArch64_Disassembler) is
   begin
      Dis_Opcodes.Delete_Disassembler (Object.Handle);
   end Finalize;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   function Get_Insn_Length
     (Self : AArch64_Disassembler; Insn_Bin : Binary_Content) return Positive
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Insn_Bin);
   begin
      return 4;
   end Get_Insn_Length;

   ----------------------
   -- Disassemble_Insn --
   ----------------------

   procedure Disassemble_Insn
     (Self     : AArch64_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Traces.Pc_Type;
      Buffer   : in out Highlighting.Buffer_Type;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class) is
   begin
      Disa_Common.Opcodes_Disassemble_Insn
        (Self.Handle, Insn_Bin, Pc, Buffer, Insn_Len, Sym, 4);
   end Disassemble_Insn;

   -------------------------
   -- Get_Insn_Properties --
   -------------------------

   procedure Get_Insn_Properties
     (Self        : AArch64_Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest)
   is
      PC64      : constant Unsigned_64 := Unsigned_64 (Pc);
      Insn      : constant Unsigned_32 := To_Insn (Insn_Bin);
      Insn_Code : Unsigned_32;

   begin
      Branch_Dest := (No_PC, No_PC);
      FT_Dest := (No_PC, No_PC);

      Flag_Indir := False;
      Flag_Cond := False;
      Branch := Br_None;

      --  For instruction codes, see
      --  https://github.com/CAS-Atlantic/AArch64-Encoding/

      if (Shift_Right (Insn, 26) and 2#111#) = 2#101# then

         --  Branch, exception generation and system instructions

         Insn_Code := Shift_Right (Insn, 25);

         case Insn_Code is

            when 2#0001_010# | 2#0001_011#               =>
               --  B
               Branch := Br_Jmp;
               Branch_Dest.Target :=
                 Pc_Type (Get_Target (Get_Imm26'Access, Insn, PC64));

            when 2#10010_10# .. 2#10010_11#              =>
               --  BL
               Branch := Br_Call;
               Branch_Dest.Target :=
                 Pc_Type (Get_Target (Get_Imm26'Access, Insn, PC64));

            when 2#1101011#                              =>

               --  Unconditional branch (register)

               case Shift_Right (Insn, 21) and 2#1111# is
                  when 2#0000# =>
                     --  BR
                     Branch := Br_Jmp;
                     Flag_Indir := True;

                  when 2#0001# =>
                     --  BLR
                     Branch := Br_Call;
                     Flag_Indir := True;

                  when 2#0010# =>
                     --  RET
                     Branch := Br_Ret;
                     Flag_Indir := True;

                  when others  =>
                     null;

               end case;

            when 2#0011_011# | 2#1011_011#               =>
               --  TBZ / TBNZ
               Flag_Cond := True;
               Branch := Br_Jmp;
               Branch_Dest.Target :=
                 Pc_Type (Get_Target (Get_Imm14'Access, Insn, PC64));

            when 2#0101_010# | 2#0011_010# | 2#1011_010# =>
               --  Conditional Branch (B.cond) / CBZ / CBNZ
               Flag_Cond := True;
               Branch := Br_Jmp;
               Branch_Dest.Target :=
                 Pc_Type (Get_Target (Get_Imm19'Access, Insn, PC64));

            when others                                  =>
               null;

         end case;
      end if;

      FT_Dest.Target := Pc + Pc_Type (Get_Insn_Length (Self, Insn_Bin));
   end Get_Insn_Properties;

   ----------------
   -- Is_Padding --
   ----------------

   function Is_Padding
     (Self : AArch64_Disassembler; Insn_Bin : Binary_Content; Pc : Pc_Type)
      return Boolean
   is
      pragma Unreferenced (Self, Insn_Bin, Pc);
   begin
      return False;
   end Is_Padding;

end Disa_AArch64;
