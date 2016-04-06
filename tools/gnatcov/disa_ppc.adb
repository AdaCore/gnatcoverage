------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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
with Highlighting; use Highlighting;
with Ppc_Descs;    use Ppc_Descs;

package body Disa_Ppc is

   function To_Insn (Insn_Bin : Binary_Content) return Unsigned_32 is
     (Disa_Common.To_Big_Endian_U32
        (Slice (Insn_Bin, Insn_Bin.First, Insn_Bin.First + 3)));

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Object : in out PPC_Disassembler) is
   begin
      Object.Handle := Dis_Opcodes.Create_Ppc_Disassembler;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Object : in out PPC_Disassembler) is
   begin
      Dis_Opcodes.Delete_Disassembler (Object.Handle);
   end Finalize;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   function Get_Insn_Length
     (Self     : PPC_Disassembler;
      Insn_Bin : Binary_Content) return Positive
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
     (Self     : PPC_Disassembler;
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
     (Self        : PPC_Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest)
   is
      Insn : constant Unsigned_32 := To_Insn (Insn_Bin);

      Opc, Xo, Bo : Unsigned_32;
      D : Pc_Type;
   begin
      Branch_Dest := (No_PC, No_PC);
      FT_Dest     := (No_PC, No_PC);

      Flag_Indir := False;
      Flag_Cond  := False;

      Opc := Get_Field (F_OPC, Insn);
      Xo  := Get_Field (F_XO, Insn);

      --  To be overriden for non-common cases

      if Get_Field (F_LK, Insn) = 1 then
         Branch := Br_Call;
      else
         Branch := Br_Jmp;
      end if;

      if Opc = 18 then
         --  Opc = 18: b, ba, bl and bla

         D := Pc_Type (Shift_Left (Get_Signed_Field (F_LI, Insn), 2));
         if Get_Field (F_AA, Insn) = 1 then
            Branch_Dest.Target := D;
         else
            Branch_Dest.Target := Pc + D;
         end if;
         return;

      elsif Opc = 16 then
         --  bcx

         D := Pc_Type (Shift_Left (Get_Signed_Field (F_BD, Insn), 2));
         if Get_Field (F_AA, Insn) = 1 then
            Branch_Dest.Target := D;
         else
            Branch_Dest.Target := Pc + D;
         end if;

      elsif Opc = 19 and Xo = 16 then
         --  bclrx

         Flag_Indir := True;
         if Branch = Br_Jmp then
            Branch := Br_Ret;
         end if;

      elsif Opc = 19 and Xo = 528 then
         --  bcctrx

         Flag_Indir := True;

      else
         Branch := Br_None;
         return;
      end if;

      Bo := Get_Field (F_BO, Insn);
      Flag_Cond := not ((Bo and 2#10100#) = 2#10100#);
      FT_Dest.Target := Pc + Pc_Type (Get_Insn_Length (Self, Insn_Bin));
   end Get_Insn_Properties;

   ----------------
   -- Is_Padding --
   ----------------

   function Is_Padding
     (Self     : PPC_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type) return Boolean
   is
      pragma Unreferenced (Self, Insn_Bin, Pc);
   begin
      return False;
   end Is_Padding;

end Disa_Ppc;
