------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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

with Interfaces;   use Interfaces;

with Disa_Common;  use Disa_Common;
with Sparc_Descs;  use Sparc_Descs;

package body Disa_Sparc is

   function Get_Field
     (Field : Sparc_Fields; V : Unsigned_32) return Unsigned_32;
   function Get_Field_Sext
     (Field : Sparc_Fields; V : Unsigned_32) return Unsigned_32;
   --  Extract a field from an instruction.

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Field : Sparc_Fields; V : Unsigned_32) return Unsigned_32
   is
      F : constant Field_Type := Fields_Mask (Field);
      Len : constant Natural := F.First - F.Last + 1;
   begin
      return Shift_Right (Shift_Left (V, 31 - F.First), 32 - Len);
   end Get_Field;

   --------------------
   -- Get_Field_Sext --
   --------------------

   function Get_Field_Sext
     (Field : Sparc_Fields; V : Unsigned_32) return Unsigned_32
   is
      F : constant Field_Type := Fields_Mask (Field);
      Len : constant Natural := F.First - F.Last + 1;
   begin
      return Shift_Right_Arithmetic (Shift_Left (V, 31 - F.First), 32 - Len);
   end Get_Field_Sext;

   ----------------
   -- Initialize --
   ----------------
   overriding procedure Initialize
     (Object : in out SPARC_Disassembler) is
   begin
      Object.Handle := Dis_Opcodes.Create_Sparc_Disassembler;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Object : in out SPARC_Disassembler) is
   begin
      Dis_Opcodes.Delete_Disassembler (Object.Handle);
   end Finalize;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   function Get_Insn_Length
     (Self     : SPARC_Disassembler;
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
     (Self     : SPARC_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Traces.Pc_Type;
      Buffer   : in out Highlighting.Buffer_Type;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class) is
   begin
      Opcodes_Disassemble_Insn
        (Self.Handle, Insn_Bin, Pc, Buffer, Insn_Len, Sym, 4);
   end Disassemble_Insn;

   -------------------------
   -- Get_Insn_Properties --
   -------------------------

   procedure Get_Insn_Properties
     (Self        : SPARC_Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest)
   is
      pragma Unreferenced (Self);
      W : Unsigned_32;
      R : Unsigned_32;
   begin
      --  Make sure OUT parameters have a valid value

      Branch_Dest := (No_PC, No_PC);
      FT_Dest     := (No_PC, No_PC);
      Branch      := Br_None;

      if Length (Insn_Bin) < 4 then
         raise Program_Error;
      end if;

      W := To_Big_Endian_U32
        (Slice (Insn_Bin, Insn_Bin.First, Insn_Bin.First + 3));

      Flag_Cond := False;
      Flag_Indir := False;

      case Get_Field (F_Op, W) is
         when 2#00# =>
            case Get_Field (F_Op2, W) is
               when 2#010# | 2#110# =>
                  --  Bicc / FBfcc

                  --  Sparc v7 spec:
                  --  [...] the branch is taken, causing a delayed, PC-relative
                  --  control transfer to the address
                  --  PC + (sign extnd (disp22) * 4)
                  Branch_Dest.Target :=
                     Pc + Pc_Type (Get_Field_Sext (F_Disp22, W)) * 4;

                  --  Sparc v7 spec:
                  --  If the branch is not taken, the annul bit field (a) is
                  --  checked.  [...]
                  --  If the branch is taken, the annul field is ignored, and
                  --  the delay instruction is executed.
                  Branch_Dest.Delay_Slot := Pc + 4;

                  FT_Dest.Target := Pc + 8;
                  if Get_Field (F_A, W) = 0 then
                     --  If the annul field is zero, the delay instruction is
                     --  executed.
                     FT_Dest.Delay_Slot := Pc + 4;
                  else
                     --  If a is set, the instruction immediately following
                     --  the branch instruction is not executed (ie, it is
                     --  annulled).
                     FT_Dest.Delay_Slot := No_PC;
                  end if;

                  if (Get_Field (F_Cond, W) and 2#0111#) /= 0 then
                     Flag_Cond := True;
                  elsif Get_Field (F_Cond, W) = 2#1000# then
                     --  BA
                     --  Sparc v7 spec:
                     --  Branch Always (BA), because it always branch
                     --  regardless of the condition code, would normally
                     --  ignore the annul field. Instead it follows the same
                     --  annul field rules:

                     --  Isn't this redundant with the above setting???

                     if Get_Field (F_A, W) = 1 then
                        --  if a = 1, the delay instruction is annulled;
                        Branch_Dest.Delay_Slot := No_PC;
                     else
                        --  if a = 0, the delay instruction is executed.
                        Branch_Dest.Delay_Slot := Pc + 4;
                     end if;
                  end if;
                  Branch := Br_Jmp;
                  return;

               when others =>
                  null;
            end case;

         when 2#01# =>
            --  Call
            --  Sparc v7 spec:
            --  The CALL instruction causes a delayed, unconditionnal,
            --  PC-relative control transfer to the address PC + (disp30 * 4).
            --  [...], therefore the delay slot instruction following the CALL
            --  instruction is always executed.
            Branch_Dest.Target :=
               Pc + Pc_Type (Get_Field_Sext (F_Disp30, W)) * 4;
            Branch_Dest.Delay_Slot := Pc + 4;
            Branch := Br_Call;
            return;

         when 2#10# =>
            if Get_Field (F_Op3, W) = 16#38# then
               Branch_Dest.Delay_Slot := Pc + 4;
               --  jmpl.
               Flag_Indir := True;
               R := Get_Field (F_Rd, W);
               if R = 15 then
                  --  Register indirect call.
                  Branch := Br_Call;
                  return;
               elsif R = 0
                 and then Get_Field (F_I, W) = 1
                 and then Get_Field (F_Simm13, W) = 8
               then
                  --  Ret or retl.
                  R := Get_Field (F_Rs1, W);
                  if R = 15 or else R = 31 then
                     Branch := Br_Ret;
                     return;
                  end if;
               end if;
               --  Unknown.
               Branch := Br_Jmp;
               return;
            end if;

         when others =>
            null;
      end case;

      Branch := Br_None;
   end Get_Insn_Properties;

   ----------------
   -- Is_Padding --
   ----------------

   function Is_Padding
     (Self     : SPARC_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type) return Boolean
   is
      pragma Unreferenced (Self, Insn_Bin, Pc);
   begin
      return False;
   end Is_Padding;

end Disa_Sparc;
