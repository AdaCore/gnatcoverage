------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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
with Hex_Images;   use Hex_Images;
with Highlighting; use Highlighting;

package body Disa_ARM is

   function To_Insn (Insn_Bin : Binary_Content) return Unsigned_32 is
     (Disa_Common.ELF_To_U32
        (Slice (Insn_Bin, Insn_Bin.First, Insn_Bin.First + 3)));

   type Cond_Type is mod 2 ** 4;

   function Get_Cond (Insn : Unsigned_32) return Cond_Type is
     (Cond_Type (Shift_Right (Insn, 28)));

   function Get_Imm24 (Insn        : Unsigned_32;
                       Sign_Extend : Boolean) return Unsigned_32
   is (Shift_Right_Arithmetic (Shift_Left (Insn, 8), 8));

   function Get_Target24 (Insn : Unsigned_32;
                          Pc   : Unsigned_32) return Unsigned_32
   is (Pc + Shift_Left (Get_Imm24 (Insn, True), 2) + 8);
   --  The PC is always 2 instructions beyond the currently executing
   --  instruction, hence the +8 offset.

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   function Get_Insn_Length
     (Self     : ARM_Disassembler;
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
     (Self     : ARM_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Traces.Pc_Type;
      Buffer   : in out Highlighting.Buffer_Type;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class)
   is
      pragma Unreferenced (Sym);
   begin
      Insn_Len := Get_Insn_Length (Self, Insn_Bin);
      Buffer.Start_Token (Text);
      Buffer.Put
        ("<unknown ARM insn " & Hex_Image (To_Insn (Insn_Bin))
         & " at " & Hex_Image (Pc) & ">");
   end Disassemble_Insn;

   -------------------------
   -- Get_Insn_Properties --
   -------------------------

   procedure Get_Insn_Properties
     (Self        : ARM_Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest)
   is
      PC32 : constant Unsigned_32 := Unsigned_32 (Pc);
      Insn : constant Unsigned_32 := To_Insn (Insn_Bin);
      Cond : constant Cond_Type := Get_Cond (Insn);

   begin
      Branch_Dest := (No_PC, No_PC);
      FT_Dest     := (No_PC, No_PC);

      Flag_Indir := False;
      Flag_Cond  := False;

      case Cond is

      --  Unconditional instructions

      when 2#1111# =>
         Flag_Cond := False;
         Branch := Br_None;

         if (Shift_Right (Insn, 24) and 2#1110#) = 2#1010# then

            --  BLX (immediate)

            Branch := Br_Call;
            Branch_Dest.Target := Pc_Type (Get_Target24 (Insn, PC32));
         end if;

      --  Conditional instructions

      when others =>
         Flag_Cond := Cond /= 2#1110#;

         case Shift_Right (Insn, 22) and 2#111111# is

         when 2#1000_10# =>

            --  LDM/LDMIA/LDMFD (A1)

            if (Insn and 2**15) = 0 then
               Branch := Br_None;
            else
               --  This instructions writes in PC, so it's a control-flow
               --  instruction.

               Branch := Br_Jmp;
               Flag_Indir := True;
            end if;

         when 2#1010_00# .. 2#1010_11# =>

            --  B

            Branch := Br_Jmp;
            Branch_Dest.Target := Pc_Type (Get_Target24 (Insn, PC32));

         when 2#1011_00# .. 2#1011_11# =>

            --  BL

            Branch := Br_Call;
            Branch_Dest.Target := Pc_Type (Get_Target24 (Insn, PC32));

         when others =>
            case Shift_Right (Insn, 4) and 16#ffffff# is
            when 2#00010010_11111111_11110011# =>

               --  BLX (register)

               Branch := Br_Call;
               Flag_Indir := True;

            when 2#00010010_11111111_11110001#
               | 2#00010010_11111111_11110010# =>

               --  BX / BJX

               Branch := Br_Jmp;
               Flag_Indir := True;

            when others =>
               if Cond /= 2#1110# then

                  --  Represent conditional instructions that are not branches
                  --  as branches to the next instruction.

                  Branch := Br_Jmp;
                  FT_Dest.Target := Pc + 4;
                  Branch_Dest.Target := Pc + 4;

               else
                  Branch := Br_None;
               end if;
            end case;

         end case;
      end case;

      FT_Dest.Target := Pc + Pc_Type (Get_Insn_Length (Self, Insn_Bin));
   end Get_Insn_Properties;

   ----------------
   -- Is_Padding --
   ----------------

   function Is_Padding
     (Self     : ARM_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type) return Boolean
   is
      pragma Unreferenced (Self, Insn_Bin, Pc);
   begin
      --  We don't need to implement this on all platforms (i.e. we have no
      --  padding problem on native Linux for instance), so let disassemblers
      --  override this if needed.

      return False;
   end Is_Padding;

end Disa_ARM;
