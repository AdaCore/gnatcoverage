------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2014, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

with Disa_Common;  use Disa_Common;
with Highlighting; use Highlighting;

package body Disa_Lmp is

   type Insn_Class is
     (Non_Storage_Relative,
      Non_Storage_Register,
      Storage_Immediate,
      Storage_Register);
   pragma Unreferenced (Non_Storage_Register, Storage_Immediate);
   --  GR5/GR6 instruction class. Determines how the instruction is encoded.

   type Operation_Type is mod 2 ** 4;
   --  Instruction fields used to dispatch through decoding tables

   OP_BRA     : constant Operation_Type := 2#1100#;

   type Condition_Type is new Natural range 0 .. 15;

   COND_FALSE : constant Condition_Type := 2#0000#;
   COND_TRUE  : constant Condition_Type := 2#1111#;

   -------------------------------
   -- Opcode decoding utilities --
   -------------------------------

   type Bits_Size is new Natural range 1 .. 16;
   type Bit_Masks is array (Bits_Size) of Unsigned_32;
   Masks : constant Bit_Masks :=
     (16#0001#, 16#0003#, 16#0007#, 16#000f#,
      16#001f#, 16#003f#, 16#007f#, 16#00ff#,
      16#01ff#, 16#03ff#, 16#07ff#, 16#0fff#,
      16#1fff#, 16#3fff#, 16#7fff#, 16#ffff#);

   type Bit_Location is new Integer range -1 .. 31;
   --  Location of a single bit in an instruction

   No_Bit_Location : constant Bit_Location := Bit_Location'First;

   type Bits_Location is record
      Start : Bit_Location := No_Bit_Location;
      --  Location of the least significant bit

      Size  : Bits_Size;
      --  Number of bits
   end record;
   --  Location of continuous bits in an instruction

   function Get
     (Insn : Unsigned_32; Location : Bits_Location) return Unsigned_32
   is
     (Shift_Right (Insn, Natural (Location.Start)) and Masks (Location.Size));
   --  Extract and return bits from an instruction
   --  TODO??? Raise an error if Location invalid

   --  Each field kind may lie in a different location, depending on the
   --  instruction class . The following constants list the different possible
   --  locations for fields. The common ones will not vary depending on the
   --  class. Decoding can then be performed as follows:
   --    - Get the instruction class
   --    - Determine field locations (from the following constants)
   --    - Get discriminants for instruction kinds (Operation and F_Inst
   --      fields).
   --    - Get the instruction descriptor from discriminants and decoding
   --      tables
   --    - Actually disassemble extracting fields (using descriptor and
   --      locations) and format some output

   --  Location of fields in instructions: common fields

   Op_Type_Loc               : constant Bits_Location := (25, 2);

   Operation_Loc             : constant Bits_Location := (21, 4);
   Condition_Loc             : constant Bits_Location := (27, 4);

   --  Non-storage relative class instructions fields

   NSRel_Signed_Relative_Loc : constant Bits_Location := (0, 16);

   function Get_Relative_Offset (Insn : Unsigned_32) return Integer_16;
   --  Return the least significant bits of Insn as a signed 16-bit integer.
   --  Can be used to extract the Relative Offset field of non-storage relative
   --  class instructions.

   function Get_Insn_Class (Insn : Unsigned_32) return Insn_Class is
     (Insn_Class'Val (Natural (Get (Insn, Op_Type_Loc))));
   --  Return the instruction class of Insn

   -------------------------
   -- Get_Relative_Offset --
   -------------------------

   function Get_Relative_Offset (Insn : Unsigned_32) return Integer_16 is
      Offset : constant Unsigned_16 :=
        Unsigned_16 (Get (Insn, NSRel_Signed_Relative_Loc));
   begin
      return
        (if Offset > 16#7fff#
         then -Integer_16 (not Offset + 1)
         else Integer_16 (Offset));
   end Get_Relative_Offset;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Object : in out LMP_Disassembler) is
   begin
      Object.Handle := Dis_Opcodes.Create_Visium_Disassembler;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Object : in out LMP_Disassembler) is
   begin
      Dis_Opcodes.Delete_Disassembler (Object.Handle);
   end Finalize;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   overriding function Get_Insn_Length
     (Self     : LMP_Disassembler;
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

   overriding procedure Disassemble_Insn
      (Self     : LMP_Disassembler;
       Insn_Bin : Binary_Content;
       Pc       : Pc_Type;
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

   overriding procedure Get_Insn_Properties
     (Self        : LMP_Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest)
   is
      pragma Unreferenced (Self);

      Insn       : constant Unsigned_32 := To_Big_Endian_U32
        (Slice (Insn_Bin, Pc, Pc + 3));
      Class      : constant Insn_Class := Get_Insn_Class (Insn);
      Delay_Slot : constant Pc_Type := Pc + 4;

      function Condition return Condition_Type is
        (Condition_Type (Get (Insn, Condition_Loc)));

   begin
      --  These are default values for control-flow "unrelated" instructions

      Branch      := Br_None;
      Flag_Indir  := False;
      Flag_Cond   := False;
      Branch_Dest := (No_PC, No_PC);
      FT_Dest     := (No_PC, No_PC);

      --  Look for a control-flow instruction and if Insn_Bin is one, update
      --  its properties. Return without changing anything otherwise.

      case Class is
         when Non_Storage_Relative =>

            --  This is a BRR instruction

            declare
               function Offset_To_PC is new Ada.Unchecked_Conversion
                 (Integer_32, Unsigned_32);

               Offset    : constant Integer_16 :=
                 Get_Relative_Offset (Insn);
               Dest      : constant Pc_Type :=
                 Pc + 4 * (if Offset < 0
                  then Pc_Type (Offset_To_PC (Integer_32 (Offset)))
                  else Pc_Type (Offset));

            begin
               case Condition is
                  when COND_FALSE =>

                     --  The branch is never taken: this is a NOP

                     return;

                  when others =>

                     --  The if branch is always taken: this is a jump,
                     --  otherwise this is a conditional jump.

                     Flag_Cond := Condition /= COND_TRUE;
               end case;
               Branch      := Br_Jmp;
               Branch_Dest := (Dest, Delay_Slot);
               FT_Dest     := (Delay_Slot + 4, Delay_Slot);
            end;

         when Storage_Register =>
            declare
               Operation : constant Operation_Type :=
                 Operation_Type (Get (Insn, Operation_Loc));
            begin
                  if Operation = OP_BRA and then Condition /= COND_FALSE then
                     Branch      := Br_Call;
                     Flag_Indir  := True;
                     Flag_Cond   := Condition /= COND_TRUE;
                     Branch_Dest := (No_PC, Delay_Slot);
                     FT_Dest     := (Delay_Slot + 4, Delay_Slot);
                  end if;
            end;

         when others =>
            null;
      end case;
   end Get_Insn_Properties;

   ----------------
   -- Is_Padding --
   ----------------

   function Is_Padding
     (Self     : LMP_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type) return Boolean
   is
      pragma Unreferenced (Self, Insn_Bin, Pc);
   begin
      return False;
   end Is_Padding;

end Disa_Lmp;
