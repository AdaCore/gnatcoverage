------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2013, AdaCore                     --
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
--  This package uses the same naming convention as the Annex A ("Opcode Map")
--  in Intel's software developper's manual volume 2B, as well as the section
--  A.1 and chapter 2 ("Instruction format") in volume 2A.
--  These manuals can be found at http://www.intel.com/product/manuals/

with Disa_Common;
with Elf_Common;
with Interfaces;   use Interfaces;
with Outputs;      use Outputs;
with Hex_Images;   use Hex_Images;
with Highlighting; use Highlighting;

package body Disa_X86 is

   subtype Byte is Interfaces.Unsigned_8;
   type Bytes is array (Pc_Type range <>) of Byte;
   type Bit_Field_2 is mod 2 ** 2;
   type Bit_Field_3 is mod 2 ** 3;

   type Width_Type is (W_None, W_8, W_16, W_32, W_64, W_128);
   --  Width for operands, addresses and registers

   --  Bits extraction from byte functions

   --  For a byte, MSB (most significant bit) is bit 7 while LSB (least
   --  significant bit) is bit 0.

   function Ext_210 (B : Byte) return Bit_Field_3;
   pragma Inline (Ext_210);
   --  Extract bits 2, 1 and 0

   function Ext_543 (B : Byte) return Bit_Field_3;
   pragma Inline (Ext_543);
   --  Extract bits 5-3 of byte B

   function Ext_76 (B : Byte) return Bit_Field_2;
   pragma Inline (Ext_76);
   --  Extract bits 7-6 of byte B

   Bad_Memory : exception;

   type Mem_Read is access function (Off : Pc_Type) return Byte;

   function Decode_Val
     (Mem            : Mem_Read;
      Off            : Pc_Type;
      Width          : Width_Type;
      Sign_Extend    : Boolean)
     return Unsigned_64;
   --  Decode an immediate value given its memory location, its size and its
   --  signedness.
   --  Off is the immediate address and is relative to a certain PC. Mem is a
   --  function that reads one byte at an offset from this PC.

   function Truncate_To_Pc_Type (Value : Unsigned_64) return Pc_Type is
     (Pc_Type (Value and Unsigned_64 (Pc_Type'Last)));

   -------------
   -- Ext_210 --
   -------------

   function Ext_210 (B : Byte) return Bit_Field_3 is
   begin
      return Bit_Field_3 (B and 2#111#);
   end Ext_210;

   -------------
   -- Ext_543 --
   -------------

   function Ext_543 (B : Byte) return Bit_Field_3 is
   begin
      return Bit_Field_3 (Shift_Right (B, 3) and 2#111#);
   end Ext_543;

   ------------
   -- Ext_76 --
   ------------

   function Ext_76 (B : Byte) return Bit_Field_2 is
   begin
      return Bit_Field_2 (Shift_Right (B, 6) and 2#11#);
   end Ext_76;

   function Ext_Modrm_Mod (B : Byte) return Bit_Field_2 renames Ext_76;
   function Ext_Modrm_Rm  (B : Byte) return Bit_Field_3 renames Ext_210;
   function Ext_Modrm_Reg (B : Byte) return Bit_Field_3 renames Ext_543;

   function Ext_Sib_Base  (B : Byte) return Bit_Field_3 renames Ext_210;
   function Ext_Sib_Index (B : Byte) return Bit_Field_3 renames Ext_543;
   function Ext_Sib_Scale (B : Byte) return Bit_Field_2 renames Ext_76;

   ----------------
   -- Decode_Val --
   ----------------

   function Decode_Val
     (Mem         : Mem_Read;
      Off         : Pc_Type;
      Width       : Width_Type;
      Sign_Extend : Boolean)
     return Unsigned_64
   is
      Is_Negative : Boolean;
      V : Unsigned_64;

      subtype Sign_Extension_Width_Type is Width_Type range W_8 .. W_64;
      type Sign_Extension_Type is
         array (Sign_Extension_Width_Type) of Unsigned_64;
      Sign_Extension : constant Sign_Extension_Type :=
        (16#ffff_ffff_ffff_ff00#,
         16#ffff_ffff_ffff_0000#,
         16#ffff_ffff_0000_0000#,
         16#0000_0000_0000_0000#);
   begin
      --  For each size, once the value is read from memory, sign extend if
      --  needed.

      case Width is
         when W_8 =>
            V := Unsigned_64 (Mem (Off));
            Is_Negative := Sign_Extend and then V >= 16#80#;

         when W_16 =>
            V := Shift_Left (Unsigned_64 (Mem (Off + 1)), 8)
              or Unsigned_64 (Mem (Off));
            Is_Negative := Sign_Extend and then V >= 16#8000#;

         when W_32 =>
            V := Shift_Left (Unsigned_64 (Mem (Off + 3)), 24)
              or Shift_Left (Unsigned_64 (Mem (Off + 2)), 16)
              or Shift_Left (Unsigned_64 (Mem (Off + 1)), 8)
              or Shift_Left (Unsigned_64 (Mem (Off + 0)), 0);
            Is_Negative := Sign_Extend and then V >= 16#8000_0000#;

         when W_64 =>
            V := Shift_Left (Unsigned_64 (Mem (Off + 7)), 56)
              or Shift_Left (Unsigned_64 (Mem (Off + 6)), 48)
              or Shift_Left (Unsigned_64 (Mem (Off + 5)), 40)
              or Shift_Left (Unsigned_64 (Mem (Off + 4)), 32)
              or Shift_Left (Unsigned_64 (Mem (Off + 3)), 24)
              or Shift_Left (Unsigned_64 (Mem (Off + 2)), 16)
              or Shift_Left (Unsigned_64 (Mem (Off + 1)), 8)
              or Shift_Left (Unsigned_64 (Mem (Off + 0)), 0);
            Is_Negative := Sign_Extend and then V >= 16#8000_0000_0000_0000#;

         when W_128 =>
            raise Invalid_Insn with "invalid 128-bit immediate decoding";

         when W_None =>
            raise Invalid_Insn;
      end case;

      if Is_Negative then
         V := Sign_Extension (Width) or V;
      end if;

      return V;
   end Decode_Val;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Object : in out X86_Disassembler) is
   begin
      Object.Handle := Dis_Opcodes.Create_X86_Disassembler;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Object : in out X86_Disassembler) is
   begin
      Dis_Opcodes.Delete_Disassembler (Object.Handle);
   end Finalize;

   ----------------------
   -- Disassemble_Insn --
   ----------------------

   procedure Disassemble_Insn
     (Self     : X86_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type;
      Buffer   : in out Highlighting.Buffer_Type;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class) is
   begin
      Disa_Common.Opcodes_Disassemble_Insn
        (Self.Handle, Insn_Bin, Pc, Buffer, Insn_Len, Sym, 15);
   end Disassemble_Insn;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   function Get_Insn_Length
     (Self     : X86_Disassembler;
      Insn_Bin : Binary_Content) return Positive
   is
      Buffer   : Highlighting.Buffer_Type (0);
      Len      : Natural;

   begin
      Disassemble_Insn
        (Self, Insn_Bin, Insn_Bin.First, Buffer, Len, Nul_Symbolizer);
      return Len;
   end Get_Insn_Length;

   -------------------------
   -- Get_Insn_Properties --
   -------------------------

   procedure Get_Insn_Properties
     (Self        : X86_Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest)
   is
      Is_64bit   : constant Boolean := Machine = Elf_Common.EM_X86_64;

      Opcode_Off : Pc_Type := 0;
      B, B1      : Byte;

      function Mem (Off : Pc_Type) return Byte;

      function Length return Pc_Type is
        (Pc_Type (Get_Insn_Length (Self, Insn_Bin)));

      ---------
      -- Mem --
      ---------

      function Mem (Off : Pc_Type) return Byte is
      begin
         if Opcode_Off + Off > Length (Insn_Bin)  then
            raise Bad_Memory;
         end if;
         return Get (Insn_Bin, Insn_Bin.First + Opcode_Off + Off);
      end Mem;

   --  Start of processing for Get_Insn_Properties

   begin
      --  Make sure OUT parameters have a valid value

      Branch      := Br_None;
      Flag_Indir  := False;
      Flag_Cond   := False;
      Branch_Dest := (No_PC, No_PC);
      FT_Dest     := (No_PC, No_PC);

      B := Get (Insn_Bin, Insn_Bin.First);

      --  Discard any REX prefix in 64-bit mode and REP/REPNE ones

      while (Is_64bit and then (B and 16#f0#) = 16#40#)
            or else
         (B = 16#f2# or else B = 16#f3#)
      loop
         Opcode_Off := Opcode_Off + 1;
         B := Get (Insn_Bin, Insn_Bin.First + Opcode_Off);
      end loop;

      case B is
         when 16#70# .. 16#7f#
           | 16#e0# .. 16#e2#
           | 16#e3# =>
            --  Jcc Jb / Loop Jb / jrcxz
            Branch     := Br_Jmp;
            Flag_Cond  := True;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 2;
            Branch_Dest.Target :=
              FT_Dest.Target
                + Truncate_To_Pc_Type
              (Decode_Val (Mem'Unrestricted_Access, 1, W_8, True));
            return;

         when 16#0f# =>
            B := Get (Insn_Bin, Insn_Bin.First + 1);
            if B in 16#80# .. 16#8f# then
               --  Jcc Jz
               Branch     := Br_Jmp;
               Flag_Cond  := True;
               Flag_Indir := False;
               FT_Dest.Target := Pc + 6;
               Branch_Dest.Target :=
                 FT_Dest.Target
                   + Truncate_To_Pc_Type
                 (Decode_Val (Mem'Unrestricted_Access, 2, W_32, True));
            end if;
            return;

         when 16#c2# --  ret
           | 16#c3#
           | 16#ca#  --  retf
           | 16#cb#
           | 16#cf# =>  -- iret
            Branch     := Br_Ret;
            Flag_Cond  := False;
            Flag_Indir := True;
            return;

         when 16#e8# =>
            --  Call near, relative (32-bit offset in both 32-bit and 64-bit
            --  mode).
            Branch     := Br_Call;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 5;
            Branch_Dest.Target :=
              FT_Dest.Target
                + Truncate_To_Pc_Type
              (Decode_Val (Mem'Unrestricted_Access, 1, W_32, True));
            return;

         when 16#9a# =>
            --  Callf, doesn't exist in 64-bit
            Branch     := Br_Call;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 5;
            Branch_Dest.Target :=
              Truncate_To_Pc_Type
                (Decode_Val (Mem'Unrestricted_Access, 1, W_32, False));
            return;

         when 16#e9# =>
            --  jmp rel32
            Branch     := Br_Jmp;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 5;
            Branch_Dest.Target :=
              FT_Dest.Target
                + Truncate_To_Pc_Type
              (Decode_Val (Mem'Unrestricted_Access, 1, W_32, True));
            return;

         when 16#ea# =>
            --  jmp ptr32, doesn't exist in 64-bit
            Branch     := Br_Jmp;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 5;
            Branch_Dest.Target :=
              Truncate_To_Pc_Type
                (Decode_Val (Mem'Unrestricted_Access, 1, W_32, False));
            return;

         when 16#eb# =>
            --  jmp rel8
            Branch     := Br_Jmp;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 2;
            Branch_Dest.Target :=
              FT_Dest.Target
                + Truncate_To_Pc_Type
              (Decode_Val (Mem'Unrestricted_Access, 1, W_8, True));
            return;

         when 16#ff# =>
            B1 := Get (Insn_Bin, Insn_Bin.First + 1);
            case Ext_543 (B1) is
               when 2#010# | 2#011# =>
                  --  call / callf, absolute indirect
                  Branch     := Br_Call;
                  Flag_Cond  := False;
                  Flag_Indir := True;
                  FT_Dest.Target := Pc + Length;
                  return;

               when 2#100# | 2#101# =>
                  --  jmp / jmpf, absolute indirect
                  Branch     := Br_Jmp;
                  Flag_Cond  := False;
                  Flag_Indir := True;
                  FT_Dest.Target := Pc + Length;
                  return;

               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;

   exception
      when Bad_Memory =>
         Warn ("assembler analysis truncated at PC = " & Hex_Image (Pc));
   end Get_Insn_Properties;

   ----------------
   -- Is_Padding --
   ----------------

   overriding function Is_Padding
     (Self     : X86_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type) return Boolean
   is
      pragma Unreferenced (Self);

      PC_Cursor : Pc_Type := Pc;
      --  The memory accessors below fetch bytes from an offset plus this
      --  address.  This address is incremented as prefix bytes are discovered
      --  so that memory accesses have constant offsets.

      function Mem (Off : Pc_Type) return Byte;
      function Mem (Off, Size : Pc_Type) return Bytes;

      ---------
      -- Mem --
      ---------

      function Mem (Off : Pc_Type) return Byte is
      begin
         if Off > Length (Insn_Bin) then
            raise Bad_Memory;
         end if;
         return Get (Insn_Bin, PC_Cursor + Off);
      end Mem;

      ---------
      -- Mem --
      ---------

      function Mem (Off, Size : Pc_Type) return Bytes is
         Result : Bytes (PC_Cursor + Off .. PC_Cursor + Off + Size - 1);
      begin
         if Off + Size > Length (Insn_Bin) then
            raise Bad_Memory;
         end if;
         for I in Result'Range loop
            Result (I) := Get (Insn_Bin, I);
         end loop;
         return Result;
      end Mem;

   begin
      --  In the Intel Manuals, there is a list of "Recommented Multi-Byte
      --  Sequence of NOP Instruction", in which all instructions have "nop"
      --  mnemonics.  However we noticed that linker sometimes insert other
      --  sequences for NOPs: mostly LEA instructions: also match these as
      --  NOPs.

      --  Strip the operand size override prefix

      if Mem (0) = 16#66# then
         PC_Cursor := PC_Cursor + 1;
      end if;

      case Mem (0) is
         when 16#0f# =>
            --  All "NOP DWORD ptr [...]" instructions start with 0F 1F:
            --  assuming this instruction is valid, there is no need to decode
            --  further.
            return Mem (1) = 16#1f#;

         when 16#90# =>
            --  NOP
            return True;

         when 16#8d# =>
            declare
               --  LEA [...]
               ModRM     : constant Byte        := Mem (1);
               Mod_Value : constant Bit_Field_2 := Ext_Modrm_Mod (ModRM);
               RM        : constant Bit_Field_3 := Ext_Modrm_Rm (ModRM);
               Reg       : constant Bit_Field_3 := Ext_Modrm_Reg (ModRM);
            begin
               case RM is
               when 2#100# =>
                  if Mod_Value = 2#11# then
                     return False;
                  end if;

                  --  This corresponds to the [--][--] +disp8/32 lines in
                  --  Intel's manual: we have a SIB byte.
                  declare
                     SIB   : constant Byte := Mem (2);
                     Scale : constant Bit_Field_2 := Ext_Sib_Scale (SIB);
                     Index : constant Bit_Field_3 := Ext_Sib_Index (SIB);
                     Base  : constant Bit_Field_3 := Ext_Sib_Base (SIB);
                  begin
                     if Base = 2#101# then
                        --  The base depends on Mod_Value...
                        if Mod_Value /= 2#00# then
                           --  %ebp is part of the computation: this cannot be
                           --  a NOP.
                           return False;
                        end if;
                        --  At this point, we have a NOP iff the scaled index
                        --  is 1*Reg and the (32-bit) displacement is 0.
                        return (Scale = 0 and then Index = Reg
                                  and then
                                Mem (3, 4) = (0, 0, 0, 0));

                     --  Past this point, the base address is a register
                     elsif Index = 2#100# then
                        --  If we have no index to scale, this is a NOP iff
                        --  we're just copying a register to itself
                        return Base = Reg;

                     --  Past this point, the base address is a register and we
                     --  have an index to scale: this cannot be a NOP.
                     else
                        return False;
                     end if;
                  end;

               when 2#101# =>
                  --  Loading something independent of a register in the
                  --  register: actually doing something.
                  return False;

               when others =>
                  --  This is a NOP iff loading the register with its own
                  --  value.

                  --  Source and destination must be the same register
                  if RM /= Reg then
                     return False;
                  end if;

                  --  Check the displacement is null (if any)
                  return (case Mod_Value is
                          when 2#00#  => True,
                          when 2#01#  => Mem (2) = 0,
                          when 2#10#  => Mem (2, 4) = (0, 0, 0, 0),
                          when others => False);
               end case;
            end;

         when others =>
            return False;
      end case;

   exception
      when Bad_Memory =>
         Warn ("assembler analysis truncated at PC = " & Hex_Image (Pc));
         return False;
   end Is_Padding;

end Disa_X86;
