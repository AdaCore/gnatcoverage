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

with Ada.Unchecked_Conversion;

with Arch;        use Arch;
with Dis_Opcodes; use Dis_Opcodes;
with Swaps;       use Swaps;
with Traces;      use Traces;

package body Disa_Common is

   function Opcodes_Run_Disassembler
     (Handle       : Dis_Opcodes.Disassemble_Handle;
      Insn_Bin     : Binary_Content;
      Pc           : Traces.Pc_Type;
      Insn_Max_Len : Positive;
      Sym          : Symbolizer'Class;
      Buffer       : out C.char_array) return Positive;
   --  Helper to factorize common code for the public Opcodes_* subprograms.
   --  Run the libopcode Handle disassembler on the instruction in Insn_Bin at
   --  Pc and put the disassembly text in Buffer. Return the length in bytes of
   --  the disassembled instruction.
   --
   --  See Opcodes_Disassemble_Insn for the meaning of Insn_Max_Len and Sym.

   ----------------
   -- ELF_To_U16 --
   ----------------

   function ELF_To_U16 (Bin : Binary_Content) return Unsigned_16 is
      pragma Assert (Length (Bin) = 2);

      type Bin_U16 is array (Elf_Addr range 1 .. 2) of Unsigned_8;

      Bin_Aligned : Bin_U16 := Bin_U16 (Bin.Content (0 .. 1));
      for Bin_Aligned'Alignment use Unsigned_16'Alignment;

      Result : Unsigned_16;
      pragma Import (Ada, Result);
      for Result'Address use Bin_Aligned'Address;
   begin
      if Big_Endian_Host /= Big_Endian_ELF then
         Swap_16 (Result);
      end if;
      return Result;
   end ELF_To_U16;

   ----------------
   -- ELF_To_U32 --
   ----------------

   function ELF_To_U32 (Bin : Binary_Content) return Unsigned_32 is
      pragma Assert (Length (Bin) = 4);

      type Bin_U32 is array (Elf_Addr range 1 .. 4) of Unsigned_8;

      Bin_Aligned : Bin_U32 := Bin_U32 (Bin.Content (0 .. 3));
      for Bin_Aligned'Alignment use Unsigned_32'Alignment;

      Result : Unsigned_32;
      pragma Import (Ada, Result);
      for Result'Address use Bin_Aligned'Address;
   begin
      if Big_Endian_Host /= Big_Endian_ELF then
         Swap_32 (Result);
      end if;
      return Result;
   end ELF_To_U32;

   -----------------------
   -- To_Big_Endian_U32 --
   -----------------------

   function To_Big_Endian_U32 (Bin : Binary_Content) return Unsigned_32 is
      pragma Assert (Length (Bin) = 4);

      type Bin_U32 is array (Elf_Addr range 1 .. 4) of Unsigned_8;

      Bin_Aligned : Bin_U32 := Bin_U32 (Bin.Content (0 .. 3));
      for Bin_Aligned'Alignment use Unsigned_32'Alignment;

      Result : Unsigned_32;
      pragma Import (Ada, Result);
      for Result'Address use Bin_Aligned'Address;
   begin
      if not Big_Endian_Host then
         Swap_32 (Result);
      end if;
      return Result;
   end To_Big_Endian_U32;

   -----------------------
   -- Print_Symbol_Func --
   -----------------------

   function Print_Symbol_Func
     (Addr           : BFD_VMA;
      Symbol_Manager : System.Address;
      Buff_Addr      : System.Address;
      Buff_Size      : int) return int
   is

      type Symbolizer_Access is access Symbolizer'Class;

      function Symbolizer_Cast is new
        Ada.Unchecked_Conversion (System.Address, Symbolizer_Access);

      SBuff  : String (1 .. Integer (Buff_Size));
      for SBuff'Address use Buff_Addr;
      Symb   : Symbolizer_Access renames Symbolizer_Cast (Symbol_Manager);
      Symbol : constant String := Symb.Symbolize (Pc_Type'Mod (Addr));
      --  When Libopcode is built in 64bits mode, BFD_VMA is 64bits wide
      --  regardless of the target arch for the disassembled code.
      --  The disassembler does not take care of wrapping addresses to 32bits
      --  when computing them so we need to do it whenever the disassembler
      --  provides us with and address.

      Copy_Size : constant Natural :=
        Natural'Min (Symbol'Length, Natural (Buff_Size));
   begin
      SBuff (1 .. Copy_Size) :=
        Symbol (Symbol'First .. Symbol'First + Copy_Size - 1);

      return int (Copy_Size);
   end Print_Symbol_Func;

   ------------------------------
   -- Opcodes_Run_Disassembler --
   ------------------------------

   function Opcodes_Run_Disassembler
     (Handle       : Dis_Opcodes.Disassemble_Handle;
      Insn_Bin     : Binary_Content;
      Pc           : Traces.Pc_Type;
      Insn_Max_Len : Positive;
      Sym          : Symbolizer'Class;
      Buffer       : out C.char_array) return Positive
   is
      pragma Assert (Big_Endian_ELF_Initialized);

      Insn_Bytes : Dis_Opcodes.BFD_Byte_Array (0 .. Insn_Max_Len - 1)
      with Import, Address => Insn_Bin.Content.all'Address;
   begin
      Dis_Opcodes.Set_Disassembler_Symbolizer
        (Handle, Sym'Address, Disa_Common.Print_Symbol_Func'Access);

      return
        Positive
          (Dis_Opcodes.Disassemble_To_Text
             (DH          => Handle,
              Pc          => Dis_Opcodes.BFD_VMA (Pc),
              Dest        => Buffer,
              Dest_Size   => Buffer'Length,
              Insn_Buffer => Insn_Bytes,
              Ib_Size     =>
                C.unsigned
                  (Unsigned_32'Min
                     (Insn_Bytes'Length, Unsigned_32 (Length (Insn_Bin)))),
              Endian      =>
                (if Big_Endian_ELF
                 then Dis_Opcodes.BFD_ENDIAN_BIG
                 else Dis_Opcodes.BFD_ENDIAN_LITTLE)));
   end Opcodes_Run_Disassembler;

   ------------------------------
   -- Opcodes_Disassemble_Insn --
   ------------------------------

   procedure Opcodes_Disassemble_Insn
     (Handle       : Dis_Opcodes.Disassemble_Handle;
      Insn_Bin     : Binary_Content;
      Pc           : Pc_Type;
      Buffer       : in out Highlighting.Buffer_Type;
      Insn_Len     : out Positive;
      Sym          : Symbolizer'Class;
      Insn_Max_Len : Positive)
   is
      Buff : C.char_array := (C.size_t (1) .. C.size_t (256) => <>);
   begin
      Insn_Len :=
        Opcodes_Run_Disassembler
          (Handle, Insn_Bin, Pc, Insn_Max_Len, Sym, Buff);
      Buffer.Start_Token (Highlighting.Text);
      Buffer.Put (C.To_Ada (Buff));
   end Opcodes_Disassemble_Insn;

   -----------------------------
   -- Opcodes_Get_Insn_Length --
   -----------------------------

   function Opcodes_Get_Insn_Length
     (Handle       : Dis_Opcodes.Disassemble_Handle;
      Insn_Bin     : Binary_Content;
      Pc           : Traces.Pc_Type;
      Insn_Max_Len : Positive) return Positive
   is
      Buff : C.char_array := (1 .. 0 => <>);
   begin
      return
        Opcodes_Run_Disassembler
          (Handle, Insn_Bin, Pc, Insn_Max_Len, Nul_Symbolizer, Buff);
   end Opcodes_Get_Insn_Length;

end Disa_Common;
