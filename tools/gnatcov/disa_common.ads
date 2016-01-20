------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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

--  Common subprograms shared by several disassembler components

with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with System;

with Binary_Files; use Binary_Files;
with Dis_Opcodes;
with Disa_Symbolize; use Disa_Symbolize;
with Highlighting;
with Traces;

package Disa_Common is

   function ELF_To_U16 (Bin : Binary_Content) return Unsigned_16;
   --  Decode a 16bit value from ELF to native endianness

   function ELF_To_U32 (Bin : Binary_Content) return Unsigned_32;
   --  Decode a 32bit value from ELF to native endianness

   function To_Big_Endian_U32 (Bin : Binary_Content) return Unsigned_32;
   --  Convert big endian binary representation to Unsigned_32 value

   procedure Opcodes_Disassemble_Insn
     (Handle       : Dis_Opcodes.Disassemble_Handle;
      Insn_Bin     : Binary_Content;
      Pc           : Traces.Pc_Type;
      Buffer       : in out Highlighting.Buffer_Type;
      Insn_Len     : out Natural;
      Sym          : Symbolizer'Class;
      Insn_Max_Len : Positive)
     with Inline => True;
   --  Common function to disassemble through libopcode bindings.
   --  Disassemble instruction at ADDR, and put the result in LINE/LINE_POS.
   --  LINE_POS is the index of the next character to be written (ie line
   --  length if Line'First = 1).
   --  INSN_MAX_LEN must correspond to the size of the largest instruction
   --  available on the architecture HANDLE was created for.

   function Print_Symbol_Func
     (Addr           : Dis_Opcodes.BFD_VMA;
      Symbol_Manager : System.Address;
      Buff_Addr      : System.Address;
      Buff_Size      : int) return int
     with Convention => C,
          Post => Print_Symbol_Func'Result <= Buff_Size;
   --  Prints symbol at address Addr to Buff if there is one.
   --  Does nothing otherwise.
   --  Writes at most Buff_Size characters.
   --  Returns number of characters written.

end Disa_Common;
