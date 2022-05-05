------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2022, AdaCore                     --
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

with Binary_Files;   use Binary_Files;
private with Dis_Opcodes;
with Disa_Symbolize; use Disa_Symbolize;
with Disassemblers;  use Disassemblers;
with Highlighting;
with Traces;         use Traces;

with Ada.Finalization;

package Disa_Lmp is

   type LMP_Disassembler is
     new Ada.Finalization.Limited_Controlled and Disassembler with private;

   overriding function Get_Insn_Length
     (Self     : LMP_Disassembler;
      Insn_Bin : Binary_Content) return Positive;
   --  Return the length of the instruction that starts at Insn_Bin

   overriding procedure Disassemble_Insn
     (Self     : LMP_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type;
      Buffer   : in out Highlighting.Buffer_Type;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class);
   --  Disassemble the instruction at Pc in Insn_Bin and put the text
   --  disassembly in Line/Line_Pos. Line_Pos is the index of the next
   --  character to be written (i.e. line length if Line'First = 1).

   overriding procedure Get_Insn_Properties
     (Self        : LMP_Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest);
   --  Determine whether the instruction at Pc in Insn_Bin is a branch
   --  instruction. Store the kind of branch in Branch, whether it is indirect
   --  (dynamic target) in Flag_Indir and whether it is conditional in
   --  Flag_Cond. Also determine its destinations when possible in
   --  Branch_Dest and FT_Dest (for fallthrough).

   overriding function Is_Padding
     (Self     : LMP_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type) return Boolean;
   --  See disassemblers.ads

   overriding function Is_Nop
     (Self     : LMP_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type) return Boolean is (False);
   --  See disassembler.ads

   overriding procedure Initialize
     (Object : in out LMP_Disassembler);
   --  Override of controlled object primitive

   overriding procedure Finalize
     (Object : in out LMP_Disassembler);
   --  Override of controlled object primitive

private

   type LMP_Disassembler is
     new Ada.Finalization.Limited_Controlled and Disassembler with record
      Handle : Dis_Opcodes.Disassemble_Handle;
   end record;

end Disa_Lmp;
