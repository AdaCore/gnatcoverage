------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

--  Abstract support for disassembly engines

with Traces;         use Traces;
with Traces_Elf;     use Traces_Elf;
with Disa_Symbolize; use Disa_Symbolize;

package Disassemblers is

   type Disassembler is limited interface;

   function Get_Insn_Length
     (Self     : Disassembler;
      Insn_Bin : Binary_Content) return Positive is abstract;
   --  Return the length of the instruction at the beginning of Insn_Bin

   procedure Disassemble_Insn
     (Self     : Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type;
      Line     : out String;
      Line_Pos : out Natural;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class) is abstract;
   --  Disassemble instruction at ADDR, and put the result in LINE/LINE_POS.
   --  LINE_POS is the index of the next character to be written (ie line
   --  length if Line'First = 1).

   type Dest is record
      Target     : Pc_Type;
      --  Target address of the branch destination

      Delay_Slot : Pc_Type := No_PC;
      --  Set to the delay slot address
   end record;

   function "<" (Left, Right : Dest) return Boolean;
   --  Lexicographical order

   procedure Get_Insn_Properties
     (Self        : Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest) is abstract;
   --  Determine whether the given instruction, located at PC, is a branch
   --  instruction of some kind (indicated by Branch).
   --  For a branch, indicate whether it is indirect (Flag_Indir) and whether
   --  it is conditional (Flag_Cond), and determine its destination
   --  (Branch_Dest); if it is conditional, determine the destination if the
   --  condition is no verified (FT_Dest).

   --  Note: Delay_Slot needs to be set even if the case of a fallthrough
   --  destination, where the Target will be the next sequential instruction
   --  after the delay slot.

end Disassemblers;
