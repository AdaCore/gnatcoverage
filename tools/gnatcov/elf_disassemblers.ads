------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Ada.Containers.Ordered_Sets;

with Disassemblers; use Disassemblers;
with Traces;        use Traces;

package Elf_Disassemblers is

   type Insn_Set_Type is (Default, Data, ARM, Thumb);
   --  Even for a specific machine, there can be multiple instruction sets, and
   --  thus multiple disassemblers. This type provides enumerators to designate
   --  a specific instruction set. Data indicates that there is no instruction
   --  to disassemble.

   function Disa_For_Machine
     (Machine  : Machine_Type;
      Insn_Set : Insn_Set_Type) return access Disassembler'Class;
   pragma Inline (Disa_For_Machine);

   type Insn_Set_Ranges is private;
   type Insn_Set_Ranges_Acc is access Insn_Set_Ranges;
   type Insn_Set_Ranges_Cst_Acc is access constant Insn_Set_Ranges;

   No_Insn_Set_Ranges : aliased constant Insn_Set_Ranges;

   --  Collection of associations: address range -> instruction set

   procedure Free (I_Range : in out Insn_Set_Ranges_Acc);

   procedure Add_Range
     (Ranges      : in out Insn_Set_Ranges;
      First, Last : Pc_Type;
      Insn_Set    : Insn_Set_Type);
   --  Add an association: address range -> instruction set

   type Insn_Set_Cache is private;
   --  When disassembling a lot of instructions, performing a full instruction
   --  set lookup for each is costly. This acts as a cache for the lookup so
   --  that as few lookups as possible are done when disassembling contiguous
   --  instructions.

   Empty_Cache : constant Insn_Set_Cache;

   function Get_Insn_Set
     (Ranges : Insn_Set_Ranges;
      Cache  : in out Insn_Set_Cache;
      PC     : Pc_Type) return Insn_Set_Type;
   --  Return the instruction set associated to the PC address. Return Default
   --  is there is no such association.

   function Go_To_Next_Insn
     (Ranges   : Insn_Set_Ranges;
      Cache    : in out Insn_Set_Cache;
      PC       : in out Pc_Type;
      Insn_Set : out Insn_Set_Type) return Boolean;
   --  Move Cache/PC to the address of the next instruction to decode
   --  (including PC, if it is inside a code block) and update Insn_Set for
   --  this instruction. This must be used only when Get_Insn_Set returned
   --  Data. Return whether we found an instruction (i.e. True when we did
   --  and False when the end of section is reached).
   --
   --  This is useful because in ARM, code section can contain both code and
   --  data interleaved. This procedure makes it easy to skip to the next
   --  instruction.

   function Disa_For_Machine
     (Machine : Machine_Type;
      Ranges  : Insn_Set_Ranges;
      Cache   : in out Insn_Set_Cache;
      PC      : Pc_Type) return access Disassembler'Class
   is (Disa_For_Machine (Machine, Get_Insn_Set (Ranges, Cache, PC)));
   --  Shortcut for Disa_For_Machine that automatically computes the
   --  instruction set.

   function Iterate_Over_Insns
     (Ranges   : Insn_Set_Ranges;
      Cache    : in out Insn_Set_Cache;
      Last_PC  : Pc_Type;
      PC       : in out Pc_Type;
      Insn_Set : out Insn_Set_Type) return Boolean;
   --  Utility around Go_To_Next_Insn: return whether we found in an code
   --  block an address greater or equal to PC but lesser or equal to Last_PC.
   --  This makes disassembly loops concise.
   --
   --  Common pitfal: in order to get the next instruction, one has to
   --  increase PC with the size of the current instruction before calling
   --  this function. Typical disassembly loops will look like:
   --
   --     while Iterate_Over_Insns (Ranges, Cache, Insn.Last, PC, Insn_Set)
   --     loop
   --        Next_PC :=
   --           PC + Disa_For_Machine (Machine, Insn_set).Get_Insn_Length
   --             (Slice (Insn, PC, Insn.Last));
   --        --  ... perform work on instruction at PC...
   --        PC := Next_PC;
   --     end loop;
private

   type Insn_Set_Range is record
      First, Last : Pc_Type;
      Insn_Set    : Insn_Set_Type;
   end record;

   function "<" (L, R : Insn_Set_Range) return Boolean;

   package Ranges_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Insn_Set_Range);

   type Insn_Set_Ranges is new Ranges_Sets.Set with null record;

   type Insn_Set_Cache is record
      Cur : Ranges_Sets.Cursor;
   end record;

   No_Insn_Set_Ranges : aliased constant Insn_Set_Ranges :=
     (Ranges_Sets.Empty_Set with null record);
   Empty_Cache : constant Insn_Set_Cache := (Cur => Ranges_Sets.No_Element);

end Elf_Disassemblers;
