------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;  use Ada.Text_IO;
with Interfaces;

with Elf_Disassemblers; use Elf_Disassemblers;
with Strings;           use Strings;
with Traces;            use Traces;
with Traces_Elf;        use Traces_Elf;
with Traces_Names;      use Traces_Names;

package body Decision_Map is

   procedure Analyze_Routine
     (Name : String_Acc;
      Info : in out Subprogram_Info);
   --  Build decision map for the given subprogram

   -------------
   -- Analyze --
   -------------

   procedure Analyze is
   begin
      Traces_Names.Iterate (Analyze_Routine'Access);
   end Analyze;

   ---------------------
   -- Analyze_Routine --
   ---------------------

   procedure Analyze_Routine
     (Name : String_Acc;
      Info : in out Subprogram_Info)
   is
      use type Interfaces.Unsigned_32;

      Insns    : Binary_Content renames Info.Insns.all;

      PC       : Pc_Type;
      Insn_Len : Natural;
   begin
      Put_Line ("Building decision map for " & Name.all);

      --  Iterate over instructions, looking for conditional branches

      PC := Insns'First;
      while PC < Insns'Last loop
         Insn_Len :=
           Disa_For_Machine (Machine).
             Get_Insn_Length (Insns (PC .. Insns'Last));

         PC := PC + Pc_Type (Insn_Len);
      end loop;
   end Analyze_Routine;

   --------------
   -- Dump_Map --
   --------------

   procedure Dump_Map is
   begin
      Put_Line ("Dump_Map: Not implemented");
   end Dump_Map;

end Decision_Map;
