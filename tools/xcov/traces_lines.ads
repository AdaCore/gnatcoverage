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

--  Support for computing lines states from the branch coverage of
--  its instructions and from the coverage level.

with Traces; use Traces;

package Traces_Lines is

   type DO178B_Level_Type is (Level_Raw, Level_A, Level_C);
   --  Control the output line state.
   --  Level_Raw gives the most detailed state.

   DO178B_Level : DO178B_Level_Type := Level_Raw;
   --  DO178B level for output state

   --  Coverage state of a source line of code.
   type Line_State is
     (Not_Covered,
      --  No instructions executed

      Partially_Covered,
      --  Some instructions not covered in the line

      Covered, -- Branch_Partially_Covered
      --  Covered at instruction level but some branches partially covered

      Branch_Taken,
      Branch_Fallthrough,
      --  All instructions executed, only one side of branch taken

      Branch_Covered,
      --  Covered at branch level

      Covered_No_Branch,
      --  Same as covered but no branches

      No_Code
      --  Initial state: no code for this line
     );

   subtype Known_Trace_State is
     Trace_State range Not_Covered .. Trace_State'Last;
   type State_Update_Table_Type is array (Line_State, Known_Trace_State)
     of Line_State;

   Update_Table : constant State_Update_Table_Type;
   --  Table to merge trace_states.
   --  The initial Line_State must be No_Code.
   --  Update the line_state using:
   --    New_State := Update_Table (Old_State, Trace_State);

   type State_Map_Array is array (DO178B_Level_Type, Line_State) of Line_State;
   State_Map : constant State_Map_Array;
   --  Degradation of a state according to the level.

   type State_Char_Array is array (Line_State) of Character;
   State_Char : constant State_Char_Array;
   --  Characters identifying a Line_State.

private
   --  Tables below need comments???

   Update_Table : constant State_Update_Table_Type :=
     (
      No_Code =>
        (Not_Covered => Not_Covered,
         Covered => Covered_No_Branch,
         Branch_Taken => Branch_Taken,
         Fallthrough_Taken => Branch_Fallthrough,
         Both_Taken => Branch_Covered),
      Not_Covered =>
        (Not_Covered => Not_Covered,
         others => Partially_Covered),
      Partially_Covered =>
        (others => Partially_Covered),
      Covered =>
        (Not_Covered => Partially_Covered,
         others => Covered),
      Covered_No_Branch =>
        (Not_Covered => Partially_Covered,
         Covered => Covered_No_Branch,
         Branch_Taken => Branch_Taken,
         Fallthrough_Taken => Branch_Fallthrough,
         Both_Taken => Branch_Covered),
      Branch_Taken =>
        (Not_Covered => Partially_Covered,
         Covered => Branch_Taken,
         Branch_Taken => Covered,
         Fallthrough_Taken => Covered,
         Both_Taken => Covered),
      Branch_Fallthrough =>
        (Not_Covered => Partially_Covered,
         Covered => Branch_Fallthrough,
         Branch_Taken => Covered,
         Fallthrough_Taken => Covered,
         Both_Taken => Covered),
      Branch_Covered =>
        (Not_Covered => Partially_Covered,
         Branch_Taken | Fallthrough_Taken => Covered,
         Covered | Both_Taken => Branch_Covered)
      );

   State_Char : constant State_Char_Array :=
     (No_Code => '.',
      Not_Covered => '-',
      Partially_Covered => '!',
      Covered => '?',
      Covered_No_Branch => '+',
      Branch_Taken => '>',
      Branch_Fallthrough => 'v',
      Branch_Covered => '*');

   State_Map : constant State_Map_Array :=
     (Level_Raw => (No_Code => No_Code,
                    Not_Covered => Not_Covered,
                    Partially_Covered => Partially_Covered,
                    Covered => Covered,
                    Covered_No_Branch => Covered_No_Branch,
                    Branch_Taken => Branch_Taken,
                    Branch_Fallthrough => Branch_Fallthrough,
                    Branch_Covered => Branch_Covered),
      Level_A   => (No_Code => No_Code,
                    Not_Covered => Not_Covered,
                    Partially_Covered => Partially_Covered,
                    Covered => Partially_Covered,
                    Covered_No_Branch => Covered_No_Branch,
                    Branch_Taken => Partially_Covered,
                    Branch_Fallthrough => Partially_Covered,
                    Branch_Covered => Covered_No_Branch),
      Level_C   => (No_Code => No_Code,
                    Not_Covered => Not_Covered,
                    Partially_Covered => Covered_No_Branch,
                    Covered => Covered_No_Branch,
                    Covered_No_Branch => Covered_No_Branch,
                    Branch_Taken => Covered_No_Branch,
                    Branch_Fallthrough => Covered_No_Branch,
                    Branch_Covered => Covered_No_Branch));

end Traces_Lines;
