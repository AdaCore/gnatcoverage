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

   --  Coverage state of a source line of code.
   type Line_State is
     (Not_Covered,
      --  No instructions executed

      Partially_Covered,
      --  The coverage criteria is partially satisfied on this line

      Covered,
      --  The coverage criteria is satisfied

      No_Code
      --  Initial state: no code for this line
     );

   subtype Known_Trace_State is
     Trace_State range Not_Covered .. Trace_State'Last;
   type State_Update_Table_Type is array (Line_State, Known_Trace_State)
     of Line_State;

   procedure Update_Line_State (L : in out Line_State;
                                I : Known_Trace_State);
   --  Update a line state with the object coverage status of one
   --  of its instructions.
   --  This is typically meant to be used to compute the state of a source
   --  line by iterating on its instructions and, for each of them, calling
   --  Update_Line_State on its state.
   --  ??? This works for both instruction coverage and branch coverage...
   --  Source coverage would require a different procedure (Taking, say, a
   --  decision state instead of a instruction trace state).

   type State_Char_Array is array (Line_State) of Character;
   State_Char : constant State_Char_Array;
   --  Characters identifying a Line_State.

private
   State_Char : constant State_Char_Array :=
     (No_Code => '.',
      Not_Covered => '-',
      Partially_Covered => '!',
      Covered => '+');

end Traces_Lines;
