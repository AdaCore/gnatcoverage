------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2010, AdaCore                     --
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

--  Support for computing lines states from the branch coverage of its
--  instructions and from the coverage level.

package Traces_Lines is

   --  Coverage state of a source line of code

   type Any_Line_State is
     (Not_Covered,
      --  No instructions executed

      Partially_Covered,
      --  The coverage criteria is partially satisfied on this line

      Covered,
      --  The coverage criteria is satisfied

      No_Code,
      --  Initial state: no code for this line

      Exempted_With_Violation,
      --  Exempted line in an exemption block that has a violation

      Exempted_No_Violation
      --  Exempted line in an exemption block with no violation
     );

   subtype Line_State is Any_Line_State range Not_Covered .. No_Code;
   --  Non-exempted line state

   type State_Char_Array is array (Any_Line_State) of Character;
   State_Char : constant State_Char_Array;
   --  Characters identifying a Line_State

   function "*" (L, R : Line_State) return Line_State;
   --  Combine the given individual states to determine a cumulative state

private
   State_Char : constant State_Char_Array :=
     (No_Code                 => '.',
      Not_Covered             => '-',
      Partially_Covered       => '!',
      Covered                 => '+',
      Exempted_With_Violation => '*',
      Exempted_No_Violation   => '#');

end Traces_Lines;
