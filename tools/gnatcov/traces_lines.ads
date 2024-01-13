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

      Not_Coverable,
      --  No code for this line, but report presence of SCO

      Undetermined_Coverage,
      --  Code present but coverage assessment impossible (for instance code
      --  not instrumented).

      Exempted_With_Violation,
      --  Exempted line in an exemption block that has a violation

      Exempted_With_Undetermined_Cov,
      --  Exempted line in an exemption block that has an item with
      --  undetermined coverage state (for instance due to it not being
      --  instrumented). For reporting purposes, Exempted_With_Violation should
      --  take precedence over Exempted_With_Undetermined_Cov.

      Exempted_No_Violation
      --  Exempted line in an exemption block with no violation nor
      --  undetermined coverage SCOs. The two previous states should take
      --  precedence over it.
     );

   subtype Line_State is Any_Line_State
     range Not_Covered .. Undetermined_Coverage;
   --  Non-exempted line state

   type State_Char_Array is array (Any_Line_State) of Character;
   State_Char : constant State_Char_Array;
   --  Characters identifying a Line_State

   function "*" (L, R : Line_State) return Line_State;
   --  Combine the given individual states to determine a cumulative state

private
   State_Char : constant State_Char_Array :=
     (No_Code                        => '.',
      Not_Coverable                  => '0',
      Undetermined_Coverage          => '?',
      Not_Covered                    => '-',
      Partially_Covered              => '!',
      Covered                        => '+',
      Exempted_With_Violation        => '*',
      Exempted_With_Undetermined_Cov => '@',
      Exempted_No_Violation          => '#');

end Traces_Lines;
