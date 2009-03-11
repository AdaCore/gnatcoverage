------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                      Copyright (C) 2009, AdaCore                         --
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

--  Support for MC/DC coverage analysis

package MC_DC is

   Max_Conditions : constant := 32;
   --  Maximum conditions count in a decision

   type Any_Condition_Index is range 0 .. Max_Conditions;
   No_Condition : constant Any_Condition_Index := 0;
   subtype Condition_Index is Any_Condition_Index range 1 .. Max_Conditions;

   type Condition_Vector_Type is array (Condition_Index) of Boolean;
   pragma Pack (Condition_Vector_Type);
   --  A set of assignments of values to conditions within a decision

   --  Type Evaluation denotes one evaluation of a decision.

   type Evaluation is record
      Values    : Condition_Vector_Type;
      --  Values of the conditions (for a decision with N conditions, only
      --  elements with indices 1 .. N are meaningful).

      Evaluated : Condition_Vector_Type;
      --  For each condition, indicate whether it has been evaluated
      --  (False for masked contitions, in which case the corresponding bit
      --  in Conditions_Vector is meaningless).

      Outcome           : Boolean;
      --  Outcome of the decision with the given set of conditions values

   end record;

   function Is_MC_DC_Pair
     (Conditions_Count : Condition_Index;
      Eval_1, Eval_2   : Evaluation) return Any_Condition_Index;
   --  For two evaluations of a decision with Conditions_Count conditions,
   --  determine whether the two evaluations demonstrate independent influence
   --  of a condition on the decision outcome, and if so, return the index
   --  of the condition (note that any two evaluations can't be an MC/DC
   --  independant pair for more than one condition). No_Condition is
   --  returned if this is not an MC/DC pair for any condition.

end MC_DC;
