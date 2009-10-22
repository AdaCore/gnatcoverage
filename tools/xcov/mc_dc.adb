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

package body MC_DC is

   function Find_True
     (Conditions_Count : Condition_Index;
      CV : Condition_Vector_Type) return Any_Condition_Index;
   pragma Unreferenced (Find_True);
   --  Return the lowest index in CV (1 .. Conditions_Count) for which
   --  CV (Index) is True.
   --  Return No_Condition if none such index exists.

   ---------------
   -- Find_True --
   ---------------

   function Find_True
     (Conditions_Count : Condition_Index;
      CV : Condition_Vector_Type) return Any_Condition_Index
   is
   begin
      for J in 1 .. Conditions_Count loop
         if CV (J) then
            return J;
         end if;
      end loop;
      return No_Condition;
   end Find_True;

   -------------------
   -- Is_MC_DC_Pair --
   -------------------

   function Is_MC_DC_Pair
     (Conditions_Count : Condition_Index;
      Eval_1, Eval_2   : Evaluation) return Any_Condition_Index
   is
      Evaluated_Both : constant Condition_Vector_Type :=
                         Eval_1.Evaluated and Eval_2.Evaluated;
      Evaluated_Different : Condition_Vector_Type :=
                              Evaluated_Both
                                 and
                              (Eval_1.Values xor Eval_2.Values);
      First_Different : Any_Condition_Index;
   begin
      --  Not an MC/DC pair if same outcome in both cases, or if no condition
      --  was evaluated in both cases and with different values.

      if (Eval_1.Outcome = Eval_2.Outcome)
           or else
         Evaluated_Different = Condition_Vector_Type'(others => False)
      then
         return No_Condition;
      end if;

      --  Find first condition that has different non-masked values in
      --  both evaluations.

      First_Different := No_Condition;
      for J in 1 .. Conditions_Count loop
         if Evaluated_Different (J) then
            First_Different := J;
            exit;
         end if;
      end loop;
      pragma Assert (First_Different /= No_Condition);

      --  Mask out first condition with different values in either evaluation

      Evaluated_Different (First_Different) := False;

      if Evaluated_Different = Condition_Vector_Type'(others => False) then
         --  No other condition differs: this is a pair showing independent
         --  influence of the first different condition.

         return First_Different;

      else
         --  Other conditions differ: no independent influence

         return No_Condition;
      end if;
   end Is_MC_DC_Pair;

end MC_DC;
