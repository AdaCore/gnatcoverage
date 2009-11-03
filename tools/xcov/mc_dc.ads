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

with Ada.Containers.Vectors;

with SC_Obligations; use SC_Obligations;

package MC_DC is

   package Condition_Evaluation_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Condition_Index,
      Element_Type => Tristate);

   --  Type Evaluation denotes one evaluation of a decision

   type Evaluation is record
      Decision  : SCO_Id;
      --  The decision being evaluated

      Values    : Condition_Evaluation_Vectors.Vector;
      --  Values of the conditions (True or False if condition has been
      --  evaluated, Unknown if it is masked or not evaluated yet).

      Outcome           : Tristate;
      --  Outcome of the decision with the given set of conditions values
   end record;

   function Is_MC_DC_Pair
     (Eval_1, Eval_2 : Evaluation) return Any_Condition_Index;
   --  For two evaluations Eval_1 and Eval_2 of a decision, determine whether
   --  the two evaluations demonstrate independent influence of a condition on
   --  the decision outcome, and if so, return the index of the condition (note
   --  that any two evaluations can't be an MC/DC independant pair for more
   --  than one condition). No_Condition_Index is returned if this is not an
   --  MC/DC pair for any condition.

end MC_DC;
