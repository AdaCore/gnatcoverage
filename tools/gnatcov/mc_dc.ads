------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                   Copyright (C) 2009-2011, AdaCore                       --
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
      Decision       : SCO_Id;
      --  The decision being evaluated

      Values         : Condition_Evaluation_Vectors.Vector;
      --  Values of the conditions (True or False if condition has been
      --  evaluated, Unknown if it is masked or not evaluated yet).

      Outcome        : Tristate;
      --  Outcome of the decision with the given set of conditions values,
      --  Unknown as long as the evaluation is not completed).

      Next_Condition : Any_Condition_Index;
      --  Next condition expected to be evaluated. Initially 0 before first
      --  condition is evaluated, No_Condition_Index once an outcome has been
      --  reached.
   end record;

   function "<" (L, R : Evaluation) return Boolean;
   --  Operator used to build an ordered set of evaluations

   function Is_MC_DC_Pair
     (Eval_1, Eval_2 : Evaluation;
      Unique_Cause   : Boolean) return Any_Condition_Index;
   --  For two evaluations Eval_1 and Eval_2 of a decision, determine whether
   --  the two evaluations demonstrate independent influence of a condition on
   --  the decision outcome, and if so, return the index of the condition (note
   --  that any two evaluations can't be an MC/DC independant pair for more
   --  than one condition). No_Condition_Index is returned if this is not an
   --  MC/DC pair for any condition.
   --  If Unique_Cause is True, use Unique Cause MC/DC independance, else use
   --  Masking MC/DC independence.

   function Infer_Values
     (Condition : SCO_Id) return Condition_Evaluation_Vectors.Vector;
   --  Deduce the values of conditions preceding the given one in the decision,
   --  in the case where that last condition is reachable through only one
   --  path.

   function Image (E : Evaluation) return String;
   --  Image of E, for reporting purposes

   function Image (EV : Condition_Evaluation_Vectors.Vector) return String;
   --  Image of EV, for reporting purposes

end MC_DC;
