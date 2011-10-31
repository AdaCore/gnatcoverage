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

package body MC_DC is

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Evaluation) return Boolean is
      use type Ada.Containers.Count_Type;
   begin
      if L.Decision /= R.Decision then
         return L.Decision < R.Decision;
      end if;

      for J in R.Values.First_Index .. R.Values.Last_Index loop
         if J > L.Values.Last_Index then
            return True;

         elsif L.Values.Element (J) < R.Values.Element (J) then
            return True;

         elsif L.Values.Element (J) > R.Values.Element (J) then
            return False;
         end if;
      end loop;

      --  Here when L.Values = R.Values

      pragma Assert (L.Outcome = R.Outcome);
      return False;
   end "<";

   -----------
   -- Image --
   -----------

   function Image (E : Evaluation) return String is
      Masked_Len : constant Integer :=
                     Integer (Last_Cond_Index (E.Decision)
                                - E.Values.Last_Index);
      --  Count of masked conditions after the last non-masked one.

      Masked_Values : constant String (1 .. Masked_Len) := (others => '-');
   begin
      return Image (E.Values) & Masked_Values & " -> " & E.Outcome'Img;
   end Image;

   function Image (EV : Condition_Evaluation_Vectors.Vector) return String is
      Cond_Vector : String (1 .. Integer (EV.Length));
   begin
      for J in EV.First_Index .. EV.Last_Index loop
         case EV.Element (J) is
            when False   =>
               Cond_Vector (1 + Integer (J - EV.First_Index)) := 'F';
            when True    =>
               Cond_Vector (1 + Integer (J - EV.First_Index)) := 'T';
            when Unknown =>
               Cond_Vector (1 + Integer (J - EV.First_Index)) := '-';
         end case;
      end loop;

      return Cond_Vector;
   end Image;

   ------------------
   -- Infer_Values --
   ------------------

   function Infer_Values
     (Condition : SCO_Id) return Condition_Evaluation_Vectors.Vector
   is
      use Ada.Containers;
      use Condition_Evaluation_Vectors;

      D_SCO : constant SCO_Id := Enclosing_Decision (Condition);
      pragma Assert (not Has_Diamond (D_SCO));

      Values : Vector;

      C_SCO        : SCO_Id;
      Prev_C_SCO   : SCO_Id;
      Prev_C_Value : Boolean;

   begin
      Values := To_Vector (Unknown, Length => Count_Type (Index (Condition)));

      C_SCO := Condition;
      loop
         Get_Origin (C_SCO, Prev_C_SCO, Prev_C_Value);
         exit when Prev_C_SCO = No_SCO_Id;

         Values.Replace_Element
           (Index (Prev_C_SCO), To_Tristate (Prev_C_Value));
         C_SCO := Prev_C_SCO;
      end loop;
      return Values;
   end Infer_Values;

   -------------------
   -- Is_MC_DC_Pair --
   -------------------

   function Is_MC_DC_Pair
     (Eval_1, Eval_2 : Evaluation;
      Unique_Cause   : Boolean) return Any_Condition_Index
   is
      First_Different : Any_Condition_Index := No_Condition_Index;
   begin
      pragma Assert (Eval_1.Decision = Eval_2.Decision);
      pragma Assert (Eval_1.Outcome /= Unknown
                       and then
                     Eval_2.Outcome /= Unknown);

      --  Not an MC/DC pair if both evaluations produced the same outcome

      if Eval_1.Outcome = Eval_2.Outcome then
         return No_Condition_Index;
      end if;

      --  Look for last condition evaluated in both evaluations and with
      --  different value in both, and in the case of Unique Cause, check
      --  whether it is the only one.

      for J in reverse 0 .. Condition_Index'Max
        (Eval_1.Values.Last_Index, Eval_2.Values.Last_Index)
      loop
         Check_Condition : declare
            function Cond_J
              (V : Condition_Evaluation_Vectors.Vector) return Tristate;
            --  Return the value of condition J in V, or Unknown if not
            --  evaluated.

            ------------
            -- Cond_J --
            ------------

            function Cond_J
              (V : Condition_Evaluation_Vectors.Vector) return Tristate
            is
            begin
               if J in V.First_Index .. V.Last_Index then
                  return V.Element (J);
               else
                  return Unknown;
               end if;
            end Cond_J;

            Val_1 : constant Tristate := Cond_J (Eval_1.Values);
            Val_2 : constant Tristate := Cond_J (Eval_2.Values);

         --  Start of processing for Check_Condition

         begin
            if Val_1 /= Unknown and then Val_2 /= Unknown
              and then Val_1 /= Val_2
            then
               if not Unique_Cause then
                  --  Remarkable property of Masking MC/DC with short circuit:
                  --  any evaluation pair with varied outcome is an independent
                  --  influence pair for the rightmost varied condition.

                  return J;

               elsif First_Different = No_Condition_Index then
                  First_Different := J;

               else
                  --  More than one condition had different values in both
                  --  evaluations: not an MC/DC pair.

                  return No_Condition_Index;
               end if;
            end if;
         end Check_Condition;
      end loop;

      return First_Different;
   end Is_MC_DC_Pair;

end MC_DC;
