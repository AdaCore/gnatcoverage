pragma Ada_2022;

package body Pkg is

   ------------
   -- Do_Sum --
   ------------

   function Do_Sum (N_Values : Positive) return Natural
   is
      Arr : array (1 .. N_Values) of Positive range 1 .. Positive'
        (If Constrained and then Really_Constrained           -- # Constrained
         then 10
         else Positive'Last) :=
        [for I in 1 .. N_Values => I];
      Acc : Natural := 0;
   begin
      --  According to RM 5.5.2 5/5, the subtype indication in the generalized
      --  loop iteration must *statically* match the one of the component of
      --  the iterated value. We thus can't produce any non-static decisions
      --  in the subtype indication.

      for Val : Positive range 1 .. Positive'
        (If Constrained and then Really_Constrained           -- # Constrained
         then 10
         else Positive'Last)
         of Arr
      loop
         Acc := Acc + Val;
      end loop;
      return Acc;
   end Do_Sum;

end Pkg;
