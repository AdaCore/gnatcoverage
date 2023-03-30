pragma Ada_2022;

package Pkg is

   Constrained : constant Boolean := True;
   Really_Constrained : constant Boolean := False;

   function Do_Sum (N_Values : Positive) return Natural;
   --  Return the sum of the values from 1 to N_Values

end Pkg;
