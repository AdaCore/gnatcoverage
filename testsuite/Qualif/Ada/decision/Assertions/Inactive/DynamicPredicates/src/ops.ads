pragma Ada_12;

package Ops is
   
   pragma Assertion_Policy (Dynamic_Predicate => Disable);
   
   type T_Pair is record
      X, Y : Integer;
   end record with
     Dynamic_Predicate => T_Pair.X > 0 and then T_Pair.Y > T_Pair.X; -- # check
   
   N_Positives : Integer := 0;
   
   procedure Check (P : T_Pair);
end;
