pragma Ada_2012;

package Ops is
   pragma Assertion_Policy (Ignore);
   
   function Twice (I : Integer) return Integer is (I * 2);
   function Plus1 (I : Integer) return Integer is (I + 1);
   
   procedure Add_Or_Mult (X, Y : Integer; Z : in out Integer) with
     Pre  => (X in 1 .. Y | Twice (Y)),
     Post => (Z in 1 | 3 | Plus1 (X));
end;
