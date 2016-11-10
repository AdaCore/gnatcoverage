pragma Ada_2012;

package Ops is
   pragma Assertion_Policy (Ignore);
   
   function Plus1 (I : Integer) return Integer is (I + 1);
   
   procedure Add_Or_Mult (X, Y : Integer; Z : out Integer) with
     Post => (Z in 1 .. Y | 3 | Plus1 (X));
end;
