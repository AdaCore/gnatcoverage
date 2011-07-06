with Args; use Args;

package body Flip is

   pragma Unsuppress (All_Checks);

   procedure Neg (A : Integer; R : out Boolean) is
   begin
      R := not Bool (Boolval(A)); -- # eval
   end;

end;
