with Args; use Args;

package body Flip is

   procedure Neg (A : Integer; R : out Boolean) is
   begin
      R := not Bool (Intval(A)); -- # eval
   end;
end;
