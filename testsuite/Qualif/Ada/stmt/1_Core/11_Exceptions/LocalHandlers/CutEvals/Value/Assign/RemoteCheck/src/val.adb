with Args; use Args;

package body Val is

   procedure Bool (A : Integer; R : out Boolean) is
   begin
      R := Bool (Intval(A)); -- # eval
   end;

end;
