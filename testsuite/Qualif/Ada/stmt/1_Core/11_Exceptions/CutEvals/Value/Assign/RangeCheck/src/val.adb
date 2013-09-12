with Args; use Args;

package body Val is

   pragma Unsuppress (All_Checks);

   procedure Bool (A : Integer; R : out Boolean) is
   begin
      R := Bool (Boolval(A)); -- # eval
   end;

end;
