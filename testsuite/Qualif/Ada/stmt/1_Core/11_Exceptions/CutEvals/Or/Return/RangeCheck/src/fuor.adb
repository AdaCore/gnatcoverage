with Args; use Args;

package body Fuor is

   pragma Unsuppress (All_Checks);

   procedure Orelse (A, B : Integer; R : out Boolean) is
   begin
      --  Possible range check failure on conversion here
      R := Bool (Boolval(A)) or else Bool (Boolval(B)); -- # eval
   end;

end;
