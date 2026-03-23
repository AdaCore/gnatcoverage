with Args; use Args;

package body Fuand is

   pragma Unsuppress (All_Checks);

   procedure Andthen (A, B : Integer; R : out Boolean) is
   begin
      --  Possible range check failure on conversion here
      R := Bool (Boolval(A)) and then Bool (Boolval(B)); -- # eval
   end;

end;
