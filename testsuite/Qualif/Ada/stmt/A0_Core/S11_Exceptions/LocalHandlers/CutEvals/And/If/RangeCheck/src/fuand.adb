with Args; use Args;

package body Fuand is

   pragma Unsuppress (All_Checks);

   procedure Andthen (A, B : Integer; R : out Boolean) is
   begin
      --  Possible range check failure on conversion here
      if Bool (Boolval(A)) and then Bool (Boolval(B)) then -- # eval
         R := True;  -- # true
      else
         R := False; -- # false
      end if;
   end;

end;
