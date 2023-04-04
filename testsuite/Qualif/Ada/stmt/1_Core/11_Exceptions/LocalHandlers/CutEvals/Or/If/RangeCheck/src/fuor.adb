with Args; use Args;

package body Fuor is

   pragma Unsuppress (All_Checks);

   procedure Orelse (A, B : Integer; R : out Boolean) is
   begin
      --  Possible range check failure on conversion here
      if Bool (Boolval(A)) or else Bool (Boolval(B)) then -- # eval
         R := True;  -- # true
      else
         R := False; -- # false
      end if;
   end;

end;
