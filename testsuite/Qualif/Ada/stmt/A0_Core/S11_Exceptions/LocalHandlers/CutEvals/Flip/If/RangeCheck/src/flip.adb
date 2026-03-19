with Args; use Args;

package body Flip is

   pragma Unsuppress (All_Checks);

   procedure Neg (A : Integer; R : out Boolean) is
   begin
      if not Bool (Boolval(A)) then -- # eval
         R := True;  -- # true
      else
         R := False; -- # false
      end if;
   end;

end;
