with Args; use Args;

package body Flip is

   procedure Neg (A : Integer; R : out Boolean) is
   begin
      if not Bool (Intval(A)) then -- # eval
         R := True;  -- # true
      else
         R := False; -- # false
      end if;
   end;

end;
