with Args; use Args;

package body Val is

   procedure Bool (A : Integer; R : out Boolean) is
   begin
      if Bool (Intval(A)) then -- # eval
         R := True;  -- # true
      else
         R := False; -- # false
      end if;
   end;

end;
