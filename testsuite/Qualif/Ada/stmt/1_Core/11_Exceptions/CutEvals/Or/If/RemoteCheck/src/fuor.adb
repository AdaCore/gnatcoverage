with Args; use Args;

package body Fuor is

   procedure Orelse (A, B : Integer; R : out Boolean) is
   begin
      -- Safe conversion here, possible raise from Args.Bool
      if Bool (Intval(A)) or else Bool (Intval(B)) then -- # eval
         R := True;  -- # true
      else
         R := False; -- # false
      end if;
   end;

end;
