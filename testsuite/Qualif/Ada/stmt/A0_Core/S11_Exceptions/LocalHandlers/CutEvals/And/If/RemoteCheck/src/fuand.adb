with Args; use Args;

package body Fuand is

   procedure Andthen (A, B : Integer; R : out Boolean) is
   begin
      -- Safe conversion here, possible raise from Args.Bool
      if Bool (Intval(A)) and then Bool (Intval(B)) then -- # eval
         R := True;  -- # true
      else
         R := False; -- # false
      end if;
   end;

end;
