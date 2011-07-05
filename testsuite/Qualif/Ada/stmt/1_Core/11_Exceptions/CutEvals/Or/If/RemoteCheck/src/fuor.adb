with Args; use Args;

package body Fuor is

   function Orelse (A, B : Integer) return Boolean is
   begin
      -- Safe conversion here, possible raise from Args.Bool
      if Bool (Intval(A)) or else Bool (Intval(B)) then -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
