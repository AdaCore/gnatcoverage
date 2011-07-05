with Args; use Args;

package body Fuand is

   function Andthen (A, B : Integer) return Boolean is
   begin
      -- Safe conversion here, possible raise from Args.Bool
      if Bool (Intval(A)) and then Bool (Intval(B)) then -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
