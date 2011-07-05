with Args; use Args;

package body Val is

   function Bool (A : Integer) return Boolean is
   begin
      if Bool (Intval(A)) then -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
