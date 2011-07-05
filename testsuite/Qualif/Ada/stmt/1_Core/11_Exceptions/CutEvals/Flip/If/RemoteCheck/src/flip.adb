with Args; use Args;

package body Flip is

   function Neg (A : Integer) return Boolean is
   begin
      if not Bool (Intval(A)) then -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
