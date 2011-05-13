with Args; use Args;

package body Flip is

   pragma Unsuppress (All_Checks);

   function Neg (A : Integer) return Boolean is
   begin
      if not Bool (Boolval(A)) then -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
