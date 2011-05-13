with Args; use Args;

package body Fuor is

   pragma Unsuppress (All_Checks);

   function Orelse (A, B : Integer) return Boolean is
   begin
      --  Possible range check failure on conversion here
      if Bool (Boolval(A)) or else Bool (Boolval(B)) then -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
