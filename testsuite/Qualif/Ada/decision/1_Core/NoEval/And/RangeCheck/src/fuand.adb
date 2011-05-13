with Args; use Args;

package body Fuand is

   pragma Unsuppress (All_Checks);

   function Andthen (A, B : Integer) return Boolean is
   begin
      --  Possible range check failure on conversion here
      if Bool (Boolval(A)) and then Bool (Boolval(B)) then -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
