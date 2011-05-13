with Args; use Args;

package body Fuand is

   pragma Unsuppress (All_Checks);

   function Andthen (A, B : Integer) return Boolean is
   begin
      -- Possible out of range array access here
      if Bool_For (A) and then Bool_For(B) then -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
