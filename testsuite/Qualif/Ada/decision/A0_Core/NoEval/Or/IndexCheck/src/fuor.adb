with Args; use Args;

package body Fuor is

   pragma Unsuppress (All_Checks);

   function Orelse (A, B : Integer) return Boolean is
   begin
      -- Possible out of range array access here
      if Bool_For (A) or else Bool_For(B) then -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
