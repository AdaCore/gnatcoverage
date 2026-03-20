with Args; use Args;

package body Flip is

   pragma Unsuppress (All_Checks);

   function Neg (A : Integer) return Boolean is
   begin
      -- Possible index check failure here
      if not Bool_For (A) then -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
