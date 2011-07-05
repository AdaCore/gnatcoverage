with Args; use Args;

package body Fuor is

   pragma Unsuppress (All_Checks);

   procedure Orelse (A, B : Integer; R : out Boolean) is
   begin
      -- Possible out of range array access here
      if Bool_For (A) or else Bool_For(B) then -- # eval
         R := True;  -- # true
      else
         R := False; -- # false
      end if;
   end;

end;
