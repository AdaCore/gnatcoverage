with Args; use Args;

package body Fuand is

   pragma Unsuppress (All_Checks);

   procedure Andthen (A, B : Integer; R : out Boolean) is
   begin
      -- Possible out of range array access here
      if Bool_For (A) and then Bool_For(B) then -- # eval
         R := True;  -- # true
      else
         R := False; -- # false
      end if;
   end;

end;
