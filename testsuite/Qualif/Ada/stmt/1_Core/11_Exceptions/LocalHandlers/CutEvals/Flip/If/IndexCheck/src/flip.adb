with Args; use Args;

package body Flip is

   pragma Unsuppress (All_Checks);

   procedure Neg (A : Integer; R : out Boolean) is
   begin
      -- Possible index check failure here
      if not Bool_For (A) then -- # eval
         R := True;  -- # true
      else
         R := False; -- # false
      end if;
   end;

end;
