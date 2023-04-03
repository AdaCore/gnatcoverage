with Args; use Args;

package body Val is

   pragma Unsuppress (All_Checks);

   procedure Bool (A : Integer; R : out Boolean) is
   begin
      -- Possible index check failure here
      if Bool_For (A) then -- # eval
         R := True;  -- # true
      else
         R := False; -- # false
      end if;
   end;

end;
