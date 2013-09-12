with Args; use Args;

package body Val is

   pragma Unsuppress (All_Checks);

   procedure Bool (A : Integer; R : out Boolean) is
   begin
      -- Possible index check failure here
      R := Bool_For (A); -- # eval
   end;

end;
