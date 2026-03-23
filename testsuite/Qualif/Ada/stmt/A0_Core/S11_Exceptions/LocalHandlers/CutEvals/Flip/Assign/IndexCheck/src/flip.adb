with Args; use Args;

package body Flip is

   pragma Unsuppress (All_Checks);

   procedure Neg (A : Integer; R : out Boolean) is
   begin
      -- Possible index check failure here
      R := not Bool_For (A); -- # eval
   end;

end;
