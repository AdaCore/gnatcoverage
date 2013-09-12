with Args; use Args;

package body Fuand is

   pragma Unsuppress (All_Checks);

   procedure Andthen (A, B : Integer; R : out Boolean) is
   begin
      -- Possible out of range array access here
      R := Bool_For (A) and then Bool_For(B); -- # eval
   end;

end;
