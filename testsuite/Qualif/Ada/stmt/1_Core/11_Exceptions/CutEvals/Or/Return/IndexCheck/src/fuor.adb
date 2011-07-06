with Args; use Args;

package body Fuor is

   pragma Unsuppress (All_Checks);

   procedure Orelse (A, B : Integer; R : out Boolean) is
   begin
      -- Possible out of range array access here
      R := Bool_For (A) or else Bool_For(B); -- # eval
   end;

end;
