with Args; use Args;

package body Flip is

   pragma Unsuppress (All_Checks);
   
   function Neg (A : Integer) return Boolean is
   begin
      -- Possible index check failure here
      return not Bool_For (A); -- # eval
   end;
   
   procedure Neg (A : Integer; R : out Boolean) is
   begin
      R := Neg (A); -- # eval
   end;

end;
