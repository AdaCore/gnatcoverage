with Args; use Args;

package body Fuand is

   pragma Unsuppress (All_Checks);
   
   function Andthen (A, B : Integer) return Boolean is
   begin
      -- Possible out of range array access here
      return Bool_For (A) and then Bool_For(B); -- # eval
   end;
   
   procedure Andthen (A, B : Integer; R : out Boolean) is
   begin
      R := Andthen (A, B); -- # eval
   end;

end;
