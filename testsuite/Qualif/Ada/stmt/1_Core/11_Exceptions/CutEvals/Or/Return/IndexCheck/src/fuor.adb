with Args; use Args;

package body Fuor is

   pragma Unsuppress (All_Checks);
   
   function Orelse (A, B : Integer) return Boolean is
   begin
      -- Possible out of range array access here
      return Bool_For (A) or else Bool_For(B); -- # eval
   end;
      
   procedure Orelse (A, B : Integer; R : out Boolean) is
   begin
      R := Orelse (A, B); -- # eval
   end;

end;
