with Args; use Args;

package body Fuand is

   pragma Unsuppress (All_Checks);
   
   function Andthen (A, B : Integer) return Boolean is
   begin
      --  Possible range check failure on conversion here
      return Bool (Boolval(A)) and then Bool (Boolval(B)); -- # eval
   end;
   
   procedure Andthen (A, B : Integer; R : out Boolean) is
   begin
      R := Andthen (A, B); -- # eval
   end;

end;
