with Args; use Args;

package body Fuor is

   pragma Unsuppress (All_Checks);

   function Orelse (A, B : Integer) return Boolean is
   begin
      --  Possible range check failure on conversion here
      return Bool (Boolval(A)) or else Bool (Boolval(B)); -- # eval
   end;
      
   procedure Orelse (A, B : Integer; R : out Boolean) is
   begin
      R := Orelse (A, B); -- # eval
   end;

end;
