with Args; use Args;

package body Flip is

   pragma Unsuppress (All_Checks);

   function Neg (A : Integer) return Boolean is
   begin
      -- Possible range check failure on conversion here
      return not Bool (Boolval(A)); -- # eval
   end;
   
   procedure Neg (A : Integer; R : out Boolean) is
   begin
      R := Neg (A); -- # eval
   end;
   
end;
