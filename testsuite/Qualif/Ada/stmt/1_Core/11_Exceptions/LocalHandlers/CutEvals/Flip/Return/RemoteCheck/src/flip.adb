with Args; use Args;

package body Flip is

   function Neg (A : Integer) return Boolean is
   begin
      -- Safe conversion here, possible raise from Args.Bool
      return not Bool (Intval(A)); -- # eval
   end;
   
   procedure Neg (A : Integer; R : out Boolean) is
   begin
      R := Neg (A); -- # eval
   end;
   
end;
