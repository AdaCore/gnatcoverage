with Args; use Args;

package body Fuand is

   function Andthen (A, B : Integer) return Boolean is
   begin
      -- Safe conversion here, possible raise from Args.Bool
      return Bool (Intval(A)) and then Bool (Intval(B)); -- # eval
   end;
   
   procedure Andthen (A, B : Integer; R : out Boolean) is
   begin
      R := Andthen (A, B); -- # eval
   end;

end;
