with Args; use Args;

package body Fuand is

   procedure Andthen (A, B : Integer; R : out Boolean) is
   begin
      -- Safe conversion here, possible raise from Args.Bool
      R := Bool (Intval(A)) and then Bool (Intval(B)); -- # eval
   end;

end;
