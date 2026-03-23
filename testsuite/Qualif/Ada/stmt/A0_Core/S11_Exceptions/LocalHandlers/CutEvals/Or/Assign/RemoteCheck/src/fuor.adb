with Args; use Args;

package body Fuor is

   procedure Orelse (A, B : Integer; R : out Boolean) is
   begin
      -- Safe conversion here, possible raise from Args.Bool
      R := Bool (Intval(A)) or else Bool (Intval(B)); -- # eval
   end;

end;
