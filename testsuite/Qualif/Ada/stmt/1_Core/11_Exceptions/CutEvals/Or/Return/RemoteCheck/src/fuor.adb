with Args; use Args;

package body Fuor is

   function Orelse (A, B : Integer) return Boolean is
   begin
      -- Safe conversion here, possible raise from Args.Bool
      return Bool (Intval(A)) or else Bool (Intval(B)); -- # eval
   end;
      
   procedure Orelse (A, B : Integer; R : out Boolean) is
   begin
      R := Orelse (A, B); -- # eval
   end;

end;
