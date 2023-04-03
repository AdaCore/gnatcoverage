with Args; use Args;

package body Val is

   function To_Bool (A : Integer) return Boolean is
   begin
      -- Safe conversion here, possible raise from Args.Bool
      return Bool (Intval(A)); -- # eval
   end;

   procedure Bool (A : Integer; R : out Boolean) is
   begin
      R := To_Bool (A); -- # eval
   end;

end;
