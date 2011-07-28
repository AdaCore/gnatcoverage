with Support;

package body AndPorP is
   function F (A, B, C : Boolean) return Boolean is
   begin
      return V : Boolean :=                           -- # returnValue
        Support.Identity(A and then (B or else C)) do -- # evalOther :o/e:
        null; -- # returnValue
      end return;
   end;
end;

