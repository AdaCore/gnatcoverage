with Support;

package body PorPandPorP is
   function F (A, B, C, D : Boolean) return Boolean is
   begin
      return V : Boolean :=                           -- # returnValue
        Support.Identity((A or else B) and then (C or else D)) do -- # evaluate
        null; -- # returnValue
      end return;
   end;
end;

