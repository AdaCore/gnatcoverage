with Support; use Support;

package body PorPandPorP is

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      return Value ((A or else B) and then (C or else D));   -- # evalStmt
   end;
end;
