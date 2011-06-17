with Support; use Support;

package body AndPorP is

   function F (A, B, C : Boolean) return Boolean is
   begin
      return Value (A and then (B or else C));   -- # evalStmt
   end;

end;
