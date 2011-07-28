with Support; use Support;

package body AndIdOr is

   function F (A, B, C : Boolean) return Boolean is
   begin
      return Value (A and then Identity (B or else C));   -- # evalStmt :o/e:
   end;
end;
