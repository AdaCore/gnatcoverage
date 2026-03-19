with Support; use Support;

package body Andnot is

   function F (A, B : Boolean) return Boolean is
   begin
      return Value (A and then not B);   -- # evalStmt :o/e:
   end;

end;
