with Support; use Support;

package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
   begin
      return Value (A and then B);   -- # evalStmt :o/e:
   end;

end;
