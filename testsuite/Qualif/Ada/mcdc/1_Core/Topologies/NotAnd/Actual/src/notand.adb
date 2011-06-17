with Support; use Support;

package body Notand is

   function F (A, B : Boolean) return Boolean is
   begin
      return Value ((not A) and then B);   -- # evalStmt
   end;

end;
