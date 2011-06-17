with Support; use Support;

package body Notor is

   function F (A, B : Boolean) return Boolean is
   begin
      return Value ((not A) or else B);   -- # evalStmt
   end;
end;
