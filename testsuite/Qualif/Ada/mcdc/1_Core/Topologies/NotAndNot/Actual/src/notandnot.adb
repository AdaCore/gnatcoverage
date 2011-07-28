with Support; use Support;

package body Notandnot is

   function F (A, B : Boolean) return Boolean is
   begin
      return Value ((not A) and then (not B)); -- # evalStmt :o/e:
   end;

end;
