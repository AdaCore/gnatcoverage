with Support; use Support;

package body Ornot is

   function F (A, B : Boolean) return Boolean is
   begin
      return Value (A or else (not B));   -- # evalStmt
   end;

end;
