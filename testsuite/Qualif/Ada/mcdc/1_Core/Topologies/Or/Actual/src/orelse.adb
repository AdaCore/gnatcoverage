with Support; use Support;

package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
   begin
      return Value (A or else B);   -- # evalStmt
   end;

end;
