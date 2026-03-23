pragma ada_2012;
package body Orelse is
   function Or_Else (A, B : Boolean) return Boolean is
   begin
      return (if A or else B then True else False);  -- # evalStmt :o/d:
   end;
end;
