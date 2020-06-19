pragma Ada_2012;
package body Andthen is
   function And_Then (A, B : Boolean) return Boolean is
   begin
      return (if A and then B then True else False);  -- # evalStmt :o/d:
   end;
end;

