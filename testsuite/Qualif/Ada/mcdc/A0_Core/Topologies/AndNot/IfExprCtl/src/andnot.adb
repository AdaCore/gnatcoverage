pragma ada_2012;
package body Andnot is
   function F (A, B : Boolean) return Boolean is
   begin
      return (if A and then not B then True else False);  -- # evalStmt :o/d:
   end;
end;
