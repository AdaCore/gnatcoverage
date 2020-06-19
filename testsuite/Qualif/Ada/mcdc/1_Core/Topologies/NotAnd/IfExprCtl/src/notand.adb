pragma ada_2012;
package body Notand is
   function F (A, B : Boolean) return Boolean is
   begin
      return (if (not A) and then B then True else False);  -- # evalStmt :o/d:
   end;
end;
