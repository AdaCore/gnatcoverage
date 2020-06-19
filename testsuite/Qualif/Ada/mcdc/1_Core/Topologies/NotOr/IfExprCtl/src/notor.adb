pragma ada_2012;
package body Notor is
   function F (A, B : Boolean) return Boolean is
   begin
      return (if (not A) or else B then True else False);  -- # evalStmt :o/d:
   end;
end;
