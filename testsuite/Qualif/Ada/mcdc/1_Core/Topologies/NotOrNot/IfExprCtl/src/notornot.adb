pragma ada_2012;
package body Notornot is
   function F (A, B : Boolean) return Boolean is
   begin
      return (if (not A) or else (not B) then True else False);  -- # evalStmt :o/d:
   end;
end;
