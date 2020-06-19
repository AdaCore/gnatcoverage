pragma ada_2012;
package body AndPorP is
   function F (A, B, C : Boolean) return Boolean is
   begin
      return (if A and then (B or else C) then True else False); -- # evalStmt :o/d:
   end;
end;
