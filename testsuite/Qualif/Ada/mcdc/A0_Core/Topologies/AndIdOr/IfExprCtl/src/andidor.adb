pragma ada_2012;
with Support; use Support;
package body AndIdOr is
   function F (A, B, C : Boolean) return Boolean is
   begin
      return (if A and then Identity (B or else C) then True else False); -- # evalStmt :o/d:
   end;
end;
