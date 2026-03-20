pragma ada_2012;
package body PorPandPorP is
   function F (A, B, C, D : Boolean) return Boolean is
   begin
      return (if (A or else B) and then (C or else D) then True else False); -- # evalStmt :o/d:
   end;
end;
