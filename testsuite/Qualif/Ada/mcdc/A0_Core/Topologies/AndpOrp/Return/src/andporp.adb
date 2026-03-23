package body AndPorP is
   function F (A, B, C : Boolean) return Boolean is
   begin
      return A and then (B or else C); -- # evalStmt :o/e:
   end;
end;
