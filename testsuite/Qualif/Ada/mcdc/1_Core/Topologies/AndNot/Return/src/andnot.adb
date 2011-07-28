package body Andnot is
   function F (A, B : Boolean) return Boolean is
   begin
      return A and then not B;  -- # evalStmt :o/e:
   end;
end;

