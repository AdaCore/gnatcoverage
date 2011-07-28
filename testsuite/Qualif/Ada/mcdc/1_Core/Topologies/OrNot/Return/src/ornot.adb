package body Ornot is
   function F (A, B : Boolean) return Boolean is
   begin
      return A or else (not B);  -- # evalStmt :o/e:
   end;
end;

