package body Notandnot is
   function F (A, B : Boolean) return Boolean is
   begin
      return (not A) and then (not B);  -- # evalStmt
   end;
end;

