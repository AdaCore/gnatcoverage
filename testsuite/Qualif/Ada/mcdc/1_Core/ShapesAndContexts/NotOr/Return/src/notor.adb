package body Notor is
   function F (A, B : Boolean) return Boolean is
   begin
      return (not A) or else B;  -- # evalStmt
   end;
end;

