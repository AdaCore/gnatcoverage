package body Notornot is
   function F (A, B : Boolean) return Boolean is
   begin
      return (not A) or else (not B);  -- # evalStmt
   end;
end;

