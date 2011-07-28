package body Notornot is
   function F (A, B : Boolean) return Boolean is
   begin
      return (not A) or else (not B);  -- # evalStmt :o/e:
   end;
end;

