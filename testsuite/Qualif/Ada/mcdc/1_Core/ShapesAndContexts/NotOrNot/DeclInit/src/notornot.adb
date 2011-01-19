package body Notornot is

   function F (A, B : Boolean) return Boolean is
      E : boolean := (not A) or else (not B);  -- # evalStmt
   begin
      return E;  -- # returnValue
   end;
end;



