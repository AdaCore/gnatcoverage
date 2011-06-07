package body Notandnot is

   function F (A, B : Boolean) return Boolean is
      E : boolean := (not A) and then (not B);  -- # evalStmt
   begin
      return E;  -- # returnValue
   end;
end;



