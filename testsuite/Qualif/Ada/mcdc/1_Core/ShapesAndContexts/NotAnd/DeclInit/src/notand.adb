package body Notand is

   function F (A, B : Boolean) return Boolean is
      E : boolean := (not A) and then B;  -- # evalStmt
   begin
      return E;  -- # returnValue
   end;
end;



