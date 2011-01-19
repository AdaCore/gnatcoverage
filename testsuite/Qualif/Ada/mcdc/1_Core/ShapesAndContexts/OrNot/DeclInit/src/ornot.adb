package body Ornot is

   function F (A, B : Boolean) return Boolean is
      E : boolean := A or else (not B);  -- # evalStmt
   begin
      return E;  -- # returnValue
   end;
end;



