package body Andnot is

   function F (A, B : Boolean) return Boolean is
      E : boolean := A and then not B;  -- # evalStmt :o/e:
   begin
      return E;  -- # returnValue
   end;
end;



