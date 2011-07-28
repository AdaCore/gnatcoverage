package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
      E : boolean := A and then B;  -- # evalStmt :o/e:
   begin
      return E;  -- # returnValue
   end;
end;



