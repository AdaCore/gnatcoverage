package body Andthen is

   function One_Of (A, B : Boolean) return Boolean is
   begin
      return A;  -- # returnValue
   end;

   function And_Then (A, B : Boolean) return Boolean is
   begin
      return One_Of (A and then B,   -- # evalStmt
                     A and then B);  -- # evalOther
   end;

end;
