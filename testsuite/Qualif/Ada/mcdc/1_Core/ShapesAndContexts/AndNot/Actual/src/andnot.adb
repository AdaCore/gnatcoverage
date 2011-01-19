package body Andnot is

   function One_Of (A, B : Boolean) return Boolean is
   begin
      return A;  -- # returnValue
   end;

   function F (A, B : Boolean) return Boolean is
   begin
      return One_Of (A and then not B,   -- # evalStmt
                     A and then not B);  -- # evalOther
   end;

end;
