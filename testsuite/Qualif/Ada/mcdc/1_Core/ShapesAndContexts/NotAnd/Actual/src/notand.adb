package body Notand is

   function One_Of (A, B : Boolean) return Boolean is
   begin
      return A;  -- # returnValue
   end;

   function F (A, B : Boolean) return Boolean is
   begin
      return One_Of ((not A) and then B,   -- # evalStmt
                     (not A) and then B);  -- # evalOther
   end;

end;
