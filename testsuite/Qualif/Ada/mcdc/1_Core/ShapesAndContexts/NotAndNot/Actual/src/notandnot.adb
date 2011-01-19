package body Notandnot is

   function One_Of (A, B : Boolean) return Boolean is
   begin
      return A;  -- # returnValue
   end;

   function F (A, B : Boolean) return Boolean is
   begin
      return One_Of ((not A) and then (not B),   -- # evalStmt
                     (not A) and then (not B));  -- # evalOther
   end;

end;
