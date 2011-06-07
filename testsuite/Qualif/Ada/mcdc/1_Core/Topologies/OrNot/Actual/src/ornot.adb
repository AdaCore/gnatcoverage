package body Ornot is

   function One_Of (A, B : Boolean) return Boolean is
   begin
      return A;  -- # returnValue
   end;

   function F (A, B : Boolean) return Boolean is
   begin
      return One_Of (A or else (not B),   -- # evalStmt
                     A or else (not B));  -- # evalOther
   end;

end;
