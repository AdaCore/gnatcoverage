package body Orelse is

   function One_Of (A, B : Boolean) return Boolean is
   begin
      return A;  -- # returnValue
   end;

   function Or_Else (A, B : Boolean) return Boolean is
   begin
      return One_Of (A or else B,   -- # evalStmt
                     A or else B);  -- # evalOther
   end;

end;
