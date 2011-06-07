package body Notor is

   function One_Of (A, B : Boolean) return Boolean is
   begin
      return A;  -- # returnValue
   end;

   function F (A, B : Boolean) return Boolean is
   begin
      return One_Of ((not A) or else B,   -- # evalStmt
                     (not A) or else B);  -- # evalOther
   end;

end;
