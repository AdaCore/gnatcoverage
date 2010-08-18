package body Andthen is

   function One_Of (A, B : Boolean) return Boolean is
   begin
      return A;  -- # returnValue
   end;

   function And_Then (A, B : Boolean) return Boolean is
   begin
      return One_Of (A and then B,   -- # evaluate
                     A and then B);  -- # evaluate
   end;

end;
