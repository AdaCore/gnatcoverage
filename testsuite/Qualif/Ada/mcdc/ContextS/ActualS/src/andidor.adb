package body AndIdOr is

   function One_Of (A, B, C : Boolean) return Boolean is
   begin
      return A;  -- # returnValue
   end;

   function F (A, B, C : Boolean) return Boolean is
   begin
      return One_Of (A and then Identity (B or else C),   -- # evaluate
                     A and then Identity (B or else C),   -- # evaluate
                     A and then Identity (B or else C));  -- # evaluate
   end;

end;
