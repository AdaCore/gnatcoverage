package body PorPandPorP is

   function One_Of (A, B, C, D : Boolean) return Boolean is
   begin
      return A;  -- # returnValue
   end;

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      return One_Of ((A or else B) and then (C or else D),   -- # evaluate
                     (A or else B) and then (C or else D),    -- # evaluate
                     (A or else B) and then (C or else D),    -- # evaluate
                     (A or else B) and then (C or else D));  -- # evaluate
   end;

end;
