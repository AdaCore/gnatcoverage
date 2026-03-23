package body PorPandPorP is
   type My_Type (Value : Boolean) is null record;

   function F (A, B, C, D : Boolean) return Boolean is
      R : My_Type (Value => (A or else B) and then (C or else D));  -- # evalStmt :o/e:
   begin
      return R.Value; -- # returnValue
   end;
end;
