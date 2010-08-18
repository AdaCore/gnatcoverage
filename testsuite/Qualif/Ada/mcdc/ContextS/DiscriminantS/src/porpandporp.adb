package body PorPandPorP is
   type My_Type (Value : Boolean) is null record;

   function F (A, B, C, D : Boolean) return Boolean is
      R : My_Type (Value => (A or else B) and then (C or else D));  -- # evaluate
   begin
      return R.Value; -- # returnValue
   end;
end;
