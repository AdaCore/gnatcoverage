package body Andnot is
   type My_Type (Value : Boolean) is null record;

   function F (A, B : Boolean) return Boolean is
      R : My_Type (Value => A and then not B);  -- # evalStmt
   begin
      return R.Value; -- # returnValue
   end;
end;
