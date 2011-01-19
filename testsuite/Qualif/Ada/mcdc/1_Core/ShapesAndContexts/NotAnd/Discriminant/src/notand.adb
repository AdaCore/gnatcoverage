package body Notand is
   type My_Type (Value : Boolean) is null record;

   function F (A, B : Boolean) return Boolean is
      R : My_Type (Value => (not A) and then B);  -- # evalStmt
   begin
      return R.Value; -- # returnValue
   end;
end;
