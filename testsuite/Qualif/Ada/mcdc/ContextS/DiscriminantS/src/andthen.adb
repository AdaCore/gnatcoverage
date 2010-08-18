package body Andthen is
   type My_Type (Value : Boolean) is null record;

   function And_Then (A, B : Boolean) return Boolean is
      R : My_Type (Value => A and then B);  -- # evaluate
   begin
      return R.Value; -- # returnValue
   end;
end;
