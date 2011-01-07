package body Andthen is

   type My_Bool (Value : Boolean) is null record;

   function And_Then (A, B : My_Bool) return Boolean is
   begin
      return A.Value and then B.Value; -- # evalStmt
   end;

   function And_Then (A, B : Boolean) return Boolean is
   begin
      return And_Then ((Value => A), (Value => B));  -- # returnValue
   end;
end;
