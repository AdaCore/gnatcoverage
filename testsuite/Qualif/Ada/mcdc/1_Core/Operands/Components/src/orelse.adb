package body Orelse is

   type My_Bool (Value : Boolean) is null record;

   function Or_Else (A, B : My_Bool) return Boolean is
   begin
      return A.Value or else B.Value; -- # evalStmt
   end;

   function Or_Else (A, B : Boolean) return Boolean is
   begin
      return Or_Else ((Value => A), (Value => B));  -- # returnValue
   end;
end;
