package body Orelse is
   type My_Type (Value : Boolean) is null record;

   function Or_Else (A, B : Boolean) return Boolean is
      R : My_Type (Value => A or else B);  -- # evaluate
   begin
      return R.Value; -- # returnValue
   end;
end;
