package body Ornot is
   type My_Type (Value : Boolean) is null record;

   function F (A, B : Boolean) return Boolean is
      R : My_Type (Value => A or else (not B));  -- # evalStmt
   begin
      return R.Value; -- # returnValue
   end;
end;
