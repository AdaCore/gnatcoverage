package body PandPor is
   type My_Type (Value : Boolean) is null record;

   function F (A, B, C : Boolean) return Boolean is
      R : My_Type (Value => (A and then B) or else C);  -- # evalStmt
   begin
      return R.Value; -- # returnValue
   end;
end;
