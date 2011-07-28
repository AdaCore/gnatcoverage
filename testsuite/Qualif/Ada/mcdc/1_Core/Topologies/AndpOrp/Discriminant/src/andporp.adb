package body AndPorP is
   type My_Type (Value : Boolean) is null record;

   function F (A, B, C : Boolean) return Boolean is
      R : My_Type (Value => A and then (B or else C));  -- # evalStmt :o/e:
   begin
      return R.Value; -- # returnValue
   end;
end;
