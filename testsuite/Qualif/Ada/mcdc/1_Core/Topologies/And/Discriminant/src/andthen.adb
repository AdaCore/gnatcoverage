package body Andthen is
   type Expr (Value : Boolean) is null record; -- # typedecl

   function And_Then (A, B : Boolean) return Boolean is
      R : Expr (Value => A and then B);  -- # evalStmt :o/e:
   begin
      return R.Value; -- # returnValue
   end;
end;
