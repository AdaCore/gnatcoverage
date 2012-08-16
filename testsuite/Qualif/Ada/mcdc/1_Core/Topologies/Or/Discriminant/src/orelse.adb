package body Orelse is
   type Expr (Value : Boolean) is null record; -- # typedecl

   function Or_Else (A, B : Boolean) return Boolean is
      R : Expr (Value => A or else B);  -- # evalStmt :o/e:
   begin
      return R.Value; -- # returnValue
   end;
end;
