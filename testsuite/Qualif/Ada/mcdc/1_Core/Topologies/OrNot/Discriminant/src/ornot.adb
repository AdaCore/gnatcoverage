package body Ornot is
   type Expr (Value : Boolean) is null record; -- # typedecl

   function F (A, B : Boolean) return Boolean is
      R : Expr (Value => A or else (not B));  -- # evalStmt :o/e:
   begin
      return R.Value; -- # returnValue
   end;
end;
