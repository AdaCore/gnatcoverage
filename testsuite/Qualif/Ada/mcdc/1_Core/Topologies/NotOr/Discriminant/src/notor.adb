package body Notor is
   type Expr (Value : Boolean) is null record; -- # typedecl

   function F (A, B : Boolean) return Boolean is
      R : Expr (Value => (not A) or else B);  -- # evalStmt :o/e:
   begin
      return R.Value; -- # returnValue
   end;
end;
