package body PorPandPorP is
   type Expr (Value : Boolean) is null record; -- # typedecl

   function F (A, B, C, D : Boolean) return Boolean is
      R : Expr (Value => (A or else B) and then (C or else D));  -- # evalStmt :o/e:
   begin
      return R.Value; -- # returnValue
   end;
end;
