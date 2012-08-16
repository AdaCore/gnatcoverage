package body AndIdOr is
   
   type Expr (Value : Boolean) is null record; -- # typedecl

   function F (A, B, C : Boolean) return Boolean is
      R : Expr (Value => A and then Identity (B or else C)); -- # evalStmt :o/e:
   begin
      return R.Value; -- # returnValue
   end;
end;
