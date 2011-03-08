package body Constructors is

   function Expr_And (A, B : Expr_Ref) return Expr_Ref is
   begin
      return new Band'(A => A, B => B);
   end;

   function Expr_And (A, B : Boolean) return Expr_Ref is
   begin
      return Expr_And (Expr_Val (A), Expr_Val (B));
   end;

   function Expr_And_And (A, B, C : Boolean) return Expr_Ref is
   begin
      return Expr_And (Expr_Val (A), Expr_And (B, C));
   end;

end;

