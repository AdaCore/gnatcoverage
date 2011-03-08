package body Constructors is

   function Expr_Andor (A, B, C : Expr_Ref) return Expr_Ref is
   begin
      return new Bandor'(A => A, B => B, C => C);
   end;

   function Expr_Andor (A, B, C : Boolean) return Expr_Ref is
   begin
      return Expr_Andor (Expr_Val (A), Expr_Val (B), Expr_Val (C));
   end;

   function Expr_Andor_Andor (A, B, C, D, E : Boolean) return Expr_Ref is
   begin
      return Expr_Andor (Expr_Val (A), Expr_Val (B), Expr_Andor (C, D, E));
   end;

end;

