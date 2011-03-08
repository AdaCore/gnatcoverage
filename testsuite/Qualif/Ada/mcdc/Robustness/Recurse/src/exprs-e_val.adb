package body Exprs.E_Val is

   function Eval (E : Bval) return Boolean is
   begin
      return E.V;
   end;

   function Expr_Val (V : Boolean) return Expr_Ref is
   begin
      return new Bval'(V => V);
   end;

end;

