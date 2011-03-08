package body exprs is
   function Eval (E : Expr_Ref) return Boolean is
   begin
      return eval (e.All);
   end;
end;
