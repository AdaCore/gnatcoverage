package Exprs is
   
   type Expr is abstract tagged null record;
   type Expr_Ref is access all Expr'Class;
   
   function Eval (E : Expr) return Boolean is abstract;
   function Eval (E : Expr_Ref) return Boolean;
end;

