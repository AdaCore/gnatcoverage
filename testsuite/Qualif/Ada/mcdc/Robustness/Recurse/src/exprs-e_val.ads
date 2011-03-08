package Exprs.E_Val is

   type Bval is new Expr with record
      V : Boolean;
   end record;

   function Eval (E : Bval) return Boolean;

   function Expr_Val (V : Boolean) return Expr_Ref;
end;

