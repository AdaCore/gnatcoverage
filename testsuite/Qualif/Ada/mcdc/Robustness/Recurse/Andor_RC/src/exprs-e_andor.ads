package Exprs.E_Andor is

   type Bandor is new Expr with record
      A, B, C : Expr_Ref;
   end record;

   function Eval (E : Bandor) return Boolean;

end;
