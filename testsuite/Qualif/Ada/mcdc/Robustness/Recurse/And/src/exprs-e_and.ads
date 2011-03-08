package Exprs.E_And is

   type Band is new Expr with record
      A, B : Expr_Ref;
   end record;

   function Eval (E : Band) return Boolean;

end;
