package Expr is
   type Opkind is (Op_And, Op_Or);
   
   function Eval (Op : Opkind; A, B : Boolean) return Boolean;
end;
