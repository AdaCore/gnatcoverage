package Ops is
   type Opkind is (Op_And, Op_Or);
   function Compute (Op : Opkind; A, B : Boolean) return Boolean;
end;
