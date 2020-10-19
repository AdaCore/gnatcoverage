package Intops is
   type Opkind is (Op_Add, Op_Sub);

   function Eval (Op: Opkind; A, B : Integer) return integer;
end;
