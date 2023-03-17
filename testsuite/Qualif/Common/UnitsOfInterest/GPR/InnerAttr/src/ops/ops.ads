package Ops is
   type Opkind is (Op_Andthen, Op_Orelse);

   function Eval (Op: Opkind; A, B : Boolean) return Boolean;
end;
