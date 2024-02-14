pragma Ada_2012;

package Expr is
   type Opkind is (Op_And, Op_Or);

   --  Simple controlling expression, complex value expressions,
   --  as expression function

   function Eval (Op : Opkind; A, B : Boolean) return Boolean is
      (if (Op = Op_And)       -- # if-opand
         then (A and then B)  -- # then-and
         else (A or else B)   -- # else-or
      );
end;
