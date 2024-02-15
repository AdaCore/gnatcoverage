pragma Ada_2012;
package body Expr is
   function Eval (Op : Opkind; A, B : Boolean) return Boolean is
   begin
      --  Simple controlling expression, complex value expressions

      return (if (Op = Op_And)       -- # if-opand
                then (A and then B)  -- # then-and
                else (A or else B)   -- # else-or
             );
   end;
end;
