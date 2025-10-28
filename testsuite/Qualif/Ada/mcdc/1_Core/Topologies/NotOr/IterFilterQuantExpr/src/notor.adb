pragma Ada_2022;

package body Notor is

   function F (A, B : Boolean) return Boolean is
      Items : constant array (Positive range 1 .. 1) of Boolean := [True]; -- # decl
   begin
      return (for some Elt of Items when (not A) or else B -- # evalStmt :o/d:
              => Elt); -- # quant_expr_pred
   end;
end;
