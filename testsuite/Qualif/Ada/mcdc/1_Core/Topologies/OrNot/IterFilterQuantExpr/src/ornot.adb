pragma Ada_2022;

package body Ornot is

   function F (A, B : Boolean) return Boolean is
      Items : constant array (Positive range 1 .. 1) of Boolean := [True]; -- # decl
   begin
      return (for some Elt of Items when A or else (not B) -- # evalStmt :o/d:
              => Elt); -- # quant_expr_pred
   end;
end;
