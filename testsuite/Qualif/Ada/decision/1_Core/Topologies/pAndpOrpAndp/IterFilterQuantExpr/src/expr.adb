pragma Ada_2022;

package body Expr is

   function F (A, B , C, D : Boolean) return Boolean is
      Arr : constant array (Positive range 1 .. 1) of Boolean := [True]; -- # decl
   begin
      return (for some Elt of Arr when (A and then B) or else (C and then D) -- # eval :o/d:
              => Elt); -- # quant_expr_pred
   end;
end;
