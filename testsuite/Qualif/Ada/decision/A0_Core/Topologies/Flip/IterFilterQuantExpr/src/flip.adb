pragma Ada_2022;
package body Flip is
   function F (X : Boolean) return Boolean is
      Arr : constant array (Positive range 1 .. 1) of Boolean := [X]; -- # decl
   begin
      return not (for all B of Arr when not B -- # eval :o/d:
                  => B); -- # quant_expr_pred
   end F;
end Flip;
