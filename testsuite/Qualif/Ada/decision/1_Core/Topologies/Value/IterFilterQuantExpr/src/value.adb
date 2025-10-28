pragma Ada_2022;
package body Value is
   function F (X : Boolean) return Boolean is
      Arr : constant array (Positive range 1 .. 1) of Boolean := [X]; -- # decl
   begin
      return (for some B of Arr when B -- # eval :o/d:
              => B); -- # quant_expr_pred
   end F;
end Value;
