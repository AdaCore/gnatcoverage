pragma Ada_2022;

package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
      Arr : constant array (Positive range 1 .. 1) of Boolean := [True]; -- # decl
   begin
      return (for some Elt of Arr when A or else B -- # orelse :o/d:
              => Elt); -- # quant_expr_pred
   end;
end;
