pragma Ada_2022;

package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
      Arr : constant array (Positive range 1 .. 1) of Boolean := [True]; -- # decl
   begin
      return (for some Elt of Arr when A and then B -- # andthen :o/d:
              => Elt); -- # quant_expr_pred
   end;
end;
