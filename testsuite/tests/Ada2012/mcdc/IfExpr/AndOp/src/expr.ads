pragma Ada_2012;

package Expr is
   -- if-expressions as outer expression operands in expression context.

   -- one outer and-then expr-decision with two operands: "if a" + "if b"
   -- plus two inner decisions: "A" and "B" controlling the if values.

   function Filter (A, Valat, Valaf, B, Valbt, Valbf : Boolean) return Boolean is
      ((if A then Valat else Valaf) and then (if B then Valbt else Valbf)); -- # eval

end;
