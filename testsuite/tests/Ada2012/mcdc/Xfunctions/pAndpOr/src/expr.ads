pragma Ada_2012;

package Expr is
   function F (A, B, C : Boolean) return Boolean is
      ((A and then B) or else C); -- # eval
end;
