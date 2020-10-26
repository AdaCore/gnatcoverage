pragma Ada_2012;
package body Expr is
   function F (A, B, C, D : Boolean) return Boolean is
   begin
      return (if (A and then B) or else (C or else D) then True else False); -- # eval :o/d:
   end;
end;



