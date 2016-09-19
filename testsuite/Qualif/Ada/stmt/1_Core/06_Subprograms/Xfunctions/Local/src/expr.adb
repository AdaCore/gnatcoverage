pragma Ada_2012;
package body Expr is
   
   function Plus (X, Y : Integer) return Integer is
      
      function Sum (X, Y : Integer) return Integer is (X + Y); -- # eval
      
      Sx : Integer := Zx + X; -- # decl
      Sy : Integer := Zy + Y; -- # decl
   begin
      return Sx + Sy; -- # stmt
   end;
   
end;
