pragma Ada_2012;
package body Expr is
   
   function Plus (X, Y : Integer; Delegate : Boolean) return Integer is
      
      function Sum (X, Y : Integer) return Integer is (X + Y); -- # delegate
      
      Sx : Integer := Zx + X; -- # decl
      Sy : Integer := Zy + Y; -- # decl
   begin
      if Delegate then -- # test
         return Sum (X, Y); -- # delegate
      else
         return Sx + Sy; -- # compute
      end if;
   end;
   
end;
