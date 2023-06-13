pragma Ada_2012;

package body Expr is
   function Plus (X, Y : Integer; Pos: in out Boolean) return Integer is
   begin
      if Pos then -- # test
         Pos := X > 0 and then Y > 0; -- # eval
      end if;
      return X + Y; -- # stmt
   end;
end;
