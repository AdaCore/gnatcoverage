with Support; use Support;

package body Fuor is

   function Eval (R1, R2 : XY) return Boolean is
   begin
      if Data(R1.X + R2.X) = 1         -- # eval0
        or else Data(R1.Y + R2.Y) = 1  -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
