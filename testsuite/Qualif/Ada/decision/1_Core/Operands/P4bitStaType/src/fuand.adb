with Support; use Support;

package body Fuand is

   function Eval (R1, R2 : XY) return Boolean is
   begin
      if Data(R1.X + R2.X) > 5          -- # eval0
        and then Data(R1.Y + R2.Y) > 5  -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
