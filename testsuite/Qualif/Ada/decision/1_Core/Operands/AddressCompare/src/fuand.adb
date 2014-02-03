with Support; use Support;
with System;

package body Fuand is
   
   use type System.Address;
   
   function Eval (R1, R2 : Integer) return Boolean is
   begin
      if R1+R2 > 12          -- # eval0
        and then Data(R1)'Address < Data(R2)'Address  -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
