with Support; use Support;

package body Fuand is

   function Eval (R1, R2 : Integer) return Boolean is
   begin
      if R1+R2 in RangeP          -- # eval0
        and then R1-R2 in RangeM  -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
