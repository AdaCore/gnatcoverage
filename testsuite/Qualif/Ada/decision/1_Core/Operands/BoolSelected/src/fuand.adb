with Support; use Support;

package body Fuand is

   function Eval (R1, R2 : Integer) return Boolean is
   begin
      if Values(R1+R2).Bit0         -- # eval0
        and then Values(R1+R2).Bit1 -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
