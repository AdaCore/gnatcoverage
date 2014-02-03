with Support; use Support;

package body Fuand is

   function Eval (R1, R2 : Integer) return Boolean is
   begin
      if BM1(R1+R2)          -- # eval0
        and then BM2(R1+R2)  -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
