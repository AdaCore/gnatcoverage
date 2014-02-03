with Support; use Support;

package body Fuand is

   function Eval (R1, R2 : Integer) return Boolean is
   begin
      if R1+R2 > 1         -- # eval0
        and then Values(R1+R2)'Valid -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
