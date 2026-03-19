with Support; use Support;

package body FUOR is

   function Eval (R1, R2 : Integer) return Boolean is
   begin
      if R1+R2 in 1 .. 10          -- # eval0
        or else R1+R2 in 20 .. 30  -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
