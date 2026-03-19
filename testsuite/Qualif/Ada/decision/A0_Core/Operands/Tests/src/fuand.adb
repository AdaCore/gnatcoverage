with Support; use Support;

package body Fuand is

   function Eval (R1, R2 : XYrange) return Boolean is
   begin
      if R2.X <= R1.Y          -- # eval0
        and then R2.Y >= R1.X  -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
