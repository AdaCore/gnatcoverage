with Support; use Support;

package body Fuand is
   
   type Conflict is (False, True); -- like Boolean
   
   function Eval (R1, R2 : Integer) return Boolean is
   begin
      if Boolean'(R1+R2 > 10)      -- # eval0
        and then (R1 < R2) = Boolean'(True) -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
