package body Fuand is

   function Eval (R1, R2 : Integer) return Boolean is
   begin
      if (R1 + R2) mod 2 = 0    -- # eval0
        and then Data(R1 + R2)  -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
