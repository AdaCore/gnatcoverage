package body Fuand is

   function Eval (R1, R2 : Integer) return Boolean is
   begin
      if Data(R1 + R2).Bit0 = 1          -- # eval0
        and then Data(R1 + R2).Bit1 = 1  -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
