package body Fuor is

   function Eval (R1, R2 : Integer) return Boolean is
   begin
      if Data(R1 + R2)  -- # eval0
        or else (R1 + R2) mod 2 = 0    -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
