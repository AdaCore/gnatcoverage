package body Fubool is

   function Eval (R1, R2 : Integer) return Boolean is
   begin
      if Data(R1 + R2) = 1 then    -- # eval0
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
