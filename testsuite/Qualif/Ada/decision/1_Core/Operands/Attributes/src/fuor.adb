package body Fuor is

   function Eval (R1, R2 : String_Access) return Boolean is
   begin
      if R2'Length < R1'Length             -- # eval0
        or else R2'Length = R1'Length then -- # eval1
         return True;   -- # true
      else
         return False;  -- # false
      end if;
   end;
end;
