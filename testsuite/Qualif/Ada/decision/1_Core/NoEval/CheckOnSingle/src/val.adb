package body Val is

   function GE0 (X : Num) return Boolean is
   begin
      if Id (X + 1) > 0 then -- # eval
         return True;   -- # true
      else
         return False;  -- # false
      end if;
   end;
end;
