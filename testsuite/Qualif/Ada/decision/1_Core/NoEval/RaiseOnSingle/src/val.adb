package body Val is

   function Id (A : Arg) return Boolean is
   begin
      if Eval (A) then  -- # eval
         return True;   -- # true
      else
         return False;  -- # false
      end if;
   end;

end;
