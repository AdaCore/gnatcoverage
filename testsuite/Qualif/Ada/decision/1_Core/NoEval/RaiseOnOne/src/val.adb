package body Val is
   function Andthen (A, B : Arg) return Boolean is
   begin
      if Eval (A) and then Eval (B) then -- # eval
         return True;                    -- # true
      else
         return False;                   -- # false
      end if;
   end;
end;
