package body Eval is
   function E_And (A, B : Boolean) return Boolean is
   begin
      if A and B then  -- # eval :o/d:
         return True;       -- # retTrue
      else
         return False;      -- # retFalse
      end if;
   end;
end;



