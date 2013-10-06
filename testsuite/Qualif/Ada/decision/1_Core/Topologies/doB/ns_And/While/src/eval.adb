package body Eval is

   function E_And (A, B : Boolean) return Boolean is
   begin
      while A and B loop  -- # eval :o/d:
         return True;          -- # retTrue
      end loop;
      return False;      -- # retFalse
   end;
end;



