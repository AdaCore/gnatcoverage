package body Expr is

   function F (A, B : Boolean) return Boolean is
   begin
      while A and B loop  -- # eval :o/d:
         return True;          -- # retTrue
      end loop;
      return False;      -- # retFalse
   end;
end;



