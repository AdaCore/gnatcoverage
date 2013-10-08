package body Expr is

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      while (A and B) or else (C and D) loop  -- # eval :o/d:
         return True;          -- # retTrue
      end loop;
      return False;      -- # retFalse
   end;
end;



