package body Expr is

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      while (A and then B) or else (C or else D) loop  -- # eval :o/d:
         return True;          -- # retTrue
      end loop;
      return False;      -- # retFalse
   end;
end;



