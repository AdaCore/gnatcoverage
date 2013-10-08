package body Expr is

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      while (A or else B) or else (C and then D) loop  -- # eval :o/d:
         return True;          -- # retTrue
      end loop;
      return False;      -- # retFalse
   end;
end;



