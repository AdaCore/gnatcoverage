package body Expr is
   function F (A, B, C, D : Boolean) return Boolean is
   begin
      if (A and B) or (C or D) then  -- # eval :o/d:
         return True;       -- # retTrue
      else
         return False;      -- # retFalse
      end if;
   end;
end;



