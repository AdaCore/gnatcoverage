package body Expr is
   function F (A, B, C, D : Boolean) return Boolean is
   begin
      if (A or B) or (C and D) then  -- # eval :o/d:
         return True;       -- # retTrue
      else
         return False;      -- # retFalse
      end if;
   end;
end;



