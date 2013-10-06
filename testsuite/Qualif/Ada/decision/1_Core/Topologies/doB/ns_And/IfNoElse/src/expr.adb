package body Expr is
   function F (A, B : Boolean) return Boolean is
   begin
      if A and B then  -- # eval :o/d:
         return True;       -- # retTrue
      end if;
      return False;      -- # retFalse
   end;
end;



