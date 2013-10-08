package body Expr is
   function F (A, B, C, D : Boolean) return Boolean is
   begin
      if (A and then B) or else (C and then D) then  -- # eval :o/d:
         return True;       -- # retTrue
      end if;
      return False;      -- # retFalse
   end;
end;



