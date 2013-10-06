package body Expr is
   function F (A, B : Boolean) return Boolean is
   begin
      loop
         exit when A or B;  -- # eval :o/d:
         return False;      -- # retFalse
      end loop;
      return True;     -- # retTrue
   end;
end;



