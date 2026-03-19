package body Expr is

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      loop
         exit when (A or else B) or else (C and then D); -- # eval :o/d:
         return False;           -- # retFalse
      end loop;
      return True;      -- # retTrue
   end;
end;
