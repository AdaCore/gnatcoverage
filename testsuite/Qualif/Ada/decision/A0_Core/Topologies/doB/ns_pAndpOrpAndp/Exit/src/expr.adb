package body Expr is

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      loop
         exit when (A and B) or (C and D); -- # eval :o/d:
         return False;           -- # retFalse
      end loop;
      return True;      -- # retTrue
   end;
end;
