package body Flip is
   function F (X : Boolean) return Boolean is
   begin
      if not X then     -- # eval :o/d:
         return True;   -- # returnTrue
      end if;
      return False;  -- # returnFalse
   end;
end;

