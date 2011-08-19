package body Value is
   function F (X : Boolean) return Boolean is
   begin
      while X loop     -- # eval :o/d:
         return True;  -- # returnTrue
      end loop;

      return False;    -- # returnFalse
   end;
end;

