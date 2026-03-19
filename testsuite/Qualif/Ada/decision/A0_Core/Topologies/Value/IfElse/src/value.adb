package body Value is
   function F (X : Boolean) return Boolean is
   begin
      if X then        -- # eval :o/d:
         return True;  -- # returnTrue
      else
         return False; -- # returnFalse
      end if;
   end;
end;
