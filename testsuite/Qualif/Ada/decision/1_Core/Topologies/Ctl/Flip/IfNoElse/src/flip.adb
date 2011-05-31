package body Flip is
   function F (X : Boolean) return Boolean is
   begin
      if not X then     -- # evaluate
         return True;   -- # returnTrue
      end if;
      return False;  -- # returnFalse
   end;
end;

