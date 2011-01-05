package body Flip is
   function F (X : Boolean) return Boolean is
   begin
      while not X loop  -- # evaluate
         return True;   -- # returnTrue
      end loop;

      return False;     -- # returnFalse
   end;
end;

