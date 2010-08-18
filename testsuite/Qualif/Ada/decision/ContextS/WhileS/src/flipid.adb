package body FlipId is
   function F (X : Boolean) return Boolean is
   begin
      while not Identity(X) loop  -- # evaluate
         return True;   -- # returnTrue
      end loop;

      return False;     -- # returnFalse
   end;
end;

