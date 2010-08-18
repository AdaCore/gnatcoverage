package body ValueId is
   function F (X : Boolean) return Boolean is
   begin
      while Identity(X) loop  -- # evaluate
         return True;   -- # returnTrue
      end loop;

      return False;     -- # returnFalse
   end;
end;

