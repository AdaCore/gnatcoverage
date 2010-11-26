package body ValueId is
   function F (X : Boolean) return Boolean is
   begin
      if Identity (X) then   -- # evaluate
         return True;        -- # returnTrue
      else
         return False;       -- # returnFalse
      end if;
   end;
end;

