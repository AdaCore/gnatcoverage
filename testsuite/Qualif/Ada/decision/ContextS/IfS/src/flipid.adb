package body FlipId is
   function F (X : Boolean) return Boolean is
   begin
      if not Identity (X) then  -- # evaluate
         return True;           -- # returnTrue
      else
         return False;          -- # returnFalse
      end if;
   end;
end;

