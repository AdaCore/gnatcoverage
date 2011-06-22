with Support; use Support;

package body Flip is
   function Doflip (X, Bomb : Boolean) return Boolean is
   begin
      if Bomb then             -- # __l!d!
         raise Program_Error;  -- # __l-s-
      elsif not X then  -- # evaluate
         return True;   -- # returnTrue
      else
         return False;  -- # returnFalse
      end if;
   end;

   function F (X : Boolean) return Boolean is
   begin
      return Doflip (X, Bomb => Identity(False)); -- # returnVal
   end;
end;

