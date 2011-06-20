with Support;

package body Value is
   function Dovalue (X, Bomb : Boolean) return Boolean is
   begin
      if Bomb then            -- # __l!d!
         raise Program_Error; -- # __l-s-
      elsif X then       -- # evaluate
         return True;    -- # returnTrue
      else
         return False;   -- # returnFalse
      end if;
   end;

   function F (X : Boolean) return Boolean is
   begin
      -- The intermediate call to Identity below is intended to prevent DCE,
      -- hence lines without code, in the callee if the call gets inlined.

      return Dovalue (X, Bomb => Support.Identity(False)); -- # returnVal
   end;
end;
