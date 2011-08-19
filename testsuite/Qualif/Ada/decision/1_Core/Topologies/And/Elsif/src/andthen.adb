with Support; use Support;

package body Andthen is
   function F (A, B, X : Boolean) return Boolean is
   begin
      if X then               -- # __l!d!
         raise Program_Error; -- # __l-s-
      elsif A and then B then -- # andthen :o/d:
         return True;         -- # retTrue
      else
         return False;        -- # retFalse
      end if;
   end;

   function And_Then (A, B : Boolean) return Boolean is
   begin
      return F (A, B, Identity(False)); -- # retVal
   end;
end;



