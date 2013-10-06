with Support; use Support;

package body Eval is
   function F (A, B, X : Boolean) return Boolean is
   begin
      if X then               -- # __l!dT-
         raise Program_Error; -- # __l-s-
      elsif A and B then -- # eval :o/d:
         return True;         -- # retTrue
      else
         return False;        -- # retFalse
      end if;
   end;

   function E_And (A, B : Boolean) return Boolean is
   begin
      return F (A, B, Identity(False)); -- # retVal
   end;
end;



