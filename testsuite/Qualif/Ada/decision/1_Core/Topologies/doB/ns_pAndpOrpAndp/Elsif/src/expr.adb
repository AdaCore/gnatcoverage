with Support; use Support;

package body Expr is
   function F (A, B, C, D, X : Boolean) return Boolean is
   begin
      if X then               -- # __l!dT-
         raise Program_Error; -- # __l-s-
      elsif (A and B) or (C and D) then -- # eval :o/d:
         return True;         -- # retTrue
      else
         return False;        -- # retFalse
      end if;
   end;

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      return F (A, B, C, D, Identity(False)); -- # retVal
   end;
end;



