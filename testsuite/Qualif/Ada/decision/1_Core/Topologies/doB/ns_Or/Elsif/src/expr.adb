with Support;

package body Expr is
   function F (A, B, X : Boolean) return Boolean is
   begin
      if X then               -- # __l!dT-
         raise Program_Error; -- # __l-s-
      elsif A or B then  -- # eval :o/d:
         return True;      -- # retTrue
      else
         return False;     -- # retFalse
      end if;
   end;

   function F (A, B : Boolean) return Boolean is
   begin
      -- Identity is intended to prevent possible constant folding
      -- bias in results, irrelevant for this testcase purposes

      return F (A, B, X => Support.Identity(False)); -- # retVal
   end;
end;



