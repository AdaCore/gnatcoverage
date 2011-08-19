with Support;

package body Orelse is
   function F (A, B, X : Boolean) return Boolean is
   begin
      if X then               -- # __l!d!
         raise Program_Error; -- # __l-s-
      elsif A or else B then  -- # orelse :o/d:
         return True;      -- # retTrue
      else
         return False;     -- # retFalse
      end if;
   end;

   function Or_Else (A, B : Boolean) return Boolean is
   begin
      -- Identity is intended to prevent possible constant folding
      -- bias in results, irrelevant for this testcase purposes

      return F (A, B, X => Support.Identity(False)); -- # retVal
   end;
end;



