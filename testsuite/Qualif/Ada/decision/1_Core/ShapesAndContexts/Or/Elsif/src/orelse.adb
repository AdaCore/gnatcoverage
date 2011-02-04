package body Orelse is
   function F (A, B, X : Boolean) return Boolean is
   begin
      if X then               -- # __l!d!
         raise Program_Error; -- # __l-s-
      elsif A or else B then  -- # orelse
         return True;      -- # retTrue
      else
         return False;     -- # retFalse
      end if;
   end;

   function Or_Else (A, B : Boolean) return Boolean is
   begin
      return F (A, B, False); -- # retVal
   end;
end;



