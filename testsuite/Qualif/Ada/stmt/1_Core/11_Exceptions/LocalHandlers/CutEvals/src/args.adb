package body Args is

   function Bool (X : Intval) return Boolean is
   begin
      if X = 0 then
         return False;
      elsif X = 1 then
         return True;
      else
         raise Constraint_Error;
      end if;
   end;

   function Bool (X : Boolval) return Boolean is
   begin
      return Bool (Intval (X));
   end;
end;
