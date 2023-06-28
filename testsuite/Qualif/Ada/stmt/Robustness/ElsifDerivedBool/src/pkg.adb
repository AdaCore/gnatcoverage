package body Pkg is

   function Or_Else (L, R : New_Bool) return Boolean is
      use type New_Bool;
   begin
      if L then          -- # if-stmt
         return True;    -- # ret-if
      elsif R then       -- # elsif-stmt
         return True;    -- # ret-elsif
      else
         return False;   -- # ret-else
      end if;
   end Or_Else;

end Pkg;
