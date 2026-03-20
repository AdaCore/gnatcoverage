package body Pkg is

   function Do_Math (L, R : int) return int is
   begin
      if L = 0 and then R = 0 then
         return 0;
      else
         return L + R;
      end if;
   end Do_Math;

end Pkg;
