package body Pkg1 is

   X : Integer := 0;
   pragma Volatile(X);

   function Fn1 (B : Boolean) return Integer is
   begin
      if B then
         return X - 1;
      else
         return x + 1;
      end if;
   end Fn1;

end Pkg1;
