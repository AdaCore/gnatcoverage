package body Pkg2 is

   Y : Integer := 42;
   pragma Volatile (Y);

   function Fn2 (B : Boolean) return Integer is
   begin
      if B then
         return Y - 1;
      else
         return Y + 1;
      end if;
   end Fn2;

end Pkg2;