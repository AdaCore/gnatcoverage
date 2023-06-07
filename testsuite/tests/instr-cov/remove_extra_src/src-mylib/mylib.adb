package body Mylib is
   function Foo (B : Boolean) return Integer is
   begin
      if B then
         return 1;
      else
         return 2;
      end if;
   end Foo;
end Mylib;
