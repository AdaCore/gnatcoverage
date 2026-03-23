with Pkh;

package body Pkg is

   function Foo (X : Integer) return Integer is
   begin
      if Pkh.Bar (X) = 0 then
         return 0;
      else
         return 1;
      end if;
   end Foo;

end Pkg;
