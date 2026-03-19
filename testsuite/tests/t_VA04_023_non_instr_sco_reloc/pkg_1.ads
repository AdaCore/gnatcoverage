pragma Ada_2012;

package Pkg_1 is

   generic
      Incr : Natural := 1;
   function Generic_Incr (Value : Integer) return Integer;

   function Generic_Incr (Value : Integer) return Integer
   is (Value + Incr);

   I : Integer := 0;

end Pkg_1;
