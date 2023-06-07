pragma Ada_2012;

package Pkg_2 is

   generic
      Decr : Natural := 1;
   function Generic_Decr (Value : Integer) return Integer;

   function Generic_Decr (Value : Integer) return Integer
   is (Value - Decr);

   I : Integer := 0;

end Pkg_2;
