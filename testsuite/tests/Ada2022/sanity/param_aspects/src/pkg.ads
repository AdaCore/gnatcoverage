pragma Ada_2022;

package Pkg is

   function Foo (X : Integer; Y : Boolean with Unreferenced) return Integer is
     (X); -- # expr

end Pkg;
