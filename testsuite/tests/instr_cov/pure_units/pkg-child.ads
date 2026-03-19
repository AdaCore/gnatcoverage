pragma Ada_2012;

package Pkg.Child is

   pragma Pure (Child);

   type Stuff_Child is null record;

   procedure Do_Nothing_3 is null;
   procedure Do_Nothing_4 is null;

   function Identity_Child (B : Boolean) return Boolean is (B);
   function Opposite_Child (B : Boolean) return Boolean is (not B);

end Pkg.Child;
