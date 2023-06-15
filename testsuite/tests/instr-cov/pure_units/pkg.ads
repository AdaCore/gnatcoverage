pragma Ada_2012;

package Pkg is

   pragma Pure;

   function Fact (I : Integer) return Integer;

   type Stuff is null record;

   procedure Do_Nothing_1 is null;
   procedure Do_Nothing_2 is null;

   function Identity (B : Boolean) return Boolean is (B);
   function Opposite (B : Boolean) return Boolean is (not B);

end Pkg;
