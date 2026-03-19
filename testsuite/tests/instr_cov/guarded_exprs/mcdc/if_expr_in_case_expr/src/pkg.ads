package Pkg is
   type Animal is (Dog, Cat, Cow);

   function Foo (A : Animal; B, C : Boolean) return String;
end Pkg;
