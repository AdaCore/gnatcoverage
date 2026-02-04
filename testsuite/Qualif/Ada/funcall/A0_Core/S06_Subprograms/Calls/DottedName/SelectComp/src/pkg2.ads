pragma Ada_2012;

with Pkg1;
with Pkg1.Child;

package Pkg2 is
   function Foo return Pkg1.Type_A is ((Field => True));       -- # fun
   function Bar return Pkg1.Child.Type_B is ((Field => True)); -- # fun
end Pkg2;
