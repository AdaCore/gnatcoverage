pragma Ada_2012;

with Pkg3;
with Pkg3.Child;

package Pkg4 is
   function Foo return Pkg3.Type_C is ((Field => True));       -- # fun
   function Bar return Pkg3.Child.Type_D is ((Field => True)); -- # fun
end Pkg4;
