pragma Ada_2012;

with Pkg4;

package Pkg5 is
   function F return Pkg4.T is ((B => True)); -- # fun
end Pkg5;
