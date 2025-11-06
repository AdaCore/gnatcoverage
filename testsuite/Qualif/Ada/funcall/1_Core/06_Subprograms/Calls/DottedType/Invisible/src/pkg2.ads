pragma Ada_2012;

--  Import Pkg1 to make T visible in order to declare function F.

with Pkg1;

package Pkg2 is
   function F return Pkg1.T is ((B => True)); -- # fun
end Pkg2;
