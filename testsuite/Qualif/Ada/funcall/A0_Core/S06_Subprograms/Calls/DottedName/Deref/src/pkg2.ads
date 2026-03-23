pragma Ada_2012;

with Pkg1;

package Pkg2 is
   function F return Pkg1.T is ((others => <>));
end Pkg2;
