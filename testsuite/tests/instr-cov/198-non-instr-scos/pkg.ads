pragma Ada_2012;

package Pkg is
   type T is tagged null record;

   function Length (Self : T) return Natural;

   function Create return T is (null record);
end Pkg;
