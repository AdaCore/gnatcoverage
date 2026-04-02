generic
package Pkg.Gen is
   pragma Pure (Gen);

   type Stuff_1 is null record;
   function Identity (B : Boolean) return Boolean;

end Pkg.Gen;
