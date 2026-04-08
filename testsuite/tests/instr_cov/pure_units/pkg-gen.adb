package body Pkg.Gen is

   type Stuff_2 is null record;
   function Identity (B : Boolean) return Boolean is
   begin
      return B;
   end Identity;

end Pkg.Gen;
