pragma Ada_2012;
pragma Assertion_Policy (Disable);

package Pkg is

   type R1 is record
      X, Y : Integer;
   end record
   with Ghost_Predicate => R1.X /= 0 and then R1.Y /= 0;

   subtype R2 is R1
   with Ghost_Predicate => R2.X mod 2 = 0 and then R2.Y mod 2 = 0;

   procedure Foo is null;

end Pkg;
