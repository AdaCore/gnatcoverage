pragma Ada_2012;

package Pkg is

   Counter : Natural := 2 with Ghost;

   function Is_Even return Boolean
   is (Counter > 0 and then Counter mod 2 = 0)  -- # is-even
   with Ghost;

   Dummy : Integer;
   --  Dummy variable just so that "pkg.ads" appears in the debug info and so
   --  that "gnatcov coverage" finds it in binary traces mode.

   procedure Say_Even with Pre => Is_Even;

end Pkg;
