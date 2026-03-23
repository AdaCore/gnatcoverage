--  Declares type Type_A, which is imported in pkg2.ads to define a function
--  but not in make_call.adb where this function is called.

package Pkg1 is

   type Type_A is record   -- # decl
      Field : Boolean;     -- # decl
   end record;             -- # decl

end Pkg1;
