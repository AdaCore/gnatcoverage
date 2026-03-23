--  Declare type T, which is imported inpkg2.ads to define a function but not
--  in make_call.adb where this function is called.

package Pkg1 is
   type T is record -- # decl
      B : Boolean;  -- # decl
   end record;      -- # decl
end Pkg1;
