pragma Ada_2012;

package Pkg5 is

   type T is record                              -- # decl
      Field : Boolean;                           -- # decl
   end record;                                   -- # decl

   function Id (Arg : T) return T;
   function Create_T return T;

end Pkg5;
