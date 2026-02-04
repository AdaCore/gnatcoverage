pragma Ada_2012;

package body Pkg5 is

   function Create_T return T is ((Field => <>)); -- # fun
   function Id (Arg : T) return T is (Arg);       -- # fun

end Pkg5;
