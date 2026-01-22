pragma Ada_2005;

package body Pkg is

   overriding procedure P1 (Self : T; Arg : Arg_Type'Class) is null;
   overriding procedure P2 (Self : T; Arg : access Arg_Type'Class) is null;

end Pkg;
