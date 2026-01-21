pragma Ada_2005;

package Pkg is

   type Arg_Type (I : Integer) is tagged limited null record;

   type I_Type is interface;
   procedure P1 (Self : I_Type; Arg : Arg_Type'Class) is abstract;
   procedure P2 (Self : I_Type; Arg : access Arg_Type'Class) is abstract;

   type T is new I_Type with null record;
   overriding procedure P1 (Self : T; Arg : Arg_Type'Class);
   overriding procedure P2 (Self : T; Arg : access Arg_Type'Class);

end Pkg;
