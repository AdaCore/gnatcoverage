pragma Ada_2012;

package Pkg7.Child is

   function Foo return Type_F is ((others => <>)); -- # fun
   --  Return Type_F defined in the parent package. Only this child package
   --  is imported in the unit making the call to Foo.

end Pkg7.Child;
