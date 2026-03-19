pragma Ada_2012;

package Aspect is

   function Foo return Integer is (42)                     -- # expr
     with Convention => Stdcall;

   type Foo_Type is access
      function return Integer with Convention => Stdcall;

end Aspect;
