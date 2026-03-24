pragma Ada_2012;

package body Pkg6 is
   function Bar return Boolean is (Foo); -- # fun
end Pkg6;
