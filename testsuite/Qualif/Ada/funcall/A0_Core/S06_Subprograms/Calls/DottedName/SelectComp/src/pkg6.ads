pragma Ada_2012;

private with Pkg5;

package Pkg6 is
   function Bar return Boolean;
private
   function Foo return Boolean is (Pkg5.Id (Pkg5.Create_T).Field); -- # fun
end Pkg6;
