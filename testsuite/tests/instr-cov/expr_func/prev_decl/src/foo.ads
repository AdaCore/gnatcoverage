pragma  Ada_2012;

with Bar;

package Foo is

   type T is tagged null record;

   function Prim (Self : T) return Bar.T_Bar; -- # decl

private
   use Bar;

   function Prim (Self : T) return T_Bar is
   (False);                                   -- # EF
end Foo;