pragma Ada_2012;

package Pkg is
   procedure Foo (X : Integer);
   pragma Convention (C, Foo);

   procedure Bar (X : Integer) with
     Convention => C;

   procedure Baz (X : Integer);

   procedure Blop (X : Integer);
   pragma Export (C, Blop, "blop");

end Pkg;
