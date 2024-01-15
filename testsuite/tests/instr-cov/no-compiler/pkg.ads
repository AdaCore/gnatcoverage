package Pkg is
   procedure Foo;
   pragma Export (C, Foo, "pkg_foo");
end Pkg;
