package Pkg is
   procedure Foo is null;
   procedure Bar;
   pragma Import (C, Bar, External_Name => "bar");
   procedure Baz;
   pragma Import (C, Baz, External_Name => "baz");
end Pkg;
