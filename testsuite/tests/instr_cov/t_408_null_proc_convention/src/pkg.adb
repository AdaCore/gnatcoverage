pragma Ada_2012;

package body Pkg is
   procedure Foo (X : Integer) is null;  -- # st

   procedure Bar (X : Integer) is null;  -- # st

   procedure Qux (X : Integer) is null;  -- # st
   pragma Convention (C, Qux);

   procedure Corge (X : Integer) is null with Convention => C;  -- # st

   procedure Clip (X : Integer) is null; -- # st
   pragma Export
     (Convention    => C,
      Entity        => Clip,
      External_Name => "clip");

   procedure Baz (X : Integer) is
   begin
      Qux (X);  -- # st
      Corge (X);  -- # st
      Clip (X);  -- # st
   end Baz;

   procedure Blop (X : Integer) is null; -- # st

end Pkg;
