with Interfaces.C; use Interfaces.C;

procedure Ada_Main is
   function Foo (I : int) return int;
   pragma Import (C, Foo, "foo");

   function Bar (A, B, C : int) return int;
   pragma Import (C, Bar, "bar");

   I : constant int := Foo (1) + Bar (1, 0, 2);
begin
   null;
end Ada_Main;
