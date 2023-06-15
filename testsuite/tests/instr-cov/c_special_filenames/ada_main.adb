with Interfaces.C; use Interfaces.C;

procedure Ada_Main is
   function Foo (I : int) return int;
   pragma Import (C, Foo, "foo");

   I : constant int := Foo (1);
begin
   null;
end Ada_Main;
