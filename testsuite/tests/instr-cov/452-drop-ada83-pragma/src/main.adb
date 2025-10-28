pragma Ada_83;
with Foo;

procedure Main is
   pragma Ada_83;
   I : Integer := 1;
begin
   pragma Ada_83;
   I := Foo.Foo (I);
end Main;
