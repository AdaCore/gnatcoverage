with Pkg;

procedure Main is
   X : Integer := Pkg.Foo;
   pragma Volatile (X);
begin
   null;
end Main;
