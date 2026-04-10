with Pkg;

procedure Main is
begin
   Pkg.Foo;
   pragma Annotate (Xcov, Dump_Buffers);
end Main;
