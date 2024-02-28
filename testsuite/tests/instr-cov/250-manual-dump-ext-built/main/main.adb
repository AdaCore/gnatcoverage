with Mylib;

procedure Main is
begin
   Mylib.Say_Hello;
   pragma Annotate (Xcov, Dump_Buffers);
end Main;
