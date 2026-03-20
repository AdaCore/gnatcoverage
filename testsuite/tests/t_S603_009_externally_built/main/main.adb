with Mylib;

procedure Main is
   Dummy : Natural := Mylib.Fact (1);
begin
   null;
   pragma Annotate (Xcov, Dump_Buffers);
end Main;
