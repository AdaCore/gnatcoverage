with Common;

procedure Main is
begin
   Common.Say_Hello;
   pragma Annotate (Xcov, Dump_Buffers);
end Main;
