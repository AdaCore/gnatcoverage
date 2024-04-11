with Ada.Text_IO;

procedure Foo_Skipped is
begin
   Ada.Text_IO.Put_Line ("Hello world!");
   pragma Annotate (Xcov, Dump_Buffers);
end Foo_Skipped;
