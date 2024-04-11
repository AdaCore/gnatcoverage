with Ada.Text_IO;

procedure Foo is
begin
   Ada.Text_IO.Put_Line ("Hello world!");
   pragma Annotate (Xcov, Dump_Buffers);
end Foo;
