with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
   Put ("Hello");
   pragma Annotate (Xcov, Dump_Buffers);
   Put (" Ada");
   pragma Annotate (Xcov, Dump_Buffers);
   Put_Line (" world!");
   pragma Annotate (Xcov, Dump_Buffers);
end Main;
