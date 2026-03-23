with Ada.Text_IO; use Ada.Text_IO;

with Unit_Ada_Skipped;

procedure Main_Ada_Skipped is
begin
   Put_Line ("Start main");
   Unit_Ada_Skipped;
   Put_Line ("End main");
   pragma Annotate (Xcov, Dump_Buffers);
end Main_Ada_Skipped;
