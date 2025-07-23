with Ada.Text_IO; use Ada.Text_IO;

procedure Unit_Ada_Process is
begin
   Put_Line ("Start unit");
   pragma Annotate (Xcov, Reset_Buffers);
   Put_Line ("End unit");
end Unit_Ada_Process;
