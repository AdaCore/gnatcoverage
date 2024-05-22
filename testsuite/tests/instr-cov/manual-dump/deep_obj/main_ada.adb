with Ada.Text_IO; use Ada.Text_IO;

procedure Main_Ada is
begin
   Put_Line ("Hello Ada world!");
   pragma Annotate (Xcov, Dump_Buffers);
end Main_Ada;
