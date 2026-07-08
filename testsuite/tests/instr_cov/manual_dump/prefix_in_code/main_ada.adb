with Ada.Text_IO; use Ada.Text_IO;
with Get_Prefix_Ada;

procedure Main_Ada is
begin
   Put_Line ("Hello Ada world!");
   pragma Annotate (XCov, Dump_Buffers, Get_Prefix_Ada.Get);
   pragma Annotate (XCov, Dump_Buffers, "valid" & "prefix");
end Main_Ada;
