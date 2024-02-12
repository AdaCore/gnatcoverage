with Ada.Text_IO; use Ada.Text_IO;

procedure Main_Ada is
   Trace_Prefix : constant String := "ada_trace";
begin
   Put_Line ("Hello Ada world!");
   pragma Annotate (XCov, Dump_Buffers, Trace_Prefix);
end Main_Ada;
