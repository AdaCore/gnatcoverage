with Ada.Text_IO; use Ada.Text_IO;

procedure Main_Ada is
   Trace_Prefix : constant String := "ada_trace";
begin
   Put_Line ("Hello Ada world!");
   pragma Annotate (XCov, Dump_Buffers, Main_Ada.Trace_Prefix);
   pragma Annotate (XCov, Dump_Buffers, "valid" & "prefix");
end Main_Ada;
