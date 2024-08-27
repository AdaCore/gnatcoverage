with Support; use Support;

with Pkg; use Pkg;

procedure Main_Ada is
begin
   Assert (not Or_Else (False, False));
   pragma Annotate (Xxxx, Dump_Buffers, "ada-0");
   pragma Annotate (Xxxx, Reset_Buffers);
   Assert (Or_Else (True, False));
   pragma Annotate (Xxxx, Dump_Buffers, "ada-1");
   pragma Annotate (Xxxx, Reset_Buffers);
   Assert (Or_Else (False, True));
   pragma Annotate (Xxxx, Dump_Buffers, "ada-2");
end Main_Ada;
