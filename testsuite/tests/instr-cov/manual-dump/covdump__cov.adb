with Ada.Command_Line;

with GCVRT.P;
with GNATcov_RTS.Traces.Output.Files;

procedure Covdump is
begin
   GNATcov_RTS.Traces.Output.Files.Write_Trace_File
     (Buffers_Groups => GCVRT.P.List,
      Filename       => Ada.Command_Line.Command_Name & ".srctrace");
end Covdump;
