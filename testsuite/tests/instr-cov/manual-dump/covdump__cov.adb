with Ada.Command_Line;

with Interfaces.C.Strings; use Interfaces.C.Strings;

with GCVRT.P;
with GNATcov_RTS.Traces.Output.Files;

procedure Covdump is
   Filename : chars_ptr :=
     New_String (Ada.Command_Line.Command_Name & ".srctrace");
begin
   GNATcov_RTS.Traces.Output.Files.Write_Trace_File
     (Buffers_Groups => GCVRT.P.List,
      Filename       => Filename);
   Free (Filename);
end Covdump;
