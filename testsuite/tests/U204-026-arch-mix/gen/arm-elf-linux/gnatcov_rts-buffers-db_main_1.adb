pragma Style_Checks (Off); pragma Warnings (Off);
with GNATcov_RTS.Traces.Output.Base64;
with Interfaces.C;
with GNATcov_RTS.Buffers.Lists.foo;
package body GNATcov_RTS.Buffers.DB_main_1 is

   procedure Dump_Buffers is
   begin
      GNATcov_RTS.Traces.Output.Base64.Write_Trace_File_Wrapper
        (GNATcov_RTS.Buffers.Lists.foo.List,
         Program_Name => "main_1",
         Exec_Date => 0);
   end Dump_Buffers;

end GNATcov_RTS.Buffers.DB_main_1;
