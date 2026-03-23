pragma Style_Checks (Off); pragma Warnings (Off);
with GNATcov_RTS.Traces.Output.Base64;
with GNATcov_RTS.Buffers.Lists;
with GCVRT.Foo;
package body GCVRT.DB_z794ac68b is

   procedure Dump_Buffers is
   begin
      GNATcov_RTS.Traces.Output.Base64.Write_Trace_File
        (Buffers_Groups => GCVRT.Foo.List,
         Program_Name => "Foo",
         Exec_Date => 0);
   end Dump_Buffers;

   procedure Finalize (Self : in out Dump_Controlled_Type) is
   begin
      Dump_Buffers;
   end Finalize;

end GCVRT.DB_z794ac68b;
