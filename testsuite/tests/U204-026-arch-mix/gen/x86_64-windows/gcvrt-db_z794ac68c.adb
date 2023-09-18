pragma Style_Checks (Off); pragma Warnings (Off);
with GNATcov_RTS.Traces.Output.Base64;
with Interfaces.C;
with GCVRT.Foo;
package body GCVRT.DB_z794ac68c is

   procedure Dump_Buffers is
   begin
      GNATcov_RTS.Traces.Output.Base64.Write_Trace_File_Wrapper
        (GCVRT.Foo.List,
         Program_Name => "main_2",
         Exec_Date => 0);
   end Dump_Buffers;

   overriding procedure Finalize (Self : in out Dump_Controlled_Type) is
   begin
      Dump_Buffers;
   end Finalize;

end GCVRT.DB_z794ac68c;
