with System.GNATcov.Buffers.Lists; use System.GNATcov.Buffers.Lists;

package System.GNATcov.Traces.Output is

   GNATCOV_TRACE_FILE : constant String := "GNATCOV_TRACE_FILE";
   --  Name of the environment variable to contain the default file name for
   --  output trace files.

   procedure Write_Trace_File
     (Buffers  : Unit_Coverage_Buffers_Array;
      Filename : String := "");
   --  Write a trace file in Filename to contain the data in Buffers.
   --
   --  If Filename is not provided, use the name in the GNATCOV_TRACE_FILE
   --  environment variable. If this environment variable is not defined or
   --  empty, fallback to "PROGRAM.srctrace", where PROGRAM is the name of the
   --  running program.

end System.GNATcov.Traces.Output;
