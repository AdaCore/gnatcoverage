--  This unit needs to be compilable with Ada 2005 compilers

with Ada.Calendar;
with Ada.Command_Line;

with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

package GNATcov_RTS.Traces.Output is

   GNATCOV_TRACE_FILE : constant String := "GNATCOV_TRACE_FILE";
   --  Name of the environment variable to contain the default file name for
   --  output trace files.

   procedure Write_Trace_File
     (Buffers      : Unit_Coverage_Buffers_Array;
      Program_Name : String := Ada.Command_Line.Command_Name;
      Filename     : String := "";
      Exec_Date    : Ada.Calendar.Time := Ada.Calendar.Clock);
   --  Write a trace file in Filename to contain the data in Buffers.
   --
   --  If Filename is not provided, use the name in the GNATCOV_TRACE_FILE
   --  environment variable. If this environment variable is not defined or
   --  empty, fallback to "PROGRAM.srctrace", where PROGRAM is the name of the
   --  running program.
   --
   --  Program_Name and Exec_Date are used to fill the corresponding metadata
   --  in the written trace file.

end GNATcov_RTS.Traces.Output;
