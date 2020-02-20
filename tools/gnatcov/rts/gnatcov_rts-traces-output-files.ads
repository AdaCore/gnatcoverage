--  This unit needs to be compilable with Ada 2005 compilers

with Ada.Calendar;
with Ada.Command_Line;

with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

package GNATcov_RTS.Traces.Output.Files is

   GNATCOV_TRACE_FILE : constant String := "GNATCOV_TRACE_FILE";
   --  Name of the environment variable to contain the default file name for
   --  output trace files.

   function Default_Trace_Filename return String;
   --  Return the default name of the trace file to write.
   --
   --  This returns the content of the GNATCOV_TRACE_FILE environment variable.
   --  If this environment variable is not defined or empty, fallback to
   --  "PROGRAM.srctrace", where PROGRAM is the name of the running program.

   function Format_Date (Date : Ada.Calendar.Time) return Serialized_Timestamp;
   --  Return Date represented as a little-endian 64-bit Unix timestamp

   procedure Write_Trace_File
     (Buffers      : Unit_Coverage_Buffers_Array;
      Program_Name : String := Ada.Command_Line.Command_Name;
      Filename     : String := "";
      Exec_Date    : Ada.Calendar.Time := Ada.Calendar.Clock;
      User_Data    : String := "");
   --  Write a trace file in Filename to contain the data in Buffers.
   --
   --  If Filename is not provided, use Default_Trace_Filename.
   --
   --  Program_Name, Exec_Date, and User_Data are used to fill the
   --  corresponding metadata in the written trace file.

end GNATcov_RTS.Traces.Output.Files;
