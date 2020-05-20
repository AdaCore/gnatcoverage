--  This unit needs to be compilable with Ada 95 compilers

with System;

with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

package GNATcov_RTS.Traces.Output is

   subtype Serialized_Timestamp is String (1 .. 8);
   --  Little-endian 64-bit unsigned integer. Represents a date/time as the
   --  number of seconds that passed since the Unix Epoch. In other words, a
   --  64-bit Unix timestamp.

   generic
      type Output_Stream (<>) is limited private;
      --  Entity where the trace file is written

      with procedure Write_Bytes
        (Stream : in out Output_Stream;
         Bytes  : System.Address;
         Count  : Natural) is <>;
      --  Write Count bytes located at Bytes to the given Stream

   procedure Generic_Write_Trace_File
     (Output       : in out Output_Stream;
      Buffers      : Unit_Coverage_Buffers_Array;
      Program_Name : String;
      Exec_Date    : Serialized_Timestamp;
      User_Data    : String := "");
   --  Write a trace file to Output to contain the coverage data in Buffers.
   --
   --  Program_Name, Exec_Date, and User_Data are used to fill the
   --  corresponding metadata in the written trace file.

end GNATcov_RTS.Traces.Output;
