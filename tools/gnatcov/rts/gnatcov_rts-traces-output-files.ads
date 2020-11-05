------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This unit needs to be compilable with Ada 95 compilers

with Ada.Command_Line;

with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

package GNATcov_RTS.Traces.Output.Files is

   type Time is new Interfaces.Unsigned_64;
   --  Derived type to represent timestamps (as a number of seconds elapsed).

   GNATCOV_TRACE_FILE : constant String := "GNATCOV_TRACE_FILE";
   --  Name of the environment variable to contain the default file name for
   --  output trace files.

   function Clock return Time;
   --  Returns the number of seconds since the UNIX epoch.

   function Default_Trace_Filename return String;
   --  Return the default name of the trace file to write.
   --
   --  This returns the content of the GNATCOV_TRACE_FILE environment variable.
   --  If this environment variable is not defined or empty, fallback to
   --  "PROGRAM.srctrace", where PROGRAM is the name of the running program.

   function Format_Date
     (Timestamp : Time) return Serialized_Timestamp;
   --  Return Date represented as a little-endian 64-bit Unix timestamp

   procedure Write_Trace_File
     (Buffers      : Unit_Coverage_Buffers_Array;
      Program_Name : String := Ada.Command_Line.Command_Name;
      Filename     : String := "";
      Exec_Date    : Time := Clock;
      User_Data    : String := "");
   --  Write a trace file in Filename to contain the data in Buffers.
   --
   --  If Filename is not provided, use Default_Trace_Filename.
   --
   --  Program_Name, Exec_Date, and User_Data are used to fill the
   --  corresponding metadata in the written trace file.
   --
   --  Exec_Date is given to produce the Timestamp. Use the current
   --  time by default.

end GNATcov_RTS.Traces.Output.Files;
