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
   --  Derived type to represent timestamps (as a number of seconds elapsed)

   Default_Trace_Filename_Env_Var : constant String := "GNATCOV_TRACE_FILE";
   --  Default name of the environment variable which, if set, contains the
   --  default traces filename in Default_Trace_Filename.

   function Clock return Time;
   --  Returns the number of seconds since the UNIX epoch

   function Default_Trace_Filename_Prefix return String;
   --  Return the basename for the current program

   function Default_Trace_Filename
     (Env_Var : String := Default_Trace_Filename_Env_Var;
      Prefix  : String := Default_Trace_Filename_Prefix;
      Simple  : Boolean := False) return String;
   --  Return the default name of the trace file to write.
   --
   --  This returns the content of the Env_Var environment variable.
   --  If this environment variable is not defined or empty, fallback to:
   --
   --  * "PROGRAM.srctrace" if Simple is True (PROGRAM is the name of the
   --    running program).
   --
   --  * "PROGRAM-PID-CLOCK.srctrace" if Simple is False (PID is the current
   --    process ID and CLOCK is the result of the Clock function in decimal
   --    representation.

   function Format_Date
     (Timestamp : Time) return Serialized_Timestamp;
   --  Return Timestamp represented as a little-endian 64-bit Unix timestamp

   procedure Write_Trace_File
     (Buffers      : Unit_Coverage_Buffers_Array;
      Filename     : String := Default_Trace_Filename;
      Program_Name : String := Ada.Command_Line.Command_Name;
      Exec_Date    : Time := Clock;
      User_Data    : String := "");
   --  Write a trace file in Filename to contain the data in Buffers.
   --
   --  Program_Name, Exec_Date, and User_Data are used to fill the
   --  corresponding metadata in the written trace file.
   --
   --  Exec_Date is given to produce the Timestamp. Use the current
   --  time by default.

end GNATcov_RTS.Traces.Output.Files;
