------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with Interfaces; use Interfaces;

with GNATcov_RTS.Base_IO;       use GNATcov_RTS.Base_IO;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

package GNATcov_RTS.Traces.Output.Files is

   type Time is new Interfaces.Unsigned_64;
   --  Derived type to represent timestamps (as a number of seconds elapsed)

   Default_Trace_Filename_Env_Var : constant String := "GNATCOV_TRACE_FILE";
   --  Default name of the environment variable which controls the default
   --  filename for source traces: see the Default_Trace_Filename function
   --  below.

   function Clock return Time;
   --  Returns the number of seconds since the UNIX epoch

   function Default_Trace_Filename
     (Env_Var : String  := Default_Trace_Filename_Env_Var;
      Prefix  : String  := "gnatcov";
      Tag     : String  := "";
      Simple  : Boolean := False) return String;
   --  Return the default name of the trace file to write. Please refer to the
   --  gnatcov_rts_default_trace_filename function defined in
   --  gnatcov_rts_c-traces-output-files.h for more information.

   IO_Error : exception;
   --  Exception we raise in case of errors during the trace file creation

   procedure Write_Trace_File
     (Buffers_Groups : Coverage_Buffers_Group_Array;
      Filename       : String := Default_Trace_Filename;
      Program_Name   : String := Ada.Command_Line.Command_Name;
      Exec_Date      : Time   := Clock;
      User_Data      : String := "");
   --  Write a trace file in Filename to contain the data in Buffers_Groups.
   --  If unsuccessful, raise IO_Error and leave the error code in errno.

   procedure Write_Trace_File_Wrapper
     (Buffers_Groups : Coverage_Buffers_Group_Array;
      Filename       : String := Default_Trace_Filename;
      Program_Name   : String := Ada.Command_Line.Command_Name;
      Exec_Date      : Time   := Clock;
      User_Data      : String := "");
   --  Wrapper around Write_Trace_File that writes an error message to the
   --  standard error if the trace file could not be written.

end GNATcov_RTS.Traces.Output.Files;
