------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with Checkpoints;
with Coverage;
with Traces_Files; use Traces_Files;

with GNATcov_RTS.Traces;

--  The list of all processed trace files

package Traces_Files_List is

   type Trace_File_Element is limited record
      Filename : Ada.Strings.Unbounded.Unbounded_String;
      --  File name for the trace file, as passed to "gnatcov coverage"

      Kind : Trace_File_Kind;
      --  Kind of trace file this describes

      Context : Ada.Strings.Unbounded.Unbounded_String;
      --  Empty string if this trace file was loaded by this instance of
      --  gnatcov.  Otherwise, string to be decoded with Coverage.From_String,
      --  that describes the context where this trace file has actually been
      --  processed.

      Program_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  Name of the program that was executed to produce this trace

      Time : Ada.Strings.Unbounded.Unbounded_String;
      --  Production time for this trace file. The date format is the one
      --  returned by Traces_Files.Format_Date_Info.

      User_Data : Ada.Strings.Unbounded.Unbounded_String;
      --  Custom user data attached to this trace file
   end record;

   type Trace_File_Element_Acc is access Trace_File_Element;

   function Create_Trace_File_Element
     (Filename : String;
      Kind     : Trace_File_Kind) return Trace_File_Element_Acc;
   --  Create a new trace file element for our list

   procedure Update_From_Binary_Trace
     (Element : in out Trace_File_Element;
      File    : Trace_File_Type);
   --  Initialize Element fields using information read from File

   procedure Update_From_Source_Trace
     (Element : in out Trace_File_Element;
      Kind    : GNATcov_RTS.Traces.Supported_Info_Kind;
      Data    : String);
   --  Initialize Element fields using a trace info entry from a source trace
   --  file.

   package Traces_Files_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Trace_File_Element_Acc);

   Files : Traces_Files_Lists.List;

   procedure Checkpoint_Save
     (CSS     : access Checkpoints.Checkpoint_Save_State;
      Context : access Coverage.Context);
   --  Save the current list of trace files to S

   procedure Checkpoint_Clear;
   --  Clear the internal data structures used to create checkpoints

   procedure Checkpoint_Load (CLS : access Checkpoints.Checkpoint_Load_State);
   --  Load list of trace files from S

end Traces_Files_List;
