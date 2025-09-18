------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

with Checkpoints;
with Coverage;
with Strings;      use Strings;
with Traces_Files; use Traces_Files;
with Traces_Source;

--  This package keeps track of all processed trace files

package Traces_Files_Registry is

   type Trace_File_Element is limited record
      Filename : Unbounded_String;
      --  File name for the trace file, as passed to "gnatcov coverage"

      Kind : Trace_File_Kind;
      --  Kind of trace file this describes

      Context : Unbounded_String;
      --  Empty string if this trace file was loaded by this instance of
      --  gnatcov.  Otherwise, string to be decoded with Coverage.From_String,
      --  that describes the context where this trace file has actually been
      --  processed.

      Program_Name : Unbounded_String;
      --  Name of the program that was executed to produce this trace

      Time : Unbounded_String;
      --  Production time for this trace file. The date format is the one
      --  returned by Traces_Files.Format_Date_Info.

      User_Data : Unbounded_String;
      --  Custom user data attached to this trace file
   end record;

   type Trace_File_Element_Acc is access Trace_File_Element;

   --  The following comparison/hashing functions consider Trace_File_Element
   --  records as a tuple: comparisons implement structual equivalence.

   function Equivalent (Left, Right : Trace_File_Element_Acc) return Boolean;
   function "<" (Left, Right : Trace_File_Element_Acc) return Boolean;
   function Hash
     (Element : Trace_File_Element_Acc) return Ada.Containers.Hash_Type;
   procedure Free is new
     Ada.Unchecked_Deallocation (Trace_File_Element, Trace_File_Element_Acc);

   function Create_Trace_File_Element
     (Filename : String; Kind : Trace_File_Kind) return Trace_File_Element_Acc;
   --  Create a new trace file element for our list

   procedure Update_From_Binary_Trace
     (Element : in out Trace_File_Element; File : Trace_File_Type);
   --  Initialize Element fields using information read from File

   procedure Update_From_Source_Trace
     (Element : in out Trace_File_Element;
      Kind    : Traces_Source.Supported_Info_Kind;
      Data    : String);
   --  Initialize Element fields using a trace info entry from a source trace
   --  file.

   package Traces_Files_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Trace_File_Element_Acc);

   procedure Add_Traces_File (TF : in out Trace_File_Element_Acc);
   --  Add TF to the list of known traces files. If TF was already known, free
   --  it.

   function Sorted_Traces_Files return Traces_Files_Vectors.Vector;
   --  Return a sorted vector for all traces files

   procedure Iterate_On_Traces_Files
     (Callback : access procedure (Element : Trace_File_Element));
   --  Iterate on the sorted list of all traces files

   procedure Checkpoint_Save
     (CSS     : access Checkpoints.Checkpoint_Save_State;
      Context : access Coverage.Context);
   --  Save the current list of trace files to S

   procedure Checkpoint_Clear;
   --  Clear the internal data structures used to create checkpoints

   procedure Checkpoint_Load (CLS : in out Checkpoints.Checkpoint_Load_State);
   --  Load list of trace files from S

end Traces_Files_Registry;
