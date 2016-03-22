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

with Ada.Streams; use Ada.Streams;
with Interfaces; use Interfaces;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Checkpoints;
with Qemu_Traces;  use Qemu_Traces;
with Traces_Dbase; use Traces_Dbase;
with Traces;

package Traces_Files is

   type Trace_File_Type is limited private;
   --  In memory content of a trace file. Note that this only contains the Info
   --  section, not the trace entries themselves.

   type Trace_File_Descriptor is limited private;
   --  Descriptor to open/read a trace file

   Bad_File_Format : exception;
   --  Exception is raised if an input trace file is invalid or corrupted

   Write_Error : exception;
   --  Exception is raised in case of OS error during write

   procedure Create_Trace_File
     (Kind       : Trace_Kind;
      Trace_File : out Trace_File_Type);
   --  Create an empty Trace_File object of the given kind

   generic
      with procedure Process_Info_Entries
        (Trace_File : Trace_File_Type) is null;
      --  Called right before processing trace entries

      with procedure Process_Loadaddr
        (Trace_File : Trace_File_Type;
         Offset     : Traces.Pc_Type) is null;
      --  Called when coming across a Loadaddr special trace entry. Note that
      --  this is not called when such an entry is unexpected.

      with procedure Process_Trace_Entry
        (Trace_File : Trace_File_Type;
         E          : Traces.Trace_Entry) is null;
      --  Called for each regular trace entry (i.e. not for Loadaddr ones)

      Handle_Loadaddr : Boolean := True;
      --  Whether the Loadaddr special trace entry should be handled. If it is,
      --  trace entries whose addresses are located before the load address are
      --  discarded and the addresses are relocated as if the module was
      --  loadded at address 0. Otherwise, all trace entries are yielded
      --  unchanged. In any case, Process_Loadaddr is called when the Loadaddr
      --  entry is decoded.

   procedure Read_Trace_File_Gen
     (Filename   : String;
      Trace_File : out Trace_File_Type);
   --  Open a trace file and read its content. The file is expected to contain
   --  an Info section and a traces section (either flat or with history). Put
   --  the result in Trace_File.
   --
   --  In case of failure, an exception is raised and the file is considered as
   --  not open.

   procedure Read_Trace_File
     (Filename   : String;
      Trace_File : out Trace_File_Type;
      Base       : in out Traces_Base);
   --  Specialization of Read_Trace_File_Gen that imports traces into a base

   procedure Free (Trace_File : in out Trace_File_Type);
   --  Deallocate all dynamic data associated with Trace_File

   procedure Append_Info
     (File : in out Trace_File_Type;
      Kind : Info_Kind_Type;
      Data : String);
   --  Add an info to trace file.
   --  We use a string type even if any byte stream is allowed.

   function Get_Info
     (File : Trace_File_Type; Kind : Info_Kind_Type) return String;
   --  Get an info from trace file.
   --  Return an empty string if the info is not found.

   function Format_Date_Info (Raw_String : String) return String;
   --  Decode a Trace_Info_Date into a human-readable date string. The string
   --  passed as Raw_String must have the same memory layout as a
   --  Trace_Info_Date record.

   procedure Open_Output_Flat_Trace_File
     (Filename   : String;
      Desc       : out Trace_File_Descriptor;
      Trace_File : out Trace_File_Type);
   --  Open a trace file for output and write a flat (raw) trace second header.
   --  In case of failure, an exception is raised and the file is considered
   --  as not open.

   procedure Open_Decision_Map_File
     (Filename   : String;
      Desc       : out Trace_File_Descriptor);
   --  Open a decision map file, without reading the traces. In case of
   --  failure, an exception is raised and the file is considered as not open.

   procedure Write_Trace_Entry
     (Desc       : Trace_File_Descriptor;
      E          : Traces.Trace_Entry);
   --  Write a trace to DESC. In case of failure, an exception is raised and
   --  the file is closed.

   procedure Close_Trace_File
     (Desc : in out Trace_File_Descriptor);
   --  Close DESC

   procedure Write_Trace_File
     (Filename   : String;
      Trace_File : Trace_File_Type;
      Base       : Traces_Base);
   --  Write traces to a file, including trace entries from Base

   procedure Write_Trace_File
     (Filename   : String;
      Trace_File : Trace_File_Type);
   --  Write a trace file of kind Info (no traces base needed)

   procedure Dump_Trace_File (Filename : String);
   --  Dump of a trace file, but handle loadaddr

   procedure Dump_Raw_Trace_File (Filename : String);
   --  Raw dump of a trace file

   procedure Checkpoint_Save
     (S          : access Root_Stream_Type'Class;
      Trace_File : Trace_File_Type);
   --  Save Trace_File's infos to S

   procedure Checkpoint_Load
     (S          : access Root_Stream_Type'Class;
      CS         : access Checkpoints.Checkpoint_State;
      Trace_File : in out Trace_File_Type);
   --  Load Trace_File's infos from S

private
   type Trace_File_Info (Raw_Length : Natural);
   type Trace_File_Info_Acc is access Trace_File_Info;

   type Trace_File_Info (Raw_Length : Natural) is record
      Next : Trace_File_Info_Acc;
      Kind : Info_Kind_Type;

      --  Data for the infos

      Data : String (1 .. Raw_Length);
      --  String type is used for simplicity - although it might be binary.
      --  Should be a Storage_Array???
   end record;

   type Trace_File_Type is record
      --  Parameters from header

      Kind             : Trace_Kind;
      Sizeof_Target_Pc : Unsigned_8;
      Big_Endian       : Boolean;
      Machine          : Unsigned_16;

      --  Linked list of infos

      First_Infos : Trace_File_Info_Acc;
      Last_Infos  : Trace_File_Info_Acc;
   end record;

   type Trace_File_Descriptor is record
      Fd : File_Descriptor;

      --  Parameter from header
      Kind             : Trace_Kind;
      Sizeof_Target_Pc : Unsigned_8;
      Big_Endian       : Boolean;
   end record;
   --  Descriptor to open/read a trace file

end Traces_Files;
