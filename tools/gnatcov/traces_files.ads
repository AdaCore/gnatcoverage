------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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
with Interfaces; use Interfaces;

with Qemu_Traces;  use Qemu_Traces;
with Traces_Dbase; use Traces_Dbase;
with Traces;

package Traces_Files is

   type Trace_File_Type is limited private;
   --  Trace file descriptor

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
   --  Needs comment???

   procedure Free (Trace_File : in out Trace_File_Type);
   --  Deallocate all dynamic data associated with Trace_File

   Bad_File_Format : exception;
   --  Exception is raised if the trace file is invalid or corrupted

   Write_Error : exception;
   --  Exception is raised in case of OS error during write

   procedure Read_Trace_File
     (Filename   : String;
      Trace_File : out Trace_File_Type;
      Base       : in out Traces_Base);
   --  Load in memory (and possibly merge) a trace file

   procedure Read_Trace_File
     (Filename   : String;
      Trace_File : out Trace_File_Type;
      Info_Cb    : access procedure (File : Trace_File_Type);
      Trace_Cb   : access procedure (E : Traces.Trace_Entry));
   --  Read a trace file, call Info_Cb after reading infos (if not null), and
   --  call Trace_Cb for each entry.

   procedure Write_Trace_File
     (Filename   : String;
      Trace_File : Trace_File_Type;
      Base       : Traces_Base);
   --  Write traces to a file

   procedure Write_Trace_File
     (Filename   : String;
      Trace_File : Trace_File_Type);
   --  Write a trace file of kind Info (no traces base needed)

   procedure Dump_Trace_File (Filename : String);
   --  Raw dump of a trace file

   procedure Create_Trace_File
     (Kind       : Trace_Kind;
      Trace_File : out Trace_File_Type);
   --  Create an empty Trace_File object of the given kind

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

end Traces_Files;
