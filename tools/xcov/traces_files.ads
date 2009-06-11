------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Qemu_Traces;  use Qemu_Traces;
with Traces_Dbase; use Traces_Dbase;
with Traces;

package Traces_Files is
   --  Trace file descriptor.
   type Trace_File_Type is limited private;

   --  Add an info to trace file.
   --  We use a string type even if any byte stream is allowed.
   procedure Append_Info (File : in out Trace_File_Type;
                          Kind : Info_Kind_Type;
                          Data : String);

   --  Get an info from trace file.
   --  Return an empty string if the info is not found.
   function Get_Info
     (File : Trace_File_Type; Kind : Info_Kind_Type) return String;
   function Format_Date_Info (Raw_String : String) return String;

   --  Deallocate all dynamic data.
   procedure Free (Trace_File : in out Trace_File_Type);

   --  This exception is raised if the trace file is invalid or corrupted.
   Bad_File_Format : exception;

   --  This exception is raised in case of OS error during write.
   Write_Error : exception;

   --  Load in memory (and possibly merge) a trace file.
   procedure Read_Trace_File (Filename : String;
                              Trace_File : out Trace_File_Type;
                              Base : in out Traces_Base);

   --  Read a trace file, call Info_Cb after reading infos (if not null)
   --  call Trace_Cb for each entry.
   procedure Read_Trace_File
     (Filename : String;
      Trace_File : out Trace_File_Type;
      Info_Cb : access procedure (File : Trace_File_Type);
      Trace_Cb : access procedure (E : Traces.Trace_Entry));

   --  Write traces to a file.
   --  Always generate a consolidated file.
   procedure Write_Trace_File (Filename : String;
                               Trace_File : Trace_File_Type;
                               Base : Traces_Base);

   procedure Write_Trace_File (Filename : String;
                               Trace_File : Trace_File_Type);

   --  Raw dump of a trace file.
   procedure Dump_Trace_File (Filename : String);

   --  Create (in memory) an empty trace_file.
   procedure Create_Trace_File (Trace_File : out Trace_File_Type);

   --  Add coverage annotations to the objdump disassembly output.
   --  Read objdump output from standard input.
   --  procedure Annotate_Objdump;

private
   type Trace_File_Info (Raw_Length : Natural);
   type Trace_File_Info_Acc is access Trace_File_Info;

   type Trace_File_Info (Raw_Length : Natural) is record
      Next : Trace_File_Info_Acc;
      Kind : Info_Kind_Type;

      --  Data for the infos.
      --  String type is used for simplicity - although it might be binary.
      --  Should be a Storage_Array???
      Data : String (1 .. Raw_Length);
   end record;

   type Trace_File_Type is record
      --  Parameter from header.
      Kind             : Trace_Kind;
      Sizeof_Target_Pc : Unsigned_8;
      Big_Endian       : Boolean;
      Machine          : Unsigned_16;

      --  Infos.
      --  Simply linked list.
      First_Infos : Trace_File_Info_Acc;
      Last_Infos  : Trace_File_Info_Acc;
   end record;

end Traces_Files;
