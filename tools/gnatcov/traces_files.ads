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
with Interfaces;  use Interfaces;
private with System.Storage_Elements;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNATCOLL.Mmap;

with Binary_Files; use Binary_Files;
with Qemu_Traces;  use Qemu_Traces;
with Strings;      use Strings;
with Traces_Dbase; use Traces_Dbase;
with Traces;

package Traces_Files is

   type Read_Result_Kind is (Success, Open_Error, Other_Error);

   subtype Error_Read_Result_Kind is
      Read_Result_Kind range Open_Error .. Other_Error;

   subtype Recoverable_Read_Result_Kind is
      Read_Result_Kind range Open_Error .. Open_Error;
   --  Errors that are actually recoverable which should only trigger a warning

   type Read_Result (Kind : Read_Result_Kind := Success) is record
      case Kind is
         when Success =>
            null;
         when others =>
            Error : Unbounded_String;
      end case;
   end record;
   --  Description of the result of a trace read operation: either it was
   --  successful or it failed with some error message.

   --  While this unit historically only deals with binary traces files (i.e.
   --  traces coming from the execution of uninstrumented programs), the
   --  following procedure enables to probe a trace file in order to determine
   --  if it's a binary trace file or a source trace file.

   function Is_Success (R : Read_Result) return Boolean is
     (R.Kind = Success);

   type Any_Accepted_Trace_Kind is
     (Unknown, Binary_Trace_File, Source_Trace_File, All_Trace_Files);
   --  We normally do not allow to mix trace kinds, but in case this should be
   --  supported in the future we have "All_Trace_File" to represent that
   --  gnatcov accepts both kinds of traces.

   function Currently_Accepted_Trace_Kind return Any_Accepted_Trace_Kind;
   --  The trace kind accepted by gnatcov during this run. Can be updated
   --  with Update_Current_Trace_Kind.

   procedure Update_Current_Trace_Kind (New_Kind : Any_Accepted_Trace_Kind);
   --  Update the accepted trace kind, and emit a fatal error when detecting
   --  inconsistent trace kinds. If Switches.Allow_Mix_Trace_Kinds is True,
   --  only emit a warning and not an error.

   subtype Trace_File_Kind
     is Any_Accepted_Trace_Kind range Binary_Trace_File .. Source_Trace_File;

   function Image (Kind : Trace_File_Kind) return String;
   --  Human-readable name for Kind

   procedure Probe_Trace_File
     (Filename : String;
      Kind     : out Trace_File_Kind;
      Result   : out Read_Result);
   --  Open Filename in read mode as a trace file and try to determine the kind
   --  of trace file it is.
   --
   --  If there is an error opening or reading this file or if its kind is
   --  unknown, put the relevant error information in Result. Otherwise, set
   --  Result.Success to True and put the trace file kind in Kind.

   type Trace_File_Type is limited private;
   --  In memory content of a trace file. Note that this only contains the Info
   --  section, not the trace entries themselves.

   type Trace_File_Descriptor is limited private;
   --  Descriptor to read or write a trace file

   Read_Success : constant Read_Result := (Kind => Success);

   procedure Create_Error (Result : out Read_Result; Error : String);
   procedure Create_Open_Error (Result : out Read_Result; Filename : String);
   --  Shortcut to create a Read_Result that contains an error

   procedure Success_Or_Fatal_Error (Filename : String; Result : Read_Result);
   --  If Result.Success is true, do nothing. Otherwise, raise a fatal error.

   Write_Error : exception;
   --  Exception is raised in case of OS error during write

   procedure Create_Trace_File
     (Filename   : String;
      Kind       : Trace_Kind;
      Trace_File : out Trace_File_Type);
   --  Create an empty Trace_File object of the given kind

   function Kind (Trace_File : Trace_File_Type) return Trace_Kind;
   --  Return Trace_File's kind

   function Filename (Trace_File : Trace_File_Type) return String;
   --  Return Traces_File's filename, or an empty string if it was not read
   --  from a file.

   generic
      type Shared_Object_Type is private;
      --  Type to use to represent shared objects

      No_Shared_Object : Shared_Object_Type;
      --  Value to use to mean "it's not in a shared object, it's in the main
      --  executable".

      with procedure Process_Info_Entries
        (Trace_File : Trace_File_Type;
         Result     : out Read_Result) is null;
      --  Called right before processing trace entries. If Result.Success is
      --  set to False, this will abort trace reading and will forward error
      --  information.

      with procedure Process_Loadaddr
        (Trace_File : Trace_File_Type;
         Offset     : Traces.Pc_Type) is null;
      --  Called when coming across a Loadaddr special trace entry. Note that
      --  this is not called when such an entry is unexpected.

      with function Load_Shared_Object
         (Trace_File  : Trace_File_Type;
          Filename    : String;
          Signature   : Binary_File_Signature;
          First, Last : Traces.Pc_Type) return Shared_Object_Type is <>;
      --  Called when processing a load shared object event. The result will be
      --  used as an actual for the SO formal in Process_Trace_Entry when
      --  processing trace entries related to this shared object.

      with procedure Process_Trace_Entry
        (Trace_File : Trace_File_Type;
         SO         : Shared_Object_Type;
         E          : Traces.Trace_Entry) is null;
      --  Called for each regular trace entry (i.e. not for special ones). Some
      --  regular entries may also be skipped: see documentation below for
      --  Handle_Relocations.

      Handle_Relocations : Boolean := True;
      --  Whether trace entries are relocated using Loadaddr and shared object
      --  load/unload events.
      --
      --  Most uses need relocations, hence the default value. The only use
      --  case for disabling relocation is to be able to dump the trace file as
      --  it is encoded on the disk, so only for debug purposes.
      --
      --  Note that when this is True, in the case of Loadaddr handling, trace
      --  entries:
      --    * for which the PC is lower than the Loadaddr load address,
      --    * that appear before the Loadaddr special trace entry in the stream
      --      of trace entries
      --  are discarded: we will not call Process_Trace_Entry on them, as they
      --  are irrelevant after the relocation process.

   procedure Read_Trace_File_Gen
     (Filename   : String;
      Trace_File : out Trace_File_Type;
      Result     : out Read_Result);
   --  Open a trace file and read its content. The file is expected to contain
   --  an Info section and a traces section (either flat or with history). Put
   --  the result in Trace_File.
   --
   --  If successful, Result.Success is set to True. Otherwise, Result is set
   --  to the corresponding error information. In case of error, Trace_File is
   --  left undefined.

   procedure Check_Trace_File_From_Exec
     (Trace_File : Trace_File_Type;
      Result     : out Read_Result);
   --  If Trace_File is not a trace file that is the result of program
   --  execution, set Result to the corresponding error information.

   procedure Read_Trace_File
     (Filename   : String;
      Trace_File : out Trace_File_Type;
      Result     : out Read_Result;
      Base       : in out Traces_Base);
   --  Specialization of Read_Trace_File_Gen that imports traces into a base.
   --  In case of error, the Base is partially completed with trace entries
   --  that were successfully read from Filename.
   --
   --  TODO??? This does not handle shared objects.

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

   function Parse_Date_Info (Formatted : String) return String;
   --  Decode the human-readable date string that Format_Date_Info returns into
   --  a Trace_Info_Data record memory dump.

   procedure Open_Output_Flat_Trace_File
     (Filename   : String;
      Desc       : out Trace_File_Descriptor;
      Trace_File : out Trace_File_Type);
   --  Open a trace file for output and write a flat (raw) trace second header.
   --  In case of failure, a fatal error is raised.

   procedure Write_Trace_Entry
     (Desc : Trace_File_Descriptor;
      E    : Traces.Trace_Entry);
   --  Write a trace to DESC. In case of failure, an exception is raised and
   --  the file is closed.

   procedure Close_Trace_File
     (Desc : in out Trace_File_Descriptor);
   --  Close DESC

   function Get_Signature
     (File : Trace_File_Type)
      return Binary_File_Signature;
   --  Return the signature of the executable that was used to produce the File
   --  trace file.

   procedure Write_Trace_File
     (Trace_File : Trace_File_Type;
      Base       : Traces_Base);
   --  Write traces to a file, including trace entries from Base

   procedure Write_Trace_File (Trace_File : Trace_File_Type)
      with Pre => Kind (Trace_File) = Info;
   --  Write a trace file of kind Info (no traces base needed)

   procedure Dump_Trace_File (Filename : String);
   --  Dump of a trace file, but handle loadaddr

   procedure Dump_Raw_Trace_File (Filename : String);
   --  Raw dump of a trace file

   type Requested_Trace is record
      Filename   : Unbounded_String;
      Executable : Unbounded_String;
   end record;
   --  Trace file that is passed to gnatcov for analysis. Filename designates
   --  the trace file, and, if non-empty, Executable overrides the binary used
   --  for decision mapping when analyzing this trace file.

   package Requested_Trace_Vectors is new Ada.Containers.Vectors
     (Positive, Requested_Trace);

private

   package SSE renames System.Storage_Elements;
   package Mmap renames GNATCOLL.Mmap;

   type Trace_File_Header is record
      Kind             : Trace_Kind;
      Sizeof_Target_Pc : Unsigned_8;
      Big_Endian       : Boolean;
      Machine          : Unsigned_16;
   end record;
   --  Holder for the information present in trace file headers. They are
   --  necessary to decode the rest of the trace file.

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
      Filename : Unbounded_String;
      Header   : Trace_File_Header;

      --  Linked list of infos

      First_Infos : Trace_File_Info_Acc;
      Last_Infos  : Trace_File_Info_Acc;
   end record;

   type Trace_File_Descriptor (Writeable : Boolean := False) is record
      Filename : Unbounded_String;
      Header   : Trace_File_Header;

      --  Use memory mapping to read trace files (for efficiency), and
      --  traditional I/O primitives to write them (for simplicity).

      case Writeable is
         when False =>
            File   : Mmap.Mapped_File;
            Region : Mmap.Mapped_Region;
            --  File and region for the trace file to read

            Buffer : System.Address;
            --  Address of the first byte in Region

            Length : SSE.Storage_Count;
            --  Number of bytes available in Region

            Position : SSE.Storage_Offset;
            --  Offset of the next byte to process in the trace file. We
            --  process files from the beginnig to the end, like with
            --  tratidional I/O.

         when True =>
            Fd : File_Descriptor;
            --  Descriptor for the trace file to write
      end case;
   end record;
   subtype Input_Trace_File is Trace_File_Descriptor (Writeable => False);
   subtype Output_Trace_File is Trace_File_Descriptor (Writeable => True);

   type Read_Status is (None, Partial, Full);
   --  Whether a read was null (no byte read while some requested), partial (at
   --  least one read, but not as many as requested) or full (all requested
   --  bytes read).

   function Read_Or_None
     (Desc   : in out Input_Trace_File;
      Size   : Natural;
      Buffer : out System.Address) return Read_Status;
   --  Read Size bytes from Desc and put the address of the buffer holding the
   --  bytes in Buffer. Return how the read completed.

   function Read
     (Desc   : in out Input_Trace_File;
      Size   : Natural;
      Buffer : out System.Address) return Boolean;
   --  Like Read_Or_None, but return instead whether the read was full

   function Write
     (Desc   : Output_Trace_File;
      Buffer : System.Address;
      Size   : Positive) return Boolean;
   --  Try to write the Size bytes at the Buffer adress to Desc. Return whether
   --  the write completed.

   function Kind (Trace_File : Trace_File_Type) return Trace_Kind is
     (Trace_File.Header.Kind);

   function Filename (Trace_File : Trace_File_Type) return String is
     (+Trace_File.Filename);

end Traces_Files;
