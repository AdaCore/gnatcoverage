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

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Calendar_Utils;
with Hex_Images; use Hex_Images;
with Inputs;     use Inputs;
with Outputs;    use Outputs;
with Qemu_Traces_Entries;
with Swaps;
with Switches;
with Traces;     use Traces;
with Traces_Source;

package body Traces_Files is

   type File_Open_Mode is (Read_File, Create_File, Append_File);

   subtype Qemu_Trace_Entry is Qemu_Traces_Entries.Trace_Entry;

   -----------------------------------------------------------
   -- Helpers to read/write binary data from/to trace files --
   -----------------------------------------------------------

   Trace_Header_Size      : constant Natural :=
      Trace_Header'Size / System.Storage_Unit;
   Trace_Info_Header_Size : constant Natural :=
      Trace_Info_Header'Size / System.Storage_Unit;
   Trace_Entry_Size       : constant Natural :=
      Qemu_Trace_Entry'Size / System.Storage_Unit;

   generic
      type Object (<>) is limited private;
      --  Object to read in memory

      Object_Size : Natural;
      --  Size of this object, in bytes

   package Object_IO is
      --  Wrappers around the Read_Or_None/Read function to deal with actual
      --  object types instead of System.Address values.

      type Ref is access all Object;

      function Read_Or_None
        (Desc   : in out Input_Trace_File;
         Object : out Ref) return Read_Status;
      function Read
        (Desc   : in out Input_Trace_File;
         Object : out Ref) return Boolean;

   end Object_IO;

   ---------------
   -- Object_IO --
   ---------------

   package body Object_IO is

      function Convert is new Ada.Unchecked_Conversion (System.Address, Ref);

      ------------------
      -- Read_Or_None --
      ------------------

      function Read_Or_None
        (Desc   : in out Input_Trace_File;
         Object : out Ref) return Read_Status
      is
         Object_Address : System.Address;
      begin
         return Result : constant Read_Status := Read_Or_None
           (Desc, Object_Size, Object_Address)
         do
            Object := Convert (Object_Address);
         end return;
      end Read_Or_None;

      ----------
      -- Read --
      ----------

      function Read
        (Desc   : in out Input_Trace_File;
         Object : out Ref) return Boolean
      is
      begin
         return Read_Or_None (Desc, Object) = Full;
      end Read;

   end Object_IO;

   package Trace_Header_IO is new Object_IO
     (Trace_Header, Trace_Header_Size);
   package Trace_Info_Header_IO is new Object_IO
     (Trace_Info_Header, Trace_Info_Header_Size);
   package Trace_Entry_IO is new Object_IO
     (Qemu_Trace_Entry, Trace_Entry_Size);

   Current_Trace_Kind : Any_Accepted_Trace_Kind := Unknown;
   --  The currently accepted trace kind, start as Unknown and is updated as
   --  soon as we load a trace, a checkpoint or start instrumenting a project.

   procedure Open_Trace_File
     (Filename   : String;
      Desc       : out Trace_File_Descriptor;
      Trace_File : out Trace_File_Type);
   --  Open a trace file, check headers consistency and read leading Info
   --  entries without reading the traces. In case of failure, an exception is
   --  raised and the file is considered as not open.

   generic
      with procedure Process_Info (Kind : Info_Kind_Type; Data : String);
      --  Called for each trace info entry

   procedure Read_Trace_File_Entries
     (Desc : in out Trace_File_Descriptor; Result : out Read_Result);
   --  Read all trace file info entries from Desc and call Process_Info for
   --  each of them. Raise a fatal error if one entry is invalid.

   procedure Append_Info_Entries_From_Descriptor
     (Desc       : in out Trace_File_Descriptor;
      Trace_File : in out Trace_File_Type;
      Result     : out Read_Result);
   --  Calls Read_Trace_File_Entries to add all file info entries to Trace_File

   procedure Read_Trace_Entry
     (Desc   : in out Trace_File_Descriptor;
      Eof    : out Boolean;
      E      : out Trace_Entry_IO.Ref;
      Result : out Read_Result);
   --  Read a trace from DESC. Set EOF to True in case of end-of-file (in
   --  this case E isn't set), otherwise EOF is set to False and E is
   --  valid. In case of failure, an exception is raised and the file is
   --  closed.

   procedure Dump_Infos (Trace_File : Trace_File_Type);
   procedure Write_Trace_File_Info
     (Desc       : Output_Trace_File;
      Trace_File : Trace_File_Type);
   procedure Write_Trace_File_Traces
     (Desc : Output_Trace_File;
      Kind : Trace_Kind;
      Base : Traces_Base);
   --  Need comments???

   procedure Dump_Trace_File (Filename : String; Raw : Boolean);
   --  Dump a trace file, offsetting addresses unless Raw is true

   function Make_Trace_Header (Kind : Trace_Kind) return Trace_Header;
   --  Create a trace file header with the given kind

   Truncated_File : exception;

   procedure Check_Header
     (Desc   : in out Trace_File_Descriptor;
      Hdr    : out Trace_Header_IO.Ref;
      Result : out Read_Result);
   --  Read and perform basic validation on Hdr: make sure it has the proper
   --  magic header, format version and that the endianity is valid. Update the
   --  endianity information in Desc if everything is fine.
   --
   --  If something is invalid, report the error in Result.

   procedure Read_Trace_File_Headers
     (Desc             : in out Trace_File_Descriptor;
      Trace_File       : in out Trace_File_Type;
      Result           : out Read_Result;
      For_Trace_Output : Boolean := False);
   --  Read headers in Desc, update Trace_File with the information we get.
   --  According to Qemu_Traces:
   --
   --  If For_Trace_Output is False:
   --
   --    * read one header;
   --
   --    * if the first header has an Info kind, try to read the second header
   --      and check that its kind is either Flat or History.
   --
   --  If For_Trace_Output is True:
   --
   --    * read one header and check that its kind is Info
   --
   --  If one header has an invalid format, or if the sequence of headers is
   --  invalid, raise a fatal error.

   procedure Decode_Trace_Header
     (Hdr        : Trace_Header;
      Trace_File : in out Trace_File_Type;
      Desc       : in out Trace_File_Descriptor;
      Result     : out Read_Result);

   function Open_File
     (Filename : String;
      Mode     : File_Open_Mode) return Trace_File_Descriptor;
   --  Open a file, without reading or writing to it. In case of failure, an
   --  exception is raised.

   procedure Read_SO_Info
     (Desc      : in out Trace_File_Descriptor;
      Filename  : out String_Access;
      Signature : out Binary_File_Signature;
      Code_Size : out Traces.Pc_Type;
      Result    : out Read_Result);
   --  Read the trace info entries related to a shared object load event.
   --
   --  If any information is missing, set Result to the corresponding error
   --  information. Set it to Read_Success otherwise.

   function Decode_Trace_Entry
     (E      : Qemu_Trace_Entry;
      Offset : Pc_Type := 0)
      return Trace_Entry;
   --  Turn a raw trace entry from a trace file into a high-level trace entry
   --  that fits our internal data structures. Offset is used to relocate the
   --  entry. It is 0 for statically linked code and non-null for dynamically
   --  linked code.

   -----------------------------------
   -- Currently_accepted_Trace_Kind --
   -----------------------------------

   function Currently_Accepted_Trace_Kind return Any_Accepted_Trace_Kind is
     (Current_Trace_Kind);

   -------------------------------
   -- Update_Current_Trace_Kind --
   -------------------------------

   procedure Update_Current_Trace_Kind (New_Kind : Any_Accepted_Trace_Kind) is
      Trace_Mix_Error_Msg : constant String :=
        "Mixing source traces and binary traces is not supported. Please only"
        & " use a single kind of traces.";
   begin
      case Current_Trace_Kind is
         when Unknown =>
            Current_Trace_Kind := New_Kind;
         when Source_Trace_File | Binary_Trace_File =>
            if Current_Trace_Kind /= New_Kind then

               --  Warn or emit an error message if we have inconsistent
               --  supported trace kinds.

               if Switches.Allow_Mixing_Trace_Kinds then
                  Warn (Trace_Mix_Error_Msg);
               else
                  Fatal_Error (Trace_Mix_Error_Msg);
               end if;
               Current_Trace_Kind := All_Trace_Files;
            end if;
         when All_Trace_Files =>

            --  We cannot accept more trace kinds than All_Trace_File,
            --  so there is nothing to do.

            null;
      end case;
   end Update_Current_Trace_Kind;

   ----------------------------
   -- Success_Or_Fatal_Error --
   ----------------------------

   procedure Success_Or_Fatal_Error (Filename : String; Result : Read_Result)
   is
   begin
      if not Is_Success (Result) then
         Fatal_Error (Filename & ": " & (+Result.Error));
      end if;
   end Success_Or_Fatal_Error;

   -----------
   -- Image --
   -----------

   function Image (Kind : Trace_File_Kind) return String is
   begin
      case Kind is
         when Binary_Trace_File =>
            return "binary";
         when Source_Trace_File =>
            return "source";
      end case;
   end Image;

   ------------------
   -- Read_Or_None --
   ------------------

   function Read_Or_None
     (Desc   : in out Input_Trace_File;
      Size   : Natural;
      Buffer : out System.Address) return Read_Status
   is
      use SSE;

      Old_Position : constant Storage_Offset := Desc.Position;
      New_Position : constant Storage_Offset :=
         Desc.Position + Storage_Count (Size);
   begin
      Buffer := Desc.Buffer + Desc.Position;
      Desc.Position := Storage_Offset'Min (New_Position, Desc.Length);

      --  If there are not enough bytes left in the buffer to read in order to
      --  provide Size bytes, return that it's a partial read.

      if New_Position <= Desc.Length or else Size = 0 then
         return Full;
      elsif Old_Position = Desc.Length then
         return None;
      else
         return Partial;
      end if;
   end Read_Or_None;

   ----------
   -- Read --
   ----------

   function Read
     (Desc   : in out Input_Trace_File;
      Size   : Natural;
      Buffer : out System.Address) return Boolean
   is
   begin
      return Read_Or_None (Desc, Size, Buffer) = Full;
   end Read;

   -----------
   -- Write --
   -----------

   function Write
     (Desc   : Output_Trace_File;
      Buffer : System.Address;
      Size   : Positive) return Boolean
   is
   begin
      return Write (Desc.Fd, Buffer, Size) = Size;
   end Write;

   ----------------------
   -- Probe_Trace_File --
   ----------------------

   procedure Probe_Trace_File
     (Filename : String;
      Kind     : out Trace_File_Kind;
      Result   : out Read_Result)
   is
      Fd : constant File_Descriptor := Open_Read (Filename, Binary);

      Binary_Magic : String renames Qemu_Trace_Magic;
      Source_Magic : String renames Traces_Source.Trace_File_Magic;

      Magic_Max_Size : constant Natural := Integer'Max
        (Binary_Magic'Length, Source_Magic'Length);
      Buffer         : String (1 .. Magic_Max_Size);

      function Magic_Matches (Magic : String) return Boolean
      is (Buffer (1 .. Magic'Length) = Magic);
      --  Return whether the given Magic matches the first bytes in Buffer

   begin
      Result := (Kind => Success);
      Kind := Binary_Trace_File;

      if Fd = Invalid_FD then
         Create_Open_Error (Result, Filename);
         return;
      end if;

      if Read (Fd, Buffer'Address, Buffer'Length) /= Buffer'Length then
         Create_Error (Result, "cannot read header of " & Filename);
      elsif Magic_Matches (Binary_Magic) then
         Kind := Binary_Trace_File;
      elsif Magic_Matches (Source_Magic) then
         Kind := Source_Trace_File;
      else
         Create_Error (Result, "invalid trace file: " & Filename);
      end if;

      Close (Fd);
   end Probe_Trace_File;

   ------------------
   -- Check_Header --
   ------------------

   procedure Check_Header
     (Desc   : in out Trace_File_Descriptor;
      Hdr    : out Trace_Header_IO.Ref;
      Result : out Read_Result)
   is
   begin
      --  First read the header

      if not Trace_Header_IO.Read (Desc, Hdr) then
         Create_Error (Result, "cannot read header");
      end if;

      --  Then check that it contains valid data

      if Hdr.Magic /= Qemu_Trace_Magic then
         Create_Error (Result, "invalid header (bad magic)");

      elsif Hdr.Version /= Qemu_Trace_Version then
         Create_Error (Result, "invalid header (bad version)");

      elsif not Hdr.Big_Endian'Valid then
         Create_Error (Result, "invalid header (bad endianness)");

      elsif not Hdr.Kind'Valid then
         Create_Error (Result, "invalid header (bad kind)");
      end if;

      --  Finally, return queried data

      Desc.Header.Big_Endian := Hdr.Big_Endian;
   end Check_Header;

   -----------------------------
   -- Read_Trace_File_Headers --
   -----------------------------

   procedure Read_Trace_File_Headers
     (Desc             : in out Trace_File_Descriptor;
      Trace_File       : in out Trace_File_Type;
      Result           : out Read_Result;
      For_Trace_Output : Boolean := False)
   is
      Hdr : Trace_Header_IO.Ref;
   begin
      Result := Read_Success;

      --  Read the first header, making sure it has a valid kind as a first
      --  header. If it is supposed to be the only header (Decision_Map), stop
      --  there.

      Check_Header (Desc, Hdr, Result);
      if not Is_Success (Result) then
         return;
      end if;

      case Hdr.Kind is
         when Flat | History =>
            Create_Error
              (Result, "invalid first header: " & Trace_Kind'Image (Hdr.Kind));
            return;

         when Decision_Map =>

            if For_Trace_Output then
               Create_Error
                 (Result, "invalid first header for trace output: "
                          & Trace_Kind'Image (Hdr.Kind));
               return;

            else
               Decode_Trace_Header (Hdr.all, Trace_File, Desc, Result);
               if not Is_Success (Result) then
                  return;
               end if;
            end if;

            return;

         when Info =>
            null;
      end case;

      --  If we reach this point, we know Hdr.Kind = Info: read the info
      --  entries. Then read the second header, also checking it has a valid
      --  kind as a second header.

      Append_Info_Entries_From_Descriptor (Desc, Trace_File, Result);
      if For_Trace_Output or else not Is_Success (Result) then
         return;
      end if;

      Check_Header (Desc, Hdr, Result);
      if not Is_Success (Result) then
         return;
      end if;

      if Hdr.Kind not in Flat | History then
         Create_Error
           (Result, "invalid second header: " & Trace_Kind'Image (Hdr.Kind));
         return;
      end if;

      Decode_Trace_Header (Hdr.all, Trace_File, Desc, Result);
   end Read_Trace_File_Headers;

   -------------------------
   -- Decode_Trace_Header --
   -------------------------

   procedure Decode_Trace_Header
     (Hdr        : Trace_Header;
      Trace_File : in out Trace_File_Type;
      Desc       : in out Trace_File_Descriptor;
      Result     : out Read_Result) is
   begin
      Result := Read_Success;
      Desc.Header.Kind := Hdr.Kind;
      Trace_File.Header.Kind := Hdr.Kind;

      Desc.Header.Sizeof_Target_Pc := Hdr.Sizeof_Target_Pc;
      if Desc.Header.Sizeof_Target_Pc /= 4
         and then Desc.Header.Sizeof_Target_Pc /= 8
      then
         Create_Error (Result, "invalid header (bad pc size)");
         return;
      end if;

      Trace_File.Header.Machine := Unsigned_16 (Hdr.Machine_Hi) * 256
        + Unsigned_16 (Hdr.Machine_Lo);

      if ELF_Machine = 0 or else ELF_Machine = Trace_File.Header.Machine then
         ELF_Machine := Trace_File.Header.Machine;
         Machine := Decode_EM (ELF_Machine);
      else
         Create_Error (Result, "target machine doesn't match previous one");
         return;
      end if;
   end Decode_Trace_Header;

   ---------------
   -- Open_File --
   ---------------

   function Open_File
     (Filename : String;
      Mode     : File_Open_Mode) return Trace_File_Descriptor
   is
      procedure Cannot_Open_Error with No_Return;
      --  Call Fatal_Error to complain that we cannot open Filename

      procedure Cannot_Open_Error is
      begin
         Fatal_Error (Filename & ": cannot open file");
      end Cannot_Open_Error;

   begin
      return Desc : Trace_File_Descriptor (Writeable => Mode /= Read_File) do
         Desc.Filename := +Filename;
         case Mode is
            when Read_File =>
               Log_File_Open (Filename);
               begin
                  Desc.File := Mmap.Open_Read (Filename);
               exception
                  when Name_Error =>
                     Cannot_Open_Error;
               end;

               --  Make the region mutable so that we can perform byte swap if
               --  needed.

               Desc.Region := Mmap.Read (Desc.File, Mutable => True);
               Desc.Buffer := Mmap.Data_Address (Desc.Region);
               Desc.Length := SSE.Storage_Count (Mmap.Data_Size (Desc.Region));
               Desc.Position := 0;

            when Create_File =>
               Desc.Fd := Create_File (Filename, Binary);
            when Append_File =>
               Desc.Fd := Open_Append (Filename, Binary);
         end case;

         if Desc.Writeable and then Desc.Fd = Invalid_FD then
            Cannot_Open_Error;
         end if;
      end return;
   end Open_File;

   ------------------
   -- Read_SO_Info --
   ------------------

   procedure Read_SO_Info
     (Desc      : in out Trace_File_Descriptor;
      Filename  : out String_Access;
      Signature : out Binary_File_Signature;
      Code_Size : out Traces.Pc_Type;
      Result    : out Read_Result)
   is
      Fname : String_Access;
      Sig   : Binary_File_Signature;
      CS    : Pc_Type := 0;

      procedure Process_Info_Entry
        (Kind   : Info_Kind_Type;
         Data   : String);
      --  Decode shared object information and import them in the local
      --  variables above.

      procedure Read_Info_Entries is new Read_Trace_File_Entries
        (Process_Info_Entry);

      ------------------------
      -- Process_Info_Entry --
      ------------------------

      procedure Process_Info_Entry
        (Kind : Info_Kind_Type;
         Data : String)
      is
      begin
         case Kind is
         when Exec_File_Name  => Fname := new String'(Data);
         when Exec_File_Size  => Sig.Size := Long_Integer'Value (Data);
         when Exec_File_Time_Stamp =>
            Sig.Time_Stamp := Time_Stamp_Value (Data);
         when Exec_File_CRC32 => Sig.CRC32 := Unsigned_32'Value (Data);
         when Exec_Code_Size  => CS := Pc_Type'Value (Data);
         when others => null;
         end case;
      end Process_Info_Entry;

   begin
      Code_Size := 0;
      Result := Read_Success;

      Read_Info_Entries (Desc, Result);
      if not Is_Success (Result) then
         return;
      end if;

      if Fname = null
         or else Sig.Size = 0
         or else Sig.Time_Stamp = Invalid_Time
         or else Sig.CRC32 = 0
         or else CS = 0
      then
         Create_Error (Result, "incomplete shared object load event");
         return;
      end if;

      Filename := Fname;
      Signature := Sig;
      Code_Size := CS;
   end Read_SO_Info;

   ------------------------
   -- Decode_Trace_Entry --
   ------------------------

   function Decode_Trace_Entry
     (E      : Qemu_Trace_Entry;
      Offset : Pc_Type := 0)
      return Trace_Entry
   is
      First : constant Pc_Type := E.Pc - Offset;
   begin
      return
        (First => First,
         Last  => First + Pc_Type (E.Size) - 1,
         Op    => E.Op,
         State => Unknown);
   end Decode_Trace_Entry;

   ---------------------
   -- Open_Trace_File --
   ---------------------

   procedure Open_Trace_File
     (Filename   : String;
      Desc       : out Trace_File_Descriptor;
      Trace_File : out Trace_File_Type)
   is
      Result : Read_Result;
   begin
      Desc := Open_File (Filename, Read_File);
      Read_Trace_File_Headers (Desc, Trace_File, Result);
      Success_Or_Fatal_Error (Filename, Result);
   exception
      when E : others =>
         Close_Trace_File (Desc);
         Fatal_Error
           ("processing of trace file " & Filename
            & " failed: " & Exception_Message (E)
            & (if Exception_Identity (E) = Truncated_File'Identity
               then " (truncated file?)"
               else ""));
   end Open_Trace_File;

   ---------------------------------
   -- Open_Output_Flat_Trace_File --
   ---------------------------------

   procedure Open_Output_Flat_Trace_File
     (Filename   : String;
      Desc       : out Trace_File_Descriptor;
      Trace_File : out Trace_File_Type)
   is
      Result   : Read_Result;
      Flat_Hdr : constant Trace_Header := Make_Trace_Header (Flat);
   begin
      --  First, read the current partial trace file

      Desc := Open_File (Filename, Read_File);
      Read_Trace_File_Headers
        (Desc, Trace_File, Result, For_Trace_Output => True);
      Success_Or_Fatal_Error (Filename, Result);
      Close_Trace_File (Desc);

      --  Then, append to this file the second header, for a flat (raw) trace
      --  file.

      Desc := Open_File (Filename, Append_File);
      if not Write (Desc, Flat_Hdr'Address, Trace_Header_Size) then
         raise Write_Error with "failed to write Flat trace header";
      end if;

      --  Set traces info in the Trace_File_Descriptor

      Desc.Header.Kind := Flat_Hdr.Kind;
      Desc.Header.Sizeof_Target_Pc := Flat_Hdr.Sizeof_Target_Pc;
      Desc.Header.Big_Endian := Flat_Hdr.Big_Endian;
      Desc.Header.Machine := 0;

   exception
      when E : others =>
         Close_Trace_File (Desc);
         Fatal_Error
           ("processing of trace file " & Filename
            & " failed: " & Exception_Message (E)
            & (if Exception_Identity (E) = Truncated_File'Identity
               then " (truncated file?)"
               else ""));
   end Open_Output_Flat_Trace_File;

   -----------------------------
   -- Read_Trace_File_Entries --
   -----------------------------

   procedure Read_Trace_File_Entries
     (Desc : in out Trace_File_Descriptor; Result : out Read_Result)
   is
      Ihdr : Trace_Info_Header_IO.Ref;
      Kind : Info_Kind_Type;

      Data_Length  : Natural;
      Data_Address : System.Address;
   begin
      Result := Read_Success;

      loop
         --  Read the Trace_Info_Header and decode it

         if not Trace_Info_Header_IO.Read (Desc, Ihdr) then
            Create_Error (Result, "cannot read info header");
            return;
         end if;

         if Desc.Header.Big_Endian /= Big_Endian_Host then
            Swaps.Swap_32 (Ihdr.Info_Kind);
            Swaps.Swap_32 (Ihdr.Info_Length);
         end if;

         begin
            Kind := Info_Kind_Type'Val (Ihdr.Info_Kind);
         exception
            when Constraint_Error =>
               Create_Error
                 (Result, "unknown trace info kind: 0x"
                           & Hex_Image (Ihdr.Info_Kind));
               return;
         end;

         --  If it's an "end" trace info entry, just stop

         if Kind = Info_End then
            if Ihdr.Info_Length /= 0 then
               Create_Error (Result, "bad end info length");
               return;
            end if;
            exit;
         end if;

         --  Read the associated data

         Data_Length := Natural (Ihdr.Info_Length);
         if not Read (Desc, Data_Length, Data_Address) then
            Create_Error (Result, "cannot read info data");
            return;
         end if;

         --  Discard padding. Still check that it's only null bytes.

         declare
            Pad_Len     : Natural := Data_Length mod Trace_Info_Alignment;
            Pad_Address : System.Address;
         begin
            if Pad_Len /= 0 then
               Pad_Len := Trace_Info_Alignment - Pad_Len;
            end if;

            if not Read (Desc, Pad_Len, Pad_Address) then
               Create_Error (Result, "cannot read info pad");
               return;
            end if;

            declare
               Pad : constant String (1 .. Pad_Len)
                  with Import, Address => Pad_Address;
            begin
               for B of Pad loop
                  if B /= Character'Val (0) then
                     Create_Error (Result, "bad padding content");
                  end if;
               end loop;
            end;
         end;

         --  Finally, interpret the data

         declare
            Data : constant String (1 .. Data_Length)
               with Import, Address => Data_Address;
         begin
            Process_Info (Kind, Data);
         end;
      end loop;
   end Read_Trace_File_Entries;

   -----------------------------------------
   -- Append_Info_Entries_From_Descriptor --
   -----------------------------------------

   procedure Append_Info_Entries_From_Descriptor
     (Desc       : in out Trace_File_Descriptor;
      Trace_File : in out Trace_File_Type;
      Result     : out Read_Result)
   is
      procedure Process_Info (Kind : Info_Kind_Type; Data : String);
      --  Hook for Read_Trace_Info_Entries, processing one info entry that was
      --  just read from Desc. Append a representation of that entry to
      --  Trace_File internals.

      procedure Read_Info_Entries is new Read_Trace_File_Entries
        (Process_Info);

      ------------------
      -- Process_Info --
      ------------------

      procedure Process_Info (Kind : Info_Kind_Type; Data : String) is
      begin
         Append_Info (Trace_File, Kind, Data);
      end Process_Info;

   begin
      Read_Info_Entries (Desc, Result);
   end Append_Info_Entries_From_Descriptor;

   -------------------------
   -- Read_Trace_File_Gen --
   -------------------------

   procedure Read_Trace_File_Gen
     (Filename   : String;
      Trace_File : out Trace_File_Type;
      Result     : out Read_Result)
   is
      Desc   : Trace_File_Descriptor;
      Offset : Pc_Type := 0;

      type Shared_Object_Desc is record
         First, Last : Pc_Type;
         --  Address range for the shared object executable code in the process
         --  address space for this trace.

         SO          : Shared_Object_Type;
         --  User data for this shared object
      end record;
      --  Describe the features of a loaded shared library

      function "=" (L, R : Shared_Object_Desc) return Boolean is
        (L.First <= R.Last and then L.Last >= R.First);
      function "<" (L, R : Shared_Object_Desc) return Boolean is
        (L.Last < R.First);
      --  Ordering predicates to order shared objects in our internal mapping.
      --  We consider any overlapping entries to be conflicting: processes are
      --  not supposed to be able to map two things at the same address.

      package Shared_Object_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => Shared_Object_Desc);

      SO_Set    : Shared_Object_Sets.Set;
      --  Set of shared objects that are loaded at some point. Updated each
      --  time we process a loading/unloading event.

      function SOD_For_PC (PC : Pc_Type) return Shared_Object_Desc is
        ((First  => PC, Last => PC, others => <>));
      --  Return a Shared_Object_Desc that can be used as a key for lookups in
      --  SO_Set.

      EOF       : Boolean;
      Raw_Entry : Trace_Entry_IO.Ref;
   begin
      Open_Trace_File (Filename, Desc, Trace_File);
      Trace_File.Filename := +Filename;
      Process_Info_Entries (Trace_File, Result);
      if not Is_Success (Result) then
         return;
      end if;

      --  Look for a Loadaddr special trace entry, if expected

      if Get_Info (Trace_File, Qemu_Traces.Kernel_File_Name)'Length /= 0 then

         --  If execution reaches this point, we know there is a kernel and
         --  therefore a load address. As a consequence, we also know that the
         --  rest of this trace file is only mere trace entries until one that
         --  must be a "loadaddr" special entry.

         loop
            Read_Trace_Entry (Desc, EOF, Raw_Entry, Result);
            if not Is_Success (Result) then
               return;
            elsif EOF then
               Create_Error (Result, "No 'loadaddr' special trace entry");
               return;
            end if;

            if Raw_Entry.Op = Qemu_Traces.Trace_Op_Special then
               if Raw_Entry.Size /= Qemu_Traces.Trace_Special_Loadaddr then
                  Create_Error
                    (Result,
                     "'loadaddr' special trace entry expected but got instead"
                     & " a 0x" & Hex_Image (Raw_Entry.Size)
                     & " special entry");
                  return;
               elsif Raw_Entry.Pc = 0 then
                  Create_Error
                    (Result,
                     "Invalid 'loadaddr' special trace entry: offset must not"
                     & " be 0");
                  return;
               end if;

               Process_Loadaddr (Trace_File, Raw_Entry.Pc);

               if Handle_Relocations then
                  Offset := Raw_Entry.Pc;
               end if;

               exit;
            end if;

            if not Handle_Relocations then
               Process_Trace_Entry
                 (Trace_File, No_Shared_Object,
                  Decode_Trace_Entry (Raw_Entry.all));
            end if;
         end loop;
      end if;

      --  Then process the remaining trace entries

      loop
         Read_Trace_Entry (Desc, EOF, Raw_Entry, Result);
         if not Is_Success (Result) then
            return;
         end if;
         exit when EOF;

         if Raw_Entry.Op = Qemu_Traces.Trace_Op_Special then
            case Raw_Entry.Size is
            when Trace_Special_Loadaddr =>

               --  There can be only one loadaddr special trace entry per trace
               --  file.  If it exists, we already processed it before the
               --  loop, above.

               Create_Error
                 (Result, "Unexpected 'loadaddr' special trace entry.");
               return;

            when Trace_Special_Load_Shared_Object =>
               declare
                  Filename  : String_Access;
                  Sig       : Binary_File_Signature;
                  Code_Size : Pc_Type;

                  First, Last : Pc_Type;
               begin
                  Read_SO_Info (Desc, Filename, Sig, Code_Size, Result);
                  if not Is_Success (Result) then
                     return;
                  end if;
                  First := Raw_Entry.Pc;
                  Last := First + Code_Size - 1;
                  SO_Set.Insert
                    ((First => First,
                      Last  => Last,
                      SO    => Load_Shared_Object
                                 (Trace_File, Filename.all, Sig, First,
                                  Last)));
                  Free (Filename);
               end;

            when Trace_Special_Unload_Shared_Object =>
               SO_Set.Delete (SOD_For_PC (Raw_Entry.Pc));

            when others =>
               Create_Error
                 (Result, "Unknown special trace entry: 0x"
                          & Hex_Image (Raw_Entry.Size));
               return;
            end case;
            goto Skip;
         end if;

         --  If there is an offset, it means that we care about only one module
         --  whereas this trace file contains trace entries inside and outside
         --  this module. We want to process only the relevant entries and
         --  relocate them as if the module was loaded at address 0.
         --
         --  Skip all trace entries that cannot possibly be relevant to this
         --  module (i.e. the one with addresses below "Offset"), and remove
         --  "Offset" from the bounds of the remainder.

         if Offset /= 0 and then Raw_Entry.Pc < Offset then
            goto Skip;
         end if;

         --  Determine if this trace entry is related to a shared object or to
         --  the main executable.

         declare
            use Shared_Object_Sets;

            Cur          : constant Cursor :=
               SO_Set.Find (SOD_For_PC (Raw_Entry.Pc));
            SOD          : Shared_Object_Desc;
            Trace_Offset : Pc_Type;
            SO           : Shared_Object_Type;
         begin
            if Cur = No_Element then
               Trace_Offset := Offset;
               SO := No_Shared_Object;
            else
               SOD := Element (Cur);
               Trace_Offset := (if Handle_Relocations
                                then SOD.First
                                else 0);
               SO := SOD.SO;
            end if;

            Process_Trace_Entry
              (Trace_File, SO,
               Decode_Trace_Entry (Raw_Entry.all, Trace_Offset));
         end;

         << Skip >> null;
      end loop;

      Close_Trace_File (Desc);
      Result := Read_Success;
   end Read_Trace_File_Gen;

   ----------------------
   -- Read_Trace_Entry --
   ----------------------

   procedure Read_Trace_Entry
     (Desc   : in out Trace_File_Descriptor;
      Eof    : out Boolean;
      E      : out Trace_Entry_IO.Ref;
      Result : out Read_Result)
   is
   begin
      Eof := False;
      Result := Read_Success;
      if Desc.Header.Sizeof_Target_Pc /= Pc_Type_Size then
         Create_Error
           (Result, "only" & Unsigned_8'Image (Pc_Type_Size)
                    & " bytes pc are handled");
         return;
      end if;

      --  Read an entry, making sure the read operation could get a whole one

      case Trace_Entry_IO.Read_Or_None (Desc, E) is
         when None =>
            Eof := True;
            return;

         when Partial =>
            Close_Trace_File (Desc);
            Create_Error (Result, "file truncated");
            return;

         when Full =>
            Eof := False;

            if Desc.Header.Big_Endian /= Big_Endian_Host then
               Qemu_Traces.Swap_Pc (E.Pc);
               Swaps.Swap_16 (E.Size);
            end if;
      end case;
   end Read_Trace_Entry;

   -----------------------
   -- Write_Trace_Entry --
   -----------------------

   procedure Write_Trace_Entry
     (Desc : Trace_File_Descriptor;
      E    : Trace_Entry)
   is
      Ent : Qemu_Traces_Entries.Trace_Entry;
   begin

      if Desc.Header.Sizeof_Target_Pc /= Pc_Type_Size then
         raise Write_Error with
            "only" & Unsigned_8'Image (Pc_Type_Size)
            & " bytes pc are handled";
      end if;

      Ent.Pc   := E.First;
      Ent.Size := Unsigned_16 (E.Last - E.First + 1);
      Ent.Op   := E.Op;

      if Desc.Header.Big_Endian /= Big_Endian_Host then
         Qemu_Traces.Swap_Pc (Ent.Pc);
         Swaps.Swap_16 (Ent.Size);
      end if;

      --  Write an entry

      if not Write (Desc, Ent'Address, Trace_Entry_Size) then
         raise Write_Error with "file truncated";
      end if;
   end Write_Trace_Entry;

   ----------------------
   -- Close_Trace_File --
   ----------------------

   procedure Close_Trace_File (Desc : in out Trace_File_Descriptor)
   is
   begin
      if Desc.Writeable then
         Close (Desc.Fd);
         Desc.Fd := Invalid_FD;
      else
         Desc.Position := 0;
         Desc.Length := 0;
         Desc.Buffer := System.Null_Address;
         Mmap.Free (Desc.Region);
         Mmap.Close (Desc.File);
      end if;
      Desc.Filename := Null_Unbounded_String;
   end Close_Trace_File;

   --------------------------------
   -- Check_Trace_File_From_Exec --
   --------------------------------

   procedure Check_Trace_File_From_Exec
     (Trace_File : Trace_File_Type;
      Result     : out Read_Result) is
   begin
      Result := Read_Success;
      case Trace_File.Header.Kind is
         when Flat | History =>
            null;

         when Decision_Map =>
            Create_Error
              (Result, "execution trace expected, but this is a decision map");

         when Info =>
            --  If Trace_File's first header has an Info kind, then it is
            --  supposed to have a second header, and Trace_File's kind must
            --  come from this second header. Header reading must have already
            --  ensured that.

            raise Program_Error;
      end case;
   end Check_Trace_File_From_Exec;

   ---------------------
   -- Read_Trace_File --
   ---------------------

   procedure Read_Trace_File
     (Filename   : String;
      Trace_File : out Trace_File_Type;
      Result     : out Read_Result;
      Base       : in out Traces_Base)
   is
      function Load_Shared_Object
         (Ignored_Trace_File : Trace_File_Type;
          Ignored_Filename   : String;
          Ignored_Signature  : Binary_File_Signature;
          Ignored_First      : Traces.Pc_Type;
          Ignored_Last       : Traces.Pc_Type) return Boolean
      is (True);

      procedure Process_Trace_Entry
        (Trace_File : Trace_File_Type;
         SO         : Boolean;
         E          : Trace_Entry);

      procedure Read_Trace_File is new Read_Trace_File_Gen
        (Shared_Object_Type   => Boolean,
         No_Shared_Object     => False,
         Process_Info_Entries => Check_Trace_File_From_Exec,
         Load_Shared_Object   => Load_Shared_Object,
         Process_Trace_Entry  => Process_Trace_Entry);

      -------------------------
      -- Process_Trace_Entry --
      -------------------------

      procedure Process_Trace_Entry
        (Trace_File : Trace_File_Type;
         SO         : Boolean;
         E          : Trace_Entry)
      is
         pragma Unreferenced (Trace_File);
         pragma Unreferenced (SO);
      begin
         Add_Entry (Base, First => E.First, Last => E.Last, Op => E.Op);
      end Process_Trace_Entry;

   begin
      Read_Trace_File (Filename, Trace_File, Result);
   end Read_Trace_File;

   ----------------
   -- Dump_Infos --
   ----------------

   procedure Dump_Infos (Trace_File : Trace_File_Type)
   is
      Info : Trace_File_Info_Acc;
      Is_Print : Boolean;
   begin
      Info := Trace_File.First_Infos;
      while Info /= null loop
         Put ("Tag  : " & Info.Kind'Img);
         case Info.Kind is
            when User_Data =>
               Put (" (User_Tag)");
            when Date_Time =>
               Put (" (Date)");
            when others =>
               null;
         end case;
         New_Line;
         Put_Line ("Len  :" & Natural'Image (Info.Raw_Length));

         Put ("Data :");
         Is_Print := True;
         for I in Info.Data'Range loop
            if not Ada.Characters.Handling.Is_Graphic (Info.Data (I)) then
               Is_Print := False;
               exit;
            end if;
         end loop;
         if Is_Print then
            Put (' ');
            Put_Line (Info.Data);
         else
            for I in Info.Data'Range loop
               Put (' ');
               Put (Hex_Image (Unsigned_8 (Character'Pos (Info.Data (I)))));
            end loop;
            New_Line;
         end if;

         case Info.Kind is
            when Date_Time =>
               Put ("       ");
               if Info.Data'Length /= 8 then
                  Put ("!Bad Format!");
               else
                  Put (Format_Date_Info (Info.Data));
               end if;
               New_Line;
            when others =>
               null;
         end case;
         New_Line;

         Info := Info.Next;
         exit when Info = null;
      end loop;
      Put_Line ("Traces:");
   end Dump_Infos;

   ---------------------
   -- Dump_Trace_File --
   ---------------------

   procedure Dump_Trace_File (Filename : String; Raw : Boolean) is

      procedure Process_Info_Entries
        (Trace_File : Trace_File_Type;
         Result     : out Read_Result);

      procedure Process_Loadaddr
        (Trace_File : Trace_File_Type;
         Offset     : Pc_Type);

      procedure Process_Trace_Entry
        (Trace_File : Trace_File_Type;
         SO         : Unbounded_String;
         E          : Trace_Entry);

      function Load_Shared_Object
        (Trace_File  : Trace_File_Type;
         Filename    : String;
         Signature   : Binary_File_Signature;
         First, Last : Pc_Type)
         return Unbounded_String;

      procedure Read_Trace_File is new Read_Trace_File_Gen
        (Shared_Object_Type   => Unbounded_String,
         No_Shared_Object     => Null_Unbounded_String,
         Process_Info_Entries => Process_Info_Entries,
         Process_Loadaddr     => Process_Loadaddr,
         Load_Shared_Object   => Load_Shared_Object,
         Process_Trace_Entry  => Process_Trace_Entry,
         Handle_Relocations   => not Raw);

      --------------------------
      -- Process_Info_Entries --
      --------------------------

      procedure Process_Info_Entries
        (Trace_File : Trace_File_Type;
         Result     : out Read_Result) is
      begin
         Put_Line ("Kind: " & Trace_Kind'Image (Kind (Trace_File)));
         New_Line;
         Dump_Infos (Trace_File);
         Result := Read_Success;
      end Process_Info_Entries;

      ----------------------
      -- Process_Loadaddr --
      ----------------------

      procedure Process_Loadaddr
        (Trace_File : Trace_File_Type;
         Offset     : Pc_Type)
      is
         pragma Unreferenced (Trace_File);
      begin
         Put_Line ("Kernel was loaded at: " & Hex_Image (Offset));
      end Process_Loadaddr;

      ------------------------
      -- Load_Shared_Object --
      ------------------------

      function Load_Shared_Object
        (Trace_File  : Trace_File_Type;
         Filename    : String;
         Signature   : Binary_File_Signature;
         First, Last : Pc_Type)
         return Unbounded_String
      is
         pragma Unreferenced (Trace_File);
         pragma Unreferenced (Signature);
      begin
         Put_Line ("Loading shared object " & Filename);
         Put_Line ("   at " & Hex_Image (First) & " .. " & Hex_Image (Last));
         return +Filename;
      end Load_Shared_Object;

      -------------------------
      -- Process_Trace_Entry --
      -------------------------

      procedure Process_Trace_Entry
        (Trace_File : Trace_File_Type;
         SO         : Unbounded_String;
         E          : Trace_Entry)
      is
         pragma Unreferenced (Trace_File);
         pragma Unreferenced (SO);
      begin
         Dump_Entry (E);
      end Process_Trace_Entry;

      Trace_File : Trace_File_Type;
      Result     : Read_Result;

   begin
      Read_Trace_File (Filename, Trace_File, Result);
      Free (Trace_File);
      Success_Or_Fatal_Error (Filename, Result);
   end Dump_Trace_File;

   ---------------------
   -- Dump_Trace_File --
   ---------------------

   procedure Dump_Trace_File (Filename : String) is
   begin
      Dump_Trace_File (Filename, Raw => False);
   end Dump_Trace_File;

   -------------------------
   -- Dump_Raw_Trace_File --
   -------------------------

   procedure Dump_Raw_Trace_File (Filename : String) is
   begin
      Dump_Trace_File (Filename, Raw => True);
   end Dump_Raw_Trace_File;

   -----------------------
   -- Make_Trace_Header --
   -----------------------

   function Make_Trace_Header (Kind : Trace_Kind) return Trace_Header is
   begin
      return Trace_Header'
        (Magic            => Qemu_Trace_Magic,
         Version          => Qemu_Trace_Version,
         Kind             => Kind,
         Sizeof_Target_Pc => Pc_Type_Size,
         Big_Endian       => Big_Endian_Host,
         Machine_Hi       => Unsigned_8 (Shift_Right (ELF_Machine, 8)),
         Machine_Lo       => Unsigned_8 (ELF_Machine and 16#Ff#),
         Padding          => 0);
   end Make_Trace_Header;

   ---------------------------
   -- Write_Trace_File_Info --
   ---------------------------

   procedure Write_Trace_File_Info
     (Desc       : Output_Trace_File;
      Trace_File : Trace_File_Type)
   is
      Hdr     : constant Trace_Header := Make_Trace_Header (Info);
      Tr_Info : Trace_File_Info_Acc;
      Ihdr    : Trace_Info_Header;
   begin
      if not Write (Desc, Hdr'Address, Trace_Header_Size) then
         raise Write_Error with "failed to write first header";
      end if;

      Tr_Info := Trace_File.First_Infos;
      while Tr_Info /= null loop
         declare
            Pad : constant String :=
                    (1 .. (-Tr_Info.Raw_Length) mod Trace_Info_Alignment =>
                       ASCII.NUL);
         begin
            Ihdr.Info_Kind := Info_Kind_Type'Pos (Tr_Info.Kind);
            Ihdr.Info_Length := Unsigned_32 (Tr_Info.Raw_Length);

            if not Write (Desc, Ihdr'Address, Trace_Info_Header_Size) then
               raise Write_Error with "failed to write info header";
            end if;

            if not Write (Desc, Tr_Info.Data'Address, Tr_Info.Raw_Length) then
               raise Write_Error with "failed to write info data";
            end if;

            if Pad'Length /= 0
              and then not Write (Desc, Pad'Address, Pad'Length)
            then
               raise Write_Error with "failed to write info pad";
            end if;
         end;

         Tr_Info := Tr_Info.Next;
      end loop;

      --  Write the terminator

      Ihdr.Info_Kind    := Info_Kind_Type'Pos (Info_End);
      Ihdr.Info_Length := 0;
      if not Write (Desc, Ihdr'Address, Trace_Info_Header_Size) then
         raise Write_Error with "failed to write info header";
      end if;
   end Write_Trace_File_Info;

   -----------------------------
   -- Write_Trace_File_Traces --
   -----------------------------

   procedure Write_Trace_File_Traces
     (Desc : Output_Trace_File;
      Kind : Trace_Kind;
      Base : Traces_Base)
   is
      pragma Assert (Kind /= Info);
      Hdr : constant Trace_Header := Make_Trace_Header (Kind);

      E        : Trace_Entry;
      Ent      : Qemu_Trace_Entry;
      Addr     : System.Address;
      Res_Size : Natural;
      Cur      : Entry_Iterator;
   begin
      if not Write (Desc, Hdr'Address, Trace_Header_Size) then
         raise Write_Error with "failed to write header";
      end if;

      Addr := Ent'Address;
      Res_Size := Trace_Entry_Size;

      Init (Base, Cur, 0);
      Get_Next_Trace (E, Cur);
      while E /= Bad_Trace loop

         Ent := (Pc     => E.First,
                 Size   => Unsigned_16 (E.Last - E.First + 1),
                 Op     => E.Op,
                 others => <>);

         if not Write (Desc, Addr, Res_Size) then
            raise Write_Error with "failed to write entry";
         end if;

         Get_Next_Trace (E, Cur);
      end loop;
   end Write_Trace_File_Traces;

   ----------------------
   -- Write_Trace_File --
   ----------------------

   procedure Write_Trace_File
     (Trace_File : Trace_File_Type;
      Base       : Traces_Base)
   is
      Filename : constant String := Traces_Files.Filename (Trace_File);
      Desc     : Output_Trace_File := Open_File (Filename, Create_File);
   begin
      if Trace_File.First_Infos /= null
        or else Trace_File.Header.Kind = Info
      then
         Write_Trace_File_Info (Desc, Trace_File);
      end if;

      --  Nothing else to do if we only dump infos

      if Trace_File.Header.Kind /= Info then
         Write_Trace_File_Traces (Desc, Trace_File.Header.Kind, Base);
      end if;

      Close_Trace_File (Desc);
   exception
      when others =>
         Close_Trace_File (Desc);
         raise;
   end Write_Trace_File;

   ----------------------
   -- Write_Trace_File --
   ----------------------

   procedure Write_Trace_File (Trace_File : Trace_File_Type)
   is
      Filename : constant String := Traces_Files.Filename (Trace_File);
      Desc     : Output_Trace_File := Open_File (Filename, Create_File);
   begin
      Write_Trace_File_Info (Desc, Trace_File);
      Close_Trace_File (Desc);
   exception
      when others =>
         Close_Trace_File (Desc);
         raise;
   end Write_Trace_File;

   -----------------
   -- Append_Info --
   -----------------

   procedure Append_Info (File : in out Trace_File_Type;
                          Kind : Info_Kind_Type;
                          Data : String)
   is
      Info : constant Trace_File_Info_Acc :=
        new Trace_File_Info'(Data'Length, null, Kind, Data);
   begin
      if File.Last_Infos = null then
         File.First_Infos := Info;
      else
         File.Last_Infos.Next := Info;
      end if;
      File.Last_Infos := Info;
   end Append_Info;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (File : Trace_File_Type; Kind : Info_Kind_Type)
                     return String
   is
      Info : Trace_File_Info_Acc;
   begin
      Info := File.First_Infos;
      while Info /= null loop
         if Info.Kind = Kind then
            return Info.Data;
         end if;
         Info := Info.Next;
      end loop;
      return "";
   end Get_Info;

   ----------------------
   -- Format_Date_Info --
   ----------------------

   function Format_Date_Info (Raw_String : String) return String
   is
      use Ada.Calendar;
      use Calendar_Utils;

      Date_Info : Trace_Info_Date;
      Date      : Time;

      subtype String_8 is String (1 .. 8);
      function Str_To_Date_Info is new Ada.Unchecked_Conversion
        (String_8, Trace_Info_Date);

   begin
      if Raw_String = "" then
         return "";
      end if;
      Date_Info := Str_To_Date_Info (Raw_String);

      --  Convert Trace_Date_Info (interpreted as UTC) to an Ada.Calendar.Time
      --  value.

      Date :=
        Ada.Calendar.Formatting.Time_Of
          (Year   => Integer (Date_Info.Year),
           Month  => Integer (Date_Info.Month),
           Day    => Integer (Date_Info.Day),
           Hour   => Integer (Date_Info.Hour),
           Minute => Integer (Date_Info.Min),
           Second => Integer (Date_Info.Sec));

      return Image (Date);

   end Format_Date_Info;

   ---------------------
   -- Parse_Date_Info --
   ---------------------

   function Parse_Date_Info (Formatted : String) return String is
      Result : String (1 .. Trace_Info_Date'Size / 8);
      Struct : Trace_Info_Date with Import, Address => Result'Address;
      O      : constant Positive := Formatted'First;
      Year   : String renames Formatted (O + 0  .. O + 3);
      Month  : String renames Formatted (O + 5  .. O + 6);
      Day    : String renames Formatted (O + 8  .. O + 9);
      Hour   : String renames Formatted (O + 11 .. O + 12);
      Minute : String renames Formatted (O + 14 .. O + 15);
      Second : String renames Formatted (O + 17 .. O + 18);

   begin
      Struct.Year  := Unsigned_16'Value (Year);
      Struct.Month := Unsigned_8'Value (Month);
      Struct.Day   := Unsigned_8'Value (Day);
      Struct.Hour  := Unsigned_8'Value (Hour);
      Struct.Min   := Unsigned_8'Value (Minute);
      Struct.Sec   := Unsigned_8'Value (Second);
      Struct.Pad   := 0;
      return Result;
   end Parse_Date_Info;

   -------------------
   -- Get_Signature --
   -------------------

   function Get_Signature
     (File : Trace_File_Type)
      return Binary_File_Signature
   is
      Result : Binary_File_Signature;

      Size  : constant String := Get_Info (File, Exec_File_Size);
      TS    : constant String := Get_Info (File, Exec_File_Time_Stamp);
      CRC32 : constant String := Get_Info (File, Exec_File_CRC32);
   begin
      if Size'Length > 0 then
         Result.Size := Long_Integer'Value (Size);
      end if;
      if TS'Length > 0 then
         Result.Time_Stamp := Time_Stamp_Value (TS);
      end if;
      if CRC32'Length > 0 then
         Result.CRC32 := Unsigned_32'Value (CRC32);
      end if;
      return Result;
   end Get_Signature;

   ----------
   -- Free --
   ----------

   procedure Free (Trace_File : in out Trace_File_Type)
   is
      procedure Unchecked_Deallocation is new
        Ada.Unchecked_Deallocation (Trace_File_Info, Trace_File_Info_Acc);
      Info, N_Info : Trace_File_Info_Acc;
   begin
      Info := Trace_File.First_Infos;
      while Info /= null loop
         N_Info := Info.Next;
         Unchecked_Deallocation (Info);
         Info := N_Info;
      end loop;
   end Free;

   -----------------------
   -- Create_Trace_File --
   -----------------------

   procedure Create_Trace_File
     (Filename   : String;
      Kind       : Trace_Kind;
      Trace_File : out Trace_File_Type)
   is
   begin
      Trace_File := Trace_File_Type'
        (Filename    => +Filename,
         Header      => (Kind, Pc_Type_Size, Big_Endian_Host, 0),
         First_Infos => null,
         Last_Infos  => null);
   end Create_Trace_File;

   ------------------
   -- Create_Error --
   ------------------

   procedure Create_Error (Result : out Read_Result; Error : String) is
   begin
      Result := (Kind => Other_Error, Error => +Error);
   end Create_Error;

   procedure Create_Open_Error (Result : out Read_Result; Filename : String) is
   begin
      Result := (Kind => Open_Error, Error => +("cannot open " & Filename));
   end Create_Open_Error;
end Traces_Files;
