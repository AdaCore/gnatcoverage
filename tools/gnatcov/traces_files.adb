------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System;

with Hex_Images; use Hex_Images;
with Inputs;     use Inputs;
with Outputs;    use Outputs;
with Qemu_Traces_Entries;
with Swaps;
with Traces;     use Traces;

package body Traces_Files is

   type File_Open_Mode is (Read_Only, Read_Write);

   subtype Qemu_Trace_Entry is Qemu_Traces_Entries.Trace_Entry;

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

   procedure Read_Trace_File_Entries (Desc : Trace_File_Descriptor);
   --  Read all trace file info entries from Desc and call Process_Info for
   --  each of them. Close Desc and raise a Bad_File_Format if one entry is
   --  invalid.

   procedure Append_Info_Entries_From_Descriptor
     (Desc       : Trace_File_Descriptor;
      Trace_File : in out Trace_File_Type);
   --  Calls Read_Trace_File_Entries to add all file info entries to Trace_File

   procedure Read_Trace_Entry
     (Desc       : Trace_File_Descriptor;
      Eof        : out Boolean;
      E          : out Qemu_Trace_Entry);
   --  Read a trace from DESC. Set EOF to True in case of end-of-file (in
   --  this case E isn't set), otherwise EOF is set to False and E is
   --  valid. In case of failure, an exception is raised and the file is
   --  closed.

   procedure Dump_Infos (Trace_File : Trace_File_Type);
   procedure Write_Trace_File_Info (Fd : File_Descriptor;
                                    Trace_File : Trace_File_Type);
   procedure Write_Trace_File_Traces
     (Fd : File_Descriptor; Kind : Trace_Kind; Base : Traces_Base);
   --  Need comments???

   procedure Dump_Trace_File (Filename : String; Raw : Boolean);
   --  Dump a trace file, offsetting addresses unless Raw is true

   function Make_Trace_Header (Kind : Trace_Kind) return Trace_Header;
   --  Create a trace file header with the given kind

   Trace_Header_Size : constant Natural :=
                         Trace_Header'Size / System.Storage_Unit;

   Trace_Info_Header_Size : constant Natural :=
                              Trace_Info_Header'Size / System.Storage_Unit;

   Trace_Entry_Size : constant Natural :=
      Qemu_Trace_Entry'Size / System.Storage_Unit;

   Truncated_File : exception;

   procedure Check_Header (Desc : in out Trace_File_Descriptor;
                           Hdr  : out Trace_Header);
   --  Read and perform basic validation on Hdr: make sure it has the proper
   --  magic header, format version and that the endianity is valid. Raise a
   --  Bad_File_Format if something is invalid. Update the endianity
   --  information in Desc otherwise.

   procedure Check_Trace_File_Headers
     (Desc             : in out Trace_File_Descriptor;
      Trace_File       : in out Trace_File_Type;
      Check_2nd_Header : Boolean);
   --  Perform validation of trace file headers:
   --    - a first header whose kind is Info
   --    - trace info entries
   --    - an optional second header whose kind is anything but Info

   procedure Decode_Trace_Header (Hdr : Trace_Header;
                                  Trace_File : in out Trace_File_Type;
                                  Desc : in out Trace_File_Descriptor);

   function Open_File (Filename : String;
                       Mode     : File_Open_Mode) return File_Descriptor;
   --  Open a file, without reading or writing to it. In case of failure, an
   --  exception is raised and the file is considered as not open.

   procedure Read_SO_Info
     (Desc      : Trace_File_Descriptor;
      Filename  : out String_Access;
      Signature : out Binary_File_Signature;
      Code_Size : out Traces.Pc_Type);
   --  Read the trace info entries related to a shared object load event. This
   --  raises a Bad_File_Format exception if any information is missing.

   function Decode_Trace_Entry
     (E      : Qemu_Trace_Entry;
      Offset : Pc_Type := 0)
      return Trace_Entry;
   --  Turn a raw trace entry from a trace file into a high-level trace entry
   --  that fits our internal data structures. Offset is used to relocate the
   --  entry. It is 0 for statically linked code and non-null for dynamically
   --  linked code.

   ------------------
   -- Check_Header --
   ------------------

   procedure Check_Header
     (Desc : in out Trace_File_Descriptor;
      Hdr  : out Trace_Header)
   is
   begin
      if Read (Desc.Fd, Hdr'Address, Trace_Header_Size)
        /= Trace_Header_Size
      then
         raise Truncated_File with "cannot read header";
      end if;

      if Hdr.Magic /= Qemu_Trace_Magic then
         raise Bad_File_Format with "invalid header (bad magic)";
      end if;

      if Hdr.Version /= Qemu_Trace_Version then
         raise Bad_File_Format with "invalid header (bad version)";
      end if;

      if not Hdr.Big_Endian'Valid then
         raise Bad_File_Format with "invalid header (bad endianness)";
      end if;

      Desc.Big_Endian := Hdr.Big_Endian;
   end Check_Header;

   ------------------------------
   -- Check_Trace_File_Headers --
   ------------------------------

   procedure Check_Trace_File_Headers
     (Desc             : in out Trace_File_Descriptor;
      Trace_File       : in out Trace_File_Type;
      Check_2nd_Header : Boolean)
   is
      Hdr : Trace_Header;
   begin
      --  We expect the following:
      --    (1) a first header whose kind is Info;
      --    (2) trace info entries;
      --    (3) optional second header whose kind is anything but Info.
      --  We raise a Bad_File_Format for anything else.

      --  (1) Read the first header

      Check_Header (Desc, Hdr);

      if Hdr.Kind /= Info then
         raise Bad_File_Format with
            "first header must describe an information section";
      end if;

      --  (2) Read trace info entries

      Append_Info_Entries_From_Descriptor (Desc, Trace_File);

      if Check_2nd_Header then

         --  (3) Read the second header

         Check_Header (Desc, Hdr);

         if Hdr.Kind = Info then
            raise Bad_File_Format with
              "second header must not describe an information section";
         end if;

         Decode_Trace_Header (Hdr, Trace_File, Desc);
      end if;
   end Check_Trace_File_Headers;

   -------------------------
   -- Decode_Trace_Header --
   -------------------------

   procedure Decode_Trace_Header (Hdr : Trace_Header;
                                  Trace_File : in out Trace_File_Type;
                                  Desc : in out Trace_File_Descriptor)
   is
   begin
      if not Hdr.Kind'Valid then
         raise Bad_File_Format with "invalid header (bad kind)";
      end if;
      Desc.Kind := Hdr.Kind;

      Desc.Sizeof_Target_Pc := Hdr.Sizeof_Target_Pc;
      if Desc.Sizeof_Target_Pc /= 4 and then Desc.Sizeof_Target_Pc /= 8 then
         raise Bad_File_Format with "invalid header (bad pc size)";
      end if;

      Trace_File.Machine := Unsigned_16 (Hdr.Machine_Hi) * 256
        + Unsigned_16 (Hdr.Machine_Lo);

      if ELF_Machine = 0 or else ELF_Machine = Trace_File.Machine then
         ELF_Machine := Trace_File.Machine;
         Machine := Decode_EM (ELF_Machine);
      else
         raise Bad_File_Format
           with "target machine doesn't match previous one";
      end if;
   end Decode_Trace_Header;

   ---------------
   -- Open_File --
   ---------------

   function Open_File (Filename : String;
                       Mode     : File_Open_Mode)
                       return File_Descriptor
   is
      Fd : File_Descriptor;
   begin
      Log_File_Open (Filename);
      case Mode is
         when Read_Only =>
            Fd := Open_Read (Filename, Binary);
         when Read_Write =>
            Fd := Open_Read_Write (Filename, Binary);
      end case;

      if Fd = Invalid_FD then
         raise Bad_File_Format with "cannot open file " & Filename;
      end if;
      return Fd;
   end Open_File;

   ------------------
   -- Read_SO_Info --
   ------------------

   procedure Read_SO_Info
     (Desc      : Trace_File_Descriptor;
      Filename  : out String_Access;
      Signature : out Binary_File_Signature;
      Code_Size : out Traces.Pc_Type)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;

      Fname : String_Access;
      Sig   : Binary_File_Signature;
      CS    : Pc_Type := 0;

      procedure Process_Info_Entry
        (Kind : Info_Kind_Type;
         Data : String);
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
            Sig.Time_Stamp := Ada.Strings.Unbounded.To_Unbounded_String (Data);
         when Exec_File_CRC32 => Sig.CRC32 := Unsigned_32'Value (Data);
         when Exec_Code_Size  => CS := Pc_Type'Value (Data);
         when others => null;
         end case;
      end Process_Info_Entry;

   begin
      Read_Info_Entries (Desc);

      if Fname = null
         or else Sig.Size = 0
         or else Sig.Time_Stamp = Ada.Strings.Unbounded.Null_Unbounded_String
         or else Sig.CRC32 = 0
         or else CS = 0
      then
         raise Bad_File_Format with "incomplete shared object load event";
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
      Trace_File : out Trace_File_Type) is
   begin
      Desc.Fd := Open_File (Filename, Read_Only);
      Check_Trace_File_Headers (Desc, Trace_File, Check_2nd_Header => True);
   exception
      when E : others =>
         Close (Desc.Fd);
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

      Flat_Hdr : constant Trace_Header := Make_Trace_Header (Flat);
   begin
      Desc.Fd := Open_File (Filename, Read_Write);

      Check_Trace_File_Headers (Desc,
                                Trace_File,
                                Check_2nd_Header => False);

      --  Write flat (raw) trace header

      if Write (Desc.Fd, Flat_Hdr'Address, Trace_Header_Size) /=
        Trace_Header_Size
      then
         raise Write_Error with "failed to write Flat trace header";
      end if;

      --  Set traces info in the Trace_File_Descriptor

      Desc.Kind := Flat_Hdr.Kind;
      Desc.Sizeof_Target_Pc := Flat_Hdr.Sizeof_Target_Pc;
      Desc.Big_Endian := Flat_Hdr.Big_Endian;

   exception
      when E : others =>
         Close (Desc.Fd);
         Fatal_Error
           ("processing of trace file " & Filename
            & " failed: " & Exception_Message (E)
            & (if Exception_Identity (E) = Truncated_File'Identity
               then " (truncated file?)"
               else ""));
   end Open_Output_Flat_Trace_File;

   ----------------------------
   -- Open_Decision_Map_File --
   ----------------------------

   procedure Open_Decision_Map_File
     (Filename   : String;
      Desc       : out Trace_File_Descriptor)
   is
      Hdr : Trace_Header;
   begin

      Desc.Fd := Open_File (Filename, Read_Only);

      --  Read the first header

      Check_Header (Desc, Hdr);
      if Hdr.Kind /= Decision_Map then
         raise Bad_File_Format with
           "first header must describe an history section, is " & Hdr.Kind'Img;
      end if;

      Desc.Kind := Hdr.Kind;
      Desc.Sizeof_Target_Pc := Hdr.Sizeof_Target_Pc;
      Desc.Big_Endian := Hdr.Big_Endian;
   end Open_Decision_Map_File;

   -----------------------------
   -- Read_Trace_File_Entries --
   -----------------------------

   procedure Read_Trace_File_Entries (Desc : Trace_File_Descriptor) is
      Ihdr    : Trace_Info_Header;
      Pad     : String (1 .. Trace_Info_Alignment);
      Pad_Len : Natural;
      Kind    : Info_Kind_Type;
   begin
      loop
         --  Read the Trace_Info_Header

         if Read (Desc.Fd, Ihdr'Address, Trace_Info_Header_Size)
               /= Trace_Info_Header_Size
         then
            raise Bad_File_Format with "cannot read info header";
         end if;

         if Desc.Big_Endian /= Big_Endian_Host then
            Swaps.Swap_32 (Ihdr.Info_Kind);
            Swaps.Swap_32 (Ihdr.Info_Length);
         end if;

         declare
            Data : String (1 .. Natural (Ihdr.Info_Length));
         begin
            --  Read the associated data

            if Read (Desc.Fd, Data'Address, Data'Length) /= Data'Length
            then
               raise Bad_File_Format with "cannot read info data";
            end if;

            --  Discard padding. Still check that it's only null bytes.

            Pad_Len := Natural (Ihdr.Info_Length) mod Trace_Info_Alignment;
            if Pad_Len /= 0 then
               Pad_Len := Trace_Info_Alignment - Pad_Len;
               if Read (Desc.Fd, Pad'Address, Pad_Len) /= Pad_Len then
                  raise Bad_File_Format with "cannot read info pad";
               end if;
               if Pad (1 .. Pad_Len) /= (1 .. Pad_Len => Character'Val (0))
               then
                  raise Bad_File_Format with "bad padding content";
               end if;
            end if;

            begin
               Kind := Info_Kind_Type'Val (Ihdr.Info_Kind);
            exception
               when Constraint_Error =>
                  raise Bad_File_Format with
                    ("unknown trace info kind: 0x"
                     & Hex_Image (Ihdr.Info_Kind));
            end;

            --  If it's an "end" trace info entry, just stop

            if Kind = Info_End then
               if Data'Length /= 0 then
                  raise Bad_File_Format with "bad end info length";
               end if;
               exit;
            else
               Process_Info (Kind, Data);
            end if;
         end;
      end loop;
   end Read_Trace_File_Entries;

   -----------------------------------------
   -- Append_Info_Entries_From_Descriptor --
   -----------------------------------------

   procedure Append_Info_Entries_From_Descriptor
     (Desc       : Trace_File_Descriptor;
      Trace_File : in out Trace_File_Type)
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
      Read_Info_Entries (Desc);
   end Append_Info_Entries_From_Descriptor;

   ---------------------
   -- Read_Trace_File --
   ---------------------

   procedure Read_Trace_File_Gen
     (Filename   : String;
      Trace_File : out Trace_File_Type)
   is
      Desc   : Trace_File_Descriptor;
      F      : Trace_File_Type;
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
      Raw_Entry : Qemu_Trace_Entry;
   begin
      Open_Trace_File (Filename, Desc, F);
      Process_Info_Entries (F);

      --  Look for a Loadaddr special trace entry, if expected

      if Get_Info (F, Qemu_Traces.Kernel_File_Name)'Length /= 0 then

         --  If execution reaches this point, we know there is a kernel and
         --  therefore a load address. As a consequence, we also know that the
         --  rest of this trace file is only mere trace entries until one that
         --  must be a "loadaddr" special entry.

         loop
            Read_Trace_Entry (Desc, EOF, Raw_Entry);
            if EOF then
               raise Bad_File_Format with "No 'loadaddr' special trace entry";
            end if;

            if Raw_Entry.Op = Qemu_Traces.Trace_Op_Special then
               if Raw_Entry.Size /= Qemu_Traces.Trace_Special_Loadaddr then
                  raise Bad_File_Format with
                    ("'loadaddr' special trace entry expected but got instead"
                     & " a 0x" & Hex_Image (Raw_Entry.Size)
                     & " special entry");
               elsif Raw_Entry.Pc = 0 then
                  raise Bad_File_Format with
                     ("Invalid 'loadaddr' special trace entry: offset must not"
                      & " be 0");
               end if;

               Process_Loadaddr (F, Raw_Entry.Pc);

               if Handle_Relocations then
                  Offset := Raw_Entry.Pc;
               end if;

               exit;
            end if;

            if not Handle_Relocations then
               Process_Trace_Entry
                 (F, No_Shared_Object, Decode_Trace_Entry (Raw_Entry));
            end if;
         end loop;
      end if;

      --  Then process the remaining trace entries

      loop
         Read_Trace_Entry (Desc, EOF, Raw_Entry);
         exit when EOF;

         if Raw_Entry.Op = Qemu_Traces.Trace_Op_Special then
            case Raw_Entry.Size is
            when Trace_Special_Loadaddr =>

               --  There can be only one loadaddr special trace entry per trace
               --  file.  If it exists, we already processed it before the
               --  loop, above.

               Fatal_Error
                 ("Unexpected 'loadaddr' special trace entry.");

            when Trace_Special_Load_Shared_Object =>
               declare
                  Filename  : String_Access;
                  Sig       : Binary_File_Signature;
                  Code_Size : Pc_Type;

                  First, Last : Pc_Type;
               begin
                  Read_SO_Info (Desc, Filename, Sig, Code_Size);
                  First := Raw_Entry.Pc;
                  Last := First + Code_Size - 1;
                  SO_Set.Insert
                    ((First => First,
                      Last  => Last,
                      SO    => Load_Shared_Object
                                 (F, Filename.all, Sig, First, Last)));
                  Free (Filename);
               end;

            when Trace_Special_Unload_Shared_Object =>
               SO_Set.Delete (SOD_For_PC (Raw_Entry.Pc));

            when others =>
               Fatal_Error
                 ("Unknown special trace entry: 0x"
                  & Hex_Image (Raw_Entry.Size));
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
              (F, SO, Decode_Trace_Entry (Raw_Entry, Trace_Offset));
         end;

         << Skip >> null;
      end loop;

      Close_Trace_File (Desc);
      Trace_File := F;
   end Read_Trace_File_Gen;

   ----------------------
   -- Read_Trace_Entry --
   ----------------------

   procedure Read_Trace_Entry
     (Desc       : Trace_File_Descriptor;
      Eof        : out Boolean;
      E          : out Qemu_Trace_Entry)
   is
      Res : Integer;
   begin
      if Desc.Sizeof_Target_Pc /= Pc_Type_Size then
         raise Bad_File_Format with
            "only" & Unsigned_8'Image (Pc_Type_Size)
            & " bytes pc are handled";
      end if;

      --  Read an entry, making sure the read operation could get a whole one

      Res := Read (Desc.Fd, E'Address, Trace_Entry_Size);
      if Res = 0 then
         Eof := True;
         return;
      elsif Res /= Trace_Entry_Size then
         Close (Desc.Fd);
         raise Bad_File_Format with "file truncated";
      end if;

      Eof := False;

      if Desc.Big_Endian /= Big_Endian_Host then
         Qemu_Traces.Swap_Pc (E.Pc);
         Swaps.Swap_16 (E.Size);
      end if;
   end Read_Trace_Entry;

   -----------------------
   -- Write_Trace_Entry --
   -----------------------

   procedure Write_Trace_Entry
     (Desc       : Trace_File_Descriptor;
      E          : Trace_Entry)
   is
      Ent : Qemu_Traces_Entries.Trace_Entry;
      Res : Integer;
   begin

      if Desc.Sizeof_Target_Pc /= Pc_Type_Size then
         raise Bad_File_Format with
            "only" & Unsigned_8'Image (Pc_Type_Size)
            & " bytes pc are handled";
      end if;

      Ent.Pc   := E.First;
      Ent.Size := Unsigned_16 (E.Last - E.First + 1);
      Ent.Op   := E.Op;

      if Desc.Big_Endian /= Big_Endian_Host then
         Qemu_Traces.Swap_Pc (Ent.Pc);
         Swaps.Swap_16 (Ent.Size);
      end if;

      --  Write an entry

      Res := Write (Desc.Fd, Ent'Address, Trace_Entry_Size);

      --  Check result

      if Res /= Trace_Entry_Size then
         raise Bad_File_Format with "file truncated";
      end if;

   end Write_Trace_Entry;

   ----------------------
   -- Close_Trace_File --
   ----------------------

   procedure Close_Trace_File (Desc : in out Trace_File_Descriptor)
   is
   begin
      Close (Desc.Fd);
      Desc.Fd := Invalid_FD;
   end Close_Trace_File;

   ---------------------
   -- Read_Trace_File --
   ---------------------

   procedure Read_Trace_File
     (Filename   : String;
      Trace_File : out Trace_File_Type;
      Base       : in out Traces_Base)
   is
      function Load_Shared_Object
         (Trace_File  : Trace_File_Type;
          Filename    : String;
          Signature   : Binary_File_Signature;
          First, Last : Traces.Pc_Type) return Boolean
      is (True);

      procedure Process_Trace_Entry
        (Trace_File : Trace_File_Type;
         SO         : Boolean;
         E          : Trace_Entry);

      procedure Read_Trace_File is new Read_Trace_File_Gen
        (Shared_Object_Type  => Boolean,
         No_Shared_Object    => False,
         Load_Shared_Object  => Load_Shared_Object,
         Process_Trace_Entry => Process_Trace_Entry);

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
      Read_Trace_File (Filename, Trace_File);
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
      use Ada.Strings.Unbounded;

      procedure Process_Info_Entries (Trace_File : Trace_File_Type);

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
        (Shared_Object_Type  => Unbounded_String,
         No_Shared_Object    => Null_Unbounded_String,
         Process_Info_Entries => Process_Info_Entries,
         Process_Loadaddr    => Process_Loadaddr,
         Load_Shared_Object  => Load_Shared_Object,
         Process_Trace_Entry => Process_Trace_Entry,
         Handle_Relocations  => not Raw);

      --------------------------
      -- Process_Info_Entries --
      --------------------------

      procedure Process_Info_Entries (Trace_File : Trace_File_Type) is
      begin
         Dump_Infos (Trace_File);
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
         return To_Unbounded_String (Filename);
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

   begin
      Read_Trace_File (Filename, Trace_File);
      Free (Trace_File);
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
     (Fd         : File_Descriptor;
      Trace_File : Trace_File_Type)
   is
      Hdr     : constant Trace_Header := Make_Trace_Header (Info);
      Tr_Info : Trace_File_Info_Acc;
      Ihdr    : Trace_Info_Header;
   begin
      if Write (Fd, Hdr'Address, Trace_Header_Size) /= Trace_Header_Size then
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

            if Write (Fd, Ihdr'Address, Trace_Info_Header_Size)
              /= Trace_Info_Header_Size
            then
               raise Write_Error with "failed to write info header";
            end if;

            if Write (Fd, Tr_Info.Data'Address, Tr_Info.Raw_Length)
              /= Tr_Info.Raw_Length
            then
               raise Write_Error with "failed to write info data";
            end if;

            if Pad'Length /= 0
              and then Write (Fd, Pad'Address, Pad'Length) /= Pad'Length
            then
               raise Write_Error with "failed to write info pad";
            end if;
         end;

         Tr_Info := Tr_Info.Next;
      end loop;

      --  Write the terminator

      Ihdr.Info_Kind    := Info_Kind_Type'Pos (Info_End);
      Ihdr.Info_Length := 0;
      if Write (Fd, Ihdr'Address, Trace_Info_Header_Size)
        /= Trace_Info_Header_Size
      then
         raise Write_Error with "failed to write info header";
      end if;
   end Write_Trace_File_Info;

   -----------------------------
   -- Write_Trace_File_Traces --
   -----------------------------

   procedure Write_Trace_File_Traces
     (Fd   : File_Descriptor;
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
      if Write (Fd, Hdr'Address, Trace_Header_Size) /= Trace_Header_Size then
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

         if Write (Fd, Addr, Res_Size) /= Res_Size then
            raise Write_Error with "failed to write entry";
         end if;

         Get_Next_Trace (E, Cur);
      end loop;
   end Write_Trace_File_Traces;

   ----------------------
   -- Write_Trace_File --
   ----------------------

   procedure Write_Trace_File
     (Filename   : String;
      Trace_File : Trace_File_Type;
      Base       : Traces_Base)
   is
      Fd : File_Descriptor;
   begin
      Fd := Create_File (Filename, Binary);
      if Fd = Invalid_FD then
         raise Write_Error with "failed to create the file";
      end if;

      if Trace_File.First_Infos /= null
        or else Trace_File.Kind = Info
      then
         Write_Trace_File_Info (Fd, Trace_File);
      end if;

      --  Stop now if we only dump infos

      if Trace_File.Kind = Info then
         return;
      end if;

      Write_Trace_File_Traces (Fd, Trace_File.Kind, Base);

      Close (Fd);
   exception
      when others =>
         Close (Fd);
         raise;
   end Write_Trace_File;

   ----------------------
   -- Write_Trace_File --
   ----------------------

   procedure Write_Trace_File (Filename : String;
                               Trace_File : Trace_File_Type)
   is
      Fd : File_Descriptor;
   begin
      pragma Assert (Trace_File.Kind = Info);
      Fd := Create_File (Filename, Binary);
      if Fd = Invalid_FD then
         raise Write_Error with "failed to create the file";
      end if;

      Write_Trace_File_Info (Fd, Trace_File);

      Close (Fd);
   exception
      when others =>
         Close (Fd);
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

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save
     (S          : access Root_Stream_Type'Class;
      Trace_File : Trace_File_Type)
   is
      Info : Trace_File_Info_Acc := Trace_File.First_Infos;

   begin
      --  Note: we identify the last info record using a null string, so
      --  omit any empty info record when saving.

      while Info /= null loop
         pragma Assert (Info.Kind /= Info_End);
         Info_Kind_Type'Write (S, Info.Kind);
         String'Output (S, Info.Data);
         Info := Info.Next;
      end loop;

      Info_Kind_Type'Write (S, Info_End);
   end Checkpoint_Save;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load
     (S          : access Root_Stream_Type'Class;
      CS         : access Checkpoints.Checkpoint_State;
      Trace_File : in out Trace_File_Type)
   is
      pragma Unreferenced (CS);

      Kind : Info_Kind_Type;
   begin
      loop
         Info_Kind_Type'Read (S, Kind);
         exit when Kind = Info_End;
         Append_Info (Trace_File, Kind, String'Input (S));
      end loop;
   end Checkpoint_Load;

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
      Date_Info  : Trace_Info_Date;
      subtype String_8 is String (1 .. 8);
      function Str_To_Date_Info is new Ada.Unchecked_Conversion
        (String_8, Trace_Info_Date);

      Res : String (1 .. 19) := "YYYY-MM-DD HH:MM:SS";
      procedure Put_Pad (Num : Natural; S : out String);

      -------------
      -- Put_Pad --
      -------------

      procedure Put_Pad (Num : Natural; S : out String)
      is
         V : Natural := Num;
      begin
         for I in reverse S'Range loop
            S (I) := Character'Val ((V rem 10)
                                    + Character'Pos ('0'));
            V := V / 10;
         end loop;
      end Put_Pad;
   begin
      if Raw_String = "" then
         return "";
      end if;
      Date_Info := Str_To_Date_Info (Raw_String);
      Put_Pad (Natural (Date_Info.Year), Res (1 .. 4));
      Put_Pad (Natural (Date_Info.Month), Res (6 .. 7));
      Put_Pad (Natural (Date_Info.Day), Res (9 .. 10));
      Put_Pad (Natural (Date_Info.Hour), Res (12 .. 13));
      Put_Pad (Natural (Date_Info.Min), Res (15 .. 16));
      Put_Pad (Natural (Date_Info.Sec), Res (18 .. 19));
      return Res;
   end Format_Date_Info;

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
         Result.Time_Stamp := Ada.Strings.Unbounded.To_Unbounded_String (TS);
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
     (Kind       : Trace_Kind;
      Trace_File : out Trace_File_Type) is
   begin
      Trace_File := Trace_File_Type'(Kind             => Kind,
                                     Sizeof_Target_Pc => Pc_Type_Size,
                                     Big_Endian       => Big_Endian_Host,
                                     Machine          => 0,
                                     First_Infos      => null,
                                     Last_Infos       => null);
   end Create_Trace_File;
end Traces_Files;
