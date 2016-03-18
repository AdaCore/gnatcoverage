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
with Ada.Exceptions;          use Ada.Exceptions;
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
      Qemu_Traces_Entries.Trace_Entry'Size / System.Storage_Unit;

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

   ----------------------
   -- Read_Trace_Entry --
   ----------------------

   procedure Read_Trace_Entry
     (Desc       : Trace_File_Descriptor;
      Eof        : out Boolean;
      E          : out Trace_Entry)
   is
      Ent : Qemu_Traces_Entries.Trace_Entry;
      Res : Integer;
   begin
      if Desc.Sizeof_Target_Pc /= Pc_Type_Size then
         raise Bad_File_Format with
            "only" & Unsigned_8'Image (Pc_Type_Size)
            & " bytes pc are handled";
      end if;

      --  Read an entry

      Res := Read (Desc.Fd, Ent'Address, Trace_Entry_Size);

      --  Check result

      if Res = 0 then
         Eof := True;
         return;
      else
         Eof := False;
      end if;

      if Res /= Trace_Entry_Size then
         Close (Desc.Fd);
         raise Bad_File_Format with "file truncated";
      end if;

      --  Basic checks

      if Desc.Big_Endian /= Big_Endian_Host then
         Qemu_Traces.Swap_Pc (Ent.Pc);
         Swaps.Swap_16 (Ent.Size);
      end if;

      E := Trace_Entry'(First  => Pc_Type (Ent.Pc),
                        Last   => Pc_Type (Ent.Pc) + Pc_Type (Ent.Size) - 1,
                        Op     => Ent.Op,
                        State  => Unknown);
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

   -------------------------------
   -- Read_Loadaddr_Trace_Entry --
   -------------------------------

   procedure Read_Loadaddr_Trace_Entry
     (Desc       : Trace_File_Descriptor;
      Trace_File : Trace_File_Type;
      Offset     : out Pc_Type)
   is
      Eof : Boolean;
      E : Trace_Entry;
   begin
      if Get_Info (Trace_File, Qemu_Traces.Kernel_File_Name)'Length > 0 then
         --  Has a kernel and therefore a load address.

         --  Go to the loadaddr entry
         loop
            Read_Trace_Entry (Desc, Eof, E);
            if Eof then
               raise Bad_File_Format with "No 'loadaddr' special trace entry";
            end if;

            if E.Op = Qemu_Traces.Trace_Op_Special then
               if E.First = 0 then
                  raise Bad_File_Format
                    with "Invalid 'loadaddr' special trace entry";
               else
                  Offset := E.First;
               end if;
               return;
            end if;
         end loop;
      else
         Offset := 0;
      end if;
   end Read_Loadaddr_Trace_Entry;

   ----------------------
   -- Close_Trace_File --
   ----------------------

   procedure Close_Trace_File
     (Desc : in out Trace_File_Descriptor)
   is
   begin
      Close (Desc.Fd);
   end Close_Trace_File;

   ---------------------
   -- Read_Trace_File --
   ---------------------

   procedure Read_Trace_File
     (Filename   : String;
      Trace_File : out Trace_File_Type;
      Info_Cb    : access procedure (File : Trace_File_Type);
      Trace_Cb   : not null access procedure (E : Trace_Entry))
   is
      Desc : Trace_File_Descriptor;
      E : Trace_Entry;
      Eof : Boolean;
   begin
      --  Open file
      Open_Trace_File (Filename, Desc, Trace_File);

      if Info_Cb /= null then
         Info_Cb.all (Trace_File);
      end if;

      loop
         Read_Trace_Entry (Desc, Eof, E);
         exit when Eof;

         --  Supply entry to caller

         Trace_Cb.all (E);
      end loop;

      Close_Trace_File (Desc);
   end Read_Trace_File;

   ---------------------
   -- Read_Trace_File --
   ---------------------

   procedure Read_Trace_File
     (Filename   : String;
      Trace_File : out Trace_File_Type;
      Base       : in out Traces_Base)
   is
      Desc   : Trace_File_Descriptor;
      E      : Trace_Entry;
      Eof    : Boolean;
      Offset : Pc_Type;
   begin
      Open_Trace_File (Filename, Desc, Trace_File);

      Read_Loadaddr_Trace_Entry (Desc, Trace_File, Offset);

      loop
         Read_Trace_Entry (Desc, Eof, E);
         exit when Eof;

         if E.Op = Trace_Op_Special then
            raise Bad_File_Format with "Unexpected special trace entry";
         elsif E.First >= Offset then
            Add_Entry (Base,
                       First => E.First - Offset, Last => E.Last - Offset,
                       Op => E.Op);
         end if;
      end loop;

      Close_Trace_File (Desc);
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
      Trace_File : Trace_File_Type;
      Desc : Trace_File_Descriptor;
      E : Trace_Entry;
      Eof : Boolean;
      Offset : Pc_Type;
   begin
      --  Open file
      Open_Trace_File (Filename, Desc, Trace_File);

      Dump_Infos (Trace_File);

      if Raw then
         Offset := 0;
      else
         Read_Loadaddr_Trace_Entry (Desc, Trace_File, Offset);
      end if;

      loop
         Read_Trace_Entry (Desc, Eof, E);
         exit when Eof;

         if Raw or else E.First >= Offset then
            E.First := E.First - Offset;
            E.Last := E.Last - Offset;
            Dump_Entry (E);
         end if;
      end loop;

      Close_Trace_File (Desc);
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
      Ent      : Qemu_Traces_Entries.Trace_Entry;
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
