------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces;

with System;
with System.Storage_Elements;

with GNAT.OS_Lib;        use GNAT.OS_Lib;
with GNAT.Byte_Swapping; use GNAT.Byte_Swapping;

with GNATcov_RTS.Buffers;
with GNATcov_RTS.Traces; use GNATcov_RTS.Traces;

with Hex_Images;
with Strings;
with Outputs;

package body Instrument.Input_Traces is

   use GNATCOLL.Projects;

   Unit_Part_Map : constant array (Supported_Unit_Part) of Unit_Parts :=
     (GNATcov_RTS.Traces.Unit_Body     => GNATCOLL.Projects.Unit_Body,
      GNATcov_RTS.Traces.Unit_Spec     => GNATCOLL.Projects.Unit_Spec,
      GNATcov_RTS.Traces.Unit_Separate => GNATCOLL.Projects.Unit_Separate);

   Native_Endianity : constant Supported_Endianity :=
      GNATcov_RTS.Traces.Native_Endianity;

   subtype Read_Result is Traces_Files.Read_Result;
   procedure Create_Error (Result : out Read_Result; Error : String)
      renames Traces_Files.Create_Error;

   function Buffer_Size
     (Encoding  : Supported_Bit_Buffer_Encoding;
      Bit_Count : Any_Bit_Count) return Natural;
   --  Return the size (in bytes) of the representation of a coverage buffer in
   --  a source trace file given an encoding and a bit count.

   function With_Padding
     (Alignment : Supported_Alignment;
      Size      : Natural) return Natural;
   --  Return Size plus the number of padding bytes required so that the result
   --  is a multiple of Alignment.

   type Bytes_Array is array (Positive range <>) of Interfaces.Unsigned_8;

   type Bytes_Access is access all Bytes_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Bytes_Array, Bytes_Access);

   type Buffer_Range is record
      Offset, Size : Natural;
      --  Offset and size of the range

      Padded_Size : Natural;
      --  Total size of the buffer range so that Offset + Padded_Size is a
      --  multiple of some alignment.
   end record;
   --  Describe a range of bytes in a buffer

   function Range_For
     (Alignment    : Supported_Alignment;
      Offset, Size : Natural) return Buffer_Range
      with Pre => Offset = With_Padding (Alignment, Offset);
   --  Return a Buffer_Range object corresponding to the given constraints

   function Offset_After (Self : Buffer_Range) return Natural;
   --  Return the offset for the next byte range that can appear after Self

   type Binary_Stream is record
      File : File_Descriptor;
      --  File from which to read the binary stream

      Offset : Interfaces.Unsigned_32;
      --  Offset for the next bytes to read from the binary stream

      Buffer : Bytes_Access;
      --  Temporary buffer to hold data to read. When not null, first bound is
      --  always 1.

      Buffer_Last : Natural;
      --  Index in Buffer for the last byte that was read
   end record;

   procedure Reserve (Buffer : in out Bytes_Access; Size : Natural);
   --  Reallocate Buffer if needed so that it can contain at least Size bytes

   procedure Read_Bytes
     (Stream       : in out Binary_Stream;
      Size         : Natural;
      EOF          : out Boolean;
      Result       : in out Read_Result;
      Error_If_EOF : Boolean := True)
      with Pre => Result.Success;
   --  Try to read Size bytes from Stream. Put the result in Stream.Buffer and
   --  Stream.Buffer_Last. By default, set EOF to False (see below).
   --
   --  If Stream is at the end of file before the call, then set EOF to True
   --  and:
   --
   --    * if Error_If_EOF is true, put an error in Result;
   --    * otherwise leave Result as it is.
   --
   --  If the end of file is reached in the middle of the read, set Result to
   --  an error about a truncated file and set EOF to True.

   function Buffer_Address (Stream : Binary_Stream) return System.Address
      with Pre => Stream.Buffer /= null;
   --  Return the address for the content of the buffer in Stream

   type Trace_Entry_Elements is record
      Unit_Name        : System.Address;
      Statement_Buffer : System.Address;
      Decision_Buffer  : System.Address;
      MCDC_Buffer      : System.Address;
   end record;

   type Coverage_Buffer_Access is access all Coverage_Buffer;

   procedure Free is new Ada.Unchecked_Deallocation
     (Coverage_Buffer, Coverage_Buffer_Access);

   procedure Reserve
     (Buffer : in out Coverage_Buffer_Access;
      Size   : Any_Bit_Count);
   --  Reallocate Buffer if needed so that it can contain at least Size bits

   procedure Read_Trace_File_Header
     (Stream      : in out Binary_Stream;
      File_Header : out Trace_File_Header;
      Result      : in out Read_Result)
      with Pre => Result.Success;
   --  Read a trace file header from Stream and store it in File_Header. Set
   --  Result to an error if something wrong happened.

   procedure Read_Trace_Info
     (Stream      : in out Binary_Stream;
      File_Header : Trace_File_Header;
      Kind        : out Supported_Info_Kind;
      Data        : out String_Access;
      Result      : in out Read_Result)
      with Pre => Result.Success;
   --  Read a trace info entry from Stream. Return an error if something wrong
   --  happened, otherwise put its kind in Kind and allocate a string in Data
   --  to hold the data associated to this entry.

   function Read_Trace_Entry
     (Stream       : in out Binary_Stream;
      File_Header  : Trace_File_Header;
      Entry_Header : out Trace_Entry_Header;
      Trace_Entry  : out Trace_Entry_Elements;
      Result       : in out Read_Result) return Boolean
      with Pre => Result.Success;
   --  Try to read a trace etnry header from Stream. If Stream already reached
   --  EOF, just return False.
   --
   --  Otherwise, decode it and fill Entry_Header with information from it.
   --  Then fetch the remaining entry data in Stream.Buffer and make
   --  Trace_Entry reference this data.
   --
   --  If all goes well, keep Result as it is, otherwise set it to the
   --  corresponding error information.

   procedure Decode_Buffer
     (Encoding   : Supported_Bit_Buffer_Encoding;
      Raw_Buffer : Bytes_Array;
      Buffer     : out Coverage_Buffer;
      Result     : in out Read_Result)
      with Pre => Result.Success;
   --  Decode the given Raw_Buffer according to the given Encoding, and but the
   --  result in Buffer. If all goes well, keep Result as it is, otherwise set
   --  it to the corresponding error information.

   -----------------
   -- Buffer_Size --
   -----------------

   function Buffer_Size
     (Encoding  : Supported_Bit_Buffer_Encoding;
      Bit_Count : Any_Bit_Count) return Natural is
   begin
      case Encoding is
         when LSB_First_Bytes =>
            return With_Padding (8, Natural (Bit_Count)) / 8;
      end case;
   end Buffer_Size;

   ------------------
   -- With_Padding --
   ------------------

   function With_Padding
     (Alignment : Supported_Alignment;
      Size      : Natural) return Natural
   is
      Result : constant Natural := Size mod Natural (Alignment);
   begin
      return Size + (if Result = 0 then 0 else Natural (Alignment) - Result);
   end With_Padding;

   ---------------
   -- Range_For --
   ---------------

   function Range_For
     (Alignment    : Supported_Alignment;
      Offset, Size : Natural) return Buffer_Range is
   begin
      return (Offset, Size, With_Padding (Alignment, Size));
   end Range_For;

   ------------------
   -- Offset_After --
   ------------------

   function Offset_After (Self : Buffer_Range) return Natural is
   begin
      return Self.Offset + Self.Padded_Size;
   end Offset_After;

   -------------
   -- Reserve --
   -------------

   procedure Reserve (Buffer : in out Bytes_Access; Size : Natural) is
   begin
      if Buffer = null or else Buffer'Length < Size then
         Free (Buffer);
         Buffer := new Bytes_Array (1 .. Size);
      end if;
   end Reserve;

   ----------------
   -- Read_Bytes --
   ----------------

   procedure Read_Bytes
     (Stream       : in out Binary_Stream;
      Size         : Natural;
      EOF          : out Boolean;
      Result       : in out Read_Result;
      Error_If_EOF : Boolean := True) is
   begin
      Reserve (Stream.Buffer, Size);
      EOF := False;

      declare
         use Interfaces;

         Read_Size : Integer;
         Buffer    : Bytes_Array renames Stream.Buffer.all (1 .. Size);
      begin
         Stream.Buffer_Last := 0;
         while Stream.Buffer_Last < Size loop
            Read_Size := Read
              (Stream.File,
               Buffer (Stream.Buffer_Last + 1)'Address,
               Size - Stream.Buffer_Last);

            if Read_Size = 0 then
               EOF := True;
               if Error_If_EOF or else Stream.Buffer_Last /= 0 then
                  Create_Error (Result, "truncated file");
               end if;
               return;
            else
               pragma Assert (Read_Size > 0);
               Stream.Offset := Stream.Offset + Unsigned_32 (Read_Size);
               Stream.Buffer_Last := Stream.Buffer_Last + Read_Size;
            end if;
         end loop;
      end;
   end Read_Bytes;

   --------------------
   -- Buffer_Address --
   --------------------

   function Buffer_Address (Stream : Binary_Stream) return System.Address is
   begin
      return Stream.Buffer.all (1) 'Address;
   end Buffer_Address;

   -------------
   -- Reserve --
   -------------

   procedure Reserve
     (Buffer : in out Coverage_Buffer_Access;
      Size   : Any_Bit_Count) is
   begin
      if Buffer = null or else Size > Buffer.all'Length then
         Free (Buffer);
         Buffer := new Coverage_Buffer (0 .. Any_Bit_Id (Size) - 1);
      end if;
   end Reserve;

   ----------------------------
   -- Read_Trace_File_Header --
   ----------------------------

   procedure Read_Trace_File_Header
     (Stream      : in out Binary_Stream;
      File_Header : out Trace_File_Header;
      Result      : in out Read_Result)
   is
      Ignored_EOF : Boolean;
   begin
      Read_Bytes
        (Stream, File_Header'Size / 8, Ignored_EOF, Result);
      if not Result.Success then
         return;
      end if;

      declare
         use type Interfaces.Unsigned_16;

         Raw_Header : Trace_File_Header
            with Import, Address => Buffer_Address (Stream);
      begin
         if Raw_Header.Magic /= Trace_File_Magic then
            Create_Error (Result, "invalid magic");
            return;

         elsif Raw_Header.Endianity not in Little_Endian | Big_Endian then
            Create_Error (Result, "invalid endianity");
            return;
         end if;

         --  Now that the endianity is known, swap bytes if needed

         if Raw_Header.Endianity /= Native_Endianity then
            Swap4 (Raw_Header.Format_Version'Address);
         end if;

         --  Go on checking fields...

         if Raw_Header.Format_Version /= Current_Version then
            Create_Error (Result, "unsupported format version");
            return;

         elsif Raw_Header.Alignment not in 1 | 2 | 4 | 8 then
            Create_Error (Result, "invalid alignment");
            return;

         elsif Raw_Header.Padding /= 0 then
            Create_Error (Result, "invalid file header padding");
            return;
         end if;

         File_Header := Raw_Header;
      end;
   end Read_Trace_File_Header;

   ---------------------
   -- Read_Trace_Info --
   ---------------------

   procedure Read_Trace_Info
     (Stream      : in out Binary_Stream;
      File_Header : Trace_File_Header;
      Kind        : out Supported_Info_Kind;
      Data        : out String_Access;
      Result      : in out Read_Result)
   is
      Ignored_EOF : Boolean;
   begin
      --  Default initialize OUT arguments to avoid pointless warnings

      Kind := Info_End;
      Data := null;

      --  Read the trace info header

      Read_Bytes
        (Stream, Trace_Info_Header'Size / 8, Ignored_EOF, Result);
      if not Result.Success then
         return;
      end if;

      declare
         use type Interfaces.Unsigned_32;

         Header : Trace_Info_Header
            with Import, Address => Buffer_Address (Stream);
      begin
         --  Swap bytes if needed, according to the file endianity

         if File_Header.Endianity /= Native_Endianity then
            Swap4 (Header.Kind'Address);
            Swap4 (Header.Length'Address);
         end if;

         if Header.Kind not in Supported_Info_Kind then
            Create_Error (Result, "invalid trace info kind");
            return;
         end if;

         Kind := Header.Kind;

         if Header.Kind = Info_End then
            if Header.Length /= 0 then
               Create_Error
                 (Result, "invalid end marker for trace info sequence");
               return;
            end if;
         end if;

         Data := new String (1 .. Natural (Header.Length));
      end;

      --  Read the associated data

      declare
         Read_Size : constant Natural :=
            With_Padding (File_Header.Alignment, Data.all'Length);
      begin
         Read_Bytes (Stream, Read_Size, Ignored_EOF, Result);
      end;
      if not Result.Success then
         Free (Data);
         return;
      end if;
      declare
         Data_As_String : String (Data.all'Range)
            with Import, Address => Buffer_Address (Stream);
      begin
         Data.all := Data_As_String;
      end;
   end Read_Trace_Info;

   ----------------------
   -- Read_Trace_Entry --
   ----------------------

   function Read_Trace_Entry
     (Stream       : in out Binary_Stream;
      File_Header  : Trace_File_Header;
      Entry_Header : out Trace_Entry_Header;
      Trace_Entry  : out Trace_Entry_Elements;
      Result       : in out Read_Result) return Boolean
   is
      EOF : Boolean;
   begin
      --  First read the trace entry header and decode it

      Read_Bytes
        (Stream, Entry_Header'Size / 8, EOF, Result, Error_If_EOF => False);
      if EOF or else not Result.Success then
         return False;
      end if;

      declare
         Raw_Header : Trace_Entry_Header
            with Import, Address => Buffer_Address (Stream);
      begin
         --  Swap bytes if needed, according to the file endianity

         if File_Header.Endianity /= Native_Endianity then
            Swap4 (Raw_Header.Unit_Name_Length'Address);
            Swap4 (Raw_Header.Statement_Bit_Count'Address);
            Swap4 (Raw_Header.Decision_Bit_Count'Address);
            Swap4 (Raw_Header.MCDC_Bit_Count'Address);
         end if;

         if Raw_Header.Unit_Part not in Supported_Unit_Part then
            Create_Error (Result, "invalid unit part");
            return False;

         elsif Raw_Header.Bit_Buffer_Encoding not in
            Supported_Bit_Buffer_Encoding
         then
            Create_Error (Result, "invalid bit buffer encoding");
            return False;

         elsif Raw_Header.Padding /= (1 .. 2 => ASCII.NUL) then
            Create_Error (Result, "invalid entry header padding");
            return False;
         end if;

         Entry_Header := Raw_Header;
      end;

      --  Now read the varible-length parts (unit name, statement, decision and
      --  MC/DC coverage buffers) in one single read.

      declare
         Unit_Name_Range        : constant Buffer_Range :=
            Range_For (File_Header.Alignment, 0,
                       Natural (Entry_Header.Unit_Name_Length));
         Statement_Buffer_Range : constant Buffer_Range :=
            Range_For (File_Header.Alignment,
                       Offset_After (Unit_Name_Range),
                       Buffer_Size (Entry_Header.Bit_Buffer_Encoding,
                                    Entry_Header.Statement_Bit_Count));
         Decision_Buffer_Range  : constant Buffer_Range :=
            Range_For (File_Header.Alignment,
                       Offset_After (Statement_Buffer_Range),
                       Buffer_Size (Entry_Header.Bit_Buffer_Encoding,
                                    Entry_Header.Decision_Bit_Count));
         MCDC_Buffer_Range      : constant Buffer_Range :=
            Range_For (File_Header.Alignment,
                       Offset_After (Decision_Buffer_Range),
                       Buffer_Size (Entry_Header.Bit_Buffer_Encoding,
                                    Entry_Header.MCDC_Bit_Count));

         Data_Size    : constant Natural := Offset_After (MCDC_Buffer_Range);
         Base_Address : System.Address;

         function "+"
           (Base_Address : System.Address; Offset : Natural)
            return System.Address
         is (System.Storage_Elements."+"
               (Base_Address,
                System.Storage_Elements.Storage_Offset (Offset)));
      begin
         Read_Bytes (Stream, Data_Size, EOF, Result);
         if not Result.Success then
            return False;
         end if;

         --  Communicate their address in Stream.Buffer to Trace_Entry

         Base_Address := Buffer_Address (Stream);
         Trace_Entry := (Base_Address + Unit_Name_Range.Offset,
                         Base_Address + Statement_Buffer_Range.Offset,
                         Base_Address + Decision_Buffer_Range.Offset,
                         Base_Address + MCDC_Buffer_Range.Offset);
         return True;
      end;
   end Read_Trace_Entry;

   -------------------
   -- Decode_Buffer --
   -------------------

   procedure Decode_Buffer
     (Encoding   : Supported_Bit_Buffer_Encoding;
      Raw_Buffer : Bytes_Array;
      Buffer     : out Coverage_Buffer;
      Result     : in out Read_Result)
   is
      use Interfaces;

      Bit_Index : Any_Bit_Id := Buffer'First;
   begin
      case Encoding is
         when LSB_First_Bytes =>
            for Byte_Index in Raw_Buffer'Range loop
               declare
                  Byte : Unsigned_8 := Raw_Buffer (Byte_Index);
                  Bit  : Boolean;
               begin
                  for I in 0 .. 7 loop
                     Bit := Byte mod 2 = 1;
                     Byte := Byte / 2;
                     if Bit_Index in Buffer'Range then
                        Buffer (Bit_Index) := Bit;
                     elsif Bit then
                        Create_Error (Result, "invalid bit buffer padding");
                     end if;
                     Bit_Index := Bit_Index + 1;
                  end loop;
               end;
            end loop;
      end case;
   end Decode_Buffer;

   ------------------------------------
   -- Generic_Read_Source_Trace_File --
   ------------------------------------

   procedure Generic_Read_Source_Trace_File
     (Filename : String;
      Result   : out Traces_Files.Read_Result)
   is
      Stream           : Binary_Stream;
      File_Header      : Trace_File_Header;
      Entry_Header     : Trace_Entry_Header;
      Trace_Entry      : Trace_Entry_Elements;
      Statement_Buffer : Coverage_Buffer_Access;
      Decision_Buffer  : Coverage_Buffer_Access;
      MCDC_Buffer      : Coverage_Buffer_Access;
   begin
      Result := (Success => True);

      --  Try to open the file

      Stream.File := Open_Read (Filename, Binary);
      if Stream.File = Invalid_FD then
         Create_Error (Result, "cannot open " & Filename);
         return;
      end if;
      Stream.Offset := 0;

      --  Read the trace file header

      Read_Trace_File_Header (Stream, File_Header, Result);
      if not Result.Success then
         goto Cleanup_And_Exit;
      end if;

      --  Read the sequence of trace info entries

      loop
         declare
            Kind : Supported_Info_Kind;
            Data : String_Access;
         begin
            Read_Trace_Info (Stream, File_Header, Kind, Data, Result);
            if not Result.Success then
               goto Cleanup_And_Exit;
            end if;

            if Kind /= Info_End then
               On_Trace_Info (Kind, Data.all);
            end if;
            Free (Data);

            if Kind = Info_End then
               exit;
            end if;
         end;
      end loop;

      --  If all went well so far, go through all trace entries

      while Read_Trace_Entry
        (Stream, File_Header, Entry_Header, Trace_Entry, Result)
      loop
         declare
            Unit_Name    : constant String
              (1 .. Natural (Entry_Header.Unit_Name_Length))
               with Import, Address => Trace_Entry.Unit_Name;

            function Convert is new Ada.Unchecked_Conversion
              (GNATcov_RTS.Buffers.SCOs_Hash, SC_Obligations.SCOs_Hash);
            Fingerprint : constant SC_Obligations.SCOs_Hash :=
               Convert (Entry_Header.Fingerprint);

            Statement_Buffer_Size : constant Natural :=
               Buffer_Size (Entry_Header.Bit_Buffer_Encoding,
                            Entry_Header.Statement_Bit_Count);
            Raw_Statement_Buffer  : constant Bytes_Array
              (1 .. Statement_Buffer_Size)
               with Import, Address => Trace_Entry.Statement_Buffer;

            Decision_Buffer_Size : constant Natural :=
               Buffer_Size (Entry_Header.Bit_Buffer_Encoding,
                            Entry_Header.Decision_Bit_Count);
            Raw_Decision_Buffer  : constant Bytes_Array
              (1 .. Decision_Buffer_Size)
               with Import, Address => Trace_Entry.Decision_Buffer;

            MCDC_Buffer_Size : constant Natural :=
               Buffer_Size (Entry_Header.Bit_Buffer_Encoding,
                            Entry_Header.MCDC_Bit_Count);
            Raw_MCDC_Buffer  : constant Bytes_Array
              (1 .. MCDC_Buffer_Size)
               with Import, Address => Trace_Entry.MCDC_Buffer;

            function Last_Bit (Bit_Count : Any_Bit_Count) return Any_Bit_Id
            is (Any_Bit_Id (Bit_Count) - 1);

         begin
            Reserve (Statement_Buffer, Entry_Header.Statement_Bit_Count);
            Reserve (Decision_Buffer, Entry_Header.Decision_Bit_Count);
            Reserve (MCDC_Buffer, Entry_Header.MCDC_Bit_Count);

            Decode_Buffer
              (Entry_Header.Bit_Buffer_Encoding,
               Raw_Statement_Buffer,
               Statement_Buffer
                 (0 .. Last_Bit (Entry_Header.Statement_Bit_Count)),
               Result);
            if not Result.Success then
               goto Cleanup_And_Exit;
            end if;

            Decode_Buffer
              (Entry_Header.Bit_Buffer_Encoding,
               Raw_Decision_Buffer,
               Decision_Buffer
                 (0 .. Last_Bit (Entry_Header.Decision_Bit_Count)),
               Result);
            if not Result.Success then
               goto Cleanup_And_Exit;
            end if;

            Decode_Buffer
              (Entry_Header.Bit_Buffer_Encoding,
               Raw_MCDC_Buffer,
               MCDC_Buffer (0 .. Last_Bit (Entry_Header.MCDC_Bit_Count)),
               Result);
            if not Result.Success then
               goto Cleanup_And_Exit;
            end if;

            On_Trace_Entry
              (Filename, Fingerprint,
               Unit_Name,
               Unit_Part_Map (Entry_Header.Unit_Part),
               Statement_Buffer
                 (0 .. Last_Bit (Entry_Header.Statement_Bit_Count)),
               Decision_Buffer
                 (0 .. Last_Bit (Entry_Header.Decision_Bit_Count)),
               MCDC_Buffer
                 (0 .. Last_Bit (Entry_Header.MCDC_Bit_Count)));
         end;
      end loop;

   <<Cleanup_And_Exit>>
      Free (Statement_Buffer);
      Free (Decision_Buffer);
      Free (MCDC_Buffer);
      Close (Stream.File);
   end Generic_Read_Source_Trace_File;

   ----------------------------
   -- Dump_Source_Trace_File --
   ----------------------------

   procedure Dump_Source_Trace_File (Filename : String) is

      procedure On_Trace_Info
        (Kind : GNATcov_RTS.Traces.Supported_Info_Kind;
         Data : String);
      procedure On_Trace_Entry
        (Filename        : String;
         Fingerprint     : SC_Obligations.SCOs_Hash;
         Unit_Name       : String;
         Unit_Part       : GNATCOLL.Projects.Unit_Parts;
         Stmt_Buffer     : Coverage_Buffer;
         Decision_Buffer : Coverage_Buffer;
         MCDC_Buffer     : Coverage_Buffer);
      --  Callbacks for Read_Source_Trace_File

      Last_Is_Info : Boolean := False;
      --  Whether the last line printed describes a trace info. Used to emit an
      --  empty line between trace infos and entries.

      procedure Dump_Buffer (Label : String; Buffer : Coverage_Buffer);

      -------------------
      -- On_Trace_Info --
      -------------------

      procedure On_Trace_Info
        (Kind : GNATcov_RTS.Traces.Supported_Info_Kind;
         Data : String)
      is
         use Ada.Text_IO;
         Kind_Name : constant String :=
           (case Kind is
            when Info_End          => raise Program_Error,
            when Info_Program_Name => "Program_Name",
            when Info_Exec_Date    => "Exec_Date",
            when Info_User_Data    => "User_Data");
      begin
         Put_Line ("Info " & Kind_Name & ": " & Data);
         Last_Is_Info := True;
      end On_Trace_Info;

      --------------------
      -- On_Trace_Entry --
      --------------------

      procedure On_Trace_Entry
        (Filename        : String;
         Fingerprint     : SC_Obligations.SCOs_Hash;
         Unit_Name       : String;
         Unit_Part       : GNATCOLL.Projects.Unit_Parts;
         Stmt_Buffer     : Coverage_Buffer;
         Decision_Buffer : Coverage_Buffer;
         MCDC_Buffer     : Coverage_Buffer)
      is
         pragma Unreferenced (Filename);
         use Ada.Text_IO;
      begin
         if Last_Is_Info then
            New_Line;
            Last_Is_Info := False;
         end if;

         Put ("Unit " & Unit_Name & " (" & Unit_Part'Image & ", hash=");
         for B of Fingerprint loop
            Put (Hex_Images.Hex_Image (Interfaces.Unsigned_8 (B)));
         end loop;
         Put_Line (")");
         Dump_Buffer ("Statement", Stmt_Buffer);
         Dump_Buffer ("Decision", Decision_Buffer);
         Dump_Buffer ("MCDC", MCDC_Buffer);
         New_Line;
      end On_Trace_Entry;

      -----------------
      -- Dump_Buffer --
      -----------------

      procedure Dump_Buffer (Label : String; Buffer : Coverage_Buffer) is
         use Ada.Text_IO;
         Is_Empty : Boolean := True;
      begin
         Put (Label & " buffer:");
         Put ((1 .. 9 - Label'Length => ' '));
         Put (" [");
         if Buffer'Length = 0 then
            Put ("empty range");
         else
            Put (Strings.Img (Integer (Buffer'First))
                 & "-" & Strings.Img (Integer (Buffer'Last)));
         end if;
         Put ("]");
         for Bit_Id in Buffer'Range loop
            if Buffer (Bit_Id) then
               Is_Empty := True;
               Put (Bit_Id'Image);
            end if;
         end loop;
         if Is_Empty then
            Put (" <empty>");
         end if;
         New_Line;
      end Dump_Buffer;

      procedure Read_Source_Trace_File is new Generic_Read_Source_Trace_File;

      Result : Read_Result;

   --  Start of processing for Dump_Source_Trace_File

   begin
      Read_Source_Trace_File (Filename, Result);
      if not Result.Success then
         Outputs.Fatal_Error (Ada.Strings.Unbounded.To_String (Result.Error));
      end if;
   end Dump_Source_Trace_File;

end Instrument.Input_Traces;
