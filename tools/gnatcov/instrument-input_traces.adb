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

with Ada.Direct_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Streams;     use Ada.Streams;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces;

with Switches;
with System;
with System.Storage_Elements;

with GNAT.OS_Lib;        use GNAT.OS_Lib;
with GNAT.Byte_Swapping; use GNAT.Byte_Swapping;

with Coverage.Source;
with Hashes; use Hashes;
with Hex_Images;
with Outputs;

package body Instrument.Input_Traces is

   Native_Endianity : constant Supported_Endianity :=
     Traces_Source.Native_Endianity;

   subtype Read_Result is Traces_Files.Read_Result;
   procedure Create_Error (Result : out Read_Result; Error : String)
   renames Traces_Files.Create_Error;

   function Buffer_Size
     (Encoding : Supported_Bit_Buffer_Encoding; Bit_Count : Any_Bit_Count)
      return Natural;
   --  Return the size (in bytes) of the representation of a coverage buffer in
   --  a source trace file given an encoding and a bit count.

   function With_Padding
     (Alignment : Supported_Alignment; Size : Natural) return Natural;
   --  Return Size plus the number of padding bytes required so that the result
   --  is a multiple of Alignment.

   type Bytes_Array is array (Positive range <>) of Interfaces.Unsigned_8;

   type Bytes_Access is access all Bytes_Array;

   procedure Free is new
     Ada.Unchecked_Deallocation (Bytes_Array, Bytes_Access);

   type Buffer_Range is record
      Offset, Size : Natural;
      --  Offset and size of the range

      Padded_Size : Natural;
      --  Total size of the buffer range so that Offset + Padded_Size is a
      --  multiple of some alignment.
   end record;
   --  Describe a range of bytes in a buffer

   function Range_For
     (Alignment : Supported_Alignment; Offset, Size : Natural)
      return Buffer_Range
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

   procedure Free is new
     Ada.Unchecked_Deallocation (Coverage_Buffer, Coverage_Buffer_Access);

   procedure Reserve
     (Buffer : in out Coverage_Buffer_Access; Size : Any_Bit_Count);
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
      Data        : out GNAT.OS_Lib.String_Access;
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
   --  Try to read a trace entry header from Stream.
   --
   --  If Stream already reached EOF, just return False. Otherwise, decode it
   --  and fill Entry_Header with information from it. Then fetch the
   --  remaining entry data in Stream.Buffer and make Trace_Entry reference
   --  this data.
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
     (Encoding : Supported_Bit_Buffer_Encoding; Bit_Count : Any_Bit_Count)
      return Natural is
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
     (Alignment : Supported_Alignment; Size : Natural) return Natural
   is
      Result : constant Natural := Size mod Natural (Alignment);
   begin
      return Size + (if Result = 0 then 0 else Natural (Alignment) - Result);
   end With_Padding;

   ---------------
   -- Range_For --
   ---------------

   function Range_For
     (Alignment : Supported_Alignment; Offset, Size : Natural)
      return Buffer_Range is
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
            Read_Size :=
              Read
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
      return Stream.Buffer.all (1)'Address;
   end Buffer_Address;

   -------------
   -- Reserve --
   -------------

   procedure Reserve
     (Buffer : in out Coverage_Buffer_Access; Size : Any_Bit_Count)
   is
      Last_Bit : constant Any_Bit_Id := Any_Bit_Id (Size) - 1;

      --  ??? Due to a compiler bug, we should avoid slices on empty
      --  packed arrays (T325-007), so make sure our packed arrays are never
      --  empty.

      Actual_Last_Bit : constant Bit_Id := Any_Bit_Id'Max (0, Last_Bit);
   begin
      if Buffer = null or else Actual_Last_Bit > Buffer.all'Last then
         Free (Buffer);
         Buffer := new Coverage_Buffer (0 .. Actual_Last_Bit);
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
      Read_Bytes (Stream, File_Header'Size / 8, Ignored_EOF, Result);
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
            Create_Error
              (Result,
               "Trace format v"
               & Strings.Img (Integer (Raw_Header.Format_Version))
               & " is not supported by this version of gnatcov. "
               & "Handling v"
               & Strings.Img (Integer (Current_Version))
               & " only.");
            return;

         elsif Raw_Header.Alignment not in 1 | 2 | 4 | 8 | 16 then
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
      Data        : out GNAT.OS_Lib.String_Access;
      Result      : in out Read_Result)
   is
      Ignored_EOF : Boolean;
   begin
      --  Default initialize OUT arguments to avoid pointless warnings

      Kind := Info_End;
      Data := null;

      --  Read the trace info header

      Read_Bytes (Stream, Trace_Info_Header'Size / 8, Ignored_EOF, Result);
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

         if Raw_Header.Language_Kind not in Supported_Language_Kind then
            Create_Error (Result, "invalid language kind");
            return False;

         --  Check that the unit part has a valid value, which depends on the
         --  current language kind.

         elsif (case Raw_Header.Language_Kind is
                  when Traces_Source.Unit_Based_Language =>
                    Raw_Header.Unit_Part not in Supported_Unit_Part,
                  when Traces_Source.File_Based_Language =>
                    Raw_Header.Unit_Part /= Traces_Source.Not_Applicable_Part,
                  when others                            =>
                    raise Program_Error
                      with "invalid language while already validated")
         then
            Create_Error (Result, "invalid unit part");
            return False;

         elsif Raw_Header.Bit_Buffer_Encoding
               not in Supported_Bit_Buffer_Encoding
         then
            Create_Error (Result, "invalid bit buffer encoding");
            return False;

         elsif Raw_Header.Padding /= (1 .. 1 => ASCII.NUL) then
            Create_Error (Result, "invalid entry header padding");
            return False;
         end if;

         Entry_Header := Raw_Header;
      end;

      --  Now read the varible-length parts (unit name, statement, decision and
      --  MC/DC coverage buffers) in one single read.

      declare
         Unit_Name_Range        : constant Buffer_Range :=
           Range_For
             (File_Header.Alignment,
              0,
              Natural (Entry_Header.Unit_Name_Length));
         Statement_Buffer_Range : constant Buffer_Range :=
           Range_For
             (File_Header.Alignment,
              Offset_After (Unit_Name_Range),
              Buffer_Size
                (Entry_Header.Bit_Buffer_Encoding,
                 Entry_Header.Statement_Bit_Count));
         Decision_Buffer_Range  : constant Buffer_Range :=
           Range_For
             (File_Header.Alignment,
              Offset_After (Statement_Buffer_Range),
              Buffer_Size
                (Entry_Header.Bit_Buffer_Encoding,
                 Entry_Header.Decision_Bit_Count));
         MCDC_Buffer_Range      : constant Buffer_Range :=
           Range_For
             (File_Header.Alignment,
              Offset_After (Decision_Buffer_Range),
              Buffer_Size
                (Entry_Header.Bit_Buffer_Encoding,
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
         Trace_Entry :=
           (Base_Address + Unit_Name_Range.Offset,
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

      Bit_Index : Any_Bit_Id := Any_Bit_Id (Buffer'First);
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
     (Filename : String; Result : out Traces_Files.Read_Result)
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
            Data : GNAT.OS_Lib.String_Access;
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
            Unit_Name :
              constant String (1 .. Natural (Entry_Header.Unit_Name_Length))
            with Import, Address => Trace_Entry.Unit_Name;

            function Convert is new
              Ada.Unchecked_Conversion
                (Traces_Source.Fingerprint_Type,
                 SC_Obligations.Fingerprint_Type);

            Fingerprint          : constant SC_Obligations.Fingerprint_Type :=
              Convert (Entry_Header.Fingerprint);
            Bit_Maps_Fingerprint : constant SC_Obligations.Fingerprint_Type :=
              Convert (Entry_Header.Bit_Maps_Fingerprint);

            Annotations_Fingerprint :
              constant SC_Obligations.Fingerprint_Type :=
                Convert (Entry_Header.Annotations_Fingerprint);

            Statement_Buffer_Size : constant Natural :=
              Buffer_Size
                (Entry_Header.Bit_Buffer_Encoding,
                 Entry_Header.Statement_Bit_Count);
            Raw_Statement_Buffer  :
              constant Bytes_Array (1 .. Statement_Buffer_Size)
            with Import, Address => Trace_Entry.Statement_Buffer;

            Decision_Buffer_Size : constant Natural :=
              Buffer_Size
                (Entry_Header.Bit_Buffer_Encoding,
                 Entry_Header.Decision_Bit_Count);
            Raw_Decision_Buffer  :
              constant Bytes_Array (1 .. Decision_Buffer_Size)
            with Import, Address => Trace_Entry.Decision_Buffer;

            MCDC_Buffer_Size : constant Natural :=
              Buffer_Size
                (Entry_Header.Bit_Buffer_Encoding,
                 Entry_Header.MCDC_Bit_Count);
            Raw_MCDC_Buffer  : constant Bytes_Array (1 .. MCDC_Buffer_Size)
            with Import, Address => Trace_Entry.MCDC_Buffer;

            function Last_Bit (Bit_Count : Any_Bit_Count) return Any_Bit_Id
            is (Any_Bit_Id (Bit_Count) - 1);

            CU_Name : Compilation_Unit_Part;

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

            case Supported_Language_Kind (Entry_Header.Language_Kind) is
               when Traces_Source.Unit_Based_Language =>
                  CU_Name :=
                    CU_Name_For_Unit
                      (Unit => To_Qualified_Name (Unit_Name),
                       Part => +Entry_Header.Unit_Part);

               when Traces_Source.File_Based_Language =>
                  CU_Name := CU_Name_For_File (+Unit_Name);
            end case;

            On_Trace_Entry
              (Filename,
               Fingerprint,
               CU_Name,
               Bit_Maps_Fingerprint,
               Annotations_Fingerprint,
               Statement_Buffer
                 (0 .. Last_Bit (Entry_Header.Statement_Bit_Count)),
               Decision_Buffer
                 (0 .. Last_Bit (Entry_Header.Decision_Bit_Count)),
               MCDC_Buffer (0 .. Last_Bit (Entry_Header.MCDC_Bit_Count)));
         end;
      end loop;

      <<Cleanup_And_Exit>>
      Free (Statement_Buffer);
      Free (Decision_Buffer);
      Free (MCDC_Buffer);
      Free (Stream.Buffer);
      Close (Stream.File);
   end Generic_Read_Source_Trace_File;

   ----------------------------
   -- Dump_Source_Trace_File --
   ----------------------------

   procedure Dump_Source_Trace_File (Filename : String) is

      procedure On_Trace_Info
        (Kind : Traces_Source.Supported_Info_Kind; Data : String);
      procedure On_Trace_Entry
        (Filename                : String;
         Fingerprint             : SC_Obligations.Fingerprint_Type;
         CU_Name                 : Compilation_Unit_Part;
         Bit_Maps_Fingerprint    : SC_Obligations.Fingerprint_Type;
         Annotations_Fingerprint : SC_Obligations.Fingerprint_Type;
         Stmt_Buffer             : Coverage_Buffer;
         Decision_Buffer         : Coverage_Buffer;
         MCDC_Buffer             : Coverage_Buffer);
      --  Callbacks for Read_Source_Trace_File

      Last_Is_Info : Boolean := False;
      --  Whether the last line printed describes a trace info. Used to emit an
      --  empty line between trace infos and entries.

      procedure Dump_Buffer (Label : String; Buffer : Coverage_Buffer);

      -------------------
      -- On_Trace_Info --
      -------------------

      procedure On_Trace_Info
        (Kind : Traces_Source.Supported_Info_Kind; Data : String)
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
        (Filename                : String;
         Fingerprint             : SC_Obligations.Fingerprint_Type;
         CU_Name                 : Compilation_Unit_Part;
         Bit_Maps_Fingerprint    : SC_Obligations.Fingerprint_Type;
         Annotations_Fingerprint : SC_Obligations.Fingerprint_Type;
         Stmt_Buffer             : Coverage_Buffer;
         Decision_Buffer         : Coverage_Buffer;
         MCDC_Buffer             : Coverage_Buffer)
      is
         pragma Unreferenced (Filename);
         use Ada.Text_IO;
      begin
         if Last_Is_Info then
            New_Line;
            Last_Is_Info := False;
         end if;

         case CU_Name.Language_Kind is
            when Traces_Source.Unit_Based_Language =>
               Put
                 ("Unit "
                  & To_Ada (CU_Name.Unit)
                  & " ("
                  & CU_Name.Part'Image
                  & ", ");

            when Traces_Source.File_Based_Language =>
               Put ("Unit " & (+CU_Name.Filename) & " (");
         end case;

         Put ("SCOs hash=");
         for B of Fingerprint loop
            Put (Hex_Images.Hex_Image (Interfaces.Unsigned_8 (B)));
         end loop;

         Put (", bit maps hash=");
         for B of Bit_Maps_Fingerprint loop
            Put (Hex_Images.Hex_Image (Interfaces.Unsigned_8 (B)));
         end loop;

         Put (", annotations hash=");
         for B of Annotations_Fingerprint loop
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
            Put
              (Strings.Img (Integer (Buffer'First))
               & "-"
               & Strings.Img (Integer (Buffer'Last)));
         end if;
         Put ("]");
         for Bit_Id in Buffer'Range loop
            if Buffer (Bit_Id) then
               Is_Empty := False;
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
         Outputs.Fatal_Error (+Result.Error);
      end if;
   end Dump_Source_Trace_File;

   --------------------------
   -- Extract_Base64_Trace --
   --------------------------

   procedure Extract_Base64_Trace (Input_File, Output_File : String) is
      use Interfaces;

      package TIO renames Ada.Text_IO;
      package BIO is new Ada.Direct_IO (Interfaces.Unsigned_8);

      subtype Whitespace is Character
      with
        Static_Predicate => Whitespace in ' ' | ASCII.HT | ASCII.CR | ASCII.LF;

      procedure Trim
        (S : String; First : in out Positive; Last : in out Natural);
      --  Adjust First/Last so that S (First .. Last) has no leading/trailing
      --  whitespaces, tabs, carriage returns and newline characters.

      function Base64_Digit (C : Character) return Unsigned_8;
      --  Return the 6-bit number that the C Base64 digit means

      function Nth_Trace_Filename (N : Natural) return String;
      --  Return the filename of the nth (0-indexed) trace filename following
      --  this pattern:
      --  - N-th when (N = 0) -> Output_File (for retro compatibility)
      --  - N-th              -> Base(Output_File)_N.Extension(Output_File)

      Trace_Count : Natural := 0;
      --  Count the number of traces we encountered.

      Input  : TIO.File_Type;
      Output : BIO.File_Type;

      ----------
      -- Trim --
      ----------

      procedure Trim
        (S : String; First : in out Positive; Last : in out Natural) is
      begin
         while First in S'Range and then S (First) in Whitespace loop
            First := First + 1;
         end loop;
         while Last in S'Range and then S (Last) in Whitespace loop
            Last := Last - 1;
         end loop;
      end Trim;

      ------------------
      -- Base64_Digit --
      ------------------

      function Base64_Digit (C : Character) return Unsigned_8 is
         C_Pos : constant Unsigned_8 := Character'Pos (C);

         subtype Upper is Character range 'A' .. 'Z';
         Upper_A_Pos  : constant Unsigned_8 := Character'Pos ('A');
         Upper_Offset : constant Unsigned_8 := 0;

         subtype Lower is Character range 'a' .. 'z';
         Lower_A_Pos  : constant Unsigned_8 := Character'Pos ('a');
         Lower_Offset : constant Unsigned_8 := Upper_Offset + 26;

         subtype Digit is Character range '0' .. '9';
         Digit_0_Pos  : constant Unsigned_8 := Character'Pos ('0');
         Digit_Offset : constant Unsigned_8 := Lower_Offset + 26;

         Plus_Offset : constant Unsigned_8 := Digit_Offset + 10;

      begin
         case C is
            when Upper  =>
               return C_Pos - Upper_A_Pos + Upper_Offset;

            when Lower  =>
               return C_Pos - Lower_A_Pos + Lower_Offset;

            when Digit  =>
               return C_Pos - Digit_0_Pos + Digit_Offset;

            when '+'    =>
               return Plus_Offset;

            when '/'    =>
               return Plus_Offset + 1;

            when others =>
               Outputs.Fatal_Error ("Invalid Base64 digit: " & C);
         end case;
      end Base64_Digit;

      ------------------------
      -- Nth_Trace_Filename --
      ------------------------

      function Nth_Trace_Filename (N : Natural) return String is
      begin
         if N = 0 then
            return Output_File;
         else
            return
              Base_Name (Output_File)
              & "-"
              & Img (N)
              & "."
              & Extension (Output_File);
         end if;
      end Nth_Trace_Filename;

      Start_Marker : constant String := "== GNATcoverage source trace file ==";
      End_Marker   : constant String := "== End ==";

      Started_Trace : Boolean := False;
      --  Whether we are in the middle of reading a source trace

      Prefix      : String (1 .. 200) := (others => ASCII.NUL);
      Prefix_Last : Natural := 0;
      --  If Started_Trace is True, line prefix for the base64 dump for that
      --  source trace.

      --  Start of processing for Extract_Base64_Trace

   begin
      --  Read the input file line by line. We use gotos to Read_Next_Line as a
      --  way to "continue" the loop (i.e. skip to the next iteration), which
      --  simplifies the loop body.
      --
      --  Each time we come across a line that ends with Start_Marker, create
      --  the output source trace file, take note of the rest of the line as
      --  "the prefix" and ignore the next lines that do not have the same
      --  prefix, then process all lines until finding a line with the
      --  End_Marker. Then repeat with the remaining lines if another line ends
      --  with Start_Marker.
      --
      --  For instance:
      --
      --     A> == GNATcoverage source trace file ==
      --     B> == GNATcoverage source trace file ==
      --     A> ...
      --     A> == End ==
      --     B> ...
      --     B> == End ==
      --
      --  will create a source trace file for all lines with the "A> " prefix,
      --  whereas:
      --
      --     A> == GNATcoverage source trace file ==
      --     B> == GNATcoverage source trace file ==
      --     A> ...
      --     A> == End ==
      --     B> ...
      --     C> == GNATcoverage source trace file ==
      --     B> == End ==
      --     C> ...
      --     C> == End ==
      --
      --  will first create a source trace file for all lines with the "A> "
      --  prefix, then overwrite it with a file for all lines with the "C> "
      --  prefix.
      --
      --  The prefix-related rules allow the processing of source traces for a
      --  program with several concurrent traces dumps, as long as lines are
      --  printed atomically and as long as each tasks writes a dedicated line
      --  prefix.
      --
      --  The overwriting rules ensures that if there are several start/end
      --  marker couples in the input file, we consider only the last one. This
      --  is convenient when running GNATcoverage's testsuite, as we may have
      --  one trace per task termination and this also makes sense in real
      --  life: the last trace should be the one with the final coverage state.
      --
      --  However, when the `--split-extracted-traces` option is passed, the
      --  command will create several files for each trace sequentially found.
      --  This is necessary since now that we can reset coverage buffers, there
      --  is no warranty that the last trace is the most complete one.

      TIO.Open (Input, TIO.In_File, Input_File);
      <<Read_Next_Line>>
      while not TIO.End_Of_File (Input) loop
         declare
            use type TIO.Count;

            Buffer      : String (1 .. 200);
            Ignore_Line : Boolean;
            First       : Positive := 1;
            Last        : Natural;
         begin
            --  Try to read a line, abort if we reached the end of file

            begin
               TIO.Get_Line (Input, Buffer, Last);
            exception
               when TIO.End_Error =>
                  exit;
            end;

            --  Remove leading/trailing whitespaces, which copy/pasting may
            --  insert.

            Trim (Buffer, First, Last);

            --  If we are in the middle of reading a source trace, ignore lines
            --  that do not have the same prefix.

            Ignore_Line :=
              Started_Trace
              and then not Has_Prefix
                             (Buffer (First .. Last),
                              Prefix (Prefix'First .. Prefix_Last));

            --  If the line is too long for our buffer:
            --
            --  * If that line has the current prefix and we are in the middle
            --    of reading a source trace, stop with an error: this is not
            --    supposed to happen as we dump source traces with at most
            --    80-columns wide lines.
            --
            --  * Otherwise, just skip that line.
            --
            --  Note that, for some reason, End_Of_Line may return False even
            --  if we managed to read the full line. To consider that we did
            --  manage to read the full line in such cases, also check the
            --  column (1 = we reached the next line, so we managed to read the
            --  previous line in one go).

            if not TIO.End_Of_Line (Input) and then TIO.Col (Input) > 1 then
               if Started_Trace and then not Ignore_Line then

                  --  Remove any corrupted source trace file that
                  --  started to be generated.

                  BIO.Delete (Output);

                  Outputs.Fatal_Error ("Unexpected long line in Base64 trace");
               else
                  TIO.Skip_Line (Input);
                  goto Read_Next_Line;
               end if;
            end if;

            --  If this line does not have the expected prefix, ignore it

            if Ignore_Line then
               goto Read_Next_Line;

            --  If we have not started reading a trace, check if the current
            --  line is the beginning of a trace.

            elsif not Started_Trace then

               --  If this is the beginning of a trace, keep track of the
               --  current line prefix and open the source trace file to write.

               if Has_Suffix (Buffer (First .. Last), Start_Marker) then
                  Started_Trace := True;
                  declare
                     Prefix_Length : constant Natural :=
                       Last - First + 1 - Start_Marker'Length;
                  begin
                     Prefix_Last := Prefix_Length + Prefix'First - 1;
                     Prefix (Prefix'First .. Prefix_Last) :=
                       Buffer (First .. First + Prefix_Length - 1);
                  end;
                  BIO.Create
                    (Output,
                     BIO.Out_File,
                     (if Switches.Split_Extracted_Traces
                      then Nth_Trace_Filename (Trace_Count)
                      else Output_File));
                  Trace_Count := Trace_Count + 1;
               end if;

               goto Read_Next_Line;
            end if;

            --  If execution reaches this point, we know that we are in the
            --  middle of processing a source trace. Adjust First to strip the
            --  line prefix.

            pragma Assert (Started_Trace);
            First := First + Prefix_Last - Prefix'First + 1;

            --  If we reached the end of this source trace, close the file and
            --  go to the next line.

            if Buffer (First .. Last) = End_Marker then
               BIO.Close (Output);
               Started_Trace := False;
               goto Read_Next_Line;
            end if;

            --  Otherwise, expect the rest of the line to be a groups of 4
            --  base64 characters.

            if Buffer (First .. Last)'Length mod 4 /= 0 then

               --  If there is a format error in the base64 encoding,
               --  properly exit by removing the output file that may have been
               --  created, to avoid leaving a corrupted file to the user.

               BIO.Delete (Output);

               Outputs.Fatal_Error
                 ("Invalid Base64 trace: incomplete group of 4 characters");
            end if;

            --  Now process each group

            declare
               Next : Positive := First;

               function D (Index : Natural) return Unsigned_8
               is (Base64_Digit (Buffer (Next + Index)));

            begin
               while Next <= Last loop
                  --  Here, process the Base64 digits in the slice:
                  --
                  --    Buffer (Next .. Next + 3)
                  --
                  --  This slice contains 4 Base64 digits, and each digit
                  --  encodes 6 bits (total: 24 bits), so we can decode 3
                  --  bytes (3 * 8 bits).  The actual number of bytes to
                  --  decode depends on the amount of padding characters
                  --  ('=' character).

                  BIO.Write
                    (Output, Shift_Left (D (0), 2) or Shift_Right (D (1), 4));
                  if Buffer (Next + 2) /= '=' then
                     BIO.Write
                       (Output,
                        Shift_Left (D (1), 4) or Shift_Right (D (2), 2));
                     if Buffer (Next + 3) /= '=' then
                        BIO.Write (Output, Shift_Left (D (2), 6) or D (3));
                     end if;
                  end if;

                  Next := Next + 4;
               end loop;
            end;
         end;
      end loop;
      TIO.Close (Input);

      --  Make sure we had at least one trace, and a matching "end" marker for
      --  the last trace.

      if Trace_Count = 0 then
         Outputs.Fatal_Error ("No Base64 trace found");
      elsif Started_Trace then
         BIO.Delete (Output);
         Outputs.Fatal_Error ("Incomplete Base64 trace");
      end if;
   end Extract_Base64_Trace;

   ----------
   -- Hash --
   ----------

   function Hash
     (Self : Consolidated_Trace_Key) return Ada.Containers.Hash_Type is
   begin
      return Result : Ada.Containers.Hash_Type := Hash (Self.CU_Name) do
         for Digit of Self.Fingerprint loop
            Result := Combine (Result, Stream_Element'Pos (Digit));
         end loop;
      end return;
   end Hash;

   ------------------
   -- Update_State --
   ------------------

   procedure Update_State
     (Self                    : in out Consolidation_State;
      Filename                : String;
      Fingerprint             : SC_Obligations.Fingerprint_Type;
      CU_Name                 : Compilation_Unit_Part;
      Bit_Maps_Fingerprint    : SC_Obligations.Fingerprint_Type;
      Annotations_Fingerprint : SC_Obligations.Fingerprint_Type;
      Stmt_Buffer             : Coverage_Buffer;
      Decision_Buffer         : Coverage_Buffer;
      MCDC_Buffer             : Coverage_Buffer)
   is
      use Consolidated_Trace_Maps;

      function Part_Image (Part : GPR2.Valid_Unit_Kind) return String;
      --  Helper to include Part in an error message

      function Unit_Image return String
      is (case CU_Name.Language_Kind is
            when Unit_Based_Language =>
              (Part_Image (CU_Name.Part) & " " & To_Ada (CU_Name.Unit)),
            when File_Based_Language => +CU_Name.Filename);
      --  Helper to refer to the instrumented unit in an error message

      ----------------
      -- Part_Image --
      ----------------

      function Part_Image (Part : GPR2.Valid_Unit_Kind) return String is
      begin
         return
           (case Part is
              when GPR2.S_Body     => "body of",
              when GPR2.S_Spec     => "spec of",
              when GPR2.S_Separate => "separate");
      end Part_Image;

      Key : constant Consolidated_Trace_Key := (CU_Name, Fingerprint);
      Elt : Consolidated_Trace_Entry;
      Cur : Cursor;
      CU  : CU_Id;
   begin
      if Misc_Trace.Is_Active then
         Misc_Trace.Trace ("processing traces for unit " & Unit_Image);
      end if;

      --  Look for a consolidated trace entry corresponding to these coverage:
      --  if we find one, we have already found the corresponding compilation
      --  unit.

      Cur := Self.Map.Find (Key);
      if Has_Element (Cur) then
         CU := Element (Cur).CU;
      else
         CU := Find_Instrumented_Unit (CU_Name);

         if CU = No_CU_Id then

            --  When using a single instrumented program to compute separate
            --  coverage for all units (common in unit testing), it is
            --  legitimate to process source trace files that contain entries
            --  relating to units not of interest. So in this case, do not even
            --  warn about it: just log the fact that we skip this trace entry
            --  in the verbose about, just in case.

            Misc_Trace.Trace
              ("discarding source trace entry for unknown instrumented unit: "
               & Unit_Image);
            return;

         elsif Provider (CU) /= Instrumenter then

            --  We loaded compiler-generated SCOs for this unit before
            --  processing its source trace buffer, so we have inconsistent
            --  information. Just ignore this coverage information and proceed.

            Outputs.Warn
              ("inconsistent coverage method, ignoring coverage information"
               & " for "
               & Unit_Image);
            return;
         end if;
      end if;

      --  Sanity check that Fingerprint is consistent with what the
      --  instrumenter recorded in the CU info.

      if not Has_Fingerprint (CU, Fingerprint)
        or else Bit_Maps_Fingerprint
                /= SC_Obligations.Bit_Maps_Fingerprint (CU, Fingerprint)
        or else Annotations_Fingerprint
                /= SC_Obligations.Annotations_Fingerprint (CU, Fingerprint)
      then
         Outputs.Warn
           ("traces for "
            & Unit_Image
            & " (from "
            & Filename
            & ") are"
            & " inconsistent with the corresponding Source Instrumentation"
            & " Data");
         return;
      end if;

      if Has_Element (Cur) then

         --  Update existing coverage buffers for this unit

         Elt := Element (Cur);
         pragma Assert (Elt.CU = CU);

         pragma Assert (Stmt_Buffer'First = Elt.Stmt_Buffer'First);
         pragma Assert (Stmt_Buffer'Last = Elt.Stmt_Buffer'Last);

         pragma Assert (Decision_Buffer'First = Elt.Decision_Buffer'First);
         pragma Assert (Decision_Buffer'Last = Elt.Decision_Buffer'Last);

         pragma Assert (MCDC_Buffer'First = Elt.MCDC_Buffer'First);
         pragma Assert (MCDC_Buffer'Last = Elt.MCDC_Buffer'Last);

         --  Update coverage buffers in this entry

         for B in Stmt_Buffer'Range loop
            if Stmt_Buffer (B) then
               Elt.Stmt_Buffer (B) := True;
            end if;
         end loop;
         for B in Decision_Buffer'Range loop
            if Decision_Buffer (B) then
               Elt.Decision_Buffer (B) := True;
            end if;
         end loop;
         for B in MCDC_Buffer'Range loop
            if MCDC_Buffer (B) then
               Elt.MCDC_Buffer (B) := True;
            end if;
         end loop;

      else
         --  Create a new entry for coverage buffers and initialize them from
         --  this trace entry.

         pragma Assert (Stmt_Buffer'First = 0);
         pragma Assert (Decision_Buffer'First = 0);
         pragma Assert (MCDC_Buffer'First = 0);
         Elt :=
           new Consolidated_Trace_Entry_Record
                 (Stmt_Buffer'Last, Decision_Buffer'Last, MCDC_Buffer'Last);
         Elt.CU := CU;
         Elt.Stmt_Buffer := Stmt_Buffer;
         Elt.Decision_Buffer := Decision_Buffer;
         Elt.MCDC_Buffer := MCDC_Buffer;
         Self.Map.Insert (Key, Elt);
      end if;
   end Update_State;

   -------------------
   -- Process_State --
   -------------------

   procedure Process_State (Self : in out Consolidation_State) is
   begin
      for Cur in Self.Map.Iterate loop
         declare
            use Consolidated_Trace_Maps;

            K : constant Consolidated_Trace_Key := Key (Cur);
            E : constant Consolidated_Trace_Entry := Element (Cur);
         begin
            Coverage.Source.Compute_Source_Coverage
              (E.CU,
               K.Fingerprint,
               E.Stmt_Buffer,
               E.Decision_Buffer,
               E.MCDC_Buffer);
         end;
      end loop;
   end Process_State;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Consolidation_State) is
   begin
      for Key of Self.Map loop
         Free (Key);
      end loop;
      Self.Map.Clear;
   end Release;

end Instrument.Input_Traces;
