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

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

with GNAT.CRC32;  use GNAT.CRC32;
with GNAT.Regpat; use GNAT.Regpat;

with Hex_Images; use Hex_Images;

package body Binary_Files is

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Binary_Content_Bytes_Acc);

   function Convert is new Ada.Unchecked_Conversion
     (Str_Access, System.Address);

   function Compute_CRC32 (File : Binary_File) return Unsigned_32;
   --  Compute and return the CRC32 of File

   Time_Stamp_Regexp  : constant String :=
      "([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+):([0-9]+)";
   Time_Stamp_Matcher : constant Pattern_Matcher :=
      Compile (Time_Stamp_Regexp);

   --------------
   -- Allocate --
   --------------

   function Allocate (Size : Arch.Arch_Addr) return Loaded_Section is
   begin
      return (Kind => Allocated, Buffer => new String (1 .. Natural (Size)));
   end Allocate;

   ---------
   -- "+" --
   ---------

   function "+" (Region : Mapped_Region) return Loaded_Section is
   begin
      return (Kind => Mapped, Region => Region);
   end "+";

   ----------
   -- Size --
   ----------

   function Size (LS : Loaded_Section) return Arch.Arch_Addr is
   begin
      case LS.Kind is
         when None =>
            raise Program_Error;
         when Mapped =>
            return Arch.Arch_Addr (Data_Size (LS.Region));
         when Allocated =>
            return LS.Buffer.all'Length;
      end case;
   end Size;

   -------------
   -- Content --
   -------------

   function Content (LS : Loaded_Section) return Binary_Content is
      Addr : System.Address;
      Size : Arch.Arch_Addr;
   begin
      case LS.Kind is
         when None =>
            raise Program_Error;

         when Mapped =>
            Addr := Convert (Data (LS.Region));
            Size := Arch.Arch_Addr (Data_Size (LS.Region));

         when Allocated =>
            Addr := LS.Buffer.all'Address;
            Size := LS.Buffer.all'Length;
      end case;

      return (if Size > 0
              then Wrap (Addr, 0, Size - 1)
              else Wrap (Addr, 1, 0));
   end Content;

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of
     (LS     : Loaded_Section;
      Offset : Arch.Arch_Addr := 0) return System.Address is
   begin
      return Address_Of (Content (LS), Offset);
   end Address_Of;

   ----------
   -- Free --
   ----------

   procedure Free (LS : in out Loaded_Section) is
   begin
      case LS.Kind is
         when None =>
            null;

         when Mapped =>
            Free (LS.Region);

         when Allocated =>
            Free (LS.Buffer);
      end case;
      LS := No_Loaded_Section;
   end Free;

   --------
   -- Fd --
   --------

   function Fd (F : Binary_File) return File_Descriptor is
   begin
      return F.Fd;
   end Fd;

   ----------
   -- File --
   ----------

   function File (F : Binary_File) return Mapped_File is
   begin
      return F.File;
   end File;

   -----------------
   -- File_Region --
   -----------------

   function File_Region (F : Binary_File) return Mapped_Region is
   begin
      return F.Region;
   end File_Region;

   ---------------------
   -- Set_File_Region --
   ---------------------

   procedure Set_File_Region (F : in out Binary_File; R : Mapped_Region) is
   begin
      F.Region := R;
   end Set_File_Region;

   --------------
   -- Filename --
   --------------

   function Filename (F : Binary_File) return String is
   begin
      return F.Filename.all;
   end Filename;

   ----------------------
   -- Get_Nbr_Sections --
   ----------------------

   function Get_Nbr_Sections (File : Binary_File) return Section_Index is
   begin
      return File.Nbr_Sections;
   end Get_Nbr_Sections;

   procedure Set_Nbr_Sections (File : in out Binary_File; Nbr : Section_Index)
   is
   begin
      File.Nbr_Sections := Nbr;
   end Set_Nbr_Sections;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (File : Binary_File) return Binary_File_Status is
   begin
      return File.Status;
   end Get_Status;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status
     (File : in out Binary_File; Status : Binary_File_Status) is
   begin
      File.Status := Status;
   end Set_Status;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (File : Binary_File) return Long_Integer is
   begin
      return File.Size;
   end Get_Size;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   function Get_Time_Stamp (File : Binary_File) return OS_Time is
   begin
      return File.Time_Stamp;
   end Get_Time_Stamp;

   ---------------
   -- Get_CRC32 --
   ---------------

   function Get_CRC32 (File : Binary_File) return Interfaces.Unsigned_32 is
   begin
      return File.CRC32;
   end Get_CRC32;

   ---------------
   -- Init_File --
   ---------------

   function Create_File
     (Fd : File_Descriptor; Filename : String_Access) return Binary_File is
   begin
      return Res : Binary_File := (Fd         => Fd,
                                   Filename   => Filename,
                                   File       => Invalid_Mapped_File,
                                   Region     => Invalid_Mapped_Region,
                                   Status     => Status_Ok,
                                   Size       => File_Length (Fd),
                                   Nbr_Sections => 0,
                                   Time_Stamp => File_Time_Stamp (Fd),
                                   CRC32      => 0)
      do
         Res.File := Open_Read (Filename.all);
         Res.CRC32 := Compute_CRC32 (Res);
      end return;
   end Create_File;

   ----------------
   -- Close_File --
   ----------------

   procedure Close_File (File : in out Binary_File) is
   begin
      Free (File.Region);
      Close (File.File);
      File.Fd := Invalid_FD;

      --  Note: File.Filename may be referenced later on to produce error
      --  messages, so we don't deallocate it.
   end Close_File;

   -------------------
   -- Compute_CRC32 --
   -------------------

   function Compute_CRC32 (File : Binary_File) return Unsigned_32 is
      C              : CRC32;
      Content        : Mapped_Region := Read (File.File);
      Content_Length : constant Integer := Integer (Length (File.File));
   begin
      Initialize (C);
      Update (C, String (Data (Content).all (1 .. Content_Length)));
      Free (Content);
      return Get_Value (C);
   end Compute_CRC32;

   ------------------
   -- Make_Mutable --
   ------------------

   procedure Make_Mutable
     (File : Binary_File; LS : in out Loaded_Section) is
   begin
      --  If the region is already mutable (this can happen, for instance, if
      --  it was byte-swapped), do not risk losing changes remapping it.

      case LS.Kind is
         when None =>
            raise Program_Error;

         when Mapped =>
            if not Is_Mutable (LS.Region) then
               Read
                 (File    => File.File,
                  Region  => LS.Region,
                  Offset  => Offset (LS.Region),
                  Length  => File_Size (Last (LS.Region)),
                  Mutable => True);
            end if;

         when Allocated =>
            null;
      end case;
   end Make_Mutable;

   ------------------------
   -- Get_Section_Length --
   ------------------------

   function Get_Section_Length
     (File : Binary_File;
      Index : Section_Index) return Arch.Arch_Addr is
   begin
      raise Program_Error;
      return 0;
   end Get_Section_Length;

   ------------------
   -- Load_Section --
   ------------------

   function Load_Section
     (File : Binary_File; Index : Section_Index) return Loaded_Section
   is
      pragma Unreferenced (File, Index);
   begin
      return raise Program_Error;
   end Load_Section;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (Content     : System.Address;
      First, Last : Arch.Arch_Addr) return Binary_Content
   is
   begin
      return (Content => Convert (Content),
              First   => First,
              Last    => Last);
   end Wrap;

   --------------
   -- Relocate --
   --------------

   procedure Relocate
     (Bin_Cont  : in out Binary_Content;
      New_First : Arch.Arch_Addr) is
   begin
      Bin_Cont.Last := New_First + Length (Bin_Cont) - 1;
      Bin_Cont.First := New_First;
   end Relocate;

   ------------
   -- Length --
   ------------

   function Length (Bin_Cont : Binary_Content) return Arch.Arch_Addr is
   begin
      if Bin_Cont.First > Bin_Cont.Last then
         return 0;
      else
         return Bin_Cont.Last - Bin_Cont.First + 1;
      end if;
   end Length;

   ---------------
   -- Is_Loaded --
   ---------------

   function Is_Loaded (Bin_Cont : Binary_Content) return Boolean is
   begin
      return Bin_Cont.Content /= null;
   end Is_Loaded;

   ---------
   -- Get --
   ---------

   function Get
     (Bin_Cont : Binary_Content;
      Offset : Arch.Arch_Addr) return Interfaces.Unsigned_8 is
   begin
      return Bin_Cont.Content (Offset - Bin_Cont.First);
   end Get;

   -----------
   -- Slice --
   -----------

   function Slice
     (Bin_Cont    : Binary_Content;
      First, Last : Arch.Arch_Addr) return Binary_Content
   is
      RFirst : constant Arch.Arch_Addr :=
        (if Bin_Cont.First <= First
         then First
         else raise Constraint_Error with "First out of bounds");
      RLast : constant Arch.Arch_Addr :=
        (if Bin_Cont.Last >= Last
         then Last
         else raise Constraint_Error with "Last out of bounds");
   begin
      return
        (Content => Convert (Address_Of (Bin_Cont, RFirst)),
         First   => RFirst,
         Last    => RLast);
   end Slice;

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of
     (Bin_Cont : Binary_Content;
      Offset   : Arch.Arch_Addr := 0) return System.Address is
   begin
      if Bin_Cont.Content = null then
         return System.Null_Address;
      else
         return Bin_Cont.Content (Offset - Bin_Cont.First)'Address;
      end if;
   end Address_Of;

   ----------------------
   -- Time_Stamp_Image --
   ----------------------

   function Time_Stamp_Image (TS : GNAT.OS_Lib.OS_Time) return String is

      function Pad (N, Length : Natural) return String;
      --  Pad the given number with zeros on the left until the given length of
      --  the image is reached.

      ---------
      -- Pad --
      ---------

      function Pad (N, Length : Natural) return String
      is
         Raw_Image   : constant String  := Natural'Image (N);
         First_Idx   : constant Natural :=
            (if Raw_Image (1) = ' ' then 2 else 1);
         Digits_Number : constant Natural := Raw_Image'Length - First_Idx + 1;
         Padding_Len : constant Natural :=
            (if Length > Digits_Number then Length - Digits_Number else 0);
         Padding     : constant String (1 .. Padding_Len) := (others => '0');

      begin
         return Padding & Raw_Image (First_Idx .. Raw_Image'Last);
      end Pad;

      Year   : Year_Type;
      Month  : Month_Type;
      Day    : Day_Type;
      Hour   : Hour_Type;
      Minute : Minute_Type;
      Second : Second_Type;
   begin
      GM_Split (TS, Year, Month, Day, Hour, Minute, Second);
      return
         Pad (Integer (Year), 0)
         & "-" & Pad (Natural (Month), 2)
         & "-" & Pad (Natural (Day), 2)
         & " " & Pad (Natural (Hour), 2)
         & ":" & Pad (Natural (Minute), 2)
         & ":" & Pad (Natural (Second), 2);
   end Time_Stamp_Image;

   ----------------------
   -- Time_Stamp_Value --
   ----------------------

   function Time_Stamp_Value (TS : String) return GNAT.OS_Lib.OS_Time is

      Matches : Match_Array (1 .. 6);

      function Match (Index : Positive) return String is
        (if Matches (Index) = No_Match
         then raise Program_Error
         else TS (Matches (Index).First
               .. Matches (Index).Last));

   begin
      Match (Time_Stamp_Matcher, TS, Matches);
      if Matches (1) = No_Match then
         raise Constraint_Error with "invalid time stamp: " & TS;
      end if;

      return GM_Time_Of
        (Year   => Year_Type'Value (Match (1)),
         Month  => Month_Type'Value (Match (2)),
         Day    => Day_Type'Value (Match (3)),
         Hour   => Hour_Type'Value (Match (4)) + 1,
         Minute => Minute_Type'Value (Match (5)),
         Second => Second_Type'Value (Match (6)));
   end Time_Stamp_Value;

   ----------------------
   -- Match_Signatures --
   ----------------------

   function Match_Signatures
     (S_File, S_Trace : Binary_File_Signature)
      return String
   is
      use Ada.Strings.Unbounded;
   begin
      if S_Trace.Size /= 0 and then S_File.Size /= S_Trace.Size then
         return
            ("Executable file is" & Long_Integer'Image (S_File.Size)
             & " bytes long, but trace indicates"
             & Long_Integer'Image (S_Trace.Size));

      elsif S_Trace.Time_Stamp /= Null_Unbounded_String
            and then S_File.Time_Stamp /= S_Trace.Time_Stamp
      then
         return
            ("Executable file created on "
             & To_String (S_File.Time_Stamp)
             & " but trace indicates "
             & To_String (S_Trace.Time_Stamp));

      elsif S_Trace.CRC32 /= 0 and then S_File.CRC32 /= S_Trace.CRC32 then
         return
            ("Executable file CRC32 checksum is 0x" & Hex_Image (S_File.CRC32)
             & " but trace indicates 0x" & Hex_Image (S_Trace.CRC32));

      else
         return "";
      end if;
   end Match_Signatures;

end Binary_Files;
