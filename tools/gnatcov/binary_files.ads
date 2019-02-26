------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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

with Interfaces;
with System;

with GNAT.OS_Lib;   use GNAT.OS_Lib;
with GNATCOLL.Mmap; use GNATCOLL.Mmap;

with Arch;

package Binary_Files is

   --------------------
   -- Binary content --
   --------------------

   type Binary_Content_Bytes is
     array (Arch.Arch_Addr) of Interfaces.Unsigned_8;

   type Binary_Content_Bytes_Acc is access Binary_Content_Bytes;
   pragma No_Strict_Aliasing (Binary_Content_Bytes_Acc);

   type Binary_Content is record
      Content     : Binary_Content_Bytes_Acc;
      First, Last : Arch.Arch_Addr;
      --  Content is an unconstrained array, so we can set it to some memory
      --  mapped content. Thus, we have to store bounds ourselves.
   end record;
   --  An array of byte, used to store ELF sections

   Invalid_Binary_Content : constant Binary_Content :=
     (null, 0, 0);

   function Wrap
     (Content     : System.Address;
      First, Last : Arch.Arch_Addr) return Binary_Content
     with Inline;
   --  Constructor for Binary_Content

   procedure Relocate
     (Bin_Cont  : in out Binary_Content;
      New_First : Arch.Arch_Addr)
     with Inline;
   --  Update First and Last in Bin_Cont so that First is New_First

   function Length (Bin_Cont : Binary_Content) return Arch.Arch_Addr
     with Inline;
   --  Return the number of bytes in Bin_Cont

   function Is_Loaded (Bin_Cont : Binary_Content) return Boolean
     with Inline;
   --  Return whether Bin_Cont actually contains something

   function Get
     (Bin_Cont : Binary_Content;
      Offset : Arch.Arch_Addr) return Interfaces.Unsigned_8
     with Inline;
   --  Return the byte in Bin_Cont at Offset

   function Slice
     (Bin_Cont    : Binary_Content;
      First, Last : Arch.Arch_Addr) return Binary_Content
     with Inline;
   --  Return a new Binary_Content value referencing the slice of bytes in
   --  Bin_Cont from First to Last (no copy is done).

   function Address_Of
     (Bin_Cont : Binary_Content;
      Offset   : Arch.Arch_Addr := 0) return System.Address
     with Inline;
   --  Return the address of the Offset'th item in the binary content

   ---------------------
   -- Loaded Sections --
   ---------------------

   type Loaded_Section is private;
   --  Holder for a loaded section. Once created, it must be released with the
   --  corresponding Free function.

   No_Loaded_Section : constant Loaded_Section;

   function Allocate (Size : Arch.Arch_Addr) return Loaded_Section
     with Inline;
   --  Allocate a new section content for the given size

   function "+" (Region : Mapped_Region) return Loaded_Section
     with Inline;
   --  Wrap a memory mapped file region into a loaded section

   function Size (LS : Loaded_Section) return Arch.Arch_Addr with
     Inline,
     Pre => LS /= No_Loaded_Section;
   --  Return the size of the section in LS

   function Content (LS : Loaded_Section) return Binary_Content with
     Inline,
     Pre => LS /= No_Loaded_Section;
   --  Return the content of the given loaded section

   function Address_Of
     (LS     : Loaded_Section;
      Offset : Arch.Arch_Addr := 0) return System.Address with
     Inline,
     Pre => LS /= No_Loaded_Section;
   --  Return the address of the Offset'th item in the binary content

   procedure Free (LS : in out Loaded_Section);
   --  Release resources allocated to LS and set it to No_Leaded_Section

   -------------------
   --  Binary Files --
   -------------------

   Error : exception;

   type Binary_File_Status is
     (
      --  No error.
      Status_Ok,

      --  Cannot open file.
      Status_Open_Failure,

      Status_Bad_File,
      Status_Memory,
      Status_Read_Error,
      Status_Bad_Magic,
      Status_Bad_Class,
      Status_Bad_Version
      );

   type Binary_File is tagged private;

   --  Direct accessors
   function Fd (F : Binary_File) return File_Descriptor;
   function File (F : Binary_File) return Mapped_File;
   function File_Region (F : Binary_File) return Mapped_Region;
   function Filename (F : Binary_File) return String;

   function Get_Status (File : Binary_File) return Binary_File_Status;
   --  Return status of previous operation.

   procedure Set_File_Region (F : in out Binary_File; R : Mapped_Region);

   procedure Set_Status
     (File : in out Binary_File; Status : Binary_File_Status);

   function Get_Size (File : Binary_File) return Long_Integer;
   --  Get the size of the file

   function Get_Time_Stamp (File : Binary_File) return OS_Time;
   --  Get the time stamp of the file

   function Get_CRC32 (File : Binary_File) return Interfaces.Unsigned_32;
   --  Get the CRC32 checksum of the content of the file

   function Create_File
     (Fd : File_Descriptor; Filename : String_Access) return Binary_File;
   --  Create a Binary_File from Fd and Filename.  Note that Filename is not
   --  copied.

   procedure Close_File (File : in out Binary_File);

   procedure Make_Mutable (File : Binary_File; LS : in out Loaded_Section)
     with Pre => LS /= No_Loaded_Section;
   --  If Loaded_Section is a read-only mapped region, reload it as a mutable
   --  one.

   type Section_Index is new Natural;
   No_Section : constant Section_Index := Section_Index'Last;

   function Get_Nbr_Sections (File : Binary_File) return Section_Index;
   procedure Set_Nbr_Sections (File : in out Binary_File; Nbr : Section_Index);

   function Get_Section_Length
     (File : Binary_File; Index : Section_Index) return Arch.Arch_Addr;

   function Load_Section
     (File : Binary_File; Index : Section_Index) return Loaded_Section;

   --  We wish we could expose memory mapped files as unconstrained arrays,
   --  but it's not possible in Ada. So instead we use the following types
   --  and primitives to emulate this.

   type Binary_File_Signature is record
      Size       : Long_Integer := 0;
      --  File size (in bytes), or 0 if unknown.

      Time_Stamp : OS_Time := Invalid_Time;
      --  File modification time, or Invalid_Time if not known

      CRC32      : Interfaces.Unsigned_32 := 0;
      --  CRC32 checksum for the file content or 0 if unknown.
   end record;
   --  Container for several binary file features like file size. Used to
   --  perform consistency checks.

   No_Signature : constant Binary_File_Signature := (others => <>);

   function Time_Stamp_Image (TS : OS_Time) return String;
   --  Return a representation of a timestamp as a string containing
   --  a broken-down (year/month/day/hour/minute/second) value in UTC.

   function Time_Stamp_Value (S : String) return OS_Time;
   --  Reciprocal function of Time_Stamp_Image: parse broken down UTC time
   --  and return the corresponding OS_Time.

   function Get_Signature (File : Binary_File) return Binary_File_Signature is
     ((Size       => Get_Size (File),
       Time_Stamp => Get_Time_Stamp (File),
       CRC32      => Get_CRC32 (File)));

   function Match_Signatures
     (S_File, S_Trace : Binary_File_Signature)
      return String;
   --  If the two input signatures (one for an executable file, the other from
   --  a trace file) do not match, return an non-empty string telling why.
   --  Return an empty string otherwise.

private

   type Binary_File is tagged record
      Filename         : String_Access;
      --  Name of the file

      Fd               : File_Descriptor;
      File             : Mapped_File;
      Region           : Mapped_Region;
      --  Access the ELF content. FD is open first, then File is open using FD,
      --  and Region maps the whole content of File.

      Status           : Binary_File_Status;
      --  Status, used to report errors.

      --  A few characteristics for this file. They will be saved here as soon
      --  as the file is open, since the ELF might be closed when they are
      --  requested.

      Nbr_Sections     : Section_Index;
      --  Number of sections.  This is the index of the last section + 1.

      Size             : Long_Integer;
      Time_Stamp       : GNAT.OS_Lib.OS_Time;
      CRC32            : Interfaces.Unsigned_32;
   end record;

   type Loaded_Section_Kind is (None, Mapped, Allocated);

   type Loaded_Section (Kind : Loaded_Section_Kind := None) is record
      case Kind is
         when None =>
            null;
         when Mapped =>
            Region : Mapped_Region;
         when Allocated =>
            Buffer : String_Access;
      end case;
   end record;

   No_Loaded_Section : constant Loaded_Section := (Kind => None);

end Binary_Files;
