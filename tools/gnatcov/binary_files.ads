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
with Interfaces;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNATCOLL.Mmap; use GNATCOLL.Mmap;

package Binary_Files is
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
   function Filename (F : Binary_File) return String;

   function Get_Status (File : Binary_File) return Binary_File_Status;
   --  Return status of previous operation.

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
private

   type Binary_File is tagged record
      Filename         : String_Access;
      --  Name of the file

      Fd               : File_Descriptor;
      File             : Mapped_File;
      --  Access the ELF content. FD is open first, then File is open using FD.

      Status           : Binary_File_Status;
      --  Status, used to report errors.

      --  A few characteristics for this file. They will be saved here as soon
      --  as the file is open, since the ELF might be closed when they are
      --  requested.

      Size             : Long_Integer;
      Time_Stamp       : GNAT.OS_Lib.OS_Time;
      CRC32            : Interfaces.Unsigned_32;
   end record;
end Binary_Files;
