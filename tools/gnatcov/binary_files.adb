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

with GNAT.CRC32; use GNAT.CRC32;
with Interfaces; use Interfaces;

package body Binary_Files is
   function Compute_CRC32 (File : Binary_File) return Unsigned_32;
   --  Compute and return the CRC32 of File

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

   --------------
   -- Filename --
   --------------

   function Filename (F : Binary_File) return String is
   begin
      return F.Filename.all;
   end Filename;

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
                                   Status     => Status_Ok,
                                   Size       => File_Length (Fd),
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

end Binary_Files;
