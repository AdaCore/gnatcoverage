------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2015, AdaCore                       --
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

with Interfaces; use Interfaces;

package body PECoff_Files is

   function Read_Coff_Header_Offset (Fd : File_Descriptor) return Long_Integer;
   --  Read the offset of the PE signature (4 bytes before the COFF header).
   --  Returns 0 in case of error (bad magic number).

   -----------------------------
   -- Read_Coff_Header_Offset --
   -----------------------------

   function Read_Coff_Header_Offset (Fd : File_Descriptor) return Long_Integer
   is
      MS_Hdr : PEHdr;
   begin
      Lseek (Fd, 0, Seek_Set);

      if Read (Fd, MS_Hdr'Address, PEHdrsz) /= PEHdrsz then
         return 0;
      end if;

      --  Only handle little endian
      if MS_Hdr.E_MZHdr /= MZhdr then
         return 0;
      end if;

      return Long_Integer (MS_Hdr.E_Lfanew);
   end Read_Coff_Header_Offset;

   ----------------
   -- Is_PE_File --
   ----------------

   function Is_PE_File (Fd : File_Descriptor) return Boolean
   is
      Off    : Long_Integer;
      PE_Sig : Unsigned_32;
   begin
      Lseek (Fd, 0, Seek_Set);
      Off := Read_Coff_Header_Offset (Fd);

      if Off = 0 then
         return False;
      end if;

      Lseek (Fd, Off, Seek_Set);

      if Read (Fd, PE_Sig'Address, 4) /= 4 then
         return False;
      end if;

      return PE_Sig = Pe_Magic;
   end Is_PE_File;

   function Create_File
     (Fd : File_Descriptor; Filename : String_Access) return PE_File is
      procedure Exit_With_Error
        (File : in out PE_File; Status : Binary_File_Status; Msg : String);
      --  Assign Status to File, close the file if needed and raise Error with
      --  the filename and Msg.

      ---------------------
      -- Exit_With_Error --
      ---------------------

      procedure Exit_With_Error
        (File : in out PE_File; Status : Binary_File_Status; Msg : String) is
      begin
         Set_Status (File, Status);
         Close_File (File);
         raise Error with File.Filename & ": " & Msg;
      end Exit_With_Error;

      Hdr_Off : Long_Integer;
   begin
      Hdr_Off := Read_Coff_Header_Offset (Fd);
      pragma Assert (Hdr_Off > 0);

      return File : PE_File := (Binary_File'(Create_File (Fd, Filename))
                                with others => <>) do
         Lseek (Fd, Hdr_Off + 4, Seek_Set);
         if Read (Fd, File.Hdr'Address, Filehdr_Size) /= Filehdr_Size then
            Exit_With_Error
              (File, Status_Read_Error, "failed to read COFF header");
         end if;

         Set_Nbr_Sections (File, Section_Index (File.Hdr.F_Nscns));

         --  Map sections.
         declare
            Size : constant File_Size :=
              File_Size (File.Hdr.F_Nscns) * File_Size (Scnhdr_Size);
            Scn_Off : constant File_Size :=
              File_Size (Hdr_Off + 4) + File_Size (File.Hdr.F_Opthdr)
              + File_Size (Filehdr_Size);
         begin
            File.Scn_Map := Read
              (File    => File.File,
               Offset  => Scn_Off,
               Length  => Size,
               Mutable => False);
            File.Scn := To_PE_Scn_Arr_Acc (Data (File.Scn_Map).all'Address);

            if File_Size (GNATCOLL.Mmap.Last (File.Scn_Map)) /= Size then
               raise Error;
            end if;
         end;

      end return;
   end Create_File;

   -------------
   -- Get_Hdr --
   -------------

   function Get_Hdr (File : PE_File) return Filehdr is
   begin
      return File.Hdr;
   end Get_Hdr;

   ----------------------
   -- Get_Section_Name --
   ----------------------

   function Get_Section_Name (File : PE_File; Sec : Section_Index)
                             return String
   is
      pragma Assert (Sec < Section_Index (File.Hdr.F_Nscns));
      S : Scnhdr renames File.Scn (Sec);
   begin
      for I in S.S_Name'Range loop
         if S.S_Name (I) = ASCII.NUL then
            return S.S_Name (1 .. I - 1);
         end if;
      end loop;
      return S.S_Name;
   end Get_Section_Name;

   ----------------
   -- Get_Scnhdr --
   ----------------

   function Get_Scnhdr (File : PE_File; Sec : Section_Index) return Scnhdr is
   begin
      pragma Assert (Sec < Section_Index (File.Hdr.F_Nscns));
      return File.Scn (Sec);
   end Get_Scnhdr;
end PECoff_Files;
