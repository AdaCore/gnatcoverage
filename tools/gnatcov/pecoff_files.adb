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

   ----------------
   -- Is_PE_File --
   ----------------

   function Is_PE_File (Fd : File_Descriptor) return Boolean
   is
      MS_Hdr : PEHdr;
      PE_Sig : Unsigned_32;
   begin
      Lseek (Fd, 0, Seek_Set);

      if Read (Fd, MS_Hdr'Address, PEHdrsz) /= PEHdrsz then
         return False;
      end if;

      --  Only handle little endian
      if MS_Hdr.E_MZHdr /= MZhdr then
         return False;
      end if;

      Lseek (Fd, Long_Integer (MS_Hdr.E_Lfanew), Seek_Set);

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
   begin
      return File : PE_File := (Binary_File'(Create_File (Fd, Filename))
                                with others => <>) do
         if Read (Fd, File.Hdr'Address, Filehdr_Size) /= Filehdr_Size then
            Exit_With_Error
              (File, Status_Read_Error, "failed to read COFF header");
         end if;
      end return;
   end Create_File;

end PECoff_Files;
