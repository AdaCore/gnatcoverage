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

with System;
with Ada.Unchecked_Conversion;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNATCOLL.Mmap;
with Coff; use Coff;
with Binary_Files; use Binary_Files;

package PECoff_Files is
   type PE_File is new Binary_File with private;

   function Is_PE_File (Fd : File_Descriptor) return Boolean;
   --  Return True if FD is a PE-COFF file

   --  Open a binary file
   function Create_File
     (Fd : File_Descriptor; Filename : String_Access) return PE_File;

private
   use GNATCOLL.Mmap;

   type PE_Scn_Arr is array (Section_Index) of Scnhdr;
   type PE_Scn_Arr_Acc is access PE_Scn_Arr;

   function To_PE_Scn_Arr_Acc is new Ada.Unchecked_Conversion
     (System.Address, PE_Scn_Arr_Acc);

   type PE_File is new Binary_File with record
      Hdr : Filehdr;

      Scn_Map : Mapped_Region;
      Scn     : PE_Scn_Arr_Acc;
   end record;

end PECoff_Files;
