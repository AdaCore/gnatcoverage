------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2022-2024, AdaCore                     --
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

--  This package provides helpers to deal with JSON file I/O

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.VFS;  use GNATCOLL.VFS;

package JSON is

   procedure Write
     (Filename : String; Value : JSON_Value; Compact : Boolean := True);
   --  Serialize Value and write it to the Filename text file.
   --
   --  Compact is passed to GNATCOLL.JSON.Write to control the formatting.

   function Read (Filename : String) return Read_Result;
   --  Read and parse the JSON document in the Filename text file

   function Read (File : Virtual_File) return Read_Result;
   --  Likewise, but starting from a Virtual_File

end JSON;
