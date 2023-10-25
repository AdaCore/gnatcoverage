------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;

--  Helpers to implement support for the Ada sources.
--
--  Just like for the Ada unit provider (Instrument.Ada_Unit_Provider), we need
--  the Ada instrumenter to work without loading the GPR tree. To achieve this,
--  preprocessor configuration data is first extracted from project files, then
--  stored in a JSON file (Create_Preprocessor_Data_File below), which the Ada
--  instrumenter can then load (Create_Preprocessor below).

package Instrument.Ada_Preprocessing is

   procedure Create_Preprocessor_Data_File (Filename : String);
   --  Write in Filename a JSON representation of the preprocessor data
   --  extracted from the root project

   function Create_Preprocessor
     (Filename : String) return File_Reader_Reference;
   --  Load preprocessor data from a file written by
   --  Create_Preprocessor_Data_File and create a file reader to preprocess Ada
   --  sources according to this data.

end Instrument.Ada_Preprocessing;
