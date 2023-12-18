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

--  Stub of Instrument.Ada_Preprocessing, to avoid pulling a dependency to
--  libadalang when gnatcov is not built with Ada instrumentation support
--  (basically the gnatcov32 executable that has support for binary traces
--  only).

package Instrument.Ada_Preprocessing is

   pragma Elaborate_Body;

   type File_Reader_Reference is null record;

   procedure Create_Preprocessor_Data_File (Filename : String) is null;

   function Create_Preprocessor
     (Filename : String) return File_Reader_Reference;

end Instrument.Ada_Preprocessing;
