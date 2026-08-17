------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2026, AdaCore                       --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Files_Handling; use Files_Handling;
with Strings;        use Strings;

package Shared_Lib_Deps is

   procedure Dump_Shared_Libs
     (Exe_Inputs : String_Vectors.Vector; Output : String_Access);
   --  Compute the shared library dependency closure for the given executable
   --  files, and output them (one absolute filename per line, to the standard
   --  output if Output is null, or to the given filename Output).

   function Imported_Shared_Libs (Filename : String) return File_Sets.Set;
   --  Return the set of shared libraries in the dependency closure of the
   --  executable Filename.

   procedure Warn_Not_Found (Filename : String);
   --  Emit a warning because Filename (a shared library) could not be found

end Shared_Lib_Deps;
