------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

with GNAT.Strings; use GNAT.Strings;
with Ada.Text_IO;  use Ada.Text_IO;

package Traces_Dump is

   procedure Dump_Routines_Traces (Output_Name : String_Access);
   --  Go through the routine database and display their object coverage
   --  information along with an assembly dump. If OUTPUT_NAME is not null,
   --  create the corresponding file and dump the results there. Dump to
   --  the current default output stream otherwise.

   procedure Dump_Uncovered_Routines (Report : File_Access);
   --  Go through the routine database and dump the list of uncovered
   --  routines into Report.

end Traces_Dump;
