------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2015, AdaCore                     --
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

package Rundrv is

   function Real_Target (Target : String_Access) return String_Access;
   --  If Target is null, return the default target, otherwise return Target
   --  itself. Callers must *not* free the result.

   function Available_Targets return String;
   --  Return a list of available targets

   procedure Driver
     (Exe_File : String;
      Target   : String_Access;
      Tag      : String_Access;
      Output   : String_Access;
      Histmap  : String_Access;
      Kernel   : String_Access;
      Eargs    : String_List_Access);
   --  Run Exe_File on QEMU for Target (e.g. "powerpc-elf") with Output
   --  for trace output file; if Tag is not null, append it to the trace
   --  header. Add Eargs to QEMU's options.

end Rundrv;
