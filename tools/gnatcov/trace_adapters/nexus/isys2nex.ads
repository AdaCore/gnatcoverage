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

--  iSystem's winIdea software, which is used to control iSystem On Chip
--  Debugging (OCD) boxes, can be directed to gather OCD realtime program
--  trace data and record it to a file. The function Ocdfile_To_Nexus_List
--  takes the name of an OCD file created by winIdea, containing Nexus
--  program trace data (the OCD trace format used by Freescale PowerPC
--  SOCs, and others) and returns a structure representing a list of
--  Nexus messages.

with Nexus_Rep;  use Nexus_Rep;

package Isys2nex is
   Isys2nex_Error : exception;

   function Ocdfile_To_Nexus_List (OCD_Filename : String)
                                   return Nexus_Message_List_T;
   --  If there are any problems with the file or it's contents, the
   --  Isys2nex_Error is raised with a message that provides some detail.

end Isys2nex;
