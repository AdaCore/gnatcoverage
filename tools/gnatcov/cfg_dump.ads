------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
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

with Inputs;
with Object_Locations; use Object_Locations;
with Slocs;            use Slocs;

package CFG_Dump is

   type Output_Format is (None, Dot, SVG, PDF, PNG);
   --  Format used to output the CFG. None stands for "output the Dot graph
   --  without passing it to dot(1)".

   procedure Dump (Exec_Path         : String;
                   Locations         : User_Locations;
                   Output            : String_Access;
                   Format            : Output_Format;
                   SCO_Files_List    : Inputs.Inputs_Type;
                   Traces_Files_List : Inputs.Inputs_Type;
                   Keep_Edges        : Boolean);

end CFG_Dump;
