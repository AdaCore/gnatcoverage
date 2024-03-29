------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2013-2024, AdaCore                     --
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

with Command_Line;
with Coverage;

package Annotations.Dynamic_Html is

   function Installed return Boolean;
   --  Return TRUE if the Dynamic HTML plugin is installed, i.e. the required
   --  resource files are installed in lib/gnatcoverage/.

   procedure Generate_Report
     (Context      : Coverage.Context_Access;
      Report_Title : Command_Line.Parser.String_Option);
   --  Generate the Dynamic HTML report providing the plugin is correctly
   --  installed.

end Annotations.Dynamic_Html;
