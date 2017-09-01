------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Annotations.Xml; use Annotations.Xml;
with Command_Line;
with Coverage;

package Annotations.Html is

   procedure Generate_Report
     (Context      : Coverage.Context_Access;
      Show_Details : Boolean;
      Report_Title : Command_Line.Parser.String_Option);
   --  Annotate the source file in HTML with line states, embedding
   --  justifications on each non-fully-covered line if Show_Details is True.
   --  If Report_Title is present, use its value as the title for the generated
   --  HTML documents.

   function Title_Prefix
     (Report_Title : Command_Line.Parser.String_Option)
      return Unbounded_String
   is ((if Report_Title.Present and then Length (Report_Title.Value) > 0
        then To_Unbounded_String
          (To_Xml_String (To_String (Report_Title.Value & " - ")))
        else Null_Unbounded_String));
   --  If Report_Title is present and non-empty, return the corresponding
   --  report title prefix (for instance: "foo" -> "foo - "). Return an empty
   --  string otherwise.

end Annotations.Html;
