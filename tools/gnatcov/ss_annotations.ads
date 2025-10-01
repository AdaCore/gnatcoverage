------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
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

--  General interfacing with Stable_Sloc for external annotations

with Stable_Sloc;

with Command_Line;
with Instrument.Common;
with Logging;
with Slocs;
with Strings;        use Strings;
with SC_Obligations; use SC_Obligations;
with Types;          use Types;

package SS_Annotations is

   Ext_Annotation_Trace : constant Logging.GNATCOLL_Trace :=
     Logging.Create_Trace ("EXT_ANNOTATION");
   --  Trace for external annotation handling

   Ext_Annotation_DB : Stable_Sloc.Entry_DB;
   --  Database of external annotations entries.

   --  Stable_Sloc sloc type to our sloc type utilities

   function "+" (S : Stable_Sloc.Sloc) return Slocs.Local_Source_Location
   is (Line => S.Line, Column => S.Column);

   function "+" (S : Slocs.Local_Source_Location) return Stable_Sloc.Sloc
   is (Line => S.Line, Column => S.Column);

   function "+"
     (S : Stable_Sloc.Sloc_Span) return Slocs.Local_Source_Location_Range
   is (First_Sloc => +S.Start_Sloc, Last_Sloc => +S.End_Sloc);

   function To_Sloc
     (S : Stable_Sloc.Sloc; FI : Source_File_Index)
      return Slocs.Source_Location
   is (Source_File => FI, L => +S);

   function To_Sloc_Range
     (SR : Stable_Sloc.Sloc_Span; FI : Source_File_Index)
      return Slocs.Source_Location_Range
   is (Source_File => FI, L => +SR);

   procedure Load_Ext_Annotations (Annotation_File : Unbounded_String);
   --  Load the annotations in Annotation_File into our internal annotation
   --  database.

   procedure Validate_Annotations;
   --  Iterate over the loaded entries, and validate the TOML annotations.
   --  Warn about the invalid annotations.

   procedure Import_External_Exemptions
     (FI : Source_File_Index; Filter : Boolean := False);
   --  Search for external exemptions in FI, from the annotations loaded in
   --  Ext_Annotation_DB.
   --
   --  If Filter is True, reject annotations that lie within a statement SCO.

   function Get_Buffer_Annotations
     (Filename : String) return Instrument.Common.Instr_Annotation_Map;
   --  Get the buffer annotations relevant to instrumentation for Filename.

   function Get_Disabled_Cov_Annotations
     (Filename : String) return Instrument.Common.Instr_Annotation_Map;
   --  Return the map of annotations that disable coverage regions for
   --  Filename.
   --
   --  The resulting map already filters out annotations that do not come in
   --  pairs: any Cov_Off annotation immediately following a Cov_Off annotation
   --  will be discarded, and likewise for Cov_On annotations.
   --
   --  The map is also guaranteed to start with a Cov_Off annotation, and end
   --  on a Cov_On annotation, so that regions can always be determined from
   --  two subsequent annotations.
   --
   --  Finally, the resulting annotations are guaranteed not to conflict with
   --  any pre-existing annotations for Filename.

   procedure Add_Annotation (Args : Command_Line.Parser.Parsed_Arguments);
   --  Generate the annotation corresponding to the given Args

   procedure Delete_Annotation (Args : Command_Line.Parser.Parsed_Arguments);
   --  Delete the annotations corresponding to the given Args

   procedure Show_Annotations (Args : Command_Line.Parser.Parsed_Arguments);
   --  List the entries and corresponding matches from the options in Args

end SS_Annotations;
