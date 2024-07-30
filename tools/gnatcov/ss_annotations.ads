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

   function "+" (S : Stable_Sloc.Sloc) return Slocs.Local_Source_Location is
     (Line   => S.Line,
      Column => S.Column);

   function "+"
     (S : Stable_Sloc.Sloc_Span) return Slocs.Local_Source_Location_Range is
     (First_Sloc => +S.Start_Sloc,
      Last_Sloc  => +S.End_Sloc);

   function To_Sloc
     (S  : Stable_Sloc.Sloc;
      FI : Source_File_Index) return Slocs.Source_Location is
      (Source_File => FI, L => +S);

   function To_Sloc_Range
     (SR : Stable_Sloc.Sloc_Span;
      FI : Source_File_Index) return Slocs.Source_Location_Range is
      (Source_File => FI, L => +SR);

   procedure Load_Ext_Annotations (Annotation_File : Unbounded_String);
   --  Load the annotations in Annotation_File into our internal annotation
   --  database.

   procedure Import_External_Exemptions
     (FI : Source_File_Index);
   --  Search for external exemptions in FI, from the annotations loaded in
   --  Ext_Annotation_DB.
   --
   --  The newly added annotations will include CU in the corresponding CU
   --  field.

   function Get_Buffer_Annotations
     (Filename : String) return Instrument.Common.Instr_Annotation_Map;
   --  Get the buffer annotations relevant to instrumentation for Filename.

end SS_Annotations;
