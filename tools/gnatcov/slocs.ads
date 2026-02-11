------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

--  Source locations

with SCOs;
with Types; use Types;

package Slocs is

   First_Source_File : constant Source_File_Index := 1;
   --  0 is No_Source_File

   -----------------------------
   -- Locations within a file --
   -----------------------------

   type Local_Source_Location is record
      Line   : Natural;
      Column : Natural;
   end record;

   function "<" (L, R : Local_Source_Location) return Boolean;
   function "<=" (L, R : Local_Source_Location) return Boolean;

   function Image (Sloc : Local_Source_Location) return String;
   function Value (Str : String) return Local_Source_Location;
   --  Attempt to interpret Str as a LINE:COL local source location string.
   --  Raise Constraint_Error if not possible.

   No_Local_Location : constant Local_Source_Location := (0, 0);

   function "+" (Self : Local_Source_Location) return SCOs.Source_Location
   is ((Logical_Line_Number (Self.Line), Column_Number (Self.Column)));

   type Local_Source_Location_Range is record
      First_Sloc, Last_Sloc : Local_Source_Location;
   end record;

   function "<" (L, R : Local_Source_Location_Range) return Boolean;
   --  Sort on lower bound first, then REVERSED on higher bound, so that
   --  for two nested ranges, the innermost one always sorts higher.

   function Image (SLOC : Local_Source_Location_Range) return String;
   --  Return a string in the form `(LL:CC -> LL:CC)`.

   function Contained_In (C, P : Local_Source_Location_Range) return Boolean;
   --  Return True if C(hild) bounds are contained in P(arent).
   --  The relation is reflexive : `Contained_In (X, X)` is always true.

   No_Local_Range : constant Local_Source_Location_Range :=
     (No_Local_Location, No_Local_Location);

   --------------------------------------------
   -- Locations across the whole application --
   --------------------------------------------

   type Source_Location is record
      Source_File : Source_File_Index;
      L           : Local_Source_Location;
   end record;

   function "<" (L, R : Source_Location) return Boolean;
   function "<=" (L, R : Source_Location) return Boolean;

   No_Location : constant Source_Location :=
     (No_Source_File, No_Local_Location);
   --  Note: No_Location must sort higher than any non-null location

   function Image
     (Sloc : Source_Location; Unique_Name : Boolean := False) return String;

   type Source_Location_Range is record
      Source_File : Source_File_Index;
      L           : Local_Source_Location_Range;
   end record;

   No_Range : constant Source_Location_Range :=
     (No_Source_File, No_Local_Range);

   function "<" (L, R : Source_Location_Range) return Boolean;

   function To_Sloc
     (Source_File : Source_File_Index; Local_Sloc : Local_Source_Location)
      return Source_Location;
   --  Return global location corresponding to the given source file and
   --  local location.
   --  Always return No_Location if Local_Sloc is No_Local_Location.

   function To_Range
     (First_Sloc, Last_Sloc : Source_Location) return Source_Location_Range;
   --  Return range from First_Sloc to Last_Sloc, which must be in the same
   --  source file, unless either is No_Location.

   function In_Range
     (Sloc : Source_Location; Sloc_Range : Source_Location_Range)
      return Boolean;
   --  True if Sloc is in Sloc_Range

   function First_Sloc (R : Source_Location_Range) return Source_Location;
   function Last_Sloc (R : Source_Location_Range) return Source_Location;

   function Image (Sloc_Range : Source_Location_Range) return String;

   type Source_Locations is array (Positive range <>) of Source_Location;

   function Clang_Predefined_File (Filename : String) return Boolean
   is (Filename = "<command line>" or else Filename = "<built-in>");
   --  Return whether this filename represents a command-line / built-in
   --  location.

end Slocs;
