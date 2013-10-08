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

--  Source locations

with Types; use Types;

package Slocs is

   First_Source_File : constant Source_File_Index := 1;
   --  0 is No_Source_File

   -----------------------------
   -- Locations within a file --
   -----------------------------

   type Local_Source_Location is record
      Line        : Natural;
      Column      : Natural;
   end record;

   function "<"  (L, R : Local_Source_Location) return Boolean;
   function "<=" (L, R : Local_Source_Location) return Boolean;

   No_Local_Location : constant Local_Source_Location := (0, 0);

   type Local_Source_Location_Range is record
      First_Sloc, Last_Sloc : Local_Source_Location;
   end record;

   function "<" (L, R : Local_Source_Location_Range) return Boolean;

   No_Local_Range : constant Local_Source_Location_Range :=
                      (No_Local_Location, No_Local_Location);

   --------------------------------------------
   -- Locations across the whole application --
   --------------------------------------------

   type Source_Location is record
      Source_File : Source_File_Index;
      L           : Local_Source_Location;
   end record;

   function "<"  (L, R : Source_Location) return Boolean;
   function "<=" (L, R : Source_Location) return Boolean;

   No_Location       : constant Source_Location :=
                                  (No_Source_File, No_Local_Location);
   --  Note: No_Location must sort higher than any non-null location

   function Image (Sloc : Source_Location) return String;

   type Source_Location_Range is record
      Source_File : Source_File_Index;
      R           : Local_Source_Location_Range;
   end record;

   No_Range : constant Source_Location_Range :=
                (No_Source_File, No_Local_Range);

   function "<" (L, R : Source_Location_Range) return Boolean;
   --  Lexicographic order

   function To_Sloc
     (Source_File : Source_File_Index;
      Local_Sloc  : Local_Source_Location) return Source_Location;
   --  Return global location corresponding to the given source file and
   --  local location.
   --  Always return No_Location if Local_Sloc is No_Local_Location.

   function To_Range
     (First_Sloc, Last_Sloc : Source_Location) return Source_Location_Range;
   --  Return range from First_Sloc to Last_Sloc, which must be in the same
   --  source file, unless either is No_Location.

   function First_Sloc (R : Source_Location_Range) return Source_Location;
   function Last_Sloc (R : Source_Location_Range) return Source_Location;

   function Image (Sloc_Range : Source_Location_Range) return String;

   type Source_Locations is array (Positive range <>) of Source_Location;

end Slocs;
