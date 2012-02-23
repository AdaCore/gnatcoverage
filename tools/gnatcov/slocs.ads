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

with Ada.Containers.Ordered_Sets;

with Types; use Types;

package Slocs is

   First_Source_File : constant Source_File_Index := 1;
   --  0 is No_Source_File

   --  A source location within the application

   type Source_Location is record
      Source_File : Source_File_Index;
      Line        : Natural;
      Column      : Natural;
   end record;

   No_Location : constant Source_Location := (No_Source_File, 0, 0);

   function "<" (L, R : Source_Location) return Boolean;
   function "<=" (L, R : Source_Location) return Boolean;
   --  Order function used to build ordered maps keyed by source locations.
   --  No_Location sorts higher than any specific location.

   function Image (Sloc : Source_Location) return String;

   type Source_Location_Range is record
      First_Sloc, Last_Sloc : Source_Location;
   end record;

   No_Range : constant Source_Location_Range := (No_Location, No_Location);

   function "<" (L, R : Source_Location_Range) return Boolean;
   --  Lexicographic order

   function Image (Sloc_Range : Source_Location_Range) return String;

   type Source_Locations is array (Positive range <>) of Source_Location;

   package Sloc_Sets is new Ada.Containers.Ordered_Sets (Source_Location);

end Slocs;
