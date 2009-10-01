------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

--  Source locations
with Types;       use Types;

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
   function Image (First_Sloc, Last_Sloc : Source_Location) return String;
end Slocs;
