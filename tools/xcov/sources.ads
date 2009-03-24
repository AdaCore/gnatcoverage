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

private with Ada.Containers.Vectors;
private with Strings;

package Sources is

   --  Global directory of all source files

   type Any_Source_File_Index is new Natural;
   No_Source_File    : constant Any_Source_File_Index := 0;
   First_Source_File : constant Any_Source_File_Index := 1;

   subtype Source_File_Index is Any_Source_File_Index
     range First_Source_File .. Any_Source_File_Index'Last;

   function Get_Index (Name : String) return Source_File_Index;
   function Get_Name (Index : Source_File_Index) return String;

   --  A source location within the application

   type Source_Location is record
      Source_File : Source_File_Index;
      Line        : Natural;
      Column      : Natural;
   end record;

   function "<" (L, R : Source_Location) return Boolean;
   --  Order function used to build ordered maps keyed by source locations

private

   use Strings;

   package Filename_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Source_File_Index,
      Element_Type => String_Acc,
      "="          => Equal);

end Sources;
