------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Strings; use Strings;

package Files_Handling is

   function Executable_Suffix return String;
   --  Wrapper around GNAT.OS_Lib.Get_Executable_Suffix to return the string
   --  by-value.

   function Create_Normalized (Filename : String) return Virtual_File;
   --  Wrapper around GNATCOLL.VFS.Create_From_Base, that normalizes by
   --  default the Virtual_File.

   function Full_Name (File : Virtual_File) return Unbounded_String;
   --  Wrapper around GNATCOLL.VFS.Full_Name, converting the Filesystem_String
   --  to an Unbounded_String.

   package File_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Virtual_File,
      "="          => GNATCOLL.VFS."=");

   package File_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Virtual_File,
      "<"          => GNATCOLL.VFS."<",
      "="          => GNATCOLL.VFS."=");

   package File_To_String_Vectors_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Virtual_File,
      Element_Type => String_Vectors.Vector,
      "<"          => GNATCOLL.VFS."<",
      "="          => String_Vectors."=");

   package File_To_File_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Virtual_File,
      Element_Type => Virtual_File,
      "<"          => GNATCOLL.VFS."<",
      "="          => GNATCOLL.VFS."=");

   package File_To_String_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Virtual_File,
      Element_Type => Unbounded_String,
      "<"          => GNATCOLL.VFS."<",
      "="          => Ada.Strings.Unbounded."=");

end Files_Handling;
