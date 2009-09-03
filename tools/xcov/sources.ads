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

with Ada.Text_IO; use Ada.Text_IO;
with Types;       use Types;

package Sources is

   First_Source_File : constant Source_File_Index := 1;
   --  0 is No_Source_File

   --  Global directory of all source files

   function Get_Index (Name : String) return Source_File_Index;
   function Get_Name (Index : Source_File_Index) return String;

   --  Utilities to open files from the source file table. Source
   --  files will be searched on the local filesystem, in the following
   --  order:
   --  (1) from xcov's execution directory;
   --  (2) after rebasing them using the rebase list;
   --  (3) from the source search path.

   procedure Add_Source_Rebase (Old_Prefix : String; New_Prefix : String);
   --  Add a new entry to the rebase list.  This entry says that a file
   --  whose name is Old_Prefix & "something" should be found in
   --  New_Prefix & "something".

   procedure Add_Source_Search (Prefix : String);
   --  Add Prefix to the source search path. A file named "something" would
   --  be looked for in Prefix & "something".

   procedure Open
     (File    : in out File_Type;
      Index   : Source_File_Index;
      Success : out Boolean);
   --  Try to open the file from the source file table whose index is Index,
   --  using the rebase/search information. If one found, Success is True;
   --  False otherwise.

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

private

   use Strings;

   subtype Valid_Source_File_Index is
     Source_File_Index range First_Source_File .. Source_File_Index'Last;
   package Filename_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_Source_File_Index,
      Element_Type => String_Acc,
      "="          => Equal);
end Sources;
