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

with Ada.Containers.Doubly_Linked_Lists;
with GNAT.Strings; use GNAT.Strings;
with Strings; use Strings;

package Inputs is
   --  This package provides a simple way to accumulate a set of
   --  command line inputs into a container, plus basic support for
   --  reading such inputs from a list in a file.

   procedure Read_List_From_File
     (File_Name : String;
      Process   : not null access procedure (Name : String));
   --  Read a list of names from a text file in the following format:
   --   * lines starting with '#' are ignored
   --   * one name per line
   --   * no blanks allowed.
   --
   --  For each name, call Process.
   --  May raise Name_Error or Status_Error if the corresponding text file
   --  cannot be opened.

   type Inputs_Type is private;
   --  Input lists. Can be used to accumulate the argument given on
   --  a command line.

   procedure Add_Input (Inputs : in out Inputs_Type; Name : String);
   --  If Name does not begin with '@', it is an input: add it to Inputs.
   --
   --  If Name begins with '@', it is a file name; this corresponding
   --  file constains a list of inputs to append to Inputs, in the format
   --  described by Read_List_From_File.
   --
   --  If Name begins with '@@', it is an input ('@' is escaped): add
   --  Name (Name'First + 1 .. Name'Last) to Inputs.

   procedure Iterate
     (Inputs  : in out Inputs_Type;
      Process : not null access procedure (Input : String));
   --  Go through the input list and call Process on each entry

   function Length (Inputs : Inputs_Type) return Ada.Containers.Count_Type;
   --  Return the number of elements in Inputs

private
   package Input_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => String_Access,
      "="          => Equal);

   type Inputs_Type is new Input_Lists.List with null record;

   pragma Inline (Length);

end Inputs;
