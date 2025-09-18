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

with GNAT.Regexp;

with Strings; use Strings;

package Inputs is

   use all type Unbounded_String;

   --  This package provides a simple way to accumulate a set of
   --  command line inputs into a container, plus basic support for
   --  reading such inputs from a list in a file.

   procedure Read_List_From_File
     (File_Name : String; Process : not null access procedure (Name : String));
   --  Read a list of names from a text file in the following format:
   --   * lines starting with '#' are ignored
   --   * one name per line
   --   * no blanks allowed.
   --
   --  For each name, call Process.
   --  May raise Name_Error or Status_Error if the corresponding text file
   --  cannot be opened.

   procedure Log_File_Open (File_Name : String);
   --  When in verbose mode, add a debug message indicating that File_Name is
   --  open. Try to include the CRC32 checksum.

   procedure Create_Matcher
     (Pattern_List     : String_Vectors.Vector;
      Matcher          : out GNAT.Regexp.Regexp;
      Has_Matcher      : out Boolean;
      Case_Insensitive : Boolean := False);
   --  If Pattern_List is empty, leave Matcher uninitialized and set
   --  Has_Matcher to False. Otherwise, set it to True and put in Matcher a
   --  pattern matching each of the globbing patterns in Pattern_List.
   --
   --  If Case_Insensitive is True, the returned matcher is made case
   --  insensitive. Note that it may be case insensitive even if
   --  Case_Insensitive is False (for instance on Windows, where all glob
   --  patterns are interpreted as case insensitive).

end Inputs;
