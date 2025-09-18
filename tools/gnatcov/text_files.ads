------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
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

--  Convenience wrapper around Ada.Text_IO.

private with Ada.Finalization;
with Ada.Text_IO;

with Strings; use Strings;

package Text_Files is

   subtype File_Mode is Ada.Text_IO.File_Mode;

   type File_Type is tagged limited private;
   --  Reference to a text file. On destruction, the file is automatically
   --  closed if open.

   function Is_Open (Self : File_Type) return Boolean
   with Inline;
   --  Return whether Self references an open file. By default, files are
   --  closed.

   function Mode (Self : File_Type) return File_Mode
   with Pre => Self.Is_Open, Inline;
   --  Return the opening mode for Self. See the Open primitive.

   function Open
     (Self : in out File_Type;
      Name : String;
      Mode : File_Mode := Ada.Text_IO.In_File) return Boolean
   with Pre => not Self.Is_Open, Post => Open'Result = Self.Is_Open, Inline;
   --  Try to open Name in the given mode. Return whether it was successful.

   function Create
     (Self : in out File_Type;
      Name : String;
      Mode : File_Mode := Ada.Text_IO.Out_File) return Boolean
   with Pre => not Self.Is_Open, Post => Create'Result = Self.Is_Open, Inline;
   --  Try to open Name in the given mode. Return whether it was successful.

   procedure Open
     (Self : in out File_Type;
      Name : String;
      Mode : File_Mode := Ada.Text_IO.In_File)
   with Pre => not Self.Is_Open, Inline;
   --  Try to open Name in the given mode. If unsuccessful, emit a fatal error.

   procedure Create
     (Self : in out File_Type;
      Name : String;
      Mode : File_Mode := Ada.Text_IO.Out_File)
   with Pre => not Self.Is_Open, Inline;
   --  Try to open Name in the given mode. If unsuccessful, emit a fatal error.

   procedure Put (Self : in out File_Type; Item : String)
   with Pre => Self.Is_Open, Inline;
   procedure Put (Self : in out File_Type; Item : Unbounded_String)
   with Pre => Self.Is_Open, Inline;
   --  Write Item to Self

   procedure Put_Line (Self : in out File_Type; Item : String)
   with Pre => Self.Is_Open, Inline;
   procedure Put_Line (Self : in out File_Type; Item : Unbounded_String)
   with Pre => Self.Is_Open, Inline;
   --  Write Item to Self and start a new line

   procedure New_Line (Self : in out File_Type)
   with Pre => Self.Is_Open, Inline;
   --  Start a new line in Self

   procedure Close (Self : in out File_Type)
   with Pre => Self.Is_Open, Inline;
   --  Close the text file that Self references

   procedure Run_GNATpp (Filename : String);
   --  Run "gnatpp" on the given file (i.e. reformat/pretty-print it)

   procedure Run_Clang_Format (Filename : String);
   --  Run "clang-format" on the given file (i.e. reformat/pretty-print it)

private

   use Ada.Text_IO;

   type File_Type is new Ada.Finalization.Limited_Controlled with record
      File     : Ada.Text_IO.File_Type;
      Filename : Unbounded_String;
   end record;

   overriding
   procedure Finalize (Self : in out File_Type);

end Text_Files;
