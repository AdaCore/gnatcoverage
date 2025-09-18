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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.CRC32;
with GNATCOLL.Mmap;

with Hex_Images; use Hex_Images;
with Paths;
with Switches;

package body Inputs is

   -------------------------
   -- Read_List_From_File --
   -------------------------

   procedure Read_List_From_File
     (File_Name : String; Process : not null access procedure (Name : String))
   is
      function Get_Line_Compatible (F : File_Type) return String;
      --  Same as Get_Line but eat trailing ASCII.CR so that DOS text files can
      --  be read easily.

      -------------------------
      -- Get_Line_Compatible --
      -------------------------

      function Get_Line_Compatible (F : File_Type) return String is
         L : constant String := Get_Line (F);
      begin
         if L'Length > 1 and then L (L'Last) = ASCII.CR then
            return L (L'First .. L'Last - 1);
         else
            return L;
         end if;
      end Get_Line_Compatible;

      F : File_Type;

      --  Start of processing for Read_List_From_File

   begin
      Open (F, In_File, File_Name);
      while not End_Of_File (F) loop
         declare
            L : constant String := Get_Line_Compatible (F);
         begin
            if L = "" or else L (L'First) = '#' then
               null;
            else
               Process (L);
            end if;
         end;
      end loop;
      Close (F);
   end Read_List_From_File;

   -------------------
   -- Log_File_Open --
   -------------------

   procedure Log_File_Open (File_Name : String) is

      function Get_CRC32 return String;
      --  Return the CRC32 of the content of File_Name, or "<None>" if
      --  File_Name cannot be read.

      ---------------
      -- Get_CRC32 --
      ---------------

      function Get_CRC32 return String is
         use GNAT.CRC32;
         use GNATCOLL.Mmap;

         F       : Mapped_File := Open_Read (File_Name);
         Content : Mapped_Region := Read (F);
         C       : CRC32;
      begin
         Initialize (C);
         Update (C, String (Data (Content).all (1 .. Last (Content))));
         Free (Content);
         Close (F);
         return "0x" & Hex_Image (Get_Value (C));
      exception
         when Name_Error =>
            return "<None>";
      end Get_CRC32;

   begin
      if Switches.Misc_Trace.Is_Active then
         Switches.Misc_Trace.Trace
           ("--- notice: open """
            & File_Name
            & """ (CRC32 = "
            & Get_CRC32
            & ")");
      end if;
   end Log_File_Open;

   --------------------
   -- Create_Matcher --
   --------------------

   procedure Create_Matcher
     (Pattern_List     : String_Vectors.Vector;
      Matcher          : out GNAT.Regexp.Regexp;
      Has_Matcher      : out Boolean;
      Case_Insensitive : Boolean := False)
   is
      use String_Vectors;

      Pattern : Unbounded_String;
      --  Regular expression pattern (using regexp syntax described in
      --  GNAT.Regexp) to match the same set of strings as all patterns in
      --  Pattern_List.
      --
      --  Note that we use Glob_To_Regexp instead of GNAT.Regexp's internal
      --  globbing pattern support to keep the globbing syntax that gnatcov
      --  support coherent among all options that accept them.

      First : Boolean := True;

      procedure Process (C : Cursor);
      --  Include the globbing pattern referenced by C into Pattern

      -------------
      -- Process --
      -------------

      procedure Process (C : Cursor) is
         Raw_Pattern  : constant String := +String_Vectors.Element (C);
         Glob_Pattern : constant String :=
           (if Case_Insensitive then To_Lower (Raw_Pattern) else Raw_Pattern);
      begin
         --  Ignore blank lines

         if Glob_Pattern'Length = 0 then
            return;
         end if;

         if First then
            First := False;
         else
            Append (Pattern, "|");
         end if;
         Append (Pattern, Paths.Glob_To_Regexp (Glob_Pattern));
      end Process;

      --  Start of processing for Create_Matcher

   begin
      String_Vectors.Iterate (Pattern_List, Process'Access);
      Has_Matcher := not First;
      if Has_Matcher then
         Matcher := GNAT.Regexp.Compile (Pattern => +Pattern);
      end if;
   end Create_Matcher;

end Inputs;
