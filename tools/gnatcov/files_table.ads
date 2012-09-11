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

with Ada.Containers.Vectors;
with Ada.Text_IO;  use Ada.Text_IO;

with GNAT.Strings; use GNAT.Strings;

with Coverage;       use Coverage;
with Diagnostics;    use Diagnostics;
with SC_Obligations; use SC_Obligations;
with Slocs;          use Slocs;
with Traces_Elf;     use Traces_Elf;
with Traces_Dbase;   use Traces_Dbase;
with Traces_Stats;   use Traces_Stats;
with Traces_Lines;   use Traces_Lines;
with Types;          use Types;

package Files_Table is

   --  This package manages a source file table and, for each file, a table of
   --  its source lines. Coverage information can be associated with each
   --  file/line. Only object coverage is supported.

   function Build_Filename
     (Dir      : String;
      Filename : String) return String_Access;
   --  Create a filename from a directory name and a filename.
   --  The directory name is expected to be not empty.
   --  If the filename looks like a Windows filename, it is canonicalized.

   --  Global directory of all source files

   function Get_Index_From_Full_Name
     (Full_Name : String;
      Insert    : Boolean := True) return Source_File_Index;
   function Get_Index_From_Simple_Name
     (Simple_Name : String;
      Insert      : Boolean := True) return Source_File_Index;
   function Get_Full_Name (Index : Source_File_Index) return String;
   function Get_Simple_Name (Index : Source_File_Index) return String;
   --  Comments needed???

   --  Utilities to open files from the source file table. Source files will be
   --  searched on the local filesystem, in the following order:
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

   procedure Add_Line_For_Object_Coverage
     (File  : Source_File_Index;
      State : Line_State;
      Line  : Positive;
      Addrs : Addresses_Info_Acc;
      Base  : Traces_Base_Acc;
      Exec  : Exe_File_Acc);
   --  Add File:Line to set of known source lines, if it doesn't exist already.
   --  Record the association of File:File with the given associated object
   --  code.

   procedure Add_Line_For_Source_Coverage
     (File : Source_File_Index;
      Line : Positive;
      SCO  : SCO_Id);
   --  Associate SCO with File:Line

   type Object_Coverage_Info;
   type Object_Coverage_Info_Acc is access Object_Coverage_Info;

   type Object_Coverage_Info is record
      --  This records maps an instruction set to its coverage information.
      --  An instruction set being a set of addresses in a given executable,
      --  this mapping can be built from a Traces_Base (that maps addresses to
      --  coverage information) and an Exe_File (that maps addresses
      --  to instructions).

      State : Line_State;
      --  Coverage of this instruction set

      Instruction_Set : Addresses_Info_Acc;
      --  Range of instruction handled by this record

      Base : Traces_Base_Acc;
      --  Object coverage information for this instruction set

      Exec : Exe_File_Acc;
      --  Exec from where the address range has been extracted

      Next : Object_Coverage_Info_Acc;
      --  Next element in the chain
   end record;

   package SCO_Id_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => SCO_Id);

   type Line_States is array (Coverage_Level) of Line_State;

   type Line_Info is record
      --  Coverage information associated with a source line

      State : Line_States := (others => No_Code);
      --  Coverage state for each coverage level

      Obj_First, Obj_Last : Object_Coverage_Info_Acc;
      --  Detailed object coverage information for this line

      SCOs : SCO_Id_Vectors.Vector;
      --  SCOs for this source line

      Messages : Message_Vectors.Vector;
      --  Various diagnostic messages attached to this line

      Line_Cache : String_Access := null;
      --  Cached source line content

      Exemption : Source_Location := Slocs.No_Location;
      --  If this line is covered by an exemption, this is set to the sloc of
      --  the Exempt_On annotation.
   end record;

   type Line_Info_Access is access Line_Info;

   Empty_Line_Info : constant Line_Info_Access;

   type Source_Lines is private;

   type File_Info is record
      --  Source file information.

      Full_Name : String_Access;
      --  Full path name

      Simple_Name : String_Access;
      --  File name of the source file, without the path

      Has_Source : Boolean := True;
      --  False if no source file is found that corresponds to this file name

      Alias_Num : Natural;
      --  0 if no other source file has the same basename, otherwise a unique
      --  index.

      Lines : Source_Lines;
      --  Source file to display in the reports

      Stats : Stat_Array := (others => 0);
      --  Counters associated with the file (e.g total number of lines, number
      --  of lines that are covered).

      Has_Source_Coverage_Info : Boolean := False;
      --  True if source coverage information has been registered for this
      --  source file.

      Has_Object_Coverage_Info : Boolean := False;
      --  True if object coverage information has been registered for this
      --  source file.
   end record;

   type File_Info_Access is access File_Info;

   procedure Files_Table_Iterate
     (Process : not null access procedure (Index : Source_File_Index));

   function Get_File
     (Index : Source_File_Index) return File_Info_Access;

   procedure Iterate_On_Lines
     (File    : File_Info_Access;
      Process : not null access procedure (Index : Positive));

   function Get_Line
     (File  : File_Info_Access;
      Index : Positive) return Line_Info_Access;

   function Get_Line (Sloc : Source_Location) return Line_Info_Access;

   function Get_Line
     (File  : File_Info_Access;
      Index : Positive) return String;

   function Get_Line (Sloc : Source_Location) return String;

   --  Get line info (or text) at File:Index (or at Sloc)

   function End_Lex_Element (Sloc : Source_Location) return Source_Location;
   --  Assuming that Sloc points to the beginning of an Ada lexical element,
   --  return an approximative source location for the end of this element.
   --
   --  This function is used as a simple heuristic to "complete" the source
   --  text of conditions, decisions and statements. Do not expect to get
   --  a precise Ada parsing from it.

   procedure Open
     (File    : in out File_Type;
      FI      : File_Info_Access;
      Success : out Boolean);
   --  Try to open the file from the source file table whose index is Index,
   --  using the rebase/search information. If one found, Success is True;
   --  False otherwise.

   procedure Fill_Line_Cache (FI : File_Info_Access);
   --  Try to open FI and populate its line cache

   procedure Invalidate_Line_Cache (FI : File_Info_Access);
   --  Free FI's line cache

   procedure Warn_File_Missing (File : File_Info);
   --  Report that File cannot be found in source path

   function To_Display (File : File_Info_Access) return Boolean;
   --  Return True if there is some relevant coverage information to display
   --  for this file and for the current coverage criteria.

   function Is_Multistatement_Line (Sloc : Source_Location) return Boolean;
   --  True if there is more than one Statement SCO for the line of Sloc

private
   --  Describe a source file - one element per line

   package Source_Line_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Line_Info_Access);

   type Source_Lines is new Source_Line_Vectors.Vector with null record;

   Empty_Line_Info : constant Line_Info_Access := new Line_Info;
end Files_Table;
