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

with Ada.Containers.Vectors;

with GNAT.Strings; use GNAT.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Traces_Elf;   use Traces_Elf;
with Traces_Dbase; use Traces_Dbase;
with Traces_Stats; use Traces_Stats;
with Traces_Lines; use Traces_Lines;
with Slocs;        use Slocs;
with Types;        use Types;
with SC_Obligations; use SC_Obligations;

package Files_Table is
   --  This package manages a source file table and, for each file, a table of
   --  its source lines. Coverage information can be associated with each
   --  file/line. Only object coverage is supported.

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

   procedure New_Source_File (File : Source_File_Index);
   --  Initialize entry for File in source files table

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

   type Source_Coverage_Info;
   type Source_Coverage_Info_Acc is access Source_Coverage_Info;

   type Source_Coverage_Info is record
      State : Line_State;

      SCO : SCO_Id;
      --  SCO that generated this info

      Next : Source_Coverage_Info_Acc;
      --  Next element in the chain
   end record;

   package SCO_Id_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => SCO_Id);

   type Line_Info is record
      --  Coverage information associated with a source line

      State : Line_State := No_Code;
      --  Coverage state

      Obj_First, Obj_Last : Object_Coverage_Info_Acc;
      --  Detailed object coverage information for this line

      SCOs : SCO_Id_Vectors.Vector;
      --  SCOs for this source line
   end record;

   type Line_Info_Access is access Line_Info;

   Empty_Line_Info : constant Line_Info_Access;

   type Source_Lines is private;

   type File_Info is record
      --  Source file information.

      Full_Name : String_Access;
      --  Full path name

      Simple_Name  : String_Access;
      --  File name of the source file, without the path

      Alias_Num : Natural;
      --  0 if no other source file has the same basename, otherwise a uniq
      --  index.

      Lines      : Source_Lines;
      --  Source file to display in the reports

      Stats      : Stat_Array := (others => 0);
      --  Counters associated with the file (e.g total number of lines, number
      --  of lines that are covered).

      To_Display : Boolean := False;
      --  If True, this file should be displayed in the reports. If False,
      --  it should be skipped.
      --  The global source table in sources.adb and the table of sources
      --  that are concerned by the current coverage operation
      --  (Source_Line_Tables) share the sames indices. However, the
      --  latter is a subset of the former. A non-contiguous subset, in the
      --  general case. So, To_Display is used to discriminate files that are
      --  really concerned by the coverage from files that just happens to
      --  have an index between two "real" elements.
      --
      --  ??? This should be removable at some point. Either we have
      --  some coverage information for this file, or we don't...
   end record;

   type File_Info_Access is access File_Info;

   procedure Files_Table_Iterate
     (Process : not null access procedure (FI : File_Info_Access));

   function Files_Table_Element
     (Index : Source_File_Index)
     return File_Info_Access;

   procedure Iterate_On_Lines
     (File    : File_Info_Access;
      Process : not null access procedure (Index : Positive));

   function Get_Line_Info
     (File  : File_Info_Access;
      Index : Positive)
     return Line_Info_Access;

   procedure Open
     (File    : in out File_Type;
      FI      : File_Info_Access;
      Success : out Boolean);
   --  Try to open the file from the source file table whose index is Index,
   --  using the rebase/search information. If one found, Success is True;
   --  False otherwise.
private
   --  Describe a source file - one element per line

   package Source_Line_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Line_Info_Access);

   type Source_Lines is new Source_Line_Vectors.Vector with null record;

   Empty_Line_Info : constant Line_Info_Access := new Line_Info;
end Files_Table;
