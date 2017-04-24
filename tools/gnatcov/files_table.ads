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
with Ada.Streams;  use Ada.Streams;
with Ada.Text_IO;  use Ada.Text_IO;

with GNAT.Strings; use GNAT.Strings;

with Checkpoints;    use Checkpoints;
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

   subtype Valid_Source_File_Index is
     Source_File_Index range First_Source_File .. Source_File_Index'Last;

   type File_Kind is
   --  Specify a kind of file this package handles. Different kinds imply
   --  different consolidation behaviors.

     (Stub_File,
      --  Stub files can be consolidated with any other kind of file. Also,
      --  they are excluded from coverage reports.

      Source_File,
      --  Source files can be consolidated with other source files only. They
      --  can be included in coverage reports.

      Library_File);
      --  Library files can be consolidated with library files only. They are
      --  excluded from coverage reports.

   function Canonicalize_Filename (Filename : String) return String;
   function Canonicalize_Filename (Filename : String) return String_Access;
   --  If the filename looks like a Windows filename, it is canonicalized: the
   --  drive letter is uppercased and the other characters are lowercased.

   function Build_Filename
     (Dir      : String;
      Filename : String) return String;
   function Build_Filename
     (Dir      : String;
      Filename : String) return String_Access;
   --  Create a filename from a directory name and a filename.
   --  The directory name is expected to be not empty.
   --  If the filename looks like a Windows filename, it is canonicalized.

   --  Global directory of all source files

   function Get_Index_From_Full_Name
     (Full_Name           : String;
      Kind                : File_Kind;
      Insert              : Boolean := True;
      Indexed_Simple_Name : Boolean := False) return Source_File_Index;
   function Get_Index_From_Simple_Name
     (Simple_Name : String;
      Kind        : File_Kind;
      Insert      : Boolean := True) return Source_File_Index;
   --  Register a full or simple name in the files table.
   --
   --  If Insert is False and the file is not registered yet, return
   --  No_Source_File.
   --
   --  If the file is already registered, check that it was registered with
   --  Kind (or Stub_File) and raise a fatal error if it was not.
   --
   --  Indexed_Simple_Name can be set to True only when loading a checkpoint.

   function Get_Index_From_Generic_Name
     (Name                : String;
      Kind                : File_Kind;
      Insert              : Boolean := True;
      Indexed_Simple_Name : Boolean := False) return Source_File_Index;
   --  Call Get_Index_From_Simple_Name or Get_Index_From_Full_Name depending
   --  on whether Name is an absolute path. Return the result of this call.
   --
   --  Indexed_Simple_Name is passed when calling Get_Index_From_Full_Name and
   --  is ignored otherwise. It can be set True only when loading a checkpoint.

   procedure Consolidate_File_Kind
     (Index : Valid_Source_File_Index;
      Kind  : File_Kind);
   --  If Index designates a Stub_File and Kind is not Stub_File, change its
   --  kind to Kind. Raise a fatal error if the Kind for Index isn't Kind
   --  *and* neither are stub files.
   --
   --  This is used make sure all accesses to a file expect a consistent kind.

   function Get_Full_Name (Index : Source_File_Index) return String;
   function Get_Simple_Name (Index : Source_File_Index) return String;
   --  Return the full/simple name for the given index

   function Get_Unique_Name (Index : Source_File_Index) return String
      with Pre => Get_File (Index).Kind = Source_File;
   --  Return the shortest unambiguous file name. It is the smallest suffix for
   --  full name that is unique to this file (multiple files can have the same
   --  base name). This is availble only for source files. Since unicity
   --  changes when new files are registered in the table, it is invalid to
   --  register file once Get_Unique_Name has been invoked once.

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

   procedure Set_Exec_Prefix (Prefix : String);
   --  Set the executable search prefix to Prefix. If an executable named
   --  "/path/to/something" is not found, it would be searched in
   --  Build_Filename(Prefix, "something").

   function Lookup_Exec (Name : String) return String;
   --  If Name exists, return its name unchanged. Otherwise look for the Name
   --  executable in the prefix registered through Add_Exec_Prefix. If it is
   --  found, return the full path, return an empty string otherwise. Name can
   --  be both a base name or a full path: only the base name part will be used
   --  during the search.

   procedure Expand_Line_Table (File : Source_File_Index; Line : Positive);
   --  If Line is not in File's line table, expand this table and mark the new
   --  line as No_Code.

   procedure Add_Line_For_Object_Coverage
     (File  : Source_File_Index;
      State : Line_State;
      Line  : Positive;
      Addrs : Address_Info_Acc;
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

      Instruction_Set : Address_Info_Acc;
      --  Range of instruction handled by this record

      Base : Traces_Base_Acc;
      --  Object coverage information for this instruction set

      Exec : Exe_File_Acc;
      --  Exec from where the address range has been extracted
   end record;

   package SCO_Id_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => SCO_Id);

   type Line_State_Cell is (Cell_1, Cell_2);
   Coverage_Level_To_Cell : constant
      array (Coverage_Level) of Line_State_Cell :=
     (Insn     => Cell_1,
      Branch   => Cell_2,
      Stmt     => Cell_1,
      Decision => Cell_2,
      MCDC     => Cell_2,
      UC_MCDC  => Cell_2);
   --  For one specific execution of GNATcov, we know that each line needs at
   --  most only two states (insn, branch, stmt, stmt+decision, stmt+mcdc or
   --  stmt+uc_mcdc). Thus, there is no need to store the state for all
   --  coverage levels at the same time. This table is thus used to convert the
   --  coverage level to the appropriate state "storage cell".

   type Line_States is array (Line_State_Cell) of Line_State;

   --  The following Line_Info record is heavily used, thus its memory size is
   --  very important. That's why the three lists it contains are not
   --  implemented using vectors, but using accesses to unconstraint arrays.
   --  Those lists never contain a lot of items, so reallocating them at each
   --  new item is acceptable.

   type Object_Coverage_Info_Array is
      array (Natural range <>) of Object_Coverage_Info;
   type Object_Coverage_Info_Array_Acc is access Object_Coverage_Info_Array;

   type SCO_Id_Array is array (Natural range <>) of SCO_Id;
   type SCO_Id_Array_Acc is access SCO_Id_Array;

   type Message_Array is array (Natural range <>) of Message;
   type Message_Array_Acc is access Message_Array;

   generic
      type Index_Type is range <>;
      type Element_Type is private;

      type Resizeable_Array is array (Index_Type range <>) of Element_Type;
      type Resizeable_Array_Access is access Resizeable_Array;
   procedure Append_To_Array
      (A : in out Resizeable_Array_Access;
       E : Element_Type);
   --  Resize an array in order to append an element to it

   type Line_Info is record
      --  Coverage information associated with a source line

      Obj_Infos : Object_Coverage_Info_Array_Acc;
      --  Detailed object coverage information for this line, null when empty

      SCOs : SCO_Id_Array_Acc;
      --  SCOs for this source line, null when empty

      Messages : Message_Array_Acc;
      --  Various diagnostic messages attached to this line, null when empty

      Line_Cache : String_Access := null;
      --  Cached source line content

      Exemption : Source_Location := Slocs.No_Location;
      --  If this line is covered by an exemption, this is set to the sloc of
      --  the Exempt_On annotation.

      Is_Multistatement : Tristate := Unknown;
      --  Whether there are more than one statement SCO on this line, or
      --  Unonkwn if not computed yet.

      State : Line_States := (others => No_Code);
      --  Coverage state for each available coverage level (see previous
      --  comment block).
   end record;

   type Line_Info_Access is access all Line_Info;
   Empty_Line_Info : constant Line_Info_Access;

   type Source_Lines is private;

   type File_Info (Kind : File_Kind := Source_File) is record
      --  Source file information.

      Full_Name : String_Access;
      --  Full path name

      Simple_Name : String_Access;
      --  File name of the source file, without the path

      Indexed_Simple_Name : Boolean;
      --  True if simple name has been entered in the Simple_Name_Map (i.e.
      --  if Get_Index_From_Simple_Name was called for that name). This needs
      --  to be preserved across checkpointing, because full names of some
      --  source files must have their simple name indexed (dependent on the
      --  language), and full names of LI files must not (we never look up LI
      --  files by simple name, and we need to allow different LI files with
      --  the same base name to coexist: this can occur for C units, and this
      --  can also happen when consolidating checkpoints with different object
      --  directories for the same unit).

      Has_Source : Boolean := True;
      --  False if no source file is found that corresponds to this file name

      case Kind is
         when Stub_File =>
            null;

         when Source_File =>
            Unique_Name : String_Access;
            --  Shortest unambiguous file name. It is the smallest Full_Name
            --  suffix that is unique to this file (multiple files can have the
            --  same base name). Computed by Get_Unique_Name once all files are
            --  registered in the table.

            LI : Source_File_Index := No_Source_File;
            --  Name of the library file corresponding to this source file

            Lines : Source_Lines;
            --  Source file to display in the reports

            Sloc_To_SCO_Maps : Sloc_To_SCO_Map_Array_Acc;
            --  Sloc -> SCO_Id indices for this file

            Stats : Stat_Array := (others => 0);
            --  Counters associated with the file (e.g total number of lines,
            --  number of lines that are covered).

            Has_Source_Coverage_Info : Boolean := False;
            --  True if source coverage information has been registered for
            --  this source file.

            Has_Object_Coverage_Info : Boolean := False;
            --  True if object coverage information has been registered for
            --  this source file.

         when Library_File =>
            Main_Source : Source_File_Index := No_Source_File;
            --  Main source file. For Ada, this is a simple C name; for C this
            --  is either a simple name or a full name, depending on whether
            --  the information is available.
      end case;
   end record;

   type File_Info_Access is access File_Info;

   procedure Files_Table_Iterate
     (Process : not null access procedure (Index : Source_File_Index));

   function First_File return Source_File_Index;
   --  Return the first valid source file index

   function Last_File return Source_File_Index;
   --  Return the last valid source file index

   function Get_File
     (Index : Source_File_Index) return File_Info_Access;

   procedure Iterate_On_Lines
     (File    : File_Info_Access;
      Process : not null access procedure (Index : Positive))
     with Pre => File.Kind = Source_File;

   function Get_Line
     (File  : File_Info_Access;
      Index : Positive) return Line_Info_Access
     with Pre => File.Kind = Source_File;

   function Get_Line (Sloc : Source_Location) return Line_Info_Access;

   function Get_Line
     (File  : File_Info_Access;
      Index : Positive) return String
     with Pre => File.Kind = Source_File;

   function Get_Line (Sloc : Source_Location) return String;

   --  Get line info (or text) at File:Index (or at Sloc)

   function Writeable_Sloc_To_SCO_Map
     (Index : Source_File_Index;
      Kind  : SCO_Kind) return access Sloc_To_SCO_Maps.Map
     with Pre => Get_File (Index).Kind = Source_File,
          Post => Writeable_Sloc_To_SCO_Map'Result /= null;
   --  Return (allocating, if necessary) the requested map, in a writeable
   --  state.

   function Sloc_To_SCO_Map
     (Index : Source_File_Index;
      Kind  : SCO_Kind) return access constant Sloc_To_SCO_Maps.Map
     with Post => Sloc_To_SCO_Map'Result /= null;
   --  Return the requested map, in a read-only state

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

   procedure Fill_Line_Cache (FI : File_Info_Access)
     with Pre => FI.Kind = Source_File;
   --  Try to open FI and populate its line cache

   procedure Invalidate_Line_Cache (FI : File_Info_Access)
     with Pre => FI.Kind = Source_File;
   --  Free FI's line cache

   procedure Warn_File_Missing (File : File_Info);
   --  Report that File cannot be found in source path

   function To_Display (File : File_Info_Access) return Boolean
     with Pre => File.Kind = Source_File;
   --  Return True if there is some relevant coverage information to display
   --  for this file and for the current coverage criteria.

   function Is_Multistatement_Line (Sloc : Source_Location) return Boolean;
   function Is_Multistatement_Line (LI : in out Line_Info) return Boolean;
   --  True if there is more than one Statement SCO for the line of Sloc/LI

   -----------------
   -- Checkpoints --
   -----------------

   procedure Checkpoint_Save (S : access Root_Stream_Type'Class);
   --  Save the current files table to S

   procedure Checkpoint_Load
     (S  : access Root_Stream_Type'Class;
      CS : access Checkpoint_State);
   --  Load checkpointed files table from S and merge in current state

private
   --  Describe a source file - one element per line

   package Source_Line_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Line_Info_Access);

   type Source_Lines is new Source_Line_Vectors.Vector with null record;
   type Source_Line_Array is array (Positive range <>) of aliased Line_Info;
   type Source_Line_Array_Acc is access all Source_Line_Array;

   Empty_Line_Info : constant Line_Info_Access := new Line_Info;

end Files_Table;
