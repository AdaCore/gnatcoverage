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
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Strings; use GNAT.Strings;
with GNAT.Regexp;

limited with Checkpoints;
limited with LLVM_JSON_Checkpoints;
with Coverage_Options; use Coverage_Options;
with Diagnostics;      use Diagnostics;
with Logging;
with Paths;            use Paths;
with SC_Obligations;   use SC_Obligations;
with Slocs;            use Slocs;
with Strings;          use Strings;
with Traces_Dbase;     use Traces_Dbase;
with Traces_Elf;       use Traces_Elf;
with Traces_Lines;     use Traces_Lines;
with Traces_Source;    use Traces_Source;
with Traces_Stats;     use Traces_Stats;
with Types;            use Types;

package Files_Table is

   use all type Unbounded_String;

   Files_Table_Trace : constant Logging.GNATCOLL_Trace :=
     Logging.Create_Trace ("FILE_TABLE");

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

   --  Global directory of all source files

   function Get_Index_From_Full_Name
     (Full_Name           : String;
      Kind                : File_Kind;
      Insert              : Boolean := True;
      Indexed_Simple_Name : Boolean := False;
      Insert_After_Freeze : Boolean := False) return Source_File_Index;
   function Get_Index_From_Simple_Name
     (Simple_Name         : String;
      Kind                : File_Kind;
      Insert              : Boolean := True;
      Insert_After_Freeze : Boolean := False) return Source_File_Index;
   --  Register a full or simple name in the files table.
   --
   --  If Insert is False and the file is not registered yet, return
   --  No_Source_File.
   --
   --  If the file is already registered, check that it was registered with
   --  Kind (or Stub_File) and raise a fatal error if it was not.
   --
   --  If Indexed_Simple_Name is set to True, register both the full name and
   --  the simple name to our internal maps.
   --
   --  If Insert_After_Freeze is set to True, allow the insertion of the file
   --  in the files table after it was frozen. For note, this means that you
   --  won't be able to get a unique name for this file, as this is computed
   --  when the file table is frozen.
   --
   --  Windows-looking absolute filenames are canonicalized by upper casing the
   --  drive letter and lower casing all other letters.

   function Get_Index_From_Generic_Name
     (Name                : String;
      Kind                : File_Kind;
      Insert              : Boolean := True;
      Indexed_Simple_Name : Boolean := False;
      Insert_After_Freeze : Boolean := False) return Source_File_Index;
   --  Call Get_Index_From_Simple_Name or Get_Index_From_Full_Name depending
   --  on whether Name is an absolute path. Return the result of this call.
   --
   --  Indexed_Simple_Name is passed when calling Get_Index_From_Full_Name and
   --  is ignored otherwise. It can be set True only when loading a checkpoint.
   --
   --  Windows-looking absolute filenames are canonicalized by upper casing the
   --  drive letter and lower casing all other letters.
   --
   --  If Insert_After_Freeze is True, allow the inserting after the files
   --  table has been frozen. Note that this breaks the uniqueness invariant
   --  of names returned by Get_Unique_Name.

   procedure Consolidate_File_Kind
     (Index : Valid_Source_File_Index; Kind : File_Kind);
   --  If Index designates a Stub_File and Kind is not Stub_File, change its
   --  kind to Kind. Raise a fatal error if the Kind for Index isn't Kind
   --  *and* neither are stub files.
   --
   --  This is used make sure all accesses to a file expect a consistent kind.

   function Get_Full_Name
     (Index : Source_File_Index; Or_Simple : Boolean := False) return String;
   --  Return the full name for the given index. If there is no full name and
   --  Or_Simple is true, return the simple name instead. Otherwise, abort with
   --  an error message.

   function Get_Simple_Name (Index : Source_File_Index) return String;
   --  Return the simple name for the given index

   function Get_Unique_Name (Index : Source_File_Index) return String
   with Pre => Get_File (Index).Kind = Source_File;
   --  Return the shortest unambiguous file name. It is the smallest suffix for
   --  full name that is unique to this file (multiple files can have the same
   --  base name). This is available only for source files. Since unicity
   --  changes when new files are registered in the table, it is invalid to
   --  register a new file once Get_Unique_Name has been invoked once.

   --  Utilities to open files from the source file table. Source files will be
   --  searched on the local filesystem, in the following order:
   --  (1) from xcov's execution directory;
   --  (2) after rebasing them using the rebase list;
   --  (3) from the source search path.

   function Get_Unique_Filename
     (Index : Source_File_Index; Extension : String) return String
   with Pre => Get_File (Index).Kind = Source_File;
   --  Wrapper around Get_Unique_Name, replacing slashes with dashes in the
   --  unique name and suffixing with Extension.

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
     (File : Source_File_Index; Line : Positive; SCO : SCO_Id);
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

   type Line_State_Cell is (Cell_1, Cell_2, Cell_3, Cell_4, Cell_5);

   Coverage_Level_To_Cell :
     constant array (Coverage_Level) of Line_State_Cell :=
       (Insn     => Cell_1,
        Branch   => Cell_2,
        Stmt     => Cell_1,
        Decision => Cell_2,
        MCDC     => Cell_2,
        UC_MCDC  => Cell_2,
        ATC      => Cell_3,
        ATCC     => Cell_3,
        Fun_Call => Cell_4,
        GExpr    => Cell_5);
   --  For one specific execution of GNATcov, we know that
   --  each line needs at most only four states (insn, branch,
   --  stmt(+decision|+mcdc|+uc_mcdc)?(+atc|+atcc)?(funcall)?(+gexpr)?).
   --  Thus, there is no need to store the state for all coverage levels at the
   --  same time. This table is thus used to convert the coverage level to the
   --  appropriate state "storage cell".

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
     (A : in out Resizeable_Array_Access; E : Element_Type);
   --  Resize an array in order to append an element to it

   type Line_Info is record
      --  Coverage information associated with a source line

      Obj_Infos : Object_Coverage_Info_Array_Acc;
      --  Detailed object coverage information for this line, null when empty

      SCOs : SCO_Id_Array_Acc;
      --  SCOs for this source line, null when empty. Does _not_ contain MC/DC
      --  coverage obligations: to iterate over them, you can use the Condition
      --  function defined in SC_Obligations, passing it the decision SCO and
      --  the condition index (Last_Cond_Index (D_SCO) will give you the last
      --  condition index).

      Messages : Message_Array_Acc;
      --  Various diagnostic messages attached to this line, null when empty

      Line_Cache : String_Access := null;
      --  Cached source line content

      Exemption : Source_Location := Slocs.No_Location;
      --  If this line is covered by an exemption, this is set to the sloc of
      --  the Exempt_On annotation.

      Disabled_Cov : Source_Location := Slocs.No_Location;
      --  If this line is covered by a coverage disabling annotation, this is
      --  set to the sloc of the Cov_Off annotation.

      Is_Multistatement : Tristate := Unknown;
      --  Whether there are more than one statement SCO on this line, or
      --  Unknown if not computed yet.

      State : Line_States := (others => No_Code);
      --  Coverage state for each available coverage level (see previous
      --  comment block).

      Coverage_Processed : Boolean := False;
      --  Whether this line has already had its coverage state computed.
      --  Used to avoid recomputing the coverage state for different output
      --  formats.
   end record;

   type Line_Info_Access is access all Line_Info;
   Empty_Line_Info : constant Line_Info_Access;

   --  Describe a source file - one element per line

   package Source_Line_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Line_Info_Access);
   type Source_Lines is new Source_Line_Vectors.Vector with null record;

   type Any_Ignore_Status is (Unknown, Always, Sometimes, Never);
   --  Represents the different states a source file can be regarding the
   --  option --excluded-source-files. Consolidation rules are described in
   --  procedure Consolidate_Ignore_Status.

   type Compilation_Unit is record
      Language  : Supported_Language_Kind;
      Unit_Name : Unbounded_String;
   end record;
   --  This record is used to uniquely identify a unit of any language
   --  supported by gnatcov. The unique identifier, stored as Unit_Name is
   --  the unit name for unit-based language, and the file fullname for
   --  file-based languages.

   function Image (U : Compilation_Unit) return String
   is (case U.Language is
         when Unit_Based_Language => To_Lower (+U.Unit_Name),
         when File_Based_Language => Fold_Filename_Casing (+U.Unit_Name));

   function "<" (L, R : Compilation_Unit) return Boolean
   is (Image (L) < Image (R));

   function "=" (L, R : Compilation_Unit) return Boolean
   is (Image (L) = Image (R));

   No_Compilation_Unit : constant Compilation_Unit :=
     (Language => File_Based_Language, Unit_Name => Strings."+" (""));

   package Unit_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Compilation_Unit);

   type Owning_Unit (Known : Boolean := False) is record
      case Known is
         when True =>
            Name : Compilation_Unit;

         when False =>
            null;
      end case;
   end record;
   --  Whether the unit of a file is known or not. If it is, stores the name of
   --  the unit.

   type File_Info (Kind : File_Kind := Source_File) is record
      --  Source file information

      Full_Name : String_Access;
      --  Full path name. This path is normalized (and lowercased on
      --  windows platforms).

      Preserved_Full_Name : String_Access;
      --  Full canonicalized path name that inconditionally matches the casing
      --  of the original filename that was given to gnatcov.
      --  It is used for display purpose.
      --  It is also this, instead of Full_Name, that gets saved in
      --  checkpoint files.

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

            Has_Decoding_Error : Boolean := False;
            --  Whether we got at least one error when decoding a source line
            --  to UTF-8.

            Sloc_To_SCO_Maps : Sloc_To_SCO_Map_Array_Acc;
            --  Sloc -> SCO_Id indices for this file

            Li_Stats : Li_Stat_Array := (others => 0);
            --  Line counters associated with the file (e.g total number of
            --  lines, number of lines that are covered).

            Ob_Stats : Ob_Stat_Array;
            --  Obligation counters for each kind of SCO (stmt, decision, mcdc,
            --  atc and atcc). Stores how many statement are covered, how many
            --  decisions are covered etc.

            Has_Source_Coverage_Info : Boolean := False;
            --  True if source coverage information has been registered for
            --  this source file.

            Has_Object_Coverage_Info : Boolean := False;
            --  True if object coverage information has been registered for
            --  this source file.

            Ignore_Status : Any_Ignore_Status := Unknown;
            --  How often this source file has been ignored for the coverage
            --  report.

            Unit : Owning_Unit;
            --  Information about the unit this source file belongs to

         when Library_File =>
            Main_Source : Source_File_Index := No_Source_File;
            --  Main source file. For Ada, this is a simple name; for C this
            --  is either a simple name or a full name, depending on whether
            --  the information is available. In any case, it must be a valid
            --  source file (not No_Source_File).
      end case;
   end record;

   type File_Info_Access is access File_Info;

   procedure Files_Table_Iterate
     (Process : not null access procedure (Index : Source_File_Index));
   --  Call Process for all file info entries in the files table, in sorted
   --  simple names order. Note that it is invalid to register a new file past
   --  the first files table iteration.

   function First_File return Source_File_Index;
   --  Return the first valid source file index

   function Last_File return Source_File_Index;
   --  Return the last valid source file index

   procedure Consolidate_Ignore_Status
     (Index : Valid_Source_File_Index; Status : Any_Ignore_Status);
   --  Consolidate the ignore status of the file represented by Index
   --  with the new status.
   --
   --    Sometimes      Consolidation is done according to this hierarchy.
   --     /     \       The consolidation result between A and B is the
   --  Never    Always  lowest status that is a parent to both A and B (this
   --     \     /       can be A or B).
   --     Unknown
   --
   --  Does nothing if the file represented by index is not tagged as
   --  Source_File.

   package Excluded_Sources_Vector is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => File_Info_Access);
   --  Vector of source files, used mainly to group excluded source files

   type Excluded_Sources_Vector_Access is
     access Excluded_Sources_Vector.Vector;

   function Get_File (Index : Source_File_Index) return File_Info_Access;

   procedure Consolidate_Source_File_Unit
     (Index : Valid_Source_File_Index; New_Unit : Compilation_Unit)
   with Pre => Get_File (Index).Kind = Source_File;
   --  Update the unit name info for the source file represented by Index.
   --  Does nothing if the new unit name is the empty string.
   --
   --  Calling this procedure is valid iff the source file at Index has no
   --  owning unit already, or that it is the same as New_Unit.

   procedure Iterate_On_Lines
     (File    : File_Info_Access;
      Process : not null access procedure (Index : Positive))
   with Pre => File.Kind = Source_File;

   function Last_Line (File : File_Info_Access) return Natural
   with Pre => File.Kind = Source_File;
   --  Return the last line of the file

   function Get_Line
     (File : File_Info_Access; Index : Positive) return Line_Info_Access
   with Pre => File.Kind in Stub_File | Source_File;

   function Get_Line (Sloc : Source_Location) return Line_Info_Access
   with
     Pre =>
       Sloc = Slocs.No_Location
       or else Get_File (Sloc.Source_File).Kind in Stub_File | Source_File;

   function Get_SCOs
     (Source_Range : Source_Location_Range) return SCO_Sets.Set;
   --  Return all of the SCOs under the given Source_Range

   procedure Set_Encoding (Encoding : String);
   --  Set the encoding used to interpret source code (this is a global
   --  setting). This raises a fatal error if Encoding is not supported. If not
   --  called, assume latin-1 (the default for Ada, has the advantage of being
   --  able to decode any binary content).

   function To_UTF8 (S : String) return String;
   --  Transcode the given string according to the last call to Set_Encoding to
   --  UTF-8. Bytes that could not be decoded are turned into replacement
   --  codepoints.

   procedure Move_Forward_UTF8
     (S : String; Index : in out Natural; Count : Natural);
   --  Assuming that S is a valid UTF-8 string and that Index refers to the
   --  first byte of a codepoint in S, increment Index so that it moves forward
   --  by Count codepoints. If Index is out of S's range, do nothing.

   function Slice_Last_UTF8 (S : String; Length : Natural) return Natural;
   --  Return the index I that would make S (S'First .. I) a slice that
   --  contains Length codepoints.

   function Get_Line
     (File : File_Info_Access; Index : Positive; UTF8 : Boolean := False)
      return String
   with Pre => File.Kind = Source_File;

   function Get_Line
     (Sloc : Source_Location; UTF8 : Boolean := False) return String
   with
     Pre =>
       Sloc = Slocs.No_Location
       or else Get_File (Sloc.Source_File).Kind = Source_File;

   --  Get line info (or text) at File:Index (or at Sloc)

   function Sloc_Intersects_SCO (Sloc : Source_Location) return SCO_Id;
   --  Returns a SCO_Id in which Sloc lies. If Sloc is not within any SCO,
   --  return No_SCO_Id.
   --
   --  In case multiple SCOs contain Sloc, the result is the Id of the first
   --  SCO registered in the line info corresponding to Sloc.

   type ALI_Region_Annotation_Kind is (Exemption, Disable_Coverage);

   procedure Populate_Annotations
     (FI : Source_File_Index; Kind : ALI_Region_Annotation_Kind);
   --  Fill the Exemption/Disabled_Cov field according to Kind in the Line_Info
   --  records of FI based on the information in Annotation_Map.

   function Writeable_Sloc_To_SCO_Map
     (Index : Source_File_Index; Kind : SCO_Kind)
      return access Sloc_To_SCO_Maps.Map
   with
     Pre  => Get_File (Index).Kind = Source_File,
     Post => Writeable_Sloc_To_SCO_Map'Result /= null;
   --  Return (allocating, if necessary) the requested map, in a writeable
   --  state.

   function Sloc_To_SCO_Map
     (Index : Source_File_Index; Kind : SCO_Kind)
      return access constant Sloc_To_SCO_Maps.Map
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
     (File : in out File_Type; FI : File_Info_Access; Success : out Boolean);
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

   procedure Checkpoint_Save (CSS : access Checkpoints.Checkpoint_Save_State);
   --  Save the current files table to S

   procedure Checkpoint_Clear;
   --  Clear the internal data structures used to create checkpoints

   procedure Checkpoint_Load
     (CLS                   : in out Checkpoints.Checkpoint_Load_State;
      Excluded_Source_Files : access GNAT.Regexp.Regexp);
   --  Load checkpointed files table from S and merge in current state.
   --  Excluded_Source_Files should be null if the checkpoint purpose is
   --  Consolidation.

   procedure LLVM_JSON_Load
     (Ckpt : access constant LLVM_JSON_Checkpoints.LLVM_Coverage_Ckpt);
   --  Use the JSON loaded checkpoint to create file tables,

   procedure Postprocess_Source
     (Preprocessed_Filename : String; Postprocessed_Filename : String);
   --  Specific to the C instrumentation.
   --
   --  Postprocess the given Preprocessed_Filename to remove redundant line
   --  markers. This is done to keep the following invariant intact: we must
   --  be able to use presumed source locations to have unique SCOs source
   --  locations. As a reminder, presumed locations are computed through
   --  preprocessor-inserted line markers.
   --
   --  This means that the following code:
   --  # 1 main.c
   --  int a;
   --  # 1 main.c 3
   --  int ab;
   --
   --  will be rewritten to
   --  # 1 main.c
   --  int a; int ab;
   --
   --  Note that this will remove the "system header "flag (the "3" at the
   --  end of the line marker). We expect that this won't be a problem in
   --  practice.
   --
   --  Important: do not do that in case the code lines are pragma directives,
   --  which are preprocessor directives that are preserved by a preprocessing
   --  pass, e.g.
   --  # 1 main.c
   --  #pragma GCC diagnostic push
   --  # 1 main.c
   --  #pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
   --
   --  as we would otherwise produce the following (invalid) code:
   --  # 1 main.c
   --  #pragma GCC diagnostic push#pragma GCC diagnostic ignored \
   --    "-Wimplicit-fallthrough"

private

   Empty_Line_Info : constant Line_Info_Access := new Line_Info;

end Files_Table;
