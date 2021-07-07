------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2021, AdaCore                     --
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

--  Common data structures for source instrumentation-based coverage
--
--  Organization of instrumented projects
--  =====================================
--
--  "gnatcov instrument" generates instrumented sources in the
--  "$project_name-gnatcov-instr" subdirectory of each project's object
--  directory. The following compilation units (and source files) are
--  generated:
--
--  * "gnatcov_rts-buffers-B<SLUG>.ads" for all compilation units of interest.
--    (<SLUG> is the result of Instrumented_Unit_Slug for the compilation unit
--    to instrument). These units are preelaborated and contain definitions for
--    the coverage buffers themselves, but also for buffers "metadata".
--
--  * "gnatcov_rts-buffers-P<SLUG>.ads" for all compilation units of interest
--    (<SLUG> is the result of Instrumented_Unit_Slug for the compilation unit
--    to instrument). These units are pure and contain one System.Address
--    constant per coverage buffer for the corresponding compilation unit.
--
--  * A single "gnatcov_rts-buffers-lists-<NAME>.ads" unit (<NAME> is the name
--    of the root project). This unit contains a list of access to buffers for
--    the coverage buffers of all units of interest.
--
--  * When the buffers dump trigger is not manual (see the --dump-trigger
--    switch), "gnatcov_rts-buffers-D<SLUG>.ads" for all mains in all projects.
--    This unit contains trigger-dependent routines to implement the automatic
--    dump of coverage buffers.
--
--  * An instrumented version of all compilation units of interest. These new
--    units are replacements for the original units. They fill the coverage
--    buffers for the unit.

with Ada.Characters.Handling;         use Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Libadalang.Analysis;   use Libadalang.Analysis;
with Libadalang.Rewriting;  use Libadalang.Rewriting;

with ALI_Files;           use ALI_Files;
with Checkpoints;
with GNATcov_RTS;         use GNATcov_RTS;
with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;
with Namet;               use Namet;
with SC_Obligations;      use SC_Obligations;
with Strings;             use Strings;
with Text_Files;
with Types;               use Types;

package Instrument.Common is

   pragma Elaborate_Body;

   function "/" (Dir, Name : String) return String is
     (Ada.Directories.Compose (Dir, Name));

   --  TODO??? Handle Unicode file names and source text

   function Str_To_Language (Language : String) return Any_Language;
   --  Return the (supported) language kind represented by the string (case-
   --  insensitive). Raise a fatal error if the given language is not
   --  supported.

   function Language_To_Str (Language : Any_Language) return String;
   --  Reverse operation of the above function

   function Str_To_Language_Kind
     (Language : String) return Any_Language_Kind;
   --  Returns the language kind (unit-based or file-based) for the given
   --  language.

   type Ada_Identifier is new Ada.Strings.Unbounded.Unbounded_String;
   --  Simple Ada identifier

   package Ada_Identifier_Vectors is new Ada.Containers.Vectors
     (Positive, Ada_Identifier);

   subtype Ada_Qualified_Name is Ada_Identifier_Vectors.Vector;
   --  Sequence of ada identifiers, representing a qualified name. For
   --  instance: Scope_A.Scope_B.Scope_C

   function "&" (Left, Right : Ada_Qualified_Name) return Ada_Qualified_Name
      renames Ada_Identifier_Vectors."&";

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Name) return Ada_Qualified_Name;
   --  Return the qualified name corresponding to the given name from a parse
   --  tree.

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Unbounded_Text_Type_Array)
      return Ada_Qualified_Name;
   --  Convert a Libadalang fully qualified name into our format

   function To_Qualified_Name (Name : String) return Ada_Qualified_Name;
   --  Convert a String qualified name into our format

   function Canonicalize (Name : Ada_Qualified_Name) return Ada_Qualified_Name;
   --  Fold casing of Ada identifiers

   function To_Ada (Name : Ada_Qualified_Name) return String
      with Pre => not Name.Is_Empty;
   --  Turn the given qualified name into Ada syntax

   function To_Symbol_Name (Name : Ada_Qualified_Name) return String
      with Pre => not Name.Is_Empty;
   --  Lower case each name of the qualified name, and joined them with an
   --  underscore, to have a C-like syntax.
   --
   --  Example: passing the qualified name Foo.Bar will return the string
   --  "foo_bar".

   Sys_Prefix : Ada_Qualified_Name;
   --  Scope for all instrumentation runtime files beyond the instrumented
   --  sources.

   Sys_Buffers : Ada_Qualified_Name;
   --  Scope in Sys_Prefix for all packages to contain coverage buffers

   Sys_Buffers_Lists : Ada_Qualified_Name;
   --  Scope in Sys_Prefix for all packages to contain generic procedures for
   --  iterations on coverage buffers.

   Statement_Buffer_Name : Ada_Qualified_Name;
   --  Qualified name (relative to the unit buffer package) of the buffer to
   --  contain coverage data corresponding to statement obligations.

   Decision_Buffer_Name : Ada_Qualified_Name;
   --  Qualified name (relative to the unit buffer package) of the buffer to
   --  contain coverage data corresponding to decision obligations.

   MCDC_Buffer_Name : Ada_Qualified_Name;
   --  Qualified name (relative to the unit buffer package) of the buffer to
   --  contain coverage data corresponding to decision BDD paths.

   Dump_Procedure_Name : constant Ada_Identifier :=
      To_Unbounded_String ("Dump_Buffers");
   --  Name of the procedure (in main dump helper packages) that dumps all
   --  coverage buffers to the source trace file.

   function Dump_Procedure_Symbol (Main : Ada_Qualified_Name) return String is
     ("gnatcov_rts_" & To_Symbol_Name (Main) & "_"
      & To_Lower (To_String (Dump_Procedure_Name)));
   --  Return the name of the exported symbol for the Dump_Buffers function

   Register_Dump_Procedure_Name : constant Ada_Identifier :=
      To_Unbounded_String ("Register_Dump_Buffers");
   --  Name of the procedure (in main dump helper packages) that registers the
   --  coverage buffers dump through atexit(3).

   type Compilation_Unit_Name
     (Language_Kind : Any_Language_Kind := Unit_Based_Language)
   is record

      case Language_Kind is
         when Unit_Based_Language =>
            Unit : Ada_Qualified_Name := Ada_Identifier_Vectors.Empty_Vector;
            Part : Unit_Parts         := Unit_Body;
            --  Identifies an Ada compilation unit (unit-based)

         when File_Based_Language =>
            Filename : Unbounded_String;
            --  Fallback for file-based languages (like C). We will use the
            --  simple filename for now.

            Project_Name : Unbounded_String;
            --  We also need the project name as different projects can have
            --  the same file.

      end case;
   end record;
   --  Unique identifier for an instrumented unit

   Part_Tags : constant array (Unit_Parts) of Character :=
     (Unit_Spec     => 'S',
      Unit_Body     => 'B',
      Unit_Separate => 'U');

   function CU_Name_For_Unit
     (Unit : Ada_Qualified_Name;
      Part : Unit_Parts) return Compilation_Unit_Name;
   --  Return the compilation unit name for the Ada compilation unit
   --  corresponding to the unit name and the unit part parameters.

   function CU_Name_For_File
     (Filename     : Unbounded_String;
      Project_Name : Unbounded_String) return Compilation_Unit_Name;
   --  Return the compilation unit name for the C translation unit
   --  corresponding to the filename parameter.

   function To_Compilation_Unit_Name
     (Source_File : GNATCOLL.Projects.File_Info) return Compilation_Unit_Name;
   --  Return the compilation unit name corresponding to the unit in
   --  Source_File.

   function To_Filename
     (Project  : Project_Type;
      CU_Name  : Compilation_Unit_Name;
      Language : Any_Language) return String;
   --  Return the name of the file to contain the given compilation unit,
   --  according to Project's naming scheme.

   function Image (CU_Name : Compilation_Unit_Name) return String;
   --  Return a string representation of CU_Name for use in diagnostics

   function Instrumented_Unit_Slug
     (Instrumented_Unit : Compilation_Unit_Name) return String;
   --  Given a unit to instrument, return a unique identifier to describe it
   --  (the so called slug).
   --
   --  One can use this slug to generate unique names for this unit.

   function "<" (Left, Right : Compilation_Unit_Name) return Boolean;
   --  Compare the result of a call to Instrumented_Unit_Slug (which gives
   --  unique identifiers for each compilation unit name) for both operands.

   function "=" (Left, Right : Compilation_Unit_Name) return Boolean;
   --  Same as above (but checking for equality)

   function Statement_Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Name) return String;
   --  Given a unit to instrument, return the name of the symbol to use for the
   --  entity that contains address of the statement coverage buffer.

   function Decision_Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Name) return String;
   --  Given a unit to instrument, return the name of the symbol to use for the
   --  entity that contains address of the decision coverage buffer.

   function MCDC_Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Name) return String;
   --  Given a unit to instrument, return the name of the symbol to use for the
   --  entity that contains address of the decision coverage buffer.

   function Buffer_Unit
     (Instrumented_Unit : Compilation_Unit_Name) return Ada_Qualified_Name;
   --  Given a unit to instrument, return the name of the unit that holds
   --  its coverage buffers (Coverage_Buffer_Type and Unit_Coverage_Buffers
   --  records).

   function Pure_Buffer_Unit
     (Instrumented_Unit : Compilation_Unit_Name) return Ada_Qualified_Name;
   --  Given a unit to instrument, return the name of the unit that holds
   --  addresses to its coverage buffers.

   function Project_Output_Dir (Project : Project_Type) return String;
   --  Return the directory in which we must create instrumented sources for
   --  Project. This retuns an empty strings for projects that do not have an
   --  object directory.

   package Instrumented_Unit_To_CU_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Compilation_Unit_Name,
      Element_Type => CU_Id);

   Instrumented_Unit_CUs : Instrumented_Unit_To_CU_Maps.Map;
   --  Associate a CU id for all instrumented units. Updated each time we
   --  instrument a unit (or load a checkpoint) and used each time we read a
   --  coverage buffer (or save to a checkpoint).

   function Find_Instrumented_Unit
     (CU_Name : Compilation_Unit_Name) return CU_Id;
   --  Return the CU_Id corresponding to the given instrumented unit, or
   --  No_CU_Id if not found.

   package File_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Ada.Strings.Unbounded.Unbounded_String,
      "="                 => Ada.Strings.Unbounded."=",
      Equivalent_Elements => Ada.Strings.Unbounded."=",
      Hash                => Ada.Strings.Unbounded.Hash);

   type Project_Info is record
      Project : Project_Type;
      --  Project that this record describes

      Externally_Built : Boolean;
      --  Whether this project is externaly built. In that case, we assume its
      --  units of interest have already been instrumented.

      Output_Dir : Ada.Strings.Unbounded.Unbounded_String;
      --  Subdirectory in the project file's object directory. All we generate
      --  for this project must land in it.

      Instr_Files : aliased File_Sets.Set;
      --  Set of files that were written to Output_Dir
   end record;

   type Project_Info_Access is access all Project_Info;

   package Project_Info_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Project_Info_Access,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      Hash            => Ada.Strings.Unbounded.Hash);
   --  Mapping from project name (as returned by GNATCOLL.Projects.Name) to
   --  Project_Info records. Project_Info records are owned by this map, and
   --  thus must be deallocated when maps are deallocated.

   type Main_To_Instrument is record
      CU_Name : Compilation_Unit_Name;
      --  Compilation unit of the main to instrument

      File : GNATCOLL.VFS.Virtual_File;
      --  Source file to instrument

      Prj_Info : Project_Info_Access;
      --  Reference to the Project_Info record corresponding to the project
      --  that owns the main to instrument.
   end record;

   package Main_To_Instrument_Vectors is new Ada.Containers.Vectors
     (Positive, Main_To_Instrument);

   type Instrumented_Unit_Info is record
      Filename : Ada.Strings.Unbounded.Unbounded_String;
      --  Name of the source file for this unit

      Prj_Info : Project_Info_Access;
      --  Reference to the Project_Info record corresponding to the project
      --  that owns the source file for this unit.

      Is_Main : Boolean;
      --  Whether this unit is a main

      Language : Any_Language;
      --  Language for this unit
   end record;

   type Instrumented_Unit_Info_Access is access all Instrumented_Unit_Info;

   package Instrumented_Unit_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Compilation_Unit_Name,
      Element_Type => Instrumented_Unit_Info_Access);

   Max_Get_From_File_Count : constant := 50;
   --  In addition to nodes and text buffers for each loaded unit, Libadalang
   --  maintains caches in Analysis_Context objects so that semantic queries
   --  are fast. This means that if we keep the same context to process a lot
   --  of units, we end up with excessive memory consumption, which can trigger
   --  heap exhaustion on big projects.
   --
   --  Replacing an analysis context with a new one clears all the caches, but
   --  makes semantic queries slower, as the units are re-loaded and caches are
   --  re-populated as needed.
   --
   --  To compromise between memory consumption and performance, we reset the
   --  analysis context each Max_Get_From_File_Count number of calls to
   --  Libadalang's Get_From_File function.

   type Inst_Context is limited record
      Project_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  Name of the root project. It is also used to name the list of buffers

      Tag : Ada.Strings.Unbounded.Unbounded_String;
      --  String which allows differentiating trace files produced by a
      --  specific program instrumentation from the ones produced by other
      --  instrumentations.
      --
      --  To achieve this, Tag is based on the date/time at which the
      --  instrumentation takes place. Automatic coverage buffer dumps (in
      --  instrumented mains) will pass this string to
      --  GNATcov_RTS.Traces.Output.Files.Default_Trace_Name (unless the
      --  --dump-trace-filename-simple option is passed to "gnatcov
      --  instrument").

      Provider : Libadalang.Analysis.Unit_Provider_Reference;
      --  Unit provider to create an analysis context (Context member below)

      Context : Libadalang.Analysis.Analysis_Context;
      --  Libadalang context to load all units to rewrite

      Get_From_File_Count : Natural;
      --  Count how many times we called Context.Get_From_File. See the
      --  Max_Get_From_File_Count constant.

      Dump_Config      : Any_Dump_Config;
      Language_Version : Any_Language_Version;
      --  See the eponym arguments in Instrument.Intrument_Units_Of_Interest

      Ignored_Source_Files_Present : Boolean;
      Ignored_Source_Files         : GNAT.Regexp.Regexp;
      --  If present, instrumentation will ignore files whose names match the
      --  accessed pattern.

      Instrumented_Units : Instrumented_Unit_Maps.Map;
      --  Mapping from instrumented unit names to information used during
      --  instrumentation.

      Project_Info_Map : Project_Info_Maps.Map;
      --  For each project that contains units of interest, this tracks a
      --  Project_Info record.
   end record;

   function Create_Context
     (Provider             : Libadalang.Analysis.Unit_Provider_Reference;
      Dump_Config          : Any_Dump_Config;
      Language_Version     : Any_Language_Version;
      Ignored_Source_Files : access GNAT.Regexp.Regexp) return Inst_Context;
   --  Create an instrumentation context for the currently loaded project

   procedure Destroy_Context (Context : in out Inst_Context);
   --  Free dynamically allocated resources in Context

   function Get_From_File
     (IC       : in out Inst_Context;
      Filename : String) return Libadalang.Analysis.Analysis_Unit;
   --  Fetch the analysis unit for the given filename

   function Is_Ignored_Source_File
     (Context : Inst_Context; Filename : String) return Boolean;
   --  Return whether the instrumentation process must ignore the Filename
   --  source file.

   function Get_Or_Create_Project_Info
     (Context : in out Inst_Context;
      Project : Project_Type) return Project_Info_Access;
   --  Return the Project_Info record corresponding to Project. Create it if it
   --  does not exist.

   function Unit_Info
     (CU_Name : Compilation_Unit_Name;
      Info    : out GNATCOLL.Projects.File_Info) return Boolean;
   --  Look for a compilation unit in the loaded project. If found, put its
   --  file info in Info and return True. Return False otherwise.

   procedure Register_Main_To_Instrument
     (Context : in out Inst_Context;
      Mains   : in out Main_To_Instrument_Vectors.Vector;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type)
      with Pre => Context.Dump_Config.Trigger /= Manual;
   --  Register in Mains a main to be instrumented so that it dumps coverage
   --  buffers. File is the source file for this main, and Project is the
   --  project that owns this main. If File is actually a unit of interest in
   --  Context, do nothing.

   procedure Add_Instrumented_Unit
     (Context     : in out Inst_Context;
      Project     : GNATCOLL.Projects.Project_Type;
      Source_File : GNATCOLL.Projects.File_Info);
   --  Add the given source file to the list of units to instrument

   procedure Create_File
     (Info : in out Project_Info;
      File : in out Text_Files.File_Type;
      Name : String);
   --  Shortcut to Text_Files.Create: create a text file with the given name in
   --  Info.Output_Dir and register it in Info.Instr_Files.
   --
   --  Name can be a basename, a relative name or an absolute one: in all
   --  cases, the basename is taken and the file is created in Info.Output_Dir.

   procedure Put_Warnings_And_Style_Checks_Pragmas
     (File : in out Text_Files.File_Type);
   --  Code generation helper: write "pragma Style_Checks (Off); pragma
   --  Warnings (Off);" to File.
   --
   --  This is useful when writing instrumented sources, as they may introduce
   --  warnings and break the original codebase's coding style, and since some
   --  projects are built with "warnings-as-errors" (GNAT's -gnatwe option),
   --  this could mean that instrumentation breaks the build. When written at
   --  the very beginning of each written source, these pragmas avoid this.

   -------------------------
   -- Source instrumenter --
   -------------------------

   type Source_Rewriter is tagged limited private;
   --  Helper object to instrument a source file

   procedure Start_Rewriting
     (Self           : out Source_Rewriter;
      IC             : in out Inst_Context;
      Info           : in out Project_Info;
      Input_Filename : String);
   --  Start a rewriting session for the given Input_Filename. If the rewriting
   --  process is successful, the result will be written to a file in
   --  Info.Output_Dir with the basename of Output_Filename.
   --
   --  This registers the output file in Info.Instr_Files.
   --
   --  If there are parsing errors while reading Input_Filename, this raises a
   --  fatal error and prints the corresponding error messages.

   function Rewritten_Unit
     (Self : Source_Rewriter) return Libadalang.Analysis.Analysis_Unit;
   --  Return the analysis unit for the source that Self instruments

   procedure Apply (Self : in out Source_Rewriter);
   --  Write the instrumented source to the filename passed as Output_Filename
   --  to Start_Rewriting. If rewriting failed, raise a fatal error and print
   --  the corresponding error message.

   -----------------
   -- Checkpoints --
   -----------------

   --  Note: the following procedures must be called after the SCO units
   --  table has been saved/loaded.

   procedure Checkpoint_Save (CSS : access Checkpoints.Checkpoint_Save_State);
   --  Save the current instrumented units map to stream

   procedure Checkpoint_Clear;
   --  Clear the internal data structures used to create checkpoints

   procedure Checkpoint_Load (CLS : access Checkpoints.Checkpoint_Load_State);
   --  Load checkpointed instrumented unit map from stream and merge them in
   --  current state.

   ------------------------------------------------------
   --  Common declarations for Ada / C instrumentation --
   ------------------------------------------------------

   --  All documentation below refer to "qualified names". This only makes
   --  sense in Ada.
   --  TODO??? Adjust the semantics when C instrumentation is implemented.

   type Instrumentation_Entities is record
      Common_Buffers : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
      --  Qualified name for the unit that contains coverage buffer types and
      --  witness subprograms.

      Unit_Buffers : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
      --  Qualified name for the unit that contains addresses to coverage
      --  buffers (Pure_Buffer_Unit).

      Statement_Buffer : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
      --  Qualified name for the buffer corresponding to statement coverage
      --  obligations.

      Decision_Buffer : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
      --  Qualified name for the buffer corresponding to decision coverage
      --  obligations.

      MCDC_Buffer : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
      --  Qualified name for the buffer corresponding to paths in decision
      --  BDDs.
   end record;

   -----------------------------
   -- Bit allocation tracking --
   -----------------------------

   --  Bitmap information for statements:
   --  One bit witnessing "statement executed"

   type Statement_Bit_Ids is record
      LL_S_SCO : Nat;
      Executed : Bit_Id;
   end record;

   package LL_Statement_SCO_Bit_Allocs is
      new Ada.Containers.Vectors (Nat, Statement_Bit_Ids);

   --  Bitmap information for decisions:
   --  One bit witnessing each outcome

   type Outcome_Bit_Ids is array (Boolean) of Any_Bit_Id;
   No_Outcome_Bit_Ids : constant Outcome_Bit_Ids := (others => No_Bit_Id);

   type Decision_Bit_Ids is record
      LL_D_SCO       : Nat;

      Outcome_Bits   : Outcome_Bit_Ids := No_Outcome_Bit_Ids;
      Path_Bits_Base : Any_Bit_Id := No_Bit_Id;
   end record;

   package LL_Decision_SCO_Bit_Allocs is
     new Ada.Containers.Vectors (Nat, Decision_Bit_Ids);

   type LL_Unit_Bit_Allocs is record
      Statement_Bits     : LL_Statement_SCO_Bit_Allocs.Vector;
      Last_Statement_Bit : Any_Bit_Id := No_Bit_Id;

      Decision_Bits    : LL_Decision_SCO_Bit_Allocs.Vector;
      Last_Outcome_Bit : Any_Bit_Id := No_Bit_Id;
      Last_Path_Bit    : Any_Bit_Id := No_Bit_Id;
   end record;

   -----------------------------
   -- Instrumentation context --
   -----------------------------

   --  This is the global state for the process of instrumenting a compilation
   --  unit.

   --  Decisions and conditions are not instrumented during the initial tree
   --  traversal, but after high-level SCOs have been generated, because for
   --  MC/DC instrumentation depends on BDD information.

   type Source_Decision is record
      LL_SCO : Nat;
      --  Low-level SCO id of decision

      Decision : Expr;
      --  Decision expression

      State : Unbounded_String;
      --  Name of MC/DC state local variable
   end record;

   type Source_Condition is record
      LL_SCO : Nat;
      --  Low-level SCO id of condition

      Condition : Expr;
      --  Condition expression

      State : Unbounded_String;
      --  Name of MC/DC state local variable

      First : Boolean;
      --  True if this condition is the first one in its decision
   end record;

   package Source_Decision_Vectors is
     new Ada.Containers.Vectors (Natural, Source_Decision);
   package Source_Condition_Vectors is
     new Ada.Containers.Vectors (Natural, Source_Condition);

   type Annotation_Couple is record
      Sloc       : Source_Location;
      Annotation : ALI_Annotation;
   end record;
   --  When instrumenting sources, annotations are registred in two steps:
   --
   --  * collect couples of sloc/annotations during the tree traversal;
   --  * once the CU_Id for the instrumented file is known, fill in the
   --    Annotation.CU component and add the sloc/annotation couple to
   --    ALI_Files.ALI_Annotation map.
   --
   --  This record type is just a helper to hold data between these two steps.

   package Annotation_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Annotation_Couple);

   type Unit_Inst_Context is tagged record
      Instrumented_Unit : Compilation_Unit_Name;
      --  Name of the compilation unit being instrumented

      Language_Version_Pragma : Unbounded_Wide_Wide_String;
      --  Language version configuration pragma for unit, if any

      SFI : Source_File_Index := No_Source_File;
      --  Source file index of the compilation unit being instrumented

      CU : CU_Id := No_CU_Id;
      --  SCO identifier of the compilation unit being instrumented

      Buffer_Unit : Compilation_Unit_Name;
      --  Name of the compilation unit that holds coverage buffers for the
      --  unit currently being instrumented (see Common.Buffer_Unit).

      Pure_Buffer_Unit : Compilation_Unit_Name;
      --  Name of the compilation unit that holds addresses for the coverage
      --  buffers of the unit being instrumented (see Common.Pure_Buffer_Unit).

      Unit_Bits : LL_Unit_Bit_Allocs;
      --  Record of allocation of coverage buffer bits for low-level SCOs

      Source_Decisions  : Source_Decision_Vectors.Vector;
      Source_Conditions : Source_Condition_Vectors.Vector;
      --  Decisions and (for MC/DC) conditions to be instrumented

      Entities : Instrumentation_Entities;
      --  Bank of nodes to use during instrumentation

      Annotations : Annotation_Vectors.Vector;
      --  Annotations created during the instrumentation process, to insert in
      --  ALI_Files.ALI_Annotations afterwards, when the compilation unit
      --  (SC_Obligations.CU_Info) for this annotation is ready.
   end record;

   function Img (Bit : Any_Bit_Id) return String is
     (Strings.Img (Integer (Bit)));

   Runtime_Version_Check : constant String :=
     "pragma Compile_Time_Error (GNATcov_RTS.Version /="
     & GNATcov_RTS.Version'Image
     & ",""Incompatible GNATcov_RTS version, please use"
     & " the GNATcov_RTS project provided with your"
     & " GNATcoverage distribution."");";
   --  Check to be inserted in Ada units generated by the instrumenter to
   --  verify that they are built with the version of GNATcov_RTS the
   --  instrumenter expects.

   procedure Append_SCO
     (C1, C2             : Character;
      From, To           : Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name);
   --  Append a new entry to the low-level SCO table

private

   type Source_Rewriter is limited new Ada.Finalization.Limited_Controlled with
   record
      Input_Filename  : Ada.Strings.Unbounded.Unbounded_String;
      Output_Filename : Ada.Strings.Unbounded.Unbounded_String;

      Unit   : Libadalang.Analysis.Analysis_Unit;
      Handle : Libadalang.Rewriting.Rewriting_Handle;
   end record;

   overriding procedure Finalize (Self : in out Source_Rewriter);

end Instrument.Common;
