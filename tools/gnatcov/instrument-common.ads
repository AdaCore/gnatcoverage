------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2022, AdaCore                     --
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

--  Common operations for source instrumentation-based coverage
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
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Rewriting; use Libadalang.Rewriting;

with ALI_Files;             use ALI_Files;
with Files_Table;           use Files_Table;
with GNATcov_RTS;
with GNATcov_RTS.Buffers;   use GNATcov_RTS.Buffers;
with Instrument.Base_Types; use Instrument.Base_Types;
with Namet;                 use Namet;
with SC_Obligations;        use SC_Obligations;
with Slocs;                 use Slocs;
with Strings;               use Strings;
with Switches;              use Switches;
with Text_Files;
with Types;                 use Types;

package Instrument.Common is

   pragma Elaborate_Body;

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

   Witness_Dummy_Type_Name : Ada_Qualified_Name;
   --  Qualified name for the type returned by the Witness function used for
   --  declaration instrumentation.

   Dump_Procedure_Name : constant Ada_Identifier :=
      To_Unbounded_String ("Dump_Buffers");
   --  Name of the procedure (in main dump helper packages) that dumps all
   --  coverage buffers to the source trace file.

   Register_Dump_Function_Name : constant Ada_Identifier :=
      To_Unbounded_String ("Register_Dump_Buffers");
   --  Name of the function (in main dump helper packages) that registers the
   --  coverage buffers dump through atexit(3) or through a task termination
   --  handler.

   function Dump_Procedure_Symbol
     (Main : Compilation_Unit_Name) return String
   is
     ("gnatcov_rts_" & Instrumented_Unit_Slug (Main) & "_"
      & To_Lower (To_String (Dump_Procedure_Name)));
   --  Return the name of the exported symbol for the Dump_Buffers function

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
   --  its coverage buffers (Coverage_Buffer_Type and
   --  GNATcov_RTS_Coverage_Buffers records).

   function Unit_Buffers_Name (Unit : Compilation_Unit_Name) return String;
   --  Name of the symbol that references the
   --  gnatcov_rts_coverage_buffers_group struct for this unit.

   function Project_Output_Dir (Project : Project_Type) return String;
   --  Return the directory in which we must create instrumented sources for
   --  Project. This retuns an empty strings for projects that do not have an
   --  object directory.

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

      Language : Some_Language;
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

      Event_Handler : Libadalang.Analysis.Event_Handler_Reference;
      --  Event handler to warn about missing source files

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
      Event_Handler        : Libadalang.Analysis.Event_Handler_Reference;
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

   function Register_New_File
     (Info : in out Project_Info; Name : String) return String;
   --  Helper for Create_File and Start_Rewriting: compute the path to the file
   --  to create and register it to Info.Instr_Files.

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

   function Create_Missing_File_Reporter
     return Libadalang.Analysis.Event_Handler_Reference;
   --  Create an event handler to warn about source files that Libadalang needs
   --  to perform semantic analysis (so mandated by Ada), but which are not
   --  available.

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

   ------------------------------------------------------
   --  Common declarations for Ada / C instrumentation --
   ------------------------------------------------------

   -----------------------------
   -- Bit allocation tracking --
   -----------------------------

   --  Bitmap information for statements:
   --  One bit witnessing "statement executed"

   type Statement_Bit_Ids is record
      LL_S_SCO : Nat;
      --  Low-level SCO for this statement

      Executed : Bit_Id;
      --  Corresponding bit in the "statement" coverage buffer
   end record;

   package LL_Statement_SCO_Bit_Allocs is
      new Ada.Containers.Vectors (Nat, Statement_Bit_Ids);

   --  Bitmap information for decisions:
   --  One bit witnessing each outcome

   type Outcome_Bit_Ids is array (Boolean) of Any_Bit_Id;
   No_Outcome_Bit_Ids : constant Outcome_Bit_Ids := (others => No_Bit_Id);

   type Decision_Bit_Ids is record
      LL_D_SCO : Nat;
      --  Low-level SCO for this decision

      Outcome_Bits : Outcome_Bit_Ids := No_Outcome_Bit_Ids;
      --  Bits in the "decision" coverage buffer, corresponding to the False
      --  and True outcomes.

      Path_Bits_Base : Any_Bit_Id := No_Bit_Id;
      --  First bit in the "mcdc" coverage buffer, corresponding to the first
      --  path in the BDD. The other paths for the BDD are associated to the
      --  bits that follow it.
   end record;

   package LL_Decision_SCO_Bit_Allocs is
     new Ada.Containers.Vectors (Nat, Decision_Bit_Ids);

   type Allocated_Bits is record
      SFI : Valid_Source_File_Index;
      --  Source file index for the SCOs associated to these coverage buffers

      Statement_Bits     : LL_Statement_SCO_Bit_Allocs.Vector;
      Last_Statement_Bit : Any_Bit_Id := No_Bit_Id;

      Decision_Bits    : LL_Decision_SCO_Bit_Allocs.Vector;
      Last_Outcome_Bit : Any_Bit_Id := No_Bit_Id;
      Last_Path_Bit    : Any_Bit_Id := No_Bit_Id;
   end record;
   --  Vectors of coverage buffer allocated bits for the low-level SCOs of a
   --  given source file.

   function Allocate_Statement_Bit
     (Unit_Bits : in out Allocated_Bits; LL_S_SCO : Nat) return Any_Bit_Id;
   --  Allocate a bit for a statement in the coverage buffers referenced by
   --  Unit_Bits and return its index.
   --
   --  LL_S_SCO must be the low-level SCO for that statement.

   function Allocate_Decision_Bits
     (Unit_Bits      : in out Allocated_Bits;
      Decision_Sloc  : Source_Location;
      LL_D_SCO       : Nat;
      State_Variable : Unbounded_String;
      Path_Count     : Natural) return Decision_Bit_Ids;
   --  Allocate bits for a decision ("decision" bits, and optionally "path"
   --  bits) in the coverage buffers referenced by Unit_Bits.
   --
   --  LL_D_SCO must be the low-level SCO for that decision and Decision_Sloc
   --  must be its location (used for diagnostics).
   --
   --  State_Variable is the local state variable: if it is empty, it means
   --  that we were not able to generate one, and thus that we are unable to
   --  compute MC/DC on this decision: no path bits are allocated in this case.
   --
   --  Path_Count is the number of paths in this decision. If the number of
   --  paths exceeds the limit, must be 0: this function emits a warning in
   --  this case.

   package Allocated_Bits_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Allocated_Bits);
   --  Allocated bits in coverage buffers for low-level SCOs (one per source
   --  file). Because of #include directives in C/C++, a single compilation
   --  unit may yield multiple sets of coverage buffers: one for the compiled
   --  source file, one for each included source.

   function Create_Unit_Bits
     (Allocated_Bits : in out Allocated_Bits_Vectors.Vector;
      SFI            : Valid_Source_File_Index) return Positive;
   --  Allocate a new set of coverage buffers for the given source file. Return
   --  the index for the newly created set of buffers.

   -----------------------------
   -- Instrumentation context --
   -----------------------------

   --  This is the global state for the process of instrumenting a compilation
   --  unit.

   --  Decisions and conditions are not instrumented during the initial tree
   --  traversal, but after high-level SCOs have been generated, because for
   --  MC/DC instrumentation depends on BDD information.

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

      Annotations : Annotation_Vectors.Vector;
      --  Annotations created during the instrumentation process, to insert in
      --  ALI_Files.ALI_Annotations afterwards, when the compilation unit
      --  (SC_Obligations.CU_Info) for this annotation is ready.

      Current_Scope_Entity : Scope_Entity_Acc := null;
      --  Information about the name, sloc, SCO range and children scopes of
      --  the current scope entity. This is modified when entering a scope
      --  (updated to the current scope), and when leaving it (updated to the
      --  current scope parent, if any).

   end record;

   procedure Import_Annotations
     (UIC : in out Unit_Inst_Context; Created_Units : Created_Unit_Maps.Map);
   --  Import ALI annotations for this unit in the global annotations table.
   --  This should be called once the unit was instrumented and its low level
   --  SCOS have been transformed into high-level ones.

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
      From, To           : Local_Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name);
   --  Append a new entry to the low-level SCO table

   package CU_Name_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Compilation_Unit_Name);

   package Ada_Qualified_Name_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Ada_Qualified_Name,
      "="          => Ada_Identifier_Vectors."=");

   function Instr_Units_For_Closure
     (IC   : Inst_Context;
      Main : Compilation_Unit_Name) return CU_Name_Vectors.Vector;
   --  Return the list of instrumented units in Main's closure

   procedure Enter_Scope
     (UIC        : in out Unit_Inst_Context;
      Scope_Name : Unbounded_String;
      Sloc       : Local_Source_Location);
   --  Enter a scope. This must be completed with a call to the function
   --  Exit_Scope, defined below. Scope_Name is the name of the scope, which
   --  is defined at location Sloc. Assume that the scope first SCO is the next
   --  generated SCO (SCOs.SCO_Table.Last + 1). Update UIC.Current_Scope_Entity
   --  to the created entity.

   procedure Exit_Scope (UIC : in out Unit_Inst_Context)
     with Pre => UIC.Current_Scope_Entity /= null;
   --  Exit the current scioe, updating UIC.Current_Scope_Entity to
   --  UIC.Current_Scope_Entity.Parent, if any. Assume that the last generated
   --  SCO (SCOs.SCO_Table.Last) is the last SCO for the current scope.

   type Language_Instrumenter is abstract tagged null record;
   --  Set of operations to allow the instrumentation of sources files for a
   --  given language.

   function Language
     (Self : Language_Instrumenter) return Src_Supported_Language is abstract;
   --  Return the language that this instrumenter is designed to process

   function Skip_Source_File
     (Self        : Language_Instrumenter;
      Source_File : GNATCOLL.Projects.File_Info) return Boolean
   is (False);
   --  Whether the instrumenter skips the given source file.
   --
   --  There is currently only one case where this is needed: the C
   --  instrumenter must skip header files, as it instruments only bodies (.c
   --  files).

   procedure Instrument_Unit
     (Self      : Language_Instrumenter;
      CU_Name   : Compilation_Unit_Name;
      IC        : in out Inst_Context;
      Unit_Info : in out Instrumented_Unit_Info) is abstract;
   --  Instrument a single source file for the language that Self supports.
   --
   --  CU_Name must be the name of the compilation unit for this source file,
   --  and Unit_Info must be the Instrumented_Unit_Info record corresponding to
   --  this source file.

   procedure Auto_Dump_Buffers_In_Main
     (Self     : Language_Instrumenter;
      IC       : in out Inst_Context;
      Main     : Compilation_Unit_Name;
      Filename : String;
      Info     : in out Project_Info) is abstract;
   --  Try to instrument the Main/Filename source file (whose language is
   --  assumed to be Self's) to insert a call to dump the list of coverage
   --  buffers for all units of interest in Main's closure. Do nothing if not
   --  successful.
   --
   --  Info must be the Project_Info record corresponding to the project that
   --  owns the Main unit.

   procedure Emit_Buffers_List_Unit
     (Self              : Language_Instrumenter;
      IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info) is abstract;
   --  Emit in the root project a unit (in Self's language) to contain the list
   --  of coverage buffers for all units of interest.

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
