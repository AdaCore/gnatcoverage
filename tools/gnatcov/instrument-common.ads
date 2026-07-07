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
--    (<SLUG> is the result of Filename_Slug for the compilation unit to
--    instrument). These units are preelaborated and contain definitions for
--    the coverage buffers themselves, but also for buffers "metadata".
--
--  * "gnatcov_rts-buffers-P<SLUG>.ads" for all compilation units of interest
--    (<SLUG> is the result of Filename_Slug for the compilation unit to
--    instrument). These units are pure and contain one System.Address constant
--    per coverage buffer for the corresponding compilation unit.
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

with GNAT.Regexp; use GNAT.Regexp;
with GNAT.Regpat; use GNAT.Regpat;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.VFS;  use GNATCOLL.VFS;
with GPR2.Project.View;

with GPR2.Build.Source;
with GPR2.Path_Name;

with Files_Handling; use Files_Handling;
with Files_Table;    use Files_Table;
with Namet;          use Namet;
with Slocs;          use Slocs;
with Text_Files;

package Instrument.Common is

   pragma Elaborate_Body;

   Sys_Prefix : Ada_Qualified_Name;
   --  Scope for all instrumentation runtime files beyond the instrumented
   --  sources.

   Sys_Buffers : Ada_Qualified_Name;
   --  Runtime package containing coverage buffer type definitions

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

   Reset_Procedure_Name : constant Ada_Identifier :=
     To_Unbounded_String ("Reset_Buffers");
   --  Name of the procedure (in main dump helper packages) that resets all the
   --  coverage buffers accessible from that dump helper package (i.e. the
   --  whole project tree, rooted at the project to which the helper unit
   --  belongs).

   Register_Dump_Function_Name : constant Ada_Identifier :=
     To_Unbounded_String ("Register_Dump_Buffers");
   --  Name of the function (in main dump helper packages) that registers the
   --  coverage buffers dump through atexit(3) or through a task termination
   --  handler.

   function Dump_Procedure_Symbol
     (Main     : String;
      Manual   : Boolean := False;
      Prj_Name : Ada_Qualified_Name := Ada_Identifier_Vectors.Empty_Vector)
      return String
   is ("gnatcov_rts_"
       & (if Manual then "manual" else Filename_Slug (Main))
       & "_"
       & To_Lower (To_String (Dump_Procedure_Name))
       & (if Manual then "_" & To_Symbol_Name (Prj_Name) else ""));
   --  Return the name of the exported symbol for the Dump_Buffers function

   function Reset_Procedure_Symbol
     (Prj_Name : Ada_Qualified_Name) return String
   is ("gnatcov_rts_"
       & To_Lower (To_String (Reset_Procedure_Name))
       & "_"
       & To_Symbol_Name (Prj_Name));
   --  Return the name of the exported symbol for the Reset_Buffers procedure

   function Is_Manual_Indication_Procedure_Symbol
     (Symbol : String) return Boolean;
   --  For C, manual dump/reset procedures are suffixed by the project's name.
   --  Check that Symbol corresponds to the name of one such procedure.

   function Statement_Buffer_Symbol
     (Instrumented_Unit : Unbounded_String) return String;
   --  Given a unit to instrument, return the name of the symbol to use for the
   --  entity that contains address of the statement coverage buffer.

   function Decision_Buffer_Symbol
     (Instrumented_Unit : Unbounded_String) return String;
   --  Given a unit to instrument, return the name of the symbol to use for the
   --  entity that contains address of the decision coverage buffer.

   function MCDC_Buffer_Symbol
     (Instrumented_Unit : Unbounded_String) return String;
   --  Given a unit to instrument, return the name of the symbol to use for the
   --  entity that contains address of the decision coverage buffer.

   function Unit_Buffers_Name (Unit : Compilation_Unit) return String;
   --  Name of the symbol that references the
   --  gnatcov_rts_coverage_buffers_group struct for this file.

   function Unit_Buffers_Array_Name
     (Prj_Name : Ada_Qualified_Name) return String
   is ("gnatcov_rts_buffers_array_" & To_Symbol_Name (Prj_Name));
   --  Name of the symbol that designates the
   --  gnatcov_rts_coverage_buffers_array struct, which contains an array of
   --  coverage buffers for all instrumented units in this project.
   --
   --  We need this to be unique per root project instrumented, as gnatcov
   --  gives the possibility to link two separately-instrumented libraries in
   --  the same executable.

   function Project_Output_Dir
     (Project : GPR2.Project.View.Object; In_Extending : Boolean := True)
      return GNATCOLL.VFS.Virtual_File;
   --  Return the directory in which we must create instrumented sources for
   --  Project. This returns an empty string for projects that do not have an
   --  object directory.
   --
   --  If In_Extending is False, return systematically the instrumentation
   --  directory for Project, otherwise consider the most extending project.

   function Format_Fingerprint
     (Fingerprint : SC_Obligations.Fingerprint_Type; Opening, Closing : String)
      return String;
   --  Somewhat language agnostic formatter for fingerprint values in generated
   --  code.
   --
   --  Opening and Closing are strings used at the beginning and the end of the
   --  returned literal expression (aggregate in Ada, compound expression in
   --  C/C++).

   type Instrumented_Unit_Info is record
      Filename : Unbounded_String;
      --  Name of the source file for this unit

      Language : Some_Language;
      --  Language for this unit
   end record;

   type Instrumented_Unit_Info_Access is access all Instrumented_Unit_Info;

   package Instrumented_Unit_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Unbounded_String,
        Element_Type => Instrumented_Unit_Info_Access);

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

   package LL_Statement_SCO_Bit_Allocs is new
     Ada.Containers.Vectors (Nat, Statement_Bit_Ids);

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

   package LL_Decision_SCO_Bit_Allocs is new
     Ada.Containers.Vectors (Nat, Decision_Bit_Ids);

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

   package Allocated_Bits_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Allocated_Bits);
   --  Allocated bits in coverage buffers for low-level SCOs (one per source
   --  file). A single compilation unit may yield multiple sets of coverage
   --  buffers: one for each part of the unit in the case of unit-based
   --  language units (the spec, the body, and the separates). It may also be
   --  the case for unit of file-based languages: one for the compiled source
   --  file and one for each included source (e.g. through inclusion directives
   --  for C and C++).

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
   --    the ALI_Annotation map of the right SC_Obligations.CU_Vector entry.
   --
   --  This record type is just a helper to hold data between these two steps.

   package Annotation_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Annotation_Couple);

   package Sloc_Range_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Source_Location_Range);

   package Nat_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Nat);

   type Unit_Inst_Context is tagged record
      Instrumented_Unit : Unbounded_String;
      --  Absolute filename for the source that is being instrumented

      SFI : Source_File_Index := No_Source_File;
      --  Source file index of the compilation unit being instrumented

      Fullname : Unbounded_String;
      --  Fullname of the compilation unit being instrumented

      Annotations : Annotation_Vectors.Vector;
      --  Annotations created during the instrumentation process, to insert in
      --  CU_Info.ALI_Annotations afterwards.

      Disable_Instrumentation : Boolean := False;
      --  Set to True to deactivate instrumentation and prevent any code
      --  rewriting.

      Disable_Coverage : Boolean := False;
      --  Set to True to deactivate instrumentation and SCO emissions,
      --  regardless of the contents of Disable_Cov_Region.

      Non_Instr_LL_SCOs : SCO_Sets.Set;
      --  Set of low level SCO ids that were not instrumented

      True_Static_LL_SCOs  : SCO_Sets.Set;
      False_Static_LL_SCOs : SCO_Sets.Set;
      --  Keeps track of static decision SCOs. Static SCOs are registered
      --  identically as non-static ones, to permit consolidation when
      --  an expression is static in some cases only (e.g. endianness
      --  depending on system architecture)

      Blocks : SCO_Id_Vector_Vector;
      --  This is used when the block coverage level is enabled: list of blocks

      Disable_Cov_Regions : Sloc_Range_Vectors.Vector;
      --  List of regions where coverage is disabled, as delimited by
      --  GNATCOV_COV_OFF/ON markers or external annotations.

   end record;

   procedure Import_Annotations
     (UIC : in out Unit_Inst_Context; Created_Units : Created_Unit_Maps.Map);
   --  Import ALI annotations for this unit in the global annotations table.
   --  This should be called once the unit was instrumented and its low level
   --  SCOS have been transformed into high-level ones.

   procedure Import_Non_Instrumented_LL_SCOs
     (UIC : Unit_Inst_Context; SCO_Map : LL_HL_SCO_Map);
   --  Import the low level SCO in UIC marked as non-instrumented in the high
   --  level non-instrumented SCO_Id sets.

   function Is_Disabled_Region
     (UIC : Unit_Inst_Context; Sloc : Source_Location) return Boolean;
   --  Return True if Sloc lies within one of the disabled regions in
   --  UIC.Disable_Cov_Region.

   function Img (Bit : Any_Bit_Id) return String
   is (Strings.Img (Integer (Bit)));

   Runtime_Version : constant Natural := 12;
   Runtime_Error   : constant String :=
     "Incompatible GNATcov_RTS version, please use"
     & " the GNATcov_RTS project provided with your"
     & " GNATcoverage distribution.";
   --  Must be up-to-date with GNATcov_RTS.Version

   Ada_Runtime_Version_Check : constant String :=
     "pragma Compile_Time_Error (GNATcov_RTS.Version /= "
     & Img (Runtime_Version)
     & " ,"""
     & Runtime_Error
     & """);";
   C_Runtime_Version_Check   : constant String :=
     "#if GNATCOV_RTS_VERSION != "
     & Img (Runtime_Version)
     & ASCII.LF
     & "#error """
     & Runtime_Error
     & """"
     & ASCII.LF
     & "#endif";
   --  Check to be inserted in Ada / C units generated by the instrumenter to
   --  verify that they are built with the version of GNATcov_RTS the
   --  instrumenter expects.
   --
   --  The hardcoded version number must be updated in synchronization with the
   --  Version constant in gnatcov_rts.ads. It is fine to forget one or the
   --  other: the testsuite will not work without this.

   procedure Append_Unit (SFI : Source_File_Index);
   --  Append a new entry to the SCO_Unit_Table, with an empty SCO range

   procedure Append_SCO
     (C1, C2             : Character;
      From, To           : Local_Source_Location;
      SFI                : Source_File_Index;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name);
   --  Append a new entry to the low-level SCO table. If SFI designates a new
   --  source file, also append a new entry to the SCO_Unit_Table, otherwise
   --  complete its last entry.

   procedure Remap_Blocks
     (Blocks : in out SCO_Id_Vector_Vector; SCO_Map : LL_HL_SCO_Map);
   --  Convert low level SCOs in Blocks to high-level SCOs using the
   --  mapping in SCO_Map.

   package Ada_Qualified_Name_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Ada_Qualified_Name,
        "="          => Ada_Identifier_Vectors."=");

   type Language_Instrumenter is abstract tagged record
      Tag : Unbounded_String;
      --  Tag specific to an instrumentation run
   end record;
   --  Set of operations to allow the instrumentation of sources files for a
   --  given language.

   type Language_Instrumenter_Acc is access all Language_Instrumenter'Class;

   function Language
     (Self : Language_Instrumenter) return Src_Supported_Language
   is abstract;
   --  Return the language that this instrumenter is designed to process

   function Language_Name (Self : Language_Instrumenter'Class) return String
   is (Image (Self.Language));

   procedure Instrument_Unit
     (Self               : in out Language_Instrumenter;
      Unit_Name          : String;
      Prj                : in out Prj_Desc;
      Files_Of_Interest  : File_Sets.Set;
      Instrumented_Files : File_Sets.Set)
   is null;
   --  Instrument a single source file for the language that Self supports.
   --
   --  Unit_Name identifies this compilation unit (either through a unit name,
   --  for Unit_Based_Language, or through a full name, for file-based). Prj
   --  describes information coming from the project, and relevant to the
   --  instrumentation of the unit. Files_Of_Interest provides the list of
   --  files of interest, to ignore e.g. part of the unit (e.g. a separate)
   --  when instrumenting it.
   --
   --  Instrumented indicates whether the unit was already instrumented or not.
   --  Practically speaking, this only happens for C/C++ units when using the
   --  manual dump trigger.

   procedure Auto_Dump_Buffers_In_Main
     (Self        : in out Language_Instrumenter;
      Filename    : GNATCOLL.VFS.Virtual_File;
      Dump_Config : Any_Dump_Config;
      Prj         : in out Prj_Desc)
   is null;
   --  Try to instrument the Filename source file (whose language is assumed
   --  to be Self's) to insert a call to dump the list of coverage buffers,
   --  assumed to be named after Prj.Prj_Name. Do nothing if not successful.
   --  Instrumented is set to True when the source was instrumented for
   --  source instrumentation purposes.

   procedure Emit_Buffers_List_Unit
     (Self        : Language_Instrumenter;
      Instr_Units : Unit_Sets.Set;
      Prj         : in out Prj_Desc)
   is null;
   --  Emit in the root project a unit (in Self's language) to contain the list
   --  of coverage buffers for the given instrumented files.
   --
   --  The variable holding the list of coverage buffers is exported to a
   --  unique C symbol whose name is defined by the Unit_Buffers_Array_Name
   --  function. This procedure should thus be called only once, for one of
   --  the supported languages of the project.

   function Emit_Buffers_List_Unit
     (Self           : Language_Instrumenter;
      Buffer_Symbols : String_Sets.Set;
      Prj            : in out Prj_Desc) return Compilation_Unit;
   --  Same as above except Buffer_Symbols contains the list of C symbols
   --  holding coverage buffers for units of interest. Return the buffers list
   --  compilation unit.

   function Buffer_Unit
     (Self : Language_Instrumenter; CU : Compilation_Unit; Prj : Prj_Desc)
      return Compilation_Unit
   is (No_Compilation_Unit);
   --  Return the compilation unit holding coverage buffers. This is especially
   --  useful for integrated instrumentation.

   function Dump_Manual_Helper_Unit
     (Self : Language_Instrumenter; Prj : Prj_Desc) return Compilation_Unit
   is (No_Compilation_Unit);

   function Dump_Helper_Unit
     (Self : Language_Instrumenter; CU : Compilation_Unit; Prj : Prj_Desc)
      return Compilation_Unit
   is (No_Compilation_Unit);
   --  Return the compilation unit holding the dump helper subprogram

   function Has_Main
     (Self     : in out Language_Instrumenter;
      Filename : GNATCOLL.VFS.Virtual_File;
      Prj      : in out Prj_Desc) return Boolean;
   --  Return whether the given file is a main or not
   --
   --  Note that for C/C++, it preprocesses the file.

   procedure Emit_Dump_Helper_Unit_Manual
     (Self        : in out Language_Instrumenter;
      Dump_Config : Any_Dump_Config;
      Prj         : in out Prj_Desc)
   is null;
   --  Emit the dump helper unit with the appropriate content to allow for a
   --  simple call to a procedure dumping the coverage buffers to be made in
   --  the instrumented source files.

   procedure Replace_Manual_Indications
     (Self                 : in out Language_Instrumenter;
      Prj                  : in out Prj_Desc;
      Source               : Virtual_File;
      Has_Dump_Indication  : out Boolean;
      Has_Reset_Indication : out Boolean);
   --  Look for the pragmas (for Ada) or comments (for C family languages)
   --  indicating where the user wishes to the buffers to be dumped and/or
   --  reset in Source.
   --  When found, replace it with a call to the buffers dump/reset procedure
   --  defined in the dump helper unit.
   --
   --  Has_Dump_Indication indicates whether a manual dump indication was
   --  found - and replaced with a call to dump buffers - in the given source.
   --
   --  Likewise, Has_Reset_Indication indicates whether a manual buffer reset
   --  indication was found and processed.
   --
   --  We expect this primitive to be overridden by actual instrumenters, but
   --  not stub instrumenters (e.g. the one declared in instrument-c__stub) in
   --  which case it will raise a Program_Error.

   procedure Emit_Observability_Unit
     (Self : in out Language_Instrumenter; Prj : in out Prj_Desc)
   is null;
   --  Emit in the root project a file that exposes live coverage observability
   --  features, such as the number of bits set in the buffers.

   function Instrumentation_File
     (Prj : Prj_Desc; File : GNATCOLL.VFS.Virtual_File)
      return GNATCOLL.VFS.Virtual_File;
   --  Compute the path to the file to create in the instrumentation directory
   --  of the given File.

   function Is_Instrumented_File
     (Prj : Prj_Desc; File : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Return whether the given file identified by its fullname is in the
   --  instrumentation directory.

   function Get_Main_File
     (Self : Language_Instrumenter; Unit_Name : String) return Virtual_File
   is (GNATCOLL.VFS.No_File);
   --  Retrieve the main file for the given unit, i.e. the body if it exists,
   --  the specification otherwise.

   procedure For_All_Part
     (Self      : in out Language_Instrumenter;
      Unit_Name : String;
      Process   : access procedure (Filename : Virtual_File))
   is null;
   --  Call process for every part of the given unit, including separates

   procedure Create_File
     (Prj  : in out Prj_Desc;
      File : in out Text_Files.File_Type;
      Name : String);
   --  Shortcut to Text_Files.Create: create a text file with the given name in
   --  Prj.Output_Dir.
   --
   --  Name can be a basename, a relative name or an absolute one: in all
   --  cases, the basename is taken and the file is created in Prj.Output_Dir.
   --
   --  In addition to that, the file is added to Prj.Instrumentation_Artifacts.

   type Macro_Definition (Define : Boolean := True) is record
      Name : Unbounded_String;
      --  Name of the macro

      case Define is
         when True =>
            Args : Unbounded_String;
            --  String representation of the macro arguments, e.g. (x, y)

            Value : Unbounded_String;
            --  Value for the macro definition

         when False =>
            null;
      end case;
   end record;
   --  Whether a macro should be defined, its name, and when it must be
   --  defined, its optional arguments and value.

   function "<" (L, R : Macro_Definition) return Boolean
   is (L.Name < R.Name);
   function "=" (L, R : Macro_Definition) return Boolean
   is (L.Name = R.Name);
   --  As we store Macro_Definition in sets, we do not want two conflicting
   --  definitions of the same macro to coexist. Thus, we compare only the
   --  name, meaning that when we insert a new definition, it will replace
   --  the previous one.

   package Macro_Sets is new Ada.Containers.Ordered_Sets (Macro_Definition);
   subtype Macro_Set is Macro_Sets.Set;

   function Find (Self : Macro_Set; Name : String) return Macro_Sets.Cursor
   is (Self.Find (Macro_Definition'(Name => +Name, others => <>)));

   type Analysis_Options is record
      PP_Search_Path : String_Vectors.Vector;
      --  List of directories to search when looking for an included file

      Include_Files : String_Vectors.Vector;
      --  Header files passed through a -include switch

      Builtin_Macros : Macro_Set;
      --  Set of predefined macros for the project compiler driver

      PP_Macros : Macro_Set;
      --  Set of macros for the preprocessor

      Compiler_Switches : String_Vectors.Vector;
      --  List of compiler switches that can influence the file preprocessing.
      --  The list should be amended alongside our discoveries. It is
      --  currently: -std, -fno-exceptions, -fno-rtti, and -W* -include.

      Raw_Switches : String_Vectors.Vector;
      --  List of switches passed to the compiler driver without filtering

      Clang_Needs_M32 : Boolean := False;
      --  Wether we need to pass -m32 to libclang's parsing commands. This is
      --  only the case when the compiler driver is a native 32 bit compiler.

      Dep_File_Options : String_Vectors.Vector;
      --  Options to generate a dependency file, for incremental
      --  instrumentation.

   end record;
   --  Options to analyze (preprocess and/or parse) a compilation unit

   Macro_Cmdline_Regexp : constant Pattern_Matcher :=
     Compile
       ("([a-zA-Z_]\w*)"
        --  The name of the macro

        & "(\(.*\))?"
        --  The optional list of macro arguments

        & "([^ =]+)?"
        --  Then, there can be any character before the assignment: they will
        --  be part of the macro value (e.g. A(b)b will yield #define A b 1)

        & "(?:=(.*))?"
        --  The macro value itself
       );

   Macro_Def_Regexp : constant Pattern_Matcher :=
     Compile
       ("#define"
        & "(?: |\t)+"
        --  "#define", then a non-empty blank

        & "([a-zA-Z_]\w*)"
        --  The name of the macro

        & "(\(.*\))?"
        --  The optional list of macro arguments

        & "(.*)"
        --  The macro value itself
       );
   --  Regular expression to analyze definitions for builtin macros (see
   --  Builtin_Macros)

   procedure Parse_Macro_Definition
     (Str : String; Parsed_Def : out Macro_Definition; Success : out Boolean);
   --  Parse a macro definition. If the parsing failed, set Success to False.
   --  Otherwise, set Parsed_Def to the parsed definition and set Success to
   --  True.

   procedure Parse_Cmdline_Macro_Definition
     (Str : String; Parsed_Def : out Macro_Definition; Success : out Boolean);
   --  Same as above, but with a command-line macro definition

   procedure Add_Options
     (Args          : in out String_Vectors.Vector;
      Options       : Analysis_Options;
      Pass_Builtins : Boolean := True;
      Preprocessed  : Boolean := False);
   --  Append to Args the command line options corresponding to Options. If
   --  Pass_Builtins is True, pass builtin macros in Options to Args. If
   --  Preprocessed is True, consider that we will use these options on a
   --  file that was already preprocessed.

   procedure Import_From_Args
     (Self : in out Analysis_Options; Args : String_Vectors.Vector);
   --  Extract analysis options from the Args command line arguments and update
   --  Self accordingly.

   subtype Instr_Annotation_Kind is
     Any_Annotation_Kind range Dump_Buffers .. Any_Annotation_Kind'Last;
   --  Annotation kinds that are relevant for the instrumentation step

   type Instr_Annotation (Kind : Instr_Annotation_Kind := Dump_Buffers) is
   record

      Insert_After : Boolean := False;
      --  For instrumenters where this is applicable, consider that the
      --  annotation should apply after the designated entity rather than
      --  before.

      --  Explicitly list all annotation kinds to get a compilation warning
      --  when adding new annotation kind in ALI_Files but not here.

      case Kind is
         when Dump_Buffers =>
            Trace_Prefix : Unbounded_String;
            --  Optional trace prefix for the buffer dump, left empty if not
            --  specified.

         when Cov_Off =>
            Justification : Unbounded_String;
            --  Justification for why the region is disabled for coverage

         when Reset_Buffers | Cov_On =>
            null;
      end case;
   end record;
   --  Represents one annotation, with all the relevant information needed by
   --  the instrumenters.

   package Instr_Annotation_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Local_Source_Location,
        Element_Type => Instr_Annotation);
   subtype Instr_Annotation_Map is Instr_Annotation_Maps.Map;

   procedure Populate_Ext_Disabled_Cov
     (UIC    : in out Unit_Inst_Context;
      Annots : Instr_Annotation_Map;
      SFI    : Source_File_Index);
   --  Populate the Annotations and Disabled_Cov_Regions of UIC with the
   --  annotations in Annots. The resulting annotations are tied to SFI.

   function Dependency_File
     (Prj : Prj_Desc; Filename : GNATCOLL.VFS.Virtual_File)
      return GNATCOLL.VFS.Virtual_File;
   --  Return the dependency filename for the given file

   function Instrumented_Files_File
     (Prj : Prj_Desc; Filename : GNATCOLL.VFS.Virtual_File)
      return GNATCOLL.VFS.Virtual_File;
   --  Return the file containing the list of instrumented files when
   --  instrumenting the given file / main unit part.

   function Files_Instrumentation_Info_File
     (Prj : Prj_Desc; Unit_Name : String) return GNATCOLL.VFS.Virtual_File;
   --  Files containing instrumentation information for the given Unit_Name.
   --
   --  This is used in the context of manual dump trigger instrumentation, to
   --  indicate whether the source contains dump / reset annotations or not.

   use type GPR2.Path_Name.Object;

   function Less (L, R : GPR2.Build.Source.Object) return Boolean
   is (L.Path_Name < R.Path_Name);
   function Equal (L, R : GPR2.Build.Source.Object) return Boolean
   is (L.Path_Name = R.Path_Name);

   package Source_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets
       (Element_Type => GPR2.Build.Source.Object,
        "<"          => Less,
        "="          => Equal);

   type Project_Info is record
      Project : GPR2.Project.View.Object;
      --  Project that this record describes

      Externally_Built : Boolean;
      --  Whether this project is externally built. In that case, we assume its
      --  units of interest have already been instrumented.

      Desc : Prj_Desc;
      --  Description containing all the project information needed for
      --  instrumentation purposes.

   end record;

   type Project_Info_Access is access all Project_Info;

   package Project_Info_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Project_Info_Access,
        Equivalent_Keys => "=",
        Hash            => Strings.Hash);
   --  Mapping from project name (as returned by GNATCOLL.Projects.Name) to
   --  Project_Info records. Project_Info records are owned by this map, and
   --  thus must be deallocated when maps are deallocated.

   type Inst_Context is limited record
      Ada_Default_Charset : Unbounded_String;
      --  Default charset to analyze Ada source code

      Mapping_File : Unbounded_String;
      --  File that describes the mapping of units to source files for all Ada
      --  units.

      Config_Pragmas_Mapping : Unbounded_String;
      --  File that describes the mapping of Ada source files to configuration
      --  pragma files. See the Save_Config_Pragmas_Mapping and
      --  Load_Config_Pragmas_Mapping procedures in Instrument.Ada_Unit for
      --  more information.

      Sources_Of_Interest_Response_File : Unbounded_String;
      --  File containing the list of units of interest, identified by their
      --  fullname. This is passed on to gnatcov instrument-source invokations
      --  (for Ada), to know which part of a unit (spec / body / separate) must
      --  be instrumented.

      Ada_Preprocessor_Data_File : Unbounded_String;
      --  JSON file that contains the preprocessor data necessary to analyze
      --  Ada sources (see Instrument.Ada_Unit.Create_Preprocessor_Data_File).

      Excluded_Source_Files_Present : Boolean;
      Excluded_Source_Files         : GNAT.Regexp.Regexp;
      --  If present, instrumentation will ignore files whose names match the
      --  accessed pattern.

      Project_Info_Map : Project_Info_Maps.Map;
      --  For each project that contains units of interest, this tracks a
      --  Project_Info record.

      Files_Of_Interest : File_Sets.Set;
      --  List of files of interest.
      --
      --  This is passed on to instrument-source invocations when instrumenting
      --  an Ada file (to know which part of a compilation unit must be
      --  instrumented, i.e. spec / body / separates). It is also passed to
      --  instrument-main invocations to know the full list of instrumented
      --  sources.

      Tag : Unbounded_String;
      --  Tag relative to the current instrumentation run

   end record;

   type Inst_Context_Acc is access all Inst_Context;

   function Load_Naming_Scheme
     (Prj : GPR2.Project.View.Object) return Naming_Scheme_Desc;
   --  Retrieve the naming scheme from the given project Prj

   type Library_Unit_Info is record
      Main_Part_Src : GPR2.Build.Source.Object;
      --  Main part for this compilation unit

      Is_UOI : Boolean;
      --  Whether the unit is a unit of interest

      Is_Main : Boolean;
      --  Whether the unit is a main

      Instr_Project : GPR2.Project.View.Object;
      --  Project in which instrumentation artifacts for this unit are
      --  generated.

      Language_Kind : Supported_Language_Kind;
      --  Higher level representation of a language (unit-based or file-based)

      Language : Src_Supported_Language;
      --  Actual language representation

      Spec_Project, Body_Project : GPR2.Project.View.Object;
      --  Track the owning project of this unit's spec source file (if present)
      --  and body source file (likewise).

      Sources : Source_Sets.Set;
      --  Set of sources that belong to this unit

   end record;

   package Unit_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Library_Unit_Info);
   --  Map to unit names to unit info of files implementing this unit. For
   --  file-based languages, the unit name is the full name (to simplify
   --  dealing with homonym in different projects).

   function SID_Filename
     (Main_Part_Src : GPR2.Build.Source.Object; In_Library_Dir : Boolean)
      return String;
   --  Return the filename of the SID file to create for the given compilation
   --  unit. If In_Library_Dir is True and the the unit lives in a library
   --  project, then return a filename located in the project library
   --  directory. Otherwise, the filename is located in the object directory.

   function Compilation_Unit_Options
     (IC   : Inst_Context;
      Prj  : Prj_Desc;
      Lang : Src_Supported_Language;
      Src  : GPR2.Build.Source.Object) return Command_Line_Args;
   --  Return the list of options to pass to a gnatcov instrument-source /
   --  instrument-main for the given compilation unit Unit_Name, belonging to
   --  the project Prj.

   function Load_From_Project (Prj : GPR2.Project.View.Object) return Prj_Desc;
   --  Load the project description from the given project

   function Get_Or_Create_Project_Info
     (Context : in out Inst_Context; Project : GPR2.Project.View.Object)
      return Project_Info_Access;
   --  Return the Project_Info record corresponding to Project. Create it if it
   --  does not exist.

   function Instrumentation_Artifacts
     (Main_Part_Src : GPR2.Build.Source.Object; Prj : Prj_Desc)
      return File_Sets.Set;
   --  Return the list of instrumentation artifacts generated by the source
   --  instrumentation of Unit. This is used by incremental instrumentation,
   --  to clean up instrumentation artifacts not needed by the current
   --  instrumentation run.
end Instrument.Common;
