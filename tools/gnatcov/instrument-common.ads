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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

with GNAT.Regpat; use GNAT.Regpat;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.VFS;  use GNATCOLL.VFS;

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
     (Main     : Compilation_Unit_Part;
      Manual   : Boolean := False;
      Prj_Name : Ada_Qualified_Name := Ada_Identifier_Vectors.Empty_Vector)
      return String
   is
     ("gnatcov_rts_"
      & (if Manual
        then "manual"
        else Instrumented_Unit_Slug (Main))
      & "_"
      & To_Lower (To_String (Dump_Procedure_Name))
      & (if Manual
        then "_" & To_Symbol_Name (Prj_Name)
        else ""));
   --  Return the name of the exported symbol for the Dump_Buffers function

   function Reset_Procedure_Symbol
     (Prj_Name : Ada_Qualified_Name) return String is
     ("gnatcov_rts_" & To_Lower (To_String (Reset_Procedure_Name))
      & "_" & To_Symbol_Name (Prj_Name));
   --  Return the name of the exported symbol for the Reset_Buffers procedure

   function Is_Manual_Indication_Procedure_Symbol
     (Symbol : String) return Boolean;
   --  For C, manual dump/reset procedures are suffixed by the project's name.
   --  Check that Symbol corresponds to the name of one such procedure.

   function Statement_Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Part) return String;
   --  Given a unit to instrument, return the name of the symbol to use for the
   --  entity that contains address of the statement coverage buffer.

   function Decision_Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Part) return String;
   --  Given a unit to instrument, return the name of the symbol to use for the
   --  entity that contains address of the decision coverage buffer.

   function MCDC_Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Part) return String;
   --  Given a unit to instrument, return the name of the symbol to use for the
   --  entity that contains address of the decision coverage buffer.

   function Unit_Buffers_Name (Unit : Compilation_Unit) return String;
   --  Name of the symbol that references the
   --  gnatcov_rts_coverage_buffers_group struct for this file.

   function Unit_Buffers_Array_Name
     (Prj_Name : Ada_Qualified_Name) return String is
     ("gnatcov_rts_buffers_array_" & To_Symbol_Name (Prj_Name));
   --  Name of the symbol that designates the
   --  gnatcov_rts_coverage_buffers_array struct, which contains an array of
   --  coverage buffers for all instrumented units in this project.
   --
   --  We need this to be unique per root project instrumented, as gnatcov
   --  gives the possibility to link two separately-instrumented libraries in
   --  the same executable.

   function Project_Output_Dir
     (Project : GPR2.Project.View.Object) return String;
   --  Return the directory in which we must create instrumented sources for
   --  Project. This returns an empty string for projects that do not have an
   --  object directory.

   function Format_Fingerprint
     (Fingerprint      : SC_Obligations.Fingerprint_Type;
      Opening, Closing : String) return String;
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

   package Instrumented_Unit_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Compilation_Unit_Part,
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

   package Annotation_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Annotation_Couple);

   package Sloc_Range_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Source_Location_Range);

   package Nat_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Nat);

   type Unit_Inst_Context is tagged record
      Instrumented_Unit : Compilation_Unit_Part;
      --  Name of the compilation unit being instrumented

      SFI : Source_File_Index := No_Source_File;
      --  Source file index of the compilation unit being instrumented

      Fullname : Unbounded_String;
      --  Fullname of the compilation unit being instrumented

      Buffer_Unit : Compilation_Unit_Part;
      --  Name of the compilation unit that holds coverage buffers for the
      --  unit currently being instrumented (see Common.Buffer_Unit).

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

   function Img (Bit : Any_Bit_Id) return String is
     (Strings.Img (Integer (Bit)));

   Runtime_Version : constant Natural := 9;
   Runtime_Error   : constant String :=
     "Incompatible GNATcov_RTS version, please use"
     & " the GNATcov_RTS project provided with your"
     & " GNATcoverage distribution.";
   --  Must be up-to-date with GNATcov_RTS.Version

   Ada_Runtime_Version_Check : constant String :=
     "pragma Compile_Time_Error (GNATcov_RTS.Version /= "
     & Img (Runtime_Version) & " ,""" & Runtime_Error & """);";
   C_Runtime_Version_Check : constant String :=
     "#if GNATCOV_RTS_VERSION != "
     & Img (Runtime_Version)
     & ASCII.LF
     & "#error """ & Runtime_Error & """"
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
     (Blocks  : in out SCO_Id_Vector_Vector;
      SCO_Map : LL_HL_SCO_Map);
   --  Convert low level SCOs in Blocks to high-level SCOs using the
   --  mapping in SCO_Map.

   package CU_Name_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Compilation_Unit_Part);

   package Ada_Qualified_Name_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Ada_Qualified_Name,
      "="          => Ada_Identifier_Vectors."=");

   type Language_Instrumenter is abstract tagged record
      Tag : Unbounded_String;
      --  Tag specific to an instrumentation run
   end record;
   --  Set of operations to allow the instrumentation of sources files for a
   --  given language.

   function Language
     (Self : Language_Instrumenter) return Src_Supported_Language is abstract;
   --  Return the language that this instrumenter is designed to process

   function Language_Name (Self : Language_Instrumenter'Class) return String
   is (Image (Self.Language));

   procedure Instrument_Unit
     (Self              : in out Language_Instrumenter;
      Unit_Name         : String;
      Prj               : Prj_Desc;
      Files_Of_Interest : File_Sets.Set) is null;
   --  Instrument a single source file for the language that Self supports.
   --
   --  Unit_Name identifies this compilation unit (either through a unit name,
   --  for Unit_Based_Language, or through a full name, for file-based). Prj
   --  describes information coming from the project, and relevant to the
   --  instrumentation of the unit. Files_Of_Interest provides the list of
   --  files of interest, to ignore e.g. part of the unit (e.g. a separate)
   --  when instrumenting it.

   procedure Auto_Dump_Buffers_In_Main
     (Self          : in out Language_Instrumenter;
      Filename      : String;
      Dump_Config   : Any_Dump_Config;
      Prj           : Prj_Desc) is null;
   --  Try to instrument the Filename source file (whose language is assumed
   --  to be Self's) to insert a call to dump the list of coverage buffers,
   --  assumed to be named after Prj.Prj_Name. Do nothing if not successful.

   procedure Emit_Buffers_List_Unit
     (Self        : Language_Instrumenter;
      Instr_Units : Unit_Sets.Set;
      Prj         : Prj_Desc) is null;
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
      Prj            : Prj_Desc) return Compilation_Unit
   is (No_Compilation_Unit);
   --  Same as above except Buffer_Symbols contains the list of C symbols
   --  holding coverage buffers for units of interest. Return the buffers list
   --  compilation unit.

   function Buffer_Unit
     (Self : Language_Instrumenter;
      CU   : Compilation_Unit;
      Prj  : Prj_Desc) return Compilation_Unit
   is (No_Compilation_Unit);
   --  Return the compilation unit holding coverage buffers

   function Dump_Manual_Helper_Unit
     (Self : Language_Instrumenter;
      Prj  : Prj_Desc) return Compilation_Unit
   is (No_Compilation_Unit);

   function Dump_Helper_Unit
     (Self : Language_Instrumenter;
      CU   : Compilation_Unit;
      Prj  : Prj_Desc) return Compilation_Unit
   is (No_Compilation_Unit);
   --  Return the compilation unit holding the dump helper subprogram

   function Has_Main
     (Self     : in out Language_Instrumenter;
      Filename : String;
      Prj      : Prj_Desc) return Boolean is (False);
   --  Return whether the given file is a main or not

   procedure Emit_Dump_Helper_Unit_Manual
     (Self          : in out Language_Instrumenter;
      Dump_Config   : Any_Dump_Config;
      Prj           : Prj_Desc) is null;
   --  Emit the dump helper unit with the appropriate content to allow for a
   --  simple call to a procedure dumping the coverage buffers to be made in
   --  the instrumented source files.

   procedure Replace_Manual_Indications
     (Self                  : in out Language_Instrumenter;
      Prj                   : in out Prj_Desc;
      Source                : Virtual_File;
      Has_Dump_Indication   : out Boolean;
      Has_Reset_Indication  : out Boolean);
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
     (Self : in out Language_Instrumenter;
      Prj  : in out Prj_Desc) is null;
   --  Emit in the root project a file that exposes live coverage observability
   --  features, such as the number of bits set in the buffers.

   function New_File
     (Prj : Prj_Desc; Name : String) return String;
   --  Compute the path to the file to create in Self.Output_Dir

   procedure Create_File
     (Prj  : Prj_Desc;
      File : in out Text_Files.File_Type;
      Name : String);
   --  Shortcut to Text_Files.Create: create a text file with the given name in
   --  Prj.Output_Dir.
   --
   --  Name can be a basename, a relative name or an absolute one: in all
   --  cases, the basename is taken and the file is created in Prj.Output_Dir.

   function To_Filename
     (Prj : Prj_Desc; CU_Name : Compilation_Unit_Part) return String;
   --  Convert a Compilation_Unit_Name to a file basename, using the body /
   --  spec suffix and dot replacement (for unit based languages) defined in
   --  Prj.

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

   function "<" (L, R : Macro_Definition) return Boolean is (L.Name < R.Name);
   function "=" (L, R : Macro_Definition) return Boolean is (L.Name = R.Name);
   --  As we store Macro_Definition in sets, we do not want two conflicting
   --  definitions of the same macro to coexist. Thus, we compare only the
   --  name, meaning that when we insert a new definition, it will replace
   --  the previous one.

   package Macro_Sets is new Ada.Containers.Ordered_Sets (Macro_Definition);
   subtype Macro_Set is Macro_Sets.Set;

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

   end record;
   --  Options to analyze (preprocess and/or parse) a compilation unit

   Macro_Cmdline_Regexp : constant Pattern_Matcher := Compile (
     "([a-zA-Z_]\w*)"
     --  The name of the macro

     & "(\(.*\))?"
     --  The optional list of macro arguments

     & "([^ =]+)?"
     --  Then, there can be any character before the assignment: they will be
     --  part of the macro value (e.g. A(b)b will yield #define A b 1)

     & "(?:=(.*))?"
     --  The macro value itself
   );

   Macro_Def_Regexp : constant Pattern_Matcher := Compile (
     "#define"
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
     (Str        : String;
      Parsed_Def : out Macro_Definition;
      Success    : out Boolean);
     --  Parse a macro definition. If the parsing failed, set Success to False.
     --  Otherwise, set Parsed_Def to the parsed definition and set Success to
     --  True.

   procedure Parse_Cmdline_Macro_Definition
     (Str        : String;
      Parsed_Def : out Macro_Definition;
      Success    : out Boolean);
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

   subtype Instr_Annotation_Kind is Any_Annotation_Kind range
     Dump_Buffers .. Any_Annotation_Kind'Last;
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

   package Instr_Annotation_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Local_Source_Location,
      Element_Type => Instr_Annotation);
   subtype Instr_Annotation_Map is Instr_Annotation_Maps.Map;

   procedure Populate_Ext_Disabled_Cov
     (UIC    : in out Unit_Inst_Context;
      Annots : Instr_Annotation_Map;
      SFI    : Source_File_Index);
   --  Populate the Annotations and Disabled_Cov_Regions of UIC with the
   --  annotations in Annots. The resulting annotations are tied to SFI.

end Instrument.Common;
