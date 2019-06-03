------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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
--  "gnatcov instrument" generates instrumented sources in a subdirectory of
--  the root project's object directory. The following compilation units (and
--  source files) are generated:
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
--  * An instrumented version of all compilation units of interest. These new
--    units are replacements for the original units. They fill the coverage
--    buffers for the unit.

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;

with Libadalang.Analysis;
private with Libadalang.Rewriting;

with Checkpoints;
with SC_Obligations; use SC_Obligations;
with Text_Files;

package Instrument.Common is

   pragma Elaborate_Body;

   function "/" (Dir, Name : String) return String is
     (Ada.Directories.Compose (Dir, Name));

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+" (US : Ada.Strings.Unbounded.Unbounded_String) return String
      renames Ada.Strings.Unbounded.To_String;

   --  TODO??? Handle Unicode file names and source text

   type Ada_Identifier is new Ada.Strings.Unbounded.Unbounded_String;
   --  Simple Ada identifier

   package Ada_Identifier_Vectors is new Ada.Containers.Vectors
     (Positive, Ada_Identifier);

   subtype Ada_Qualified_Name is Ada_Identifier_Vectors.Vector;
   --  Sequence of ada identifiers, representing a qualified name. For
   --  instance: Scope_A.Scope_B.Scope_C

   function "&" (Left, Right : Ada_Qualified_Name) return Ada_Qualified_Name
      renames Ada_Identifier_Vectors."&";

   function To_Ada (Name : Ada_Qualified_Name) return String
      with Pre => not Name.Is_Empty;
   --  Turn the given qualified name into Ada syntax

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

   type Compilation_Unit_Name is record
      Unit : Ada_Qualified_Name;
      Part : Unit_Parts;
   end record;
   --  Unique identifier for an instrumented unit

   function To_Compilation_Unit_Name
     (Source_File : GNATCOLL.Projects.File_Info) return Compilation_Unit_Name;
   --  Return the compilation unit name corresponding to the unit in
   --  Source_File.

   function To_Filename (CU_Name : Compilation_Unit_Name) return String;
   --  Return the name of the file to contain the given compilation unit. This
   --  assumes standard GNAT naming scheme.

   function Image (CU_Name : Compilation_Unit_Name) return String;
   --  Return a string representation of CU_Name for use in diagnostics

   function "<" (Left, Right : Compilation_Unit_Name) return Boolean;
   --  Compare qualified name, identifier by identifier. If one is the
   --  prefix of the other, the shorter is considered to come first. If
   --  qualified name is the same, compare the kind.

   function Instrumented_Unit_Slug
     (Instrumented_Unit : Compilation_Unit_Name) return Ada_Identifier;
   --  Given a unit to instrument, return a unique identifier to describe it
   --  (the so called slug).
   --
   --  One can use this slug to generate unique names for this unit.

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

   package Instrumented_Unit_To_CU_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Compilation_Unit_Name,
      Element_Type => CU_Id);

   Instrumented_Unit_CUs : Instrumented_Unit_To_CU_Maps.Map;
   --  Associate a CU id for all instrumented units. Updated each time we
   --  instrument a unit (or load a checkpoint) and used each time we read a
   --  coverage buffer (or save to a checkpoint).

   function Find_Instrumented_Unit
     (Unit_Name : String;
      Unit_Part : Unit_Parts) return CU_Id;
   --  Return the CU_Id corresponding to the given instrumented unit, or
   --  No_CU_Id if not found.

   package File_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Ada.Strings.Unbounded.Unbounded_String,
      "="                 => Ada.Strings.Unbounded."=",
      Equivalent_Elements => Ada.Strings.Unbounded."=",
      Hash                => Ada.Strings.Unbounded.Hash);

   type Project_Info is record
      Output_Dir : Ada.Strings.Unbounded.Unbounded_String;
      --  Subdirectory in the project file's object directory. All we generate
      --  for this project must land in it.

      Instr_Files : File_Sets.Set;
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
      Unit : Ada_Qualified_Name;
      --  Name of the main to instrument

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
   end record;

   package Instrumented_Unit_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Compilation_Unit_Name,
      Element_Type => Instrumented_Unit_Info);

   type Inst_Context is limited record
      Project_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  Name of the root project. It is also used to name the list of buffers

      Auto_Dump_Buffers : Boolean;
      --  See the eponym argument in Instrument.Intrument_Units_Of_Interest

      Main_To_Instrument_Vector : Main_To_Instrument_Vectors.Vector;
      --  List of mains to instrument *which are not units of interest*. Always
      --  empty when Auto_Dump_Buffers is false.
      --
      --  We need a separate list for these as mains which are units of
      --  interest are instrumented to dump coverage buffers at the same time
      --  they are instrumented to fill coverage buffers.

      Instrumented_Units : Instrumented_Unit_Maps.Map;
      --  Mapping from instrumented unit names to information used during
      --  instrumentation.

      Project_Info_Map : Project_Info_Maps.Map;
      --  For each project that contains units of interest, this tracks a
      --  Project_Info record.
   end record;

   function Create_Context (Auto_Dump_Buffers : Boolean) return Inst_Context;
   --  Create an instrumentation context for the currently loaded project

   procedure Destroy_Context (Context : in out Inst_Context);
   --  Free dynamically allocated resources in Context

   function Get_Or_Create_Project_Info
     (Context : in out Inst_Context;
      Project : Project_Type) return Project_Info_Access;
   --  Return the Project_Info record corresponding to Project. Create it if it
   --  does not exist.

   procedure Register_Main_To_Instrument
     (Context : in out Inst_Context;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type)
      with Pre => Context.Auto_Dump_Buffers;
   --  Register a main to be instrumented so that it dumps coverage buffers.
   --  File is the source file for this main, and Project is the project that
   --  owns this main.

   procedure Create_File
     (Info : in out Project_Info;
      File : in out Text_Files.File_Type;
      Name : String);
   --  Shortcut to Text_Files.Create: create a text file with the given name in
   --  IC.Output_Dir and register it in IC.Instr_Files.
   --
   --  Name can be a basename, a relative name or an absolute one: in all
   --  cases, the basename is taken and the file is created in IC.Output_Dir.

   -------------------------
   -- Source instrumenter --
   -------------------------

   type Source_Rewriter is tagged limited private;
   --  Helper object to instrument a source file

   procedure Start_Rewriting
     (Self           : out Source_Rewriter;
      Info           : in out Project_Info;
      Input_Filename : String);
   --  Start a rewriting session for the given Input_Filename. If the rewriting
   --  process is successful, the result will be written to a file in
   --  IC.Output_Dir with the basename of Output_Filename.
   --
   --  This registers the output file in IC.Instr_Files.
   --
   --  If there are parsing errors while reading Input_Filename, this raises a
   --  fatal error and prints the corresponding error messages.

   function Rewritten_Context
     (Self : Source_Rewriter) return Libadalang.Analysis.Analysis_Context;
   --  Return the analysis context that Self uses to instrument

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

   procedure Checkpoint_Load (CLS : access Checkpoints.Checkpoint_Load_State);
   --  Load checkpointed instrumented unit map from stream and merge them in
   --  current state.

private

   type Source_Rewriter is limited new Ada.Finalization.Limited_Controlled with
   record
      Input_Filename  : Ada.Strings.Unbounded.Unbounded_String;
      Output_Filename : Ada.Strings.Unbounded.Unbounded_String;

      Context : Libadalang.Analysis.Analysis_Context;
      Unit    : Libadalang.Analysis.Analysis_Unit;
      Handle  : Libadalang.Rewriting.Rewriting_Handle;
   end record;

   overriding procedure Finalize (Self : in out Source_Rewriter);

end Instrument.Common;
