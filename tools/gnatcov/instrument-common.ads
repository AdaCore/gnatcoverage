------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with GNATCOLL.Projects; use GNATCOLL.Projects;

with Libadalang.Analysis;
private with Libadalang.Rewriting;

with Checkpoints;
with SC_Obligations; use SC_Obligations;

package Instrument.Common is

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

   Stmt_Buffer_Name : Ada_Qualified_Name;
   --  Qualified name (relative to the unit buffer package) of the buffer to
   --  contain coverage data corresponding to statement obligations.

   Decision_Buffer_Name : Ada_Qualified_Name;
   --  Qualified name (relative to the unit buffer package) of the buffer to
   --  contain coverage data corresponding to decision obligations.

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

   function "<" (Left, Right : Compilation_Unit_Name) return Boolean;
   --  Compare qualified name, identifier by identifier. If one is the
   --  prefix of the other, the shorter is considered to come first. If
   --  qualified name is the same, compare the kind.

   function Buffer_Unit
     (Instrumented_Unit : Compilation_Unit_Name) return Ada_Qualified_Name;
   --  Given a unit to instrument, return the name of the unit that holds
   --  its coverage buffers.

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

   type Instrumented_Unit_Info is record
      Filename : Ada.Strings.Unbounded.Unbounded_String;
      --  Name of the source file for this unit

      Is_Main : Boolean;
      --  Whether this unit is a main
   end record;

   package Instrumented_Unit_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Compilation_Unit_Name,
      Element_Type => Instrumented_Unit_Info);

   package File_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Ada.Strings.Unbounded.Unbounded_String,
      "="                 => Ada.Strings.Unbounded."=",
      Equivalent_Elements => Ada.Strings.Unbounded."=",
      Hash                => Ada.Strings.Unbounded.Hash);

   type Inst_Context is limited record
      Project_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  Name of the root project. It is also used to name the list of buffers

      Output_Dir : Ada.Strings.Unbounded.Unbounded_String;
      --  Subdirectory in the root project file's object directory. All we
      --  generate here must land in it.

      Instr_Dir : Ada.Strings.Unbounded.Unbounded_String;
      --  Directory to contain all instrumented sources

      Buffers_Dir : Ada.Strings.Unbounded.Unbounded_String;
      --  Directory to contain all sources that create coverage buffers

      Auto_Dump_Buffers : Boolean;
      --  See the eponym argument in Instrument.Intrument_Units_Of_Interest

      Instrumented_Units : Instrumented_Unit_Maps.Map;
      --  Mapping from instrumented unit names to information used during
      --  instrumentation.

      Instr_Files : File_Sets.Set;
      --  Set of files that were written to Instr_Dir
   end record;

   function Create_Context (Auto_Dump_Buffers : Boolean) return Inst_Context;
   --  Create an instrumentation context for the currently loaded project

   -------------------------
   -- Source instrumenter --
   -------------------------

   type Source_Rewriter is tagged limited private;
   --  Helper object to instrument a source file

   procedure Start_Rewriting
     (Self            : out Source_Rewriter;
      Input_Filename  : String;
      Output_Filename : String);
   --  Start a rewriting session for the given Input_Filename. If the rewriting
   --  process is successful, the result will be written to Output_Filename.
   --
   --  If there are parsing errors while reading Input_Filename, this raises a
   --  fatal error and prints the corresponding error messages.

   procedure Start_Instr_Rewriting
     (Self            : out Source_Rewriter;
      IC              : in out Inst_Context;
      Input_Filename  : String);
   --  Like Start_Rewriting but automatically compute the output filename to be
   --  the same as Input_Filename's basename relocated in IC.Instr_Dir. Also,
   --  register this basename in IC.Instr_Files.

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

   procedure Checkpoint_Save (CSS : in out Checkpoints.Checkpoint_Save_State);
   --  Save the current instrumented units map to stream

   procedure Checkpoint_Load (CLS : in out Checkpoints.Checkpoint_Load_State);
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
