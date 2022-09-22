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

--  Instrumentation of a C source file

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Projects;

with Namet; use Namet;
with Types; use Types;

with Clang.Index;   use Clang.Index;
with Clang.Rewrite; use Clang.Rewrite;

with Instrument.Base_Types; use Instrument.Base_Types;
with Instrument.Common;     use Instrument.Common;
with SC_Obligations;        use SC_Obligations;
with Slocs;                 use Slocs;
with Switches;              use Switches;

package Instrument.C is

   type C_Family_Instrumenter_Type is
     abstract new Language_Instrumenter with null record;
   --  Common instrumentation primitives for C/C++

   overriding function Skip_Source_File
     (Self        : C_Family_Instrumenter_Type;
      Source_File : GNATCOLL.Projects.File_Info) return Boolean;

   overriding procedure Instrument_Unit
     (Self      : C_Family_Instrumenter_Type;
      CU_Name   : Compilation_Unit_Name;
      IC        : in out Inst_Context;
      Unit_Info : in out Instrumented_Unit_Info);

   overriding procedure Auto_Dump_Buffers_In_Main
     (Self     : C_Family_Instrumenter_Type;
      IC       : in out Inst_Context;
      Main     : Compilation_Unit_Name;
      Filename : String;
      Info     : in out Project_Info);

   overriding procedure Emit_Buffers_List_Unit
     (Self              : C_Family_Instrumenter_Type;
      IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info);

   function Extern_Prefix
     (Self : C_Family_Instrumenter_Type) return String
   is ("extern ");
   --  Return the prefix for declarations and definitions so have C linkage

   type C_Instrumenter_Type is
     new C_Family_Instrumenter_Type with null record;
   --  Instrumentation primitives for C

   overriding function Language
     (Self : C_Instrumenter_Type) return Src_Supported_Language
   is (C_Language);

   type CPP_Instrumenter_Type is
     new C_Family_Instrumenter_Type with null record;
   --  Instrumentation primitives for C++

   overriding function Language
     (Self : CPP_Instrumenter_Type) return Src_Supported_Language
   is (CPP_Language);

   overriding function Extern_Prefix
     (Self : CPP_Instrumenter_Type) return String
   is ("extern ""C"" ");

   C_Instrumenter   : aliased constant C_Instrumenter_Type := (null record);
   CPP_Instrumenter : aliased constant CPP_Instrumenter_Type := (null record);

   type Instr_Scheme_Type is (Instr_Stmt, Instr_Expr);
   --  Depending on the statement construct, we can instrument it either with
   --  another statement right before (Instr_Stmt), which is the case for most
   --  statements:
   --
   --    int a = 1;
   --
   --  will become:
   --
   --    witness;
   --    int a = 1;
   --
   --  Sometimes we have to augment instead the underlying expression
   --  (Instr_Expr, when it is a statement expression):
   --
   --    while (a = 2) {}
   --
   --  will become:
   --
   --    while (witness, a = 2) {}
   --
   --  Here, we can't have the witness call go before the while, as there could
   --  very well be a goto pointing inside the loop, making it skip the
   --  execution of the witness statement, but we would still be executing the
   --  condition of the loop on the second iteration.

   type C_Source_Statement is record
      LL_SCO : Nat;
      --  Low-level SCO id of statement

      Instr_Scheme : Instr_Scheme_Type;
      --  How should the statement be instrumented. See documentation of
      --  Instr_Scheme_Type.

      Statement : Cursor_T;
      --  Statement node
   end record;

   type C_Source_Decision is record
      LL_SCO : Nat;
      --  Low-level SCO id of decision

      Decision : Cursor_T;
      --  Decision expression

      State : Ada.Strings.Unbounded.Unbounded_String;
      --  Name of MC/DC state local variable
   end record;

   type C_Source_Condition is record
      LL_SCO : Nat;
      --  Low-level SCO id of condition

      Condition : Cursor_T;
      --  Condition expression

      State : Ada.Strings.Unbounded.Unbounded_String;
      --  Name of MC/DC state local variable

      First : Boolean;
      --  True if this condition is the first one in its decision
   end record;

   package Source_Statement_Vectors is
      new Ada.Containers.Vectors (Natural, C_Source_Statement);
   package Source_Decision_Vectors is
     new Ada.Containers.Vectors (Natural, C_Source_Decision);
   package Source_Condition_Vectors is
     new Ada.Containers.Vectors (Natural, C_Source_Condition);

   type Pass_Kind is abstract tagged private;
   type Pass_Kind_Acc is access all Pass_Kind'Class;
   --  As we want to keep some information about coverage obligations inside
   --  macro expansions (mainly to make the reporting of such coverage
   --  obligations clearer), the instrumentation of a C source file is done in
   --  2 passes:
   --
   --    * The first pass consists in recording preprocessing information. We
   --      will be able to record when a macro expansion occurs (and save the
   --      adequate information, e.g. the name of the expanded macro) and save
   --      actual source locations (we lose the column numbering when working
   --      with unpreprocessed code). Still, we are unable to instrument code
   --      in this pass (we would not be able to instrument expanded code).
   --      This leads to the second pass...
   --
   --    * The second pass consists in emitting SCOs and instrumenting the
   --      source (and works on the preprocessed source code). The SCOs
   --      designates presumed preprocessed source locations (i.e. preprocessed
   --      slocs that account for line directives). This pass also complete
   --      the preprocessing information with actual preprocessed source
   --      locations (to get an actual text of the obligation when producing a
   --      coverage report).
   --
   --  The two "passes" are intrisincally related: as we map preprocessing
   --  information over SCO ids, we need them to be consistent and identical in
   --  both passes, and thus to traverse the AST in the exact same way.
   --
   --  They thus run the same code with a few tweaks:
   --
   --     * The first pass only records preprocessing information and does not
   --       implement any of the instrumentation code.
   --
   --     * The second pass instruments the code.
   --
   --  Note that both passes fill the SCO_Table, but in the first pass, it is
   --  just done to track SCO ids. All of its content is discarded at the end
   --  of the first pass.
   --
   --  Also note that the first pass preprocesses the code internally with
   --  clang, but the second pass uses a preprocessed version of the code
   --  produced by the user's preprocessor. To have the same AST in the end, we
   --  make the assumption that we can emulate the user's preprocessor with
   --  clang (with the right set of flags, to override clang's preprocessor
   --  defaults). If this assumption does not hold, then we will probably get a
   --  different AST for both passes. For that reason, we make a consistency
   --  check after having ran both passes: if it does not hold, we will simply
   --  discard the preprocessed information recorded by the first pass, and
   --  produce a degraded report.

   package LL_SCO_PP_Info_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Nat,
      Element_Type => PP_Info);

   type Macro_Definition is record
      Define : Boolean;
      --  Whether this defines a macro or undefines it

      Value : Unbounded_String;
      --  If this undefines a macro, this just contains the name of that macro.
      --
      --  If this defines a macro, "-D" option value for the definition of this
      --  macro. For instance "A(x, y)=B" for "#define A(x, y) B".
   end record;
   --  Whether a macro should be defined, its name, and when it must be
   --  defined, its value.

   package Macro_Vectors is
     new Ada.Containers.Vectors (Positive, Macro_Definition);

   type Analysis_Options is record
      PP_Search_Path : String_Vectors.Vector;
      --  List of directories to search when looking for an included file

      Builtin_Macros : Macro_Vectors.Vector;
      --  List of predefined macros for the project compiler driver

      PP_Macros : Macro_Vectors.Vector;
      --  List of macros for the preprocessor

      Std : Unbounded_String;
      --  -std=X argument to pass to the preprocessor and the parser, or the
      --  empty string.
   end record;
   --  Options to analyze (preprocess and/or parse) a compilation unit

   procedure Add_Options
     (Args          : in out String_Vectors.Vector;
      Options       : Analysis_Options;
      Pass_Builtins : Boolean := True);
   --  Append to Args the command line options corresponding to Options. If
   --  Pass_Builtins is True, pass builtin macros in Options to Args.

   procedure Import_From_Project
     (Self     : out Analysis_Options;
      Language : C_Family_Language;
      Info     : Project_Info;
      Filename : String);
   --  Initialize Self from compiler switches corresponding to the Filename
   --  source file in the Info project.

   procedure Import_From_Args
     (Self : in out Analysis_Options; Args : String_Vectors.Vector);
   --  Extract analysis options from the Args command line arguments and update
   --  Self accordingly.

   function Split_Args (Args : Unbounded_String) return String_Vectors.Vector;
   --  Split a comma-separated list of arguments

   procedure Import_Options
     (Self     : out Analysis_Options;
      Language : C_Family_Language;
      Info     : Project_Info;
      Filename : String);
   --  Shortcut to call Import_From_Project, and Import_From_Agrs on the
   --  --c-opts/--c++-opts option.

   type C_Unit_Inst_Context is new Instrument.Common.Unit_Inst_Context with
      record
         TU       : Translation_Unit_T;
         CIdx     : Index_T;
         Rewriter : Rewriter_T;

         File : Ada.Strings.Unbounded.Unbounded_String;
         --  Original filename

         Source_Statements : Source_Statement_Vectors.Vector;
         Source_Decisions  : Source_Decision_Vectors.Vector;
         Source_Conditions : Source_Condition_Vectors.Vector;
         --  Statements, decisions and (for MC/DC) conditions to be
         --  instrumented.

         MCDC_State_Declaration_Node : Cursor_T;
         --  Where should MCDC state declaration be inserted (the beginning of
         --  the procedure).

         Options : Analysis_Options;
         --  Configuration for the preprocessor/parser when working on this
         --  source file.

         Pass : Pass_Kind_Acc;
         --  Current pass. See the Pass_Kind documentation for more details.

         LL_PP_Info_Map : LL_SCO_PP_Info_Maps.Map;
         --  Preprocessing information for low level SCOs

      end record;

   type C_Source_Rewriter is tagged limited private;
   --  Helper object to instrument a source file

private

   type Pass_Kind is abstract tagged null record;

   procedure Append_SCO
     (Pass               : Pass_Kind;
      UIC                : in out C_Unit_Inst_Context'Class;
      N                  : Cursor_T;
      C1, C2             : Character;
      From, To           : Local_Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name) is null;

   procedure Instrument_Statement
     (Pass         : Pass_Kind;
      UIC          : in out C_Unit_Inst_Context'Class;
      LL_SCO       : Nat;
      Insertion_N  : Cursor_T;
      Instr_Scheme : Instr_Scheme_Type) is null;

   procedure Instrument_Decision
     (Pass     : Pass_Kind;
      UIC      : in out C_Unit_Inst_Context'Class;
      LL_SCO   : Nat;
      Decision : Cursor_T;
      State    : US.Unbounded_String) is null;

   procedure Instrument_Condition
     (Pass      : Pass_Kind;
      UIC       : in out C_Unit_Inst_Context'Class;
      LL_SCO    : Nat;
      Condition : Cursor_T;
      State     : US.Unbounded_String;
      First     : Boolean) is null;

   procedure Insert_MCDC_State
     (Pass       : Pass_Kind;
      UIC        : in out C_Unit_Inst_Context'Class;
      Name       : String;
      MCDC_State : out US.Unbounded_String) is null;

   procedure Curlify
     (Pass : Pass_Kind; N : Cursor_T; Rew : Rewriter_T) is null;

   type C_Source_Rewriter is limited new Ada.Finalization.Limited_Controlled
   with record
      CIdx     : Index_T;
      TU       : Translation_Unit_T;
      Rewriter : Rewriter_T;
      --  Structures that should be freed after rewriting

      Extern_Insertion_Location : Source_Location_T;
      --  Where we can insert extern declarations in the file being rewritten.
      --  Such declarations must go before being used, so this should
      --  correspond to the first rewritable location.

      Output_Filename : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding procedure Initialize (Self : in out C_Source_Rewriter);
   overriding procedure Finalize (Self : in out C_Source_Rewriter);

end Instrument.C;
