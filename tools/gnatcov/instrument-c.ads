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

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Namet; use Namet;
with Types; use Types;

with Clang.Index;   use Clang.Index;
with Clang.Rewrite; use Clang.Rewrite;

with Instrument.Base_Types; use Instrument.Base_Types;
with Instrument.Common;     use Instrument.Common;
with SC_Obligations;        use SC_Obligations;
with Strings;               use Strings;

private package Instrument.C is

   type Instr_Scheme_Type is (Instr_Stmt, Instr_Expr);
   --  Depending on the statement construct, we can instrument it either with
   --  another statement right before, which is the case for most expressions,
   --
   --  int a = 1;
   --
   --  will become
   --
   --  witness;
   --  int a = 1;
   --
   --  or we have to instrument it by augmenting the underlying expression
   --  (when it is a statement expression), like:
   --
   --  while (a = 2) {}
   --
   --  will become
   --
   --  while (witness && a = 2) {}
   --
   --  Here, we can't have the witness call go before the while, as there could
   --  very well be a goto pointing inside the loop, making it skip the
   --  execution of the witness statement, but we would still be executing the
   --  condition of the loop on the second iteration.
   --
   --  TODO: we still have to figure out what should be done with such valid
   --  C++ constructs:
   --
   --  while (int i = calc()){}

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
   --     * The first pass only records preprocessing information and do not
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
   --  defaults). If this assumption does not hold, then we will probably get
   --  a different AST for both passes. For that reason, we make a
   --  consistency check after having ran both passes: if it does not hold, we
   --  will simply discard the preprocessed information recorded by the first
   --  pass, and produce a degraded report.

   package LL_SCO_PP_Info_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Nat,
      Element_Type => PP_Info);

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

         PP_Search_Paths, PP_Predefined_Macros : String_Vectors.Vector;
         --  Configuration of the user's preprocessor. We need to pass it to
         --  clang when parsing the unpreprocessed view of the sources.

         Pass : Pass_Kind_Acc;
         --  Current pass. See the Pass_Kind documentation for more details.

         LL_PP_Info_Map : LL_SCO_PP_Info_Maps.Map;
         --  Preprocessing information for low level SCOs

      end record;

   type C_Source_Rewriter is tagged limited private;
   --  Helper object to instrument a source file

   procedure Add_Auto_Dump_Buffers
     (IC   : Inst_Context;
      Info : in out Project_Info;
      Main : Compilation_Unit_Name;
      Rew  : C_Source_Rewriter)
     with Pre => IC.Dump_Config.Trigger /= Manual;
   --  Try to insert in the sources of Main (a main subprogram) a call to dump
   --  the list of coverage buffers for all units of interest in Main's
   --  closure. Return without doing anything if unsuccessful.
   --
   --  Info must be the project that owns the Main unit, and Rew is a rewriter
   --  for the Main compilation unit.

   procedure Extend_Statement_Sequence
     (N           : Cursor_T;
      Typ         : Character;
      UIC         : C_Unit_Inst_Context;
      Insertion_N : Cursor_T := Get_Null_Cursor;
      Instr_Scheme : Instr_Scheme_Type := Instr_Stmt);

   procedure Apply (Self : in out C_Source_Rewriter);

   procedure Start_Rewriting
     (Self         : out C_Source_Rewriter;
      Info         : in out Project_Info;
      Filename     : String;
      Preprocessed : Boolean := False);
   --  Start a rewriting session for the given file identified by its full
   --  name.
   --
   --  If Preprocessed is set to True, consider that the file was preprocessed
   --  beforehand. Otherwise, generate a preprocessed version of it in
   --  Info.Output_Dir and start a rewriting session on the latter.

   procedure Instrument_Unit
     (CU_Name   : Compilation_Unit_Name;
      IC        : in out Inst_Context;
      Unit_Info : in out Instrumented_Unit_Info);
   --  Instrument a single source file of interest from the project

   procedure Emit_Buffers_List_Unit
     (IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info);
   --  Emit in the root project a unit to contain the list of coverage buffers
   --  for all units of interest.

private

   type Pass_Kind is abstract tagged null record;

   procedure Append_SCO
     (Pass               : Pass_Kind;
      UIC                : in out C_Unit_Inst_Context'Class;
      N                  : Cursor_T;
      C1, C2             : Character;
      From, To           : Source_Location;
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
     (Pass : Pass_Kind;
      N    : Cursor_T;
      Rew  : Rewriter_T) is null;

   type C_Source_Rewriter is limited new Ada.Finalization.Limited_Controlled
   with record
      CIdx     : Index_T;
      TU       : Translation_Unit_T;
      Rewriter : Rewriter_T;
      --  Structures that should be freed after rewriting

      Output_Filename : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding procedure Initialize (Self : in out C_Source_Rewriter);
   overriding procedure Finalize (Self : in out C_Source_Rewriter);

end Instrument.C;
