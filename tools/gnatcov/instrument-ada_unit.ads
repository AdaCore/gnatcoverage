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

--  Instrumentation of an Ada source file

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.File_Readers;
with Langkit_Support.Text;      use Langkit_Support.Text;
with Libadalang.Analysis;       use Libadalang.Analysis;
with Libadalang.Config_Pragmas; use Libadalang.Config_Pragmas;
with Libadalang.Rewriting;      use Libadalang.Rewriting;

with GNATCOLL.Refcount; use GNATCOLL.Refcount;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Files_Handling;    use Files_Handling;
with Files_Table;       use Files_Table;
with Instrument.Ada_Unit_Provider;
with Instrument.Common; use Instrument.Common;
with Switches;

package Instrument.Ada_Unit is

   type Ada_Instrumenter_Type is new Language_Instrumenter with record
      Provider : Instrument.Ada_Unit_Provider.Provider_Type;
      --  Unit provider to create an analysis context (Context member
      --  below). We use a custom provider there, to be able to turn
      --  a filename to our Compilation_Unit_Name internal representation,
      --  and to not depend on project files in the unit instrumentation
      --  process.

      File_Reader : Langkit_Support.File_Readers.File_Reader_Reference;
      --  File reader to preprocess source files when needed

      Event_Handler : Libadalang.Analysis.Event_Handler_Reference;
      --  Event handler to warn about missing source files

      Config_Pragmas_Mapping : Unbounded_String;
      --  Filename for a JSON description file of a configuration pragmas
      --  mapping: see the Save_Config_Pragmas_Mapping and
      --  Load_Config_Pragmas_Mapping procedures below.

      Context : Libadalang.Analysis.Analysis_Context;
      --  Libadalang context to load all units to rewrite

      Get_From_File_Count : Natural;
      --  Count how many times we called Context.Get_From_File. See the
      --  Max_Get_From_File_Count constant.

   end record;
   --  Instrumentation primitives for Ada

   overriding
   function Language
     (Self : Ada_Instrumenter_Type) return Switches.Src_Supported_Language
   is (Switches.Ada_Language);

   overriding
   procedure Instrument_Unit
     (Self              : in out Ada_Instrumenter_Type;
      Unit_Name         : String;
      Prj               : Prj_Desc;
      Files_Of_Interest : File_Sets.Set);

   overriding
   procedure Auto_Dump_Buffers_In_Main
     (Self        : in out Ada_Instrumenter_Type;
      Filename    : String;
      Dump_Config : Any_Dump_Config;
      Prj         : Prj_Desc);

   overriding
   procedure Emit_Dump_Helper_Unit_Manual
     (Self        : in out Ada_Instrumenter_Type;
      Dump_Config : Any_Dump_Config;
      Prj         : Prj_Desc);

   overriding
   function Dump_Manual_Helper_Unit
     (Self : Ada_Instrumenter_Type; Prj : Prj_Desc)
      return Files_Table.Compilation_Unit;

   overriding
   procedure Replace_Manual_Indications
     (Self                 : in out Ada_Instrumenter_Type;
      Prj                  : in out Prj_Desc;
      Source               : Virtual_File;
      Has_Dump_Indication  : out Boolean;
      Has_Reset_Indication : out Boolean);
   --  Once the instrumentation has finished, if the dump trigger is "manual"
   --  we expect the user to have indicated the place where a call to the
   --  manual dump buffers procedure should be inserted by the pragma
   --  statement:
   --
   --    pragma Annotate (Xcov, Dump_Buffers);
   --
   --  This pragma must be found and replaced by the actual call to the dump
   --  procedure defined in the dump helper unit.
   --
   --  Likewise, if the user wishes to reset the coverage buffers during the
   --  execution, it should be done through a pragma statement:
   --
   --    pragma Annotate (Xcov, Reset_Buffers);
   --
   --  If found, this pragma will be replaced by a call to the buffer reset
   --  procedure defined in the dump helper unit.
   --
   --  If Is_Main is True and no pragmas were found, still add a reference to
   --  the helper unit to ensure the coverage buffers are included in the
   --  main's compilation closure.

   procedure Insert_With_Dump_Helper
     (Self   : in out Ada_Instrumenter_Type;
      Source : GNATCOLL.VFS.Virtual_File;
      Prj    : in out Prj_Desc);
   --  Add a reference to the helper unit to ensure the coverage buffers are
   --  included in the main's compilation closure.

   overriding
   procedure Emit_Buffers_List_Unit
     (Self        : Ada_Instrumenter_Type;
      Instr_Units : Unit_Sets.Set;
      Prj         : Prj_Desc);

   overriding
   procedure Emit_Observability_Unit
     (Self : in out Ada_Instrumenter_Type; Prj : in out Prj_Desc);

   procedure Save_Config_Pragmas_Mapping (Filename : String);
   --  Create a configuration pragmas mapping for the loaded project and write
   --  a JSON description file of it to Filename.

   procedure Load_Config_Pragmas_Mapping
     (Mapping  : out Config_Pragmas_Mapping;
      Filename : String);
   --  Create a config pragmas mapping from the JSON description in Filename
   --  (created by the Save_Config_Pragmas_Mapping procedure) and assign it to
   --  Mapping.

   function Create_Ada_Instrumenter
     (Tag                        : Unbounded_String;
      Config_Pragmas_Mapping     : String;
      Mapping_Filename           : String;
      Preprocessor_Data_Filename : String) return Ada_Instrumenter_Type;
   --  Create an Ada instrumenter.
   --
   --  Config_Pragmas_Mapping is the fullname to the configuration pragma
   --  mapping file (created by the Save_Config_Pragmas_Mapping procedure).
   --
   --  Mapping_Filename is the fullname to the mapping file, which maps unit
   --  names to file fullnames.
   --
   --  Mapping_Filename is used to instantiate our custom unit provider, which
   --  does not rely on project files (see Instrument.Ada_Unit_Provider).
   --
   --  Preprocessor_Data_Filename is the name of the file that contains
   --  preprocessor configuration data (see Instrument.Ada_Preprocessing).

   --  Private declarations relative to the AST traversal
private
   --  Insertion_Info defines the current state for traversal of a list of
   --  statements or declarations in which witness calls are inserted.

   type Insertion_Info;
   type Insertion_Info_Access is access all Insertion_Info;

   type Insertion_Method is
     (None, Statement, Declaration, Expression_Function);
   --  Describe how to insert a call to a witness subprogram:
   --
   --  * None: no insertion method yet.
   --
   --  * Statement: insert a witness call statement.
   --
   --  * Declaration: insert a dummy declaration whose initialization
   --    expression is a witness call.
   --
   --  * Expression_Function: wrap the expression in an expression function
   --    in a case expression whose controlling expr is a witness call.

   type Insertion_Info (Method : Insertion_Method := None) is record
      case Method is
         when Statement | Declaration =>
            RH_List : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
            --  Rewriting handle for the statement/declaration list

            Preelab : Boolean := False;
            --  Whether we are traversing a list of top-level declarations in a
            --  preelaborate package. In this context, we cannot insert witness
            --  calls, precisely because of the preelaborate restriction.

            Parent : Insertion_Info_Access;
            --  Insertion_Info for the upper enclosing list

            case Method is
               when Declaration =>
                  RH_Private_List : Node_Rewriting_Handle :=
                    No_Node_Rewriting_Handle;
                  --  If RH_List is the declarations of a public part, and
                  --  there is a corresponding private part, declarations
                  --  list of the private part.

               when others =>
                  null;
            end case;

         when Expression_Function =>
            Witness_Actual, Witness_Formal : Node_Rewriting_Handle;
            --  Before creating the witness call, both components are
            --  uninitialized.
            --
            --  After the insertion, Witness_Actual is a call to the Witness
            --  function expression to discharge the statement obligation for
            --  this expression function. Witness_Formal is the augmented
            --  function formal to which the Witness_Actual is meant to be
            --  passed.
            --
            --  See the explanation about "Degenerate subprograms" in
            --  instrument-ada_unit.adb for a description of the bigger plan.

         when others =>
            null;
      end case;
   end record;

   package Insertion_Info_SP is new
     Shared_Pointers (Element_Type => Insertion_Info);
   subtype Insertion_Info_Ref is Insertion_Info_SP.Ref;

   type Source_Decision is record
      LL_SCO : Nat;
      --  Low-level SCO id of decision

      Decision : Expr;
      --  Decision expression

      State : Unbounded_String;
      --  Name of MC/DC state local variable

      Is_Contract : Boolean := False;
      --  Whether the decision belongs to an assert-like pragma statement or an
      --  equivalent aspect.
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

   package Source_Decision_Vectors is new
     Ada.Containers.Vectors (Natural, Source_Decision);
   package Source_Condition_Vectors is new
     Ada.Containers.Vectors (Natural, Source_Condition);

   type Instrumentation_Entities is record
      Buffers_Index : Natural := 0;
      --  Index into the set of this units' coverage buffers group for the
      --  source file being instrumented.

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

   type Root_MCDC_State_Inserter is abstract tagged null record;
   --  Abstract interface for a mechanism that allows the insertion of
   --  MC/DC state variables in a given context.

   type Any_MCDC_State_Inserter is access all Root_MCDC_State_Inserter'Class;

   package FQN_Sets is new
     Ada.Containers.Indefinite_Hashed_Sets
       (Text_Type,
        Ada.Strings.Wide_Wide_Hash,
        "=");
   --  Hashed set of fully qualified names (stored in normalized form:
   --  lower case, period separated, fully qualified).

   --  Generic_Subp defines the declaration and body for a generic subprogram
   --  that are produced during the tree traversal for the instrumentation
   --  of degenerate subprograms (null procedures and expression functions).

   type Generic_Subp is record
      Generic_Subp_Decl, Generic_Subp_Body : Unbounded_Wide_Wide_String;
   end record;

   package Generic_Subp_Vectors is new
     Ada.Containers.Vectors (Natural, Generic_Subp);

   type Instrument_Location_Type is
     (Before, After, Before_Parent, Inside_Expr);
   --  Where to insert the witness call for a statement:

   --  Before: common case, insert immediately before the statement in
   --  the same sequence, so that the statement is recorded as having
   --  been executed (at least partially), even if it raises an exception.
   --
   --  After: special cases where this is not legal (e.g. for the first
   --  statement of an alternative in a SELECT statement [except for a DELAY
   --  alternative, see below], which has special semantics). In these rare
   --  cases, the location indication is set to After to indicate that the
   --  witness must be inserted after the statement, not before.
   --
   --  Before_Parent: special case of a DELAY or entry call alternative: the
   --  evaluation of the delay duration, entry name, or entry call actuals
   --  occurs inconditionally as soon as the enclosing SELECT statement is
   --  executed, so we insert the witness immediately before the SELECT.
   --
   --  Inside_Expr: special cases where we cannot insert a witness call as a
   --  statement, or as a declaration (before or after). Such occurrences are
   --  elsif statements. In this case, we will insert the witness call inside
   --  the underlying boolean expression.

   type Stmt_Instr_Info_Type is record
      Insertion_N : Node_Rewriting_Handle;
      --  Node for the insertion of the witness call

      Instrument_Location : Instrument_Location_Type;
      --  Instrument location controlling the insertion of the witness call.
      --  Refer to the type definition of Instrument_Location_Type for more
      --  information.

      Insert_Info_Ref : Insertion_Info_Ref;
      --  Insertion information controlling the insertion of the witness call.
      --  The insertion of the witness call also fills this record when e.g.
      --  instrumenting an expression function. Refer to the type definition
      --  of Insertion_Info for more information.

      In_Decl_Expr : Boolean;
      --  True when this is a declaration in a declare expression
   end record;
   --  Information relative to the instrumentation of a statement

   type Block_Information is record
      Block : SCO_Id_Vectors.Vector;
      --  Statement SCOs for the block

      Last_Stmt_Instr_Info : Stmt_Instr_Info_Type;
      --  Instrumentation information for the last encountered statement of the
      --  block.

   end record;
   --  Information relative to a block of statements

   package Block_Stacks is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Block_Information);

   type Ada_Unit_Inst_Context is new Instrument.Common.Unit_Inst_Context
   with record
      Language_Version_Pragma : Unbounded_Wide_Wide_String;
      --  Language version configuration pragma for unit, if any

      Language_Version : Any_Language_Version :=
        Switches.Global_Language_Version;
      --  Most recent version of the language that can be used during
      --  instrumentation of the unit. It is determined by the language
      --  version pragma if present, otherwise it defaults to the value
      --  obtained from the --ada switch.

      CU : CU_Id := No_CU_Id;
      --  SCO identifier of the compilation unit being instrumented

      Root_Unit : Libadalang.Analysis.Compilation_Unit;
      --  Node of compilation unit

      Source_Decisions  : Source_Decision_Vectors.Vector;
      Source_Conditions : Source_Condition_Vectors.Vector;
      --  Decisions and (for MC/DC) conditions to be instrumented

      Unit_Bits : Instrument.Common.Allocated_Bits;
      --  Allocated bits in coverage buffers for low-level SCOs

      Entities : Instrumentation_Entities;
      --  Bank of nodes to use during instrumentation

      Pure_Buffer_Unit : Compilation_Unit_Part;
      --  Name of the compilation unit that holds addresses for the coverage
      --  buffers of the unit being instrumented.

      Has_No_Elaboration_Code_All : Boolean;
      --  Whether the No_Elaboration_Code_All applies to this unit

      Withed_Units : FQN_Sets.Set;
      --  Set of units for which we have WITH clauses

      Rewriting_Context : Rewriting_Handle;
      --  Rewriting handle for the instrumentation process

      MCDC_State_Inserter : Any_MCDC_State_Inserter;
      --  Service supporting insertion of temporary MC/DC state variables

      Current_Insertion_Info : Insertion_Info_Ref;
      --  Insertion_Info for the list being traversed

      Degenerate_Subprogram_Generics : Generic_Subp_Vectors.Vector;
      --  Generics to be generated in the pure buffers unit to support
      --  instrumentation of degenerate subprograms.

      Degenerate_Subprogram_Index : Natural := 0;
      --  Index of last processed degenerate subprogram (null procedure or
      --  expression function) in current unit. This is used to assign
      --  unique names for generated constructs.

      Short_Circuit_And_Or : Boolean := False;
      --  Whether the Standard.Boolean and/or operators should be
      --  considered as having short-circuit semantics.

      Ghost_Code : Boolean := False;
      --  Ghost_Code is True if we are in a context of ghost code
      --  (declarations in a ghost package, assignments to a ghost variable
      --  etc.). We will not emit any coverage obligation in this context
      --  and assume ghost code is absent at execution.

      In_Generic : Boolean := False;
      --  True when traversing nodes in a generic package or subprogram.
      --
      --  Used when the SPARK compatibility mode is enabled, to insert
      --  non-volatile witness result variables to be ghost compliant.

      Scope_Entities       : Scope_Entities_Tree;
      Current_Scope_Entity : Scope_Entities_Trees.Cursor;
      --  Information about the name, sloc, SCO range and children scopes of
      --  the current scope entity. This is modified when entering a scope
      --  (updated to the current scope), and when leaving it (updated to
      --  the current scope parent, if any).

      In_Decl_Expr : Boolean := False;
      --  True when traversing nodes that are child of a declare expression.
      --  Used to only insert constant object declarations in the declare
      --  expression, as non-constant objects are not allowed per
      --  RM 4.5.9 (5/5).

      Block_Stack : Block_Stacks.Vector;
      --  Currently processed blocks (blocks can nest in the source,
      --  when e.g. we have a nested subprogram declaration).

   end record;

   function Insert_MCDC_State
     (Inserter : in out Root_MCDC_State_Inserter;
      UIC      : in out Ada_Unit_Inst_Context'Class;
      Name     : String) return String
   is abstract;
   --  Create and insert the declaration of an MC/DC state temporary object.
   --  Returns a System.Address expression denoting the declared state, for
   --  use in witness calls.

end Instrument.Ada_Unit;
