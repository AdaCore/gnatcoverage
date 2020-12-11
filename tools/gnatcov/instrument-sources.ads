------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2020, AdaCore                     --
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

--  Generate SCOs and source code instrumentation

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Hash;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;
with Libadalang.Analysis;   use Libadalang.Analysis;
with Libadalang.Common;     use Libadalang.Common;
with Libadalang.Rewriting;  use Libadalang.Rewriting;

with ALI_Files;         use ALI_Files;
with SC_Obligations;    use SC_Obligations;
with Instrument.Common; use Instrument.Common;
with Strings;
with Types;             use Types;

private package Instrument.Sources is

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

   --  Generic_Subp defines the declaration and body for a generic subprogram
   --  that are produced during the tree traversal for the instrumentation
   --  of degenerate subprograms (null procedures and expression functions).

   type Generic_Subp is record
      Generic_Subp_Decl, Generic_Subp_Body : Unbounded_Wide_Wide_String;
   end record;

   package Generic_Subp_Vectors is
     new Ada.Containers.Vectors (Natural, Generic_Subp);

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

            Index : Natural := 0;
            --  Index of the element in RH_List being traversed

            Rewriting_Offset : Integer := 0;
            --  Count of nodes inserted/removed in current list so far

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

         when others =>
            null;
      end case;
   end record;

   type Root_MCDC_State_Inserter is abstract tagged null record;
   --  Abstract interface for a mechanism that allows the insertion of
   --  MC/DC state variables in a given context.

   type Any_MCDC_State_Inserter is access all Root_MCDC_State_Inserter'Class;

   package FQN_Sets is
     new Ada.Containers.Indefinite_Hashed_Sets
       (Text_Type, Ada.Strings.Wide_Wide_Hash, "=");
   --  Hashed set of fully qualified names (stored in normalized form:
   --  lower case, period separated, fully qualified).

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

   type Unit_Inst_Context is record
      Instrumented_Unit : Compilation_Unit_Name;
      --  Name of the compilation unit being instrumented

      Root_Unit : Compilation_Unit;
      --  Node of compilation unit

      Withed_Units : FQN_Sets.Set;
      --  Set of units for which we have WITH clauses

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

      Rewriting_Context : Rewriting_Handle;
      --  Rewriting handle for the instrumentation process

      MCDC_State_Inserter : Any_MCDC_State_Inserter;
      --  Service supporting insertion of temporary MC/DC state variables

      Unit_Bits : LL_Unit_Bit_Allocs;
      --  Record of allocation of coverage buffer bits for low-level SCOs

      Source_Decisions  : Source_Decision_Vectors.Vector;
      Source_Conditions : Source_Condition_Vectors.Vector;
      --  Decisions and (for MC/DC) conditions to be instrumented

      Entities : Instrumentation_Entities;
      --  Bank of nodes to use during instrumentation

      Current_Insertion_Info : Insertion_Info_Access;
      --  Insertion_Info for the list being traversed

      Degenerate_Subprogram_Index : Natural := 0;
      --  Index of last processed degenerate subprogram (null procedure or
      --  expression function) in current unit. This is used to assign
      --  unique names for generated constructs.

      Degenerate_Subprogram_Generics : Generic_Subp_Vectors.Vector;
      --  Generics to be generated in the pure buffers unit to support
      --  instrumentation of degenerate subprograms.

      Annotations : Annotation_Vectors.Vector;
      --  Annotations created during the instrumentation process, to insert in
      --  ALI_Files.ALI_Annotations afterwards, when the compilation unit
      --  (SC_Obligations.CU_Info) for this annotation is ready.
   end record;

   function Insert_MCDC_State
     (Inserter : in out Root_MCDC_State_Inserter;
      UIC      : in out Unit_Inst_Context;
      Name     : String) return String
     is abstract;
   --  Create and insert the declaration of an MC/DC state temporary object.
   --  Returns a System.Address expression denoting the declared state, for
   --  use in witness calls.

   procedure Initialize_Rewriting
     (IC                : out Unit_Inst_Context;
      Instrumented_Unit : Compilation_Unit_Name;
      Context           : Analysis_Context);
   --  Initialize a unit instrumentation context for the given unit to
   --  instrument.

   procedure Instrument_Source_File
     (CU_Name   : Compilation_Unit_Name;
      Unit_Info : Instrumented_Unit_Info;
      Prj_Info  : in out Project_Info;
      IC        : in out Inst_Context;
      UIC       : out Unit_Inst_Context);
   --  Generate the instrumented source corresponding to CU_Name/Unit_Info.
   --  Record instrumentation information in IC.
   --
   --  If the unit to instrument is also a main and the buffers dump trigger
   --  is not manual, instrumented code will also dump the coverage buffers.

   procedure Add_Auto_Dump_Buffers
     (IC   : Inst_Context;
      Info : in out Project_Info;
      Main : Ada_Qualified_Name;
      URH  : Unit_Rewriting_Handle)
      with Pre => IC.Dump_Trigger /= Manual;
   --  Try to insert in the sources of Main (a main subprogram) a call to dump
   --  the list of coverage buffers for all units of interest in Main's
   --  closure. Return without doing anything if unsuccessful.
   --
   --  Info must be the project that owns the Main unit, and URH must be a
   --  rewriting handle for the body unit that contains Main's sources.

   function Img (Bit : Any_Bit_Id) return String is
     (Strings.Img (Integer (Bit)));

   procedure Ensure_With (UIC : in out Unit_Inst_Context; Unit : Text_Type);
   --  Ensure that the unit being instrumented has a dependency on the named
   --  Unit, which must be specified in the normalized form expected for
   --  FQN_Sets (lower case, period separated, fully qualified).

   function Index_In_Rewriting_Tree (N : Ada_Node'Class) return Positive;
   --  Assuming that the rewriting node for N has a parent, return its index in
   --  that parent's list of children.

   ---------------------------------
   -- Helpers to synthetize nodes --
   ---------------------------------

   function Clone (N : Ada_Node'Class) return Node_Rewriting_Handle is
     (if N.Is_Null then No_Node_Rewriting_Handle else Clone (Handle (N)));
   --  Simple wrapper around Libadalang's Clone, except that it works on parse
   --  nodes, and accepts null nodes.

   function Detach (N : Ada_Node'Class) return Node_Rewriting_Handle;
   --  Replace N with No_Node_Rewriting_Handle, and return its previous
   --  handle for possible reuse elsewhere in the tree.

   No_Children : constant Node_Rewriting_Handle_Array :=
     (1 .. 0 => No_Node_Rewriting_Handle);

   function Make
     (UIC : Unit_Inst_Context;
      K   : Ada_Node_Kind_Type) return Node_Rewriting_Handle
   is (Create_Node (UIC.Rewriting_Context, K));
   --  Shortcut to create a node of the given kind

   function Make_Identifier
     (UIC : Unit_Inst_Context;
      Id  : Wide_Wide_String) return Node_Rewriting_Handle
   is (Create_Token_Node
         (UIC.Rewriting_Context, Libadalang.Common.Ada_Identifier, Id));
   --  Shortcut to create an identifier node

   function Make_Defining_Name
     (UIC    : Unit_Inst_Context;
      D_Name : Wide_Wide_String) return Node_Rewriting_Handle
   is (Create_Defining_Name (UIC.Rewriting_Context,
                             Make_Identifier (UIC, D_Name)));
   --  Shortcut to create a defining identifier tree

end Instrument.Sources;
