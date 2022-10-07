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

with Ada.Characters.Conversions;        use Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with Langkit_Support;
with Langkit_Support.Slocs;    use Langkit_Support.Slocs;
with Langkit_Support.Symbols;  use Langkit_Support.Symbols;
with Libadalang.Common;        use Libadalang.Common;
with Libadalang.Expr_Eval;
with Libadalang.Introspection; use Libadalang.Introspection;
with Libadalang.Sources;       use Libadalang.Sources;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with ALI_Files;           use ALI_Files;
with Coverage_Options;    use Coverage_Options;
with Coverage;            use Coverage;
with Diagnostics;         use Diagnostics;
with Files_Table;         use Files_Table;
with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;
with Namet;               use Namet;
with Outputs;             use Outputs;
with Project;
with SCOs;
with Slocs;
with Snames;              use Snames;
with Strings;             use Strings;
with Table;
with Text_Files;          use Text_Files;

package body Instrument.Ada_Unit is

   package GPR renames GNATCOLL.Projects;
   package LAL renames Libadalang.Analysis;

   function Create_Context_Instrument
     (N : Libadalang.Analysis.Ada_Node'Class) return Context_Handle;
   --  Create a context to show that gnatcov is instrumenting the given node

   --  Internal errors are by nature bound to be fixed, so we need to
   --  artificially trigger errors to exercize the error handling machinery,
   --  and thus to check that it works as expected. This is the role of the
   --  following helpers.

   -------------------------------
   -- Create_Context_Instrument --
   -------------------------------

   function Create_Context_Instrument
     (N : Libadalang.Analysis.Ada_Node'Class) return Context_Handle is
   begin
      return Create_Context
        ("Instrumenting " & N.Kind_Name
         & " at " & N.Unit.Get_Filename & ":" & Image (N.Sloc_Range));
   end Create_Context_Instrument;

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Name) return Ada_Qualified_Name;
   --  Return the qualified name corresponding to the given name from a parse
   --  tree.

   function To_Qualified_Name
     (Name : Unbounded_String) return Ada_Qualified_Name;
   --  Return the qualified name corresponding to the given name (e.g.
   --  foo-bar -> Foo.Bar).

   procedure Auto_Dump_Buffers_In_Main
     (IC   : Inst_Context;
      Info : in out Project_Info;
      Main : Compilation_Unit_Name;
      URH  : Unit_Rewriting_Handle)
     with Pre => IC.Dump_Config.Trigger /= Manual;
   --  Common code for auto dump insertion in the main procedure, used in the
   --  Auto_Dump_Buffers_In_Main primitive for Ada_Instrumenter_Type, and
   --  from the Instrument_Source_File procedure.
   --
   --  Arguments have the same semantics as in the Auto_Dump_Buffers_In_Main
   --  primitive. The additional URH argument is the Ada source rewriter that
   --  is ready to use for the source file to instrument.

   -----------------------
   -- To_Qualified_Name --
   -----------------------

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Name) return Ada_Qualified_Name
   is
   begin
      return Result : Ada_Qualified_Name do
         case Ada_Name (Name.Kind) is
            when Ada_Dotted_Name =>
               declare
                  DN     : constant Dotted_Name := Name.As_Dotted_Name;
                  Suffix : constant Ada_Qualified_Name := To_Qualified_Name
                     (DN.F_Suffix.As_Name);
               begin
                  Result := To_Qualified_Name (DN.F_Prefix);
                  Result.Append (Suffix);
               end;

            when Ada_Single_Tok_Node =>
               declare

                  --  ??? GNATCOLL.Projects does not specify how to encode
                  --  Unicode unit names as strings, so for now, assume that we
                  --  process only codepoints in the ASCII range and thus use
                  --  Langkit_Support.Text.Image.

                  Identifier : constant Base_Types.Ada_Identifier :=
                     To_Unbounded_String (Image (Name.Text));
               begin
                  Result.Append (Identifier);
               end;

            when others =>
               raise Constraint_Error
                  with "no qualified name for " & Name.Kind'Image & " nodes";
         end case;
      end return;
   end To_Qualified_Name;

   -----------------------
   -- To_Qualified_Name --
   -----------------------

   function To_Qualified_Name
     (Name : Unbounded_String) return Ada_Qualified_Name
   is
      Result      : Ada_Qualified_Name;
      Start_Index : Positive := 1;
   begin
      for I in 1 .. Length (Name) loop
         if Element (Name, I) = '-' then
            Result.Append
              (Instrument.Base_Types.Ada_Identifier
                 (+Slice (Name, Start_Index, I - 1)));
            Start_Index := I + 1;
         end if;
      end loop;
      Result.Append
        (Instrument.Base_Types.Ada_Identifier
           (+Slice (Name, Start_Index, Length (Name))));
      return Result;
   end To_Qualified_Name;

   procedure Import_Non_Instrumented_LL_SCOs
     (UIC : Ada_Unit_Inst_Context;
      SCO_Map : LL_HL_SCO_Map);
   --  Import the low level SCO ids marked as non-instrumetned in the
   --  high level non-instrumented SCO_id sets.

   -------------------------------------
   -- Import_Non_Instrumented_LL_SCOs --
   -------------------------------------

   procedure Import_Non_Instrumented_LL_SCOs
     (UIC     : Ada_Unit_Inst_Context;
      SCO_Map : LL_HL_SCO_Map)
   is
   begin
      for LL_SCO of UIC.Non_Instr_LL_SCOs loop
         declare
            Remapped_SCO : constant SCO_Id := SCO_Map (Nat (LL_SCO));
         begin
            case Kind (Remapped_SCO) is
               when Statement => Set_Stmt_SCO_Non_Instr (Remapped_SCO);
               when Decision  => Set_Decision_SCO_Non_Instr (Remapped_SCO);
               when Condition => Set_Decision_SCO_Non_Instr_For_MCDC
                                   (Enclosing_Decision (Remapped_SCO));
               when others =>
                  null;
            end case;
         end;
      end loop;
   end Import_Non_Instrumented_LL_SCOs;

   type All_Symbols is
     (
      --  Aspects

      Dynamic_Predicate,
      Invariant,
      Post,
      Postcondition,
      Pre,
      Precondition,
      Predicate,
      Static_Predicate,
      Type_Invariant,

      --  Annotations

      Xcov);

   Symbols : constant Symbol_Table := Create_Symbol_Table;
   --  Holder for name singletons

   function Precompute_Symbol (S : All_Symbols) return Symbol_Type is
     (Find (Symbols, Canonicalize (To_Wide_Wide_String (S'Image)).Symbol));

   Precomputed_Symbols : constant array (All_Symbols) of Symbol_Type :=
     (Dynamic_Predicate => Precompute_Symbol (Dynamic_Predicate),
      Invariant         => Precompute_Symbol (Invariant),
      Post              => Precompute_Symbol (Post),
      Postcondition     => Precompute_Symbol (Postcondition),
      Pre               => Precompute_Symbol (Pre),
      Precondition      => Precompute_Symbol (Precondition),
      Predicate         => Precompute_Symbol (Predicate),
      Static_Predicate  => Precompute_Symbol (Static_Predicate),
      Type_Invariant    => Precompute_Symbol (Type_Invariant),
      Xcov              => Precompute_Symbol (Xcov));

   function As_Symbol (S : All_Symbols) return Symbol_Type is
     (Precomputed_Symbols (S));

   function As_Symbol (Id : Identifier) return Symbol_Type;
   function As_Name (Id : Identifier) return Name_Id;
   --  Canonicalize Id and return a corresponding Name_Id/Symbol_Type

   function Pragma_Name (P : Pragma_Node) return Symbol_Type;
   function Pragma_Name (P : Pragma_Node) return Name_Id;
   --  Return a symbol from Symbols corresponding to the name of the given
   --  P pragma.

   function Canonically_Equal (Left, Right : Text_Type) return Boolean;
   --  Returns whether Left and Right are equal after canonicalization

   function Aspect_Assoc_Name (A : Aspect_Assoc) return Identifier;
   function Aspect_Assoc_Name (A : Aspect_Assoc) return Symbol_Type;
   function Aspect_Assoc_Name (A : Aspect_Assoc) return Name_Id;
   --  Return a symbol from Symbols corresponding to the name of the given
   --  A aspect association.

   function Safe_Is_Ghost (N : Basic_Decl'Class) return Boolean;
   function Safe_Is_Ghost (N : LAL.Stmt'Class) return Boolean;
   --  Wrappers around P_Is_Ghost_Code to protect ourselves against property
   --  errors. If the property fails for some reason, consider that the code
   --  is not ghost.
   --
   --  For declarations, return False if at least one of the defined name is
   --  not ghost. This is what we need here, as we need to instrument the
   --  declaration if if at least one defined name is not ghost.

   function Op_Symbol_To_Name
     (Op : Libadalang.Analysis.Name) return Wide_Wide_String;
   --  Given an operator symbol (in its source representation
   --  in the form of a quoted string literal), return a name
   --  suitable for construction of a regular identifier.

   function Sloc (N : Ada_Node'Class) return Source_Location is
     (Start_Sloc (N.Sloc_Range));

   function "+" (Sloc : Source_Location) return Slocs.Local_Source_Location
   is ((Natural (Sloc.Line), Natural (Sloc.Column)));

   function Expr_Needs_Parens (Kind : Ada_Node_Kind_Type) return Boolean
   is (Kind in Ada_Quantified_Expr | Ada_If_Expr | Ada_Case_Expr);
   --  Whether nodes of type Kind must be wrapped with parens

   function Create_Identifier
     (RH : Rewriting_Handle; Text : Text_Type) return Node_Rewriting_Handle
   is (Create_Token_Node (RH, Libadalang.Common.Ada_Identifier, Text));

   function Expression_Type
     (UIC : Ada_Unit_Inst_Context;
      E   : Expr) return Base_Type_Decl;
   --  Wrapper around E.P_Expression_Type, logging a warning and returning
   --  Standard.Boolean if unable to determine the type.

   function Is_Static_Expr (E : Expr) return Boolean;
   --  Wrapper around E.P_Is_Static_Expr, logging a warning and returning
   --  False if unable to determine whether the expression is static.

   function Bool_Expr_Eval (E : Expr) return String;
   --  Wrapper around Libadalang.Expr_Eval.Expr_Eval, for static boolean
   --  expressions. Log a warning and return an empty string result if unable
   --  to evaluate the expression.

   function Is_Ghost
     (UIC : Ada_Unit_Inst_Context;
      EF  : Expr_Function) return Boolean;
   --  Return whether the given expression function is ghost (EF or its
   --  canonical declaration has a Ghost aspect).

   function Is_Generic
     (UIC  : Ada_Unit_Inst_Context;
      Decl : Basic_Decl) return Boolean;
   --  Return whether the given declaration is generic (its canonical part is
   --  generic).

   function To_Nodes
     (Handle : Rewriting_Handle;
      Name   : Ada_Qualified_Name) return Node_Rewriting_Handle
     with Pre => not Name.Is_Empty;
   --  Turn the given qualified name into a name tree for rewriting

   function Unwrap (N : Expr) return Expr;
   --  Strip Paren_Expr from N

   function Inclusive_End_Sloc
     (SL : Source_Location_Range) return Source_Location;
   --  End slocs from Libadalang nodes are exclusive: the correspond to the
   --  source location for the (hypothetical) character right after the last
   --  character that was consumed to produce the node. In gnatcov, we need the
   --  sloc of this last character, so we need to subtract 1 from the column
   --  number.

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
     (UIC : Ada_Unit_Inst_Context'Class;
      K   : Ada_Node_Kind_Type) return Node_Rewriting_Handle
   is (Create_Node (UIC.Rewriting_Context, K));
   --  Shortcut to create a node of the given kind

   function Make_Identifier
     (UIC : Ada_Unit_Inst_Context'Class;
      Id  : Wide_Wide_String) return Node_Rewriting_Handle
   is (Create_Token_Node
       (UIC.Rewriting_Context, Libadalang.Common.Ada_Identifier, Id));
   --  Shortcut to create an identifier node

   function Make_Defining_Name
     (UIC    : Ada_Unit_Inst_Context'Class;
      D_Name : Wide_Wide_String) return Node_Rewriting_Handle
   is (Create_Defining_Name (UIC.Rewriting_Context,
                             Make_Identifier (UIC, D_Name)));
   --  Shortcut to create a defining identifier tree

   ------------------------------------
   -- Indices for syntactic children --
   ------------------------------------

   I_Accept_Stmt_With_Stmts_F_Stmts : constant Integer :=
     Index (Ada_Accept_Stmt_With_Stmts, Accept_Stmt_With_Stmts_F_Stmts);
   I_Expr_Function_F_Aspects        : constant Integer :=
     Index (Ada_Expr_Function, Basic_Decl_F_Aspects);
   I_Subp_Decl_F_Aspects            : constant Integer :=
     Index (Ada_Subp_Decl, Basic_Decl_F_Aspects);
   I_Handled_Stmts_F_Stmts          : constant Integer :=
     Index (Ada_Handled_Stmts, Handled_Stmts_F_Stmts);
   I_Subp_Spec_F_Subp_Params        : constant Integer :=
     Index (Ada_Subp_Spec, Subp_Spec_F_Subp_Params);
   I_Subp_Spec_F_Name               : constant Integer :=
     Index (Ada_Subp_Spec, Subp_Spec_F_Subp_Name);

   ---------------------
   -- Unbounded texts --
   ---------------------

   T_Ghost : constant Unbounded_Text_Type := To_Unbounded_Text ("Ghost");

   -----------------
   -- Diagnostics --
   -----------------

   procedure Report
     (UIC  : Ada_Unit_Inst_Context;
      Node : Ada_Node'Class;
      Msg  : String;
      Kind : Report_Kind := Diagnostics.Error);

   procedure Report
     (Node : Ada_Node'Class;
      Msg  : String;
      Kind : Report_Kind := Diagnostics.Error);

   ------------
   -- Report --
   ------------

   procedure Report
     (UIC  : Ada_Unit_Inst_Context;
      Node : Ada_Node'Class;
      Msg  : String;
      Kind : Report_Kind := Diagnostics.Error)
   is
      LAL_Loc : constant Source_Location := Sloc (Node);
   begin
      Diagnostics.Report ((Source_File => UIC.SFI,
                           L           => (Line   => Integer (LAL_Loc.Line),
                                           Column =>
                                             Integer (LAL_Loc.Column))),
                          Msg,
                          Kind);
   end Report;

   procedure Report
     (Node : Ada_Node'Class;
      Msg  : String;
      Kind : Report_Kind := Diagnostics.Error)
   is
      SFI     : constant Source_File_Index :=
        Get_Index_From_Generic_Name
          (Node.Unit.Get_Filename,
           Kind                => Files_Table.Source_File,
           Indexed_Simple_Name => True);
      LAL_Loc : constant Source_Location := Sloc (Node);
   begin
      Diagnostics.Report ((Source_File => SFI,
                           L           => (Line   => Integer (LAL_Loc.Line),
                                           Column =>
                                             Integer (LAL_Loc.Column))),
                          Msg,
                          Kind);
   end Report;

   -------------------------------------
   -- Generation of witness fragments --
   -------------------------------------

   function Convert_To
     (IC                 : in out Ada_Unit_Inst_Context;
      From_Type, To_Type : Base_Type_Decl;
      RH_N               : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Given an expression RH_N of type From_Type, return an expression of type
   --  To_Type, introducing a type conversion if needed. Both types are
   --  expected to be boolean types (i.e. Standard.Boolean or any of its
   --  descendants).

   function Make_Decision_Witness
     (IC         : in out Ada_Unit_Inst_Context;
      Bits       : Decision_Bit_Ids;
      MCDC_State : Unbounded_String;
      Decision   : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Create a function call to witness the outcome of the given decision,
   --  to be recorded using the given bit ids. If MC/DC is requested,
   --  MCDC_State is the name of the MC/DC state local variable, else it
   --  is the empty string.

   function Make_Condition_Witness
     (IC         : in out Ada_Unit_Inst_Context;
      MCDC_State : Unbounded_String;
      Condition  : Node_Rewriting_Handle;
      Offset     : Natural;
      First      : Boolean) return Node_Rewriting_Handle;
   --  Create a function call to witness the value of the given condition,
   --  to be recorded in the given MC/DC state local variable.

   procedure Insert_Condition_Witness
     (IC     : in out Ada_Unit_Inst_Context;
      SC     : Source_Condition;
      Offset : Natural);
   --  For use when MC/DC is requested. Insert witness function call for the
   --  identified condition.

   procedure Insert_Decision_Witness
     (IC         : in out Ada_Unit_Inst_Context;
      SD         : Source_Decision;
      Path_Count : Natural);
   --  For use when decision coverage or MC/DC is requested. Insert witness
   --  function call for the identified decision.

   type Statement_Witness_Flavor is
     (Procedure_Call, Function_Call, Declaration);
   function Make_Statement_Witness
     (UIC        : Ada_Unit_Inst_Context;
      Bit        : Bit_Id;
      Flavor     : Statement_Witness_Flavor;
      In_Generic : Boolean) return Node_Rewriting_Handle;
   --  Create a procedure call statement or object declaration to witness
   --  execution of the low level SCO with the given bit id.
   --
   --  In_Generic is used to indicate whether the statment witness is destined
   --  to be inserted in a generic package or subprogram.

   procedure Ensure_With (UIC : in out Ada_Unit_Inst_Context'Class;
                          Unit : Text_Type);
   --  Ensure that the unit being instrumented has a dependency on the named
   --  Unit, which must be specified in the normalized form expected for
   --  FQN_Sets (lower case, period separated, fully qualified).

   function Index_In_Rewriting_Tree (N : Ada_Node'Class) return Positive;
   --  Assuming that the rewriting node for N has a parent, return its index in
   --  that parent's list of children.

   function Make_MCDC_State_Name (LL_SCO_Id : Nat) return String is
     ("MCDC_State_" & Img (Integer (LL_SCO_Id)));
   --  Return the name of the MC/DC state local variable for the given
   --  decision SCO.

   --  The default MC/DC state inserter inserts MC/DC state buffers as
   --  variable declarations in the nearest enclosing subprogram.

   type Default_MCDC_State_Inserter is new Root_MCDC_State_Inserter with record
      Local_Decls : Node_Rewriting_Handle;
   end record;

   overriding function Insert_MCDC_State
     (Inserter : in out Default_MCDC_State_Inserter;
      UIC      : in out Ada_Unit_Inst_Context'Class;
      Name     : String) return String;

   ------------------------------------------------
   -- Degenerate subprograms                     --
   -- (null procedures and expression functions) --
   ------------------------------------------------
   --  Degenerate subprograms require special handling because we need a place
   --  to insert witness calls for statement coverage, and in the case of
   --  expression functions, a place to declare temporary local variables
   --  for the MC/DC state buffer for any decision in the expression.
   --
   --  We provide these locations by generating a generic subprogram in the
   --  pure buffers unit, and replacing the degenerate subprogram with an
   --  instantiation of that generic subprogram. The statement witness and
   --  MC/DC state variable declarations are inserted in the generic body.
   --
   --  For expression functions, there are three instrumentation strategies,
   --  depending on its spec and the context in which it is declared.
   --
   --  In the first strategy (the "default" and most common one, which applies
   --  if not in one of the cases described bellow), we create a new augmented
   --  expression function that takes the addresses of the MC/DC states as
   --  additional formal parameters, and we pass this new function as a generic
   --  parameter in the instantiation. If the original expression function is
   --  a primitive of some type, then the augmented expression function will
   --  also be one. As such, if the original expression function has a previous
   --  declaration in the same declarative region, we also need to emit one for
   --  the augmented expression function next to it to avoid freezing issues
   --
   --  The second strategy only deals with expression functions which are
   --  primitives of their return type, if it is a tagged type. In that case,
   --  we would need to provide an overriding expression function for each
   --  augmented expression function that we add, and for each type derivation
   --  that happens on the return type, which is not manageable. In such cases,
   --  fall back to putting the expression function in a nested package so
   --  it's not considered as a primitive and (hopefully) does not trigger
   --  any compilation bug.
   --
   --  The third strategy only applies to expression functions that are located
   --  in the body of a protected object. The only elements that may appear in
   --  a protected object body are subprogram declarations, subprogram bodies
   --  and entry bodies. This prevents us from inserting a nested package
   --  for the second strategy. To circumvent this, all expression functions
   --  declared in a protected body are transformed into regular functions.
   --
   --  Also note that we wrap the generic instantiation in a nested package,
   --  so that it does not introduce unwanted additional primitive operations.
   --  We use a renaming-as-body of the instantiation to associate it with
   --  the original subprogram name, while preserving all aspects and default
   --  parameters.
   --
   --  The following examples both provide a compact summary of the
   --  transformations, and associate names (used in the implementation) to
   --  the various constructs involved.
   --
   --  Null subprogram example
   --  =======================
   --
   --  For the following null procedure:
   --
   --     --  See Degenerate_Subp_Common_Nodes.N
   --     procedure Foo
   --       (Arg1 : Arg1_Type;
   --        Arg2 : in out Arg2_Type) is null;
   --
   --  We generate the following declaration in the pure buffer unit:
   --
   --     --  See Null_Proc_Nodes.Subp_Decl
   --     generic
   --        --  See Null_Proc_Nodes.Formals
   --        type Par1 (<>) is limited private;
   --        type Par2 (<>) is limited private;
   --     procedure Null_Proc_[Subprogram_Index](S|B|U)_Gen
   --       --  See Null_Proc_Nodes.Subp_Spec and .Param_Specs
   --       (Arg1 : Par2;
   --        Arg2 : in out Par2);
   --
   --  the following body (also in the pure buffer unit):
   --
   --     --  See Complete_Null_Proc_Decls.Subp_Body
   --     procedure Null_Proc_[Subprogram_Index](S|B|U)_Gen
   --       (Arg1 : Par2;
   --        Arg2 : in out Par2) is
   --     begin
   --        --  See Null_Proc_Nodes.Stmt_List
   --        [Witness call];
   --        null;  --  See Null_Proc_Nodes.Null_Stmt
   --     end;
   --
   --  and finally the following (in the instrumented unit, replacing the
   --  original declaration):
   --
   --     procedure Foo
   --       (Arg1 : Arg1_Type;
   --        Arg2 : in out Arg2_Type);
   --
   --     --  See Degenerate_Subp_Common_Nodes.Wrapper_Pkg
   --     package Null_Proc_[Subprogram_Index](S|B|U) is
   --        --  See Complete_Null_Proc_Decls.Instance
   --        procedure Foo is new
   --          [Pure_Unit].Null_Proc_[Subprogram_Index](S|B|U)
   --            --  See Null_Proc_Nodes.Inst_Params
   --            (Arg1_Type, Arg2_Type);
   --     end Null_Proc_[Subprogram_Index](S|B|U);
   --
   --     --  See Complete_Null_Proc_Decls.Renaming_Decl
   --     procedure Foo
   --       (Arg1 : Arg1_Type;
   --        Arg2 : in out Arg2_Type)
   --     renames Null_Proc_[Subprogram_Index](S|B|U).Foo;
   --
   --  Expression function example (MC/DC) (first strategy)
   --  ====================================================
   --
   --  For the following expression function:
   --
   --
   --     ------------- Public part -------------
   --
   --     function Foo (Arg1 : Arg1_Type; Arg2 : Arg2_Type) return Boolean;
   --
   --     ------------- Private part ------------
   --
   --     --  See Degenerate_Subp_Common_Nodes.N
   --     function Foo (Arg1 : Arg1_Type; Arg2 : Arg2_Type) return Boolean
   --     is (Arg1.X and then Arg2.Y);
   --
   --  We generate the following declarations in the instrumented unit,
   --  replacing the original declaration:
   --
   --     ------------- Public Part -------------
   --
   --     --  See Create_Augmented_Expr_Function.Augmented_Expr_Function
   --     --
   --     --  Sometimes, we don't emit this declaration,
   --     --  see Augmented_Expr_Function_Needs_Decl
   --     function Foo_With_State_[Subprogram_Index]
   --       (Arg1                 : Arg1_Type;
   --        Arg2                 : Arg2_Type;
   --        MCDC_State_2         : GNATcov_RTS.Buffers.MCDC_State_Holder;
   --        Dummy_Witness_Result : Boolean)
   --        return Boolean;
   --
   --     --  If there isn't a previous declaration for the original expression
   --     --  function, we sometimes emit a forward declaration (see
   --     --  Traverse_Degenerate_Subprogram).
   --     function Foo (Arg1 : Arg1_Type; Arg2 : Arg2_Type) return Boolean;
   --
   --     ------------ Private part -------------
   --
   --     --  See Create_Augmented_Expr_Function.Augmented_Expr_Function
   --     function Foo_With_State_[Subprogram_Index]
   --       (Arg1                 : Arg1_Type;
   --        Arg2                 : Arg2_Type;
   --        MCDC_State_2         : GNATcov_RTS.Buffers.MCDC_State_Holder;
   --        Dummy_Witness_Result : Boolean)
   --        return Boolean
   --     is ([origin expression plus Witness calls using MCDC_State_2]);;
   --
   --     --  See Create_Augmented_Expr_Function.New_Expr_Function
   --     function Foo (Arg1 : Arg1_Type; Arg2 : Arg2_Type) return Boolean
   --     is (Foo_With_State_[Subprogram_Index]
   --           (Arg1,
   --            Arg2,
   --            GNATcov_RTS.Buffers.MCDC_State_Holder'(others => <>),
   --            <witness call for stmt coverage of the expr func>));
   --
   --  Expression function primitive of its return type example (2nd strategy)
   --  =======================================================================
   --
   --  For the following expression function:
   --
   --  function Foo (Arg : Arg_Type) return Ret_Type is (<some expression>);
   --
   --  where Ret_Type is a tagged type, and Foo is a primitive of Ret_Type,
   --  we geenrate the following declarations in the instrumented unit,
   --  replacing the original declaration:
   --
   --  --  See Create_Augmented_Expr_Function.Augmented_Expr_Function
   --  package Expr_Func_Pkg_[Subprogram_Index] is
   --    function Foo_With_State_[Subprogram_Index]
   --      (Arg : Arg_Type;
   --       Dummy_Witness_Result : Boolean)
   --       return Ret_Type is (<some expression>);
   --  end Expr_Func_Pkg_[Subprogram_Index];
   --
   --  --  See Create_Augmented_Expr_Function.New_Expr_Function
   --  function Foo (Arg : Arg_Type] return Ret_Type is
   --    (Expr_Func_Pkg_[Subprogram_Index].Foo_With_State_[Subprogram_Index]
   --       (Arg, <witness call for stmt of the expr func>));
   --
   --  Expression function in a protected body example (3rd strategy)
   --  ==============================================================
   --
   --  The following expression function (located in a protected body):
   --
   --  function Foo (Arg : Arg_Type) return Boolean is (Arg.X or else Arg.Y);
   --
   --  Is replaced by the following function body:
   --
   --  function Foo (Arg : Arg_Type) return Boolean is
   --    MCDC_State_2 : GNATcov_RTS.Buffers.MCDC_State_Holder :=
   --      GNATcov_RTS.Buffers.MCDC_State_Holder'(others => <>);
   --  begin
   --    <Witness statement for stmt coverage>
   --    return ([origin expression plus witness call using MCDC_State_2]);
   --  end Foo;
   --
   --
   --  The following record tracks several parse and rewriting nodes that are
   --  useful for both the instrumentation of null subprograms and expression
   --  functions (see Traverse_Degenerate_Subprogram).

   type Degenerate_Subp_Common_Nodes is record
      N : Basic_Decl;
      --  Parse node for the subprogram to instrument

      N_Spec : Subp_Spec;
      --  Shortcut for its subprogram specification

      N_Overriding : Overriding_Node;
      --  Shortcut for the subprogram "overriding" node (if any)

      N_Name : Libadalang.Analysis.Name;
      --  Shortcut for its name

      N_Params : Param_Spec_List;
      --  List of arguments for the subprogram, or No_Param_Spec_List if there
      --  is no argument list.

      Ctrl_Type : Base_Type_Decl;
      --  If the subprogram is a primitive of a tagged type, Ctrl_Type is the
      --  tagged type (No_Base_Type_Decl otherwise).

      Append_List : Node_Rewriting_Handle;
      --  Declaration list for the current context. Note that this is always
      --  "private" one if this is a package that has a private part.

      --  The generic instantiation, must be wrapped in a package so that it
      --  does not create additional primitive operations for argument types.
      --  The following Wrapper_Pkg* components implement this package.
      --  This is also used for expression functions which are primitives of
      --  their return type.

      Wrapper_Pkg_Name : Node_Rewriting_Handle;
      --  Name for this wrapper package

      Wrapper_Pkg_Decls : Node_Rewriting_Handle;
      --  List of public declarations for this wrapper package

      Wrapper_Pkg : Node_Rewriting_Handle;
      --  Declaration for this wrapper package
   end record;

   function Create_Degenerate_Subp_Common_Nodes
     (UIC              : Ada_Unit_Inst_Context;
      N                : Basic_Decl;
      N_Spec           : Subp_Spec;
      Gen_Names_Prefix : Wide_Wide_String) return Degenerate_Subp_Common_Nodes;
   --  Create all the required nodes in Degenerate_Subp_Common_Nodes from the
   --  given arguments.

   --  The expression function MC/DC state inserter inserts MC/DC state buffers
   --  as variable declarations in the generic body, and ensures that
   --  references to these variables are passed to the instrumented expression
   --  function.

   --  Holder for the various nodes used in the instrumentation of null
   --  procedures. They all relate to the generic procedure we generate in pure
   --  buffer units.

   type Null_Proc_Nodes is record
      Name : Node_Rewriting_Handle;
      --  Name of the generic procedure (raw identifier node, not the defining
      --  identifier tree).

      Formals : Node_Rewriting_Handle;
      --  List of formals for the generic procedure (i.e. what comes next
      --  right after the "generic" keyword).

      Param_Specs : Node_Rewriting_Handle;
      --  List of parameters for the generic procedure (null if the original
      --  procedure takes no argument).

      Null_Stmt : Node_Rewriting_Handle;
      --  "null" statement in the generic procedure body. We create this
      --  statement so that statement handling machinery inserts a witness call
      --  next to it later on.

      Stmt_List : Node_Rewriting_Handle;
      --  List of statements for the generic procedure body. Only contains the
      --  "null" statement intially, then is completed to also contain the
      --  witness call later.

      Subp_Spec : Node_Rewriting_Handle;
      --  Spec for the generic procedure. Note that although this rewriting
      --  node is used in the generic procedure declaration, it is cloned in
      --  order to generate the corresponding body.

      Subp_Decl : Node_Rewriting_Handle;
      --  Declaration for the generic procedure

      Inst_Params : Node_Rewriting_Handle;
      --  List of parameters for the generic procedure instantiation
   end record;

   procedure Create_Null_Proc_Nodes
     (Nodes            : out Null_Proc_Nodes;
      UIC              : Ada_Unit_Inst_Context;
      N_Spec           : Subp_Spec;
      Gen_Names_Prefix : Wide_Wide_String);
   --  Fill in Nodes to instrument a null procedure. N_Spec is its
   --  subprogram spec.
   --
   --  Gen_Names_Prefix is used to generate the name of the generic procedure.

   procedure Collect_Null_Proc_Formals
     (Common_Nodes : Degenerate_Subp_Common_Nodes;
      NP_Nodes     : Null_Proc_Nodes;
      UIC          : Ada_Unit_Inst_Context);
   --  Go through all arguments in Common_Nodes.N_Spec and create:
   --
   --  * the corresponding formal types in NP_Nodes.Formal;
   --  * the corresponding arguments in NP_Nodes.Param_Specs;
   --  * the corresponding instantiation arguments in NP_Nodes.Inst_Params.

   procedure Complete_Null_Proc_Decls
     (UIC           : Ada_Unit_Inst_Context;
      Common_Nodes  : Degenerate_Subp_Common_Nodes;
      NP_Nodes      : Null_Proc_Nodes;
      Subp_Body     : out Node_Rewriting_Handle;
      Instance      : out Node_Rewriting_Handle;
      Renaming_Decl : out Node_Rewriting_Handle);
   --  Create the body for the generic subprogram (Subp_Body), its
   --  instantiation declaration (Instance) and the renaming for this instance
   --  (Renaming_Decl).

   function Clone_Params
     (UIC    : Ada_Unit_Inst_Context;
      N_Spec : Subp_Spec) return Node_Rewriting_Handle;
   --  Create a list of formal parameters as a copy of N_Spec's. If N_Spec has
   --  no formals, return an empty list.

   type Expr_Func_MCDC_State_Inserter is new Root_MCDC_State_Inserter with
      record
         N_Spec : Subp_Spec;
         --  Subprogram spec for the original expression function

         Call_Params : Node_Rewriting_Handle;
         --  Assoc_List node for the call to the augmented expression function

         Formal_Params : Node_Rewriting_Handle;
         --  Formal parameter list where new parameters are added to hold MC/DC
         --  temporary buffers.
      end record;

   overriding function Insert_MCDC_State
     (Inserter : in out Expr_Func_MCDC_State_Inserter;
      UIC      : in out Ada_Unit_Inst_Context'Class;
      Name     : String) return String;

   procedure Create_Augmented_Expr_Function
     (UIC                          : Ada_Unit_Inst_Context;
      Common_Nodes                 : Degenerate_Subp_Common_Nodes;
      Formal_Params                : Node_Rewriting_Handle;
      Call_Params                  : Node_Rewriting_Handle;
      Keep_Aspects                 : Boolean;
      Augmented_Expr_Function      : out Node_Rewriting_Handle;
      Augmented_Expr_Function_Decl : out Node_Rewriting_Handle;
      New_Expr_Function            : out Node_Rewriting_Handle);
   --  Create the augmented expression function from the original one
   --  (Augmented_Expr_Function) and create the expression function
   --  (New_Expr_Function) that will serve as a replacement to the original
   --  one. Also create a declaration for the augmented expression function,
   --  if needed. It should be inserted right before the previous declaration
   --  of the original expression function (which is guaranteed to exist).
   --
   --  If the original expresison function is a primitive of its return type,
   --  then Agmented_Expr_Function will not be a handle to the expression
   --  function, but rather to the nested package containing the augmented
   --  expression function. In such case, Used_Nested_Package will be true.
   --
   --  If keep_Aspects is set to True, the aspects of the original expression
   --  function will be propagated to the augmented expression function,
   --  otherwise the augmented expression function will have no aspects.

   function Augmented_Expr_Function_Needs_Decl
     (N : Expr_Function) return Boolean;
   --  Whether the augmented expression function also needs a previous
   --  declaration.
   --
   --  When the original expression function is a primitive of some type, the
   --  augmented expression function will also be a primitive for that type.
   --  To avoid freezing issues we need to make sure that if the original
   --  expression function has a previous declaration, then the augmented
   --  expression function should have one as well, and it should be inserted
   --  right before the original expression function's declaration.
   --
   --  If the original expression function is not defined in the same
   --  declarative region as its prefious declaration, then there is no need to
   --  insert a declaration for the augmented expression function, beause in
   --  that case it isn't a primitive.

   function Augmented_EF_Needs_Wrapper_Package
     (Common_Nodes : Degenerate_Subp_Common_Nodes) return Boolean;
   --  Returns whether the augmented expression function needs to be wrapped in
   --  a nested package.

   function Is_Self_Referencing
     (UIC : Ada_Unit_Inst_Context;
      EF  : Expr_Function) return Boolean;
   --  Return if EF is a self-referencing expression function, i.e. if its
   --  expression has a reference to itself (for instance: it's a recursive
   --  function).

   function Return_Type_Is_Controlling
     (UIC          : Ada_Unit_Inst_Context;
      Common_Nodes : Degenerate_Subp_Common_Nodes) return Boolean with
     Pre => not Is_Null (Common_Nodes.N_Spec.F_Subp_Returns);
   --  Return True if the expression function from which the common nodes were
   --  generated is a primitive of a tagged type, and if that tagged type is
   --  the return type of the expression function.

   function Has_Matching_Pragma_For_Unit
     (IC     : Inst_Context;
      Unit   : Compilation_Unit;
      Filter : access function (Node : Pragma_Node) return Boolean)
      return Boolean;
   --  Return whether Filter return True on at least one configuration pragma
   --  that applies to Unit or system.ads.

   Unusable_System_Reported : Boolean := False;
   --  Global variable set to True once gnatcov emits a warning about a failure
   --  to get the analysis unit for System. Used to avoid emitting duplicate
   --  messages.

   function Pragma_Restricts_Finalization
     (Prag_Node : Pragma_Node) return Boolean;
   --  Return True if Prag_Node imposes a restrictions on use of finalization

   function Finalization_Restricted_In_Unit
     (IC : Inst_Context; Unit : Compilation_Unit) return Boolean;
   --  Return True if Finalization is not available in this runtime, or if
   --  some control pragma restricts the usage of finalization in either Unit
   --  or the whole project.

   function Pragma_Prevents_Task_Termination
     (Prag_Node : Pragma_Node) return Boolean;
   --  Return True if Node prevents the use of tasks and/or
   --  Ada.Task_Termination and/or Ada.Task_Identification.

   function Task_Termination_Restricted
     (IC : Inst_Context; Unit : Compilation_Unit) return Boolean;
   --  Return True if tasking is not available in this runtime, or if some
   --  configuration pragma prevents the use of tasks and/or
   --  Ada.Task_Termination and/or Ada.Task_Identification in either the whole
   --  project or in Unit.

   function Return_From_Subp_Body
     (Ret_Node : Return_Stmt; Subp : Subp_Body) return Boolean;
   --  Return whether Ret_Node is returning from Subp

   procedure Insert_Simple_Dump_Proc_Calls
     (RH          : Rewriting_Handle;
      Helper_Unit : Ada_Qualified_Name;
      Subp_Body   : LAL.Subp_Body);
   --  Insert calls, in Subp_Body, to the <Helper_Unit>.Dump_Buffers procedure
   --  as the last statment of the top level handeled statments of the main, as
   --  the last statement of each exception handler branch, and right before
   --  each return statment returning from the main procedure.

   procedure Insert_Controlled_Dump_Object_Decl
     (RH          : Rewriting_Handle;
      Helper_Unit : Ada_Qualified_Name;
      Subp_Body   : LAL.Subp_Body);
   --  Insert, in Subp_Body, the declaration of a controlled object of type
   --  <Helper_Unit>.Dump_Controlled_Type to dump the coverage buffers during
   --  finalization of said object.

   --------------------------------
   -- Instrumentation extensions --
   --------------------------------

   procedure Enter_Scope
     (UIC        : in out Ada_Unit_Inst_Context;
      Scope_Name : Text_Type;
      Sloc       : Source_Location);
   --  Wrapper around Instrument.Common.Enter_Scope. See the documentation for
   --  this function.

   ----------------------------
   -- Source level rewriting --
   ----------------------------

   procedure Initialize_Rewriting
     (IC                : out Ada_Unit_Inst_Context;
      Instrumented_Unit : Compilation_Unit_Name;
      Context           : Analysis_Context);
   --  Initialize a unit instrumentation context for the given unit to
   --  instrument.

   procedure Instrument_Source_File
     (CU_Name   : Compilation_Unit_Name;
      Unit_Info : Instrumented_Unit_Info;
      Prj_Info  : in out Project_Info;
      IC        : in out Inst_Context;
      UIC       : out Ada_Unit_Inst_Context);
   --  Generate the instrumented source corresponding to CU_Name/Unit_Info.
   --  Record instrumentation information in IC.
   --
   --  If the unit to instrument is also a main and the buffers dump trigger
   --  is not manual, instrumented code will also dump the coverage buffers.

   --------------------------
   -- Unit instrumentation --
   --------------------------

   function Buffers_List_Unit (IC : Inst_Context) return Ada_Qualified_Name;
   --  Returns the name of the unit containing the array of coverage buffers.
   --  It is named after the root project name (e.g. if the root project is
   --  p.gpr, its name is <Sys_Buffers_Lists>.<Slug for P>).

   function Pure_Buffer_Unit
     (Instrumented_Unit : Compilation_Unit_Name) return Ada_Qualified_Name;
   --  Given a unit to instrument, return the name of the unit that holds
   --  addresses to its coverage buffers.

   procedure Emit_Buffer_Unit
     (Info : in out Project_Info; UIC : Ada_Unit_Inst_Context'Class);
   --  Emit the unit to contain coverage buffers for the given instrumented
   --  unit.

   procedure Emit_Pure_Buffer_Unit
     (Info : in out Project_Info; UIC : Ada_Unit_Inst_Context'Class);
   --  Emit the unit to contain addresses for the coverage buffers

   procedure Emit_Dump_Helper_Unit
     (IC                    : Inst_Context;
      Info                  : in out Project_Info;
      Main                  : Compilation_Unit_Name;
      Helper_Unit           : out Ada_Qualified_Name;
      Override_Dump_Trigger : Any_Dump_Trigger := Manual;
      Has_Controlled        : Boolean := False);
   --  Emit the unit to contain helpers to implement the automatic dump of
   --  coverage buffers for the given Main unit. Info must be the project that
   --  owns this main. Upon return, the name of this helper unit is stored in
   --  Helper_Unit.
   --
   --  If Override_Dump_Trigger is anything other than Manual, it will be used
   --  as a dump trigger instead of the one defined in IC.Dump_Config.
   --
   --  If Has_Controlled is True, generate a controlled type for which the
   --  Finalize procedure calls the buffer dump procedure.

   ----------------
   -- Convert_To --
   ----------------

   function Convert_To
     (IC                 : in out Ada_Unit_Inst_Context;
      From_Type, To_Type : Base_Type_Decl;
      RH_N               : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
      To_Type_Indentifier : Node_Rewriting_Handle;
   begin
      --  Guard against failure to type some expression, and return node
      --  unchanged if no conversion is required.

      if From_Type.Is_Null or else To_Type.Is_Null or else From_Type = To_Type
      then
         return RH_N;
      else
         if To_Type /= To_Type.P_Bool_Type.As_Base_Type_Decl then
            Ensure_With
              (IC,
               To_Type
               .P_Top_Level_Decl (To_Type.Unit)
               .P_Canonical_Fully_Qualified_Name);
            To_Type_Indentifier :=
              Create_Identifier
                (IC.Rewriting_Context,
                 To_Type.P_Canonical_Fully_Qualified_Name);
         else
            --  The Standard package may be hidden (and the Boolean type might
            --  very well be). To avoid issues, we have an accessible package
            --  that renames Standard in GNATcov_RTS.

            To_Type_Indentifier :=
              Create_Identifier
                (IC.Rewriting_Context,
                 To_Text ("GNATcov_RTS.Std.Boolean"));
         end if;

         return Create_Call_Expr
           (IC.Rewriting_Context,
            F_Name   => To_Type_Indentifier,
            F_Suffix => RH_N);
      end if;
   end Convert_To;

   ---------------------------
   -- Make_Decision_Witness --
   ---------------------------

   function Make_Decision_Witness
     (IC         : in out Ada_Unit_Inst_Context;
      Bits       : Decision_Bit_Ids;
      MCDC_State : Unbounded_String;
      Decision   : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
      E : Instrumentation_Entities renames IC.Entities;
      D : Node_Rewriting_Handle := Decision;

      Is_MCDC : constant Boolean := Bits.Path_Bits_Base /= No_Bit_Id;

      --  Note: we can't pass Decision directly as a substitution to
      --  Create_From_Template, as this would unparse it and create a
      --  complete new tree, whereas we want to preserve the original
      --  tree so that we can instrument individual conditions for MC/DC.

      Call_Img : constant String :=
        "{}.Witness ({}"
        & "," & Img (Bits.Outcome_Bits (False))
        & "," & Img (Bits.Outcome_Bits (True))
        & (if Is_MCDC
           then ", {}"
           & ", " & Img (Bits.Path_Bits_Base)
           & ", " & To_String (MCDC_State)
           else "")
        & ")";

      RH_Call : constant Node_Rewriting_Handle :=
        Create_From_Template
          (IC.Rewriting_Context,
           Template  => To_Wide_Wide_String (Call_Img),
           Arguments => (1 => E.Common_Buffers,
                         2 => E.Decision_Buffer)
           & (if Is_MCDC
             then (1 => E.MCDC_Buffer)
             else (1 .. 0 => No_Node_Rewriting_Handle)),
           Rule      => Expr_Rule);

      D_Node : constant Expr := Node (Decision).As_Expr;
      D_Type : constant Base_Type_Decl := Expression_Type (IC, D_Node);
      B_Type : constant Base_Type_Decl := D_Node.P_Bool_Type.As_Base_Type_Decl;

   begin
      --  Wrap decisions with parens if their syntax requires. We can't always
      --  move the parens that wrap the decision in sources because they can
      --  sometimes belong to another syntactic construct, for instance:
      --
      --     pragma Assert (if A then B);

      if Expr_Needs_Parens (Kind (D)) then
         D := Create_Paren_Expr (IC.Rewriting_Context, D);
      end if;
      D := Convert_To (IC, D_Type, B_Type, D);

      --  The second child of RH_Call is its list of actual parameters

      Append_Child (Child (RH_Call, 2), D);
      return Convert_To (IC, B_Type, D_Type, RH_Call);
   end Make_Decision_Witness;

   ----------------------------
   -- Make_Condition_Witness --
   ----------------------------

   function Make_Condition_Witness
     (IC         : in out Ada_Unit_Inst_Context;
      MCDC_State : Unbounded_String;
      Condition  : Node_Rewriting_Handle;
      Offset     : Natural;
      First      : Boolean) return Node_Rewriting_Handle
   is
      E        : Instrumentation_Entities renames IC.Entities;
      Call_Img : constant String :=
        "{}.Witness (" & To_String (MCDC_State) & ","
        & Img (Offset) & "," & First'Img & ")";

      RH_Call : constant Node_Rewriting_Handle :=
        Create_From_Template
          (IC.Rewriting_Context,
           Template  => To_Wide_Wide_String (Call_Img),
           Arguments => (1 => E.Common_Buffers),
           Rule      => Expr_Rule);

      C_Node : constant Expr := Node (Condition).As_Expr;
      C_Type : constant Base_Type_Decl := Expression_Type (IC, C_Node);
      B_Type : constant Base_Type_Decl := C_Node.P_Bool_Type.As_Base_Type_Decl;

      RH_Cond : Node_Rewriting_Handle;

   begin
      --  Expressions that needs to be wrapped in a ParenExpr in the
      --  instrumented code (e.g. quantified expressions, if expressions etc.)
      --  do not necessarily have an enclosing ParenExpr in the original code
      --  (when they are the condition of a pragma Assert for instance).
      --  Rewrap the expression to produce valid instrumented code.

      if Expr_Needs_Parens (C_Node.Kind) then
         RH_Cond := Create_Paren_Expr (IC.Rewriting_Context, Condition);
      else
         RH_Cond := Condition;
      end if;

      --  The second child of RH_Call is its list of actual parameters

      Append_Child
        (Child (RH_Call, 2), Convert_To (IC, C_Type, B_Type, RH_Cond));
      return Convert_To (IC, B_Type, C_Type, RH_Call);
   end Make_Condition_Witness;

   ----------------------------
   -- Make_Statement_Witness --
   ----------------------------

   function Make_Statement_Witness
     (UIC        : Ada_Unit_Inst_Context;
      Bit        : Bit_Id;
      Flavor     : Statement_Witness_Flavor;
      In_Generic : Boolean) return Node_Rewriting_Handle
   is
      Bit_Img : constant String  := Img (Bit);
      E       : Instrumentation_Entities renames UIC.Entities;

      function Call_Img return String is
        ("{}.Witness ({}, " & Bit_Img & ")"
         & (if Flavor = Function_Call then "" else ";"));

      --  Note: package spec and package body are instrumented separately,
      --  so we need to make sure that variables declared in a body can't
      --  clash with those from the corresponding spec, hence the inclusion
      --  of the unit part in the variable name.

      function Decl_Img return String is
        ("Discard_" & UIC.Instrumented_Unit.Part'Img & Bit_Img
         & " : {}." & (if In_Generic and then Switches.SPARK_Compat
                       then "Non_Volatile_"
                       else "")
         & "Witness_Dummy_Type := " & Call_Img);

   --  Start of processing for Make_Statement_Witness

   begin
      if Flavor = Declaration then
         return Create_From_Template
           (UIC.Rewriting_Context,
            Template  => To_Wide_Wide_String (Decl_Img),
            Arguments => (1 | 2 => E.Common_Buffers, 3 => E.Statement_Buffer),
            Rule      => Object_Decl_Rule);
      else
         return Create_From_Template
           (UIC.Rewriting_Context,
            Template  => To_Wide_Wide_String (Call_Img),
            Arguments => (E.Common_Buffers, E.Statement_Buffer),
            Rule      =>
              (if Flavor = Procedure_Call then Call_Stmt_Rule else Name_Rule));
      end if;
   end Make_Statement_Witness;

   ------------------------------
   -- Insert_Condition_Witness --
   ------------------------------

   procedure Insert_Condition_Witness
     (IC     : in out Ada_Unit_Inst_Context;
      SC     : Source_Condition;
      Offset : Natural)
   is
      N : Expr renames SC.Condition;

      RH_P : constant Node_Rewriting_Handle :=
        Create_Node
          (IC.Rewriting_Context, Libadalang.Common.Ada_Identifier);
      RH_N : Node_Rewriting_Handle;

   begin
      --  No instrumentation for condition if there is no local state variable

      if Length (SC.State) = 0 then
         return;
      end if;

      --  Special case of conditional and quantified expressions: we need to
      --  move them along with their enclosing parentheses, if they exist.
      --  Otherwise, add the needed parenthesis.

      if Expr_Needs_Parens (N.Kind)
        and then Kind (N.Parent) = Ada_Paren_Expr
      then
         RH_N := Handle (N.Parent);
      else
         RH_N := Handle (N);
      end if;

      --  Detach original condition from tree so that it can be reattached
      --  inside the witness call.

      Replace (RH_N, RH_P);

      --  Now attach witness call at the place of the original condition

      Replace
        (RH_P,
         Make_Condition_Witness (IC, SC.State, RH_N, Offset, SC.First));
   end Insert_Condition_Witness;

   -----------------------------
   -- Insert_Decision_Witness --
   -----------------------------

   procedure Insert_Decision_Witness
     (IC         : in out Ada_Unit_Inst_Context;
      SD         : Source_Decision;
      Path_Count : Natural)
   is
      N : Expr renames SD.Decision;

      RH_P : constant Node_Rewriting_Handle :=
        Create_Node
          (IC.Rewriting_Context, Libadalang.Common.Ada_Identifier);

      RH_N : constant Node_Rewriting_Handle := Handle (N);

      --  Allocate bits for this decision in coverage buffers

      Bits : constant Decision_Bit_Ids :=
        Allocate_Decision_Bits
          (IC.Unit_Bits,
           (IC.SFI, +Sloc (SD.Decision)),
           SD.LL_SCO,
           SD.State,
           Path_Count);
   begin
      --  Detach original decision from tree so that it can be reattached
      --  inside the witness call.

      Replace (RH_N, RH_P);

      --  Now attach witness call at the place of the original decision

      Replace (RH_P,
               Make_Decision_Witness (IC, Bits, SD.State, RH_N));
   end Insert_Decision_Witness;

   -----------------
   -- Ensure_With --
   -----------------

   procedure Ensure_With
     (UIC  : in out Ada_Unit_Inst_Context'Class;
      Unit : Text_Type)
   is
      RH : Rewriting_Handle renames UIC.Rewriting_Context;
   begin
      if UIC.Withed_Units.Contains (Unit) then
         return;
      end if;

      Append_Child
        (Handle (UIC.Root_Unit.F_Prelude),
         Create_From_Template
           (RH,
            Template  => "with " & Unit & ";",
            Arguments => (1 .. 0 => No_Node_Rewriting_Handle),
            Rule      => With_Clause_Rule));

      UIC.Withed_Units.Include (Unit);
   end Ensure_With;

   -----------------------------
   -- Index_In_Rewriting_Tree --
   -----------------------------

   function Index_In_Rewriting_Tree (N : Ada_Node'Class) return Positive is
      RH : constant Node_Rewriting_Handle := Handle (N);
      P  : constant Node_Rewriting_Handle := Parent (RH);
   begin
      pragma Assert (P /= No_Node_Rewriting_Handle);
      for I in 1 .. Children_Count (P) loop
         if Child (P, I) = RH then
            return I;
         end if;
      end loop;

      --  If we reach this point, this means the rewriting tree is corrupted (a
      --  node does not belong to its parent's children).

      return (raise Program_Error with "corrupted rewriting tree");
   end Index_In_Rewriting_Tree;

   -----------------------
   -- Insert_MCDC_State --
   -----------------------

   function Insert_MCDC_State
     (Inserter : in out Default_MCDC_State_Inserter;
      UIC      : in out Ada_Unit_Inst_Context'Class;
      Name     : String) return String
   is
      E             : Instrumentation_Entities renames UIC.Entities;
      Var_Decl_Img  : constant String :=
        Name & "_Var : aliased {}.MCDC_State_Type;";
      Addr_Decl_Img : constant String :=
        Name & " : constant GNATCov_RTS.Sys.Address := "
        & Name & "_Var'Address;";

   begin
      Insert_Child
        (Inserter.Local_Decls, 1,
         Create_From_Template
          (UIC.Rewriting_Context,
           Template  => To_Wide_Wide_String (Var_Decl_Img),
           Arguments => (1 => E.Common_Buffers),
           Rule      => Object_Decl_Rule));
      Insert_Child
        (Inserter.Local_Decls, 2,
         Create_From_Template
          (UIC.Rewriting_Context,
           Template  => To_Wide_Wide_String (Addr_Decl_Img),
           Arguments => (1 .. 0 => No_Node_Rewriting_Handle),
           Rule      => Object_Decl_Rule));

      --  If we are adding MCDC state variables to the decls currently being
      --  instrumented, we need to indicate that we just added two extra
      --  elements in the list of declarations, so that following insertions
      --  are made at the correct index in this list.

      if UIC.Current_Insertion_Info /= null
        and then UIC.Current_Insertion_Info.Method = Declaration
        and then UIC.Current_Insertion_Info.RH_List = Inserter.Local_Decls
      then
         UIC.Current_Insertion_Info.Rewriting_Offset :=
           UIC.Current_Insertion_Info.Rewriting_Offset + 2;
      end if;

      return Name;
   end Insert_MCDC_State;

   -----------------------------------------
   -- Create_Degenerate_Subp_Common_Nodes --
   -----------------------------------------

   function Create_Degenerate_Subp_Common_Nodes
     (UIC              : Ada_Unit_Inst_Context;
      N                : Basic_Decl;
      N_Spec           : Subp_Spec;
      Gen_Names_Prefix : Wide_Wide_String) return Degenerate_Subp_Common_Nodes
   is
      RC          : Rewriting_Handle renames UIC.Rewriting_Context;
      Insert_Info : Insertion_Info renames UIC.Current_Insertion_Info.all;
   begin
      return Result : Degenerate_Subp_Common_Nodes do
         Result.N := N;
         Result.N_Spec := N_Spec;
         Result.N_Overriding := N.As_Base_Subp_Body.F_Overriding;
         Result.N_Name := N_Spec.F_Subp_Name.F_Name;
         Result.N_Params :=
           (if N_Spec.F_Subp_Params.Is_Null
            then No_Param_Spec_List
            else N_Spec.F_Subp_Params.F_Params);
         Result.Ctrl_Type := N_Spec.P_Primitive_Subp_Tagged_Type;

         Result.Append_List :=
           (if Insert_Info.RH_Private_List /= No_Node_Rewriting_Handle
            then Insert_Info.RH_Private_List
            else Insert_Info.RH_List);

         Result.Wrapper_Pkg_Name :=
           Make_Identifier (UIC, Gen_Names_Prefix & "Pkg");

         Result.Wrapper_Pkg_Decls :=
           Create_Regular_Node (RC, Ada_Ada_Node_List, No_Children);

         Result.Wrapper_Pkg :=
           Create_Package_Decl
             (RC,
              F_Package_Name => Result.Wrapper_Pkg_Name,
              F_Aspects      => No_Node_Rewriting_Handle,
              F_Public_Part  => Create_Public_Part
                                  (RC, F_Decls => Result.Wrapper_Pkg_Decls),
              F_Private_Part => No_Node_Rewriting_Handle,
              F_End_Name     => No_Node_Rewriting_Handle);
      end return;
   end Create_Degenerate_Subp_Common_Nodes;

   ----------------------------
   -- Create_Null_Proc_Nodes --
   ----------------------------

   procedure Create_Null_Proc_Nodes
     (Nodes            : out Null_Proc_Nodes;
      UIC              : Ada_Unit_Inst_Context;
      N_Spec           : Subp_Spec;
      Gen_Names_Prefix : Wide_Wide_String)
   is
      RC : Rewriting_Handle renames UIC.Rewriting_Context;

      No_Param : constant Boolean := N_Spec.F_Subp_Params.Is_Null;
   begin
      Nodes.Name := Make_Identifier (UIC, Gen_Names_Prefix & "Gen");

      Nodes.Formals := Make (UIC, Ada_Ada_Node_List);

      Nodes.Param_Specs :=
        (if No_Param
         then No_Node_Rewriting_Handle
         else Make (UIC, Ada_Param_Spec_List));

      Nodes.Null_Stmt := Make (UIC, Ada_Null_Stmt);

      Nodes.Stmt_List :=
         Create_Regular_Node (RC, Ada_Stmt_List, (1 => Nodes.Null_Stmt));

      Nodes.Subp_Spec :=
         Create_Subp_Spec
           (RC,
            F_Subp_Kind    => Make (UIC, Ada_Subp_Kind_Procedure),
            F_Subp_Name    => Create_Defining_Name (RC, Nodes.Name),

            F_Subp_Params  =>
              (if No_Param
               then No_Node_Rewriting_Handle
               else Create_Params (RC, F_Params => Nodes.Param_Specs)),

            F_Subp_Returns => No_Node_Rewriting_Handle);

      Nodes.Subp_Decl :=
         Create_Generic_Subp_Decl
           (RC,
            F_Formal_Part => Create_Generic_Formal_Part
                               (RC, F_Decls => Nodes.Formals),
            F_Subp_Decl   => Create_Generic_Subp_Internal
                               (RC,
                                F_Subp_Spec => Nodes.Subp_Spec,
                                F_Aspects   => No_Node_Rewriting_Handle),
            F_Aspects     => No_Node_Rewriting_Handle);

      Nodes.Inst_Params := Make (UIC, Ada_Assoc_List);
   end Create_Null_Proc_Nodes;

   -------------------------------
   -- Collect_Null_Proc_Formals --
   -------------------------------

   procedure Collect_Null_Proc_Formals
     (Common_Nodes : Degenerate_Subp_Common_Nodes;
      NP_Nodes     : Null_Proc_Nodes;
      UIC          : Ada_Unit_Inst_Context)
   is
      RC : Rewriting_Handle renames UIC.Rewriting_Context;

      function Gen_Type_Expr (TE : Type_Expr) return Node_Rewriting_Handle;
      --  Return the type expression to use in the generic procedure spec for a
      --  parameter of the given type.

      function Gen_Proc_Param_For
        (Spec : Param_Spec) return Node_Rewriting_Handle
      is (Create_Param_Spec
            (RC,
             F_Ids          => Clone (Spec.F_Ids),
             F_Has_Aliased  => Clone (Spec.F_Has_Aliased),
             F_Mode         => Clone (Spec.F_Mode),
             F_Type_Expr    => Gen_Type_Expr (Spec.F_Type_Expr),
             F_Default_Expr => No_Node_Rewriting_Handle,
             F_Aspects      => No_Node_Rewriting_Handle));
      --  Create and return the param spec to be used in the generic procedure
      --  parameters for Spec (a parameter spec for the null procedure to
      --  instrument).

      function Gen_Type_Expr_For_Simple_Access_Type
        (Access_Def : Type_Access_Def) return Node_Rewriting_Handle;
      --  Helper for Gen_Type_Expr, specifically for simple access types. For
      --  instance, given:
      --     access Integer
      --  This will return:
      --     access ParN
      --  See Make_Formal_Type for the meaning of ParN.

      function Gen_Type_Expr_For_Access_To_Subp
        (Access_Def : Access_To_Subp_Def) return Node_Rewriting_Handle;
      --  Helper for Gen_Type_Expr, specifically for access to subprogram
      --  types. For instance, given:
      --     access function (S : String) return Natural
      --  This will return:
      --     access function (S : ParN) return ParM
      --  See Make_Formal_Type for the meaning of Par*.

      Next_Formal_Index : Positive := 1;
      --  Unique index for each generic procedure formal type we generate (thus
      --  increased each time we add a formal type). Index unicity allows us to
      --  generate unique formal type names. This is used exclusively in
      --  Make_Formal_Type.

      function Make_Formal_Type
        (TE : Type_Expr'Class) return Node_Rewriting_Handle;
      --  Create a formal type for the given type expression and return a
      --  reference to it. This function takes care of adding the formal type
      --  declaration to NP_Nodes.Formals and the actual type (TE) to
      --  NP_Nodes.Inst_Params.
      --
      --  For instance, given:
      --     Integer
      --  This inserts the following to NP_Nodes.Formals:
      --     type ParN (<>) is limited private;
      --  plus the following to NP_Nodes.Param_Specs:
      --     Integer
      --  and finally returns:
      --     ParN

      function Make_Anonymous_Type_Decl
        (Type_Def : Node_Rewriting_Handle) return Node_Rewriting_Handle
      is (Create_Anonymous_Type_Decl
            (RC,
             F_Name          => No_Node_Rewriting_Handle,
             F_Discriminants => No_Node_Rewriting_Handle,
             F_Type_Def      => Type_Def,
             F_Aspects       => No_Node_Rewriting_Handle));
      --  Shortcut for Gen_Type_Expr_For_* subprograms. Create and return an
      --  anonymous type declaration for the given type definition.

      -------------------
      -- Gen_Type_Expr --
      -------------------

      function Gen_Type_Expr (TE : Type_Expr) return Node_Rewriting_Handle is
      begin
         --  Compute the type for the returned param spec. In the case of
         --  anonymous access types, we must deconstruct type accessed type.
         --  For instance, we must turn the following type expression:
         --
         --     not null access procedure (Line : in out String := "")
         --
         --  into the following:
         --
         --     not null access function
         --       (Line : in out [formalX])
         --        return [formalY];

         if TE.Kind = Ada_Anonymous_Type then
            declare
               TD : constant Type_Def :=
                 TE.As_Anonymous_Type.F_Type_Decl.F_Type_Def;
            begin
               --  There are two kinds of anonymous types: "simple" access
               --  types, and access to subprogram types.

               case TD.Kind is
               when Ada_Type_Access_Def =>
                  return Gen_Type_Expr_For_Simple_Access_Type
                    (TD.As_Type_Access_Def);

               when Ada_Access_To_Subp_Def =>
                  return Gen_Type_Expr_For_Access_To_Subp
                    (TD.As_Access_To_Subp_Def);

               when others =>
                  raise Program_Error with
                    "unexpected anonymous type definition: " & TD.Kind'Image;
               end case;
            end;

         else
            return Make_Formal_Type (TE);
         end if;
      end Gen_Type_Expr;

      ------------------------------------------
      -- Gen_Type_Expr_For_Simple_Access_Type --
      ------------------------------------------

      function Gen_Type_Expr_For_Simple_Access_Type
        (Access_Def : Type_Access_Def) return Node_Rewriting_Handle
      is
         Formal_Subtype_Indication : constant Subtype_Indication :=
           Access_Def.F_Subtype_Indication;
         --  Accessed type

         --  Determine if this is a controlling access parameter, in which case
         --  the corresponding formal in the generic subprogram must be
         --  explicitly null excluding.

         Formal_Subt_Decl : constant Base_Type_Decl :=
           Formal_Subtype_Indication.P_Designated_Type_Decl;
         Ctrl_Type        : Base_Type_Decl renames
           Common_Nodes.Ctrl_Type;
         Is_Controlling   : constant Boolean :=
           (if Ctrl_Type.Is_Null
            then False
            else Formal_Subt_Decl = Ctrl_Type
                 or else Formal_Subt_Decl = Ctrl_Type.P_Full_View);
         Has_Not_Null : constant Node_Rewriting_Handle :=
           (if Is_Controlling
            then Make (UIC, Ada_Not_Null_Present)
            else Clone (Access_Def.F_Has_Not_Null));
      begin
         return Make_Anonymous_Type_Decl
           (Create_Type_Access_Def
              (RC,
               F_Has_Not_Null       => Has_Not_Null,
               F_Has_All            => No_Node_Rewriting_Handle,
               F_Has_Constant       => Clone (Access_Def.F_Has_Constant),
               F_Subtype_Indication => Make_Formal_Type
                                         (Formal_Subtype_Indication)));
      end Gen_Type_Expr_For_Simple_Access_Type;

      --------------------------------------
      -- Gen_Type_Expr_For_Access_To_Subp --
      --------------------------------------

      function Gen_Type_Expr_For_Access_To_Subp
        (Access_Def : Access_To_Subp_Def) return Node_Rewriting_Handle
      is
         Orig_Spec : constant Subp_Spec := Access_Def.F_Subp_Spec;
         Subp_Kind : constant Ada_Node_Kind_Type := Orig_Spec.F_Subp_Kind.Kind;

         Orig_Params      : constant Param_Spec_List :=
           (if Orig_Spec.F_Subp_Params.Is_Null
            then No_Param_Spec_List
            else Orig_Spec.F_Subp_Params.F_Params);
         Param_Spec_Count : constant Natural :=
           (if Orig_Params.Is_Null
            then 0
            else Orig_Params.Children_Count);

         New_Params : Node_Rewriting_Handle_Array (1 .. Param_Spec_Count);
         --  List of param spec for the returned access to subprogram type

         New_Return_Type : Node_Rewriting_Handle;
         --  Return type for the returned access to subprogram type, or
         --  No_Node_Rewriting_Handle if this is a procedure.

         New_F_Subp_Params : Node_Rewriting_Handle;
         Subp_Spec         : Node_Rewriting_Handle;
         --  Intermediate nodes to create the access to subprogram type
         --  definition.
      begin
         --  Create param specs for the returned access to subprogram type

         for J in 1 .. Param_Spec_Count loop
            New_Params (J) := Gen_Proc_Param_For
              (Orig_Params.Child (J).As_Param_Spec);
         end loop;

         New_F_Subp_Params :=
           (if Param_Spec_Count > 0
            then Create_Params
                   (RC,
                    Create_Regular_Node (RC, Ada_Param_Spec_List, New_Params))
            else No_Node_Rewriting_Handle);

         --  Create its return type (if it is a function)

         New_Return_Type :=
           (if Subp_Kind = Ada_Subp_Kind_Function
            then Gen_Type_Expr (Orig_Spec.F_Subp_Returns)
            else No_Node_Rewriting_Handle);

         --  We can now create the whole subprogram spec, and then the
         --  anonymous type.

         Subp_Spec := Create_Subp_Spec
           (RC,
            F_Subp_Kind    => Create_Node (RC, Subp_Kind),
            F_Subp_Name    => No_Node_Rewriting_Handle,
            F_Subp_Params  => New_F_Subp_Params,
            F_Subp_Returns => New_Return_Type);

         return Make_Anonymous_Type_Decl
           (Create_Access_To_Subp_Def
              (RC,
               F_Has_Not_Null  => Clone (Access_Def.F_Has_Not_Null),
               F_Has_Protected => Clone (Access_Def.F_Has_Protected),
               F_Subp_Spec     => Subp_Spec));
      end Gen_Type_Expr_For_Access_To_Subp;

      ----------------------
      -- Make_Formal_Type --
      ----------------------

      function Make_Formal_Type
        (TE : Type_Expr'Class) return Node_Rewriting_Handle
      is
         Formal_Type_Name : constant Wide_Wide_String :=
           "Par" & To_Wide_Wide_String (Img (Next_Formal_Index));
         --  We are going to add a formal type in the generic procedure for the
         --  type of this argument: this is the name of this formal.

         Is_Tagged : constant Boolean :=
           TE.P_Designated_Type_Decl.P_Is_Tagged_Type;
      begin
         Next_Formal_Index := Next_Formal_Index + 1;

         --  Create the generic formal type node and add it to the list of
         --  generic formals.

         Append_Child
           (NP_Nodes.Formals,
            Create_Generic_Formal_Type_Decl
              (RC,
               F_Decl    =>
                 Create_Formal_Type_Decl
                   (RC,
                    F_Name          =>
                      Make_Defining_Name (UIC, Formal_Type_Name),

                    F_Discriminants =>
                      Make (UIC, Ada_Unknown_Discriminant_Part),

                    F_Type_Def      =>
                      Create_Private_Type_Def
                        (RC,
                         F_Has_Abstract =>
                           (if Is_Tagged
                            then Make (UIC, Ada_Abstract_Present)
                            else No_Node_Rewriting_Handle),

                         F_Has_Tagged   =>
                           (if Is_Tagged
                            then Make (UIC, Ada_Tagged_Present)
                            else No_Node_Rewriting_Handle),

                         F_Has_Limited  =>
                           Make (UIC, Ada_Limited_Present)),

                    F_Default_Type  => No_Node_Rewriting_Handle,

                    F_Aspects       => No_Node_Rewriting_Handle),

               F_Aspects => No_Node_Rewriting_Handle));

         --  Add the actual type to the instantiation. If present, strip "not
         --  null" decorations, as we need a valid expression to pass to the
         --  generic instantiation. Not passing subtype constraints is not
         --  important in this context: the generic subprogram will just happen
         --  to have less constraints on its arguments.

         declare
            Actual : Ada_Node := TE.As_Ada_Node;
         begin
            if Actual.Kind = Ada_Subtype_Indication then
               Actual := TE.As_Subtype_Indication.F_Name.As_Ada_Node;
            end if;
            Append_Child (NP_Nodes.Inst_Params, Clone (Actual));
         end;

         --  Return a reference to this formal

         return Make_Identifier (UIC, Formal_Type_Name);
      end Make_Formal_Type;

   --  Start of processing for Collect_Null_Proc_Formals

   begin
      --  Process all formals (there is nothing to do if there is none)

      if Common_Nodes.N_Params.Is_Null then
         return;
      end if;
      for J in 1 .. Common_Nodes.N_Params.Children_Count loop
         Append_Child
           (NP_Nodes.Param_Specs,
            Gen_Proc_Param_For
              (Common_Nodes.N_Params.Child (J).As_Param_Spec));
      end loop;
   end Collect_Null_Proc_Formals;

   ------------------------------
   -- Complete_Null_Proc_Decls --
   ------------------------------

   procedure Complete_Null_Proc_Decls
     (UIC           : Ada_Unit_Inst_Context;
      Common_Nodes  : Degenerate_Subp_Common_Nodes;
      NP_Nodes      : Null_Proc_Nodes;
      Subp_Body     : out Node_Rewriting_Handle;
      Instance      : out Node_Rewriting_Handle;
      Renaming_Decl : out Node_Rewriting_Handle)
   is
      RC : Rewriting_Handle renames UIC.Rewriting_Context;
      E  : Instrumentation_Entities renames UIC.Entities;
   begin
      --  Create the generic subprogram body

      Subp_Body := Create_Subp_Body
        (RC,
         F_Overriding => No_Node_Rewriting_Handle,
         F_Subp_Spec  => Clone (NP_Nodes.Subp_Spec),
         F_Aspects    => No_Node_Rewriting_Handle,

         F_Decls      => Create_Declarative_Part
                           (RC, F_Decls => Make (UIC, Ada_Ada_Node_List)),

         F_Stmts      => Create_Handled_Stmts
                           (RC,
                            F_Stmts      => NP_Nodes.Stmt_List,
                            F_Exceptions => No_Node_Rewriting_Handle),
         F_End_Name   => No_Node_Rewriting_Handle);

      --  Create an instantiation for this generic subprogram

      Instance := Create_Generic_Subp_Instantiation
        (RC,
         F_Overriding        => No_Node_Rewriting_Handle,
         F_Kind              => Make (UIC, Ada_Subp_Kind_Procedure),
         F_Subp_Name         => Make_Defining_Name
                                  (UIC, Text (Common_Nodes.N_Name)),
         F_Generic_Subp_Name => Create_Dotted_Name
                                  (RC,
                                   F_Prefix => Clone (E.Unit_Buffers),
                                   F_Suffix => Clone (NP_Nodes.Name)),
         F_Params            => NP_Nodes.Inst_Params,
         F_Aspects           => No_Node_Rewriting_Handle);

      --  Finally, create the declaration that renames the instantiated generic
      --  subprogram.

      Renaming_Decl := Create_Subp_Renaming_Decl
        (RC,
         F_Subp_Spec  => Clone (Common_Nodes.N_Spec),
         F_Overriding => Clone (Common_Nodes.N_Overriding),

         F_Renames    => Create_Renaming_Clause
           (RC,
            F_Renamed_Object => Create_Dotted_Name
              (RC,
               F_Prefix => Clone (Common_Nodes.Wrapper_Pkg_Name),
               F_Suffix => Clone (Common_Nodes.N_Name))),

         F_Aspects    => No_Node_Rewriting_Handle);
   end Complete_Null_Proc_Decls;

   ------------------
   -- Clone_Params --
   ------------------

   function Clone_Params
     (UIC    : Ada_Unit_Inst_Context;
      N_Spec : Subp_Spec) return Node_Rewriting_Handle
   is
      P : constant Params := N_Spec.F_Subp_Params;
   begin
      return (if P.Is_Null
              then Make (UIC, Ada_Param_Spec_List)
              else Clone (P.F_Params));
   end Clone_Params;

   -----------------------
   -- Insert_MCDC_State --
   -----------------------

   overriding function Insert_MCDC_State
     (Inserter : in out Expr_Func_MCDC_State_Inserter;
      UIC      : in out Ada_Unit_Inst_Context'Class;
      Name     : String) return String
   is
      RC : Rewriting_Handle renames UIC.Rewriting_Context;

      Holder_Type : constant Wide_Wide_String :=
        "GNATcov_RTS.Buffers.MCDC_State_Holder";

      State_Identifier : constant Node_Rewriting_Handle :=
        Make_Identifier (UIC, To_Wide_Wide_String (Name));

      State_Formal : constant Node_Rewriting_Handle :=
        Create_Defining_Name (RC, State_Identifier);

      State_Param_Spec : constant Node_Rewriting_Handle :=
        Create_Param_Spec
          (RC,
           F_Ids          =>
             Create_Regular_Node
               (RC,
                Ada_Defining_Name_List,
                Children => (1 => State_Formal)),
           F_Has_Aliased  => No_Node_Rewriting_Handle,
           F_Mode         => No_Node_Rewriting_Handle,
           F_Type_Expr    => Make_Identifier (UIC, Holder_Type),
           F_Default_Expr => No_Node_Rewriting_Handle,
           F_Aspects      => No_Node_Rewriting_Handle);

      State_Actual : constant Node_Rewriting_Handle :=
        Create_Qual_Expr
          (RC,
           F_Prefix => Make_Identifier (UIC, Holder_Type),
           F_Suffix =>
             Create_Aggregate
               (RC,
                F_Ancestor_Expr => No_Node_Rewriting_Handle,
                F_Assocs        =>
                  Create_Regular_Node
                    (RC,
                     Kind     => Ada_Assoc_List,
                     Children =>
                       (1 => Create_Aggregate_Assoc
                          (RC,
                           F_Designators =>
                             Create_Regular_Node
                               (RC, Ada_Alternatives_List,
                                (1 => Create_Regular_Node
                                        (RC, Ada_Others_Designator,
                                         No_Children))),
                           F_R_Expr =>
                             Create_Regular_Node
                               (RC, Ada_Box_Expr, No_Children))))));

   begin
      if Inserter.Formal_Params = No_Node_Rewriting_Handle then

         --  This is the first MC/DC state argument we need to add for this
         --  expression function. Create a non-null copy of the formal
         --  parameter list for the augmented function.

         declare
            P : constant Params := Inserter.N_Spec.F_Subp_Params;
         begin
            Inserter.Formal_Params :=
              (if P.Is_Null
               then Make (UIC, Ada_Param_Spec_List)
               else Clone (P.F_Params));
         end;
      end if;

      Append_Child (Inserter.Formal_Params, State_Param_Spec);
      Append_Child (Inserter.Call_Params, State_Actual);

      return Name & ".State'Address";
   end Insert_MCDC_State;

   ------------------------------------
   -- Create_Augmented_Expr_Function --
   ------------------------------------

   procedure Create_Augmented_Expr_Function
     (UIC                          : Ada_Unit_Inst_Context;
      Common_Nodes                 : Degenerate_Subp_Common_Nodes;
      Formal_Params                : Node_Rewriting_Handle;
      Call_Params                  : Node_Rewriting_Handle;
      Keep_Aspects                 : Boolean;
      Augmented_Expr_Function      : out Node_Rewriting_Handle;
      Augmented_Expr_Function_Decl : out Node_Rewriting_Handle;
      New_Expr_Function            : out Node_Rewriting_Handle)
   is
      RC : Rewriting_Handle renames UIC.Rewriting_Context;

      --  Compute the name of the augmented expression function

      Orig_Name_Text : constant Wide_Wide_String :=
        Text (Common_Nodes.N_Name);
      Is_Op_Symbol : constant Boolean :=
        Orig_Name_Text (Orig_Name_Text'First) = '"';

      Augmented_Expr_Func_Name : constant Wide_Wide_String :=
        (if Is_Op_Symbol
         then Op_Symbol_To_Name (Common_Nodes.N_Name) & "_Op"
         else Orig_Name_Text)
        & "_With_State_"
        & To_Wide_Wide_String (Img (UIC.Degenerate_Subprogram_Index));

      Need_WP : constant Boolean :=
        Augmented_EF_Needs_Wrapper_Package (Common_Nodes);

      --  Create the expression for New_Expr_Function that will call that
      --  augmented expression function.

      Callee    : constant Node_Rewriting_Handle :=
        (if Need_WP
         then Create_Dotted_Name
           (RC,
            F_Prefix => Clone (Common_Nodes.Wrapper_Pkg_Name),
            F_Suffix => Create_Identifier
              (RC,
               Text => Augmented_Expr_Func_Name))
         else Create_Identifier
           (RC, Text => Augmented_Expr_Func_Name));

      Call_Expr : constant Node_Rewriting_Handle := Create_Call_Expr
        (RC,
         F_Name   => Callee,
         F_Suffix => Call_Params);

      --  No need for a declaration if we are using a nested package

      Needs_Decl : constant Boolean := not Need_WP
        and then Augmented_Expr_Function_Needs_Decl
          (Common_Nodes.N.As_Expr_Function);

   begin
      --  Create New_Expr_Function, which will go right after the augmented
      --  expression function. Move all aspects from the original function to
      --  the new one.

      New_Expr_Function :=
        Create_Expr_Function
          (RC,
           F_Overriding => Clone (Common_Nodes.N_Overriding),
           F_Subp_Spec  => Clone (Common_Nodes.N_Spec),
           F_Expr       => Create_Paren_Expr (RC, Call_Expr),
           F_Aspects    => (if Keep_Aspects
                            then Detach (Common_Nodes.N.F_Aspects)
                            else No_Node_Rewriting_Handle));

      --  The original expression function becomes the augmented one:

      Augmented_Expr_Function := Handle (Common_Nodes.N);

      --  overriding keyword is purely optional, so there is no drawback in
      --  removing it.

      Replace
        (Handle (Common_Nodes.N_Overriding),
         Create_Node (RC, Ada_Overriding_Unspecified));

      --  Replace its name with the new one

      Replace
        (Handle (Common_Nodes.N_Name),
         Make_Identifier (UIC, Augmented_Expr_Func_Name));

      --  Use the "augmented formal params" (i.e. original formals plus the
      --  witness one and the MC/DC state holders).

      Set_Child
        (Handle (Common_Nodes.N_Spec),
         I_Subp_Spec_F_Subp_Params,
         Create_Params (RC, Formal_Params));

      --  If we also need a declaration for the augmented expression
      --  function, create it. Otherwise, set it to No_Node_Rewriting_Handle.

      if Needs_Decl then
         declare
            --  If the augmented EF needs to have a previous declaration, then
            --  it should be based on the previous declaration of the original
            --  EF to avoid potential visibility issues introduced by
            --  use-clauses in between the declaration and the completion.

            Previous_Spec : constant Subp_Spec :=
              Common_Nodes.N.P_Previous_Part_For_Decl.As_Subp_Decl.F_Subp_Spec;

            New_Spec : constant Node_Rewriting_Handle := Clone (Previous_Spec);
            --  Clone the spec of the original declaration

         begin
            --  Replace the original EF name by the augmented EF name

            Set_Child (New_Spec,
                       I_Subp_Spec_F_Name,
                       Make_Identifier (UIC, Augmented_Expr_Func_Name));

            --  Add the augmented params to this spec as well

            Set_Child
              (New_Spec,
               I_Subp_Spec_F_Subp_Params,
               Create_Params (RC, Clone (Formal_Params)));

            Augmented_Expr_Function_Decl := Create_Subp_Decl
              (Handle       => UIC.Rewriting_Context,
               F_Overriding => No_Node_Rewriting_Handle,
               F_Subp_Spec  => New_Spec,
               F_Aspects    => No_Node_Rewriting_Handle);

         exception
            when Exc : Property_Error =>
               Report (Node => Common_Nodes.N,
                       Msg  => "Could not find previous declaration for the"
                                & " expression function: "
                                & Ada.Exceptions.Exception_Information (Exc),
                       Kind => Low_Warning);
         end;
      else
         Augmented_Expr_Function_Decl := No_Node_Rewriting_Handle;
      end if;

      --  If the original expression function is ghost, so must be the
      --  augmented one.

      if Is_Ghost (UIC, Common_Nodes.N.As_Expr_Function) then
         declare
            Ghost_Aspect : constant Node_Rewriting_Handle :=
              Create_Aspect_Assoc
                (RC,
                 Make_Identifier (UIC, "Ghost"),
                 No_Node_Rewriting_Handle);

            Aspects : constant Node_Rewriting_Handle :=
              Create_Regular_Node (RC,
                                   Ada_Aspect_Spec,
                                   (1 => Ghost_Aspect));
         begin
            if Needs_Decl then
               Set_Child
                 (Augmented_Expr_Function_Decl,
                  I_Subp_Decl_F_Aspects,
                  Aspects);
            else
               Set_Child
                 (Handle (Common_Nodes.N),
                  I_Expr_Function_F_Aspects,
                  Aspects);
            end if;
         end;
      end if;

      if Need_WP then

         --  Put the augmented expression function in the wrapper package, and
         --  return its handle instead of the one of the expression function.

         Append_Child (Common_Nodes.Wrapper_Pkg_Decls,
                       Child  => Augmented_Expr_Function);

         Augmented_Expr_Function := Common_Nodes.Wrapper_Pkg;
      end if;

   end Create_Augmented_Expr_Function;

   ----------------------------------------
   -- Augmented_Expr_Function_Needs_Decl --
   ----------------------------------------

   function Augmented_Expr_Function_Needs_Decl
     (N : Expr_Function) return Boolean
   is
      Previous_Decl   : Basic_Decl;
      --  Will hold the previous declaration of the expression function,
      --  if any.

      Semantic_Parent, Prev_Part_Semantic_Parent : Ada_Node;
   begin
      --  Check that N is a primitive of some type

      begin
         if N.F_Subp_Spec.P_Primitive_Subp_Types'Length = 0 then
            return False;
         end if;
      exception
         when Exc : Property_Error =>
            Report (Node => N,
                    Msg  => "Could not determine if expression function is a"
                    & " primitive: "
                    & Ada.Exceptions.Exception_Information (Exc),
                    Kind => Warning);
            return False;
      end;

      --  Check that N has a previous declaration

      begin
         Previous_Decl := N.P_Previous_Part_For_Decl;

         if Previous_Decl.Is_Null or else Previous_Decl = No_Ada_Node then
            return False;
         end if;
      exception
         when Exc : Property_Error =>
            Report (Node => N,
                    Msg  => "Could not determine if expression function"
                    & " declaration has a previous part or not: "
                    & Ada.Exceptions.Exception_Information (Exc),
                    Kind => Warning);
            return False;
      end;

      --  Check that N is in a public or private part of a package decl
      --  or that N and its previous part are declared in the same declarative
      --  region.

      begin
         Semantic_Parent := N.P_Semantic_Parent;
         Prev_Part_Semantic_Parent := Previous_Decl.P_Semantic_Parent;

         if Semantic_Parent.Is_Null
           or else Prev_Part_Semantic_Parent.Is_Null
           or else
             (Semantic_Parent.Kind not in Ada_Public_Part | Ada_Private_Part
              and then Semantic_Parent /= Prev_Part_Semantic_Parent)
         then
            return False;
         end if;
      exception
         when Exc : Property_Error =>
            Report (Node => N,
                    Msg  => "Could not determine the semantic parent of the"
                    & " expression function or the semantic parent of its"
                    & " previous declaration: "
                    & Ada.Exceptions.Exception_Information (Exc),
                    Kind => Warning);
            return False;
      end;

      --  If all of the above conditions are met then the new expression
      --  needs a declaration.

      return True;
   end Augmented_Expr_Function_Needs_Decl;

   function Augmented_EF_Needs_Wrapper_Package
     (Common_Nodes : Degenerate_Subp_Common_Nodes) return Boolean is
   begin
      return Common_Nodes.Ctrl_Type /= No_Base_Type_Decl
        and then (not Common_Nodes.N_Spec.P_Return_Type.Is_Null)
        and then Common_Nodes.N_Spec.P_Return_Type = Common_Nodes.Ctrl_Type;

   exception
      when Exc : Property_Error =>
         Report (Node => Common_Nodes.N,
                 Msg  => "Could not determine the return type of the"
                 & " expression function: "
                 & Ada.Exceptions.Exception_Information (Exc),
                 Kind => Warning);
         return False;
   end Augmented_EF_Needs_Wrapper_Package;

   -------------------------
   -- Is_Self_Referencing --
   -------------------------

   function Is_Self_Referencing
     (UIC : Ada_Unit_Inst_Context;
      EF  : Expr_Function) return Boolean
   is
      EF_Decl : constant Basic_Decl := EF.As_Basic_Decl;

      function Process_Node (N : Ada_Node'Class) return Visit_Status;
      --  If N is a reference to EF_Decl, return Stop

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node (N : Ada_Node'Class) return Visit_Status is
      begin
         declare
            Is_Self_Reference : constant Boolean :=
              (N.Kind in Ada_Single_Tok_Node
               and then N.Parent.Kind not in Ada_Defining_Name
               and then N.As_Single_Tok_Node.P_Referenced_Decl = EF_Decl);
         begin
            return (if Is_Self_Reference then Stop else Into);
         end;
      exception
         when Exc : Property_Error =>

            --  If Libadalang cannot determine what N is a reference to, emit a
            --  warning and consider it's not a self-reference.

            Report
              (UIC,
               EF,
               "failed to determine referenced declaration: "
               & Ada.Exceptions.Exception_Information (Exc),
               Warning);
            return Into;
      end Process_Node;

   --  Start of processing for Is_Self_Referencing

   begin
      --  Return whether we can find at least on enode in EF's expression that
      --  is a referenc to EF itself.

      return EF.F_Expr.Traverse (Process_Node'Access) = Stop;
   end Is_Self_Referencing;

   --------------------------------
   -- Return_Type_Is_Controlling --
   --------------------------------

   function Return_Type_Is_Controlling
     (UIC          : Ada_Unit_Inst_Context;
      Common_Nodes : Degenerate_Subp_Common_Nodes) return Boolean
   is
   begin
      if Common_Nodes.Ctrl_Type.Is_Null then
         return False;
      end if;
      return Common_Nodes.N_Spec.F_Subp_Returns.P_Designated_Type_Decl
             = Common_Nodes.Ctrl_Type;
   exception
      when Exc : Property_Error =>
      Report (UIC,
              Common_Nodes.N,
              "failed to determine return type of expression function: "
              & (if Switches.Verbose
                 then Ada.Exceptions.Exception_Message (Exc)
                 else Ada.Exceptions.Exception_Information (Exc)),
              Low_Warning);
      return False;
   end Return_Type_Is_Controlling;

   ---------------------------
   -- Return_From_Subp_Body --
   ---------------------------

   function Return_From_Subp_Body
     (Ret_Node : Return_Stmt; Subp : Subp_Body) return Boolean
   is
      function Subp_Body_Is_Parent_Decl (Node : Ada_Node'Class) return Boolean;
      --  Return True if Subp is the first body in the chain of parent
      --  declarations of Node.

      ------------------------------
      -- Subp_Body_Is_Parent_Decl --
      ------------------------------

      function Subp_Body_Is_Parent_Decl (Node : Ada_Node'Class) return Boolean
      is
         Parent_Basic_Decl : constant Basic_Decl := Node.P_Parent_Basic_Decl;
         --  Parent Basic_Decl of Ret_Node
      begin
         if Parent_Basic_Decl.Is_Null then
            return False;
         end if;
         case Parent_Basic_Decl.Kind is
            when Ada_Subp_Body_Range =>
               return Parent_Basic_Decl.As_Subp_Body = Subp;

            when Ada_Accept_Stmt
               | Ada_Accept_Stmt_With_Stmts
               | Ada_Entry_Body =>

                  --  A return statment may only appear within a callable
                  --  construct (RM 6.5 (4/2)), which are either subprogram
                  --  bodies, entry bodies or accept statements (RM 6.2)). As
                  --  Subp is a Subp_Body if we encounter an accept statement
                  --  or an entry body on the way we can't be returning from
                  --  Subp.

                  return False;
            when others =>
               return Subp_Body_Is_Parent_Decl (Parent_Basic_Decl);
         end case;
      end Subp_Body_Is_Parent_Decl;

   --  Start of processing for Return_From_Body

   begin
      return Subp_Body_Is_Parent_Decl (Ret_Node);
   exception
      when Exc : Property_Error =>
         Report
           (Node => Ret_Node,
            Msg  => "Unable to determine to which body this return statment"
                    & "applies: " & Ada.Exceptions.Exception_Information (Exc),
            Kind => Low_Warning);

         --  Inserting an extranous Dump_Buffer call isn't really a problem
         --  as, at best the trace dump generated too early will be overwritten
         --  by the trace dump happening at the main end, and at worst,
         --  multiple trace files will coexist, and passing both to gnatcov
         --  will result in the same coverage results as ony passing the trace
         --  file that was created when dumping the buffers at the actual end
         --  of the main. We can thus conservatively return True if some
         --  property call fails.

         return True;
   end Return_From_Subp_Body;

   -----------------------------------------
   -- Traverse_Declarations_Or_Statements --
   -----------------------------------------

   procedure Traverse_Declarations_Or_Statements
     (IC                         : in out Inst_Context;
      UIC                        : in out Ada_Unit_Inst_Context;
      L                          : Ada_List'Class;
      Preelab                    : Boolean       := False;
      P                          : Ada_Node      := No_Ada_Node;
      Is_Select_Stmt_Alternative : Boolean       := False;
      Priv_Part                  : Private_Part  := No_Private_Part)
     with Post => UIC.Current_Insertion_Info = UIC'Old.Current_Insertion_Info;
   --  Process L, a list of statements or declarations. If P is present, it is
   --  processed as though it had been prepended to L.
   --
   --  Preelab is True if L is a list of preelaborable declarations (which do
   --  not allow elaboration code, so do not require any SCOs, and wouldn't
   --  allow insertion of witnesses).
   --
   --  Is_Select_Stmt_Alternative is True if this is a select_alternative,
   --  entry_call_alternative, or triggering_alternative: the witness for the
   --  first statement must be inserted after it, not before as we do usually.
   --
   --  If L is the list of declarations for a public part, Priv_Part is the
   --  corresponding private part (if any).
   --
   --  The postcondition ensures that the Current_Insertion_Info has been
   --  correctly reset to its value upon entry.

   --  The following Traverse_* routines perform appropriate calls to
   --  Traverse_Declarations_Or_Statements to traverse specific node kinds.

   procedure Traverse_Context_Clause
     (UIC             : in out Ada_Unit_Inst_Context;
      L               : Ada_Node_List;
      Process_Pragmas : Boolean);
   --  Traverse the context clause of a library item. No SCOs are generated,
   --  but information is extracted to govern further processing: presence of
   --  a language version configuration pragma (if Process_Pragmas is True),
   --  and semantic dependencies.

   procedure Traverse_Generic_Package_Declaration
     (IC      : in out Inst_Context;
      UIC     : in out Ada_Unit_Inst_Context;
      N       : Generic_Package_Decl;
      Preelab : Boolean);

   procedure Traverse_Handled_Statement_Sequence
     (IC  : in out Inst_Context;
      UIC : in out Ada_Unit_Inst_Context;
      N   : Handled_Stmts);

   procedure Traverse_Package_Body
     (IC      : in out Inst_Context;
      UIC     : in out Ada_Unit_Inst_Context;
      N       : Package_Body;
      Preelab : Boolean);

   procedure Traverse_Package_Declaration
     (IC      : in out Inst_Context;
      UIC     : in out Ada_Unit_Inst_Context;
      N       : Base_Package_Decl;
      Preelab : Boolean);

   procedure Traverse_Subprogram_Or_Task_Body
     (IC  : in out Inst_Context;
      UIC : in out Ada_Unit_Inst_Context;
      N   : Ada_Node);

   procedure Traverse_Sync_Definition
     (IC  : in out Inst_Context;
      UIC : in out Ada_Unit_Inst_Context;
      N   : Ada_Node);
   --  Traverse a protected definition or task definition

   --  Note regarding traversals: In a few cases where an Alternatives list is
   --  involved, pragmas such as "pragma Page" may show up before the first
   --  alternative. We skip them because we're out of statement or declaration
   --  context, so these can't be pragmas of interest for SCO purposes, and
   --  the regular alternative processing typically involves attribute queries
   --  which aren't valid for a pragma.

   procedure Process_Decisions
     (UIC : in out Ada_Unit_Inst_Context;
      N   : Ada_Node'Class;
      T   : Character);
   --  If N is Empty, has no effect. Otherwise scans the tree for the node N,
   --  to output any decisions it contains. T is one of IEGPWX (for context of
   --  expression: if/exit when/entry guard/pragma/while/expression). If T is
   --  other than X, the node N is the if expression involved, and a decision
   --  is always present (at the very least a simple decision is present at the
   --  top level).

   --------------------------
   -- Internal Subprograms --
   --------------------------

   function Has_Decision
     (UIC : Ada_Unit_Inst_Context; T : Ada_Node'Class) return Boolean;
   --  T is the node for a subtree. Returns True if any (sub)expression in T
   --  contains a nested decision (i.e. either is a logical operator, or
   --  contains a logical operator in its subtree).

   function Operator (N : Expr'Class) return Op;
   --  Return the operator node of an unary or binary expression, or No_Op if
   --  not an operator.

   function Is_Logical_Operator
     (UIC : Ada_Unit_Inst_Context; N : Ada_Node'Class) return Boolean;
   --  Return whether N is an operator that can be part of a decision (NOT or
   --  short circuit AND/OR).

   function Is_Complex_Decision
     (UIC : Ada_Unit_Inst_Context; N : Expr'Class) return Boolean;
   --  Return whether N is a complex decision, i.e. a tree of
   --  NOT/AND-THEN/OR-ELSE operators that contains at least one AND-THEN or
   --  OR-ELSE operator.

   function Is_Standard_Boolean_And_Or (N : Op) return Boolean
     with Pre => N.Kind in Ada_Op_And | Ada_Op_Or;
   --  Return whether N is a Standard.Boolean and/or operator, i.e. is not an
   --  overloading operator, both its operands are of Standard.Boolean type
   --  and its return type is of Standard.Boolean type.

   -----------------------------------------
   -- Traverse_Declarations_Or_Statements --
   -----------------------------------------

   --  Tables used by Traverse_Declarations_Or_Statements for temporarily
   --  holding statement and decision entries. These are declared globally
   --  since they are shared by recursive calls to this procedure.
   --
   --  ??? Clarify terminology (CS, SC, SD)

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
   --
   --  SC_Entry is a single entry in the following table, From:To represents
   --  the range of entries in the CS line entry, and typ is the type, with
   --  space meaning that no type letter will accompany the entry.

   type SC_Entry is record
      N           : Ada_Node;
      --  Original statement node, providing the source location associated
      --  with the statement SCO.

      Insertion_N : Node_Rewriting_Handle;
      --  Rewriting handle of the node indicating where the witness call for
      --  the statement is to be inserted.

      From : Source_Location;
      To   : Source_Location;
      Typ  : Character;

      Index : Natural := 0;
      --  1-based index of N in enclosing list, if any

      Instrument_Location : Instrument_Location_Type := Before;
      --  Position where to insert the witness call relative to Insertion_N
      --  (see declaration of Instrument_Location_Type for the meaning of
      --  the various values).

      In_Generic : Boolean := False;
      --  Wether this statment is generic code.
   end record;

   package SC is new Table.Table
     (Table_Component_Type => SC_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1000,
      Table_Increment      => 200,
      Table_Name           => "SCO_SC");
   --  Used to store statement components for a CS entry to be output as a
   --  result of the call to this procedure. SC.Last is the last entry stored,
   --  so the current statement sequence is represented by SC_Array (SC_First
   --  .. SC.Last), where SC_First is saved on entry to each recursive call to
   --  the routine.
   --
   --  Extend_Statement_Sequence adds an entry to this array, and then
   --  Set_Statement_Entry clears the entries starting with SC_First, copying
   --  these entries to the main SCO output table. The reason that we do the
   --  temporary caching of results in this array is that we want the SCO table
   --  entries for a given CS line to be contiguous, and the processing may
   --  output intermediate entries such as decision entries.

   type SD_Entry is record
      Nod : Ada_Node;
      Typ : Character;
   end record;
   --  Used to store a single entry in the following table. Nod is the node to
   --  be searched for decisions.

   package SD is new Table.Table
     (Table_Component_Type => SD_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1000,
      Table_Increment      => 200,
      Table_Name           => "SCO_SD");
   --  Used to store possible decision information. Instead of calling the
   --  Process_Decisions procedures directly, we call Process_Decisions_Defer,
   --  which simply stores the arguments in this table. Then when we clear
   --  out a statement sequence using Set_Statement_Entry, after generating
   --  the CS lines for the statements, the entries in this table result in
   --  calls to Process_Decision. The reason for doing things this way is to
   --  ensure that decisions are output after the CS line for the statements
   --  in which the decisions occur.

   procedure Traverse_Declarations_Or_Statements
     (IC                         : in out Inst_Context;
      UIC                        : in out Ada_Unit_Inst_Context;
      L                          : Ada_List'Class;
      Preelab                    : Boolean       := False;
      P                          : Ada_Node      := No_Ada_Node;
      Is_Select_Stmt_Alternative : Boolean       := False;
      Priv_Part                  : Private_Part  := No_Private_Part)
   is

      SC_First : constant Nat := SC.Last + 1;
      SD_First : constant Nat := SD.Last + 1;
      --  Record first entries used in SC/SD at this recursive level

      Current_Insertion_Info : aliased Insertion_Info :=
        (Method => None, Unsupported => False);

      procedure Extend_Statement_Sequence
        (UIC                 : Ada_Unit_Inst_Context;
         N                   : Ada_Node'Class;
         Typ                 : Character;
         Insertion_N         : Node_Rewriting_Handle :=
                                  No_Node_Rewriting_Handle;
         Instrument_Location : Instrument_Location_Type := Before);
      --  Extend the current statement sequence to encompass the node N.
      --
      --  Typ is the letter that identifies the type of statement/declaration
      --  that is being added to the sequence.
      --
      --  N is the original node from user code, and controls the source
      --  location assigned to the statement SCO.
      --
      --  In general, this is also where the witness statement is inserted, but
      --  in some rare cases, it needs to be inserted at a different place
      --  (case of a degenerated subprogram, which gets rewritten into a
      --  generic). In that case, Insertion_N indicates where to insert the
      --  witness.
      --
      --  Instrument_Location gives further information on how the witness call
      --  should be inserted.
      --
      --  ??? Instrument_Location is sometimes ignored, we should
      --  probably refactor this so the logic for determining it is
      --  more localized.

      procedure Process_Decisions_Defer (N : Ada_Node'Class; T : Character);
      pragma Inline (Process_Decisions_Defer);
      --  This routine is logically the same as Process_Decisions, except that
      --  the arguments are saved in the SD table for later processing when
      --  Set_Statement_Entry is called, which goes through the saved entries
      --  making the corresponding calls to Process_Decision. Note: the
      --  enclosing statement must have already been added to the current
      --  statement sequence, so that nested decisions are properly
      --  identified as such.

      procedure Set_Statement_Entry;
      --  Output CS entries for all statements saved in table SC, and end the
      --  current CS sequence. Then output entries for all decisions nested in
      --  these statements, which have been deferred so far.

      procedure Traverse_One (N : Ada_Node);
      --  Traverse one declaration or statement

      procedure Traverse_Subp_Decl_Or_Stub (N : Basic_Decl);
      --  Common code to handle subprogram declarations and subprogram body
      --  stubs. Also calls Traverse_Degenerate_Subprograms for null procedures
      --  and expression functions.

      procedure Traverse_Degenerate_Subprogram
        (N      : Basic_Decl;
         N_Spec : Subp_Spec);
      --  Additional specific processing for the case of degenerate
      --  subprograms (null procedures and expression functions).

      ------------------------------------------
      -- Utility functions for node synthesis --
      ------------------------------------------

      RC : Rewriting_Handle renames UIC.Rewriting_Context;

      -------------------------------
      -- Extend_Statement_Sequence --
      -------------------------------

      procedure Extend_Statement_Sequence
        (UIC                 : Ada_Unit_Inst_Context;
         N                   : Ada_Node'Class;
         Typ                 : Character;
         Insertion_N         : Node_Rewriting_Handle :=
                                  No_Node_Rewriting_Handle;
         Instrument_Location : Instrument_Location_Type := Before)
      is
         SR      : constant Source_Location_Range := N.Sloc_Range;

         F       : Source_Location := Start_Sloc (SR);
         T       : Source_Location := Inclusive_End_Sloc (SR);
         --  Source location bounds used to produre a SCO statement. By
         --  default, this should cover the same source location range as N,
         --  however for nodes that can contain themselves other statements
         --  (for instance IN statements), we select an end bound that appears
         --  before the first nested statement (see To_Node below).

         To_Node : Ada_Node := No_Ada_Node;
         --  In the case of simple statements, set to No_Ada_Node and unused.
         --  Otherwise, use F and this node's end sloc for the emitted
         --  statement source location range.

      begin
         case Kind (N) is
            when Ada_Accept_Stmt | Ada_Accept_Stmt_With_Stmts =>

               --  Make the SCO statement span until the parameters closing
               --  parent (if present). If there is no parameter, then use the
               --  entry index. If there is no entry index, fallback to the
               --  entry name.

               declare
                  Stmt : constant Accept_Stmt := N.As_Accept_Stmt;
               begin
                  if not Stmt.F_Params.Is_Null then
                     To_Node := Stmt.F_Params.As_Ada_Node;

                  elsif not Stmt.F_Entry_Index_Expr.Is_Null then
                     To_Node := Stmt.F_Entry_Index_Expr.As_Ada_Node;

                  else
                     To_Node := Stmt.F_Name.As_Ada_Node;
                  end if;
               end;

            when Ada_Case_Stmt =>
               To_Node := N.As_Case_Stmt.F_Expr.As_Ada_Node;

            when Ada_Elsif_Stmt_Part =>
               To_Node := N.As_Elsif_Stmt_Part.F_Cond_Expr.As_Ada_Node;

            when Ada_If_Stmt =>
               To_Node := N.As_If_Stmt.F_Cond_Expr.As_Ada_Node;

            when Ada_Extended_Return_Stmt =>
               To_Node := N.As_Extended_Return_Stmt.F_Decl.As_Ada_Node;

            when Ada_Base_Loop_Stmt =>
               To_Node := N.As_Base_Loop_Stmt.F_Spec.As_Ada_Node;

            when Ada_Select_Stmt
               | Ada_Single_Protected_Decl
               | Ada_Single_Task_Decl
            =>
               T := F;

            when Ada_Protected_Type_Decl
               | Ada_Task_Type_Decl
            =>
               declare
                  Aspects       : constant Aspect_Spec :=
                    (if N.Kind = Ada_Protected_Type_Decl
                     then N.As_Protected_Type_Decl.F_Aspects
                     else N.As_Task_Type_Decl.F_Aspects);
                  Discriminants : constant Discriminant_Part :=
                    (if N.Kind = Ada_Protected_Type_Decl
                     then N.As_Protected_Type_Decl.F_Discriminants
                     else N.As_Task_Type_Decl.F_Discriminants);
               begin
                  if not Aspects.Is_Null then
                     To_Node := Aspects.As_Ada_Node;

                  elsif not Discriminants.Is_Null then
                     To_Node := Discriminants.As_Ada_Node;

                  else
                     To_Node := N.As_Base_Type_Decl.F_Name.As_Ada_Node;
                  end if;
               end;

            when Ada_Expr =>
               To_Node := N.As_Ada_Node;

            when Ada_Null_Subp_Decl =>
               --  Special case: this SCO is for the fictitious NULL statement
               --  in a null procedure. The assigned sloc is that of the NULL
               --  token in the sequence "<last token of spec> IS NULL".

               declare
                  function NNT (TR : Token_Reference) return Token_Reference is
                    (Next (TR, Exclude_Trivia => True));
                  --  Next with no trivia (i.e. excluding whitespace/comment
                  --  tokens).

                  Null_Token : constant Token_Reference :=
                    NNT (NNT (N.As_Null_Subp_Decl.F_Subp_Spec.Token_End));
               begin
                  F := Start_Sloc (Sloc_Range (Data (Null_Token)));
               end;

            when others =>
               null;
         end case;

         if not To_Node.Is_Null then
            T := Inclusive_End_Sloc (To_Node.Sloc_Range);
         end if;

         SC.Append
           ((N                   => N.As_Ada_Node,
             Insertion_N         =>
                 (if Insertion_N = No_Node_Rewriting_Handle
                  then Handle (N)
                  else Insertion_N),
             From                => F,
             To                  => T,
             Typ                 => Typ,
             Index               => (case UIC.Current_Insertion_Info.Method is
                                     when Statement | Declaration =>
                                       UIC.Current_Insertion_Info.Index,
                                     when others => 0),
             Instrument_Location =>
               --  See the comment attached to the declaration of the
               --  Instrument_Location_Type.

               (if Is_Select_Stmt_Alternative
                   and then N = L.Children (L.Children'First)
                then (case N.Kind is
                        when Ada_Delay_Stmt
                           | Ada_Call_Stmt => Before_Parent,
                        when others        => After)
                else Instrument_Location),

             In_Generic          => UIC.In_Generic));
      end Extend_Statement_Sequence;

      -----------------------------
      -- Process_Decisions_Defer --
      -----------------------------

      procedure Process_Decisions_Defer (N : Ada_Node'Class; T : Character) is
      begin
         SD.Append ((N.As_Ada_Node, T));
      end Process_Decisions_Defer;

      -------------------------
      -- Set_Statement_Entry --
      -------------------------

      procedure Set_Statement_Entry is
         SC_Last : constant Int := SC.Last;
         SD_Last : constant Int := SD.Last;

         procedure Insert_Statement_Witness (SCE : SC_Entry; LL_SCO_Id : Nat)
           with Pre =>
             (case UIC.Current_Insertion_Info.Method is
              when Statement | Declaration =>
                not UIC.Current_Insertion_Info.Preelab
                  and then
                UIC.Current_Insertion_Info.RH_List /= No_Node_Rewriting_Handle,
              when Expression_Function     => True,
              when None                    => False);
         --  Insert statement witness call for the given SCE

         ------------------------------
         -- Insert_Statement_Witness --
         ------------------------------

         procedure Insert_Statement_Witness
           (SCE : SC_Entry; LL_SCO_Id : Nat)
         is
            Insert_List : Node_Rewriting_Handle;
            Insert_Pos  : Natural;
            Insert_Info : Insertion_Info_Access := UIC.Current_Insertion_Info;

            Bit : Any_Bit_Id;
         begin
            --  Create an artificial internal error, if requested

            Raise_Stub_Internal_Error_For (Ada_Instrument_Insert_Stmt_Witness);

            --  If the current code pattern is actually unsupported, do not
            --  even try to insert the witness call or allocate bits for it in
            --  the buffers. Mark the corresponding SCO as non-instrumented
            --  instead.

            if Insert_Info.Unsupported then
               UIC.Non_Instr_LL_SCOs.Include (SCO_Id (LL_SCO_Id));
               return;
            end if;

            --  Allocate a bit in the statement coverage buffer

            Bit := Allocate_Statement_Bit (UIC.Unit_Bits, LL_SCO_Id);

            case Insert_Info.Method is

            when Statement | Declaration =>

               if SCE.Instrument_Location = Inside_Expr then
                  declare
                     Old_Cond : Node_Rewriting_Handle := SCE.Insertion_N;
                     New_Cond : constant Node_Rewriting_Handle :=
                       Create_Regular_Node
                         (RC,
                          Ada_Bin_Op,
                          Children =>
                            (1 => Make_Statement_Witness
                               (UIC,
                                Bit        => Bit,
                                Flavor     => Function_Call,
                                In_Generic => SCE.In_Generic),

                             2 => Make (UIC, Ada_Op_Or_Else),

                             --  Placeholder for relocation of old condition
                             --  after it is detached from the tree.

                             3 => No_Node_Rewriting_Handle));

                  begin
                     --  Detach old condition from tree and replace it with
                     --  OR ELSE node.

                     Replace (Old_Cond, New_Cond);

                     --  Now reattach old condition in new OR ELSE node. If it
                     --  is AND, OR, XOR, AND THEN binary operation or an IF
                     --  expression, or a quantified expression (FOR ALL, FOR
                     --  SOME), we need to wrap it in parens to generate valid
                     --  code.

                     --  Now reattach old condition in the new OR ELSE node. We
                     --  will wrap it in parens to preserve the semantics of
                     --  the condition.

                     Old_Cond := Create_Regular_Node
                       (RC,
                        Ada_Paren_Expr,
                        Children => (1 => Old_Cond));

                     Set_Child (New_Cond, 3, Old_Cond);
                  end;

               else
                  if Kind (SCE.Insertion_N) = Ada_Accept_Stmt_With_Stmts
                     and then SCE.Instrument_Location = After
                  then
                     --  In the case of an accept_statement containing a
                     --  sequence of statements, if Instrument_Location is
                     --  After, we want to call the witness after the entry has
                     --  been accepted, but before the enclosed statements are
                     --  executed, so insert the witness call in the inner
                     --  statement list at first position.

                     Insert_List :=
                       Child
                         (Child
                            (SCE.Insertion_N,
                             I_Accept_Stmt_With_Stmts_F_Stmts),
                          I_Handled_Stmts_F_Stmts);

                     Insert_Pos  := 1;

                  else
                     if SCE.Instrument_Location = Before_Parent then
                        Insert_Info := Insert_Info.Parent;
                        Insert_Pos := Insert_Info.Index;
                     else
                        Insert_Pos := SCE.Index;
                     end if;

                     Insert_List := Insert_Info.RH_List;

                     --  Adjust insertion to account for any insertion
                     --  performed outside of the processing of the current
                     --  list (case of the above special processing for accept
                     --  statements).  Note that SCE.N might not be a direct
                     --  element of the enclosing list (e.g. in the case where
                     --  it is a named statement), so we must first go up to
                     --  the parent of SCE.N that *is* an element of that list,
                     --  and *then* scan forward to determine the current
                     --  position of that parent note within the list.

                     declare
                        RH_Element_Node : Node_Rewriting_Handle :=
                          SCE.Insertion_N;
                        RH_Children_Count : constant Natural :=
                          Children_Count (Insert_Info.RH_List);
                     begin
                        --  Find the parent of SCE.N that is an element of the
                        --  enclosing list.

                        while Parent (RH_Element_Node)
                          /= Insert_Info.RH_List
                        loop
                           RH_Element_Node := Parent (RH_Element_Node);
                        end loop;

                        --  Scan forward in enclosing list for adjusted
                        --  position of the element node.

                        while Child
                          (Insert_Info.RH_List,
                           Integer (Insert_Pos
                             + Insert_Info.Rewriting_Offset))
                          /= RH_Element_Node
                        loop
                           Insert_Info.Rewriting_Offset :=
                             Insert_Info.Rewriting_Offset + 1;
                           pragma Assert
                             (Insert_Pos + Insert_Info.Rewriting_Offset
                              <= RH_Children_Count);
                        end loop;
                     end;

                     --  The witness is inserted at the current location of the
                     --  statement, so that it will occur immediately *before*
                     --  it in the instrumented sources. This is necessary
                     --  because we want to mark a statement as executed
                     --  anytime it has commenced execution (including in cases
                     --  it raises an exception or otherwise transfers
                     --  control). However in some special cases we have to
                     --  insert after the statement, see comment for
                     --  Instrument_Location_Type.
                     --
                     --  The cases where we need to instrument inside an
                     --  expression are handled before, as they do not trigger
                     --  the insertion of a new statement in a statement list.

                     Insert_Pos := Insert_Pos
                       + Insert_Info.Rewriting_Offset
                       + (case SCE.Instrument_Location is
                             when Before | Before_Parent => 0,
                             when After                  => 1,
                             when Inside_Expr            =>
                                raise Program_Error);
                  end if;

                  --  Insert witness statement or declaration

                  Insert_Child
                    (Handle => Insert_List,
                     Index  => Insert_Pos,
                     Child  =>
                       Make_Statement_Witness
                         (UIC,
                          Bit        => Bit,
                          Flavor     =>
                            (case Insert_Info.Method is
                             when Statement => Procedure_Call,
                             when Declaration => Declaration,
                             when Expression_Function | None =>
                                    raise Program_Error),
                          In_Generic => SCE.In_Generic));

                  --  Update the rewriting offset iff we inserted an element in
                  --  the current rewriting list: that offset specifically
                  --  refers to that list, whereas we may have inserted an item
                  --  in a nested list, in which case we will adjust
                  --  automatically the rewriting offset accordingly when
                  --  processing that list.

                  if Insert_Info.RH_List = Insert_List then
                     Insert_Info.Rewriting_Offset :=
                       Insert_Info.Rewriting_Offset + 1;
                  end if;
               end if;

            when Expression_Function =>

               --  Create both the witness call and a formal parameter to
               --  accept it as an actual.

               declare
                  RC : constant Rewriting_Handle := UIC.Rewriting_Context;

                  Formal_Name   : constant Node_Rewriting_Handle :=
                    Make_Identifier (UIC, "Dummy_Witness_Result");
                  Formal_Def_Id : constant Node_Rewriting_Handle :=
                    Create_Regular_Node
                      (RC,
                       Ada_Defining_Name_List,
                       Children => (1 => Create_Defining_Name
                                           (RC, Formal_Name)));
               begin
                  Insert_Info.Witness_Actual := Make_Statement_Witness
                    (UIC,
                     Bit        => Bit,
                     Flavor     => Function_Call,
                     In_Generic => SCE.In_Generic);

                  Insert_Info.Witness_Formal := Create_Param_Spec
                    (RC,
                     F_Ids          => Formal_Def_Id,
                     F_Has_Aliased  => No_Node_Rewriting_Handle,
                     F_Mode         => No_Node_Rewriting_Handle,
                     F_Type_Expr    => Make_Identifier (UIC, "Boolean"),
                     F_Default_Expr => No_Node_Rewriting_Handle,
                     F_Aspects      => No_Node_Rewriting_Handle);
               end;

            when None =>
               raise Program_Error;
            end case;
         end Insert_Statement_Witness;

      --  Start of processing for Set_Statement_Entry

      begin
         --  Output statement entries from saved entries in SC table

         for J in SC_First .. SC_Last loop
            declare
               SCE       : SC_Entry renames SC.Table (J);
               Dummy_Ctx : constant Context_Handle :=
                 Create_Context_Instrument (SCE.N);

               Is_Pragma          : constant Boolean :=
                 SCE.N.Kind = Ada_Pragma_Node;
               Pragma_Aspect_Name : constant Name_Id :=
                 (if Is_Pragma
                  then Pragma_Name (SCE.N.As_Pragma_Node)
                  else Namet.No_Name);

            begin
               Append_SCO
                 (C1                 => 'S',
                  C2                 => SCE.Typ,
                  From               => +SCE.From,
                  To                 => +SCE.To,
                  Last               => (J = SC_Last),
                  Pragma_Aspect_Name => Pragma_Aspect_Name);

               --  Insert a witness call for this statement obligation
               --  unless...

               if
                  --  ... there is no enclosing list to which a witness call
                  --  can be attached.

                  UIC.Current_Insertion_Info.Method /= None

                  --  ... this is a top-level declaration in a Preelaborate
                  --  package.

                  and then (UIC.Current_Insertion_Info.Method
                              not in Statement | Declaration
                            or else not UIC.Current_Insertion_Info.Preelab)

                  --  ... this is a pragma that we know for certain will not
                  --  generate code (such as Annotate or elaboration control
                  --  pragmas).

                  and then (not Is_Pragma
                            or else
                            Pragma_Might_Generate_Code
                              (Case_Insensitive_Get_Pragma_Id
                                 (Pragma_Aspect_Name)))

                   --  ... this is a disabled pragma that we assume will not
                   --  generate code.

                   and then SCE.Typ /= 'p'
               then
                  Insert_Statement_Witness (SCE, SCOs.SCO_Table.Last);
               end if;
            end;
         end loop;

         --  Clear out used section of SC table

         SC.Set_Last (SC_First - 1);

         --  Output any embedded decisions

         for J in SD_First .. SD_Last loop
            declare
               SDE : SD_Entry renames SD.Table (J);

            begin
               Process_Decisions (UIC, SDE.Nod, SDE.Typ);
            end;
         end loop;

         --  Clear out used section of SD table

         SD.Set_Last (SD_First - 1);
      end Set_Statement_Entry;

      ------------------------------------
      -- Traverse_Degenerate_Subprogram --
      ------------------------------------

      procedure Traverse_Degenerate_Subprogram
        (N      : Basic_Decl;
         N_Spec : Subp_Spec)
      is
         --  See the "Degenerate subprograms" comment section above for a
         --  description of the of transformation we implement in this
         --  procedure.

         Saved_Insertion_Info : constant Insertion_Info_Access :=
           UIC.Current_Insertion_Info;
         --  Insertion info inherited from the caller, which "points" to the
         --  degenerate subprogram N. We "save" it because this procedure
         --  transiently changes UIC.Current_Insertion_Info.

         Saved_MCDC_State_Inserter : constant Any_MCDC_State_Inserter :=
           UIC.MCDC_State_Inserter;
         --  Likewise for MC/DC state inserter

         procedure Insert (New_Node : Node_Rewriting_Handle);
         --  Insert New_Node in sequence at original location of the degenerate
         --  subprogram.

         procedure To_Regular_Subprogram (N : Base_Subp_Body);
         --  Turns N into an instrumented regular function, by creating a
         --  function with the same subp_spec as the original expression
         --  function or null procedure, and a single return statement with the
         --  original expression, for expression function, or a null statement
         --  for null subprograms.
         --
         --  The SCO associated with the new single statement has the
         --  sloc of the whole original subprogram.

         ------------
         -- Insert --
         ------------

         procedure Insert (New_Node : Node_Rewriting_Handle) is
         begin
            Insert_Child
              (Handle => Saved_Insertion_Info.RH_List,
               Index  => Saved_Insertion_Info.Index
               + Saved_Insertion_Info.Rewriting_Offset
               + 1,
               Child  => New_Node);
            Saved_Insertion_Info.Rewriting_Offset :=
              Saved_Insertion_Info.Rewriting_Offset + 1;
         end Insert;

         ---------------------------
         -- To_Regular_Subprogram --
         ---------------------------

         procedure To_Regular_Subprogram (N : Base_Subp_Body) is

            Single_Stmt_RH : constant Node_Rewriting_Handle :=
              (if N.Kind = Ada_Expr_Function
              then Create_Return_Stmt
                     (Handle        => UIC.Rewriting_Context,
                      F_Return_Expr => Detach (N.As_Expr_Function.F_Expr))
              else Create_Node (Handle => UIC.Rewriting_Context,
                                Kind   => Ada_Null_Stmt));
            Stmt_list_RH   : constant Node_Rewriting_Handle :=
              Create_Regular_Node
                (Handle   => UIC.Rewriting_Context,
                 Kind     => Ada_Stmt_List,
                 Children => (1 => Single_Stmt_RH));
            Stmts_RH       : constant Node_Rewriting_Handle :=
              Create_Handled_Stmts
                (Handle       => UIC.Rewriting_Context,
                 F_Stmts      => Stmt_list_RH,
                 F_Exceptions => No_Node_Rewriting_Handle);
            Proc_Name      : constant Node_Rewriting_Handle :=
              Create_End_Name
                (Handle => UIC.Rewriting_Context,
                 F_Name => Clone (N.F_Subp_Spec.F_Subp_Name.F_Name));
            Decl_List      : constant Node_Rewriting_Handle :=
              Create_Node (Handle => UIC.Rewriting_Context,
                           Kind   => Ada_Decl_List);
            Proc_Decls     : constant Node_Rewriting_Handle :=
                Create_Declarative_Part
                  (Handle  => UIC.Rewriting_Context,
                   F_Decls => Decl_List);

            II : aliased Insertion_Info (Statement);
            --  We need to change the insertion method to a statement insertion
            --  method as we are instrumenting a statement list with a single
            --  statement, and not a list of declarations.

            Local_Inserter : aliased Default_MCDC_State_Inserter;

         begin

            II.RH_List := Stmt_list_RH;
            II.Unsupported := False;
            II.Index := 1;
            II.Rewriting_Offset := 0;
            II.Preelab := False;
            II.Parent := Saved_Insertion_Info;

            UIC.Current_Insertion_Info := II'Unchecked_Access;
            Local_Inserter.Local_Decls := Decl_List;
            UIC.MCDC_State_Inserter := Local_Inserter'Unchecked_Access;

            --  Add witness statement for the single statement

            Extend_Statement_Sequence
              (UIC         => UIC,
               N           => N,
               Typ         => ' ',
               Insertion_N => Single_Stmt_RH);

            --  Process the returned expression for any decisions if we are
            --  dealing with an expression function

            if N.Kind = Ada_Expr_Function then
               Process_Decisions_Defer (N.As_Expr_Function.F_Expr, 'X');
            end if;

            --  Insert the new regular function in place of the old subprogram

            Replace (Handle (N),
                     Create_Subp_Body
                      (Handle       => UIC.Rewriting_Context,
                       F_Overriding => Detach (N.F_Overriding),
                       F_Subp_Spec  => Detach (N.F_Subp_Spec),
                       F_Aspects    => Detach (N.F_Aspects),
                       F_Decls      => Proc_Decls,
                       F_Stmts      => Stmts_RH,
                       F_End_Name   => Proc_Name));

            Set_Statement_Entry;

            UIC.Current_Insertion_Info := Saved_Insertion_Info;
            UIC.MCDC_State_Inserter := Saved_MCDC_State_Inserter;
         end To_Regular_Subprogram;

         Is_Expr_Function : constant Boolean := N.Kind = Ada_Expr_Function;

         Gen_Names_Prefix : constant Wide_Wide_String :=
           To_Wide_Wide_String
             ((if Is_Expr_Function then "Func_Expr" else "Null_Proc")
              & "_"
              & Img (UIC.Degenerate_Subprogram_Index)
              & Part_Tags (UIC.Instrumented_Unit.Part)
              & '_');
         --  Prefix for the name of all entities we create here

         Call_Params : constant Node_Rewriting_Handle :=
           (if Is_Expr_Function
            then Make (UIC, Ada_Assoc_List)
            else No_Node_Rewriting_Handle);
         --  List of formal/actual associations for the call to the augmented
         --  function. Unused if we are not processing an expression function.

         Formal_Params : constant Node_Rewriting_Handle :=
           (if Is_Expr_Function
            then Clone_Params (UIC, N_Spec)
            else No_Node_Rewriting_Handle);
         --  List of formal parameters for the augmented function. Unused if we
         --  are not processing an expression function.

         ------------------------------------
         -- Collection of various nodes... --
         ------------------------------------

         Common_Nodes : constant Degenerate_Subp_Common_Nodes :=
           Create_Degenerate_Subp_Common_Nodes
             (UIC, N, N_Spec, Gen_Names_Prefix);
         --  ... common to all processings in this subprogram

         NP_Nodes : Null_Proc_Nodes;
         --  ... specifically for the instrumentation of null procedures

         ------------------------------------------------------------
         -- Local contexts for statement and MC/DC instrumentation --
         ------------------------------------------------------------

         New_Insertion_Info : aliased Insertion_Info;
         --  Witness insertion info for statements (for both null procedures
         --  and expression functions).

         Unsupported : Boolean := False;
         --  Temporary to compute New_Insertion_Info.Unsupported

         EF_Inserter : aliased Expr_Func_MCDC_State_Inserter :=
           (N_Spec        => N_Spec,
            Call_Params   => Call_Params,
            Formal_Params => Formal_Params);
         --   MC/DC state inserter for this expression function (unused if
         --   instrumenting a null procedure).

         Keep_Aspects : Boolean := True;
         --  Whether to keep the aspects of the expression in the augmented
         --  expression function or not. If we emit a forward declaration for
         --  the expression function, the aspects need to be attached to the
         --  declaration and not the augmented expression function.

      --  Start of processing for Traverse_Degenerate_Subprogram

      begin
         --------------------------
         -- 1. Preparation steps --
         --------------------------

         --  Cannot instrument a (null procedure) primitive of an interface
         --  type, because it must be either abstract or null.

         if not Is_Expr_Function
            and then not Common_Nodes.Ctrl_Type.Is_Null
            and then Common_Nodes.Ctrl_Type.P_Is_Interface_Type
         then
            return;
         end if;

         UIC.Degenerate_Subprogram_Index :=
           UIC.Degenerate_Subprogram_Index + 1;

         if Is_Generic (UIC, N.As_Basic_Decl) then
            if Is_Expr_Function then
               Unsupported := True;
               Report (UIC, N,
                       "gnatcov limitation: "
                       & "cannot instrument generic expression functions."
                       & " Consider turning it into a regular function body.");

            else
               --  As Traverse_Degenerate_Subprogram deals only with expression
               --  functions and null procedures, we are in the case of a
               --  generic null procedure here.

               Unsupported := True;
               Report (UIC, N,
                       "gnatcov limitation:"
                       & " cannot instrument generic null procedures."
                       & " Consider turning it into a regular procedure"
                       & " body.");
            end if;
         end if;

         if Is_Expr_Function then
            if Return_Type_Is_Controlling (UIC, Common_Nodes) then

               --  For the moment when an expression function is a primitive of
               --  a tagged type T, and that T is the return type of the EF,
               --  then introducing an augmented EF also introduces a new
               --  primitive for T, for which the return type is T.
               --
               --  This means that if T has a derived type T2, it will need to
               --  override the original EF (because T2 may have different
               --  components compared to T). The instrumenter generates all
               --  the required functions, but since the names of the augmented
               --  EFs are not necessarly consistent between the augmented EFs
               --  for T and T2, we end up with missing some primitive
               --  overrides.
               --
               --  Since T2 can be defined in a unit (or a project) that will
               --  not be processed by gnatcov, there are cases where
               --  introducing a new primitive for T2 is simply impossible.
               --  So we'll just disable the instrumentation of these
               --  expression functions.
               --
               --  ??? To be investigated, we may get away with turning
               --  the return type of the augmented EF in a class wide type,
               --  so that the augmented EF is no longer a primitive of its
               --  return type. Need to check for potential freezing issues.

               Unsupported := True;
               Report (UIC, N,
                       "gnatcov limitation:"
                       & " cannot instrument an expression function which is"
                       & " a primitive of its return type, when this type is"
                       & " a tagged type. Consider turning it into a regular"
                       & " function body.",
                       Warning);
            elsif Is_Self_Referencing (UIC, N.As_Expr_Function)
                 and then not Common_Nodes.Ctrl_Type.Is_Null
            then
               Unsupported := True;
               Report (UIC, N,
                       "gnatcov limitation:"
                       & " instrumenting a self referencing (i.e. recursive)"
                       & " expression function which is a primitive of some"
                       & " tagged type will move the freezing point of that"
                       & " type if the expression function is not the last"
                       & " primitive to be declared. Consider turning the"
                       & " expression function into a regular function body"
                       & " or moving it to the end of the declarative"
                       & " region.",
                       Warning);
            end if;
         end if;

         --  Protected bodies do not allow declarations, so we cannot
         --  instrument expression functions or null procedures as we usually
         --  do, by adding an augmented subprogram in a package declared right
         --  before.
         --
         --  Since this is only an issue in a protected body, these degenerate
         --  subprograms already have a previous declaration, and we can safely
         --  turn them into regular subprograms, for which we have no
         --  instrumentation issues.

         if not N.P_Semantic_Parent.Is_Null
            and then N.P_Semantic_Parent.Kind = Ada_Protected_Body
         then
            To_Regular_Subprogram (N.As_Base_Subp_Body);
            return;
         end if;

         if Is_Expr_Function then

            --  The statement instrumentation below will take care of assigning
            --  .Witness_* components to their definitive values.

            New_Insertion_Info :=
              (Method         => Expression_Function,
               Unsupported    => Unsupported,
               Witness_Actual => No_Node_Rewriting_Handle,
               Witness_Formal => No_Node_Rewriting_Handle);

            if not New_Insertion_Info.Unsupported then

               --  Pass all expression function parameters to the augmented
               --  expression function call.

               for J in 1 .. (if Common_Nodes.N_Params.Is_Null
                              then 0
                              else Common_Nodes.N_Params.Children_Count)
               loop
                  for Id of Common_Nodes.N_Params.Child (J)
                           .As_Param_Spec.F_Ids.Children
                  loop
                     Append_Child
                       (Call_Params, Make_Identifier (UIC, Id.Text));
                  end loop;
               end loop;
            end if;

         else
            --  Null procedure handling. First create an artificial internal
            --  error, if requested.

            Raise_Stub_Internal_Error_For (Ada_Instrument_Null_Proc);

            --  Create all the nodes for the declaration to generate

            Create_Null_Proc_Nodes (NP_Nodes, UIC, N_Spec, Gen_Names_Prefix);
            Collect_Null_Proc_Formals (Common_Nodes, NP_Nodes, UIC);

            --  Allow witness insertion for the "null" statement in the generic
            --  procedure (NP_Nodes.Null_Stmt).

            New_Insertion_Info :=
              (Method                => Statement,
               Unsupported           => Unsupported,
               RH_List               => NP_Nodes.Stmt_List,
               Index                 => 1,
               Rewriting_Offset      => 0,

               --  Even if the current package has elaboration restrictions,
               --  this Insertion_Info is used to insert a witness call in the
               --  procedure in the generic body: the elaboration restriction
               --  does not apply there.

               Preelab               => False,

               Parent                => null);
         end if;

         ----------------------------------
         -- 2. Statement instrumentation --
         ----------------------------------

         UIC.Current_Insertion_Info := New_Insertion_Info'Unchecked_Access;
         UIC.MCDC_State_Inserter := EF_Inserter'Unchecked_Access;

         --  Output statement SCO for degenerate subprogram body (null
         --  statement or freestanding expression).

         if Is_Expr_Function then
            declare
               N_Expr : constant Expr := N.As_Expr_Function.F_Expr;
            begin
               Extend_Statement_Sequence (UIC, N_Expr, 'X');

               --  For unsupported expression functions, creating a statement
               --  obligation is enough: it will never be satisfied and thus
               --  violations regarding conditions/decisions will not be
               --  displayed, so no need to bother creating them and adding
               --  special cases in decision processings for unsupported
               --  expression functions.

               if not New_Insertion_Info.Unsupported then
                  Process_Decisions_Defer (N_Expr, 'X');
               end if;
            end;
         else
            --  Even though there is a "null" keyword in the null procedure,
            --  is no dedicated node for it in the Libadalang parse tree: use
            --  the whole null procedure declaration to provide a sloc.

            Extend_Statement_Sequence
              (UIC         => UIC,
               N           => N,
               Typ         => 'X',
               Insertion_N => NP_Nodes.Null_Stmt);
         end if;
         Set_Statement_Entry;

         --  Restore saved insertion context

         UIC.MCDC_State_Inserter := Saved_MCDC_State_Inserter;
         UIC.Current_Insertion_Info := Saved_Insertion_Info;

         --  There is nothing else to do if we gave up instrumenting this
         --  subprogram.

         if New_Insertion_Info.Unsupported then
            return;
         end if;

         if Is_Expr_Function then

            --  Pass the witness call to the augmented expression function

            Append_Child (Call_Params, New_Insertion_Info.Witness_Actual);
            Append_Child (Formal_Params, New_Insertion_Info.Witness_Formal);
         end if;

         ----------------------------
         -- 3. Rework declarations --
         ----------------------------

         --  Remove the original declaration (N) from the tree. Note that since
         --  .RH_List (the instrumented list of declarations from which N must
         --  be removed) may contain more elements than before instrumenting.
         --  So in order to remove it, we must recompute N's index in .RH_List.

         Remove_Child
           (Saved_Insertion_Info.RH_List, Index_In_Rewriting_Tree (N));
         Saved_Insertion_Info.Rewriting_Offset :=
           Saved_Insertion_Info.Rewriting_Offset - 1;

         --  For null procedures, if there is no previous declaration, generate
         --  one, keeping the original aspects and default parameters. Then
         --  make sure that the original null procedure is detached from the
         --  tree.
         --
         --  Note that we must not do this for expression functions, as having
         --  both a function declaration and the completing function expression
         --  in the same scope triggers early freezing for controlling types
         --  involved. This means that instrumenting would move the freezing
         --  point, which can produce invalid Ada sources (for instance
         --  primitives cannot be declared after the freezing point, and
         --  primitives could be declared after this expression function).
         --
         --  ... except for self-referencing expression functions (for instance
         --  recursive ones), as the generated code requires the declaration to
         --  be legal Ada.

         if N.As_Base_Subp_Body.P_Previous_Part.Is_Null
            and then (not Is_Expr_Function
                      or else Is_Self_Referencing (UIC, N.As_Expr_Function))
         then
            Insert (Create_Subp_Decl
              (RC,
               F_Overriding => Detach (Common_Nodes.N_Overriding),
               F_Subp_Spec  => Clone (N_Spec),
               F_Aspects    => Detach (N.F_Aspects)));

            --  For expression functions, the aspects of the subprogram were
            --  moved to the newly created declaration, so they should not be
            --  added to the augmented function later on.

            Keep_Aspects := False;
         end if;

         if Is_Expr_Function then
            declare
               Augmented_Expr_Function       : Node_Rewriting_Handle;
               Augmented_Expr_Function_Decl  : Node_Rewriting_Handle;
               New_Expr_Function             : Node_Rewriting_Handle;

            begin
               --  Create the augmented expression function and amend the
               --  original one.

               Create_Augmented_Expr_Function
                 (UIC,
                  Common_Nodes,
                  Formal_Params,
                  Call_Params,
                  Keep_Aspects,
                  Augmented_Expr_Function,
                  Augmented_Expr_Function_Decl,
                  New_Expr_Function);

               --  First comes the augmented expression function, then the new
               --  expression function.

               Insert (Augmented_Expr_Function);
               Insert (New_Expr_Function);

               --  If we need to insert a declaration for the new expression
               --  function, find the correct spot to add it, and keep track
               --  of this insertion if it happens in the same list as the
               --  one currently being instrumented.

               if Augmented_Expr_Function_Decl /= No_Node_Rewriting_Handle then
                  declare
                     Previous_Decl : constant Basic_Decl :=
                       N.P_Previous_Part_For_Decl;
                     --  This property cannot fail because to reach this point
                     --  we will already have succesfully queried the previous
                     --  part of N in Augmented_Expr_Function_Needs_Decl.

                     Insert_List  : constant Node_Rewriting_Handle :=
                       Handle (Previous_Decl.Parent);
                     Insert_Index : constant Positive :=
                       Index_In_Rewriting_Tree (Previous_Decl);
                  begin
                     Insert_Child (Insert_List,
                                   Insert_Index,
                                   Augmented_Expr_Function_Decl);
                     if Insert_List = Saved_Insertion_Info.RH_List then
                        Saved_Insertion_Info.Rewriting_Offset :=
                          Saved_Insertion_Info.Rewriting_Offset + 1;
                     end if;
                  end;
               end if;

            end;

         else
            --  For null procedures...

            declare
               Subp_Body     : Node_Rewriting_Handle;
               Instance      : Node_Rewriting_Handle;
               Renaming_Decl : Node_Rewriting_Handle;
            begin
               --  Create the generic subprogram body, its instantiation and
               --  a renaming for that instatiation.

               Complete_Null_Proc_Decls
                 (UIC,
                  Common_Nodes,
                  NP_Nodes,
                  Subp_Body,
                  Instance,
                  Renaming_Decl);

               --  Insert the renaming in the wrapper package

               Append_Child (Common_Nodes.Wrapper_Pkg_Decls, Instance);

               --  Push the wrapper package and the renaming down to the end of
               --  the current list of declarations.

               Append_Child
                 (Common_Nodes.Append_List, Common_Nodes.Wrapper_Pkg);
               Append_Child (Common_Nodes.Append_List, Renaming_Decl);

               --  Unparse the generic subprogram now, for later insertion in
               --  the pure buffers unit (at which time the rewriting context
               --  will no longer be available).

               UIC.Degenerate_Subprogram_Generics.Append
                 (Generic_Subp'
                    (Generic_Subp_Decl =>
                         To_Unbounded_Wide_Wide_String
                            (Unparse (NP_Nodes.Subp_Decl)),
                     Generic_Subp_Body =>
                       To_Unbounded_Wide_Wide_String (Unparse (Subp_Body))));
            end;
         end if;
      end Traverse_Degenerate_Subprogram;

      --------------------------------
      -- Traverse_Subp_Decl_Or_Stub --
      --------------------------------

      procedure Traverse_Subp_Decl_Or_Stub (N : Basic_Decl) is
         Dummy_Ctx : constant Context_Handle := Create_Context_Instrument (N);

         N_Spec : constant Subp_Spec := N.P_Subp_Spec_Or_Null.As_Subp_Spec;

      begin
         Set_Statement_Entry;

         --  Process decisions nested in formal parameters

         Process_Decisions_Defer (N_Spec.F_Subp_Params, 'X');

         --  Nothing else to do except for the case of degenerate subprograms
         --  (null procedures and expression functions).

         if N.Kind in Ada_Null_Subp_Decl then
            Traverse_Degenerate_Subprogram (N, N_Spec);
         elsif N.Kind in Ada_Expr_Function then
            Enter_Scope
              (UIC        => UIC,
               Scope_Name => N.As_Expr_Function.F_Subp_Spec.F_Subp_Name.Text,
               Sloc       => Sloc (N));
            Traverse_Degenerate_Subprogram (N, N_Spec);
            Exit_Scope (UIC);
         end if;
      end Traverse_Subp_Decl_Or_Stub;

      ------------------
      -- Traverse_One --
      ------------------

      procedure Traverse_One (N : Ada_Node) is
         Dummy_Ctx : constant Context_Handle := Create_Context_Instrument (N);

         Saved_In_Generic : constant Boolean := UIC.In_Generic;
      begin
         --  Initialize or extend current statement sequence. Note that for
         --  special cases such as IF and Case statements we will modify
         --  the range to exclude internal statements that should not be
         --  counted as part of the current statement sequence.

         case N.Kind is
            --  Top of the tree: Compilation unit

            when Ada_Compilation_Unit =>
               declare
                  CUN          : constant Compilation_Unit :=
                    N.As_Compilation_Unit;
                  CUN_Body     : constant Ada_Node := CUN.F_Body;
                  Is_Subunit   : constant Boolean :=
                    CUN_Body.Kind /= Ada_Library_Item;
                  CU_Decl      : constant Basic_Decl :=
                    (if Is_Subunit
                     then Basic_Decl (CUN_Body.As_Subunit.F_Body)
                     else CUN_Body.As_Library_Item.F_Item);
                  CU_Prev_Decl : constant Basic_Decl :=
                    CU_Decl.P_Previous_Part_For_Decl;
               begin
                  --  If we found a subunit, assert that the corresponding
                  --  body/parent subunit is also instrumented.

                  if Is_Subunit then
                     declare
                        Body_Name : constant Ada_Qualified_Name :=
                           Canonicalize
                             (To_Qualified_Name (CUN_Body.As_Subunit.F_Name));
                     begin
                        pragma Assert
                          (IC.Instrumented_Units.Contains
                             (CU_Name_For_Unit (Body_Name, GPR.Unit_Body))
                           or else IC.Instrumented_Units.Contains
                             (CU_Name_For_Unit
                                (Body_Name, GPR.Unit_Separate)));
                     end;

                  --  For a library unit, scan context clause. If this is a
                  --  body, also obtain WITH clauses from the spec. Also
                  --  record implicit WITHs for the unit itself and all of
                  --  its parents.

                  else
                     Traverse_Context_Clause
                       (UIC, CUN.F_Prelude, Process_Pragmas => True);
                     if not CU_Prev_Decl.Is_Null then
                        Traverse_Context_Clause
                          (UIC,
                           CU_Prev_Decl
                           .Unit.Root
                           .As_Compilation_Unit
                           .F_Prelude,
                           Process_Pragmas => False);
                     end if;

                     declare
                        Std : constant Analysis_Unit := N.P_Standard_Unit;
                        AUN : Analysis_Unit := N.Unit;
                     begin
                        while AUN /= Std loop
                           declare
                              Root_Decl : constant Basic_Decl :=
                                AUN.Root.As_Compilation_Unit
                                  .F_Body.As_Library_Item.F_Item;
                           begin
                              UIC.Withed_Units.Include
                                (Root_Decl.P_Canonical_Fully_Qualified_Name);
                              AUN := Root_Decl.P_Parent_Basic_Decl.Unit;
                           end;
                        end loop;
                     end;
                  end if;

                  --  Note: we do not traverse the context clause or generate
                  --  any SCOs for it, as nothing there can generate any code.

                  case CU_Decl.Kind is
                     when Ada_Generic_Instantiation
                        | Ada_Generic_Package_Decl
                        | Ada_Package_Body
                        | Ada_Package_Decl
                        | Ada_Protected_Body
                        | Ada_Subp_Body
                        | Ada_Subp_Decl
                        | Ada_Task_Body
                     =>
                        if CU_Decl.Kind = Ada_Generic_Package_Decl then
                           UIC.In_Generic := True;
                        end if;
                        Traverse_Declarations_Or_Statements
                          (IC, UIC,
                           P       => CU_Decl.As_Ada_Node,
                           L       => CUN.F_Pragmas,
                           Preelab => Preelab);
                        UIC.In_Generic := Saved_In_Generic;

                     --  All other cases of compilation units (e.g. renamings),
                     --  generate no SCO information.

                     when others =>
                        null;
                  end case;

                  --  All instrumented units need to reference the
                  --  corresponding unit that contains coverage buffers.

                  declare
                     Buffers_Unit : constant Node_Rewriting_Handle := To_Nodes
                       (UIC.Rewriting_Context, UIC.Pure_Buffer_Unit.Unit);
                     With_Clause  : constant Node_Rewriting_Handle :=
                        Create_From_Template
                          (UIC.Rewriting_Context, "with {};",
                           (1 => Buffers_Unit), With_Clause_Rule);
                  begin
                     Append_Child (Handle (CUN.F_Prelude), With_Clause);
                  end;
               end;

            --  Package declaration

            when Ada_Package_Decl =>
               Set_Statement_Entry;
               Traverse_Package_Declaration
                 (IC, UIC, N.As_Base_Package_Decl, Preelab);

            --  Generic package declaration

            when Ada_Generic_Package_Decl =>
               UIC.In_Generic := True;
               Set_Statement_Entry;
               Traverse_Generic_Package_Declaration
                 (IC, UIC, N.As_Generic_Package_Decl, Preelab);
               UIC.In_Generic := Saved_In_Generic;

            --  Package body

            when Ada_Package_Body =>
               UIC.In_Generic := Is_Generic (UIC, N.As_Basic_Decl);
               Set_Statement_Entry;
               Traverse_Package_Body (IC, UIC, N.As_Package_Body, Preelab);
               UIC.In_Generic := Saved_In_Generic;

            --  Subprogram declaration or subprogram body stub

            when Ada_Expr_Function
               | Ada_Null_Subp_Decl
               | Ada_Subp_Body_Stub
               | Ada_Subp_Decl
            =>
               Traverse_Subp_Decl_Or_Stub (N.As_Basic_Decl);

            --  Entry declaration

            when Ada_Entry_Decl =>
               Process_Decisions_Defer
                 (As_Entry_Decl (N).F_Spec.F_Entry_Params, 'X');

            --  Generic subprogram declaration

            when Ada_Generic_Subp_Decl =>
               declare
                  GSD : constant Generic_Subp_Decl := As_Generic_Subp_Decl (N);
               begin
                  UIC.In_Generic := True;
                  Process_Decisions_Defer
                    (GSD.F_Formal_Part.F_Decls, 'X');
                  Process_Decisions_Defer
                    (GSD.F_Subp_Decl.F_Subp_Spec.F_Subp_Params, 'X');
                  UIC.In_Generic := Saved_In_Generic;
               end;

            --  Task or subprogram body

            when Ada_Subp_Body
               | Ada_Task_Body
            =>
               if Is_Generic (UIC, N.As_Basic_Decl) then
                  UIC.In_Generic := True;
               end if;
               Set_Statement_Entry;
               Traverse_Subprogram_Or_Task_Body (IC, UIC, N);
               UIC.In_Generic := Saved_In_Generic;

            --  Entry body

            when Ada_Entry_Body =>
               declare
                  Cond : constant Expr := As_Entry_Body (N).F_Barrier;

               begin
                  Set_Statement_Entry;

                  if Switches.Analyze_Entry_Barriers and then not Cond.Is_Null
                  then
                     Process_Decisions_Defer (Cond, 'G');
                  end if;

                  Traverse_Subprogram_Or_Task_Body (IC, UIC, N);
               end;

            --  Protected body

            when Ada_Protected_Body =>
               Set_Statement_Entry;
               Traverse_Declarations_Or_Statements
                 (IC, UIC, L => As_Protected_Body (N).F_Decls.F_Decls);

            --  Exit statement, which is an exit statement in the SCO sense,
            --  so it is included in the current statement sequence, but
            --  then it terminates this sequence. We also have to process
            --  any decisions in the exit statement expression.

            when Ada_Exit_Stmt =>
               Extend_Statement_Sequence (UIC, N, 'E');
               declare
                  Cond : constant Expr := As_Exit_Stmt (N).F_Cond_Expr;
               begin
                  Process_Decisions_Defer (Cond, 'E');
                  Set_Statement_Entry;
               end;

            --  Label, which breaks the current statement sequence, but the
            --  label itself is not included in the next statement sequence,
            --  since it generates no code.

            when Ada_Label =>
               Set_Statement_Entry;

            --  Block statement, which breaks the current statement sequence

            when Ada_Decl_Block | Ada_Begin_Block =>
               Set_Statement_Entry;

               if N.Kind = Ada_Decl_Block then
                  Traverse_Declarations_Or_Statements
                    (IC, UIC, L => As_Decl_Block (N).F_Decls.F_Decls);
               end if;

               Traverse_Handled_Statement_Sequence
                 (IC, UIC,
                  N => (case N.Kind is
                           when Ada_Decl_Block  => As_Decl_Block (N).F_Stmts,
                           when Ada_Begin_Block => As_Begin_Block (N).F_Stmts,
                           when others          => raise Program_Error));

            --  If statement, which breaks the current statement sequence,
            --  but we include the condition in the current sequence.

            when Ada_If_Stmt =>
               Extend_Statement_Sequence (UIC, N, 'I');

               declare
                  If_N : constant If_Stmt := N.As_If_Stmt;
                  Alt  : constant Elsif_Stmt_Part_List := If_N.F_Alternatives;
               begin
                  Process_Decisions_Defer (If_N.F_Cond_Expr, 'I');
                  Set_Statement_Entry;

                  --  Now we traverse the statements in the THEN part

                  Traverse_Declarations_Or_Statements
                    (IC, UIC,
                     L => If_N.F_Then_Stmts.As_Ada_Node_List);

                  --  Loop through ELSIF parts if present

                  for J in 1 .. If_N.F_Alternatives.Children_Count loop
                     declare
                        Elif : constant Elsif_Stmt_Part :=
                          Alt.Child (J).As_Elsif_Stmt_Part;
                     begin
                        --  We generate a statement sequence for the construct
                        --  "ELSIF condition", so that we have a statement for
                        --  the resulting decisions.

                        Extend_Statement_Sequence
                          (UIC,
                           Ada_Node (Elif), 'I',
                           Insertion_N         => Handle (Elif.F_Cond_Expr),
                           Instrument_Location => Inside_Expr);
                        Process_Decisions_Defer (Elif.F_Cond_Expr, 'I');
                        Set_Statement_Entry;

                        --  Traverse the statements in the ELSIF

                        Traverse_Declarations_Or_Statements
                          (IC, UIC,
                           L => Elif.F_Stmts.As_Ada_Node_List);
                     end;
                  end loop;

                  --  Finally traverse the ELSE statements if present

                  Traverse_Declarations_Or_Statements
                    (IC, UIC,
                     L => If_N.F_Else_Stmts.As_Ada_Node_List);
               end;

            --  CASE statement, which breaks the current statement sequence,
            --  but we include the expression in the current sequence.

            when Ada_Case_Stmt =>
               Extend_Statement_Sequence (UIC, N, 'C');
               declare
                  Case_N : constant Case_Stmt := N.As_Case_Stmt;
                  Alt_L  : constant Case_Stmt_Alternative_List :=
                    Case_N.F_Alternatives;
               begin
                  Process_Decisions_Defer (Case_N.F_Expr, 'X');
                  Set_Statement_Entry;

                  --  Process case branches

                  for J in 1 .. Alt_L.Children_Count loop
                     declare
                        Alt : constant Case_Stmt_Alternative :=
                          Alt_L.Child (J).As_Case_Stmt_Alternative;
                     begin
                        Traverse_Declarations_Or_Statements
                          (IC, UIC,
                           L => Alt.F_Stmts.As_Ada_Node_List);
                     end;
                  end loop;
               end;

            --  ACCEPT statement

            when Ada_Accept_Stmt | Ada_Accept_Stmt_With_Stmts =>
               Extend_Statement_Sequence (UIC, N, 'A');
               Set_Statement_Entry;

               if N.Kind = Ada_Accept_Stmt_With_Stmts then
                  --  Process sequence of statements

                  Traverse_Handled_Statement_Sequence
                    (IC, UIC,
                     N => N.As_Accept_Stmt_With_Stmts.F_Stmts);
               end if;

            --  SELECT statement
            --  (all 4 non-terminals: selective_accept, timed_entry_call,
            --  conditional_entry_call, and asynchronous_select).

            when Ada_Select_Stmt =>
               Extend_Statement_Sequence (UIC, N, 'S');
               Set_Statement_Entry;

               declare
                  Sel_N : constant Select_Stmt := As_Select_Stmt (N);
               begin
                  for J in 1 .. Sel_N.F_Guards.Children_Count loop
                     declare
                        Alt : constant Select_When_Part :=
                          Sel_N.F_Guards.Child (J).As_Select_When_Part;
                        Guard : Expr;
                     begin
                        Guard := Alt.F_Cond_Expr;

                        if not Guard.Is_Null then
                           Process_Decisions (UIC, Guard, 'G');
                        end if;

                        --  Traverse the select_alternative,
                        --  entry_call_alternative, or triggering_alternative.

                        Traverse_Declarations_Or_Statements
                          (IC, UIC,
                           L => Alt.F_Stmts.As_Ada_Node_List,
                           Is_Select_Stmt_Alternative => True);
                     end;
                  end loop;

                  --  Note: the sequences of statements for ELSE and ABORT
                  --  do not require the special processing for alternatives.

                  Traverse_Declarations_Or_Statements
                    (IC, UIC,
                     L => Sel_N.F_Else_Stmts.As_Ada_Node_List);
                  Traverse_Declarations_Or_Statements
                    (IC, UIC,
                     L => Sel_N.F_Abort_Stmts.As_Ada_Node_List);
               end;

            --  There is no SCO for a TERMINATE alternative in instrumentation
            --  mode, because there is no place to attach a witness. It would
            --  be dubious anyway, since no code is actually executed if the
            --  alternative is selected.

            when Ada_Terminate_Alternative =>
               null;

            --  Unconditional exit points, which are included in the current
            --  statement sequence, but then terminate it.

            when Ada_Goto_Stmt
               | Ada_Raise_Stmt
               | Ada_Requeue_Stmt
            =>
               Extend_Statement_Sequence (UIC, N, ' ');
               Set_Statement_Entry;

            --  Simple return statement. which is an exit point, but we
            --  have to process the return expression for decisions.

            when Ada_Return_Stmt =>
               Extend_Statement_Sequence (UIC, N, ' ');
               Process_Decisions_Defer
                 (N.As_Return_Stmt.F_Return_Expr, 'X');
               Set_Statement_Entry;

            --  Extended return statement

            when Ada_Extended_Return_Stmt =>
               Extend_Statement_Sequence (UIC, N, 'R');
               declare
                  ER_N : constant Extended_Return_Stmt :=
                    N.As_Extended_Return_Stmt;
               begin
                  Process_Decisions_Defer (ER_N.F_Decl, 'X');
                  Set_Statement_Entry;

                  Traverse_Handled_Statement_Sequence
                    (IC, UIC,
                     N => ER_N.F_Stmts);
               end;

            --  Loop ends the current statement sequence, but we include
            --  the iteration scheme if present in the current sequence.
            --  But the body of the loop starts a new sequence, since it
            --  may not be executed as part of the current sequence.

            when Ada_Base_Loop_Stmt =>
               declare
                  Loop_S : constant Base_Loop_Stmt := N.As_Base_Loop_Stmt;
                  ISC    : constant Loop_Spec := Loop_S.F_Spec;

               begin
                  if not ISC.Is_Null then

                     --  If iteration scheme present, extend the current
                     --  statement sequence to include the iteration scheme
                     --  and process any decisions it contains.

                     --  WHILE loop

                     if ISC.Kind = Ada_While_Loop_Spec then
                        Extend_Statement_Sequence (UIC, N, 'W');
                        Process_Decisions_Defer
                          (ISC.As_While_Loop_Spec.F_Expr, 'W');

                     --  FOR loop

                     else
                        pragma Assert (ISC.Kind = Ada_For_Loop_Spec);

                        --  In Libadalang, there is only one kind of FOR loop:
                        --  both the RM's loop_parameter_specification and
                        --  iterator_specification are materialized with
                        --  For_Loop_Spec nodes. In each case, decisions can
                        --  only appear in the "iteration expression", i.e. the
                        --  expression that comes before the LOOP keyword.

                        Extend_Statement_Sequence (UIC, N, 'F');
                        Process_Decisions_Defer
                          (ISC.As_For_Loop_Spec.F_Iter_Expr, 'X');
                     end if;
                  end if;

                  Set_Statement_Entry;

                  Traverse_Declarations_Or_Statements
                    (IC, UIC,
                     L => Loop_S.F_Stmts.As_Ada_Node_List);
               end;

            --  Pragma

            when Ada_Pragma_Node =>

               --  Processing depends on the kind of pragma

               declare
                  Prag_N    : constant Pragma_Node := N.As_Pragma_Node;
                  Prag_Args : constant Base_Assoc_List := Prag_N.F_Args;
                  Nam       : constant Name_Id := Pragma_Name (Prag_N);
                  Arg       : Positive := 1;
                  Typ       : Character;

                  function Prag_Arg_Expr (Index : Positive) return Expr is
                    (Prag_Args.Child (Index).As_Pragma_Argument_Assoc.F_Expr);
                  --  Return the expression for the Index'th argument of the
                  --  pragma.

               begin
                  case Nam is
                     when Name_Assert
                        | Name_Assert_And_Cut
                        | Name_Assume
                        | Name_Check
                        | Name_Loop_Invariant
                        | Name_Type_Invariant
                        | Name_Postcondition
                        | Name_Precondition
                     =>
                        --  For Assert-like pragmas, we insert a statement
                        --  witness and instrument the decision if the pragma
                        --  is not disabled.
                        --
                        --  This is in line with what is done for pre/post
                        --  aspects.

                        if Nam = Name_Check then

                           --  Skip check name

                           Arg := 2;
                        end if;

                        --  We consider that the assertion policy is
                        --  "disabled".
                        --
                        --  In the compiler, we initially set the type to 'p'
                        --  (disabled pragma), and then switch it to 'P'
                        --  if/when the policy is determined to be enabled
                        --  later on.

                        Typ := 'p';

                        --  Pre/postconditions can be inherited so SCO should
                        --  never be deactivated???

                     when Name_Debug =>
                        if Prag_Args.Children_Count = 2 then

                           --  Case of a dyadic pragma Debug: first argument
                           --  is a P decision, any nested decision in the
                           --  second argument is an X decision.

                           Process_Decisions_Defer (Prag_Arg_Expr (Arg), 'P');
                           Arg := 2;
                        end if;

                        Process_Decisions_Defer (Prag_Arg_Expr (Arg), 'X');

                        --  Note: conservatively assume that the check policy
                        --  for all pragmas is enabled (see comment above for
                        --  Assert case).

                        Typ := 'P';

                     when Name_Annotate =>
                        --  If this is a coverage exemption, record it

                        if Prag_Args.Children_Count >= 2
                           and then As_Symbol (Prag_Arg_Expr (1).As_Identifier)
                                      = As_Symbol (Xcov)
                        then
                           declare
                              Ann_Kind : constant Symbol_Type :=
                                As_Symbol (Prag_Arg_Expr (2).As_Identifier);
                              Ann      : ALI_Annotation;
                           begin
                              Ann.Kind :=
                                ALI_Annotation_Kind'Value (Image (Ann_Kind));
                              Ann.CU := No_CU_Id;

                              if Ann.Kind = Exempt_On
                                 and then Prag_Args.Children_Count >= 3
                                 and then Prag_Arg_Expr (3).Kind
                                            = Ada_String_Literal
                              then
                                 Ann.Message :=
                                   new String'
                                     (To_String (Prag_Arg_Expr (3)
                                                 .As_String_Literal.Text));
                              end if;

                              UIC.Annotations.Append
                                (Annotation_Couple'
                                   ((UIC.SFI, +Sloc (N)), Ann));

                           exception
                              when Constraint_Error =>
                                 --  Invalid annotation kind for Xcov: ignore

                                 null;
                           end;
                        end if;
                        Typ := 'P';

                     --  For all other pragmas, we generate decision entries
                     --  for any embedded expressions, and the pragma is
                     --  never disabled.

                     --  Should generate P decisions (not X) for assertion
                     --  related pragmas: [{Static,Dynamic}_]Predicate???

                     when others =>
                        Process_Decisions_Defer (N, 'X');
                        Typ := 'P';

                  end case;

                  --  Add statement SCO

                  Extend_Statement_Sequence (UIC, N, Typ);
               end;

            --  Object or named number declaration
            --  Generate a single SCO even if multiple defining identifiers
            --  are present.

            when Ada_Number_Decl
               | Ada_Object_Decl
            =>
               Extend_Statement_Sequence (UIC, N, 'o');

               if Has_Decision (UIC, N) then
                  Process_Decisions_Defer (N, 'X');
               end if;

            --  All other cases, which extend the current statement sequence
            --  but do not terminate it, even if they have nested decisions.

            when Ada_Protected_Type_Decl
               | Ada_Task_Type_Decl
            =>
               Extend_Statement_Sequence (UIC, N, 't');
               declare
                  Disc_N : constant Discriminant_Part :=
                    (case N.Kind is
                        when Ada_Protected_Type_Decl =>
                          N.As_Protected_Type_Decl.F_Discriminants,
                        when Ada_Task_Type_Decl      =>
                          N.As_Task_Type_Decl.F_Discriminants,
                        when others                  =>
                           raise Program_Error);
               begin
                  Process_Decisions_Defer (Disc_N, 'X');
               end;
               Set_Statement_Entry;

               Traverse_Sync_Definition (IC, UIC, N);

            when Ada_Single_Protected_Decl
               | Ada_Single_Task_Decl
            =>
               Extend_Statement_Sequence (UIC, N, 'o');
               Set_Statement_Entry;

               Traverse_Sync_Definition (IC, UIC, N);

            when Ada_Named_Stmt =>
               Traverse_One (N.As_Named_Stmt.F_Stmt.As_Ada_Node);

            when others =>

               --  Determine required type character code, or ASCII.NUL if
               --  no SCO should be generated for this node.

               declare
                  Typ : Character;
               begin
                  case N.Kind is
                     when Ada_Base_Type_Decl =>
                        if N.Kind = Ada_Subtype_Decl then
                           Typ := 's';
                        else
                           Typ := 't';
                        end if;

                     --  Entity declaration nodes that may also be used
                     --  for entity renamings.

                     when Ada_Object_Decl | Ada_Exception_Decl =>
                        declare
                           Ren_N : constant Renaming_Clause :=
                             (case N.Kind is
                                 when Ada_Object_Decl    =>
                                   N.As_Object_Decl.F_Renaming_Clause,
                                 when Ada_Exception_Decl =>
                                   N.As_Exception_Decl.F_Renames,
                                 when others             =>
                                    raise Program_Error);
                        begin
                           if not Ren_N.Is_Null then
                              Typ := 'r';
                           else
                              Typ := 'd';
                           end if;
                        end;

                     when Ada_Package_Renaming_Decl   |
                          Ada_Subp_Renaming_Decl      |
                          Ada_Generic_Renaming_Decl   =>
                        Typ := 'r';

                     when Ada_Generic_Instantiation =>
                        Typ := 'i';

                     when Ada_Package_Body_Stub
                        | Ada_Protected_Body_Stub
                        | Ada_Aspect_Clause
                        | Ada_Task_Body_Stub
                        | Ada_Use_Package_Clause
                        | Ada_Use_Type_Clause
                     =>
                        Typ := ASCII.NUL;

                     when others =>
                        if N.Kind in Ada_Stmt then
                           Typ := ' ';
                        else
                           Typ := 'd';
                        end if;
                  end case;

                  if Typ /= ASCII.NUL then
                     Extend_Statement_Sequence (UIC, N, Typ);
                  end if;
               end;

               --  Process any embedded decisions

               if Has_Decision (UIC, N) then
                  Process_Decisions_Defer (N, 'X');
               end if;
         end case;
      end Traverse_One;

      Saved_Insertion_Info : constant Insertion_Info_Access :=
         UIC.Current_Insertion_Info;

      Items_Count : constant Natural :=
        (if L.Is_Null then 0 else L.Children_Count);

   --  Start of processing for Traverse_Declarations_Or_Statements

   begin
      --  Push new insertion info

      UIC.Current_Insertion_Info := Current_Insertion_Info'Unchecked_Access;

      --  Process single prefixed node

      if not P.Is_Null then
         Traverse_One (P);
      end if;

      --  Set up rewriting for lists of declarations/statements

      if not (L.Is_Null or else L.Kind = Ada_Pragma_Node_List) then
         declare
            Method : constant Insertion_Method :=
              (if L.Kind = Ada_Stmt_List
               then Statement
               else Declaration);
            II : Insertion_Info (Method);
         begin
            II.Unsupported := False;
            II.RH_List := Handle (L);
            II.Index := 0;
            II.Rewriting_Offset := 0;
            II.Preelab := Preelab;
            II.Parent := Saved_Insertion_Info;

            if Method = Declaration then
               II.RH_Private_List :=
                 (if Priv_Part.Is_Null
                  then No_Node_Rewriting_Handle
                  else Handle (Priv_Part.F_Decls));
            end if;

            Current_Insertion_Info := II;
         end;
      end if;

      --  Loop through statements or declarations

      for J in 1 .. Items_Count loop
         declare
            N : constant Ada_Node := L.Child (J);
         begin
            if Current_Insertion_Info.Method in Statement | Declaration then
               Current_Insertion_Info.Index := J;
            end if;

            --  Only traverse the nodes if they are not ghost entities

            if not (UIC.Ghost_Code
                    or else
                      (N.Kind in Ada_Stmt and then Safe_Is_Ghost (N.As_Stmt))
                    or else
                      (N.Kind in Ada_Basic_Decl
                       and then Safe_Is_Ghost (N.As_Basic_Decl)))
            then
               Traverse_One (N);
            end if;
         end;
      end loop;

      --  End sequence of statements and flush deferred decisions

      if not P.Is_Null or else Items_Count > 0 then
         Set_Statement_Entry;
      end if;

      --  Pop insertion info

      UIC.Current_Insertion_Info := Saved_Insertion_Info;
   end Traverse_Declarations_Or_Statements;

   -----------------------------
   -- Traverse_Context_Clause --
   -----------------------------

   procedure Traverse_Context_Clause
     (UIC             : in out Ada_Unit_Inst_Context;
      L               : Ada_Node_List;
      Process_Pragmas : Boolean)
   is
      function Withed_Unit_Normalized_Name
        (N : Libadalang.Analysis.Name)
         return Text_Type;
      --  Return the normalized name (see FQN_Sets) of N, a name for a withed
      --  unit.

      ---------------------------------
      -- Withed_Unit_Normalized_Name --
      ---------------------------------

      function Withed_Unit_Normalized_Name
        (N : Libadalang.Analysis.Name)
         return Text_Type
      is
      begin
         case N.Kind is
            when Ada_Base_Id =>
               return Canonicalize (N.Text).Symbol;

            when Ada_Dotted_Name =>
               declare
                  DN : constant Dotted_Name := N.As_Dotted_Name;
               begin
                  return (Withed_Unit_Normalized_Name (DN.F_Prefix) & "."
                          & Withed_Unit_Normalized_Name (DN.F_Suffix.As_Name));
               end;

            when others =>
               raise Program_Error with "unreachable code";
         end case;
      end Withed_Unit_Normalized_Name;

   --  Start of processing for Traverse_Context_Clause

   begin
      for J in 1 .. L.Children_Count loop
         declare
            N : constant Ada_Node := L.Child (J);
         begin
            case N.Kind is
               when Ada_Pragma_Node =>
                  if Process_Pragmas then
                     declare
                        use Ada.Strings.Wide_Wide_Fixed;

                        Pragma_Name : constant Wide_Wide_String :=
                          To_Lower (Text (N.As_Pragma_Node.F_Id));
                     begin
                        if Index (Pragma_Name, "ada_") = Pragma_Name'First then
                           UIC.Language_Version_Pragma :=
                             To_Unbounded_Wide_Wide_String (Pragma_Name);
                        end if;
                     end;
                  end if;

               when Ada_With_Clause =>
                  declare
                     With_N : constant With_Clause := N.As_With_Clause;
                     With_P : constant Libadalang.Analysis.Name_List :=
                       With_N.F_Packages;
                  begin
                     if not With_N.F_Has_Limited then
                        for J in 1 .. With_P.Children_Count loop
                           UIC.Withed_Units.Include
                             (Withed_Unit_Normalized_Name
                                (With_P.Child (J).As_Name));
                        end loop;
                     end if;
                  end;

               when others =>
                  null;
            end case;
         end;
      end loop;

   end Traverse_Context_Clause;

   ------------------------------------------
   -- Traverse_Generic_Package_Declaration --
   ------------------------------------------

   procedure Traverse_Generic_Package_Declaration
     (IC      : in out Inst_Context;
      UIC     : in out Ada_Unit_Inst_Context;
      N       : Generic_Package_Decl;
      Preelab : Boolean)
   is
   begin
      Process_Decisions (UIC, N.F_Formal_Part, 'X');
      Traverse_Package_Declaration
        (IC, UIC, N.F_Package_Decl.As_Base_Package_Decl, Preelab);
   end Traverse_Generic_Package_Declaration;

   -----------------------------------------
   -- Traverse_Handled_Statement_Sequence --
   -----------------------------------------

   procedure Traverse_Handled_Statement_Sequence
     (IC  : in out Inst_Context;
      UIC : in out Ada_Unit_Inst_Context;
      N   : Handled_Stmts)
   is
   begin
      if N.Is_Null then
         return;
      end if;

      Traverse_Declarations_Or_Statements
        (IC, UIC, L => N.F_Stmts.As_Ada_Node_List);

      for J in 1 .. N.F_Exceptions.Children_Count loop
         declare
            Handler : constant Ada_Node := N.F_Exceptions.Child (J);
         begin
            --  Note: the exceptions list can also contain pragmas

            if Handler.Kind = Ada_Exception_Handler then
               Traverse_Declarations_Or_Statements
                 (IC, UIC,
                  L => Handler.As_Exception_Handler.F_Stmts.As_Ada_Node_List);
            end if;
         end;
      end loop;
   end Traverse_Handled_Statement_Sequence;

   ---------------------------
   -- Traverse_Package_Body --
   ---------------------------

   procedure Traverse_Package_Body
     (IC      : in out Inst_Context;
      UIC     : in out Ada_Unit_Inst_Context;
      N       : Package_Body;
      Preelab : Boolean)
   is
      Saved_MCDC_State_Inserter : constant Any_MCDC_State_Inserter :=
        UIC.MCDC_State_Inserter;
      Local_Inserter            : aliased Default_MCDC_State_Inserter :=
        (Local_Decls => Handle (N.F_Decls.F_Decls));
   begin
      UIC.Ghost_Code := Safe_Is_Ghost (N);
      Enter_Scope
        (UIC        => UIC,
         Scope_Name => N.As_Package_Body.F_Package_Name.Text,
         Sloc       => Sloc (N));
      UIC.MCDC_State_Inserter := Local_Inserter'Unchecked_Access;

      Traverse_Declarations_Or_Statements
        (IC, UIC, N.F_Decls.F_Decls, Preelab);
      Traverse_Handled_Statement_Sequence (IC, UIC, N => N.F_Stmts);

      UIC.MCDC_State_Inserter := Saved_MCDC_State_Inserter;
      Exit_Scope (UIC);
      UIC.Ghost_Code := False;
   end Traverse_Package_Body;

   ----------------------------------
   -- Traverse_Package_Declaration --
   ----------------------------------

   procedure Traverse_Package_Declaration
     (IC      : in out Inst_Context;
      UIC     : in out Ada_Unit_Inst_Context;
      N       : Base_Package_Decl;
      Preelab : Boolean)
   is
      Saved_MCDC_State_Inserter : constant Any_MCDC_State_Inserter :=
        UIC.MCDC_State_Inserter;
      Local_Inserter            : aliased Default_MCDC_State_Inserter :=
        (Local_Decls => Handle (N.F_Public_Part.F_Decls));
   begin
      UIC.Ghost_Code := Safe_Is_Ghost (N);
      Enter_Scope
        (UIC        => UIC,
         Scope_Name => N.F_Package_Name.Text,
         Sloc       => Sloc (N));
      UIC.MCDC_State_Inserter := Local_Inserter'Unchecked_Access;

      Traverse_Declarations_Or_Statements
        (IC, UIC, N.F_Public_Part.F_Decls, Preelab,
         Priv_Part => N.F_Private_Part);

      if not N.F_Private_Part.Is_Null then
         Traverse_Declarations_Or_Statements
           (IC, UIC,
            L       => N.F_Private_Part.F_Decls,
            Preelab => Preelab);
      end if;
      UIC.MCDC_State_Inserter := Saved_MCDC_State_Inserter;
      Exit_Scope (UIC);
      UIC.Ghost_Code := False;
   end Traverse_Package_Declaration;

   ------------------------------
   -- Traverse_Sync_Definition --
   ------------------------------

   procedure Traverse_Sync_Definition
     (IC  : in out Inst_Context;
      UIC : in out Ada_Unit_Inst_Context;
      N   : Ada_Node)
   is
      Vis_Decl  : Public_Part := No_Public_Part;
      Priv_Decl : Private_Part := No_Private_Part;
      --  Visible and private declarations of the protected or task definition

   begin
      case N.Kind is
         when Ada_Protected_Type_Decl =>
            declare
               Prot_Def : constant Protected_Def :=
                 N.As_Protected_Type_Decl.F_Definition;
            begin
               Vis_Decl := Prot_Def.F_Public_Part;
               Priv_Decl := Prot_Def.F_Private_Part;
            end;

         when Ada_Single_Protected_Decl =>
            declare
               Prot_Def : constant Protected_Def :=
                 N.As_Single_Protected_Decl.F_Definition;
            begin
               Vis_Decl := Prot_Def.F_Public_Part;
               Priv_Decl := Prot_Def.F_Private_Part;
            end;

         when Ada_Single_Task_Decl =>
            declare
               T_Def : constant Task_Def :=
                 N.As_Single_Task_Decl.F_Task_Type.F_Definition;
            begin
               if not T_Def.Is_Null then
                  Vis_Decl := T_Def.F_Public_Part;
                  Priv_Decl := T_Def.F_Private_Part;
               end if;
            end;

         when Ada_Task_Type_Decl =>
            declare
               T_Def : constant Task_Def :=
                 N.As_Task_Type_Decl.F_Definition;
            begin
               if not T_Def.Is_Null then
                  Vis_Decl := T_Def.F_Public_Part;
                  Priv_Decl := T_Def.F_Private_Part;
               end if;
            end;

         when others =>
            raise Program_Error;
      end case;

      --  Vis_Decl and Priv_Decl may be Empty at least for empty task type
      --  declarations. Querying F_Decls is invalid in this case.

      if not Vis_Decl.Is_Null then
         Traverse_Declarations_Or_Statements
           (IC, UIC, L => Vis_Decl.F_Decls, Priv_Part => Priv_Decl);
      end if;

      if not Priv_Decl.Is_Null then
         Traverse_Declarations_Or_Statements
           (IC, UIC, L => Priv_Decl.F_Decls);
      end if;
   end Traverse_Sync_Definition;

   --------------------------------------
   -- Traverse_Subprogram_Or_Task_Body --
   --------------------------------------

   procedure Traverse_Subprogram_Or_Task_Body
     (IC  : in out Inst_Context;
      UIC : in out Ada_Unit_Inst_Context;
      N   : Ada_Node)
   is
      Decls    : Declarative_Part;
      HSS      : Handled_Stmts;

      Saved_MCDC_State_Inserter : constant Any_MCDC_State_Inserter :=
        UIC.MCDC_State_Inserter;
      Local_Inserter            : aliased Default_MCDC_State_Inserter;

      Body_Name : Defining_Name;
   begin
      case Kind (N) is
         when Ada_Subp_Body =>
            declare
               SBN : constant Subp_Body := N.As_Subp_Body;
            begin
               Decls := SBN.F_Decls;
               HSS   := SBN.F_Stmts;
               Body_Name := N.As_Subp_Body.F_Subp_Spec.F_Subp_Name;
            end;

         when Ada_Task_Body =>
            declare
               TBN : constant Task_Body := N.As_Task_Body;
            begin
               Decls := TBN.F_Decls;
               HSS   := TBN.F_Stmts;
               Body_Name := N.As_Task_Body.F_Name;
            end;

         when Ada_Entry_Body =>
            declare
               EBN : constant Entry_Body := N.As_Entry_Body;
            begin
               Decls := EBN.F_Decls;
               HSS := EBN.F_Stmts;
               Body_Name := N.As_Entry_Body.F_Entry_Name;
            end;

         when others =>
            raise Program_Error;
      end case;

      Enter_Scope
        (UIC        => UIC,
         Scope_Name => Body_Name.Text,
         Sloc       => Sloc (N));

      Local_Inserter.Local_Decls := Handle (Decls.F_Decls);
      UIC.MCDC_State_Inserter := Local_Inserter'Unchecked_Access;

      Traverse_Declarations_Or_Statements (IC, UIC, L => Decls.F_Decls);

      Traverse_Handled_Statement_Sequence (IC, UIC, N => HSS);

      Exit_Scope (UIC);

      --  Restore the MCDC_State_Inserter

      UIC.MCDC_State_Inserter := Saved_MCDC_State_Inserter;
   end Traverse_Subprogram_Or_Task_Body;

   -----------------------
   -- Process_Decisions --
   -----------------------

   procedure Process_Decisions
     (UIC : in out Ada_Unit_Inst_Context;
      N   : Ada_Node'Class;
      T   : Character)
   is
      Mark : Nat;
      --  This is used to mark the location of a decision sequence in the SCO
      --  table. We use it for backing out a simple decision in an expression
      --  context that contains only NOT operators.

      Mark_Hash : Nat;
      --  Likewise for the putative SCO_Raw_Hash_Table entries: see below

      type Hash_Entry is record
         Sloc      : Source_Location;
         SCO_Index : Nat;
      end record;
      --  We must register all conditions/pragmas in SCO_Raw_Hash_Table.
      --  However we cannot register them in the same time we are adding the
      --  corresponding SCO entries to the raw table since we may discard them
      --  later on. So instead we put all putative conditions into Hash_Entries
      --  (see below) and register them once we are sure we keep them.
      --
      --  This data structure holds the conditions/pragmas to register in
      --  SCO_Raw_Hash_Table.

      package Hash_Entries is new Table.Table
        (Table_Component_Type => Hash_Entry,
         Table_Index_Type     => Nat,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 10,
         Table_Name           => "Hash_Entries");
      --  Hold temporarily (i.e. free'd before returning) the Hash_Entry before
      --  they are registered in SCO_Raw_Hash_Table.

      ---------------------------------
      -- Decision-specific variables --
      ---------------------------------

      --  The following variables are related to the current decision being
      --  processed by this call to Process_Decisions. Note that in the case
      --  of nested decisions, this subprogram recurses, so we do not have to
      --  worry about overwriting them.

      Current_Decision : Nat;
      --  Low level SCO id of current decision

      X_Not_Decision : Boolean;
      --  This flag keeps track of whether a decision sequence in the SCO table
      --  contains only NOT operators, and is for an expression context (T=X).
      --  The flag will be set False if T is other than X, or if an operator
      --  other than NOT is in the sequence.

      Condition_Count : Natural := 0;
      --  Count of conditions for current decision (MC/DC only)

      MCDC_State : Unbounded_String;
      --  Name of MC/DC state local variable for current decision (MC/DC only)

      procedure Output_Decision_Operand
        (Operand         : Expr;
         Decision_Static : Boolean);
      --  The node Operand is the top level logical operator of a decision, or
      --  it is one of the operands of a logical operator belonging to a single
      --  complex decision. This (recursive) routine outputs the sequence of
      --  table entries corresponding to the node. Note that we do not process
      --  the sub- operands to look for further decisions, that processing is
      --  done in Find_Nested_Decisions, because we can't get decisions mixed
      --  up in the global table. Call has no effect if Operand is Empty.
      --  Increments Condition_Count (recursively) for each condition.
      --
      --  Decision_Static indicates whether the expression of the whole
      --  decision is static, and should thus not be instrumented.

      procedure Output_Element (N : Ada_Node);
      --  Node N is an operand of a logical operator that is not itself a
      --  logical operator, or it is a simple decision. This routine outputs
      --  the table entry for the element, with C1 set to ' '. Last is set
      --  False, and an entry is made in the condition hash table.

      procedure Output_Header (T : Character; N : Ada_Node'Class);
      --  Outputs a decision header node. T is I/W/E/P for IF/WHILE/EXIT WHEN/
      --  PRAGMA, and 'X' for the expression case. Resets Condition_Count to 0,
      --  and initializes MCDC_State.

      procedure Find_Nested_Decisions (Operand : Expr);
      --  This is called on node Operand, the top level node of a decision,
      --  or on one of its operands or suboperands after generating the full
      --  output for the complex decision. It process the suboperands of the
      --  decision looking for nested decisions.

      function Process_Node (N : Ada_Node'Class) return Visit_Status;
      --  Processes one node in the traversal, looking for logical operators,
      --  and if one is found, outputs the appropriate table entries.

      -----------------------------
      -- Output_Decision_Operand --
      -----------------------------

      procedure Output_Decision_Operand
        (Operand : Expr; Decision_Static : Boolean)
      is
         C1 : Character;
         C2 : Character;
         --  C1 holds a character that identifies the operation while C2
         --  indicates whether we are sure (' ') or not ('?') this operation
         --  belongs to the decision. '?' entries will be filtered out in the
         --  second (SCO_Record_Filtered) pass.

         N : constant Expr := Unwrap (Operand);

         L, R : Expr;

         Op_N  : Op;
         Op_NK : Ada_Node_Kind_Type;

      begin

         --  Logical operator

         if Is_Logical_Operator (UIC, N) then
            Op_N := Operator (N);
            Op_NK := Op_N.Kind;

            if Op_NK = Ada_Op_Not then
               C1 := '!';
               L := No_Expr;
               R := N.As_Un_Op.F_Expr;

            else
               declare
                  BN : constant Bin_Op := N.As_Bin_Op;
               begin
                  L := BN.F_Left;
                  R := BN.F_Right;
                  if Op_NK in Ada_Op_Or | Ada_Op_Or_Else then
                     C1 := '|';
                  else pragma Assert (Op_NK in Ada_Op_And | Ada_Op_And_Then);
                     C1 := '&';
                  end if;
               end;
            end if;

            C2 := ' ';
            Append_SCO
              (C1   => C1,
               C2   => C2,
               From => +Sloc (Op_N),
               To   => Slocs.No_Local_Location,
               Last => False);

            Hash_Entries.Append ((Sloc (N), SCOs.SCO_Table.Last));

            if not L.Is_Null then
               Output_Decision_Operand (L, Decision_Static);
            end if;
            Output_Decision_Operand (R, Decision_Static);

         --  Not a logical operator -> condition

         else
            Output_Element (N.As_Ada_Node);

            if MCDC_Coverage_Enabled then
               UIC.Source_Conditions.Append
                 (Source_Condition'
                    (LL_SCO          => SCOs.SCO_Table.Last,
                     Condition       => N.As_Expr,
                     State           => MCDC_State,
                     First           => Condition_Count = 0,
                     Decision_Static => Decision_Static));

               Condition_Count := Condition_Count + 1;
            end if;
         end if;
      end Output_Decision_Operand;

      --------------------
      -- Output_Element --
      --------------------

      procedure Output_Element (N : Ada_Node) is
         N_SR : constant Source_Location_Range := N.Sloc_Range;
         C2   : Character := 'c';
      begin
         if Is_Static_Expr (N.As_Expr) then

            --  This condition is static: record its value in the SCO

            declare
               Eval : constant String := Bool_Expr_Eval (N.As_Expr);
            begin
               if Eval = "True" then
                  C2 := 't';
               elsif Eval = "False" then
                  C2 := 'f';
               end if;
            end;
         end if;

         Append_SCO
           (C1   => ' ',
            C2   => C2,
            From => +Start_Sloc (N_SR),
            To   => +Inclusive_End_Sloc (N_SR),
            Last => False);
         Hash_Entries.Append ((Start_Sloc (N_SR), SCOs.SCO_Table.Last));
      end Output_Element;

      -------------------
      -- Output_Header --
      -------------------

      procedure Output_Header (T : Character; N : Ada_Node'Class) is
         Loc : Source_Location := No_Source_Location;
         --  Node whose Sloc is used for the decision

         Nam : Name_Id := Namet.No_Name;
         --  For the case of an aspect, aspect name
      begin
         case T is
            when 'I' | 'E' | 'W' | 'a' | 'A' =>

               --  For IF, EXIT, WHILE, or aspects, the token SLOC is that of
               --  the parent of the expression.

               Loc := Sloc (Parent (N));

               if T = 'a' or else T = 'A' then
                  Nam := Aspect_Assoc_Name (N.Parent.As_Aspect_Assoc);
               end if;

            when 'G' =>

               --  For an entry body guard, use the location of the entry body.
               --  For the guard on a select alternative, we do not have access
               --  to the token location for the WHEN, so we use the sloc
               --  of the condition itself.

               declare
                  Par : constant Ada_Node := N.Parent;
               begin
                  if Par.Kind = Ada_Entry_Body then
                     Loc := Sloc (Par);
                  else
                     Loc := Sloc (N);
                  end if;
               end;

            when 'P' =>

               --  For PRAGMA, we must get the location from the pragma node.
               --  Argument N is the pragma argument.

               declare
                  PN : Ada_Node := N.As_Ada_Node;
               begin
                  while PN.Kind /= Ada_Pragma_Node loop
                     PN := PN.Parent;
                  end loop;
                  Loc := Sloc (PN);
               end;

            when 'X' =>

               --  For an expression, we will use the sloc of the first
               --  condition. This is done afterwards, when processing the low
               --  level scos in sc_obligations.adb.
               --
               --  cf. the Update_Decision_Sloc procedure.

               null;

            --  No other possibilities

            when others =>
               raise Program_Error;
         end case;

         Append_SCO
           (C1                 => T,
            C2                 => ' ',
            From               => +Loc,
            To                 => Slocs.No_Local_Location,
            Last               => False,
            Pragma_Aspect_Name => Nam);

         Current_Decision := SCOs.SCO_Table.Last;

         if Coverage.Enabled (Decision)
            or else MCDC_Coverage_Enabled
         then
            if MCDC_Coverage_Enabled then
               Condition_Count := 0;

               if UIC.MCDC_State_Inserter = null then
                  Report (UIC, N,
                          "gnatcov limitation: "
                          & "cannot find local declarative part for MC/DC",
                          Kind => Diagnostics.Error);
               else
                  MCDC_State := To_Unbounded_String
                    (UIC.MCDC_State_Inserter.Insert_MCDC_State
                       (UIC, Make_MCDC_State_Name (SCOs.SCO_Table.Last)));
               end if;
            end if;

            UIC.Source_Decisions.Append
              (Source_Decision'
                 (LL_SCO    => Current_Decision,
                  Decision  => N.As_Expr,
                  State     => MCDC_State,
                  Is_Static => Is_Static_Expr (N.As_Expr)));
         end if;

         --  For an aspect specification, which will be rewritten into a
         --  pragma, enter a hash table entry now.

         if T = 'a' then
            Hash_Entries.Append ((Loc, Current_Decision));
         end if;

      end Output_Header;

      ---------------------------
      -- Find_Nested_Decisions --
      ---------------------------

      procedure Find_Nested_Decisions (Operand : Expr) is
         N : constant Expr := Unwrap (Operand);
      begin
         if Is_Logical_Operator (UIC, N) then
            if N.Kind = Ada_Un_Op then
               Find_Nested_Decisions (N.As_Un_Op.F_Expr);

            else
               Find_Nested_Decisions (N.As_Bin_Op.F_Left);
               Find_Nested_Decisions (N.As_Bin_Op.F_Right);
               X_Not_Decision := False;
            end if;

         else
            Process_Decisions (UIC, N, 'X');
         end if;
      end Find_Nested_Decisions;

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node (N : Ada_Node'Class) return Visit_Status is
         --  Test for the two cases where N is the root node of some decision:

         Decision_Root : constant Boolean :=

           --  Simple decision at outer level: a boolean expression (which is
           --  not a logical operator or short circuit form) appearing as the
           --  operand of an IF, WHILE, EXIT WHEN, or special PRAGMA construct.

           (N = Process_Decisions.N and then T /= 'X')
             or else

           --  Complex decision, whether at outer level or nested: a boolean
           --  expression involving a logical operator.

           (N.Kind in Ada_Expr
            and then Is_Complex_Decision (UIC, N.As_Expr));

      begin
         if Decision_Root then
            declare
               EN : constant Expr := N.As_Expr;
               T  : Character;

            begin
               --  If outer level, then type comes from call, otherwise it
               --  is more deeply nested and counts as X for expression.

               if N = Process_Decisions.N then
                  T := Process_Decisions.T;
               else
                  T := 'X';
               end if;

               --  Output header for sequence

               X_Not_Decision := T = 'X' and then N.Kind = Ada_Op_Not;
               Mark      := SCOs.SCO_Table.Last;
               Mark_Hash := Hash_Entries.Last;
               Output_Header (T, N);

               --  Output the decision (recursively traversing operands)

               Output_Decision_Operand
                 (EN, Is_Static_Expr (N.As_Expr));

               --  If the decision was in an expression context (T = 'X')
               --  and contained only NOT operators, then we don't output
               --  it, so delete the associated SCO entries. As a consequence,
               --  no instrumentation will be emitted.

               if X_Not_Decision then
                  SCOs.SCO_Table.Set_Last (Mark);
                  Hash_Entries.Set_Last (Mark_Hash);

                  --  Otherwise, set Last in last table entry to mark end

               else
                  SCOs.SCO_Table.Table (SCOs.SCO_Table.Last).Last := True;
               end if;

               --  Process any embedded decisions

               Find_Nested_Decisions (EN);
               return Over;
            end;
         end if;

         --  Here for cases that are known to not be logical operators

         case N.Kind is
            --  CASE expression

            --  Really hard to believe this is correct given the special
            --  handling for if expressions below ???

            when Ada_Case_Expr =>
               return Into; -- ???

            --  IF expression: processed like an if statement

            when Ada_If_Expr =>
               declare
                  IEN  : constant If_Expr := N.As_If_Expr;
                  Alt  : constant Elsif_Expr_Part_List := IEN.F_Alternatives;

               begin
                  Process_Decisions (UIC, IEN.F_Cond_Expr, 'I');
                  Process_Decisions (UIC, IEN.F_Then_Expr, 'X');

                  for J in 1 .. Alt.Children_Count loop
                     declare
                        EIN : constant Elsif_Expr_Part :=
                          Alt.Child (J).As_Elsif_Expr_Part;
                     begin
                        Process_Decisions (UIC, EIN.F_Cond_Expr, 'I');
                        Process_Decisions (UIC, EIN.F_Then_Expr, 'X');
                     end;
                  end loop;

                  Process_Decisions (UIC, IEN.F_Else_Expr, 'X');
                  return Over;
               end;

            when Ada_Quantified_Expr =>
               Process_Decisions (UIC, N.As_Quantified_Expr.F_Expr, 'W');
               return Over;

            --  Aspects for which we don't want to instrument the decision

            when Ada_Aspect_Assoc =>
               declare
                  AN : constant Aspect_Assoc := N.As_Aspect_Assoc;
               begin
                  if Aspect_Assoc_Name (AN) in As_Symbol (Dynamic_Predicate)
                    | As_Symbol (Invariant)
                    | As_Symbol (Post)
                    | As_Symbol (Postcondition)
                    | As_Symbol (Pre)
                    | As_Symbol (Precondition)
                    | As_Symbol (Predicate)
                    | As_Symbol (Static_Predicate)
                    | As_Symbol (Type_Invariant)
                  then
                     return Over;
                  end if;
                  return Into;
               end;

            --  All other cases, continue scan

            when others =>
               return Into;
         end case;
      end Process_Node;

   --  Start of processing for Process_Decisions

   begin
      if N.Is_Null then
         return;
      end if;
      Hash_Entries.Init;
      N.Traverse (Process_Node'Access);
      Hash_Entries.Free;
   end Process_Decisions;

   ------------------
   -- Has_Decision --
   ------------------

   function Has_Decision
     (UIC : Ada_Unit_Inst_Context; T : Ada_Node'Class) return Boolean
   is
      function Visit (N : Ada_Node'Class) return Visit_Status;
      --  If N's kind indicates the presence of a decision, return Stop,
      --  otherwise return Into.
      --
      --  We know have a decision as soon as we have a logical operator (by
      --  definition) or an IF-expression (its condition is a decision).

      -----------
      -- Visit --
      -----------

      function Visit (N : Ada_Node'Class) return Visit_Status is
      begin
         if N.Kind in Ada_Expr
            and then (Is_Complex_Decision (UIC, N.As_Expr)
                      or else N.Kind = Ada_If_Expr)
         then
            return Stop;
         else
            return Into;
         end if;
      end Visit;

   --  Start of processing for Has_Decision

   begin
      return T.Traverse (Visit'Access) = Stop;
   end Has_Decision;

   -------------------------
   -- Is_Logical_Operator --
   -------------------------

   function Is_Logical_Operator
     (UIC : Ada_Unit_Inst_Context; N : Ada_Node'Class) return Boolean
   is
   begin
      if N.Kind not in Ada_Expr then
         return False;
      end if;

      declare
         Op_N : constant Op := Operator (N.As_Expr);
      begin
         if Op_N.Is_Null then
            return False;
         end if;

         case Op_N.Kind is
            when Ada_Op_Not =>
               return True;

            when Ada_Op_And_Then | Ada_Op_Or_Else =>
               return True;

            when Ada_Op_And | Ada_Op_Or =>

            --  Only consider Op_N as logical operators if we are told "and"
            --  and "or" have short circuit semantics, and that it is a
            --  Standard.Boolean operator.

               return UIC.Short_Circuit_And_Or
                 and then Is_Standard_Boolean_And_Or (Op_N);

            when others =>
               return False;
         end case;
      end;
   end Is_Logical_Operator;

   -------------------------
   -- Is_Complex_Decision --
   -------------------------

   function Is_Complex_Decision
     (UIC : Ada_Unit_Inst_Context; N : Expr'Class) return Boolean
   is
      Op_N : constant Op := Operator (N);
   begin
      if Op_N.Is_Null then
         return False;
      end if;

      case Op_N.Kind is
         when Ada_Op_Not =>

            --  A "not" operator is the root of a decision iff its operand
            --  itself could be the root of a decision on its own. For
            --  instance, the following is a decision:
            --
            --     not (A and then B)
            --
            --  but not the following:
            --
            --     not A

            return Is_Complex_Decision (UIC, N.As_Un_Op.F_Expr);

         when Ada_Op_And_Then | Ada_Op_Or_Else =>
            return True;

         when Ada_Op_And | Ada_Op_Or =>

            --  Only consider these as decisions if we are told the operators
            --  have short circuit semantics, and that it is a Standard.Boolean
            --  operator.

            return UIC.Short_Circuit_And_Or
              and then Is_Standard_Boolean_And_Or (Op_N);

         when others =>
            return False;
      end case;
   end Is_Complex_Decision;

   --------------------------------
   -- Is_Standard_Boolean_And_Or --
   --------------------------------

   function Is_Standard_Boolean_And_Or (N : Op) return Boolean is
      Binop_N                       : Bin_Op;
      Expr_Typ, Left_Typ, Right_Typ : Base_Type_Decl;

      Std_Bool_Type : constant Type_Decl := N.P_Bool_Type.As_Type_Decl;
   begin
      Binop_N := Parent (N).As_Bin_Op;
      Expr_Typ := Binop_N.As_Expr.P_Expression_Type;
      Left_Typ := Binop_N.F_Left.P_Expression_Type;
      Right_Typ := Binop_N.F_Right.P_Expression_Type;

      return N.P_Referenced_Decl.Is_Null
        and then Expr_Typ /= No_Ada_Node
        and then Left_Typ /= No_Ada_Node
        and then Right_Typ /= No_Ada_Node
        and then Expr_Typ.P_Base_Subtype = Std_Bool_Type
        and then Left_Typ.P_Base_Subtype = Std_Bool_Type
        and then Right_Typ.P_Base_Subtype = Std_Bool_Type;

   exception
      when Property_Error =>
         Report
           (N,
            "Failed to determine if operator is a Standard.Boolean operator");
         return False;
   end Is_Standard_Boolean_And_Or;

   -----------------------
   -- Op_Symbol_To_Name --
   -----------------------

   function Op_Symbol_To_Name
     (Op : Libadalang.Analysis.Name) return Wide_Wide_String
   is
      function Strip_Quotes
        (WWS : Wide_Wide_String) return Wide_Wide_String
      is
        (WWS (WWS'First + 1 .. WWS'Last - 1))
          with Pre => WWS (WWS'First) = '"'
                      and WWS (WWS'Last) = '"';

      Op_Sym : constant Wide_Wide_String :=
        Strip_Quotes (Text (Op));
   begin
      if    Op_Sym = "+"  then
         return "add";
      elsif Op_Sym = "-"  then
         return "sub";
      elsif Op_Sym = "*"  then
         return "mul";
      elsif Op_Sym = "/"  then
         return "div";
      elsif Op_Sym = "**" then
         return "pow";
      elsif Op_Sym = "&"  then
         return "concat";
      elsif Op_Sym = "<"  then
         return "lt";
      elsif Op_Sym = "<=" then
         return "le";
      elsif Op_Sym = ">"  then
         return "gt";
      elsif Op_Sym = ">=" then
         return "ge";
      elsif Op_Sym = "="  then
         return "eq";
      elsif Op_Sym = "/=" then
         return "ne";
      else
         return Op_Sym;
      end if;
   end Op_Symbol_To_Name;

   --------------
   -- Operator --
   --------------

   function Operator (N : Expr'Class) return Op is
   begin
      case N.Kind is
         when Ada_Un_Op =>
            return N.As_Un_Op.F_Op;
         when Ada_Bin_Op =>
            return N.As_Bin_Op.F_Op;
         when others =>
            return No_Op;
      end case;
   end Operator;

   -------------
   -- As_Name --
   -------------

   function As_Name (Id : Identifier) return Name_Id is
   begin
      --  Note: we really care only about Name_Ids for identifiers of pragmas
      --  and aspects, which we assume never contain wide-wide characters.

      return Name_Find (To_String (Canonicalize (Id.Text).Symbol));
   end As_Name;

   -------------
   -- As_Name --
   -------------

   function As_Symbol (Id : Identifier) return Symbol_Type is
     (Find (Symbols, Canonicalize (Id.Text).Symbol));

   -----------------
   -- Pragma_Name --
   -----------------

   function Pragma_Name (P : Pragma_Node) return Symbol_Type is
     (As_Symbol (P.F_Id));
   function Pragma_Name (P : Pragma_Node) return Name_Id is
     (As_Name (P.F_Id));

   -------------------
   -- Safe_Is_Ghost --
   -------------------

   function Safe_Is_Ghost (N : Basic_Decl'Class) return Boolean is
   begin
      return not (for all DN of N.P_Defining_Names => not DN.P_Is_Ghost_Code);
   exception
      when E : Property_Error =>
         Report (N,
                 Msg => "Could not determine if decl is ghost: "
                 & Ada.Exceptions.Exception_Information (E),
                 Kind => Low_Warning);
         return False;

   end Safe_Is_Ghost;

   function Safe_Is_Ghost (N : LAL.Stmt'Class) return Boolean is
   begin
      return N.P_Is_Ghost_Code;
   exception
      when E : Property_Error =>
         Report (N,
                 Msg => "Could not determine if stmt is ghost: "
                 & Ada.Exceptions.Exception_Information (E),
                 Kind => Low_Warning);
         return False;

   end Safe_Is_Ghost;

   -----------------------
   -- Canonically_Equal --
   -----------------------

   function Canonically_Equal (Left, Right : Text_Type) return Boolean is
      Canonical_Left  : constant Symbolization_Result := Canonicalize (Left);
      Canonical_Right : constant Symbolization_Result := Canonicalize (Right);
   begin
      --  If canonicalization failed for one of the two text types, assume they
      --  are different.

      return Canonical_Left.Success
            and then Canonical_Left = Canonical_Right;
   end Canonically_Equal;

   -----------------------
   -- Aspect_Assoc_Name --
   -----------------------

   function Aspect_Assoc_Name (A : Aspect_Assoc) return Identifier is
      AM : constant Libadalang.Analysis.Name := A.F_Id;
      --  aspect_mark of A
   begin
      --  Note: we just ignore a possible 'Class (we treat [Pre|Post]'Class
      --  just like Pre/Post).

      if AM.Kind = Ada_Attribute_Ref then
         return AM.As_Attribute_Ref.F_Prefix.As_Identifier;
      else
         return AM.As_Identifier;
      end if;
   end Aspect_Assoc_Name;

   function Aspect_Assoc_Name (A : Aspect_Assoc) return Symbol_Type is
      (As_Symbol (Aspect_Assoc_Name (A)));
   function Aspect_Assoc_Name (A : Aspect_Assoc) return Name_Id is
      (As_Name (Aspect_Assoc_Name (A)));

   --------------
   -- To_Nodes --
   --------------

   function To_Nodes
     (Handle : Rewriting_Handle;
      Name   : Ada_Qualified_Name) return Node_Rewriting_Handle
   is
      Result : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
   begin
      for Id of Name loop
         declare
            Id_Node : constant Node_Rewriting_Handle := Create_Identifier
              (Handle, To_Text (To_String (Id)));
         begin
            if Result = No_Node_Rewriting_Handle then
               Result := Id_Node;
            else
               Result := Create_Dotted_Name (Handle, Result, Id_Node);
            end if;
         end;
      end loop;
      return Result;
   end To_Nodes;

   ------------
   -- Unwrap --
   ------------

   function Unwrap (N : Expr) return Expr is
      Unwrapped_N : Expr := N;
   begin
      while Unwrapped_N.Kind = Ada_Paren_Expr
      loop
         Unwrapped_N := Unwrapped_N.As_Paren_Expr.F_Expr;
      end loop;

      return Unwrapped_N;
   end Unwrap;

   ------------------------
   -- Inclusive_End_Sloc --
   ------------------------

   function Inclusive_End_Sloc
     (SL : Source_Location_Range) return Source_Location
   is
   begin
      return Result : Source_Location := End_Sloc (SL) do
         pragma Assert (Result.Column > 1);
         Result.Column := Result.Column - 1;
      end return;
   end Inclusive_End_Sloc;

   ---------------------
   -- Expression_Type --
   ---------------------

   function Expression_Type
     (UIC : Ada_Unit_Inst_Context;
      E   : Expr) return Base_Type_Decl
   is
      ET : Base_Type_Decl;
   begin
      begin
         ET := E.P_Expression_Type;

         if ET.Is_Null then
            Report
              (UIC, E,
               "failed to determine expression type (got null type)",
               Warning);
         end if;

      exception
         when Exc : Property_Error =>
            Report
              (UIC, E,
               "failed to determine expression type: "
               & Ada.Exceptions.Exception_Information (Exc),
               Warning);
      end;

      if not ET.Is_Null then
         return ET;
      else
         return E.P_Bool_Type.As_Base_Type_Decl;
      end if;
   end Expression_Type;

   --------------------
   -- Is_Static_Expr --
   --------------------

   function Is_Static_Expr (E : Expr) return Boolean is
   begin
      return E.P_Is_Static_Expr;
   exception
      when Exc : Property_Error =>
         Report
           (E,
            "failed to determine whether this is a static expression: "
            & Ada.Exceptions.Exception_Information (Exc),
            Low_Warning);
         return False;
   end Is_Static_Expr;

   --------------------
   -- Bool_Expr_Eval --
   --------------------

   function Bool_Expr_Eval (E : Expr) return String is
      use Libadalang.Expr_Eval;
   begin
      return To_String (Libadalang.Expr_Eval.Expr_Eval (E).Enum_Result.Text);
   exception
      when Exc : Property_Error =>
         Report
           (E,
            "failed to evaluate the expression: "
            & Ada.Exceptions.Exception_Information (Exc),
            Low_Warning);
         return "";
   end Bool_Expr_Eval;

   --------------
   -- Is_Ghost --
   --------------

   function Is_Ghost
     (UIC : Ada_Unit_Inst_Context;
      EF  : Expr_Function) return Boolean
   is
      Decl : Basic_Decl := EF.As_Basic_Decl;
   begin
      --  We are looking for a Ghost aspect for the given expression function.
      --  If this expression function has a declaration, the aspect must be
      --  there.

      begin
         Decl := Decl.P_Canonical_Part;
      exception
         when Exc : Property_Error =>
            Report
              (UIC, EF,
               "Failed to look for a previous declaration of this expression"
               & " function" & Ada.Exceptions.Exception_Information (Exc),
               Warning);
      end;

      begin
         return Decl.P_Has_Aspect (T_Ghost);
      exception
         when Exc : Property_Error =>
            Report
              (UIC, Decl,
               "Failed to look for a Ghost aspect for this declaration"
               & Ada.Exceptions.Exception_Information (Exc),
               Warning);
            return False;
      end;
   end Is_Ghost;

   ----------------
   -- Is_Generic --
   ----------------

   function Is_Generic
     (UIC  : Ada_Unit_Inst_Context;
      Decl : Basic_Decl) return Boolean
   is
      Canonical_Decl : Basic_Decl;
   begin
      --  Decl is generic iff its canonical part is a generic subprogram
      --  declaration or a generic package declaration.

      begin
         Canonical_Decl := Decl.P_Canonical_Part;
      exception
         when Exc : Property_Error =>
            Report
              (UIC, Decl,
               "Failed to look for a canonical part of this declaration"
               & Ada.Exceptions.Exception_Information (Exc),
               Warning);
      end;

      return Canonical_Decl.Kind in
        Ada_Generic_Subp_Decl | Ada_Generic_Package_Decl;
   end Is_Generic;

   ------------
   -- Detach --
   ------------

   function Detach (N : Ada_Node'Class) return Node_Rewriting_Handle is
   begin
      if N.Is_Null then
         return No_Node_Rewriting_Handle;
      end if;

      return H : constant Node_Rewriting_Handle := Handle (N) do
         Replace (H, No_Node_Rewriting_Handle);
      end return;
   end Detach;

   -----------------
   -- Enter_Scope --
   -----------------

   procedure Enter_Scope
     (UIC        : in out Ada_Unit_Inst_Context;
      Scope_Name : Text_Type;
      Sloc       : Source_Location) is
   begin
      Enter_Scope
        (UIC,
         +Langkit_Support.Text.To_UTF8 (Scope_Name),
         (Line => Natural (Sloc.Line), Column => Natural (Sloc.Column)));
   end Enter_Scope;

   --------------------------
   -- Initialize_Rewriting --
   --------------------------

   procedure Initialize_Rewriting
     (IC                : out Ada_Unit_Inst_Context;
      Instrumented_Unit : Compilation_Unit_Name;
      Context           : Analysis_Context) is
   begin
      IC.Instrumented_Unit := Instrumented_Unit;
      IC.Buffer_Unit :=
        CU_Name_For_Unit (Buffer_Unit (Instrumented_Unit), GPR.Unit_Spec);
      IC.Pure_Buffer_Unit :=
        CU_Name_For_Unit (Pure_Buffer_Unit (Instrumented_Unit), GPR.Unit_Spec);
      IC.Rewriting_Context := Handle (Context);

      declare
         RH : constant Rewriting_Handle := IC.Rewriting_Context;
         E  : Instrumentation_Entities renames IC.Entities;
      begin
         E.Common_Buffers := To_Nodes (RH, Sys_Buffers);
         E.Unit_Buffers := To_Nodes (RH, IC.Pure_Buffer_Unit.Unit);
         E.Statement_Buffer :=
           To_Nodes (RH, IC.Pure_Buffer_Unit.Unit & Statement_Buffer_Name);

         if Coverage.Enabled (Decision) or else MCDC_Coverage_Enabled then
            E.Decision_Buffer :=
              To_Nodes (RH, IC.Pure_Buffer_Unit.Unit & Decision_Buffer_Name);

            if MCDC_Coverage_Enabled then
               E.MCDC_Buffer :=
                 To_Nodes (RH, IC.Pure_Buffer_Unit.Unit & MCDC_Buffer_Name);
            end if;
         end if;
      end;
   end Initialize_Rewriting;

   ----------------------------
   -- Insert_Dump_Proc_Calls --
   ----------------------------

   procedure Insert_Simple_Dump_Proc_Calls
     (RH          : Rewriting_Handle;
      Helper_Unit : Ada_Qualified_Name;
      Subp_Body   : LAL.Subp_Body)
   is
      --  Simply add a call to the buffer dump procedure at the end of
      --  the top level statement list, as well as at the end of the
      --  statement list of each top level exception handler, and
      --  right before each return statment that applies to the main.

      Dump_Procedure : Ada_Qualified_Name;
      --  Name of the procedure to dump coverage buffers

      Call_Stmt : Node_Rewriting_Handle;
      --  Call invoking the dump procedure

      function Process_Returns
         (Node : Ada_Node'Class) return Visit_Status;
      --  Helper for LAL's Traverse procedure. If node is a return
      --  statement that returns from the main, insert a call to the
      --  dump buffer procedure right before.

      ---------------------
      -- Process_Returns --
      ---------------------

      function Process_Returns (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind in Ada_Return_Stmt
           and then Return_From_Subp_Body
                      (Node.As_Return_Stmt, Subp_Body)
         then
            --  Child_Index is 0 based whereas Insert_Child is 1 based

            Insert_Child
              (Handle (Node.Parent),
               Node.Child_Index + 1,
               Clone (Call_Stmt));
            return Over;
         else
            return Into;
         end if;
      end Process_Returns;

      Handled_Stmt_List : constant Node_Rewriting_Handle :=
        Handle (Subp_Body.F_Stmts.F_Stmts);

   --  Start of processing for Insert_Simple_Dump_Proc_Calls

   begin
      Dump_Procedure := Helper_Unit;
      Dump_Procedure.Append (Dump_Procedure_Name);
      Call_Stmt := Create_Regular_Node
        (RH, Ada_Call_Stmt, (1 => To_Nodes (RH, Dump_Procedure)));

      --  Add a Dump_Buffer call at the end of the main's handeled statements

      Append_Child (Handled_Stmt_List, Call_Stmt);

      --  Add a Dump_Buffer call before any return statement that returns from
      --  the main.

      Traverse (Subp_Body, Process_Returns'Access);

      if Is_Null (Subp_Body.F_Stmts.F_Exceptions) then
         return;
      end if;

      --  Add a Dump_Buffer call as the last statement of each exception
      --  handler branch.

      for Node of Subp_Body.F_Stmts.F_Exceptions loop
         if Node.Kind in Ada_Exception_Handler_Range then
            declare
               Exn_Handler   : constant Exception_Handler :=
                 Node.As_Exception_Handler;
               Exn_Stmt_List : constant Node_Rewriting_Handle :=
                 Handle (Exn_Handler.F_Stmts);
            begin
               Append_Child (Exn_Stmt_List, Clone (Call_Stmt));
            end;
         end if;
      end loop;
   end Insert_Simple_Dump_Proc_Calls;

   --------------------------------------
   -- Insert_Controlled_Dump_Proc_Call --
   --------------------------------------

   procedure Insert_Controlled_Dump_Object_Decl
     (RH          : Rewriting_Handle;
      Helper_Unit : Ada_Qualified_Name;
      Subp_Body   : LAL.Subp_Body)
   is
      --  Declare an object of the type <helper unit>.Dump_Controlled_Type
      --  which will call the procedure Dump_Buffers when finalized.

      Controlled_Type_Name : Ada_Qualified_Name := Helper_Unit;
      Dump_Object_Decl     : Node_Rewriting_Handle;
      Decl_List            : constant Node_Rewriting_Handle :=
        Handle (Subp_Body.F_Decls.F_Decls);
   begin
      Controlled_Type_Name.Append
        (To_Unbounded_String ("Dump_Controlled_Type"));
      Dump_Object_Decl := Create_From_Template
        (RH,
         "GNATcov_Dump_Object : {};",
         (1 => To_Nodes (RH, Controlled_Type_Name)),
         Object_Decl_Rule);

      --  Insert the declaration as the first declaration in the
      --  list to ensure it is finalized last.

      Insert_Child (Decl_List, 1, Dump_Object_Decl);
   end Insert_Controlled_Dump_Object_Decl;

   ----------------------------------
   -- Has_Matching_Pragma_For_Unit --
   ----------------------------------

   function Has_Matching_Pragma_For_Unit
     (IC     : Inst_Context;
      Unit   : Compilation_Unit;
      Filter : access function (Node : Pragma_Node) return Boolean)
      return Boolean
   is
      System_Unit  : constant Analysis_Unit :=
        IC.Context.Get_From_Provider ("System", Unit_Specification);
      Unit_Pragmas : constant Pragma_Node_Array :=
        Unit.P_All_Config_Pragmas
        & (if System_Unit.Has_Diagnostics
           then Pragma_Node_Array'(1 .. 0 => No_Pragma_Node)
           else System_Unit.Root.As_Compilation_Unit.P_All_Config_Pragmas);
      --  Configuration pragmas that apply to Unit. Also append the list of
      --  configuration pragmas defined in System as they often define the set
      --  of restrictions associated with the runtime.

   begin
      if not Unusable_System_Reported and then System_Unit.Has_Diagnostics
      then
         Diagnostics.Report
           (Msg  => "Could not parse the System unit for the runtime,"
                    & " instrumentation of mains may be incorrect:",
            Kind => Low_Warning);
         for Diag of System_Unit.Diagnostics loop
            Diagnostics.Report
              (Msg  => System_Unit.Format_GNU_Diagnostic (Diag),
               Kind => Low_Warning);
         end loop;
         Unusable_System_Reported := True;
      end if;
      for Prag_Node of Unit_Pragmas loop
         if Filter.all (Prag_Node) then
            return True;
         end if;
      end loop;
      return False;

   end Has_Matching_Pragma_For_Unit;

   -----------------------------------
   -- Pragma_Restricts_Finalization --
   -----------------------------------

   function Pragma_Restricts_Finalization
     (Prag_Node : Pragma_Node) return Boolean
   is
   begin
      --  We are looking for pragmas of the form:
      --     pragma Restrictions (No_Finalization);
      --  or
      --     pragma Restriction (No_Dependence => Ada.Finalization);

      if Canonically_Equal (Prag_Node.F_Id.Text, "Restrictions") then
         for Assoc of Prag_Node.F_Args loop
            declare
               Prag_Assoc : constant Pragma_Argument_Assoc :=
                 Assoc.As_Pragma_Argument_Assoc;
            begin
               if not Prag_Assoc.F_Name.Is_Null
                 and then
                   (Canonically_Equal
                      (Prag_Assoc.F_Name.Text, "No_Finalization")
                    or else
                      (Canonically_Equal
                         (Prag_Assoc.F_Name.Text, "No_Dependence")
                       and then Canonically_Equal (Prag_Assoc.F_Expr.Text,
                                                   "Ada.Finalization")))
               then
                  return True;
               end if;
            end;
         end loop;
      end if;
      return False;
   end Pragma_Restricts_Finalization;

   -------------------------------------
   -- Finalization_Restricted_In_Unit --
   -------------------------------------

   function Finalization_Restricted_In_Unit
     (IC : Inst_Context; Unit : Compilation_Unit) return Boolean
   is
   begin
      return not Project.Runtime_Supports_Finalization
            or else Has_Matching_Pragma_For_Unit
                      (IC, Unit, Pragma_Restricts_Finalization'Access);
   end Finalization_Restricted_In_Unit;

   --------------------------------------
   -- Pragma_Prevents_Task_Termination --
   --------------------------------------

   function Pragma_Prevents_Task_Termination
     (Prag_Node : Pragma_Node) return Boolean
   is
   begin
      --  We are looking for pragmas of the form:
      --     pragma Restrictions (No_Tasking);
      --  or
      --     pragma Restriction (No_Dependence => Ada.Task_Termination);
      --  or
      --     pragma Restriction (No_Depenence => Ada.Task_Identification);

      if Canonically_Equal (Prag_Node.F_Id.Text, "Restrictions") then
         for Assoc of Prag_Node.F_Args loop
            declare
               Prag_Assoc : constant Pragma_Argument_Assoc :=
                 Assoc.As_Pragma_Argument_Assoc;
            begin
               if not Prag_Assoc.F_Name.Is_Null
                 and then
                   (Canonically_Equal
                      (Prag_Assoc.F_Name.Text, "No_Finalization")
                    or else Canonically_Equal
                      (Prag_Assoc.F_Name.Text, "No_Tasking")
                    or else
                      (Canonically_Equal
                         (Prag_Assoc.F_Name.Text, "No_Dependence")
                       and then
                         (Canonically_Equal (Prag_Assoc.F_Expr.Text,
                                             "Ada.Task_Termination")
                          or else
                            Canonically_Equal (Prag_Assoc.F_Expr.Text,
                                               "Ada.Task_Identification"))))
               then
                  return True;
               end if;
            end;
         end loop;
      end if;
      return False;
   end Pragma_Prevents_Task_Termination;

   ---------------------------------
   -- Task_Termination_Restricted --
   ---------------------------------

   function Task_Termination_Restricted
     (IC : Inst_Context; Unit : Compilation_Unit) return Boolean
   is
   begin
      return not Project.Runtime_Supports_Task_Termination
            or else Has_Matching_Pragma_For_Unit
                      (IC, Unit, Pragma_Prevents_Task_Termination'Access);
   end Task_Termination_Restricted;

   -------------------------------
   -- Auto_Dump_Buffers_In_Main --
   -------------------------------

   procedure Auto_Dump_Buffers_In_Main
     (IC   : Inst_Context;
      Info : in out Project_Info;
      Main : Compilation_Unit_Name;
      URH  : Unit_Rewriting_Handle)
   is
      U   : constant Analysis_Unit := Unit (URH);
      RH  : constant Rewriting_Handle := Handle (U.Context);
      Tmp : LAL.Ada_Node := U.Root;

      CU        : LAL.Compilation_Unit;
      Subp_Body : LAL.Subp_Body;

      Controlled_Types_Available : Boolean;

      Actual_Dump_Trigger : Auto_Dump_Trigger;
      --  Resolved dump trigger after eventual override depending on the
      --  features available on the runtime.

      Instr_Units : constant CU_Name_Vectors.Vector :=
        Instr_Units_For_Closure (IC, Main);
      --  List of instrumented units for which we need to dump buffers

      Helper_Unit : Ada_Qualified_Name;
      --  Name of unit to contain helpers implementing the buffers dump

   begin
      if Instr_Units.Is_Empty then
         return;
      end if;

      --  Make sure this main source has the expected structure: a
      --  simple subprogram body in a compilation unit. If not, return without
      --  doing anything.

      if Tmp.Kind /= Ada_Compilation_Unit then
         return;
      else
         CU := Tmp.As_Compilation_Unit;
      end if;

      Tmp := CU.F_Body;
      if Tmp.Kind /= Ada_Library_Item then
         return;
      end if;

      Tmp := Tmp.As_Library_Item.F_Item.As_Ada_Node;
      if Tmp.Kind /= Ada_Subp_Body then
         return;
      else
         Subp_Body := Tmp.As_Subp_Body;
      end if;

      Controlled_Types_Available :=
        not Finalization_Restricted_In_Unit (IC, CU);

      Actual_Dump_Trigger :=
        (if IC.Dump_Config.Trigger = Main_End
           and then not Controlled_Types_Available
           and then not Task_Termination_Restricted (IC, CU)
         then Ravenscar_Task_Termination
         else IC.Dump_Config.Trigger);

      --  Emit the helper unit and add a WITH clause for it

      Emit_Dump_Helper_Unit
        (IC,
         Info,
         Main,
         Helper_Unit,
         Override_Dump_Trigger => Actual_Dump_Trigger,
         Has_Controlled        => Controlled_Types_Available);

      declare
         Prelude : constant Node_Rewriting_Handle := Handle (CU.F_Prelude);

         With_Clause : constant Node_Rewriting_Handle :=
           Create_From_Template
             (RH,
              Template  => "with {};",
              Arguments => (1 => To_Nodes (RH, Helper_Unit)),
              Rule      => With_Clause_Rule);

         With_RTS_Clause : constant Node_Rewriting_Handle :=
           Create_From_Template
             (RH,
              Template  => "with GNATcov_RTS;",
              Arguments => (1 .. 0 => No_Node_Rewriting_Handle),
              Rule      => With_Clause_Rule);

         Runtime_Version_Check_Node : constant Node_Rewriting_Handle :=
           Create_From_Template
             (RH,
              Template  => To_Wide_Wide_String (Runtime_Version_Check),
              Arguments => (1 .. 0 => No_Node_Rewriting_Handle),
              Rule      => Pragma_Rule);

      begin
         Append_Child (Prelude, With_Clause);
         Append_Child (Prelude, With_RTS_Clause);
         Append_Child (Prelude, Runtime_Version_Check_Node);
      end;

      --  Depending on the chosen coverage buffers dump trigger, insert the
      --  appropriate code.

      case Actual_Dump_Trigger is

      when At_Exit | Ravenscar_Task_Termination =>

         --  Build the call to the registration function and insert it in
         --  the main's declarative part, right before the first declaration.

         declare
            Register_Function : Ada_Qualified_Name;
            --  Name of the function to register the coverage buffers dump
            --  routine.

            Call_Expr : Node_Rewriting_Handle;
            Call_Decl : Node_Rewriting_Handle;
            Decl_List : constant Node_Rewriting_Handle :=
              Handle (Subp_Body.F_Decls.F_Decls);
         begin
            Register_Function := Helper_Unit;
            Register_Function.Append (Register_Dump_Function_Name);
            Call_Expr := To_Nodes (RH, Register_Function);

            Call_Decl := Create_From_Template
              (RH,
               "Autodump_Dummy : {} := {};",
               (1 => To_Nodes (RH, Witness_Dummy_Type_Name), 2 => Call_Expr),
               Object_Decl_Rule);
            Insert_Child (Decl_List, 1, Call_Decl);
         end;

      when Main_End =>

         --  For Main_End we have three instrumentation schemes depending on
         --  the availability of tasks and controlled types:
         --
         --  - If controlled types are available use a controlled object to
         --    dump the buffers as part of its finalization.
         --
         --  - If controlled types are unavailable but task types are, then
         --    use the Ravenscar_Task_Termination dump trigger as we otherwise
         --    have no instrumentation method that works with all code
         --    constructs. In that case, Actuall_Dump_Trigger is set to
         --    Ravenscar_Task_Termination and we should not reach this part of
         --    the code.
         --
         --  - If we do not have finalization or tasks then simply insert calls
         --    right before all the exit points of the main.

         if not Finalization_Restricted_In_Unit (IC, CU) then
            Insert_Controlled_Dump_Object_Decl (RH, Helper_Unit, Subp_Body);
         else
            Insert_Simple_Dump_Proc_Calls (RH, Helper_Unit, Subp_Body);
         end if;
      end case;
   end Auto_Dump_Buffers_In_Main;

   overriding procedure Auto_Dump_Buffers_In_Main
     (Self     : Ada_Instrumenter_Type;
      IC       : in out Inst_Context;
      Main     : Compilation_Unit_Name;
      Filename : String;
      Info     : in out Project_Info)
   is
      Rewriter : Source_Rewriter;
   begin
      Rewriter.Start_Rewriting (IC, Info, Filename);
      Auto_Dump_Buffers_In_Main
        (IC   => IC,
         Info => Info,
         Main => Main,
         URH  => Libadalang.Rewriting.Handle (Rewriter.Rewritten_Unit));
      Rewriter.Apply;
   end Auto_Dump_Buffers_In_Main;

   ----------------------------
   -- Instrument_Source_File --
   ----------------------------

   procedure Instrument_Source_File
     (CU_Name   : Compilation_Unit_Name;
      Unit_Info : Instrumented_Unit_Info;
      Prj_Info  : in out Project_Info;
      IC        : in out Inst_Context;
      UIC       : out Ada_Unit_Inst_Context)
   is
      Rewriter  : Source_Rewriter;
      Filename  : constant String := To_String (Unit_Info.Filename);
      Dummy_Ctx : constant Context_Handle :=
        Create_Context ("Instrumenting " & Filename);

      Root_Analysis_Unit : Analysis_Unit;

      Preelab : Boolean;
      --  Set to True if Unit is required to be preelaborable, i.e.  it is
      --  either preelaborated, or the declaration of a remote types or
      --  remote call interface library unit. In this case, do not generate
      --  any witness calls for elaboration of declarations: they would be
      --  pointless (there is no elaboration code anyway) and, in any case,
      --  illegal.
   begin
      Rewriter.Start_Rewriting (IC, Prj_Info, Filename);

      Root_Analysis_Unit := Rewriter.Rewritten_Unit;

      --  Determine whether Unit is required to be preelaborable, and whether
      --  we can insert witness calls (which are not preelaborable).

      UIC.Root_Unit := Root_Analysis_Unit.Root.As_Compilation_Unit;

      begin
         Preelab := (UIC.Root_Unit.P_Is_Preelaborable
                     or else UIC.Root_Unit.P_Has_Restriction
                       (To_Unbounded_Text ("No_Elaboration_Code")))
           and then UIC.Root_Unit.F_Body.Kind = Ada_Library_Item
           and then UIC.Root_Unit.F_Body.As_Library_Item.F_Item.Kind in
             Ada_Package_Decl
               | Ada_Package_Body
                 | Ada_Generic_Package_Decl;
      exception
         when Libadalang.Common.Property_Error =>
            Report
              (Msg  => "failed to determine preelaboration constraint for "
               & Filename,
               Kind => Warning);
            Preelab := False;
      end;

      Initialize_Rewriting (UIC, CU_Name, IC.Context);

      --  ??? Modify this when libadalang can provide the information about
      --  global/local configuration pragmas for Short_Circuit_And_Or.

      UIC.Short_Circuit_And_Or := Switches.Short_Circuit_And_Or;

      --  Create an artificial internal error, if requested

      Raise_Stub_Internal_Error_For (Ada_Instrument_Start_File);

      --  Make sure that the simple name of the instrumented source file is
      --  registered in our tables. This is required to properly detect when we
      --  try to load SCOs for the same unit from an ALI file, as ALI files
      --  only provide simple names.

      UIC.SFI := Get_Index_From_Generic_Name
        (Filename,
         Kind                => Files_Table.Source_File,
         Indexed_Simple_Name => True);
      UIC.Unit_Bits.SFI := UIC.SFI;

      --  Then run SCOs generation. This inserts calls to witness
      --  procedures/functions in the same pass.

      SCOs.Initialize;
      Traverse_Declarations_Or_Statements
        (IC      => IC,
         UIC     => UIC,
         L       => No_Ada_List,
         Preelab => Preelab,
         P       => Rewriter.Rewritten_Unit.Root);

      SCOs.SCO_Unit_Table.Append
        ((File_Name  => new String'(Filename),
          File_Index => UIC.SFI,
          Dep_Num    => 1,
          From       => SCOs.SCO_Table.First,
          To         => SCOs.SCO_Table.Last));

      --  Convert low level SCOs from the instrumenter to high level SCOs.
      --  This creates BDDs for every decision.

      declare
         SCO_Map       : aliased LL_HL_SCO_Map :=
           (SCOs.SCO_Table.First .. SCOs.SCO_Table.Last => No_SCO_Id);
         Bit_Maps      : CU_Bit_Maps;
         Created_Units : Created_Unit_Maps.Map;
      begin
         Process_Low_Level_SCOs
           (Provider      => SC_Obligations.Instrumenter,
            Origin        => UIC.SFI,
            Created_Units => Created_Units,
            SCO_Map       => SCO_Map'Access,
            Count_Paths   => True);

         --  In the instrumentation case, the origin of SCO information is
         --  the original source file.

         UIC.CU := Created_Units.Element (UIC.SFI);

         --  Import annotations in our internal tables

         UIC.Import_Annotations (Created_Units);

         --  Import non-instrumented SCOs in the internal tables

         Import_Non_Instrumented_LL_SCOs (UIC, SCO_Map);

         --  Insert calls to condition/decision witnesses

         if Coverage.Enabled (Decision) or else MCDC_Coverage_Enabled then
            for SD of UIC.Source_Decisions loop

               --  Instrumenting a static decision would make it non-static by
               --  wrapping it in a Witness call. This transformation would
               --  trigger legality checks on the originally non-evaluated
               --  branch, which could result in compilation errors specific to
               --  the instrumented code, e.g. on:
               --
               --   X := (if <config.static-False>
               --         then <out-of-range-static>
               --         else <value>);
               --
               --  Mark these decisions as non-instrumented so they are
               --  properly reported.

               if not SD.Is_Static then
                  Insert_Decision_Witness
                    (UIC, SD, Path_Count (SCO_Map (SD.LL_SCO)));
               else
                  Set_Decision_SCO_Non_Instr (SCO_Map (SD.LL_SCO));
               end if;
            end loop;

            if MCDC_Coverage_Enabled then

               --  As high-level SCO tables have been populated, we have built
               --  BDDs for each decisions, and we can now set the correct
               --  MC/DC path offset for each condition.
               --
               --  We do not include a witness call for conditions which appear
               --  in a decision with a path count exceeding the limit to avoid
               --  generating overly large traces / run out of memory.
               --
               --  We also do not include witness calls for conditions of
               --  static decision, as this would make the instrumented
               --  expression non-static. Mark the enclosing decision as not
               --  instrumented for MCDC instead.
               --
               --  As we go through each condition, mark their enclosing
               --  decision as not instrumented if their number of paths
               --  exceeds our limit.

               for SC of UIC.Source_Conditions loop
                  declare
                     Condition : constant SCO_Id := SCO_Map (SC.LL_SCO);
                     Decision  : constant SCO_Id :=
                       Enclosing_Decision (Condition);
                  begin
                     if Path_Count (Decision) /= 0
                        and then not SC.Decision_Static
                     then
                        Insert_Condition_Witness
                          (UIC, SC, Offset_For_True (Condition));
                     else
                        Set_Decision_SCO_Non_Instr_For_MCDC (Decision);
                     end if;
                  end;
               end loop;
            end if;
         end if;

         --  Witnesses have now been inserted, and bit indices allocated: build
         --  bit maps.

         Bit_Maps :=
           (Statement_Bits => new Statement_Bit_Map'
              (Bit_Id'First .. UIC.Unit_Bits.Last_Statement_Bit => No_SCO_Id),
            Decision_Bits  => new Decision_Bit_Map'
              (Bit_Id'First .. UIC.Unit_Bits.Last_Outcome_Bit =>
                   (No_SCO_Id, False)),
            MCDC_Bits      =>
               new MCDC_Bit_Map'(Bit_Id'First .. UIC.Unit_Bits.Last_Path_Bit =>
                                     (No_SCO_Id, 0)));

         for S_Bit_Alloc of UIC.Unit_Bits.Statement_Bits loop
            Bit_Maps.Statement_Bits (S_Bit_Alloc.Executed) :=
              SCO_Map (S_Bit_Alloc.LL_S_SCO);
         end loop;

         for D_Bit_Alloc of UIC.Unit_Bits.Decision_Bits loop
            declare
               D_SCO : constant SCO_Id := SCO_Map (D_Bit_Alloc.LL_D_SCO);
            begin
               for Outcome in Boolean loop
                  Bit_Maps.Decision_Bits
                    (D_Bit_Alloc.Outcome_Bits (Outcome)) :=
                      (D_SCO, Outcome);
               end loop;

               if MCDC_Coverage_Enabled
                  and then D_Bit_Alloc.Path_Bits_Base /= No_Bit_Id
               then
                  declare
                     Path_Count : constant Natural :=
                       SC_Obligations.Path_Count (D_SCO);
                  begin
                     for J in 1 .. Any_Bit_Id (Path_Count) loop
                        Bit_Maps.MCDC_Bits
                          (D_Bit_Alloc.Path_Bits_Base + J - 1) :=
                          (D_SCO, Natural (J - 1));
                     end loop;
                  end;
               end if;
            end;
         end loop;

         Set_Bit_Maps (UIC.CU, Bit_Maps);

          --  Remap low level SCOS in Scope_Entity records to high-level SCOs.
          --  See the comment for Scope_Entity.From/To.

         if UIC.Current_Scope_Entity /= null then
            declare
               use Scope_Entities_Vectors;

               procedure Remap_SCOs (Scope_Entity : Scope_Entity_Acc);

               ----------------
               -- Remap_SCOs --
               ----------------

               procedure Remap_SCOs (Scope_Entity : Scope_Entity_Acc) is
               begin
                  Scope_Entity.From := SCO_Map (Nat (Scope_Entity.From));
                  Scope_Entity.To := SCO_Map (Nat (Scope_Entity.To));
                  for Child of Scope_Entity.Children loop
                     Remap_SCOs (Child);
                  end loop;
               end Remap_SCOs;

            begin
               --  Iterate through the package level body entities

               Remap_SCOs (UIC.Current_Scope_Entity);
               Set_Scope_Entity (UIC.CU, UIC.Current_Scope_Entity);
            end;
         end if;
      end;

      --  Insert automatic buffer dump calls, if requested

      if IC.Dump_Config.Trigger /= Manual and then Unit_Info.Is_Main then
         Auto_Dump_Buffers_In_Main
           (IC   => IC,
            Info => Prj_Info,
            Main => UIC.Instrumented_Unit,
            URH  => Handle (Rewriter.Rewritten_Unit));
      end if;

      --  Emit the instrumented source file

      Rewriter.Apply;
   end Instrument_Source_File;

   -----------------------
   -- Buffers_List_Unit --
   -----------------------

   function Buffers_List_Unit (IC : Inst_Context) return Ada_Qualified_Name
   is
      Project_Name_Slug : constant String :=
        Qualified_Name_Slug (To_Qualified_Name (IC.Project_Name));
   begin
      return Ada_Identifier_Vectors."&"
        (Sys_Buffers_Lists,
         Instrument.Base_Types.Ada_Identifier (+Project_Name_Slug));
   end Buffers_List_Unit;

   ----------------------
   -- Pure_Buffer_Unit --
   ----------------------

   function Pure_Buffer_Unit
     (Instrumented_Unit : Compilation_Unit_Name) return Ada_Qualified_Name
   is
      Simple_Name : Instrument.Base_Types.Ada_Identifier;
   begin
      Append (Simple_Name, 'P');
      Append (Simple_Name, Instrumented_Unit_Slug (Instrumented_Unit));
      return CU_Name : Ada_Qualified_Name := Sys_Buffers do
         CU_Name.Append (Simple_Name);
      end return;
   end Pure_Buffer_Unit;

   ----------------------
   -- Emit_Buffer_Unit --
   ----------------------

   procedure Emit_Buffer_Unit
     (Info : in out Project_Info; UIC : Ada_Unit_Inst_Context'Class)
   is
      CU_Name : Compilation_Unit_Name renames UIC.Buffer_Unit;
      File    : Text_Files.File_Type;
   begin
      Create_File (Info,
                   File,
                   To_Filename (Info.Project, CU_Name, Switches.Ada_Language));
      Put_Warnings_And_Style_Checks_Pragmas (File);
      File.Put_Line ("with Interfaces.C; use Interfaces.C;");
      File.Put_Line
        ("with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;");

      declare
         Pkg_Name : constant String := To_Ada (CU_Name.Unit);
         --  Package name for the buffer unit

         Fingerprint : Unbounded_String;
         --  Fingerprint for the instrumented unit

         Unit_Name : constant String := Ada.Characters.Handling.To_Lower
           (To_Ada (UIC.Instrumented_Unit.Unit));
         --  Lower-case name for the instrumented unit

         Unit_Part : constant String :=
           (case UIC.Instrumented_Unit.Part is
               when GPR.Unit_Spec     => "Unit_Spec",
               when GPR.Unit_Body     => "Unit_Body",
               when GPR.Unit_Separate => "Unit_Separate");
         --  Do not use 'Image so that we use the original casing for the
         --  enumerators, and thus avoid compilation warnings/errors.

         Statement_Last_Bit : constant String := Img
           (UIC.Unit_Bits.Last_Statement_Bit);
         Decision_Last_Bit  : constant String := Img
           (UIC.Unit_Bits.Last_Outcome_Bit);
         MCDC_Last_Bit      : constant String := Img
           (UIC.Unit_Bits.Last_Path_Bit);

      begin
         --  Turn the fingerprint value into the corresponding Ada literal

         declare
            First : Boolean := True;
         begin
            Append (Fingerprint, "(");
            for Byte of SC_Obligations.Fingerprint (UIC.CU) loop
               if First then
                  First := False;
               else
                  Append (Fingerprint, ", ");
               end if;
               Append (Fingerprint, Strings.Img (Integer (Byte)));
            end loop;
            Append (Fingerprint, ")");
         end;

         File.Put_Line ("package " & Pkg_Name & " is");
         File.New_Line;
         File.Put_Line ("   pragma Preelaborate;");
         File.New_Line;

         --  Create declarations for individual buffers (statement, decision
         --  and MC/DC) as well as their exported addresses.

         File.Put_Line ("   Statement_Buffer : Coverage_Buffer_Type"
                        & " (0 .. " & Statement_Last_Bit & ") :="
                        & " (others => False);");
         File.Put_Line ("   Statement_Buffer_Address : constant System.Address"
                        & " := Statement_Buffer'Address;");
         File.Put_Line ("   pragma Export (C, Statement_Buffer_Address, """
                        & Statement_Buffer_Symbol (UIC.Instrumented_Unit)
                        & """);");
         File.New_Line;

         File.Put_Line ("   Decision_Buffer : Coverage_Buffer_Type"
                        & " (0 .. " & Decision_Last_Bit & ") :="
                        & " (others => False);");
         File.Put_Line ("   Decision_Buffer_Address : constant System.Address"
                        & " := Decision_Buffer'Address;");
         File.Put_Line ("   pragma Export (C, Decision_Buffer_Address, """
                        & Decision_Buffer_Symbol (UIC.Instrumented_Unit)
                        & """);");
         File.New_Line;

         File.Put_Line ("   MCDC_Buffer : Coverage_Buffer_Type"
                        & " (0 .. " & MCDC_Last_Bit & ") :="
                        & " (others => False);");
         File.Put_Line ("   MCDC_Buffer_Address : constant System.Address"
                        & " := MCDC_Buffer'Address;");
         File.Put_Line ("   pragma Export (C, MCDC_Buffer_Address, """
                        & MCDC_Buffer_Symbol (UIC.Instrumented_Unit)
                        & """);");
         File.New_Line;

         --  Create the GNATcov_RTS_Coverage_Buffers record

         File.Put_Line ("   Unit_Name : constant String := """ & Unit_Name
                        & """;");
         File.Put_Line ("   Project_Name : constant String := """";");
         File.New_Line;

         File.Put_Line ("   Buffers : aliased constant"
                        & " GNATcov_RTS_Coverage_Buffers :=");
         File.Put_Line ("     (Fingerprint => "
                        & To_String (Fingerprint) & ",");

         File.Put_Line ("      Language  => Unit_Based_Language,");
         File.Put_Line ("      Unit_Part => " & Unit_Part & ",");
         File.Put_Line ("      Unit_Name =>"
                        & " (Unit_Name'Address, Unit_Name'Length),");

         File.Put_Line ("      Project_Name =>"
                        & " (Project_Name'Address, Project_Name'Length),");

         File.Put_Line ("      Statement => Statement_Buffer'Address,");
         File.Put_Line ("      Decision  => Decision_Buffer'Address,");
         File.Put_Line ("      MCDC      => MCDC_Buffer'Address,");

         File.Put_Line ("      Statement_Last_Bit => " & Statement_Last_Bit
                        & ",");
         File.Put_Line ("      Decision_Last_Bit => " & Decision_Last_Bit
                        & ",");
         File.Put_Line ("      MCDC_Last_Bit => " & MCDC_Last_Bit & ");");
         File.New_Line;

         --  Create the buffers group

         File.Put_Line
           ("   Buffers_Group : aliased constant Coverage_Buffers_Group :="
            & " (1 => Buffers'Access);");
         File.Put_Line
           ("   C_Buffers_Group : aliased constant"
            & " GNATcov_RTS_Coverage_Buffers_Group :="
            & " (1, Buffers_Group'Address);");
         File.Put_Line ("      pragma Export (C, C_Buffers_Group, """
                        & Unit_Buffers_Name (UIC.Instrumented_Unit) & """);");
         File.New_Line;

         File.Put_Line ("end " & Pkg_Name & ";");
      end;
   end Emit_Buffer_Unit;

   ---------------------------
   -- Emit_Pure_Buffer_Unit --
   ---------------------------

   procedure Emit_Pure_Buffer_Unit
     (Info : in out Project_Info; UIC : Ada_Unit_Inst_Context'Class)
   is

      CU_Name  : Compilation_Unit_Name := UIC.Pure_Buffer_Unit;
      Pkg_Name : constant String := To_Ada (CU_Name.Unit);
      File     : Text_Files.File_Type;

      procedure Put_Language_Version_Pragma;
      --  If the instrumented unit has a language version configuration
      --  pragma, insert a consistent one here to ensure legality of
      --  degenerate subprograms supporting generics.

      ---------------------------------
      -- Put_Language_Version_Pragma --
      ---------------------------------

      procedure Put_Language_Version_Pragma is
      begin
         if Length (UIC.Language_Version_Pragma) > 0 then
            File.Put_Line
              ("pragma "
               & To_String (To_Wide_Wide_String (UIC.Language_Version_Pragma))
               & ";");
            File.New_Line;
         end if;
      end Put_Language_Version_Pragma;

   --  Start of processing for Emit_Pure_Buffer_Unit

   begin
      Create_File (Info,
                   File,
                   To_Filename (Info.Project, CU_Name, Switches.Ada_Language));

      Put_Warnings_And_Style_Checks_Pragmas (File);
      Put_Language_Version_Pragma;
      File.Put_Line ("with System;");

      File.Put_Line ("with GNATcov_RTS;");
      File.Put_Line (Runtime_Version_Check);

      File.New_Line;
      File.Put_Line ("package " & Pkg_Name & " is");
      File.New_Line;
      File.Put_Line ("   pragma Pure;");
      File.New_Line;
      File.Put_Line ("   Statement_Buffer : constant System.Address;");
      File.Put_Line ("   pragma Import (C, Statement_Buffer, """
                     & Statement_Buffer_Symbol (UIC.Instrumented_Unit)
                     & """);");
      File.New_Line;
      File.Put_Line ("   Decision_Buffer : constant System.Address;");
      File.Put_Line ("   pragma Import (C, Decision_Buffer, """
                     & Decision_Buffer_Symbol (UIC.Instrumented_Unit)
                     & """);");
      File.New_Line;
      File.Put_Line ("   MCDC_Buffer : constant System.Address;");
      File.Put_Line ("   pragma Import (C, MCDC_Buffer, """
                     & MCDC_Buffer_Symbol (UIC.Instrumented_Unit)
                     & """);");
      File.New_Line;

      for G of UIC.Degenerate_Subprogram_Generics loop
         File.Put_Line
           ("   " & To_String (To_Wide_Wide_String (G.Generic_Subp_Decl)));
      end loop;
      File.Put_Line ("end " & Pkg_Name & ";");

      Text_Files.Close (File);
      if Switches.Pretty_Print then
         Text_Files.Run_GNATpp (File);
      end if;

      if not UIC.Degenerate_Subprogram_Generics.Is_Empty then
         CU_Name.Part := GNATCOLL.Projects.Unit_Body;

         Create_File
           (Info,
            File,
            To_Filename (Info.Project, CU_Name, Switches.Ada_Language));

         Put_Warnings_And_Style_Checks_Pragmas (File);
         Put_Language_Version_Pragma;
         File.Put_Line ("package body " & Pkg_Name & " is");
         File.New_Line;
         for G of UIC.Degenerate_Subprogram_Generics loop
            File.Put_Line
              ("   " & To_String (To_Wide_Wide_String (G.Generic_Subp_Body)));
         end loop;
         File.Put_Line ("end " & Pkg_Name & ";");

         Text_Files.Close (File);
         if Switches.Pretty_Print then
            Text_Files.Run_GNATpp (File);
         end if;
      end if;
   end Emit_Pure_Buffer_Unit;

   ---------------------------
   -- Emit_Dump_Helper_Unit --
   ---------------------------

   procedure Emit_Dump_Helper_Unit
     (IC                    : Inst_Context;
      Info                  : in out Project_Info;
      Main                  : Compilation_Unit_Name;
      Helper_Unit           : out Ada_Qualified_Name;
      Override_Dump_Trigger : Any_Dump_Trigger := Manual;
      Has_Controlled        : Boolean := False)
   is
      File : Text_Files.File_Type;

      procedure Put_With (Unit : Ada_Qualified_Name);
      --  Put a "with" context clause in File

      --------------
      -- Put_With --
      --------------

      procedure Put_With (Unit : Ada_Qualified_Name) is
      begin
         File.Put_Line ("with " & To_Ada (Unit) & ";");
      end Put_With;

      Output_Unit, Output_Proc : Ada_Qualified_Name;
      --  Qualified names for the unit that contains the buffer output
      --  procedure, and for the procedure itself.

      Dump_Trigger : constant Auto_Dump_Trigger :=
        (if Override_Dump_Trigger = Manual
         then IC.Dump_Config.Trigger
         else Override_Dump_Trigger);
      --  Resolved dump trigger

   --  Start of processing for Emit_Dump_Helper_Unit

   begin
      --  Create the name of the helper unit

      Helper_Unit := Sys_Buffers;
      Helper_Unit.Append
        (To_Unbounded_String ("D") & Instrumented_Unit_Slug (Main));

      --  Compute the qualified names we need for instrumentation

      declare
         use type Ada_Qualified_Name;
         Unit : constant String :=
           (case IC.Dump_Config.Channel is
               when Binary_File            => "Files",
               when Base64_Standard_Output => "Base64");
      begin
         Output_Unit.Append (To_Unbounded_String ("GNATcov_RTS"));
         Output_Unit.Append (To_Unbounded_String ("Traces"));
         Output_Unit.Append (To_Unbounded_String ("Output." & Unit));

         Output_Proc := Output_Unit
           & To_Unbounded_String ("Write_Trace_File_Wrapper");
      end;

      declare
         Helper_Unit_Name : constant String := To_Ada (Helper_Unit);
         Dump_Procedure   : constant String := To_String (Dump_Procedure_Name);
      begin
         --  Emit the package spec. This includes one Dump_Buffers procedure,
         --  which dumps all coverage buffers in Main's closure to the source
         --  trace file.

         Create_File
           (Info,
            File,
            Name =>
              To_Filename
                (Info.Project,
                 CU_Name_For_Unit (Helper_Unit, GNATCOLL.Projects.Unit_Spec),
                 Switches.Ada_Language));

         Put_Warnings_And_Style_Checks_Pragmas (File);
         Put_With (Sys_Buffers);
         if Dump_Trigger = Main_End and then Has_Controlled then
            File.Put_Line ("with Ada.Finalization;");
         end if;
         File.Put_Line ("package " & Helper_Unit_Name & " is");
         File.New_Line;
         File.Put_Line ("   procedure " & Dump_Procedure & ";");
         File.Put_Line ("   pragma Convention (C, " & Dump_Procedure & ");");
         File.New_Line;

         case Dump_Trigger is
            when At_Exit | Ravenscar_Task_Termination =>
               File.Put_Line
                 ("function "
                  & To_String (Register_Dump_Function_Name) & " return "
                  & To_Ada (Witness_Dummy_Type_Name) & ";");
               File.New_Line;

            when Main_End =>
               if Has_Controlled then
                  File.Put_Line ("   type Dump_Controlled_Type is new"
                                 & " Ada.Finalization.Controlled with");
                  File.Put_Line ("     null record;");
                  File.Put_Line ("   overriding procedure Finalize (Self : in"
                                 & " out Dump_Controlled_Type);");
                  File.New_Line;
               end if;
         end case;

         File.Put_Line ("end " & Helper_Unit_Name & ";");
         File.Close;

         --  Emit the package body

         Create_File
           (Info,
            File,
            Name =>
              To_Filename
                (Info.Project,
                 CU_Name_For_Unit (Helper_Unit, GNATCOLL.Projects.Unit_Body),
                 Switches.Ada_Language));

         Put_Warnings_And_Style_Checks_Pragmas (File);

         Put_With (Output_Unit);
         File.Put_Line ("with Interfaces.C;");

         case Dump_Trigger is
            when Ravenscar_Task_Termination  =>
               File.Put_Line ("with Ada.Task_Identification;");
               File.Put_Line ("with Ada.Task_Termination;");
            when others =>
               null;
         end case;

         File.Put_Line ("with " & To_Ada (Buffers_List_Unit (IC)) & ";");

         File.Put_Line ("package body " & Helper_Unit_Name & " is");
         File.New_Line;

         --  Emit the procedure to write the trace file

         File.Put_Line ("   procedure " & Dump_Procedure & " is");

         File.Put_Line ("   begin");

         File.Put_Line ("      " & To_Ada (Output_Proc));
         File.Put      ("        (");

         File.Put_Line (To_Ada (Buffers_List_Unit (IC)) & ".List,");
         case IC.Dump_Config.Channel is
         when Binary_File =>
            declare
               use GNATCOLL.VFS;

               U       : constant String := To_Ada (Output_Unit);
               Indent1 : constant String := "         ";
               Indent2 : constant String := Indent1 & "  ";

               Env_Var : constant String :=
                 (if Length (IC.Dump_Config.Filename_Env_Var) = 0
                  then U & ".Default_Trace_Filename_Env_Var"
                  else """" & To_String (IC.Dump_Config.Filename_Env_Var)
                       & """");
               Prefix  : constant String :=
                 (if Length (IC.Dump_Config.Filename_Prefix) = 0
                  then """" & String'(+Info.Project.Executable_Name
                    (File => +To_Filename
                         (Project => Info.Project,
                          CU_Name => Main,
                          Language => Switches.Ada_Language),
                     Include_Suffix => True)) & """"
                  else """" & To_String (IC.Dump_Config.Filename_Prefix)
                       & """");
               Tag     : constant String := """" & To_String (IC.Tag) & """";
               Simple  : constant String :=
                 (if IC.Dump_Config.Filename_Simple
                  then "True"
                  else "False");
            begin
               File.Put_Line
                 (Indent1 & "Filename => " & U & ".Default_Trace_Filename");
               File.Put_Line (Indent2 & "(Prefix => " & Prefix & ",");
               File.Put_Line (Indent2 & " Env_Var => " & Env_Var & ",");
               File.Put_Line (Indent2 & " Tag => " & Tag & ",");
               File.Put (Indent2 & " Simple => " & Simple & ") ");
            end;

         when Base64_Standard_Output =>

            --  Configurations using this channel generally run on embedded
            --  targets and have a small runtime, so our best guess for the
            --  program name is the name of the main, and there is no way to
            --  get the current execution time.

            File.Put_Line ("         Program_Name => """
                           & To_Ada (Main.Unit) & """,");
            File.Put ("         Exec_Date => 0");
         end case;
         File.Put_Line (");");

         File.Put_Line ("   end " & Dump_Procedure & ";");
         File.New_Line;

         --  Emit trigger-specific subprograms

         case Dump_Trigger is
            when At_Exit =>

               --  Emit a function to schedule a trace dump with atexit

               File.Put_Line
                 ("function "
                  & To_String (Register_Dump_Function_Name) & " return "
                  & To_Ada (Witness_Dummy_Type_Name) & " is");
               File.Put_Line ("   type Callback is access procedure;");
               File.Put_Line ("   pragma Convention (C, Callback);");
               File.New_Line;
               File.Put_Line ("   function atexit (Func : Callback)"
                              & " return Interfaces.C.int;");
               File.Put_Line ("   pragma Import (C, atexit);");
               File.Put_Line ("   Dummy : constant Interfaces.C.int :=");
               File.Put_Line ("     atexit (" & Dump_Procedure & "'Access);");
               File.Put_Line ("begin");
               File.Put_Line ("   return (Data => False);");
               File.Put_Line
                 ("end " & To_String (Register_Dump_Function_Name) & ";");
               File.New_Line;

            when Ravenscar_Task_Termination =>

               --  Emit a protected object for the callback

               File.Put_Line ("  protected Wrapper is");
               File.Put_Line ("     procedure Do_Dump"
                              & " (T : Ada.Task_Identification.Task_Id);");
               File.Put_Line ("  end Wrapper;");
               File.New_Line;
               File.Put_Line ("  protected body Wrapper is");
               File.Put_Line ("     procedure Do_Dump"
                              & " (T : Ada.Task_Identification.Task_Id) is");
               File.Put_Line ("        pragma Unreferenced (T);");
               File.Put_Line ("     begin");
               File.Put_Line ("        " & Dump_Procedure & ";");
               File.Put_Line ("     end Do_Dump;");
               File.Put_Line ("  end Wrapper;");
               File.New_Line;

               --  Emit a function to schedule a trace dump with
               --  Ada.Task_Termination.

               File.Put_Line
                 ("function "
                  & To_String (Register_Dump_Function_Name) & " return "
                  & To_Ada (Witness_Dummy_Type_Name) & " is");
               File.Put_Line ("begin");
               File.Put_Line ("   Ada.Task_Termination"
                              & ".Set_Dependents_Fallback_Handler"
                              & " (Wrapper.Do_Dump'Access);");
               File.Put_Line ("   return (Data => False);");
               File.Put_Line
                 ("end " & To_String (Register_Dump_Function_Name) & ";");
               File.New_Line;

            when Main_End =>
               if Has_Controlled then
                  File.Put_Line ("   overriding procedure Finalize (Self : in"
                                  & " out Dump_Controlled_Type) is");
                  File.Put_Line ("   begin");
                  File.Put_Line ("      Dump_Buffers;");
                  File.Put_Line ("   end Finalize;");
                  File.New_Line;
               end if;
         end case;

         File.Put_Line ("end " & Helper_Unit_Name & ";");
         File.Close;
      end;
   end Emit_Dump_Helper_Unit;

   ----------------------------
   -- Emit_Buffers_List_Unit --
   ----------------------------

   procedure Emit_Buffers_List_Unit
     (Self              : Ada_Instrumenter_Type;
      IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info)
   is
      CU_Name : constant Compilation_Unit_Name :=
        CU_Name_For_Unit (Buffers_List_Unit (IC), GNATCOLL.Projects.Unit_Spec);
      File    : Text_Files.File_Type;

      Instr_Units : CU_Name_Vectors.Vector;
   begin
      for Cur in IC.Instrumented_Units.Iterate loop
         declare
            Instr_Unit : constant Compilation_Unit_Name :=
              Instrumented_Unit_Maps.Key (Cur);
         begin
            Instr_Units.Append (Instr_Unit);
         end;
      end loop;

      --  Now emit the unit to contain the list of buffers

      declare
         Unit_Name : constant String := To_Ada (CU_Name.Unit);
      begin
         Create_File
           (Root_Project_Info,
            File,
            To_Filename
              (Root_Project_Info.Project,
               CU_Name,
               Switches.Ada_Language));
         Put_Warnings_And_Style_Checks_Pragmas (File);

         for Instr_Unit of Instr_Units loop

            --  Even though Ada buffer units are not explicitly referenced in
            --  the generated code (we import all the coverage buffers from
            --  their C symbol, as we want common processing for all
            --  languages), we still need to "with" them. Otherwise, gprbuild
            --  would not include them in the link as they would not be in the
            --  dependency closure.

            if Instr_Unit.Language_Kind = Unit_Based_Language then
               File.Put_Line ("with " & To_Ada (Buffer_Unit (Instr_Unit))
                              & ";");
            end if;
         end loop;
         File.New_Line;
         File.Put_Line ("package " & Unit_Name & " is");
         File.New_Line;
         File.Put_Line ("   pragma Preelaborate;");
         File.New_Line;

         --  Import all the coverage buffers

         for Instr_Unit of Instr_Units loop
            declare
               Buffer_Name : constant String := Unit_Buffers_Name (Instr_Unit);
            begin
               File.Put_Line
                 ("   " & Buffer_Name
                  & " : aliased constant GNATcov_RTS_Coverage_Buffers_Group;");
               File.Put_Line ("   pragma Import (C, " & Buffer_Name & ","""
                              & Buffer_Name & """);");
            end;
         end loop;

         --  Create the list of coverage buffers

         File.Put_Line ("   List : constant GNATcov_RTS.Buffers.Lists"
                        & ".Coverage_Buffers_Group_Array := (");
         for Cur in Instr_Units.Iterate loop
            declare
               use CU_Name_Vectors;

               Index       : constant Positive := To_Index (Cur);
               Index_Img   : constant String := Img (To_Index (Cur));
               Buffer_Name : constant String :=
                 Unit_Buffers_Name (Element (Cur));
            begin
               File.Put
                 ("      " & Index_Img & " => " & Buffer_Name & "'Access");
               if Index = Instr_Units.Last_Index then
                  File.Put_Line (");");
               else
                  File.Put_Line (",");
               end if;
            end;
         end loop;

         File.New_Line;
         File.Put_Line ("end " & Unit_Name & ";");
      end;
   end Emit_Buffers_List_Unit;

   ---------------------
   -- Instrument_Unit --
   ---------------------

   overriding procedure Instrument_Unit
     (Self      : Ada_Instrumenter_Type;
      CU_Name   : Compilation_Unit_Name;
      IC        : in out Inst_Context;
      Unit_Info : in out Instrumented_Unit_Info)
   is
      Prj_Info : Project_Info renames Unit_Info.Prj_Info.all;
      UIC      : Ada_Unit_Inst_Context;
   begin
      --  Instrument the source file and create a unit to contain its coverage
      --  buffers.

      Instrument_Source_File
        (CU_Name   => CU_Name,
         Unit_Info => Unit_Info,
         Prj_Info  => Prj_Info,
         IC        => IC,
         UIC       => UIC);
      Emit_Buffer_Unit (Prj_Info, UIC);
      Emit_Pure_Buffer_Unit (Prj_Info, UIC);

      --  Track which CU_Id maps to which instrumented unit

      Instrumented_Unit_CUs.Insert (CU_Name, UIC.CU);
   end Instrument_Unit;

end Instrument.Ada_Unit;
