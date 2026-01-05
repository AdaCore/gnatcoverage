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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Containers;             use Ada.Containers;
with Ada.Finalization;
pragma Warnings (Off, "* is an internal GNAT unit");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "* is an internal GNAT unit");
with Ada.Streams.Stream_IO;

with Langkit_Support;
with Langkit_Support.Slocs;                     use Langkit_Support.Slocs;
with Langkit_Support.Symbols;                   use Langkit_Support.Symbols;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Libadalang.Common;                         use Libadalang.Common;
with Libadalang.Expr_Eval;
with Libadalang.Generic_API;
with Libadalang.Generic_API.Introspection;
use Libadalang.Generic_API.Introspection;
with Libadalang.Sources;                        use Libadalang.Sources;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.Utils;

with Coverage_Options;                    use Coverage_Options;
with Coverage;                            use Coverage;
with Diagnostics;                         use Diagnostics;
with Instrument.Ada_Preprocessing;
with Instrument.Ada_Unit.Rewriting_Utils;
use Instrument.Ada_Unit.Rewriting_Utils;
with JSON;                                use JSON;
with Namet;                               use Namet;
with Outputs;                             use Outputs;
with Paths;                               use Paths;
with Project;
with SCOs;
with Slocs;
with Snames;                              use Snames;
with SS_Annotations;                      use SS_Annotations;
with Support_Files;
with Table;
with Templates_Parser;                    use Templates_Parser;
with Text_Files;                          use Text_Files;

package body Instrument.Ada_Unit is

   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   procedure Trace_Buffer_Unit
     (Buffer_Unit : String;
      Filename    : String;
      Prj         : Prj_Desc;
      CU_Names    : CU_Name_Vectors.Vector;
      Is_Pure     : Boolean);
   --  Helper to display details about a buffer unit to be emitted

   subtype Decl_Expr_Supported_Versions is
     Any_Language_Version range Ada_2022 .. Any_Language_Version'Last;
   --  Set of versions of the Ada language that support declare
   --  expressions.

   subtype If_Expr_Supported_Versions is
     Any_Language_Version range Ada_2012 .. Any_Language_Version'Last;
   --  Set of versions of the Ada language that support if expressions.

   function Create_Context_Instrument
     (N : Libadalang.Analysis.Ada_Node'Class) return Context_Handle;
   --  Create a context to show that gnatcov is instrumenting the given node

   --  Internal errors are by nature bound to be fixed, so we need to
   --  artificially trigger errors to exercize the error handling machinery,
   --  and thus to check that it works as expected. This is the role of the
   --  following helpers.

   function Format_Fingerprint
     (Fingerprint : SC_Obligations.Fingerprint_Type) return String
   is (Instrument.Common.Format_Fingerprint (Fingerprint, "(", ")"));
   --  Helper to format a String literal for a fingerprint

   function "+" (Part : Analysis_Unit_Kind) return GPR2.Valid_Unit_Kind
   is (case Part is
         when LALCO.Unit_Body          => GPR2.S_Body,
         when LALCO.Unit_Specification => GPR2.S_Spec);

   function Referenced_Attribute (N : Ada_Node'Class) return Text_Type
   is (if N.Kind = Ada_Attribute_Ref
       then Canonicalize (N.As_Attribute_Ref.F_Attribute.Text).Symbol
       else "");
   --  If ``N`` is an attribute reference, return the canonicalized name for
   --  that attribute. Return the empty string otherwise.

   function Render_Template
     (Tmplt_Name : String; T : Translate_Set) return Unbounded_String
   is (Parse (Support_Files.In_Share_Dir ("templates/" & Tmplt_Name), T));
   function Render_Template
     (Tmplt_Name : String; T : Translate_Table) return Unbounded_String
   is (Parse (Support_Files.In_Share_Dir ("templates/" & Tmplt_Name), T));
   --  Shortcut function for rendering a template to an Unbounded_String.

   -------------------------------
   -- Create_Context_Instrument --
   -------------------------------

   function Create_Context_Instrument
     (N : Libadalang.Analysis.Ada_Node'Class) return Context_Handle is
   begin
      return
        Create_Context
          ("Instrumenting "
           & N.Kind_Name
           & " at "
           & N.Unit.Get_Filename
           & ":"
           & Image (N.Sloc_Range));
   end Create_Context_Instrument;

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Name) return Ada_Qualified_Name;
   --  Return the qualified name corresponding to the given name from a parse
   --  tree.

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Unbounded_Text_Type_Array)
      return Ada_Qualified_Name;
   --  Convert a Libadalang fully qualified name into our format

   function Find_Ada_Units
     (Instrumenter : in out Ada_Instrumenter_Type; Filename : String)
      return String_Vectors.Vector;
   --  Consider that Filename is a source file to instrument (i.e. a unit of
   --  interest) and return the list of all compilation units that must be
   --  instrumented with it (i.e. related subunits, if present).
   --
   --  The result includes Filename in first position.

   procedure Insert_External_Annotations
     (RH          : in out Rewriting_Handle;
      Unit        : Analysis_Unit;
      Annotations : Instr_Annotation_Map)
   with Pre => RH /= No_Rewriting_Handle;
   --  Insert the annotations in Annotations into Unit.
   --
   --  For each annotation, the procedure searches the inner-most statement
   --  list containing its corresponding source location, and inserts the
   --  corresponding pragma right before the statement starting after the
   --  designated source location.
   --
   --  This expects an active rewriting context, but will apply the rewriting
   --  in the LAL tree, so the rewriting session will need to be re-started
   --  by the caller if needed.

   -----------------------
   -- Trace_Buffer_Unit --
   -----------------------

   procedure Trace_Buffer_Unit
     (Buffer_Unit : String;
      Filename    : String;
      Prj         : Prj_Desc;
      CU_Names    : CU_Name_Vectors.Vector;
      Is_Pure     : Boolean) is
   begin
      if not Sources_Trace.Is_Active then
         return;
      end if;

      Sources_Trace.Increase_Indent
        ("Writing"
         & (if Is_Pure then " pure" else "")
         & " Ada buffer unit "
         & Buffer_Unit);
      Sources_Trace.Trace ("Project: " & To_Ada (Prj.Prj_Name));
      Sources_Trace.Trace ("Filename: " & Filename);
      Sources_Trace.Trace ("For units:");
      for CU of CU_Names loop
         Sources_Trace.Trace ("* " & Image (CU));
      end loop;
      Sources_Trace.Decrease_Indent;
   end Trace_Buffer_Unit;

   -----------------------
   -- To_Qualified_Name --
   -----------------------

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Name) return Ada_Qualified_Name is
   begin
      return Result : Ada_Qualified_Name do
         case Ada_Name (Name.Kind) is
            when Ada_Dotted_Name     =>
               declare
                  DN     : constant Dotted_Name := Name.As_Dotted_Name;
                  Suffix : constant Ada_Qualified_Name :=
                    To_Qualified_Name (DN.F_Suffix.As_Name);
               begin
                  Result := To_Qualified_Name (DN.F_Prefix);
                  Result.Append (Suffix);
               end;

            when Ada_Single_Tok_Node =>
               declare

                  --  ??? GPR2 does not specify how to encode Unicode unit
                  --  names as strings, so for now, assume that we process only
                  --  codepoints in the ASCII range and thus use
                  --  Langkit_Support.Text.Image.

                  Identifier : constant Ada_Identifier :=
                    To_Unbounded_String (Image (Name.Text));
               begin
                  Result.Append (Identifier);
               end;

            when others              =>
               raise Constraint_Error
                 with "no qualified name for " & Name.Kind'Image & " nodes";
         end case;
      end return;
   end To_Qualified_Name;

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Unbounded_Text_Type_Array)
      return Ada_Qualified_Name is
   begin
      return Result : Ada_Qualified_Name do
         for N of Name loop

            --  ??? Same limitation regarding non-ASCII characters as above

            Result.Append
              (To_Unbounded_String (Image (To_Wide_Wide_String (N))));
         end loop;
      end return;
   end To_Qualified_Name;

   type All_Symbols is
     (
     --  Aspects

     Dynamic_Predicate,
      Invariant,
      Ghost_Predicate,
      Post,
      Postcondition,
      Pre,
      Precondition,
      Predicate,
      Static_Predicate,
      Type_Invariant,

      --  Pragmas

      Convention,
      Debug,
      Profile,
      Restrictions,

      --  Pragma Restrictions arguments

      No_Dependence,
      No_Finalization,
      No_Tasking,
      Pure_Barriers,
      Simple_Barriers,

      --  Pragma Profile arguments

      GNAT_Extended_Ravenscar,
      GNAT_Ravenscar_EDF,
      Jorvik,
      Ravenscar,
      Restricted,

      --  Annotations

      Xcov,
      Dump_Buffers,
      Reset_Buffers);

   Symbols : constant Symbol_Table := Create_Symbol_Table;
   --  Holder for name singletons

   function Precompute_Symbol (S : All_Symbols) return Symbol_Type
   is (Find (Symbols, Canonicalize (To_Wide_Wide_String (S'Image)).Symbol));

   Precomputed_Symbols : constant array (All_Symbols) of Symbol_Type :=
     (Dynamic_Predicate       => Precompute_Symbol (Dynamic_Predicate),
      Ghost_Predicate         => Precompute_Symbol (Ghost_Predicate),
      Invariant               => Precompute_Symbol (Invariant),
      Post                    => Precompute_Symbol (Post),
      Postcondition           => Precompute_Symbol (Postcondition),
      Pre                     => Precompute_Symbol (Pre),
      Precondition            => Precompute_Symbol (Precondition),
      Predicate               => Precompute_Symbol (Predicate),
      Static_Predicate        => Precompute_Symbol (Static_Predicate),
      Type_Invariant          => Precompute_Symbol (Type_Invariant),
      Convention              => Precompute_Symbol (Convention),
      Profile                 => Precompute_Symbol (Profile),
      Debug                   => Precompute_Symbol (Debug),
      Restrictions            => Precompute_Symbol (Restrictions),
      No_Dependence           => Precompute_Symbol (No_Dependence),
      No_Finalization         => Precompute_Symbol (No_Finalization),
      No_Tasking              => Precompute_Symbol (No_Tasking),
      Pure_Barriers           => Precompute_Symbol (Pure_Barriers),
      Simple_Barriers         => Precompute_Symbol (Simple_Barriers),
      GNAT_Extended_Ravenscar => Precompute_Symbol (GNAT_Extended_Ravenscar),
      GNAT_Ravenscar_EDF      => Precompute_Symbol (Dynamic_Predicate),
      Jorvik                  => Precompute_Symbol (Jorvik),
      Ravenscar               => Precompute_Symbol (Ravenscar),
      Restricted              => Precompute_Symbol (Restricted),
      Xcov                    => Precompute_Symbol (Xcov),
      Dump_Buffers            => Precompute_Symbol (Dump_Buffers),
      Reset_Buffers           => Precompute_Symbol (Reset_Buffers));

   function As_Symbol (S : All_Symbols) return Symbol_Type
   is (Precomputed_Symbols (S));

   function As_Symbol (Id : Identifier) return Symbol_Type;
   function As_Name (Id : Identifier) return Name_Id;
   --  Canonicalize Node and return a corresponding Name_Id/Symbol_Type

   function As_Symbol (Id : Text_Type) return Symbol_Type
   is (Find (Symbols, Id));
   --  Return a symbol for the given identifier Id. Note that Id is supposed
   --  to be already canonicalized.

   function Pragma_Name (P : Pragma_Node) return Symbol_Type;
   function Pragma_Name (P : Pragma_Node) return Name_Id;
   --  Return a symbol from Symbols corresponding to the name of the given
   --  P pragma.

   type Pragma_Matcher is record
      Pragma_Name : Symbol_Type;
      Assoc_Name  : Symbol_Type;
      Expr_Name   : Symbol_Type;
   end record;
   --  Matcher for a pragma: Pragma_Name is the name of the pragmas that can
   --  match, while Assoc_Name and Expr_Name are the name/expression of at
   --  least one of the pragma's arguments (Assoc_Name can be null).
   --
   --  For example, the following matcher:
   --
   --    (As_Symbol ("foo"), null, As_Symbol ("bar"))
   --
   --  can match:
   --
   --    pragma foo (bar);
   --    pragma Foo (Bar);
   --    pragma Foo (name, Bar);
   --
   --  but not:
   --
   --    pragma name (bar);         --  unexpected pragma name ("name")
   --    pragma foo (bar => 1);     --  unexpected argument expr ("1")
   --    pragma foo (name => bar);  --  unexpected argument name ("name")
   --
   --  And the following matcher:
   --
   --    (As_Symbol ("foo"), As_Symbol ("bar"), As_Symbol ("x.y"))
   --
   --  can match:
   --
   --    pragma foo (bar => x.y);
   --    pragma Foo (Bar => X.Y);
   --    pragma Foo (name, Bar => X.Y);
   --
   --  but not:
   --
   --    pragma foo (bar);        --  unexpected argument name (none)
   --    pragma foo (bar => 1);   --  unexpected argument expr ("1")
   --    pragma foo (name => x);  --  unexpected argument name ("name")

   type Pragma_Matcher_Array is array (Positive range <>) of Pragma_Matcher;

   function Matches
     (P : Pragma_Node; Matchers : Pragma_Matcher_Array) return Boolean;
   --  Return whether pragam P matches at least one of the given pragam
   --  matchers.

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

   function Safe_Previous_Part_For_Decl
     (N : Basic_Decl'Class) return Basic_Decl;
   --  Wrapper around P_Previous_Part_For_Decl to protect ourselves against
   --  property errors. If the property fails for some reason, return the null
   --  node.

   function Op_Symbol_To_Name
     (Op : Libadalang.Analysis.Name) return Wide_Wide_String;
   --  Given an operator symbol (in its source representation
   --  in the form of a quoted string literal), return a name
   --  suitable for construction of a regular identifier.

   function Sloc (N : Ada_Node'Class) return Source_Location
   is (Start_Sloc (N.Sloc_Range));

   function "+" (Sloc : Source_Location) return Slocs.Local_Source_Location
   is ((Natural (Sloc.Line), Natural (Sloc.Column)));

   function Expr_Needs_Parens (Kind : Ada_Node_Kind_Type) return Boolean
   is (Kind
       in Ada_Quantified_Expr | Ada_If_Expr | Ada_Case_Expr | Ada_Decl_Expr);
   --  Whether nodes of type Kind must be wrapped with parens

   function Expression_Type
     (UIC : Ada_Unit_Inst_Context; E : Expr) return Base_Type_Decl;
   --  Wrapper around E.P_Expression_Type, logging a warning and returning
   --  Standard.Boolean if unable to determine the type.

   function Is_Static_Expr (E : Expr'Class) return Boolean;
   --  Wrapper around E.P_Is_Static_Expr, logging a warning and returning
   --  False if unable to determine whether the expression is static.

   function Bool_Expr_Eval (E : Expr) return String;
   --  Wrapper around Libadalang.Expr_Eval.Expr_Eval, for static boolean
   --  expressions. Log a warning and return an empty string result if unable
   --  to evaluate the expression.

   function Is_Ghost
     (UIC : Ada_Unit_Inst_Context; D : Basic_Decl) return Boolean;
   --  Return whether the given expression function is ghost (EF or its
   --  canonical declaration has a Ghost aspect).

   function Is_Generic
     (UIC : Ada_Unit_Inst_Context; Decl : Basic_Decl'Class) return Boolean;
   --  Return whether the given declaration is generic (its canonical part is
   --  generic).

   function To_Nodes
     (Handle : Rewriting_Handle; Name : Ada_Qualified_Name)
      return Node_Rewriting_Handle
   with Pre => not Name.Is_Empty;
   --  Turn the given qualified name into a name tree for rewriting

   function Unwrap (N : Expr) return Expr;
   --  Strip Paren_Expr from N

   function In_Package_Spec (N : Ada_Node'Class) return Boolean;
   --  Return whether N is a direct child of a package specification's
   --  declarative part (the public or the private one).

   function Inclusive_End_Sloc
     (SL : Source_Location_Range) return Source_Location;
   --  End slocs from Libadalang nodes are exclusive: the correspond to the
   --  source location for the (hypothetical) character right after the last
   --  character that was consumed to produce the node. In gnatcov, we need the
   --  sloc of this last character, so we need to subtract 1 from the column
   --  number.

   function Clone (N : Ada_Node'Class) return Node_Rewriting_Handle
   is (if N.Is_Null then No_Node_Rewriting_Handle else Clone (Handle (N)));
   --  Simple wrapper around Libadalang's Clone, except that it works on parse
   --  nodes, and accepts null nodes.

   function Detach (N : Ada_Node'Class) return Node_Rewriting_Handle;
   --  Replace N with No_Node_Rewriting_Handle, and return its previous
   --  handle for possible reuse elsewhere in the tree.

   No_Children : constant Node_Rewriting_Handle_Array :=
     (1 .. 0 => No_Node_Rewriting_Handle);

   function Make
     (UIC : Ada_Unit_Inst_Context'Class; K : Ada_Node_Kind_Type)
      return Node_Rewriting_Handle
   is (Create_Node (UIC.Rewriting_Context, K));
   --  Shortcut to create a node of the given kind

   function Make_Defining_Name
     (UIC : Ada_Unit_Inst_Context'Class; D_Name : Wide_Wide_String)
      return Node_Rewriting_Handle
   is (Create_Defining_Name
         (UIC.Rewriting_Context,
          Make_Identifier (UIC.Rewriting_Context, D_Name)));
   --  Shortcut to create a defining identifier tree

   function Make_Std_Ref
     (UIC : Ada_Unit_Inst_Context'Class; Entity : Text_Type)
      return Node_Rewriting_Handle
   is (Create_From_Template
         (UIC.Rewriting_Context,
          "GNATcov_RTS.Std." & Entity,
          (1 .. 0 => <>),
          Rule => Expr_Rule));
   --  Language-defined entities such as "Standard" or "Boolean" may be hidden
   --  by entities defined in the code to instrument. To avoid compilation
   --  issues, we have an accessible package that renames Standard in
   --  GNATcov_RTS: this function allows to refer to standard entities from
   --  this renaming.

   ---------------------
   -- Unbounded texts --
   ---------------------

   T_Ghost : constant Unbounded_Text_Type := To_Unbounded_Text ("Ghost");

   -----------------
   -- Diagnostics --
   -----------------

   procedure Report
     (UIC  : Ada_Unit_Inst_Context'Class;
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
     (UIC  : Ada_Unit_Inst_Context'Class;
      Node : Ada_Node'Class;
      Msg  : String;
      Kind : Report_Kind := Diagnostics.Error)
   is
      LAL_Loc : constant Source_Location := Sloc (Node);
   begin
      Diagnostics.Report
        ((Source_File => UIC.SFI,
          L           =>
            (Line   => Integer (LAL_Loc.Line),
             Column => Integer (LAL_Loc.Column))),
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
          (Node.Unit.Get_Filename, Kind => Files_Table.Source_File);
      LAL_Loc : constant Source_Location := Sloc (Node);
   begin
      Diagnostics.Report
        ((Source_File => SFI,
          L           =>
            (Line   => Integer (LAL_Loc.Line),
             Column => Integer (LAL_Loc.Column))),
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
     (UIC          : Ada_Unit_Inst_Context;
      Bit          : Bit_Id;
      Flavor       : Statement_Witness_Flavor;
      In_Generic   : Boolean;
      In_Decl_Expr : Boolean) return Node_Rewriting_Handle;
   --  Create a procedure call statement or object declaration to witness
   --  execution of the low level SCO with the given bit Node.
   --
   --  In_Generic indicates whether the statement witness is destined
   --  to be inserted in a generic package or subprogram.
   --
   --  In_Decl_Expr indicates whether the statement witness is inserted as
   --  a declaration in a declare expression.

   procedure Fill_Expression_Insertion_Info
     (UIC : in out Ada_Unit_Inst_Context; Bit : Any_Bit_Id);
   --  Fill UIC.Current_Insertion_Info with new witness formal and actual

   procedure Ensure_With
     (UIC : in out Ada_Unit_Inst_Context'Class; Unit : Text_Type);
   --  Ensure that the unit being instrumented has a dependency on the named
   --  Unit, which must be specified in the normalized form expected for
   --  FQN_Sets (lower case, period separated, fully qualified).

   function Make_MCDC_State_Name (LL_SCO_Id : Nat) return String
   is ("MCDC_State_" & Img (Integer (LL_SCO_Id)));
   --  Return the name of the MC/DC state local variable for the given
   --  decision SCO.

   --  The default MC/DC state inserter inserts MC/DC state buffers as
   --  variable declarations in the nearest enclosing subprogram.

   type Default_MCDC_State_Inserter is new Root_MCDC_State_Inserter with record
      Local_Decls : Node_Rewriting_Handle;
   end record;

   overriding
   function Insert_MCDC_State
     (Inserter : in out Default_MCDC_State_Inserter;
      UIC      : in out Ada_Unit_Inst_Context'Class;
      Name     : String) return String;

   function Require_MCDC_State_Inserter
     (UIC      : in out Ada_Unit_Inst_Context'Class;
      E        : Expr'Class;
      Inserter : aliased in out Default_MCDC_State_Inserter'Class)
      return Boolean;
   --  If UIC already has a state inserter, do nothing. Otherwise, try to
   --  create one, wrapping E in a declare expression. This only emits a
   --  warning if the current Ada version does not support declare expressions.
   --
   --  Return whether UIC has a state inserter upon return.

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
   --  For expression functions, there are four instrumentation strategies,
   --  depending on its spec, the context in which it is declared and the
   --  language version used.
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
   --  The last strategy only applies to Ada 2022 sources. In that case, a
   --  declare expression is used to prepend a list of declarations prior to
   --  the expression of the expression function, and is used to host the
   --  statement witness call, and MC/DC state holders.
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
   --     --  See Create_Augmented_Function.Augmented_Function
   --     --
   --     --  Sometimes, we don't emit this declaration,
   --     --  see Augmented_Function_Needs_Decl
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
   --     --  See Create_Augmented_Function.Augmented_Function
   --     function Foo_With_State_[Subprogram_Index]
   --       (Arg1                 : Arg1_Type;
   --        Arg2                 : Arg2_Type;
   --        MCDC_State_2         : GNATcov_RTS.Buffers.MCDC_State_Holder;
   --        Dummy_Witness_Result : Boolean)
   --        return Boolean
   --     is ([origin expression plus Witness calls using MCDC_State_2]);;
   --
   --     --  See Create_Augmented_Function.New_Function
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
   --  --  See Create_Augmented_Function.Augmented_Function
   --  package Expr_Func_Pkg_[Subprogram_Index] is
   --    function Foo_With_State_[Subprogram_Index]
   --      (Arg : Arg_Type;
   --       Dummy_Witness_Result : Boolean)
   --       return Ret_Type is (<some expression>);
   --  end Expr_Func_Pkg_[Subprogram_Index];
   --
   --  --  See Create_Augmented_Function.New_Function
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
   --  Expression function in Ada 2022 sources example (4th strategy)
   --  ==============================================================
   --
   --  In the following expression function, located in an Ada 2022 source:
   --
   --  function Foo (Arg : Arg_Type) return Boolean is (Arg.X or else Arg.Y);
   --
   --  the expression part is wrapped in a declare expression, in order to host
   --  declarations for the stmt witness call and MC/DC state variable
   --  declarations:
   --
   --  function Foo (Arg : Arg_Type) return Boolean is
   --    (declare
   --        <stmt witness declaration call>
   --        <MC/DC state variable declaration>
   --     begin
   --        [origin expression plus decision & MC/DC witness calls]);
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
      Renaming_Decl : out Node_Rewriting_Handle;
      Fun_Witness   : Node_Rewriting_Handle);
   --  Create the body for the generic subprogram (Subp_Body), its
   --  instantiation declaration (Instance) and the renaming for this instance
   --  (Renaming_Decl).
   --  Fun_Witness is only used for function coverage. It is set the
   --  No_Node_Rewriting_Handle if function coverage is not needed, and to a
   --  valid witness call if it is. If set, it is inserted at the beginning of
   --  the generic subprogram and responsible for discharging the function SCO
   --  associated to the null procedure.

   function Clone_Params
     (UIC : Ada_Unit_Inst_Context; N_Spec : Subp_Spec)
      return Node_Rewriting_Handle;
   --  Create a list of formal parameters as a copy of N_Spec's. If N_Spec has
   --  no formals, return an empty list.

   type Expr_Func_MCDC_State_Inserter is new Root_MCDC_State_Inserter
   with record
      N_Spec : Subp_Spec;
      --  Subprogram spec for the original expression function

      Call_Params : Node_Rewriting_Handle;
      --  Assoc_List node for the call to the augmented expression function

      Formal_Params : Node_Rewriting_Handle;
      --  Formal parameter list where new parameters are added to hold MC/DC
      --  temporary buffers.
   end record;

   overriding
   function Insert_MCDC_State
     (Inserter : in out Expr_Func_MCDC_State_Inserter;
      UIC      : in out Ada_Unit_Inst_Context'Class;
      Name     : String) return String;

   function Create_Function_Witness_Var
     (UIC : Ada_Unit_Inst_Context; Fun_Witness : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is (Create_From_Template
         (UIC.Rewriting_Context,
          Template  => "Dummy_Witness_Var : constant Boolean := {};",
          Arguments => (1 => Fun_Witness),
          Rule      => Basic_Decls_Rule))
   with Pre => Fun_Witness /= No_Node_Rewriting_Handle;
   --  Create a dummy variable and set it to the properly set function witness
   --  call. The function witness must be a function call.

   procedure Instrument_For_Function_Coverage
     (UIC            : in out Ada_Unit_Inst_Context;
      Spec           : Subp_Spec;
      Witness_Flavor : Statement_Witness_Flavor;
      Fun_Witness    : out Node_Rewriting_Handle)
   with Pre => Enabled (Fun_Call);
   --  Add a function coverage SCO to Spec and set Fun_Witness to a valid
   --  witness call of flavor Flavor.

   procedure Create_Augmented_Function
     (UIC                     : Ada_Unit_Inst_Context;
      Common_Nodes            : Degenerate_Subp_Common_Nodes;
      Formal_Params           : Node_Rewriting_Handle;
      Call_Params             : Node_Rewriting_Handle;
      Augmented_Function      : out Node_Rewriting_Handle;
      Augmented_Function_Decl : out Node_Rewriting_Handle;
      New_Function            : out Node_Rewriting_Handle;
      Needs_Aspects           : Boolean := False);
   --  Create the augmented function from the original one (Augmented_Function)
   --  and create the new function (New_Function) that will serve as a
   --  replacement to the original one. Also create a declaration for the
   --  augmented function, if needed. It should be inserted right before the
   --  previous declaration of the original function (which is guaranteed to
   --  exist).
   --
   --  If the original function is a primitive of its return type, then
   --  Agmented_Function will not be a handle to the function, but rather to
   --  the nested package containing the augmented function.
   --
   --  * Common_Nodes:
   --      Contains the original nodes from which the new functions will
   --      be made.
   --  * Formal_Params:
   --      The list of formals that Augmented_Function will accept
   --  * Call_Params:
   --      The list of parameters that will be passed to the call to
   --      Augmented_Function made in New_Function.
   --  * Augmented_Function:
   --      The newly created subprogram whose body contains all the statements
   --      from the original function.
   --  * Augmented_Function_Decl:
   --      The declaration of Augmented_Function if needed.
   --  * New_Function:
   --      The new intermediate function that replaces the original one.
   --      Responsible for calling Augmented_Function.

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
   --  declarative region as its previous declaration, then there is no need to
   --  insert a declaration for the augmented expression function, beause in
   --  that case it isn't a primitive.

   function Augmented_EF_Needs_Wrapper_Package
     (Common_Nodes : Degenerate_Subp_Common_Nodes) return Boolean;
   --  Returns whether the augmented expression function needs to be wrapped in
   --  a nested package.

   function Is_Self_Referencing
     (UIC : Ada_Unit_Inst_Context; EF : Expr_Function) return Boolean;
   --  Return if EF is a self-referencing expression function, i.e. if its
   --  expression has a reference to itself (for instance: it's a recursive
   --  function).

   function Return_Type_Is_Controlling
     (UIC : Ada_Unit_Inst_Context; Common_Nodes : Degenerate_Subp_Common_Nodes)
      return Boolean
   with Pre => not Is_Null (Common_Nodes.N_Spec.F_Subp_Returns);
   --  Return True if the expression function from which the common nodes were
   --  generated is a primitive of a tagged type, and if that tagged type is
   --  the return type of the expression function.

   function Has_Access_Attribute_Ref (E : Expr) return Boolean;
   --  Return whether E or one of its subexpressions is a reference to the
   --  'Access, 'Unchecked_Access or 'Unrestricted_Access attributes.

   function Has_Matching_Pragma_For_Unit
     (Context  : Analysis_Context;
      Unit     : LAL.Compilation_Unit;
      Matchers : Pragma_Matcher_Array) return Boolean;
   --  Return whether at least one matcher in Matchers accepts at least one
   --  configuration pragma that applies to Unit or system.ads.

   Unusable_System_Reported : Boolean := False;
   --  Global variable set to True once gnatcov emits a warning about a failure
   --  to get the analysis unit for System. Used to avoid emitting duplicate
   --  messages.

   function Has_Unit
     (Context : Analysis_Context; Unit : String; Part : Analysis_Unit_Kind)
      return Boolean;
   --  Return whether the given unit exists

   Pragma_Restricts_Finalization_Matchers : constant Pragma_Matcher_Array :=
     ((As_Symbol (Restrictions), No_Symbol, As_Symbol (No_Finalization)),
      (As_Symbol (Restrictions),
       As_Symbol (No_Dependence),
       As_Symbol ("ada.finalization")));
   --  Matchers for pragmas that impose a restrictions on use of finalization

   function Finalization_Restricted_In_Unit
     (Context : Analysis_Context; Unit : LAL.Compilation_Unit) return Boolean;
   --  Return True if Finalization is not available in this runtime, or if
   --  some control pragma restricts the usage of finalization in either Unit
   --  or the whole project.

   Pragma_Prevents_Task_Termination_Matchers : constant Pragma_Matcher_Array :=
     ((As_Symbol (Restrictions), No_Symbol, As_Symbol (No_Finalization)),
      (As_Symbol (Restrictions), No_Symbol, As_Symbol (No_Tasking)),
      (As_Symbol (Restrictions),
       As_Symbol (No_Dependence),
       As_Symbol ("ada.task_termination")),
      (As_Symbol (Restrictions),
       As_Symbol (No_Dependence),
       As_Symbol ("ada.identification")));
   --  Matchers for pragmas that prevent the use of tasks and/or
   --  Ada.Task_Termination and/or Ada.Task_Identification.

   function Task_Termination_Restricted
     (Context : Analysis_Context; Unit : LAL.Compilation_Unit) return Boolean;
   --  Return True if tasking is not available in this runtime, or if some
   --  configuration pragma prevents the use of tasks and/or
   --  Ada.Task_Termination and/or Ada.Task_Identification in either the whole
   --  project or in Unit.

   Pragma_Restricts_Entry_Guards_Matchers : constant Pragma_Matcher_Array :=
     ((As_Symbol (Restrictions), No_Symbol, As_Symbol (Pure_Barriers)),
      (As_Symbol (Restrictions), No_Symbol, As_Symbol (Simple_Barriers)),
      (As_Symbol (Profile), No_Symbol, As_Symbol (GNAT_Extended_Ravenscar)),
      (As_Symbol (Profile), No_Symbol, As_Symbol (GNAT_Ravenscar_EDF)),
      (As_Symbol (Profile), No_Symbol, As_Symbol (Jorvik)),
      (As_Symbol (Profile), No_Symbol, As_Symbol (Ravenscar)),
      (As_Symbol (Profile), No_Symbol, As_Symbol (Restricted)));
   --  Matchers for Restrictions pragmas that restrict entry guards so that we
   --  cannot instrument them as decisions (Pure_Barriers and Simple_Barriers,
   --  plus various Ada runtime profiles).

   function Entry_Guards_Restricted
     (Context : Analysis_Context; Unit : LAL.Compilation_Unit) return Boolean;
   --  Return if entry guards are restricted in this unit so that we cannot
   --  instrument them as decisions.

   function Return_From_Subp_Body
     (Ret_Node : Return_Stmt; Subp : Subp_Body) return Boolean;
   --  Return whether Ret_Node is returning from Subp

   function Parent_Decl (Decl : Basic_Decl'Class) return Basic_Decl;
   --  Return the parent declaration for Decl, or No_Basic_Decl if Decl has no
   --  parent, or if we cannot find it.

   function Decls_Are_Library_Level
     (Unit : Libadalang.Analysis.Compilation_Unit) return Boolean;
   --  Return whether declarations that appear directly under the given
   --  compilation unit are library-level.

   function Prag_Arg_Expr (Args : Base_Assoc_List; I : Positive) return Expr
   is (Args.Child (I).As_Pragma_Argument_Assoc.F_Expr);
   --  Return the expression for the Index'th argument of a pragma's
   --  arguments.

   function Get_Convention (Decl : Basic_Decl) return Identifier;
   --  Return the convention associated with Decl, either from the Convention
   --  aspect or from the convention specified in the Export aspect. If no
   --  convention is defined, return No_Identifier.

   procedure Process_Annotation
     (UIC       : in out Ada_Unit_Inst_Context;
      N         : Ada_Node;
      Prag_Args : Base_Assoc_List)
   with
     Pre =>
       N.Kind = Ada_Pragma_Node
       and then Pragma_Name (N.As_Pragma_Node) = Name_Annotate;
   --  Handle an Annotate pragma.
   --
   --  If this is not an Xcov annotation, do nothing. Otherwise, decode it and
   --  add it to our internal tables. If the pragma is not correctly formatted
   --  (decoding failure) just emit a warning.

   --------------------------------
   -- Instrumentation extensions --
   --------------------------------

   procedure Enter_Scope
     (UIC  : in out Ada_Unit_Inst_Context;
      N    : Ada_Node'Class;
      Decl : Basic_Decl);
   --  Enter a scope. This must be completed with a call to the function
   --  Exit_Scope, defined below. Assume that the scope first SCO is the next
   --  generated SCO (SCOs.SCO_Table.Last + 1), and also assume that Decl
   --  refers to the the specification of N, to uniquely identify the scope.
   --  Update UIC.Current_Scope_Entity to the created entity.

   procedure Exit_Scope (UIC : in out Ada_Unit_Inst_Context);
   --  Exit the current scope, updating UIC.Current_Scope_Entity to
   --  UIC.Current_Scope_Entity.Parent, if any. Assume that the last generated
   --  SCO (SCOs.SCO_Table.Last) is the last SCO for the current scope.

   procedure Start_Statement_Block (UIC : in out Ada_Unit_Inst_Context);
   --  Start a new statement block on top of the currently active block

   procedure End_Statement_Block (UIC : in out Ada_Unit_Inst_Context);
   --  End the currently active statement block

   procedure Insert_Stmt_Witness
     (UIC             : in out Ada_Unit_Inst_Context;
      Stmt_Instr_Info : Stmt_Instr_Info_Type;
      Bit             : Any_Bit_Id);
   --  Insert a statement witness call for the given Bit.
   --
   --  Stmt_Instr_Info controls the insertion of the witness call. Refer to the
   --  definition of Stmt_Instr_Info_Type for more information.

   ----------------------------
   --  Context miscellaneous --
   ----------------------------

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

   type Missing_Src_Reporter is new Libadalang.Analysis.Event_Handler_Interface
   with record
      Instrumented_File : Unbounded_String;
      --  Base name for the file that is currently instrumented. Reset to the
      --  empty string everytime we print the "While instrumenting XXX ..."
      --  message, so that we print it at most once per instrumented file.

      Reported_Files : String_Sets.Set;
      --  Set of source file names which were already reported as missing.
      --  Libadalang does not guarantee that the Unit_Requested event is
      --  triggered only once per source, so de-duplicate events with this set.
   end record;
   --  Implementation of the Libadalang event handler interface used in
   --  Create_Missing_File_Reporter.

   type Missing_Src_Reporter_Access is access all Missing_Src_Reporter;

   overriding
   procedure Release (Self : in out Missing_Src_Reporter) is null;

   overriding
   procedure Unit_Requested_Callback
     (Self               : in out Missing_Src_Reporter;
      Context            : Libadalang.Analysis.Analysis_Context'Class;
      Name               : Langkit_Support.Text.Text_Type;
      From               : Libadalang.Analysis.Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean);
   --  If the requested unit is not found and that is an error, warn about it.
   --  Make sure we warn only once about a given source file.

   function Create_Missing_File_Reporter
      return Libadalang.Analysis.Event_Handler_Reference;
   --  Create an event handler to warn about source files that Libadalang needs
   --  to perform semantic analysis (so mandated by Ada), but which are not
   --  available.

   procedure Create_LAL_Context
     (Instrumenter : in out Ada_Instrumenter_Type'Class);
   --  Create a new Libadalang analysis context for Instrumenter, assigning it
   --  to Instrumenter.Context.
   --
   --  This helper takes care of passing the unit provider and the event
   --  handler that we need for all such contexts, and resets
   --  Instrumenter.Get_From_File_Count to 0, as the new context has not been
   --  used to instrument any source file yet.

   function Get_From_File
     (Instrumenter : in out Ada_Instrumenter_Type'Class;
      Filename     : String;
      Reparse      : Boolean := False)
      return Libadalang.Analysis.Analysis_Unit;
   --  Fetch the analysis unit for the given filename

   -------------------------
   -- Source instrumenter --
   -------------------------

   type Ada_Source_Rewriter is limited new Ada.Finalization.Limited_Controlled
   with record
      Input_Filename  : Unbounded_String;
      Output_Filename : Unbounded_String;

      Unit   : Libadalang.Analysis.Analysis_Unit;
      Handle : Libadalang.Rewriting.Rewriting_Handle;
   end record;

   overriding
   procedure Finalize (Self : in out Ada_Source_Rewriter);

   procedure Start_Rewriting
     (Self           : out Ada_Source_Rewriter'Class;
      Instrumenter   : in out Ada_Instrumenter_Type'Class;
      Prj            : Prj_Desc;
      Input_Filename : String);
   --  Start a rewriting session for the given Input_Filename. If the rewriting
   --  process is successful, the result will be written to a file in
   --  Info.Output_Dir with the basename of Output_Filename.
   --
   --  This registers the output file in Info.Instr_Files.
   --
   --  If there are parsing errors while reading Input_Filename, this raises a
   --  fatal error and prints the corresponding error messages.

   procedure Start_Rewriting
     (Self         : out Ada_Source_Rewriter'Class;
      Instrumenter : in out Ada_Instrumenter_Type'Class;
      Prj          : Prj_Desc;
      Unit         : Analysis_Unit);
   --  Same as above, but initiating the rewriting session from Unit, skipping
   --  the diagnostics checks.
   --
   --  This variation must be used if some analysis on a unit had already taken
   --  place, in order to avoid a new call to Get_From_File, potentially
   --  resetting the context, voiding all previous references.

   function Rewritten_Unit
     (Self : Ada_Source_Rewriter'Class)
      return Libadalang.Analysis.Analysis_Unit;
   --  Return the analysis unit for the source that Self instruments

   procedure Apply (Self : in out Ada_Source_Rewriter'Class);
   --  Write the instrumented source to the filename passed as Output_Filename
   --  to Start_Rewriting. If rewriting failed, raise a fatal error and print
   --  the corresponding error message.

   procedure Write_To_File (Unit : Unit_Rewriting_Handle; Filename : String);
   --  Unparse Unit into the file at Filename (creating it if needed).
   --
   --  Note that this calls Put_Warnings_And_Style_Checks_Pragmas before
   --  unparsing the unit.

   ----------------------------
   -- Source level rewriting --
   ----------------------------

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

   procedure Initialize_Rewriting
     (UIC          : in out Ada_Unit_Inst_Context;
      Instrumenter : Ada_Instrumenter_Type'Class);
   --  Initialize a unit instrumentation context for the given unit to
   --  instrument.

   procedure Instrument_Source_File
     (UIC          : in out Ada_Unit_Inst_Context;
      Filename     : String;
      Instrumenter : in out Ada_Instrumenter_Type;
      Prj          : Prj_Desc);
   --  Generate the instrumented source corresponding to Filename

   ----------------------------------
   -- Main instrumentation helpers --
   ----------------------------------

   Cannot_Instrument_Main_Error : exception;
   --  See Probe_Main

   type Main_Instrumentation_Description (Synthetic : Boolean := False) is
   record
      Main : Compilation_Unit_Part;
      --  Name of the compilation unit corresponding to the main body

      Controlled_Types_Available : Boolean;
      --  Whether instrumentation can insert uses of controlled types

      Actual_Auto_Dump_Trigger : Auto_Dump_Trigger;
      --  Resolved dump trigger after eventual override depending on the
      --  features available on the runtime.

      Prelude : Node_Rewriting_Handle;
      --  Prelude (list of nodes) for the main compilation unit

      Main_Decls : Node_Rewriting_Handle;
      --  List of declarations for the procedure body that implements the main

      Main_Stmts : Node_Rewriting_Handle;
      --  List of statements for the procedure body that implements the main

      case Synthetic is
         when False =>
            Subp_Body : LAL.Subp_Body;
            --  Subprogram body in which to insert the code to dump coverage
            --  buffers.

         when True =>
            Generic_Wrapper_Body_Filename : Unbounded_String;
            Generic_Wrapper_Body          : Node_Rewriting_Handle;
            --  See homonym arguments in Expand_Main_Generic_Instantiation
      end case;
   end record;
   --  Nodes needed to instrument main subprograms so that they can dump
   --  coverage buffers.
   --
   --   Synthetic designates whether the main subprogram body comes from
   --   sources (Synthetic => False) or was created to wrap a generic
   --   subprogram instantiation (Synthetic => True).

   function Probe_Main
     (Prj         : Prj_Desc;
      Dump_Config : Any_Dump_Config;
      Rewriter    : Ada_Source_Rewriter'Class)
      return Main_Instrumentation_Description;
   --  Given a rewriter for the main source, return a description of the main
   --  unit in which to trigger the dump of coverage buffers.
   --
   --  Emit a warning and raise a Cannot_Instrument_Main_Error if the main does
   --  not have a structure that is expected for a main.

   procedure Stop_Probe_Main (Unit : Analysis_Unit; Message : String)
   with No_Return;
   --  Emit a warning with the given message and raise a
   --  Cannot_Instrument_Main_Error exception.

   procedure Expand_Main_Generic_Instantiation
     (Main                          : Generic_Subp_Instantiation;
      Prj                           : Prj_Desc;
      Generic_Wrapper_Body_Filename : out Unbounded_String;
      Generic_Wrapper_Body          : out Node_Rewriting_Handle;
      Prelude                       : out Node_Rewriting_Handle;
      Main_Decls                    : out Node_Rewriting_Handle;
      Main_Stmts                    : out Node_Rewriting_Handle);
   --  Assuming that Main is a generic procedure instantiation acting as a main
   --  for the project:
   --
   --    procedure [Main] is new ...
   --
   --  We need a procedure body in order to insert dumps for coverage buffers.
   --  We also need Main to still be a spec-only unit, so that the GPR clause
   --  "for Main use ("[main].ads");" stays valid.  To satisfy all these
   --  constraints, this procedure does the following steps:
   --
   --  1. Move the instantiation to a new unit:
   --
   --     procedure Xcov_Wrapped_[Main] is new ...
   --
   --  2. Create a generic procedure (in which to insert dumps) wrapper for it:
   --
   --     [spec]
   --     generic
   --     procedure Xcov_Genwrap_[Main];
   --
   --     [body]
   --     with Xcov_Wrapped_[Main];
   --
   --     procedure Xcov_Genwrap_[Main] is
   --     begin
   --        Xcov_Wrapped_[Main];
   --     end Xcov_Genwrap_[Main];
   --
   --  3. Replace the original main spec with the following instantiation:
   --
   --     with Xcov_Genwrap_[Main];
   --
   --     procedure [Main] is new Xcov_Genwrap_[Main];
   --
   --  All new sources but the body of Xcov_Genwrap_[Main] are written to files
   --  in Info's output directory. Nodes for the body are put into
   --  Generic_Wrapper_Body/Main_Decls/Main_Stmts, and the filename where to
   --  write it is assigned to Generic_Wrapper_Body_Filename.

   function Simple_Dump_Proc_Call
     (RH : Rewriting_Handle; Helper_Unit : Ada_Qualified_Name)
      return Node_Rewriting_Handle;
   --  Assuming that RH is the rewriting handle for the main to instrument in
   --  main-end mode and that Helper_Unit is the unit that contains the dump
   --  procedure, return a call statement node for this dump procedure.

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
      Decls       : Node_Rewriting_Handle);
   --  Assuming that Decls is a rewriting handle for the declaration list of a
   --  subprogram body, insert at the beginning of it the declaration of a
   --  controlled object of type <Helper_Unit>.Dump_Controlled_Type to dump the
   --  coverage buffers during finalization of said object.

   --------------------------
   -- Unit instrumentation --
   --------------------------

   function Buffers_List_Unit
     (Project_Name : Ada_Qualified_Name) return Ada_Qualified_Name;
   --  Returns the name of the unit containing the array of coverage buffers.
   --  It is named after the given project main name (e.g. if the
   --  project p.gpr, its name is <Sys_Prefix>.<Slug for P>).

   function Buffer_Unit
     (Unit_Name : Ada_Qualified_Name) return Ada_Qualified_Name;
   --  Given a file to instrument, return the name of the unit that holds
   --  its coverage buffers (Coverage_Buffer_Type and
   --  GNATcov_RTS_Coverage_Buffers records).

   function Pure_Buffer_Unit
     (Unit_Name : Ada_Qualified_Name) return Ada_Qualified_Name;
   --  Given a unit to instrument, return the name of the unit that holds
   --  addresses to its coverage buffers.

   procedure Emit_Buffer_Unit
     (Buffer_Unit : Compilation_Unit_Part;
      Prj         : Prj_Desc;
      Unit        : Files_Table.Compilation_Unit;
      Unit_Bits   : Allocated_Bits_Vectors.Vector;
      CU_Names    : CU_Name_Vectors.Vector;
      CUs         : CU_Id_Vectors.Vector);
   --  Emit the unit to contain coverage buffers for the given instrumented
   --  unit.

   procedure Emit_Pure_Buffer_Unit
     (PB_Unit                        : Compilation_Unit_Part;
      Prj                            : Prj_Desc;
      CU_Names                       : CU_Name_Vectors.Vector;
      Language_Version               : Unbounded_Wide_Wide_String;
      Degenerate_Subprogram_Generics : Generic_Subp_Vectors.Vector;
      Has_No_Elaboration_Code_All    : Boolean);
   --  Emit the unit to contain addresses for the coverage buffers of all of
   --  the compilation unit parts in CU_Names. PB_Unit holds the name of
   --  the pure buffer unit, which is generated in the output dir specified in
   --  the project description Prj.
   --
   --  See the documentation of the eponym fields of the Ada_Unit_Inst_Context
   --  record for the following formals:
   --
   --  * Language_Version
   --  * Degenerate_Subprogram_Generics
   --  * Has_No_Elaboration_Code_All

   function Create_Manual_Helper_Unit_Name
     (Prj : Prj_Desc) return Ada_Qualified_Name;
   --  Return the name for the dump helper unit for manual dump trigger

   procedure Emit_Dump_Helper_Unit_For_Trigger
     (Dump_Config    : Any_Dump_Config;
      Dump_Trigger   : Valid_Dump_Trigger;
      Instrumenter   : Ada_Instrumenter_Type'Class;
      Prj            : Prj_Desc;
      Main           : Compilation_Unit_Part;
      Helper_Unit    : out Ada_Qualified_Name;
      Has_Controlled : Boolean := False);
   --  Emit the unit to contain helpers to implement the automatic dump of
   --  coverage buffers for the given Main unit. Prj must contain information
   --  about the project that owns this main. Upon return, the name of this
   --  helper unit is stored in Helper_Unit.
   --
   --  The generated code provides the required interface for the given
   --  Dump_Trigger (the dump trigger information held in Dump_Config is not
   --  read).
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
               To_Type.P_Top_Level_Decl (To_Type.Unit)
                 .P_Canonical_Fully_Qualified_Name);
            To_Type_Indentifier :=
              Make_Identifier
                (IC.Rewriting_Context,
                 To_Type.P_Canonical_Fully_Qualified_Name);
         else
            --  The Standard package may be hidden (and the Boolean type might
            --  very well be). To avoid issues, we have an accessible package
            --  that renames Standard in GNATcov_RTS.

            To_Type_Indentifier := Make_Std_Ref (IC, "Boolean");
         end if;

         return
           Create_Call_Expr
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
        & ","
        & Img (Bits.Outcome_Bits (False))
        & ","
        & Img (Bits.Outcome_Bits (True))
        & (if Is_MCDC
           then
             ", {}" & ", " & Img (Bits.Path_Bits_Base) & ", " & (+MCDC_State)
           else "")
        & ")";

      RH_Call : constant Node_Rewriting_Handle :=
        Create_From_Template
          (IC.Rewriting_Context,
           Template  => To_Wide_Wide_String (Call_Img),
           Arguments =>
             (1 => E.Common_Buffers, 2 => E.Decision_Buffer)
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

      Insert_Last (Child (RH_Call, Member_Refs.Call_Expr_F_Suffix), D);
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
        "{}.Witness ("
        & (+MCDC_State)
        & ","
        & Img (Offset)
        & ","
        & First'Img
        & ")";

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

      Insert_Last
        (Child (RH_Call, Member_Refs.Call_Expr_F_Suffix),
         Convert_To (IC, C_Type, B_Type, RH_Cond));
      return Convert_To (IC, B_Type, C_Type, RH_Call);
   end Make_Condition_Witness;

   ----------------------------
   -- Make_Statement_Witness --
   ----------------------------

   function Make_Statement_Witness
     (UIC          : Ada_Unit_Inst_Context;
      Bit          : Bit_Id;
      Flavor       : Statement_Witness_Flavor;
      In_Generic   : Boolean;
      In_Decl_Expr : Boolean) return Node_Rewriting_Handle
   is
      Bit_Img : constant String := Img (Bit);
      E       : Instrumentation_Entities renames UIC.Entities;

      function Call_Img return String
      is ("{}.Witness ({}, "
          & Bit_Img
          & ")"
          & (if Flavor = Function_Call then "" else ";"));

      --  Note: package spec and package body are instrumented separately,
      --  so we need to make sure that variables declared in a body can't
      --  clash with those from the corresponding spec, hence the inclusion
      --  of the unit part in the variable name.

      function Decl_Img return String
      is ("Discard_"
          & UIC.Instrumented_Unit.Part'Img
          & Bit_Img
          & " :"
          & (if In_Decl_Expr then " constant" else "")
          & " {}."
          & (if In_Generic and then Switches.SPARK_Compat
             then "Non_Volatile_"
             else "")
          & "Witness_Dummy_Type := "
          & Call_Img);

      --  Start of processing for Make_Statement_Witness

   begin
      if Flavor = Declaration then
         return
           Create_From_Template
             (UIC.Rewriting_Context,
              Template  => To_Wide_Wide_String (Decl_Img),
              Arguments =>
                (1 | 2 => E.Common_Buffers, 3 => E.Statement_Buffer),
              Rule      => Object_Decl_Rule);
      else
         return
           Create_From_Template
             (UIC.Rewriting_Context,
              Template  => To_Wide_Wide_String (Call_Img),
              Arguments => (E.Common_Buffers, E.Statement_Buffer),
              Rule      =>
                (if Flavor = Procedure_Call
                 then Call_Stmt_Rule
                 else Name_Rule));
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
        Create_Node (IC.Rewriting_Context, Libadalang.Common.Ada_Identifier);
      RH_N : Node_Rewriting_Handle;

   begin
      --  No instrumentation for condition if there is no local state variable

      if SC.State = "" then
         return;
      end if;

      --  Special case of conditional, quantified and declare expressions: we
      --  need to move them along with their enclosing parentheses, if they
      --  exist. Otherwise, add the needed parenthesis.

      if Expr_Needs_Parens (N.Kind) and then Kind (N.Parent) = Ada_Paren_Expr
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
        (RH_P, Make_Condition_Witness (IC, SC.State, RH_N, Offset, SC.First));
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
        Create_Node (IC.Rewriting_Context, Libadalang.Common.Ada_Identifier);

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

      Replace (RH_P, Make_Decision_Witness (IC, Bits, SD.State, RH_N));
   end Insert_Decision_Witness;

   -----------------
   -- Ensure_With --
   -----------------

   procedure Ensure_With
     (UIC : in out Ada_Unit_Inst_Context'Class; Unit : Text_Type)
   is
      RH : Rewriting_Handle renames UIC.Rewriting_Context;
   begin
      if UIC.Withed_Units.Contains (Unit) then
         return;
      end if;

      Insert_Last
        (Handle (UIC.Root_Unit.F_Prelude),
         Create_From_Template
           (RH,
            Template  => "with " & Unit & ";",
            Arguments => (1 .. 0 => No_Node_Rewriting_Handle),
            Rule      => With_Clause_Rule));

      UIC.Withed_Units.Include (Unit);
   end Ensure_With;

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
        Name
        & "_Var :"
        & (if UIC.In_Decl_Expr then " constant" else "")
        & " {}.MCDC_State_Type := "
        & (if UIC.In_Decl_Expr
           then To_Ada (Sys_Buffers) & ".Identity (0);"
           else "0;");
      Addr_Decl_Img : constant String :=
        Name
        & " : constant GNATCov_RTS.Sys.Address := "
        & Name
        & "_Var'Address;";

      Decl       : constant Node_Rewriting_Handle :=
        Create_From_Template
          (UIC.Rewriting_Context,
           Template  => To_Wide_Wide_String (Var_Decl_Img),
           Arguments => (1 => E.Common_Buffers),
           Rule      => Object_Decl_Rule);
      Rep_Clause : constant Node_Rewriting_Handle :=
        Create_From_Template
          (UIC.Rewriting_Context,
           Template  => To_Wide_Wide_String (Addr_Decl_Img),
           Arguments => (1 .. 0 => No_Node_Rewriting_Handle),
           Rule      => Object_Decl_Rule);
   begin
      Insert_First (Inserter.Local_Decls, Rep_Clause);
      Insert_First (Inserter.Local_Decls, Decl);
      return Name;
   end Insert_MCDC_State;

   ---------------------------------
   -- Require_MCDC_State_Inserter --
   ---------------------------------

   function Require_MCDC_State_Inserter
     (UIC      : in out Ada_Unit_Inst_Context'Class;
      E        : Expr'Class;
      Inserter : aliased in out Default_MCDC_State_Inserter'Class)
      return Boolean
   is
      ERH : Node_Rewriting_Handle;
      --  Node rewriting handle for E

      Decl_Expr, Decl_List : Node_Rewriting_Handle;
      --  Declare expression (and list of declarations) to hold the above list

      Paren_Expr : Node_Rewriting_Handle;
      --  Paren expression to wrap the decl expression (needed for correct
      --  syntax).
   begin
      if UIC.MCDC_State_Inserter /= null then
         return True;
      end if;

      if UIC.Language_Version not in Decl_Expr_Supported_Versions then
         Report
           (UIC,
            E,
            "gnatcov limitation: cannot find local declarative part for MC/DC;"
            & " consider switching to Ada 2022: declare expressions allow to"
            & " lift this limitation",
            Kind => Diagnostics.Error);
         return False;
      end if;

      --  Create the decl expression to wrap the expression and replace the
      --  original expression with it.

      ERH := Handle (E);
      Decl_List := Create_Node (UIC.Rewriting_Context, Ada_Ada_Node_List);
      Decl_Expr :=
        Create_Decl_Expr
          (Handle  => UIC.Rewriting_Context,
           F_Decls => Decl_List,
           F_Expr  => No_Node_Rewriting_Handle);
      Paren_Expr := Create_Paren_Expr (UIC.Rewriting_Context, Decl_Expr);
      Replace (ERH, Paren_Expr);
      Set_Child (Decl_Expr, Member_Refs.Decl_Expr_F_Expr, ERH);

      --  Update instrumentation data structures so that new declarations go
      --  in this decl expression.

      Inserter.Local_Decls := Decl_List;
      UIC.MCDC_State_Inserter := Inserter'Unchecked_Access;
      UIC.In_Decl_Expr := True;
      return True;
   end Require_MCDC_State_Inserter;

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
      Insert_Info : Insertion_Info renames UIC.Current_Insertion_Info.Get;
   begin
      return Result : Degenerate_Subp_Common_Nodes do
         Result.N := N;
         Result.N_Spec := N_Spec;
         Result.N_Overriding :=
           (if Kind (N) = Ada_Subp_Decl
            then N.As_Subp_Decl.F_Overriding
            else N.As_Base_Subp_Body.F_Overriding);
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
           Make_Identifier (UIC.Rewriting_Context, Gen_Names_Prefix & "Pkg");

         Result.Wrapper_Pkg_Decls :=
           Create_Regular_Node (RC, Ada_Ada_Node_List, No_Children);

         Result.Wrapper_Pkg :=
           Create_Package_Decl
             (RC,
              F_Package_Name => Result.Wrapper_Pkg_Name,
              F_Aspects      => No_Node_Rewriting_Handle,
              F_Public_Part  =>
                Create_Public_Part (RC, F_Decls => Result.Wrapper_Pkg_Decls),
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
      Nodes.Name :=
        Make_Identifier (UIC.Rewriting_Context, Gen_Names_Prefix & "Gen");

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
           F_Formal_Part =>
             Create_Generic_Formal_Part (RC, F_Decls => Nodes.Formals),
           F_Subp_Decl   =>
             Create_Generic_Subp_Internal
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
         --
         --   As a workaround for a GNAT bug (see eng/toolchain/gnat#1048), we
         --   also need to turn X'Class into [formalX]'Class.

         case TE.Kind is
            when Ada_Anonymous_Type     =>
               declare
                  TD : constant Type_Def :=
                    TE.As_Anonymous_Type.F_Type_Decl.F_Type_Def;
               begin
                  --  There are two kinds of anonymous types: "simple" access
                  --  types, and access to subprogram types.

                  case TD.Kind is
                     when Ada_Type_Access_Def    =>
                        return
                          Gen_Type_Expr_For_Simple_Access_Type
                            (TD.As_Type_Access_Def);

                     when Ada_Access_To_Subp_Def =>
                        return
                          Gen_Type_Expr_For_Access_To_Subp
                            (TD.As_Access_To_Subp_Def);

                     when others                 =>
                        raise Program_Error
                          with
                            "unexpected anonymous type definition: "
                            & TD.Kind'Image;
                  end case;
               end;

            when Ada_Subtype_Indication =>
               declare
                  SI : constant Subtype_Indication := TE.As_Subtype_Indication;
               begin
                  if Referenced_Attribute (SI.F_Name) = "class" then
                     declare
                        Result      : constant Node_Rewriting_Handle :=
                          Clone (Handle (SI));
                        Attr_Prefix : constant Node_Rewriting_Handle :=
                          Child
                            (Result,
                             (Member_Refs.Subtype_Indication_F_Name,
                              Member_Refs.Attribute_Ref_F_Prefix));
                     begin
                        Replace (Attr_Prefix, Make_Formal_Type (SI));
                        return Result;
                     end;
                  end if;
               end;

            when others                 =>
               null;
         end case;

         return Make_Formal_Type (TE);
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
         Ctrl_Type        : Base_Type_Decl renames Common_Nodes.Ctrl_Type;
         Is_Controlling   : constant Boolean :=
           (if Ctrl_Type.Is_Null
            then False
            else
              Formal_Subt_Decl = Ctrl_Type
              or else Formal_Subt_Decl = Ctrl_Type.P_Full_View);
         Has_Not_Null     : constant Node_Rewriting_Handle :=
           (if Is_Controlling
            then Make (UIC, Ada_Not_Null_Present)
            else Clone (Access_Def.F_Has_Not_Null));
      begin
         return
           Make_Anonymous_Type_Decl
             (Create_Type_Access_Def
                (RC,
                 F_Has_Not_Null       => Has_Not_Null,
                 F_Has_All            => No_Node_Rewriting_Handle,
                 F_Has_Constant       => Clone (Access_Def.F_Has_Constant),
                 F_Subtype_Indication =>
                   Make_Formal_Type (Formal_Subtype_Indication)));
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
           (if Orig_Params.Is_Null then 0 else Orig_Params.Children_Count);

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
            New_Params (J) :=
              Gen_Proc_Param_For (Orig_Params.Child (J).As_Param_Spec);
         end loop;

         New_F_Subp_Params :=
           (if Param_Spec_Count > 0
            then
              Create_Params
                (RC, Create_Regular_Node (RC, Ada_Param_Spec_List, New_Params))
            else No_Node_Rewriting_Handle);

         --  Create its return type (if it is a function)

         New_Return_Type :=
           (if Subp_Kind = Ada_Subp_Kind_Function
            then Gen_Type_Expr (Orig_Spec.F_Subp_Returns)
            else No_Node_Rewriting_Handle);

         --  We can now create the whole subprogram spec, and then the
         --  anonymous type.

         Subp_Spec :=
           Create_Subp_Spec
             (RC,
              F_Subp_Kind    => Create_Node (RC, Subp_Kind),
              F_Subp_Name    => No_Node_Rewriting_Handle,
              F_Subp_Params  => New_F_Subp_Params,
              F_Subp_Returns => New_Return_Type);

         return
           Make_Anonymous_Type_Decl
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

         Insert_Last
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

                         F_Has_Limited  => Make (UIC, Ada_Limited_Present)),

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
            Insert_Last (NP_Nodes.Inst_Params, Clone (Actual));
         end;

         --  Return a reference to this formal

         return Make_Identifier (UIC.Rewriting_Context, Formal_Type_Name);
      end Make_Formal_Type;

      --  Start of processing for Collect_Null_Proc_Formals

   begin
      --  Process all formals (there is nothing to do if there is none)

      if Common_Nodes.N_Params.Is_Null then
         return;
      end if;
      for J in 1 .. Common_Nodes.N_Params.Children_Count loop
         Insert_Last
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
      Renaming_Decl : out Node_Rewriting_Handle;
      Fun_Witness   : Node_Rewriting_Handle)
   is
      RC : Rewriting_Handle renames UIC.Rewriting_Context;
      E  : Instrumentation_Entities renames UIC.Entities;
   begin
      --  Create the generic subprogram body

      if Fun_Witness /= No_Node_Rewriting_Handle then
         --  If the Fun_Witness is a valid witness call, insert it at the
         --  beginning of the generic subprogram.
         Insert_First (NP_Nodes.Stmt_List, Fun_Witness);
      end if;

      Subp_Body :=
        Create_Subp_Body
          (RC,
           F_Overriding => No_Node_Rewriting_Handle,
           F_Subp_Spec  => Clone (NP_Nodes.Subp_Spec),
           F_Aspects    => No_Node_Rewriting_Handle,

           F_Decls      =>
             Create_Declarative_Part
               (RC, F_Decls => Make (UIC, Ada_Ada_Node_List)),

           F_Stmts      =>
             Create_Handled_Stmts
               (RC,
                F_Stmts      => NP_Nodes.Stmt_List,
                F_Exceptions => No_Node_Rewriting_Handle),
           F_End_Name   => No_Node_Rewriting_Handle);

      --  Create an instantiation for this generic subprogram

      Instance :=
        Create_Generic_Subp_Instantiation
          (RC,
           F_Overriding        => No_Node_Rewriting_Handle,
           F_Kind              => Make (UIC, Ada_Subp_Kind_Procedure),
           F_Subp_Name         =>
             Make_Defining_Name (UIC, Text (Common_Nodes.N_Name)),
           F_Generic_Subp_Name =>
             Create_Dotted_Name
               (RC,
                F_Prefix => Clone (E.Unit_Buffers),
                F_Suffix => Clone (NP_Nodes.Name)),
           F_Params            => NP_Nodes.Inst_Params,
           F_Aspects           => No_Node_Rewriting_Handle);

      --  Finally, create the declaration that renames the instantiated generic
      --  subprogram.

      Renaming_Decl :=
        Create_Subp_Renaming_Decl
          (RC,
           F_Subp_Spec  => Clone (Common_Nodes.N_Spec),
           F_Overriding => Clone (Common_Nodes.N_Overriding),

           F_Renames    =>
             Create_Renaming_Clause
               (RC,
                F_Renamed_Object =>
                  Create_Dotted_Name
                    (RC,
                     F_Prefix => Clone (Common_Nodes.Wrapper_Pkg_Name),
                     F_Suffix => Clone (Common_Nodes.N_Name))),

           F_Aspects    => No_Node_Rewriting_Handle);
   end Complete_Null_Proc_Decls;

   ------------------
   -- Clone_Params --
   ------------------

   function Clone_Params
     (UIC : Ada_Unit_Inst_Context; N_Spec : Subp_Spec)
      return Node_Rewriting_Handle
   is
      P : constant Params := N_Spec.F_Subp_Params;
   begin
      return
        (if P.Is_Null
         then Make (UIC, Ada_Param_Spec_List)
         else Clone (P.F_Params));
   end Clone_Params;

   -----------------------
   -- Insert_MCDC_State --
   -----------------------

   overriding
   function Insert_MCDC_State
     (Inserter : in out Expr_Func_MCDC_State_Inserter;
      UIC      : in out Ada_Unit_Inst_Context'Class;
      Name     : String) return String
   is
      RC : Rewriting_Handle renames UIC.Rewriting_Context;

      Holder_Type : constant Wide_Wide_String :=
        "GNATcov_RTS.Buffers.MCDC_State_Holder";

      State_Identifier : constant Node_Rewriting_Handle :=
        Make_Identifier (UIC.Rewriting_Context, To_Wide_Wide_String (Name));

      State_Formal : constant Node_Rewriting_Handle :=
        Create_Defining_Name (RC, State_Identifier);

      State_Param_Spec : constant Node_Rewriting_Handle :=
        Create_Param_Spec
          (RC,
           F_Ids          =>
             Create_Regular_Node
               (RC, Ada_Defining_Name_List, Children => (1 => State_Formal)),
           F_Has_Aliased  => No_Node_Rewriting_Handle,
           F_Mode         => No_Node_Rewriting_Handle,
           F_Type_Expr    =>
             Make_Identifier (UIC.Rewriting_Context, Holder_Type),
           F_Default_Expr => No_Node_Rewriting_Handle,
           F_Aspects      => No_Node_Rewriting_Handle);

      State_Actual : constant Node_Rewriting_Handle :=
        Create_Qual_Expr
          (RC,
           F_Prefix => Make_Identifier (UIC.Rewriting_Context, Holder_Type),
           F_Suffix =>
             Create_Aggregate
               (RC,
                F_Ancestor_Expr => No_Node_Rewriting_Handle,
                F_Assocs        =>
                  Create_Regular_Node
                    (RC,
                     Kind     => Ada_Assoc_List,
                     Children =>
                       (1 =>
                          Create_Aggregate_Assoc
                            (RC,
                             F_Designators =>
                               Create_Regular_Node
                                 (RC,
                                  Ada_Alternatives_List,
                                  (1 =>
                                     Create_Regular_Node
                                       (RC,
                                        Ada_Others_Designator,
                                        No_Children))),
                             F_R_Expr      =>
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

      Insert_Last (Inserter.Formal_Params, State_Param_Spec);
      Insert_Last (Inserter.Call_Params, State_Actual);

      return Name & ".State'Address";
   end Insert_MCDC_State;

   --------------------------------------
   -- Instrument_For_Function_Coverage --
   --------------------------------------

   procedure Instrument_For_Function_Coverage
     (UIC            : in out Ada_Unit_Inst_Context;
      Spec           : Subp_Spec;
      Witness_Flavor : Statement_Witness_Flavor;
      Fun_Witness    : out Node_Rewriting_Handle)
   is
      Loc_Range : constant Source_Location_Range := Sloc_Range (Spec);
   begin
      --  Add a function coverage SCO to the specification Spec

      Append_SCO
        (C1                 => 'c',
         C2                 => 'F',
         From               => +Start_Sloc (Loc_Range),
         To                 => +Inclusive_End_Sloc (Loc_Range),
         SFI                => UIC.SFI,
         Last               => True,
         Pragma_Aspect_Name => Namet.No_Name);

      --  Exit early if the context disallows instrumentation

      if UIC.Disable_Instrumentation then
         UIC.Non_Instr_LL_SCOs.Include (SCO_Id (SCOs.SCO_Table.Last));
         Fun_Witness := No_Node_Rewriting_Handle;
         return;
      end if;

      --  Set Fun_Witness to a statement witness of flavor Witness_Flavor and
      --  responsible for discharging the function SCO we just created.

      Fun_Witness :=
        Make_Statement_Witness
          (UIC,
           Bit          =>
             Allocate_Statement_Bit (UIC.Unit_Bits, SCOs.SCO_Table.Last),
           Flavor       => Witness_Flavor,
           In_Generic   => UIC.In_Generic,
           In_Decl_Expr => UIC.In_Decl_Expr);
   end Instrument_For_Function_Coverage;

   -------------------------------
   -- Create_Augmented_Function --
   -------------------------------

   procedure Create_Augmented_Function
     (UIC                     : Ada_Unit_Inst_Context;
      Common_Nodes            : Degenerate_Subp_Common_Nodes;
      Formal_Params           : Node_Rewriting_Handle;
      Call_Params             : Node_Rewriting_Handle;
      Augmented_Function      : out Node_Rewriting_Handle;
      Augmented_Function_Decl : out Node_Rewriting_Handle;
      New_Function            : out Node_Rewriting_Handle;
      Needs_Aspects           : Boolean := False)
   is
      RC : Rewriting_Handle renames UIC.Rewriting_Context;

      --  Compute the name of the augmented function

      Orig_Name_Text : constant Wide_Wide_String := Text (Common_Nodes.N_Name);
      Is_Op_Symbol   : constant Boolean :=
        Orig_Name_Text (Orig_Name_Text'First) = '"';

      Fun_Cov : constant Boolean := Enabled (Fun_Call);
      --  True if function coverage is needed

      Augmented_Func_Name : constant Wide_Wide_String :=
        (if Is_Op_Symbol
         then Op_Symbol_To_Name (Common_Nodes.N_Name) & "_Op"
         else Orig_Name_Text)
        & (if not Fun_Cov then "_With_State_" else "_")
        & To_Wide_Wide_String (Img (UIC.Degenerate_Subprogram_Index))
        & (if Fun_Cov then "_GNATCOV_Aux" else "");

      Need_WP : constant Boolean :=
        Augmented_EF_Needs_Wrapper_Package (Common_Nodes);

      --  Create the expression for New_Expr_Function that will call that
      --  augmented expression function.

      Callee : constant Node_Rewriting_Handle :=
        (if Need_WP
         then
           Create_Dotted_Name
             (RC,
              F_Prefix => Clone (Common_Nodes.Wrapper_Pkg_Name),
              F_Suffix => Make_Identifier (RC, Augmented_Func_Name))
         else Make_Identifier (RC, Augmented_Func_Name));

      Call_Expr : constant Node_Rewriting_Handle :=
        (if Call_Params = No_Node_Rewriting_Handle
         then Callee
         else
           Create_Call_Expr (RC, F_Name => Callee, F_Suffix => Call_Params));

      --  No need for a declaration if we are using a nested package

      Needs_Decl : constant Boolean :=
        Common_Nodes.N.Kind = Ada_Expr_Function
        and then not Need_WP
        and then
          Augmented_Expr_Function_Needs_Decl (Common_Nodes.N.As_Expr_Function);

      Orig_Aspects : constant Aspect_Spec := Common_Nodes.N.F_Aspects;
   begin
      --  Create the new augmented function. Attach the original aspects to
      --  it if necessary, i.e. if they were not already attached to a previous
      --  decalration of New_Function.

      New_Function :=
        Create_Expr_Function
          (RC,
           F_Overriding => Clone (Common_Nodes.N_Overriding),
           F_Subp_Spec  => Clone (Common_Nodes.N_Spec),
           F_Expr       => Create_Paren_Expr (RC, Call_Expr),
           F_Aspects    =>
             (if Orig_Aspects /= No_Ada_Node and then Needs_Aspects
              then Detach (Orig_Aspects)
              else No_Node_Rewriting_Handle));

      --  The original function becomes the augmented one:

      Augmented_Function := Handle (Common_Nodes.N);

      --  overriding keyword is purely optional, so there is no drawback in
      --  removing it.

      Replace
        (Handle (Common_Nodes.N_Overriding),
         Create_Node (RC, Ada_Overriding_Unspecified));

      --  Replace its name with the new one

      Replace
        (Handle (Common_Nodes.N_Name),
         Make_Identifier (UIC.Rewriting_Context, Augmented_Func_Name));

      --  Use the "augmented formal params" (i.e. original formals plus the
      --  witness one and the MC/DC state holders).

      if Formal_Params /= No_Node_Rewriting_Handle then
         Set_Child
           (Handle (Common_Nodes.N_Spec),
            Member_Refs.Subp_Spec_F_Subp_Params,
            Create_Params (RC, Formal_Params));
      end if;

      --  If we also need a declaration for the augmented expression
      --  function, create it. Otherwise, set it to No_Node_Rewriting_Handle.

      if Needs_Decl then
         declare
            --  If the augmented EF needs to have a previous declaration, then
            --  it should be based on the previous declaration of the original
            --  EF to avoid potential visibility issues introduced by
            --  use-clauses in between the declaration and the completion.
            --
            --  Note that Needs_Decl can be true True only when
            --  Augmented_Function_Needs_Decl returns True, and that can
            --  happen only when it managed to get the previous part of
            --  Common_Node.N, so the call to P_Previous_Part_For_Decl below is
            --  guaranteed to return a non-null node.

            Previous_Decl : constant Basic_Decl :=
              Common_Nodes.N.P_Previous_Part_For_Decl;
            pragma Assert (not Previous_Decl.Is_Null);

            Previous_Spec : constant Subp_Spec :=
              Previous_Decl.As_Subp_Decl.F_Subp_Spec;

            --  Clone the spec of the original declaration

            New_Spec : constant Node_Rewriting_Handle :=
              (if Fun_Cov
               then Handle (Previous_Spec)
               else Clone (Previous_Spec));
         begin

            --  Replace the original EF name by the augmented EF name
            if not Fun_Cov then
               Set_Child
                 (New_Spec,
                  Member_Refs.Subp_Spec_F_Subp_Name,
                  Make_Identifier
                    (UIC.Rewriting_Context, Augmented_Func_Name));
            end if;

            --  Add the augmented params to this spec as well

            if Formal_Params /= No_Node_Rewriting_Handle then
               declare
                  Spec_Formal_Params : constant Node_Rewriting_Handle :=
                    Clone_Params (UIC, Previous_Spec);

                  Current_Augmented_Param : Node_Rewriting_Handle :=
                    First_Child (Formal_Params);
               begin
                  --  Add all of the parameters inserted for instrumentation
                  --  purposes (the MCDC state, and the dummy witness
                  --  parameters).

                  for J in 1 .. Previous_Spec.P_Params'Length loop
                     Current_Augmented_Param :=
                       Next_Child (Current_Augmented_Param);
                  end loop;

                  while Current_Augmented_Param /= No_Node_Rewriting_Handle
                  loop
                     Insert_Last
                       (Spec_Formal_Params, Clone (Current_Augmented_Param));
                     Current_Augmented_Param :=
                       Next_Child (Current_Augmented_Param);
                  end loop;
                  Set_Child
                    (New_Spec,
                     Member_Refs.Subp_Spec_F_Subp_Params,
                     Create_Params (RC, Spec_Formal_Params));
               end;
            end if;

            Augmented_Function_Decl :=
              (if not Fun_Cov
               then
                 Create_Subp_Decl
                   (Handle       => UIC.Rewriting_Context,
                    F_Overriding => No_Node_Rewriting_Handle,
                    F_Subp_Spec  => New_Spec,
                    F_Aspects    => No_Node_Rewriting_Handle)
               else No_Node_Rewriting_Handle);

         exception
            when Exc : Property_Error =>
               Report
                 (Node => Common_Nodes.N,
                  Msg  =>
                    "Could not find previous declaration for the"
                    & " expression function: "
                    & Switches.Exception_Info (Exc),
                  Kind => Low_Warning);
         end;
      else
         Augmented_Function_Decl := No_Node_Rewriting_Handle;
      end if;

      --  If the original expression function is ghost, so must be the
      --  augmented one.

      if Is_Ghost
           (UIC,
            (if Common_Nodes.N.Kind = Ada_Expr_Function
             then Common_Nodes.N.As_Expr_Function.As_Basic_Decl
             else Common_Nodes.N.As_Basic_Decl))
      then
         declare
            Ghost_Aspect : constant Node_Rewriting_Handle :=
              Create_Aspect_Assoc
                (RC,
                 Make_Identifier (UIC.Rewriting_Context, "Ghost"),
                 No_Node_Rewriting_Handle);

            Aspects : constant Node_Rewriting_Handle :=
              Create_Regular_Node (RC, Ada_Aspect_Spec, (1 => Ghost_Aspect));
         begin
            if Needs_Decl then
               Set_Child
                 (Augmented_Function_Decl,
                  Member_Refs.Basic_Decl_F_Aspects,
                  Aspects);
            else
               Set_Child
                 (Handle (Common_Nodes.N),
                  Member_Refs.Basic_Decl_F_Aspects,
                  Aspects);
            end if;
         end;
      end if;

      if Need_WP then

         --  Put the augmented expression function in the wrapper package, and
         --  return its handle instead of the one of the expression function.

         Insert_Last (Common_Nodes.Wrapper_Pkg_Decls, Augmented_Function);

         Augmented_Function := Common_Nodes.Wrapper_Pkg;
      end if;

   end Create_Augmented_Function;

   ----------------------------------------
   -- Augmented_Expr_Function_Needs_Decl --
   ----------------------------------------

   function Augmented_Expr_Function_Needs_Decl
     (N : Expr_Function) return Boolean
   is
      Previous_Decl : Basic_Decl;
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
            Report
              (Node => N,
               Msg  =>
                 "Could not determine if expression function is a"
                 & " primitive: "
                 & Switches.Exception_Info (Exc),
               Kind => Warning);
            return False;
      end;

      --  Check that N has a previous declaration

      Previous_Decl := Safe_Previous_Part_For_Decl (N);
      if Previous_Decl.Is_Null then
         return False;
      end if;

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
            Report
              (Node => N,
               Msg  =>
                 "Could not determine the semantic parent of the"
                 & " expression function or the semantic parent of its"
                 & " previous declaration: "
                 & Switches.Exception_Info (Exc),
               Kind => Warning);
            return False;
      end;

      --  If all of the above conditions are met then the new expression
      --  needs a declaration.

      return True;
   end Augmented_Expr_Function_Needs_Decl;

   ----------------------------------------
   -- Augmented_EF_Needs_Wrapper_Package --
   ----------------------------------------

   function Augmented_EF_Needs_Wrapper_Package
     (Common_Nodes : Degenerate_Subp_Common_Nodes) return Boolean is
   begin
      return
        Common_Nodes.Ctrl_Type /= No_Base_Type_Decl
        and then not Common_Nodes.N_Spec.P_Return_Type.Is_Null
        and then Common_Nodes.N_Spec.P_Return_Type = Common_Nodes.Ctrl_Type;

   exception
      when Exc : Property_Error =>
         Report
           (Node => Common_Nodes.N,
            Msg  =>
              "Could not determine the return type of the"
              & " expression function: "
              & Switches.Exception_Info (Exc),
            Kind => Warning);
         return False;
   end Augmented_EF_Needs_Wrapper_Package;

   -------------------------
   -- Is_Self_Referencing --
   -------------------------

   function Is_Self_Referencing
     (UIC : Ada_Unit_Inst_Context; EF : Expr_Function) return Boolean
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
               & Switches.Exception_Info (Exc),
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
     (UIC : Ada_Unit_Inst_Context; Common_Nodes : Degenerate_Subp_Common_Nodes)
      return Boolean is
   begin
      if Common_Nodes.Ctrl_Type.Is_Null then
         return False;
      end if;

      --  Always compare the full views, to avoid an equality mismatch when
      --  e.g. comparing the full view against an incomplete view.

      return
        Common_Nodes.N_Spec.F_Subp_Returns.P_Designated_Type_Decl.P_Full_View
        = Common_Nodes.Ctrl_Type.P_Full_View;
   exception
      when Exc : Property_Error =>
         Report
           (UIC,
            Common_Nodes.N,
            "failed to determine return type of expression function: "
            & Switches.Exception_Info (Exc),
            Low_Warning);
         return False;
   end Return_Type_Is_Controlling;

   ------------------------------
   -- Has_Access_Attribute_Ref --
   ------------------------------

   function Has_Access_Attribute_Ref (E : Expr) return Boolean is

      function Process_Node (N : Ada_Node'Class) return Visit_Status;
      --  Helper for Libadalang.Analysis.Traverse. Return Stop if an access
      --  attribute reference is found, Into otherwise.

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node (N : Ada_Node'Class) return Visit_Status is
      begin
         if Referenced_Attribute (N)
            in "access" | "unchecked_access" | "unrestricted_access"
         then
            return Stop;
         end if;

         return Into;
      end Process_Node;

      --  Start of processing for Has_Access_Attribute_Ref

   begin
      return E.Traverse (Process_Node'Access) = Stop;
   end Has_Access_Attribute_Ref;

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
            when Ada_Subp_Body_Range
            =>
               return Parent_Basic_Decl.As_Subp_Body = Subp;

            when Ada_Accept_Stmt | Ada_Accept_Stmt_With_Stmts | Ada_Entry_Body
            =>

               --  A return statement may only appear within a callable
               --  construct (RM 6.5 (4/2)), which are either subprogram
               --  bodies, entry bodies or accept statements (RM 6.2)). As
               --  Subp is a Subp_Body if we encounter an accept statement
               --  or an entry body on the way we can't be returning from
               --  Subp.

               return False;

            when others
            =>
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
            Msg  =>
              "Unable to determine to which body this return statment"
              & "applies: "
              & Switches.Exception_Info (Exc),
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

   -----------------
   -- Parent_Decl --
   -----------------

   function Parent_Decl (Decl : Basic_Decl'Class) return Basic_Decl is
   begin
      return Decl.P_Parent_Basic_Decl;
   exception
      when Exc : Property_Error =>
         Report
           (Node => Decl,
            Msg  =>
              "Could not find the parent package: "
              & Switches.Exception_Info (Exc),
            Kind => Warning);
         return No_Basic_Decl;
   end Parent_Decl;

   -----------------------------
   -- Decls_Are_Library_Level --
   -----------------------------

   function Decls_Are_Library_Level
     (Unit : Libadalang.Analysis.Compilation_Unit) return Boolean is
   begin
      case Unit.F_Body.Kind is
         when Ada_Library_Item =>

            --  For library units, only packages (generic, declarations and
            --  bodies) contain library-level declarations.

            return
              Unit.F_Body.As_Library_Item.F_Item.Kind
              in Ada_Package_Decl
               | Ada_Package_Body
               | Ada_Generic_Package_Decl;

         when Ada_Subunit      =>

            --  We consider that declarations in subunits are library-level if
            --  the subunit is for a package body (in Ada, the only other valid
            --  case is a subprogram body) and if the stub for that package
            --  body is itself at library-level.

            declare
               Parent : Basic_Decl :=
                 Unit.F_Body.As_Subunit.F_Body.As_Basic_Decl;
            begin
               while not Parent.Is_Null loop
                  if Parent.Kind in Ada_Base_Subp_Body then
                     return False;
                  end if;
                  Parent := Parent_Decl (Parent);
               end loop;
               return True;
            end;

         when others           =>
            raise Program_Error;
      end case;
   end Decls_Are_Library_Level;

   --------------------
   -- Get_Convention --
   --------------------

   function Get_Convention (Decl : Basic_Decl) return Identifier is
      Convention_Aspect : constant Libadalang.Analysis.Aspect :=
        Decl.P_Get_Aspect (To_Unbounded_Text ("convention"));
      Export_Aspect     : constant Libadalang.Analysis.Aspect :=
        Decl.P_Get_Aspect (To_Unbounded_Text ("export"));
   begin
      if Exists (Convention_Aspect) then
         return Value (Convention_Aspect).As_Identifier;
      elsif Exists (Export_Aspect)
        and then Node (Export_Aspect).Kind = Ada_Pragma_Node
      then

         --  If the export aspect is present as an aspect, the convention will
         --  also be specified as a standalone aspect and/or pragma, so it will
         --  have been picked up by the previous branch.

         --  Either we have a named association ...

         for Assoc_Item of Node (Export_Aspect).As_Pragma_Node.F_Args loop
            declare
               Prag_Assoc : constant Pragma_Argument_Assoc :=
                 Assoc_Item.As_Pragma_Argument_Assoc;
            begin
               if not Prag_Assoc.F_Name.Is_Null
                 and then Prag_Assoc.F_Name.Kind in LALCO.Ada_Identifier
                 and then
                   As_Symbol (Prag_Assoc.F_Name.As_Identifier)
                   = Precomputed_Symbols (Convention)
               then
                  return Prag_Assoc.F_Expr.As_Identifier;
               end if;
            end;
         end loop;

         --  ... Otherwise for a positional association the convention comes
         --  first.

         declare
            First_Child : constant Ada_Node'Class :=
              Node (Export_Aspect).As_Pragma_Node.F_Args.Child (1);
         begin
            case First_Child.Kind is
               when Ada_Pragma_Argument_Assoc =>
                  return
                    First_Child.As_Pragma_Argument_Assoc.F_Expr.As_Identifier;

               when LALCO.Ada_Identifier      =>
                  return First_Child.As_Identifier;

               when others                    =>
                  Report
                    (Node => First_Child,
                     Msg  =>
                       "Unexpected kind for a convention name: "
                       & First_Child.Kind_Name,
                     Kind => Low_Warning);
                  return No_Identifier;
            end case;
         end;
      end if;

      return No_Identifier;
   end Get_Convention;

   ------------------------
   -- Process_Annotation --
   ------------------------

   procedure Process_Annotation
     (UIC       : in out Ada_Unit_Inst_Context;
      N         : Ada_Node;
      Prag_Args : Base_Assoc_List)
   is
      function Get_Arg
        (Prag_Args : Base_Assoc_List; I : Natural) return Symbol_Type
      is (if Prag_Arg_Expr (Prag_Args, I).Kind
            = Libadalang.Common.Ada_Identifier
          then As_Symbol (Prag_Arg_Expr (Prag_Args, I).As_Identifier)
          else No_Symbol);
      --  Attempt to get the pragma's Ith argument as an identifier. If
      --  it is not an identifier, return null. Else, return the identifier
      --  as a symbol.

      procedure Safe_String_Eval
        (E : Expr; Result : out Unbounded_Text_Type; Success : out Boolean);
      --  Evaluate the given Expr E and set Result to the evaluated string
      --  and Success to True if it could be evaluated, otherwise set
      --  Success to False.

      ----------------------
      -- Safe_String_Eval --
      ----------------------

      procedure Safe_String_Eval
        (E : Expr; Result : out Unbounded_Text_Type; Success : out Boolean)
      is
         use Libadalang.Expr_Eval;
      begin
         --  TODO??? Check for P_Is_Static_Expr prior to evaluating
         --  when eng/libadalang/libadalang#1359 is implemented instead of
         --  using exception handling.

         declare
            String_Expr_Eval : constant Eval_Result := Expr_Eval (E);
         begin
            if String_Expr_Eval.Kind /= String_Lit then
               Success := False;
               return;
            end if;
            Result := As_String (String_Expr_Eval);
            Success := True;
         end;
      exception
         when Property_Error =>
            Success := False;
      end Safe_String_Eval;

      Nb_Children : constant Natural := Prag_Args.Children_Count;
      Kind        : Symbol_Type;
      Result      : ALI_Annotation;

      --  Start of processing for Process_Annotation

   begin
      --  Ignore all but Xcov annotations

      if Get_Arg (Prag_Args, 1) /= As_Symbol (Xcov) then
         return;
      end if;

      --  Decode the annotation kind

      if Nb_Children = 1 then
         Report (N, "Xcov annotation kind missing", Warning);
         return;
      end if;

      Kind := Get_Arg (Prag_Args, 2);
      begin
         Result.Kind := Src_Annotation_Kind'Value (Image (Kind));
      exception
         when Constraint_Error =>
            Report
              (N,
               "Invalid Xcov annotation kind"
               & (if Kind /= No_Symbol then ": " & Image (Kind) else ""),
               Warning);
            return;
      end;

      --  Check whether the buffer annotations are in a statement sequence.
      --  If not, silently ignore it, it will be reported by the pass dedicated
      --  to them. Otherwise, end the statement block.

      if Result.Kind in Dump_Buffers | Reset_Buffers then
         if N.Parent.Kind in Ada_Stmt_List then
            End_Statement_Block (UIC);
            Start_Statement_Block (UIC);
         else
            return;
         end if;
      end if;

      --  Now that the annotation kind is known, validate the remaining
      --  arguments expected for that kind.

      case Result.Kind is
         when Exempt_On | Cov_Off =>

            --  Expected formats:
            --  * (Xcov, <Annotation_Kind>)
            --  * (Xcov, <Annotation_Kind>, "Justification")

            if Result.Kind = Cov_Off then
               UIC.Disable_Coverage := True;
            end if;
            case Nb_Children is
               when 2      =>
                  if Result.Kind = Exempt_On then
                     Report
                       (N,
                        "No justification given for exempted region",
                        Warning);
                  elsif Result.Kind = Cov_Off then
                     Report
                       (N,
                        "No justification given for disabled coverage region",
                        Warning);
                  end if;
                  UIC.Annotations.Append
                    (Annotation_Couple'((UIC.SFI, +Sloc (N)), Result));

               when 3      =>
                  declare
                     String_Value : Unbounded_Text_Type;
                     Success      : Boolean;
                  begin
                     Safe_String_Eval
                       (Prag_Arg_Expr (Prag_Args, 3), String_Value, Success);
                     if not Success then
                        Report
                          (N,
                           "Invalid justification argument: static string"
                           & " expression expected",
                           Warning);
                        return;
                     end if;
                     Result.Message :=
                       new String'(To_UTF8 (To_Text (String_Value)));

                     UIC.Annotations.Append
                       (Annotation_Couple'((UIC.SFI, +Sloc (N)), Result));
                  end;

               when others =>
                  Report (N, "At most 3 pragma arguments allowed", Warning);
                  return;
            end case;

         when Exempt_Off          =>
            if Nb_Children > 2 then
               Report (N, "At most 2 pragma arguments allowed", Warning);
               return;
            end if;
            UIC.Annotations.Append
              (Annotation_Couple'((UIC.SFI, +Sloc (N)), Result));

         when Cov_On              =>
            if Nb_Children > 2 then
               Report (N, "At most 2 pragma arguments allowed", Warning);
               return;
            end if;
            UIC.Disable_Coverage := False;
            UIC.Annotations.Append
              (Annotation_Couple'((UIC.SFI, +Sloc (N)), Result));

         when Dump_Buffers        =>

            --  Expected formats:
            --  * (Xcov, Dump_Buffers)
            --  * (Xcov, Dump_Buffers, Prefix)

            case Nb_Children is
               when 2 | 3  =>

                  --  TODO??? check that the Prefix expression is a string
                  --  type when eng/libadalang/libadalang#1360 is dealt
                  --  with.
                  null;

               when others =>
                  Report (N, "At most 3 pragma arguments allowed", Warning);
                  return;
            end case;

         when Reset_Buffers       =>
            if Nb_Children /= 2 then
               Report (N, "At most 2 pragma arguments allowed", Warning);
               return;
            end if;
      end case;
   end Process_Annotation;

   -----------------------------------------
   -- Traverse_Declarations_Or_Statements --
   -----------------------------------------

   procedure Traverse_Declarations_Or_Statements
     (UIC                        : in out Ada_Unit_Inst_Context;
      L                          : Ada_List'Class;
      Preelab                    : Boolean := False;
      P                          : Ada_Node := No_Ada_Node;
      Is_Select_Stmt_Alternative : Boolean := False;
      Priv_Part                  : Private_Part := No_Private_Part;
      Is_Block                   : Boolean := True);
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
   --  Is_Block indicates whether the statement list should be considered as
   --  a statement block or not.

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
     (UIC     : in out Ada_Unit_Inst_Context;
      N       : Generic_Package_Decl;
      Preelab : Boolean);

   procedure Traverse_Handled_Statement_Sequence
     (UIC : in out Ada_Unit_Inst_Context; N : Handled_Stmts);

   procedure Traverse_Package_Body
     (UIC : in out Ada_Unit_Inst_Context; N : Package_Body; Preelab : Boolean);

   procedure Traverse_Package_Declaration
     (UIC     : in out Ada_Unit_Inst_Context;
      N       : Base_Package_Decl;
      Preelab : Boolean);

   procedure Traverse_Subprogram_Or_Task_Body
     (UIC : in out Ada_Unit_Inst_Context; N : Body_Node'Class);

   procedure Traverse_Sync_Definition
     (UIC : in out Ada_Unit_Inst_Context; N : Ada_Node);
   --  Traverse a protected definition or task definition

   --  Note regarding traversals: In a few cases where an Alternatives list is
   --  involved, pragmas such as "pragma Page" may show up before the first
   --  alternative. We skip them because we're out of statement or declaration
   --  context, so these can't be pragmas of interest for SCO purposes, and
   --  the regular alternative processing typically involves attribute queries
   --  which aren't valid for a pragma.

   procedure Process_Expression
     (UIC : in out Ada_Unit_Inst_Context; N : Ada_Node'Class; T : Character);
   --  If N is Empty, has no effect. Otherwise scans the tree for the node N,
   --  to output any decisions it contains. T is one of IEGPWX (for context of
   --  expression: if/exit when/entry guard/pragma/while/expression). If T is
   --  other than X, the node N is the if expression involved, and a decision
   --  is always present (at the very least a simple decision is present at the
   --  top level).
   --
   --  This also processes any nested declare expressions.

   procedure Process_Standalone_Expression
     (UIC : in out Ada_Unit_Inst_Context; N : Ada_Node'Class; T : Character);
   --  Wrapper around Process_Expression that considers that the context does
   --  not provide a valid MCDC state inserter (for instance for contracts or
   --  formal/discriminant/component default expressions).
   --
   --  If the current Ada version allows it, it inserts a declare expression to
   --  host MCDC state variables. Otherwise, it temporarily removes the MCDC
   --  state inserter from UIC (just during the call to Process_Expression).

   procedure Process_Formal_Default_Exprs
     (UIC : in out Ada_Unit_Inst_Context; N_Spec : Subp_Spec);
   --  Process default expressions for formals in N_Spec

   function Is_Call_Leaf (Node : Ada_Node'Class) return Boolean;
   --  Return True if Node is the leaf of a call expression, that is there are
   --  no nodes in the children of Node identifying the same call.
   --
   --  This is used to avoid processing the same call multiple times when
   --  traversing the AST. E.g. when processing a dotted name A.B.C (..), both
   --  A.B.C (..) and C (..) designate the same call. As such, we should only
   --  process it once; we choose to do this on the inner-most node.

   function Full_Call (Node : Ada_Node'Class) return Ada_Node;
   --  Get the outter-most node that is this call

   --------------------------
   -- Internal Subprograms --
   --------------------------

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

   procedure Traverse_Declarations_Or_Statements
     (UIC                        : in out Ada_Unit_Inst_Context;
      L                          : Ada_List'Class;
      Preelab                    : Boolean := False;
      P                          : Ada_Node := No_Ada_Node;
      Is_Select_Stmt_Alternative : Boolean := False;
      Priv_Part                  : Private_Part := No_Private_Part;
      Is_Block                   : Boolean := True)
   is
      procedure Instrument_Statement
        (UIC         : in out Ada_Unit_Inst_Context;
         N           : Ada_Node'Class;
         Typ         : Character;
         Insertion_N : Node_Rewriting_Handle := No_Node_Rewriting_Handle);
      --  Instrument the statement N, or register it into the current block
      --  if block instrumentation is active.
      --
      --  Typ is the letter that identifies the type of statement/declaration
      --  that is being instrumented.
      --
      --  N is the original node from user code, and controls the source
      --  location assigned to the statement SCO.
      --
      --  In general, this is also where the witness statement is inserted, but
      --  in some rare cases, it needs to be inserted at a different place
      --  (case of a degenerated subprogram, which gets rewritten into a
      --  generic). In that case, Insertion_N indicates where to insert the
      --  witness.

      procedure Process_Contract
        (UIC  : in out Ada_Unit_Inst_Context;
         D    : Basic_Decl'Class;
         Name : Text_Type);
      --  If Assertion_Coverage_Enabled is False, do nothing. Otherwise,
      --  register decision of contrat of name Name of declaration node D

      procedure Traverse_One (N : Ada_Node);
      --  Traverse one declaration or statement

      procedure Traverse_Subp_Decl_Or_Stub (N : Basic_Decl);
      --  Common code to handle subprogram declarations and subprogram body
      --  stubs. Also calls Traverse_Degenerate_Subprograms for null procedures
      --  and expression functions.

      procedure Traverse_Degenerate_Subprogram
        (N : Basic_Decl; N_Spec : Subp_Spec);
      --  Additional specific processing for the case of degenerate
      --  subprograms (null procedures and expression functions).

      procedure Traverse_Component_List (CL : Component_List);
      --  Traverse a list of components (if a type declaration)

      ------------------------------------------
      -- Utility functions for node synthesis --
      ------------------------------------------

      RC : Rewriting_Handle renames UIC.Rewriting_Context;

      --------------------------
      -- Instrument_Statement --
      --------------------------

      procedure Instrument_Statement
        (UIC         : in out Ada_Unit_Inst_Context;
         N           : Ada_Node'Class;
         Typ         : Character;
         Insertion_N : Node_Rewriting_Handle := No_Node_Rewriting_Handle)
      is
         Instrument_Location : Instrument_Location_Type := Before;
         --  See the documentation of Instrument_Location_Type for more
         --  information.

         Dummy_Ctx : constant Context_Handle := Create_Context_Instrument (N);

         Is_Pragma          : constant Boolean := N.Kind = Ada_Pragma_Node;
         Pragma_Aspect_Name : constant Name_Id :=
           (if Is_Pragma
            then Pragma_Name (N.As_Pragma_Node)
            else Namet.No_Name);

         SR   : constant Source_Location_Range := N.Sloc_Range;
         From : Source_Location := Start_Sloc (SR);
         To   : Source_Location := Inclusive_End_Sloc (SR);
         --  Source location bounds used to produre a SCO statement. By
         --  default, this should cover the same source location range as N,
         --  however for nodes that can contain themselves other statements
         --  (for instance IF statements), we select an end bound that appears
         --  before the first nested statement (see To_Node below).

         To_Node : Ada_Node := No_Ada_Node;
         --  In the case of simple statements, set to No_Ada_Node and unused.
         --  Otherwise, use F and this node's end sloc for the emitted
         --  statement source location range.

         Actual_Insertion_N : constant Node_Rewriting_Handle :=
           (if Insertion_N = No_Node_Rewriting_Handle
            then Handle (N)
            else Insertion_N);

      begin
         if UIC.Disable_Coverage
           or else UIC.Is_Disabled_Region ((UIC.SFI, +From))
         then
            return;
         end if;
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
                     To_Node := Stmt.F_Body_Decl.F_Name.As_Ada_Node;
                  end if;
               end;

            when Ada_Case_Stmt                                =>
               To_Node := N.As_Case_Stmt.F_Expr.As_Ada_Node;

            when Ada_Elsif_Stmt_Part                          =>
               To_Node := N.As_Elsif_Stmt_Part.F_Cond_Expr.As_Ada_Node;
               Instrument_Location := Inside_Expr;

            when Ada_If_Stmt                                  =>
               To_Node := N.As_If_Stmt.F_Cond_Expr.As_Ada_Node;

            when Ada_Extended_Return_Stmt                     =>
               To_Node := N.As_Extended_Return_Stmt.F_Decl.As_Ada_Node;

            when Ada_Base_Loop_Stmt                           =>
               To_Node := N.As_Base_Loop_Stmt.F_Spec.As_Ada_Node;

            when Ada_Select_Stmt
               | Ada_Single_Protected_Decl
               | Ada_Single_Task_Decl                         =>
               To := From;

            when Ada_Protected_Type_Decl | Ada_Task_Type_Decl =>
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

            when Ada_Expr                                     =>
               To_Node := N.As_Ada_Node;

            when Ada_Null_Subp_Decl                           =>
               --  Special case: this SCO is for the fictitious NULL statement
               --  in a null procedure. The assigned sloc is that of the NULL
               --  token in the sequence "<last token of spec> IS NULL".

               declare
                  function NNT (TR : Token_Reference) return Token_Reference
                  is (Next (TR, Exclude_Trivia => True));
                  --  Next with no trivia (i.e. excluding whitespace/comment
                  --  tokens).

                  Null_Token : constant Token_Reference :=
                    NNT (NNT (N.As_Null_Subp_Decl.F_Subp_Spec.Token_End));
               begin
                  From := Start_Sloc (Sloc_Range (Data (Null_Token)));
               end;

            when others                                       =>
               null;
         end case;

         if not To_Node.Is_Null then
            To := Inclusive_End_Sloc (To_Node.Sloc_Range);
         end if;

         Instrument_Location :=
           --  See the comment attached to the declaration of the
           --  Instrument_Location_Type.

            (if Is_Select_Stmt_Alternative
               and then N = L.Children (L.Children'First)
             then
               (case N.Kind is
                  when Ada_Delay_Stmt | Ada_Call_Stmt => Before_Parent,
                  when others                         => After)
             else Instrument_Location);

         Append_SCO
           (C1                 => 'S',
            C2                 => Typ,
            From               => +From,
            To                 => +To,
            SFI                => UIC.SFI,
            Last               => True,
            Pragma_Aspect_Name => Pragma_Aspect_Name);

         --  Insert a witness call for this statement obligation
         --  unless...

         if
         --  ... there is no enclosing list to which a witness call
         --  can be attached.

                                UIC
                                .Current_Insertion_Info
                                .Get
                                .Method
           /= None

           --  ... this is a top-level declaration in a Preelaborate
           --  package.

           and then
             (UIC.Current_Insertion_Info.Get.Method
              not in Statement | Declaration
              or else not UIC.Current_Insertion_Info.Get.Preelab)

           --  ... this is a pragma that we know for certain will not
           --  generate code (such as Annotate or elaboration control
           --  pragmas).

           and then
             (not Is_Pragma
              or else
                Pragma_Might_Generate_Code
                  (Case_Insensitive_Get_Pragma_Id (Pragma_Aspect_Name)))

           --  ... this is a disabled pragma that we assume will not
           --  generate code.

           and then Typ /= 'p'
         then
            declare
               Bit : Any_Bit_Id;

               LL_SCO_Id : constant Nat := SCOs.SCO_Table.Last;
            begin
               --  Create an artificial internal error, if requested

               Raise_Stub_Internal_Error_For
                 (Ada_Instrument_Insert_Stmt_Witness);

               --  If the current code pattern is actually unsupported, do
               --  not even try to insert the witness call or allocate bits
               --  for it in the buffers. Mark the corresponding SCO as
               --  non-instrumented instead.

               if UIC.Disable_Instrumentation then
                  UIC.Non_Instr_LL_SCOs.Include (SCO_Id (LL_SCO_Id));
                  return;
               end if;

               --  If block instrumentation is enabled, insert a single witness
               --  call at the end of the block. This is handled by
               --  End_Statement_Block.

               if Switches.Instrument_Block then
                  declare
                     Current_Block : Block_Stacks.Reference_Type renames
                       UIC.Block_Stack.Reference (UIC.Block_Stack.Last_Index);
                  begin
                     Current_Block.Block.Append (SCO_Id (LL_SCO_Id));
                     Current_Block.Last_Stmt_Instr_Info.Insertion_N :=
                       Actual_Insertion_N;
                     Current_Block.Last_Stmt_Instr_Info.Instrument_Location :=
                       Instrument_Location;
                     Current_Block.Last_Stmt_Instr_Info.Insert_Info_Ref :=
                       UIC.Current_Insertion_Info;
                     Current_Block.Last_Stmt_Instr_Info.In_Decl_Expr :=
                       UIC.In_Decl_Expr;
                     return;
                  end;
               end if;

               Bit := Allocate_Statement_Bit (UIC.Unit_Bits, LL_SCO_Id);

               Insert_Stmt_Witness
                 (UIC             => UIC,
                  Stmt_Instr_Info =>
                    Stmt_Instr_Info_Type'
                      (Insertion_N         => Actual_Insertion_N,
                       Instrument_Location => Instrument_Location,
                       Insert_Info_Ref     => UIC.Current_Insertion_Info,
                       In_Decl_Expr        => UIC.In_Decl_Expr),
                  Bit             => Bit);
            end;
         end if;
      end Instrument_Statement;

      ----------------------
      -- Process_Contract --
      ----------------------

      procedure Process_Contract
        (UIC  : in out Ada_Unit_Inst_Context;
         D    : Basic_Decl'Class;
         Name : Text_Type)
      is
         Decision : Expr;
      begin
         --  If assertion coverage is not enabled, or if the contract is not
         --  present, there is nothing to do.

         if not Assertion_Coverage_Enabled then
            return;
         end if;

         Decision := P_Get_Aspect_Spec_Expr (D, To_Unbounded_Text (Name));
         if not Decision.Is_Null then

            --  For MCDC, we may need to create locals when instrumenting the
            --  decision: these locals cannot go to the scope that contains D.

            Process_Standalone_Expression (UIC, Decision, 'A');
         end if;
      end Process_Contract;

      ------------------------------------
      -- Traverse_Degenerate_Subprogram --
      ------------------------------------

      procedure Traverse_Degenerate_Subprogram
        (N : Basic_Decl; N_Spec : Subp_Spec)
      is
         Stub : constant Node_Rewriting_Handle :=
           Make_Identifier (UIC.Rewriting_Context, "Stub");
         --  Placeholder for the the degenerate subprogram node while it is
         --  rewritten.

         --  See the "Degenerate subprograms" comment section above for a
         --  description of the of transformation we implement in this
         --  procedure.

         Saved_Insertion_Info : constant Insertion_Info_Ref :=
           UIC.Current_Insertion_Info;
         --  Insertion info inherited from the caller, which "points" to the
         --  degenerate subprogram N. We "save" it because this procedure
         --  transiently changes UIC.Current_Insertion_Info.

         Saved_MCDC_State_Inserter : constant Any_MCDC_State_Inserter :=
           UIC.MCDC_State_Inserter;
         --  Likewise for MC/DC state inserter

         Fun_Cov : constant Boolean := Enabled (Fun_Call);
         --  True if function coverage is enabled. If True, the function will
         --  be instrumented for function coverage.

         procedure To_Regular_Subprogram
           (N           : Base_Subp_Body;
            Fun_Witness : Node_Rewriting_Handle := No_Node_Rewriting_Handle);
         --  Turns N into an instrumented regular function, by creating a
         --  function with the same subp_spec as the original expression
         --  function or null procedure, and a single return statement with the
         --  original expression, for expression function, or a null statement
         --  for null subprograms.
         --
         --  The SCO associated with the new single statement has the
         --  sloc of the whole original subprogram.

         procedure Create_Witness_Formal
           (Formal      : out Node_Rewriting_Handle;
            Formal_Name : Wide_Wide_String);
         --  Fill Formal with a new Node_Rewriting_Handle being a formal to
         --  receive a witness call.

         ---------------------------
         -- To_Regular_Subprogram --
         ---------------------------

         procedure To_Regular_Subprogram
           (N           : Base_Subp_Body;
            Fun_Witness : Node_Rewriting_Handle := No_Node_Rewriting_Handle)
         is
            Single_Stmt_RH : constant Node_Rewriting_Handle :=
              (if N.Kind = Ada_Expr_Function
               then
                 Create_Return_Stmt
                   (Handle        => UIC.Rewriting_Context,
                    F_Return_Expr => Detach (N.As_Expr_Function.F_Expr))
               else
                 Create_Node
                   (Handle => UIC.Rewriting_Context, Kind => Ada_Null_Stmt));
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
              Create_Node
                (Handle => UIC.Rewriting_Context, Kind => Ada_Decl_List);
            Proc_Decls     : constant Node_Rewriting_Handle :=
              Create_Declarative_Part
                (Handle => UIC.Rewriting_Context, F_Decls => Decl_List);

            II : aliased Insertion_Info (Statement);
            --  We need to change the insertion method to a statement insertion
            --  method as we are instrumenting a statement list with a single
            --  statement, and not a list of declarations.

            Local_Inserter : aliased Default_MCDC_State_Inserter;

         begin
            II.RH_List := Stmt_list_RH;
            II.Preelab := False;
            II.Parent :=
              Insertion_Info_Access (Saved_Insertion_Info.Unchecked_Get);

            Insertion_Info_SP.Set (UIC.Current_Insertion_Info, II);
            Local_Inserter.Local_Decls := Decl_List;
            UIC.MCDC_State_Inserter := Local_Inserter'Unchecked_Access;

            --  Add witness statement for the single statement

            Instrument_Statement
              (UIC => UIC, N => N, Typ => ' ', Insertion_N => Single_Stmt_RH);

            --  Process the returned expression for any decisions if we are
            --  dealing with an expression function

            if N.Kind = Ada_Expr_Function then
               Process_Expression (UIC, N.As_Expr_Function.F_Expr, 'X');
            end if;

            if Fun_Witness /= No_Node_Rewriting_Handle then
               Insert_First
                 (Decl_List,
                  Create_From_Template
                    (UIC.Rewriting_Context,
                     Template  => "Dummy_Witness_Var : Boolean := {};",
                     Arguments => (1 => Fun_Witness),
                     Rule      => Basic_Decls_Rule));
            end if;

            --  Insert the new regular function in place of the old subprogram

            Replace
              (Handle (N),
               Create_Subp_Body
                 (Handle       => UIC.Rewriting_Context,
                  F_Overriding => Detach (N.F_Overriding),
                  F_Subp_Spec  => Detach (N.F_Subp_Spec),
                  F_Aspects    => Detach (N.F_Aspects),
                  F_Decls      => Proc_Decls,
                  F_Stmts      => Stmts_RH,
                  F_End_Name   => Proc_Name));

            UIC.Current_Insertion_Info := Saved_Insertion_Info;
            UIC.MCDC_State_Inserter := Saved_MCDC_State_Inserter;
         end To_Regular_Subprogram;

         ---------------------------
         -- Create_Witness_Formal --
         ---------------------------

         procedure Create_Witness_Formal
           (Formal : out Node_Rewriting_Handle; Formal_Name : Wide_Wide_String)
         is
            Formal_Id     : constant Node_Rewriting_Handle :=
              Make_Identifier (UIC.Rewriting_Context, Formal_Name);
            Formal_Def_Id : constant Node_Rewriting_Handle :=
              Create_Regular_Node
                (UIC.Rewriting_Context,
                 Ada_Defining_Name_List,
                 Children =>
                   (1 =>
                      Create_Defining_Name
                        (UIC.Rewriting_Context, Formal_Id)));
         begin
            Formal :=
              Create_Param_Spec
                (UIC.Rewriting_Context,
                 F_Ids          => Formal_Def_Id,
                 F_Has_Aliased  => No_Node_Rewriting_Handle,
                 F_Mode         => No_Node_Rewriting_Handle,
                 F_Type_Expr    => Make_Std_Ref (UIC, "Boolean"),
                 F_Default_Expr => No_Node_Rewriting_Handle,
                 F_Aspects      => No_Node_Rewriting_Handle);
         end Create_Witness_Formal;

         Is_Expr_Function : constant Boolean := N.Kind = Ada_Expr_Function;

         Needs_Aspects : Boolean := True;
         --  If the new expression function required the creation of a previous
         --  declaration, then the aspects of the original one have already
         --  been attached to it and there is no need to attach them to the
         --  body in the call to Create_Augmented_Function.

         Gen_Names_Prefix : constant Wide_Wide_String :=
           To_Wide_Wide_String
             ((if Is_Expr_Function
               then "Func_Expr"
               else (if Fun_Cov then "Aux" else "Null_Proc"))
              & "_"
              & Img (UIC.Degenerate_Subprogram_Index)
              & Part_Tags (UIC.Instrumented_Unit.Part)
              & '_');
         --  Prefix for the name of all entities we create here

         Call_Params : constant Node_Rewriting_Handle :=
           (if Is_Expr_Function
              or else (Fun_Cov and then not N_Spec.F_Subp_Params.Is_Null)
            then Make (UIC, Ada_Assoc_List)
            else No_Node_Rewriting_Handle);
         --  List of formal/actual associations for the call to the augmented
         --  function. Unused if we are not processing an expression function.

         Formal_Params : constant Node_Rewriting_Handle :=
           (if Is_Expr_Function
              or else (Fun_Cov and then not N_Spec.F_Subp_Params.Is_Null)
            then Clone_Params (UIC, N_Spec)
            else No_Node_Rewriting_Handle);
         --  List of formal parameters for the augmented function. Unused if we
         --  are not processing an expression function.

         Fun_Witness : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
         --  Function witness to be inserted if function coverage is needed

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

         New_Insertion_Info     : Insertion_Info_Ref;
         --  Witness insertion info for statements (for both null procedures
         --  and expression functions).
         New_Fun_Insertion_Info : aliased Insertion_Info;
         --  Witness insertion info for function SCO. This is never used
         --  outside of this subprogram (in particular, not stored in UIC), so
         --  we don't need to create a ref.

         Save_Disable_Instrumentation : constant Boolean :=
           UIC.Disable_Instrumentation;

         EF_Inserter : aliased Expr_Func_MCDC_State_Inserter :=
           (N_Spec        => N_Spec,
            Call_Params   => Call_Params,
            Formal_Params => Formal_Params);
         --   MC/DC state inserter for this expression function (unused if
         --   instrumenting a null procedure).

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

         --  Do not create coverage obligations for static expression functions

         if Is_Expr_Function
           and then N.P_Has_Aspect (To_Unbounded_Text ("Static"))
         then
            return;
         end if;

         UIC.Degenerate_Subprogram_Index :=
           UIC.Degenerate_Subprogram_Index + 1;

         --  Deal with the "easy" Ada 2022 and onwards case for expression
         --  functions: simply nest the expression in a declare expression, and
         --  use that to host the statement witness call, and local
         --  declarations.
         --
         --  Note that this is not legal if the expression function body
         --  contains *any* reference to a
         --  'Access/'Unchecked_Access/'Unrestricted_Access attribute.

         if UIC.Language_Version in Decl_Expr_Supported_Versions
           and then Is_Expr_Function
           and then not Has_Access_Attribute_Ref (N.As_Expr_Function.F_Expr)
         then
            declare
               Expr_Func : constant Expr_Function := N.As_Expr_Function;
               Decl_List : constant Node_Rewriting_Handle :=
                 Create_Node (UIC.Rewriting_Context, Ada_Ada_Node_List);
               --  List of declarations to hold the statement witness call and
               --  local MC/DC state holder variables.

               Decl_Expr : constant Node_Rewriting_Handle :=
                 Create_Decl_Expr
                   (Handle  => UIC.Rewriting_Context,
                    F_Decls => Decl_List,
                    F_Expr  => Detach (Expr_Func.F_Expr));
               --  Declare expression to hold the above list

               Dummy_Decl : constant Node_Rewriting_Handle :=
                 Create_From_Template
                   (UIC.Rewriting_Context,
                    "Dummy_Gnatcov_Decl : constant Boolean := False;",
                    (1 .. 0 => No_Node_Rewriting_Handle),
                    Rule => Object_Decl_Rule);
               --  Dummy declaration to provide a node before which the
               --  statement witness will be inserted.

               Local_Insertion_Info : constant Insertion_Info :=
                 (Method  => Declaration,
                  RH_List => Decl_List,
                  Preelab => False,
                  Parent  => null,
                  others  => <>);
               --  Insertion info points to the newly created declaration list.
               --  Index is 1 as we want to insert a witness call at the
               --  beginning of the list.

               Local_Inserter : aliased Default_MCDC_State_Inserter :=
                 (Local_Decls => Decl_List);
               --  MC/DC state inserter points to the new decl list as well

               Saved_In_Decl_Expr : constant Boolean := UIC.In_Decl_Expr;
               --  We are going to be inserting things as if we were
               --  instrumenting a declare expression, save the flag to restore
               --  it later.

            begin
               --  Add the dummy declaration to the declaration list

               Insert_Last (Decl_List, Dummy_Decl);

               --  Tie the declare expression to the expression function's
               --  F_Expr field, taking care to wrap it in parentheses.

               Set_Child
                 (Handle (Expr_Func),
                  Member_Refs.Expr_Function_F_Expr,
                  Create_Paren_Expr (UIC.Rewriting_Context, Decl_Expr));

               UIC.Current_Insertion_Info.Set (Local_Insertion_Info);
               UIC.MCDC_State_Inserter := Local_Inserter'Unchecked_Access;

               --  Flag that we are in a declare expression, in order to force
               --  the MC/DC state holder to be declared constant

               UIC.In_Decl_Expr := True;

               --  The declaration list above does not exist in the analysis
               --  tree as we just created it, so letting Set_Statement_Entry
               --  decide where to insert the statement witness does not work.
               --  Instead, force the witness to go in the newly declared list
               --  using the Insertion_N param.

               Instrument_Statement (UIC, Expr_Func.F_Expr, 'X', Dummy_Decl);

               --  Preemptively end the statement block. We need to end it in
               --  the non Ada 2022 case (see the call to End_Statement_Block
               --  below), so for consistency, we must end it here as well.

               End_Statement_Block (UIC);
               Start_Statement_Block (UIC);

               Process_Expression (UIC, Expr_Func.F_Expr, 'X');

               --  Restore context

               UIC.Current_Insertion_Info := Saved_Insertion_Info;
               UIC.MCDC_State_Inserter := Saved_MCDC_State_Inserter;
               UIC.In_Decl_Expr := Saved_In_Decl_Expr;
               return;
            end;
         end if;

         if Is_Generic (UIC, N) then
            if Is_Expr_Function then
               UIC.Disable_Instrumentation := True;
               Report
                 (UIC,
                  N,
                  "gnatcov limitation: "
                  & "cannot instrument generic expression functions."
                  & " Consider turning it into a regular function body.");

            else
               --  As Traverse_Degenerate_Subprogram deals only with expression
               --  functions and null procedures, we are in the case of a
               --  generic null procedure here.

               UIC.Disable_Instrumentation := True;
               Report
                 (UIC,
                  N,
                  "gnatcov limitation:"
                  & " cannot instrument generic null procedures."
                  & " Consider turning it into a regular procedure"
                  & " body.");
            end if;
         end if;

         if Is_Expr_Function then

            --  If N does not appear in a package spec, creating the augmented
            --  expression function for it will not create a new primitive.

            if Return_Type_Is_Controlling (UIC, Common_Nodes)
              and then In_Package_Spec (N)
            then

               --  For the moment when an expression function is a primitive of
               --  a tagged type T, and that T is the return type of the EF,
               --  then introducing an augmented EF also introduces a new
               --  primitive for T, for which the return type is T.
               --
               --  This means that if T has a derived type T2, it will need to
               --  override the original EF (because T2 may have different
               --  components compared to T). The instrumenter generates all
               --  the required functions, but since the names of the augmented
               --  EFs are not necessarily consistent between the augmented EFs
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

               UIC.Disable_Instrumentation := True;
               Report
                 (UIC,
                  N,
                  "gnatcov limitation:"
                  & " cannot instrument an expression function which is"
                  & " a primitive of its return type, when this type is"
                  & " a tagged type. Consider turning it into a regular"
                  & " function body.",
                  Warning);
            elsif Is_Self_Referencing (UIC, N.As_Expr_Function)
              and then not Common_Nodes.Ctrl_Type.Is_Null
            then
               UIC.Disable_Instrumentation := True;
               Report
                 (UIC,
                  N,
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
            declare
               Fun_Witness : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
            begin
               if Fun_Cov then
                  Instrument_For_Function_Coverage
                    (UIC,
                     N.As_Basic_Decl.P_Subp_Spec_Or_Null.As_Subp_Spec,
                     Function_Call,
                     Fun_Witness);
               end if;

               --  Turn the expression function or null procedure into a
               --  regular subprogram. If function coverage is needed, declare
               --  a variable in this new subprogram which will be set to a
               --  witness call linked to the function SCO.

               To_Regular_Subprogram (N.As_Base_Subp_Body, Fun_Witness);
            end;

            return;
         end if;

         if Is_Expr_Function then

            --  The statement instrumentation below will take care of assigning
            --  .Witness_* components to their definitive values.

            New_Insertion_Info.Set
              (Insertion_Info'
                 (Method         => Expression_Function,
                  Witness_Actual => No_Node_Rewriting_Handle,
                  Witness_Formal => No_Node_Rewriting_Handle));

            if Is_Expr_Function and then Fun_Cov then
               New_Fun_Insertion_Info :=
                 (Method         => Expression_Function,
                  Witness_Actual => No_Node_Rewriting_Handle,
                  Witness_Formal => No_Node_Rewriting_Handle);
            end if;

            if not UIC.Disable_Instrumentation then

               --  Pass all expression function parameters to the augmented
               --  expression function call.

               for J in
                 1
                 ..
                   (if Common_Nodes.N_Params.Is_Null
                    then 0
                    else Common_Nodes.N_Params.Children_Count)
               loop
                  for Id of
                    Common_Nodes.N_Params.Child (J)
                      .As_Param_Spec
                      .F_Ids
                      .Children
                  loop
                     Insert_Last
                       (Call_Params,
                        Make_Identifier (UIC.Rewriting_Context, Id.Text));
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

            New_Insertion_Info.Set
              (Insertion_Info'
                 (Method  => Statement,
                  RH_List => NP_Nodes.Stmt_List,

                  --  Even if the current package has elaboration restrictions,
                  --  this Insertion_Info is used to insert a witness call in
                  --  the procedure in the generic body: the elaboration
                  --  restriction does not apply there.

                  Preelab => False,
                  Parent  => null));
         end if;

         ----------------------------------
         -- 2. Statement instrumentation --
         ----------------------------------

         UIC.Current_Insertion_Info := New_Insertion_Info;

         UIC.MCDC_State_Inserter := EF_Inserter'Unchecked_Access;

         --  Output statement SCO for degenerate subprogram body (null
         --  statement or freestanding expression).

         if Is_Expr_Function then
            declare
               N_Expr : Expr := N.As_Expr_Function.F_Expr;
            begin
               --  Strip the wrapping paren expr so that we consider only the
               --  nested expression as a statement: this mirrors what we do
               --  for binary traces and yields better coverage reports anyway.

               if N_Expr.Kind = Ada_Paren_Expr then
                  N_Expr := N_Expr.As_Paren_Expr.F_Expr;
               end if;

               Instrument_Statement (UIC, N_Expr, 'X');
               Process_Expression (UIC, N_Expr, 'X');
            end;
         elsif N.Kind /= Ada_Subp_Body then
            --  Even though there is a "null" keyword in the null procedure,
            --  there is no dedicated node for it in the Libadalang parse tree:
            --  use the whole null procedure declaration to provide a sloc.

            Instrument_Statement
              (UIC         => UIC,
               N           => N,
               Typ         => 'X',
               Insertion_N => NP_Nodes.Null_Stmt);
         end if;

         --  Preemptively end the block to force the instrumentation of the
         --  expression function: we need the insertion info that is filled by
         --  Insert_Stmt_Witness (transitively called by End_Statement_Block).

         End_Statement_Block (UIC);
         Start_Statement_Block (UIC);

         --  The insertion info has been completed by calls to
         --  Insert_Stmt_Witness.

         New_Insertion_Info := UIC.Current_Insertion_Info;

         if Fun_Cov then

            if Is_Expr_Function then
               --  For function coverage for expression functions, we need a
               --  second formal parameter meant to receive the function
               --  coverage witness call.

               Create_Witness_Formal
                 (New_Fun_Insertion_Info.Witness_Formal,
                  "Dummy_Witness_Formal_Fun");
            end if;

            --  If function coverage is needed, append a function coverage SCO
            --  and prepare the witness statement to be inserted in the new
            --  subprogram body later.

            declare
               Witness_Flavor : constant Statement_Witness_Flavor :=
                 (if Is_Expr_Function then Function_Call else Procedure_Call);

            begin
               Instrument_For_Function_Coverage
                 (UIC, Common_Nodes.N_Spec, Witness_Flavor, Fun_Witness);
            end;
         end if;

         --  Restore saved insertion context

         UIC.MCDC_State_Inserter := Saved_MCDC_State_Inserter;
         UIC.Current_Insertion_Info := Saved_Insertion_Info;

         --  There is nothing else to do if we gave up instrumenting this
         --  subprogram.

         if UIC.Disable_Instrumentation
           or else UIC.Disable_Coverage
           or else UIC.Is_Disabled_Region ((UIC.SFI, +Sloc (N)))
         then
            UIC.Disable_Instrumentation := Save_Disable_Instrumentation;
            return;
         end if;

         if Is_Expr_Function then

            --  Pass the witness call to the augmented/auxiliary expression
            --  function.

            if Call_Params /= No_Node_Rewriting_Handle then
               Insert_Last
                 (Call_Params, New_Insertion_Info.Get.Witness_Actual);

               --  If function coverage is needed, pass a second witness call
               --  as argument. This one is responsible for discharging the
               --  function SCO.

               if Fun_Cov then
                  Insert_Last (Call_Params, Fun_Witness);
               end if;
            end if;

            if Formal_Params /= No_Node_Rewriting_Handle then
               Insert_Last
                 (Formal_Params, New_Insertion_Info.Get.Witness_Formal);

               if Fun_Cov then
                  Insert_Last
                    (Formal_Params, New_Fun_Insertion_Info.Witness_Formal);
               end if;
            end if;
         end if;

         ----------------------------
         -- 3. Rework declarations --
         ----------------------------

         Replace (Handle (N), Stub);

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

         if Kind (N) in Ada_Base_Subp_Body
           and then Safe_Previous_Part_For_Decl (N.As_Base_Subp_Body).Is_Null
           and then
             (not Is_Expr_Function
              or else Is_Self_Referencing (UIC, N.As_Expr_Function))
         then

            Insert_Before
              (Stub,
               Create_Subp_Decl
                 (RC,
                  F_Overriding => Detach (Common_Nodes.N_Overriding),
                  F_Subp_Spec  => Clone (N_Spec),
                  F_Aspects    => Detach (N.F_Aspects)));

            --  For expression functions, the aspects of the subprogram were
            --  moved to the newly created declaration, so they should not be
            --  added to the augmented function later on.

            Needs_Aspects := False;
         end if;

         if Is_Expr_Function then
            declare
               Augmented_Function      : Node_Rewriting_Handle;
               Augmented_Function_Decl : Node_Rewriting_Handle;
               New_Function            : Node_Rewriting_Handle;
            begin
               --  Create the augmented expression function and amend the
               --  original one.

               Create_Augmented_Function
                 (UIC,
                  Common_Nodes,
                  Formal_Params,
                  Call_Params,
                  Augmented_Function,
                  Augmented_Function_Decl,
                  New_Function,
                  Needs_Aspects);

               --  First comes the augmented expression function, then the new
               --  expression function.

               Insert_Before (Stub, Augmented_Function);
               Insert_Before (Stub, New_Function);

               --  If we need to insert a declaration for the new expression
               --  function, find the correct spot to add it, and keep track
               --  of this insertion if it happens in the same list as the
               --  one currently being instrumented.

               if Augmented_Function_Decl /= No_Node_Rewriting_Handle then
                  declare
                     Previous_Decl : constant Basic_Decl :=
                       N.P_Previous_Part_For_Decl;
                     pragma Assert (not Previous_Decl.Is_Null);
                     --  P_Previous_Part_For_Decl cannot fail because to reach
                     --  this point we will already have succesfully queried
                     --  the previous part of N in
                     --  Augmented_Expr_Function_Needs_Decl.
                  begin
                     Insert_Before
                       (Handle (Previous_Decl), Augmented_Function_Decl);
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
                  Renaming_Decl,
                  (if Fun_Cov then Fun_Witness else No_Node_Rewriting_Handle));

               --  Insert the instance in the wrapper package

               Insert_Last (Common_Nodes.Wrapper_Pkg_Decls, Instance);

               --  Check if there is a convention aspect on the null procedure,
               --  in which case we need to match it in the instance so that
               --  the renaming is subtype conformant.

               declare
                  Convention_Id : constant Identifier :=
                    Get_Convention (Common_Nodes.N);
               begin
                  if not Convention_Id.Is_Null then
                     Insert_Last
                       (Common_Nodes.Wrapper_Pkg_Decls,
                        Create_Pragma_Node
                          (RC,
                           F_Id   => Make_Identifier (RC, "Convention"),
                           F_Args =>
                             Create_Regular_Node
                               (RC,
                                Ada_Assoc_List,
                                (1 => Clone (Convention_Id),
                                 2 => Clone (Common_Nodes.N_Name)))));
                  end if;
               end;

               --  Push the wrapper package and the renaming down to the end of
               --  the current list of declarations.

               Insert_Last
                 (Common_Nodes.Append_List, Common_Nodes.Wrapper_Pkg);
               Insert_Last (Common_Nodes.Append_List, Renaming_Decl);

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

         --  Now that we have inserted the replacement declarations, remove the
         --  stub.

         Remove_Child (Stub);
      end Traverse_Degenerate_Subprogram;

      -----------------------------
      -- Traverse_Component_List --
      -----------------------------

      procedure Traverse_Component_List (CL : Component_List) is
      begin
         for N of CL.F_Components loop
            if N.Kind = Ada_Component_Decl then
               declare
                  C : constant Component_Decl := N.As_Component_Decl;
               begin
                  --  The component definition may contain an arbitrary
                  --  expression in its type expression. This expression is
                  --  evaluated as part as the type elaboration, so its MC/DC
                  --  coverage can be computed using a variable in the local
                  --  context.
                  --
                  --  However, the default expression is evaluated when
                  --  creating a record value, so MC/DC local state
                  --  variables for it cannot go in the current scope.

                  Process_Expression (UIC, C.F_Component_Def.As_Ada_Node, 'X');
                  Process_Standalone_Expression (UIC, C.F_Default_Expr, 'X');
               end;
            end if;
         end loop;

         if not CL.F_Variant_Part.Is_Null then
            for V of CL.F_Variant_Part.F_Variant loop
               Traverse_Component_List (V.F_Components);
            end loop;
         end if;
      end Traverse_Component_List;

      --------------------------------
      -- Traverse_Subp_Decl_Or_Stub --
      --------------------------------

      procedure Traverse_Subp_Decl_Or_Stub (N : Basic_Decl) is

         procedure Process_Contracts (D : Basic_Decl'Class);
         --  Register decisions of pre/postconditions for processing

         -----------------------
         -- Process_Contracts --
         -----------------------

         procedure Process_Contracts (D : Basic_Decl'Class) is
         begin
            Process_Contract (UIC, D, "Pre");
            Process_Contract (UIC, D, "Post");
         end Process_Contracts;

         Dummy_Ctx : constant Context_Handle := Create_Context_Instrument (N);

         N_Spec : constant Subp_Spec := N.P_Subp_Spec_Or_Null.As_Subp_Spec;

         Prev_Part : constant Basic_Decl := Safe_Previous_Part_For_Decl (N);
         --  If this is a null procedure or an expression function, it may have
         --  a previous declaration that must be used as scope identifier.

      begin
         Process_Formal_Default_Exprs (UIC, N_Spec);

         Enter_Scope
           (UIC  => UIC,
            N    => N,
            Decl => (if Prev_Part.Is_Null then N else Prev_Part));
         Start_Statement_Block (UIC);

         --  Nothing else to do except for the case of degenerate subprograms
         --  (null procedures and expression functions).

         if N.Kind in Ada_Null_Subp_Decl then
            Traverse_Degenerate_Subprogram (N, N_Spec);

         elsif N.Kind in Ada_Subp_Decl then
            Process_Contracts (N.As_Subp_Decl);

         elsif N.Kind in Ada_Expr_Function then
            Process_Contracts (N.As_Expr_Function);
            Traverse_Degenerate_Subprogram (N, N_Spec);
         end if;

         Exit_Scope (UIC);
         End_Statement_Block (UIC);
      end Traverse_Subp_Decl_Or_Stub;

      ------------------
      -- Traverse_One --
      ------------------

      procedure Traverse_One (N : Ada_Node) is
         Dummy_Ctx : constant Context_Handle := Create_Context_Instrument (N);

         Saved_In_Generic : constant Boolean := UIC.In_Generic;
      begin
         case N.Kind is
            --  Top of the tree: Compilation unit

            when Ada_Compilation_Unit                              =>
               declare
                  CUN          : constant LAL.Compilation_Unit :=
                    N.As_Compilation_Unit;
                  CUN_Body     : constant Ada_Node := CUN.F_Body;
                  Is_Subunit   : constant Boolean :=
                    CUN_Body.Kind /= Ada_Library_Item;
                  CU_Decl      : constant Basic_Decl :=
                    (if Is_Subunit
                     then Basic_Decl (CUN_Body.As_Subunit.F_Body)
                     else CUN_Body.As_Library_Item.F_Item);
                  CU_Prev_Decl : constant Basic_Decl :=
                    Safe_Previous_Part_For_Decl (CU_Decl);
               begin
                  --  For a library unit, scan context clause. If this is a
                  --  body, also obtain WITH clauses from the spec. Also
                  --  record implicit WITHs for the unit itself and all of
                  --  its parents.

                  if not Is_Subunit then
                     Traverse_Context_Clause
                       (UIC, CUN.F_Prelude, Process_Pragmas => True);
                     if not CU_Prev_Decl.Is_Null then
                        Traverse_Context_Clause
                          (UIC,
                           CU_Prev_Decl
                             .Unit
                             .Root
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
                                AUN
                                  .Root
                                  .As_Compilation_Unit
                                  .F_Body
                                  .As_Library_Item
                                  .F_Item;
                              Parent    : constant Basic_Decl :=
                                Parent_Decl (Root_Decl);
                           begin
                              UIC.Withed_Units.Include
                                (Root_Decl.P_Canonical_Fully_Qualified_Name);

                              exit when Parent.Is_Null;
                              AUN := Parent.Unit;
                           end;
                        end loop;
                     end;
                  end if;

                  --  Remove all Warnings/Style_Checks pragmas: it is not
                  --  our goal to make instrumentation generate warning-free
                  --  or well-formatted code.

                  for I in 1 .. CUN.F_Prelude.Children_Count loop
                     declare
                        C : constant Ada_Node := CUN.F_Prelude.Child (I);
                     begin
                        if C.Kind = Ada_Pragma_Node
                          and then
                            Pragma_Name (C.As_Pragma_Node)
                            in Name_Warnings | Name_Style_Checks
                        then
                           Remove_Child (Handle (C));
                        end if;
                     end;
                  end loop;

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
                        | Ada_Task_Body =>
                        if CU_Decl.Kind = Ada_Generic_Package_Decl then
                           UIC.In_Generic := True;
                        end if;

                        Traverse_Declarations_Or_Statements
                          (UIC,
                           P       => CU_Decl.As_Ada_Node,
                           L       => CUN.F_Pragmas,
                           Preelab => Preelab);
                        UIC.In_Generic := Saved_In_Generic;

                     --  All other cases of compilation units (e.g. renamings),
                     --  generate no SCO information.

                     when others        =>
                        null;
                  end case;

                  --  All instrumented units need to reference the
                  --  corresponding unit that contains coverage buffers.

                  declare
                     Buffers_Unit        : constant Node_Rewriting_Handle :=
                       To_Nodes
                         (UIC.Rewriting_Context, UIC.Pure_Buffer_Unit.Unit);
                     With_Buffers_Clause : constant Node_Rewriting_Handle :=
                       Create_From_Template
                         (UIC.Rewriting_Context,
                          "with {};",
                          (1 => To_Nodes (UIC.Rewriting_Context, Sys_Buffers)),
                          With_Clause_Rule);
                     With_PB_Clause      : constant Node_Rewriting_Handle :=
                       Create_From_Template
                         (UIC.Rewriting_Context,
                          "with {};",
                          (1 => Buffers_Unit),
                          With_Clause_Rule);
                  begin
                     Insert_Last (Handle (CUN.F_Prelude), With_Buffers_Clause);
                     Insert_Last (Handle (CUN.F_Prelude), With_PB_Clause);
                  end;
               end;

            when Ada_Ada_Node_List                                 =>
               for Child of N.As_Ada_Node_List loop
                  Traverse_One (Child.As_Ada_Node);
               end loop;

            --  Package declaration

            when Ada_Package_Decl                                  =>
               Traverse_Package_Declaration
                 (UIC, N.As_Base_Package_Decl, Preelab);

            --  Generic package declaration

            when Ada_Generic_Package_Decl                          =>
               UIC.In_Generic := True;
               Traverse_Generic_Package_Declaration
                 (UIC, N.As_Generic_Package_Decl, Preelab);
               UIC.In_Generic := Saved_In_Generic;

            --  Package body

            when Ada_Package_Body                                  =>
               declare
                  PB : constant Package_Body := N.As_Package_Body;
               begin
                  UIC.In_Generic := Is_Generic (UIC, PB);
                  Traverse_Package_Body (UIC, PB, Preelab);
                  UIC.In_Generic := Saved_In_Generic;
               end;

            --  Subprogram declaration or subprogram body stub

            when Ada_Expr_Function
               | Ada_Null_Subp_Decl
               | Ada_Subp_Body_Stub
               | Ada_Subp_Decl                                     =>
               Traverse_Subp_Decl_Or_Stub (N.As_Basic_Decl);

            --  Entry declaration

            when Ada_Entry_Decl                                    =>
               Process_Expression
                 (UIC, As_Entry_Decl (N).F_Spec.F_Entry_Params, 'X');

            --  Generic subprogram declaration

            when Ada_Generic_Subp_Decl                             =>
               declare
                  GSD : constant Generic_Subp_Decl := As_Generic_Subp_Decl (N);
               begin
                  UIC.In_Generic := True;
                  Process_Standalone_Expression
                    (UIC, GSD.F_Formal_Part.F_Decls, 'X');
                  Process_Expression
                    (UIC, GSD.F_Subp_Decl.F_Subp_Spec.F_Subp_Params, 'X');
                  UIC.In_Generic := Saved_In_Generic;
               end;

            --  Task or subprogram body

            when Ada_Subp_Body | Ada_Task_Body                     =>
               declare
                  B : constant Body_Node := N.As_Body_Node;
               begin
                  UIC.In_Generic := Is_Generic (UIC, B);

                  Traverse_Subprogram_Or_Task_Body (UIC, B);

                  if B.Kind = Ada_Subp_Body and then Enabled (Fun_Call) then
                     declare
                        SB          : constant Subp_Body := N.As_Subp_Body;
                        Fun_Witness : Node_Rewriting_Handle :=
                          No_Node_Rewriting_Handle;
                     begin
                        --  Add a function SCO for this subprogram and fill
                        --  Fun_Witness with a witness call for this new SCO.
                        --  The witness call is within a dummy variable
                        --  declaration.

                        Instrument_For_Function_Coverage
                          (UIC, SB.F_Subp_Spec, Function_Call, Fun_Witness);

                        --  Put the dummy variable containing the witness call
                        --  at the very top of the declarative part of this
                        --  subprogram. This way, it will be executed as soon
                        --  as the function is called.

                        Insert_First
                          (Handle (SB.F_Decls.F_Decls),
                           Create_Function_Witness_Var (UIC, Fun_Witness));
                     end;
                  end if;

                  UIC.In_Generic := Saved_In_Generic;
               end;

            --  Entry body

            when Ada_Entry_Body                                    =>
               declare
                  EB   : constant Entry_Body := N.As_Entry_Body;
                  Cond : constant Expr := EB.F_Barrier;
                  Unit : LAL.Analysis_Unit;

                  Save_Disable_Instrumentation : constant Boolean :=
                    UIC.Disable_Instrumentation;
               begin
                  if not Cond.Is_Null then
                     Unit := Cond.Unit;
                     UIC.Disable_Instrumentation :=
                       Entry_Guards_Restricted
                         (Unit.Context, Unit.Root.As_Compilation_Unit);
                     Process_Expression (UIC, Cond, 'G');
                     UIC.Disable_Instrumentation :=
                       Save_Disable_Instrumentation;
                  end if;

                  Traverse_Subprogram_Or_Task_Body (UIC, EB);
               end;

            --  Protected body

            when Ada_Protected_Body                                =>
               Enter_Scope
                 (UIC, N, Safe_Previous_Part_For_Decl (As_Protected_Body (N)));
               Traverse_Declarations_Or_Statements
                 (UIC, L => As_Protected_Body (N).F_Decls.F_Decls);
               Exit_Scope (UIC);

            when Ada_Exit_Stmt                                     =>
               Instrument_Statement (UIC, N, 'E');
               End_Statement_Block (UIC);
               Start_Statement_Block (UIC);
               Process_Expression (UIC, As_Exit_Stmt (N).F_Cond_Expr, 'E');

            when Ada_Decl_Block | Ada_Begin_Block                  =>

               if N.Kind = Ada_Decl_Block then

                  --  A declaration block does not start a new block

                  Traverse_Declarations_Or_Statements
                    (UIC,
                     L        => As_Decl_Block (N).F_Decls.F_Decls,
                     Is_Block => False);
               end if;

               Traverse_Handled_Statement_Sequence
                 (UIC,
                  N =>
                    (case N.Kind is
                       when Ada_Decl_Block  => As_Decl_Block (N).F_Stmts,
                       when Ada_Begin_Block => As_Begin_Block (N).F_Stmts,
                       when others          => raise Program_Error));

            when Ada_If_Stmt                                       =>
               Instrument_Statement (UIC, N, 'I');
               End_Statement_Block (UIC);

               declare
                  If_N : constant If_Stmt := N.As_If_Stmt;
                  Alt  : constant Elsif_Stmt_Part_List := If_N.F_Alternatives;
               begin
                  Process_Expression (UIC, If_N.F_Cond_Expr, 'I');

                  --  Now we traverse the statements in the THEN part

                  Traverse_Declarations_Or_Statements
                    (UIC, L => If_N.F_Then_Stmts.As_Ada_Node_List);

                  --  Loop through ELSIF parts if present

                  for J in 1 .. If_N.F_Alternatives.Children_Count loop
                     declare
                        Elif : constant Elsif_Stmt_Part :=
                          Alt.Child (J).As_Elsif_Stmt_Part;
                     begin
                        Start_Statement_Block (UIC);
                        Instrument_Statement
                          (UIC,
                           Ada_Node (Elif),
                           'I',
                           Insertion_N => Handle (Elif.F_Cond_Expr));
                        End_Statement_Block (UIC);

                        Process_Expression (UIC, Elif.F_Cond_Expr, 'I');

                        --  Traverse the statements in the ELSIF

                        Traverse_Declarations_Or_Statements
                          (UIC, L => Elif.F_Stmts.As_Ada_Node_List);
                     end;
                  end loop;

                  --  Finally traverse the ELSE statements if present

                  if not If_N.F_Else_Part.Is_Null then
                     Traverse_Declarations_Or_Statements
                       (UIC, L => If_N.F_Else_Part.F_Stmts.As_Ada_Node_List);
                  end if;
               end;

               --  Start a new statement block for statements after the if

               Start_Statement_Block (UIC);

            when Ada_Case_Stmt                                     =>
               Instrument_Statement (UIC, N, 'C');
               End_Statement_Block (UIC);

               declare
                  Case_N : constant Case_Stmt := N.As_Case_Stmt;
                  Alt_L  : constant Case_Stmt_Alternative_List :=
                    Case_N.F_Alternatives;
               begin
                  Process_Expression (UIC, Case_N.F_Expr, 'X');

                  --  Process case branches

                  for J in 1 .. Alt_L.Children_Count loop
                     declare
                        Alt : constant Case_Stmt_Alternative :=
                          Alt_L.Child (J).As_Case_Stmt_Alternative;
                     begin
                        Traverse_Declarations_Or_Statements
                          (UIC, L => Alt.F_Stmts.As_Ada_Node_List);
                     end;
                  end loop;
               end;

               --  Start a new statement sequence for statements after the case

               Start_Statement_Block (UIC);

            --  ACCEPT statement

            when Ada_Accept_Stmt | Ada_Accept_Stmt_With_Stmts      =>
               Instrument_Statement (UIC, N, 'A');
               End_Statement_Block (UIC);

               if N.Kind = Ada_Accept_Stmt_With_Stmts then
                  --  Process sequence of statements

                  Start_Statement_Block (UIC);
                  Traverse_Handled_Statement_Sequence
                    (UIC, N => N.As_Accept_Stmt_With_Stmts.F_Stmts);
                  End_Statement_Block (UIC);
               end if;

               --  Open a new statement sequence for statements after the if

               Start_Statement_Block (UIC);

            --  SELECT statement
            --  (all 4 non-terminals: selective_accept, timed_entry_call,
            --  conditional_entry_call, and asynchronous_select).

            when Ada_Select_Stmt                                   =>
               Instrument_Statement (UIC, N, 'S');
               End_Statement_Block (UIC);

               declare
                  Sel_N : constant Select_Stmt := As_Select_Stmt (N);
               begin
                  for J in 1 .. Sel_N.F_Guards.Children_Count loop
                     declare
                        Alt   : constant Select_When_Part :=
                          Sel_N.F_Guards.Child (J).As_Select_When_Part;
                        Guard : Expr;
                     begin
                        Guard := Alt.F_Cond_Expr;

                        if not Guard.Is_Null then
                           Process_Expression (UIC, Guard, 'G');
                        end if;

                        --  Traverse the select_alternative,
                        --  entry_call_alternative, or triggering_alternative.

                        Traverse_Declarations_Or_Statements
                          (UIC,
                           L                          =>
                             Alt.F_Stmts.As_Ada_Node_List,
                           Is_Select_Stmt_Alternative => True);
                     end;
                  end loop;

                  --  Note: the sequences of statements for ELSE and ABORT
                  --  do not require the special processing for alternatives.

                  if not Sel_N.F_Else_Part.Is_Null then
                     Traverse_Declarations_Or_Statements
                       (UIC, L => Sel_N.F_Else_Part.F_Stmts.As_Ada_Node_List);
                  end if;
                  if not Sel_N.F_Then_Abort_Part.Is_Null then
                     Traverse_Declarations_Or_Statements
                       (UIC,
                        L => Sel_N.F_Then_Abort_Part.F_Stmts.As_Ada_Node_List);
                  end if;
               end;
               --  Open a new statement block for statements after the select

               Start_Statement_Block (UIC);

            --  There is no SCO for a TERMINATE alternative in instrumentation
            --  mode, because there is no place to attach a witness. It would
            --  be dubious anyway, since no code is actually executed if the
            --  alternative is selected.

            when Ada_Terminate_Alternative                         =>
               null;

            when Ada_Goto_Stmt | Ada_Raise_Stmt | Ada_Requeue_Stmt =>
               Instrument_Statement (UIC, N, ' ');
               End_Statement_Block (UIC);

               --  Open a new statement block for statements afterwards

               Start_Statement_Block (UIC);

            --  Simple return statement. which is an exit point, but we
            --  have to process the return expression for decisions.

            when Ada_Return_Stmt                                   =>
               Instrument_Statement (UIC, N, ' ');
               End_Statement_Block (UIC);
               Start_Statement_Block (UIC);
               Process_Expression (UIC, N.As_Return_Stmt.F_Return_Expr, 'X');

            --  Extended return statement

            when Ada_Extended_Return_Stmt                          =>
               Instrument_Statement (UIC, N, 'R');
               declare
                  ER_N : constant Extended_Return_Stmt :=
                    N.As_Extended_Return_Stmt;
               begin
                  Process_Expression (UIC, ER_N.F_Decl, 'X');

                  Start_Statement_Block (UIC);
                  Traverse_Handled_Statement_Sequence (UIC, N => ER_N.F_Stmts);
                  End_Statement_Block (UIC);
               end;

            when Ada_Base_Loop_Stmt                                =>
               declare
                  Loop_S : constant Base_Loop_Stmt := N.As_Base_Loop_Stmt;
                  ISC    : constant Loop_Spec := Loop_S.F_Spec;

               begin
                  if not ISC.Is_Null then

                     --  WHILE loop

                     if ISC.Kind = Ada_While_Loop_Spec then
                        Instrument_Statement (UIC, N, 'W');
                        Process_Expression
                          (UIC, ISC.As_While_Loop_Spec.F_Expr, 'W');

                     --  FOR loop

                     else
                        pragma Assert (ISC.Kind = Ada_For_Loop_Spec);

                        --  In Libadalang, there is only one kind of FOR loop:
                        --  both the RM's loop_parameter_specification and
                        --  iterator_specification are materialized with
                        --  For_Loop_Spec nodes.
                        --
                        --  Ada 2022 allows for a subtype indication to be
                        --  present in the loop specification, but it must
                        --  statically match the cursor type (RM 5.5.2 5/5).
                        --  This means that only static decisions could be
                        --  present in there, which will not be instrumented.
                        --
                        --  Still go through them to generate SCOs, for the
                        --  sake of completeness.

                        Instrument_Statement (UIC, N, 'F');
                        declare
                           LS : constant For_Loop_Spec := ISC.As_For_Loop_Spec;
                        begin
                           Process_Expression (UIC, LS.F_Var_Decl, 'X');
                           Process_Expression (UIC, LS.F_Iter_Expr, 'X');
                           if not LS.F_Iter_Filter.Is_Null then
                              Process_Expression
                                (UIC, LS.F_Iter_Filter.F_Expr, 'W');
                           end if;
                        end;
                     end if;
                  end if;

                  End_Statement_Block (UIC);
                  Start_Statement_Block (UIC);
                  Traverse_Declarations_Or_Statements
                    (UIC, L => Loop_S.F_Stmts.As_Ada_Node_List);
               end;

            --  Pragma

            when Ada_Pragma_Node                                   =>

               --  Processing depends on the kind of pragma

               declare
                  Prag_N    : constant Pragma_Node := N.As_Pragma_Node;
                  Prag_Args : constant Base_Assoc_List := Prag_N.F_Args;
                  Nam       : constant Name_Id := Pragma_Name (Prag_N);
                  Arg       : Positive := 1;

               begin
                  case Nam is
                     when Name_Type_Invariant
                        | Name_Precondition
                        | Name_Postcondition
                     =>
                        Instrument_Statement (UIC, N, 'p');

                        if Assertion_Coverage_Enabled then
                           declare
                              Pragma_Name : constant String :=
                                (case Nam is
                                   when Name_Type_Invariant =>
                                     "Type_Invariant",
                                   when Name_Precondition   => "Precondition",
                                   when Name_Postcondition  => "Postcondition",
                                   when others              => "");
                              Location    : constant String :=
                                Ada.Directories.Simple_Name
                                  (N.Unit.Get_Filename)
                                & ":"
                                & Image (Sloc (N));
                           begin
                              Warn
                                ("gnatcov limitation: pragma "
                                 & Pragma_Name
                                 & " ignored during instrumentation at "
                                 & Location
                                 & ". Consider expressing it as"
                                 & " an aspect instead.");
                           end;
                        end if;

                     when Name_Assert
                        | Name_Assert_And_Cut
                        | Name_Assume
                        | Name_Check
                        | Name_Loop_Invariant
                     =>
                        --  We consider that the assertion policy is
                        --  "disabled", except if any level of assertion
                        --  coverage is enabled.
                        --
                        --  In the compiler, we initially set the type to 'p'
                        --  (disabled pragma), and then switch it to 'P'
                        --  if/when the policy is determined to be enabled
                        --  later on.

                        if Assertion_Coverage_Enabled then
                           Instrument_Statement (UIC, N, 'P');
                           declare
                              Index : constant Positive :=
                                (case Nam is
                                   when Name_Check => 2,
                                   when others     => 1);
                           begin
                              if not Is_Null (Prag_Args.Child (Index)) then
                                 Process_Expression
                                   (UIC,
                                    Prag_Arg_Expr (Prag_Args, Index),
                                    'P');
                              end if;
                           end;
                        else
                           Instrument_Statement (UIC, N, 'p');
                        end if;

                     when Name_Debug
                     =>

                        --  Note: conservatively assume that the check policy
                        --  for pragma debug is enabled.

                        Instrument_Statement (UIC, N, 'P');
                        if Prag_Args.Children_Count = 2 then

                           --  Case of a dyadic pragma Debug: first argument
                           --  is a P decision, any nested decision in the
                           --  second argument is an X decision.

                           Process_Expression
                             (UIC, Prag_Arg_Expr (Prag_Args, Arg), 'P');
                           Arg := 2;
                        end if;

                        Process_Expression
                          (UIC, Prag_Arg_Expr (Prag_Args, Arg), 'X');

                     when Name_Annotate
                     =>

                        --  If this is a coverage exemption, record it. Raise
                        --  a warning if the annotation could not be processed.

                        Process_Annotation (UIC, N, Prag_Args);
                        Instrument_Statement (UIC, N, 'P');

                     --  Even though Compile_Time_* pragmas do contain
                     --  decisions, we cannot instrument them, as they would
                     --  not be known at compile time anymore (this is a
                     --  requirements for this pragma), so just generate a
                     --  statement obligation for them.

                     when Name_Compile_Time_Error | Name_Compile_Time_Warning
                     =>
                        Instrument_Statement (UIC, N, 'P');

                     --  Instrumentation relies on Ada_95 features, which is
                     --  not valid Ada_83, so we remove the pragma.

                     when Name_Ada_83
                     =>
                        declare
                           H : constant Node_Rewriting_Handle := Handle (N);
                        begin
                           Remove_Child (H);

                           --  We are getting rid of Ada83 pragmas, but
                           --  we will prevent gnatcov from using
                           --  features above Ada95.

                           UIC.Language_Version := Ada_1995;
                        end;

                     --  Remove all Warnings/Style_Checks pragmas: it is not
                     --  our goal to make instrumentation generate warning-free
                     --  or well-formatted code.

                     when Name_Warnings | Name_Style_Checks
                     =>
                        Remove_Child (Handle (N));

                     --  For all other pragmas, we generate decision entries
                     --  for any embedded expressions, and the pragma is
                     --  never disabled.

                     --  Should generate P decisions (not X) for assertion
                     --  related pragmas: [{Static,Dynamic}_]Predicate???

                     when others
                     =>
                        Instrument_Statement (UIC, N, 'P');
                        Process_Expression (UIC, N, 'X');
                  end case;
               end;

            --  Object or named number declaration
            --  Generate a single SCO even if multiple defining identifiers
            --  are present.

            when Ada_Number_Decl | Ada_Object_Decl                 =>
               Instrument_Statement (UIC, N, 'o');
               Process_Expression (UIC, N, 'X');

            when Ada_Protected_Type_Decl | Ada_Task_Type_Decl      =>
               Instrument_Statement (UIC, N, 't');
               declare
                  Disc_N : constant Discriminant_Part :=
                    (case N.Kind is
                       when Ada_Protected_Type_Decl =>
                         N.As_Protected_Type_Decl.F_Discriminants,
                       when Ada_Task_Type_Decl      =>
                         N.As_Task_Type_Decl.F_Discriminants,
                       when others                  => raise Program_Error);
               begin
                  Process_Expression (UIC, Disc_N, 'X');
               end;

               Traverse_Sync_Definition (UIC, N);

            when Ada_Single_Protected_Decl | Ada_Single_Task_Decl  =>
               Instrument_Statement (UIC, N, 'o');

               Traverse_Sync_Definition (UIC, N);

            when Ada_Base_Subtype_Decl
               | Ada_Incomplete_Type_Decl
               | Ada_Type_Decl                                     =>
               --  Instrument the type declaration itself as a statement

               declare
                  Typ : Character;
               begin
                  if N.Kind = Ada_Subtype_Decl then
                     Typ := 's';
                  else
                     Typ := 't';
                     Process_Contract (UIC, N.As_Basic_Decl, "Type_Invariant");
                  end if;

                  Instrument_Statement (UIC, N, Typ);
               end;

               --  Process any embedded decisions

               if N.Kind /= Ada_Concrete_Type_Decl then
                  Process_Expression (UIC, N, 'X');
                  return;
               end if;

               declare
                  TD : constant Concrete_Type_Decl := N.As_Concrete_Type_Decl;
               begin
                  --  For concrete type declarations, the discriminant default
                  --  expression may contain decisions, but is not evaluated
                  --  during the type elaboration, so MC/DC local state
                  --  variables for it cannot go in the current scope.

                  Process_Standalone_Expression (UIC, TD.F_Discriminants, 'X');

                  --  Default expressions for regular components behave the
                  --  same: process them accordingly.

                  if TD.F_Type_Def.Kind = Ada_Record_Type_Def then
                     Traverse_Component_List
                       (TD
                          .F_Type_Def
                          .As_Record_Type_Def
                          .F_Record_Def
                          .F_Components);
                  else
                     Process_Expression (UIC, TD.F_Type_Def, 'X');
                  end if;
               end;

            when Ada_Named_Stmt                                    =>
               Traverse_One (N.As_Named_Stmt.F_Stmt.As_Ada_Node);

            when Ada_Package_Renaming_Decl
               | Ada_Subp_Renaming_Decl
               | Ada_Generic_Renaming_Decl
               | Ada_Generic_Instantiation                         =>
               Enter_Scope (UIC => UIC, N => N, Decl => N.As_Basic_Decl);
               Instrument_Statement
                 (UIC,
                  N,
                  (if N.Kind in Ada_Generic_Instantiation then 'i' else 'r'));
               Exit_Scope (UIC);

            when Ada_Label                                         =>
               End_Statement_Block (UIC);
               Start_Statement_Block (UIC);

            when Ada_Call_Stmt                                     =>
               Instrument_Statement (UIC, N, ' ');

               if Enabled (Fun_Call) then
                  Append_SCO
                    (C1                 => 'c',
                     C2                 => 'S',
                     From               =>
                       +Start_Sloc (N.As_Call_Stmt.F_Call.Sloc_Range),
                     To                 =>
                       +Inclusive_End_Sloc (N.As_Call_Stmt.F_Call.Sloc_Range),
                     SFI                => UIC.SFI,
                     Last               => True,
                     Pragma_Aspect_Name => Namet.No_Name);
               end if;

               Process_Expression (UIC, N, 'X');

            when others                                            =>
               --  Determine required type character code, or ASCII.NUL if
               --  no SCO should be generated for this node.

               declare
                  Typ : Character;
               begin
                  case N.Kind is

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

                     when Ada_Package_Body_Stub
                        | Ada_Protected_Body_Stub
                        | Ada_Aspect_Clause
                        | Ada_Task_Body_Stub
                        | Ada_Use_Package_Clause
                        | Ada_Use_Type_Clause                  =>
                        Typ := ASCII.NUL;

                     when others                               =>
                        if N.Kind in Ada_Stmt then
                           Typ := ' ';
                        else
                           Typ := 'd';
                        end if;
                  end case;

                  if Typ /= ASCII.NUL then
                     Instrument_Statement (UIC, N, Typ);
                  end if;

                  if Is_Select_Stmt_Alternative
                    and then N.Kind in Ada_Delay_Stmt | Ada_Call_Expr
                  then
                     End_Statement_Block (UIC);
                     Start_Statement_Block (UIC);
                  end if;
               end;

               --  Process any embedded decisions

               Process_Expression (UIC, N, 'X');
         end case;
      end Traverse_One;

      Saved_Insertion_Info : constant Insertion_Info_Ref :=
        UIC.Current_Insertion_Info;

      Items_Count : constant Natural :=
        (if L.Is_Null then 0 else L.Children_Count);

      --  Start of processing for Traverse_Declarations_Or_Statements

   begin
      if Is_Block then
         Start_Statement_Block (UIC);
      end if;

      --  Push new insertion info

      Insertion_Info_SP.Set
        (UIC.Current_Insertion_Info, Insertion_Info'(Method => None));

      --  Process single prefixed node

      if not P.Is_Null then
         Traverse_One (P);
      end if;

      --  Set up rewriting for lists of declarations/statements

      if not (L.Is_Null or else L.Kind = Ada_Pragma_Node_List) then
         declare
            Method : constant Insertion_Method :=
              (if L.Kind = Ada_Stmt_List then Statement else Declaration);
            II     : Insertion_Info (Method);
         begin
            II.RH_List := Handle (L);
            II.Preelab := Preelab;
            II.Parent :=
              Insertion_Info_Access (Saved_Insertion_Info.Unchecked_Get);

            if Method = Declaration then
               II.RH_Private_List :=
                 (if Priv_Part.Is_Null
                  then No_Node_Rewriting_Handle
                  else Handle (Priv_Part.F_Decls));
            end if;

            Insertion_Info_SP.Set (UIC.Current_Insertion_Info, II);
         end;
      end if;

      --  Loop through statements or declarations

      for J in 1 .. Items_Count loop
         declare
            N : constant Ada_Node := L.Child (J);
         begin
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

      --  Pop insertion info

      UIC.Current_Insertion_Info := Saved_Insertion_Info;

      if Is_Block then
         End_Statement_Block (UIC);
      end if;
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
        (N : Libadalang.Analysis.Name) return Text_Type;
      --  Return the normalized name (see FQN_Sets) of N, a name for a withed
      --  unit.

      ---------------------------------
      -- Withed_Unit_Normalized_Name --
      ---------------------------------

      function Withed_Unit_Normalized_Name
        (N : Libadalang.Analysis.Name) return Text_Type is
      begin
         case N.Kind is
            when Ada_Base_Id     =>
               return Canonicalize (N.Text).Symbol;

            when Ada_Dotted_Name =>
               declare
                  DN : constant Dotted_Name := N.As_Dotted_Name;
               begin
                  return
                    (Withed_Unit_Normalized_Name (DN.F_Prefix)
                     & "."
                     & Withed_Unit_Normalized_Name (DN.F_Suffix.As_Name));
               end;

            when others          =>
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
                        P_Node      : constant Pragma_Node := N.As_Pragma_Node;
                        Name        : constant Name_Id := Pragma_Name (P_Node);
                        Pragma_Name : constant Text_Type := P_Node.F_Id.Text;
                     begin
                        case Name is

                           --  Instrumentation relies on Ada_95 features, which
                           --  is not valid Ada_83, so we remove the pragma.

                           when Name_Ada_83   =>
                              declare
                                 H : constant Node_Rewriting_Handle :=
                                   Handle (N);
                              begin
                                 Remove_Child (H);

                                 --  We are getting rid of Ada83 pragmas, but
                                 --  we will prevent gnatcov from using
                                 --  features above Ada95.

                                 UIC.Language_Version := Ada_1995;
                              end;

                           --  For each other version of the language, set the
                           --  version of the language for the instrumenter.

                           when Name_Ada_95
                              | Name_Ada_05
                              | Name_Ada_2005
                              | Name_Ada_12
                              | Name_Ada_2012
                              | Name_Ada_2022 =>
                              UIC.Language_Version_Pragma :=
                                To_Unbounded_Wide_Wide_String
                                  (To_Lower (Pragma_Name));
                              declare
                                 Pragma_Str : constant String :=
                                   Image (Pragma_Name);
                              begin
                                 if not Set_Language_Version
                                          (UIC.Language_Version,
                                           From => Pragma_Str)
                                 then
                                    Report
                                      (UIC,
                                       N,
                                       "Unknown language pragma version: "
                                       & Pragma_Str,
                                       Kind => Warning);
                                 end if;
                              end;

                           when Name_Annotate =>
                              Process_Annotation (UIC, N, P_Node.F_Args);

                           --  Other pragmas are not relevant

                           when others        =>
                              null;
                        end case;
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

               when others          =>
                  null;
            end case;
         end;
      end loop;

   end Traverse_Context_Clause;

   ------------------------------------------
   -- Traverse_Generic_Package_Declaration --
   ------------------------------------------

   procedure Traverse_Generic_Package_Declaration
     (UIC     : in out Ada_Unit_Inst_Context;
      N       : Generic_Package_Decl;
      Preelab : Boolean) is
   begin
      Process_Standalone_Expression (UIC, N.F_Formal_Part, 'X');
      Traverse_Package_Declaration
        (UIC, N.F_Package_Decl.As_Base_Package_Decl, Preelab);
   end Traverse_Generic_Package_Declaration;

   -----------------------------------------
   -- Traverse_Handled_Statement_Sequence --
   -----------------------------------------

   procedure Traverse_Handled_Statement_Sequence
     (UIC : in out Ada_Unit_Inst_Context; N : Handled_Stmts) is
   begin
      if N.Is_Null then
         return;
      end if;

      Traverse_Declarations_Or_Statements
        (UIC, L => N.F_Stmts.As_Ada_Node_List, Is_Block => False);

      for J in 1 .. N.F_Exceptions.Children_Count loop
         declare
            Handler : constant Ada_Node := N.F_Exceptions.Child (J);
         begin
            --  Note: the exceptions list can also contain pragmas

            if Handler.Kind = Ada_Exception_Handler then
               Start_Statement_Block (UIC);
               Traverse_Declarations_Or_Statements
                 (UIC,
                  L => Handler.As_Exception_Handler.F_Stmts.As_Ada_Node_List);
               End_Statement_Block (UIC);
            end if;
         end;
      end loop;
   end Traverse_Handled_Statement_Sequence;

   ---------------------------
   -- Traverse_Package_Body --
   ---------------------------

   procedure Traverse_Package_Body
     (UIC : in out Ada_Unit_Inst_Context; N : Package_Body; Preelab : Boolean)
   is
      Saved_MCDC_State_Inserter : constant Any_MCDC_State_Inserter :=
        UIC.MCDC_State_Inserter;
      Local_Inserter            : aliased Default_MCDC_State_Inserter :=
        (Local_Decls => Handle (N.F_Decls.F_Decls));

      --  Fetch the package decl corresponding to N. If that fails,
      --  Safe_Previous_Part_For_Decl emits a warning: use the package body
      --  instead as an approximation.

      Decl : Basic_Decl := Safe_Previous_Part_For_Decl (N);
   begin
      if Decl.Is_Null then
         Decl := N.As_Basic_Decl;
      end if;

      UIC.Ghost_Code := Safe_Is_Ghost (N);
      Enter_Scope (UIC => UIC, N => N, Decl => Decl);
      UIC.MCDC_State_Inserter := Local_Inserter'Unchecked_Access;

      Start_Statement_Block (UIC);
      Traverse_Declarations_Or_Statements
        (UIC, N.F_Decls.F_Decls, Preelab, Is_Block => False);
      Traverse_Handled_Statement_Sequence (UIC, N => N.F_Stmts);
      End_Statement_Block (UIC);

      UIC.MCDC_State_Inserter := Saved_MCDC_State_Inserter;
      Exit_Scope (UIC);
      UIC.Ghost_Code := False;
   end Traverse_Package_Body;

   ----------------------------------
   -- Traverse_Package_Declaration --
   ----------------------------------

   procedure Traverse_Package_Declaration
     (UIC     : in out Ada_Unit_Inst_Context;
      N       : Base_Package_Decl;
      Preelab : Boolean)
   is
      Saved_MCDC_State_Inserter : constant Any_MCDC_State_Inserter :=
        UIC.MCDC_State_Inserter;
      Local_Inserter            : aliased Default_MCDC_State_Inserter :=
        (Local_Decls => Handle (N.F_Public_Part.F_Decls));
   begin
      UIC.Ghost_Code := Safe_Is_Ghost (N);
      Enter_Scope (UIC => UIC, N => N, Decl => N.As_Basic_Decl);
      UIC.MCDC_State_Inserter := Local_Inserter'Unchecked_Access;

      --  Check if the No_Elaboration_Code_All pragma applies to the unit.
      --  It can only appear in the specification of a unit-level package.

      begin
         if N.P_Is_Compilation_Unit_Root
           and then
             N.P_Has_Aspect (To_Unbounded_Text ("No_Elaboration_Code_All"))
         then
            UIC.Has_No_Elaboration_Code_All := True;
         end if;
      exception
         when Libadalang.Common.Property_Error =>
            Report
              (Msg  =>
                 "failed to determine No_Elaboration_Code_All constraint"
                 & " for "
                 & N.Unit.Get_Filename,
               Kind => Warning);
      end;

      Start_Statement_Block (UIC);
      Traverse_Declarations_Or_Statements
        (UIC,
         N.F_Public_Part.F_Decls,
         Preelab,
         Priv_Part => N.F_Private_Part,
         Is_Block  => False);

      if not N.F_Private_Part.Is_Null then
         Traverse_Declarations_Or_Statements
           (UIC,
            L        => N.F_Private_Part.F_Decls,
            Preelab  => Preelab,
            Is_Block => False);
      end if;
      End_Statement_Block (UIC);
      UIC.MCDC_State_Inserter := Saved_MCDC_State_Inserter;
      Exit_Scope (UIC);
      UIC.Ghost_Code := False;
   end Traverse_Package_Declaration;

   ------------------------------
   -- Traverse_Sync_Definition --
   ------------------------------

   procedure Traverse_Sync_Definition
     (UIC : in out Ada_Unit_Inst_Context; N : Ada_Node)
   is
      Vis_Decl  : Public_Part := No_Public_Part;
      Priv_Decl : Private_Part := No_Private_Part;
      --  Visible and private declarations of the protected or task definition

   begin
      case N.Kind is
         when Ada_Protected_Type_Decl   =>
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

         when Ada_Single_Task_Decl      =>
            declare
               T_Def : constant Task_Def :=
                 N.As_Single_Task_Decl.F_Task_Type.F_Definition;
            begin
               if not T_Def.Is_Null then
                  Vis_Decl := T_Def.F_Public_Part;
                  Priv_Decl := T_Def.F_Private_Part;
               end if;
            end;

         when Ada_Task_Type_Decl        =>
            declare
               T_Def : constant Task_Def := N.As_Task_Type_Decl.F_Definition;
            begin
               if not T_Def.Is_Null then
                  Vis_Decl := T_Def.F_Public_Part;
                  Priv_Decl := T_Def.F_Private_Part;
               end if;
            end;

         when others                    =>
            raise Program_Error;
      end case;

      --  Vis_Decl and Priv_Decl may be Empty at least for empty task type
      --  declarations. Querying F_Decls is invalid in this case.

      Start_Statement_Block (UIC);
      if not Vis_Decl.Is_Null then
         Traverse_Declarations_Or_Statements
           (UIC,
            L         => Vis_Decl.F_Decls,
            Priv_Part => Priv_Decl,
            Is_Block  => False);
      end if;

      if not Priv_Decl.Is_Null then
         Traverse_Declarations_Or_Statements
           (UIC, L => Priv_Decl.F_Decls, Is_Block => False);
      end if;
      End_Statement_Block (UIC);
   end Traverse_Sync_Definition;

   --------------------------------------
   -- Traverse_Subprogram_Or_Task_Body --
   --------------------------------------

   procedure Traverse_Subprogram_Or_Task_Body
     (UIC : in out Ada_Unit_Inst_Context; N : Body_Node'Class)
   is
      Decls   : Declarative_Part;
      HSS     : Handled_Stmts;
      Aspects : Aspect_Spec := No_Aspect_Spec;

      Saved_MCDC_State_Inserter : constant Any_MCDC_State_Inserter :=
        UIC.MCDC_State_Inserter;
      Local_Inserter            : aliased Default_MCDC_State_Inserter;

   begin
      case Kind (N) is
         when Ada_Subp_Body  =>
            declare
               SBN : constant Subp_Body := N.As_Subp_Body;
            begin
               Decls := SBN.F_Decls;
               HSS := SBN.F_Stmts;
               Aspects := SBN.F_Aspects;
            end;

         when Ada_Task_Body  =>
            declare
               TBN : constant Task_Body := N.As_Task_Body;
            begin
               Decls := TBN.F_Decls;
               HSS := TBN.F_Stmts;
            end;

         when Ada_Entry_Body =>
            declare
               EBN : constant Entry_Body := N.As_Entry_Body;
            begin
               Decls := EBN.F_Decls;
               HSS := EBN.F_Stmts;
            end;

         when others         =>
            raise Program_Error;
      end case;

      declare
         Previous_Part : constant Basic_Decl :=
           Safe_Previous_Part_For_Decl (N);
         Decl          : constant Basic_Decl :=
           (if Previous_Part.Is_Null
            then N.P_Subp_Spec_Or_Null.P_Parent_Basic_Decl
            else Previous_Part);
      begin
         Enter_Scope (UIC => UIC, N => N, Decl => Decl);
      end;

      Process_Formal_Default_Exprs (UIC, N.P_Subp_Spec_Or_Null.As_Subp_Spec);

      --  If assertion coverage is enabled, process the decisions in the
      --  contracts. This is needed in the case of a subprogram body with
      --  aspect with no prior declaration.

      if Aspects /= No_Aspect_Spec and then Assertion_Coverage_Enabled then
         declare
            Assocs  : constant Aspect_Assoc_List := Aspects.F_Aspect_Assocs;
            Idx     : Positive := 1;
            Has_Elt : Boolean := Assocs.Aspect_Assoc_List_Has_Element (Idx);
         begin
            while Has_Elt loop
               Process_Expression
                 (UIC,
                  As_Ada_Node (Assocs.Aspect_Assoc_List_Element (Idx).F_Expr),
                  'A');
               Idx := Idx + 1;
               Has_Elt := Assocs.Aspect_Assoc_List_Has_Element (Idx);
            end loop;
         end;
      end if;

      Local_Inserter.Local_Decls := Handle (Decls.F_Decls);
      UIC.MCDC_State_Inserter := Local_Inserter'Unchecked_Access;
      Start_Statement_Block (UIC);
      Traverse_Declarations_Or_Statements
        (UIC, L => Decls.F_Decls, Is_Block => False);
      Traverse_Handled_Statement_Sequence (UIC, N => HSS);
      End_Statement_Block (UIC);

      Exit_Scope (UIC);

      --  Restore the MCDC_State_Inserter

      UIC.MCDC_State_Inserter := Saved_MCDC_State_Inserter;
   end Traverse_Subprogram_Or_Task_Body;

   ------------------------
   -- Process_Expression --
   ------------------------

   procedure Process_Expression
     (UIC : in out Ada_Unit_Inst_Context; N : Ada_Node'Class; T : Character)
   is
      procedure Process_Call_Expression (N : Ada_Node'Class);
      --  Traverse the expression N to instrument calls for call coverage.
      --
      --  To instument a call expression, the original call expresison is
      --  replaced by an if-expression. The call:
      --
      --  Foo (X)
      --
      --  Becomes:
      --
      --  (if <witness call>
      --   then Foo (X)
      --   else Foo (X))
      --
      --  Note that only the parameters of the original call will be
      --  instrumented. Since the witness call should always return False,
      --  the original call is that in the "else" branch. The one in the
      --  "then" branch should never be executed.

      function Process_Decl_Expr (N : Ada_Node'Class) return Visit_Status;
      --  Helper to Libadalang's Traverse. Only operates on Decl_Exprs,
      --  instrument each declaration as a statement and process the nested
      --  expressions.

      function Process_Raise_Expr (N : Ada_Node'Class) return Visit_Status;
      --  Helper to Libadalang's Traverse. Only operates on Raise_Exprs,
      --  ending the current statement block if a raise expression is found.

      procedure Instrument_GExpr
        (E                 : Expr;
         SR                : Source_Location_Range;
         Parent_Handle     : Node_Rewriting_Handle;
         Parent_Member_Ref : Struct_Member_Ref);
      --  Instrument a guarded expression.
      --  > Register a new SCO
      --  > If the detected langage version is < Ada2022
      --       then it will not instrument the expression.
      --  > Otherwise, nest the expression inside a decl_expr
      --       and set the parent's child to this decl_expr.

      function Process_Case_Expr (N : Ada_Node'Class) return Visit_Status;
      --  Helper to Libadalang's Traverse.
      --  Only operates on Case_Exprs, call Instrument_GExpr on every case

      procedure Process_Decisions
        (UIC : in out Ada_Unit_Inst_Context;
         N   : Ada_Node'Class;
         T   : Character);

      -----------------------------
      -- Process_Call_Expression --
      -----------------------------

      procedure Process_Call_Expression (N : Ada_Node'Class) is
         function Aux_Process_Call_Expression
           (Node : Ada_Node'Class) return Visit_Status;
         --  Auxiliary function, responsible for the actual instrumentation. It
         --  is called on all children of N. if Node is a call expression it is
         --  instrumented. Otherwise, Into is returned to ensure all children
         --  are visited. This is needed in order to find nested calls in
         --  expressions.

         ---------------------------------
         -- Aux_Process_Call_Expression --
         ---------------------------------

         function Aux_Process_Call_Expression
           (Node : Ada_Node'Class) return Visit_Status
         is
            Is_Op_String_Call : constant Boolean :=
              Node.Kind in Ada_String_Literal
              and then
                (Node.Parent.Kind = Ada_Call_Expr
                 or else
                   (Node.Parent.Kind = Ada_Dotted_Name
                    and then Node.Parent.Parent.Kind = Ada_Call_Expr));
            --  For call of the form "+"(A,B), Node will be a String_Literal
            --  inside a Call_Expr. String_Literals are always considered to
            --  be static, but we don't want to instrument static expressions.
            --  Checking this is thus needed to let calls of this form be
            --  seen as calls.
            --  TODO: This is a P_Is_Static_Expr bug, reported in issue
            --  eng/libadalang/libadalang#1523. Once this is fixed this
            --  variable and associated check should be safe to remove.

            Full_Call_Node : Ada_Node;
         begin
            if Is_Call_Leaf (Node)
              and then
                (not Is_Static_Expr (Node.As_Expr) or else Is_Op_String_Call)
            then
               Full_Call_Node := Full_Call (Node);

               --  Skip calls that are direct children of call statements

               if Full_Call_Node.Parent.Kind in Ada_Call_Stmt then
                  return Into;
               end if;

               --  Generate a SCO for this call

               declare
                  Orig_Handle  : constant Node_Rewriting_Handle :=
                    Handle (Full_Call_Node);
                  Dummy_Handle : constant Node_Rewriting_Handle :=
                    Create_Node (UIC.Rewriting_Context, Full_Call_Node.Kind);
                  Then_Node    : constant Node_Rewriting_Handle :=
                    Clone (Full_Call_Node);

                  Return_Type : Base_Type_Decl := No_Base_Type_Decl;

                  Needs_Qualified_Expr : constant Boolean :=
                    Full_Call_Node.Parent.Kind
                    in Ada_Dotted_Name | Ada_Explicit_Deref;
                  --  We only need to turn the if-expression into a qualified
                  --  when the parent of the call and its parent are both
                  --  dotted named. For example, with A and B packages, F a
                  --  function returning a record and X a field of this record:
                  --
                  --  A.B.F.X
                  --
                  --  Here the instrumentation of F needs to be a name, so we
                  --  need the fully qualifed if-expression. But for:
                  --
                  --  A.B.F
                  --
                  --  The if-expression alone suffices.

                  Do_Not_Instrument : Boolean := False;
                  --  Whether we can instrument this call. Depends on whether
                  --  we need to use a qualified expression to insturment, and
                  --  if the return type of the call is visible from the call
                  --  site or not.

                  Location : constant Source_Location_Range :=
                    (if Node.Kind = Ada_Op_Concat
                     then
                       (Node
                          .Parent
                          .As_Concat_Operand
                          .F_Operator
                          .As_Ada_Node
                          .Sloc_Range)
                     else Full_Call_Node.Sloc_Range);
                  --  Special case for concatenation operators: Node holds
                  --  "& <operand>" where the call that needs to be monitored
                  --  is the operator "&". In order to produce valid code, the
                  --  if-expression holding the witness call is created around
                  --  the operand, but the location of the SCO should still be
                  --  the operator's.
               begin
                  Append_SCO
                    (C1                 => 'c',
                     C2                 => 'E',
                     From               => +Start_Sloc (Location),
                     To                 => +Inclusive_End_Sloc (Location),
                     SFI                => UIC.SFI,
                     Last               => True,
                     Pragma_Aspect_Name => Namet.No_Name);

                  --  Pre-compute the return type of the expression if we
                  --  need to generate a qualified expression.
                  --
                  --  If we cannot determine it, do not instrument the call.

                  begin
                     if Needs_Qualified_Expr then
                        Return_Type :=
                          Full_Call_Node.As_Expr.P_Expression_Type;
                     end if;
                  exception
                     when Property_Error =>
                        Report
                          (Full_Call_Node,
                           "Failed to retrieve the expression type of the"
                           & " call",
                           Kind => Warning);
                        Do_Not_Instrument := True;
                  end;

                  --  TODO: LIMITATIONS
                  --
                  --  NON-IMPORTED TYPES
                  --  Currently, gnatcov is unable to determine if the full
                  --  name of a type is visible and can be explicitely used in
                  --  a unit. For this reason, we cannot currently turn
                  --  the if-expressions into fully qualified names. This is
                  --  need for call the are in the middle of a dotted name.
                  --  For now, do not instrument calls that wouls require such
                  --  an instrumentation.

                  Do_Not_Instrument := Needs_Qualified_Expr;

                  if not Do_Not_Instrument then
                     Fill_Expression_Insertion_Info
                       (UIC,
                        Allocate_Statement_Bit
                          (UIC.Unit_Bits, SCOs.SCO_Table.Last));

                     Replace (Orig_Handle, Dummy_Handle);

                     declare
                        If_Expression : constant Node_Rewriting_Handle :=
                          Create_Paren_Expr
                            (UIC.Rewriting_Context,
                             Create_If_Expr
                               (UIC.Rewriting_Context,
                                UIC.Current_Insertion_Info.Get.Witness_Actual,
                                Then_Node,
                                No_Node_Rewriting_Handle,
                                Orig_Handle));

                        Qualified_Expr : constant Node_Rewriting_Handle :=
                          (if Needs_Qualified_Expr
                           then
                             Create_Qual_Expr
                               (UIC.Rewriting_Context,
                                F_Prefix =>
                                  To_Nodes
                                    (UIC.Rewriting_Context,
                                     To_Qualified_Name
                                       (Return_Type
                                          .As_Basic_Decl
                                          .P_Fully_Qualified_Name_Array)),
                                F_Suffix =>
                                  Create_Paren_Expr
                                    (UIC.Rewriting_Context, If_Expression))
                           else No_Node_Rewriting_Handle);

                     begin
                        if Needs_Qualified_Expr then
                           Replace (Dummy_Handle, Qualified_Expr);
                        else
                           Replace (Dummy_Handle, If_Expression);
                        end if;
                     end;
                  else
                     Report
                       (UIC,
                        Full_Call_Node,
                        "gnatcov limitation: cannot instrument calls "
                        & (if Needs_Qualified_Expr
                           then "within dotted names"
                           else "to user-defined operators"),
                        Warning);
                     UIC.Non_Instr_LL_SCOs.Include
                       (SCO_Id (SCOs.SCO_Table.Last));
                  end if;
               end;
            end if;

            return Into;

         end Aux_Process_Call_Expression;

         Saved_Insertion_Info : constant Insertion_Info_Ref :=
           UIC.Current_Insertion_Info;

         Local_Insertion_Info :
           constant Insertion_Info (Expression_Function) :=
             (Method         => Expression_Function,
              Witness_Actual => No_Node_Rewriting_Handle,
              Witness_Formal => No_Node_Rewriting_Handle);
      begin
         if N.Is_Null then
            return;
         end if;

         UIC.Current_Insertion_Info.Set (Local_Insertion_Info);

         N.Traverse (Aux_Process_Call_Expression'Access);

         UIC.Current_Insertion_Info := Saved_Insertion_Info;
      end Process_Call_Expression;

      -----------------------
      -- Process_Decl_Expr --
      -----------------------

      function Process_Decl_Expr (N : Ada_Node'Class) return Visit_Status is
      begin
         if N.Kind = Ada_Decl_Expr then
            declare
               Saved_Inserter     : constant Any_MCDC_State_Inserter :=
                 UIC.MCDC_State_Inserter;
               Saved_In_Decl_Expr : constant Boolean := UIC.In_Decl_Expr;
               Local_Inserter     : aliased Default_MCDC_State_Inserter :=
                 (Local_Decls => Handle (N.As_Decl_Expr.F_Decls));
            begin
               UIC.MCDC_State_Inserter := Local_Inserter'Unchecked_Access;
               UIC.In_Decl_Expr := True;

               --  Traverse_Declarations_Or_Statements will instrument the
               --  declarations as statements, as well as instrument the
               --  nested decisions within those declarations.

               Traverse_Declarations_Or_Statements
                 (UIC, N.As_Decl_Expr.F_Decls);
               UIC.MCDC_State_Inserter := Saved_Inserter;
               UIC.In_Decl_Expr := Saved_In_Decl_Expr;

               return Over;
            end;
         else
            return Into;
         end if;
      end Process_Decl_Expr;

      ----------------------
      -- Instrument_Gexpr --
      ----------------------

      procedure Instrument_GExpr
        (E                 : Expr;
         SR                : Source_Location_Range;
         Parent_Handle     : Node_Rewriting_Handle;
         Parent_Member_Ref : Struct_Member_Ref) is
      begin
         Append_SCO
           (C1   => 'g',
            C2   => 'c',
            From => +Start_Sloc (SR),
            To   => +Inclusive_End_Sloc (SR),
            SFI  => UIC.SFI,
            Last => False);

         if UIC.Language_Version in Decl_Expr_Supported_Versions then
            declare
               Bit                 : constant Bit_Id :=
                 Allocate_Statement_Bit (UIC.Unit_Bits, SCOs.SCO_Table.Last);
               Witness_Decl_Handle : constant Node_Rewriting_Handle :=
                 Make_Statement_Witness
                   (UIC          => UIC,
                    Bit          => Bit,
                    Flavor       => Declaration,
                    In_Generic   => UIC.In_Generic,
                    In_Decl_Expr => True);
               Decl_Expr_Handle    : constant Node_Rewriting_Handle :=
                 Create_Paren_Expr
                   (Handle => UIC.Rewriting_Context,
                    F_Expr =>
                      Create_Decl_Expr
                        (Handle  => UIC.Rewriting_Context,
                         F_Decls => Witness_Decl_Handle,
                         F_Expr  => Detach (E)));
            begin
               Set_Child (Parent_Handle, Parent_Member_Ref, Decl_Expr_Handle);
            end;
         else

            --  If the language version does not allow declare expressions,
            --  do not instrument the expression.

            Report
              (UIC  => UIC,
               Node => E,
               Msg  =>
                 "Guarded Expression coverage is not available"
                 & " before Ada2022",
               Kind => Diagnostics.Warning);
            UIC.Non_Instr_LL_SCOs.Include (SCO_Id (SCOs.SCO_Table.Last));
         end if;
      end Instrument_GExpr;

      -----------------------
      -- Process_Case_Expr --
      -----------------------

      function Process_Case_Expr (N : Ada_Node'Class) return Visit_Status is
      begin
         if N.Kind = Ada_Case_Expr then
            declare
               Case_Node    : constant Case_Expr := N.As_Case_Expr;
               Alternatives : constant Case_Expr_Alternative_List :=
                 Case_Node.F_Cases;
            begin
               for Alt of Alternatives loop

                  --  Perform witness insertion

                  Instrument_GExpr
                    (Alt.As_Case_Expr_Alternative.F_Expr,
                     Alt.Sloc_Range,
                     Handle (Alt),
                     Member_Refs.Case_Expr_Alternative_F_Expr);
                  Process_Expression
                    (UIC, Alt.As_Case_Expr_Alternative.F_Expr, T);
               end loop;
               return Over; -- Do not use `Traverse` to recurse into case-exprs
            end;
         elsif N.Kind = Ada_Decl_Expr then
            return Over;
         else
            return Into;
         end if;
      end Process_Case_Expr;

      ------------------------
      -- Process_Raise_Expr --
      ------------------------

      function Process_Raise_Expr (N : Ada_Node'Class) return Visit_Status is
      begin
         if N.Kind = Ada_Raise_Expr then
            End_Statement_Block (UIC);
            Start_Statement_Block (UIC);
            return Over;
         end if;
         return Into;
      end Process_Raise_Expr;

      -----------------------
      -- Process_Decisions --
      -----------------------

      procedure Process_Decisions
        (UIC : in out Ada_Unit_Inst_Context; N : Ada_Node'Class; T : Character)
      is
         Mark : Nat;
         --  This is used to mark the location of a decision sequence in the
         --  SCO table. We use it for backing out a simple decision in an
         --  expression context that contains only NOT operators.

         Mark_Hash : Nat;
         --  Likewise for the putative SCO_Raw_Hash_Table entries: see below

         type Hash_Entry is record
            Sloc      : Source_Location;
            SCO_Index : Nat;
         end record;
         --  We must register all conditions/pragmas in SCO_Raw_Hash_Table.
         --  However we cannot register them in the same time we are adding the
         --  corresponding SCO entries to the raw table since we may discard
         --  them later on. So instead we put all putative conditions into
         --  Hash_Entries (see below) and register them once we are sure we
         --  keep them.
         --
         --  This data structure holds the conditions/pragmas to register in
         --  SCO_Raw_Hash_Table.

         package Hash_Entries is new
           Table.Table
             (Table_Component_Type => Hash_Entry,
              Table_Index_Type     => Nat,
              Table_Low_Bound      => 1,
              Table_Initial        => 10,
              Table_Increment      => 10,
              Table_Name           => "Hash_Entries");
         --  Hold temporarily (i.e. free'd before returning) the Hash_Entry
         --  before they are registered in SCO_Raw_Hash_Table.

         ---------------------------------
         -- Decision-specific variables --
         ---------------------------------

         --  The following variables are related to the current decision being
         --  processed by this call to Process_Decisions. Note that in the case
         --  of nested decisions, this subprogram recurses, so we do not have
         --  to worry about overwriting them.

         Current_Decision : Nat;
         --  Low level SCO Id of current decision

         X_Not_Decision : Boolean;
         --  This flag keeps track of whether a decision sequence in the
         --  SCO table contains only NOT operators, and is for an expression
         --  context (T=X). The flag will be set False if T is other than X,
         --  or if an operator other than NOT is in the sequence.

         Condition_Count : Natural := 0;
         --  Count of conditions for current decision (MC/DC only)

         Conditions_State : Unbounded_String;
         --  Name of MC/DC and ATCC state local variable for current
         --  decision (MC/DC and ATCC only).

         procedure Output_Decision_Operand
           (Operand : Expr; Decision_Static : Boolean);
         --  The node Operand is the top level logical operator of a decision,
         --  or it is one of the operands of a logical operator belonging to
         --  a single complex decision. This (recursive) routine outputs the
         --  sequence of table entries corresponding to the node. Note that we
         --  do not process the sub- operands to look for further decisions,
         --  that processing is done in Find_Nested_Decisions, because we can't
         --  get decisions mixed up in the global table. Call has no effect
         --  if Operand is Empty. Increments Condition_Count (recursively)
         --  for each condition.
         --
         --  Decision_Static indicates whether the expression of the whole
         --  decision is static, and should thus not be instrumented.

         procedure Output_Element (N : Ada_Node);
         --  Node N is an operand of a logical operator that is not itself a
         --  logical operator, or it is a simple decision. This routine outputs
         --  the table entry for the element, with C1 set to ' '. Last is set
         --  False, and an entry is made in the condition hash table.

         procedure Output_Header
           (T      : Character;
            E      : Expr'Class;
            New_SI : aliased in out Default_MCDC_State_Inserter);
         --  Outputs a decision header node. T is I/W/E/P for IF/WHILE/EXIT
         --  WHEN/ PRAGMA, and 'X' for the expression case. Resets
         --  Condition_Count to 0, and initializes Conditions_State.
         --
         --  If there is MCDC state inserter for the current context, this may
         --  try to create one: if that's the case, New_SI will be used to do
         --  that. The caller is responsible for saving and restoring the old
         --  state inserter.

         procedure Find_Nested_Decisions (Operand : Expr);
         --  This is called on node Operand, the top level node of a decision,
         --  or on one of its operands or suboperands after generating the full
         --  output for the complex decision. It process the suboperands of the
         --  decision looking for nested decisions.

         function Process_Node (N : Ada_Node'Class) return Visit_Status;
         --  Processes one node in the traversal, looking for logical
         --  operators, and if one is found, outputs the appropriate
         --  table entries.

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
            --  belongs to the decision. '?' entries will be filtered out in
            --  the second (SCO_Record_Filtered) pass.

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
                     else
                        pragma Assert (Op_NK in Ada_Op_And | Ada_Op_And_Then);
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
                  SFI  => UIC.SFI,
                  Last => False);

               Hash_Entries.Append ((Sloc (N), SCOs.SCO_Table.Last));

               if not L.Is_Null then
                  Output_Decision_Operand (L, Decision_Static);
               end if;
               Output_Decision_Operand (R, Decision_Static);

            --  Not a logical operator -> condition

            else
               Output_Element (N.As_Ada_Node);

               if Decision_Static or else UIC.Disable_Instrumentation then
                  return;
               end if;
               if MCDC_Coverage_Enabled
                 or else Assertion_Condition_Coverage_Enabled
               then
                  UIC.Source_Conditions.Append
                    (Source_Condition'
                       (LL_SCO    => SCOs.SCO_Table.Last,
                        Condition => N.As_Expr,
                        State     => Conditions_State,
                        First     => Condition_Count = 0));

                  Condition_Count := Condition_Count + 1;
               end if;
            end if;
         end Output_Decision_Operand;

         --------------------
         -- Output_Element --
         --------------------

         procedure Output_Element (N : Ada_Node) is
            N_SR : constant Source_Location_Range := N.Sloc_Range;
            SCO  : SCO_Id;
         begin

            Append_SCO
              (C1   => ' ',
               C2   => 'c',
               From => +Start_Sloc (N_SR),
               To   => +Inclusive_End_Sloc (N_SR),
               SFI  => UIC.SFI,
               Last => False);
            Hash_Entries.Append ((Start_Sloc (N_SR), SCOs.SCO_Table.Last));
            SCO := SCO_Id (SCOs.SCO_Table.Last);

            if Is_Static_Expr (N.As_Expr) then

               --  This condition is static: record its value

               declare
                  Eval : constant String := Bool_Expr_Eval (N.As_Expr);
               begin
                  if Eval = "True" then
                     UIC.True_Static_LL_SCOs.Include (SCO);
                  elsif Eval = "False" then
                     UIC.False_Static_LL_SCOs.Include (SCO);
                  end if;
               end;
            end if;

            if UIC.Disable_Instrumentation then
               UIC.Non_Instr_LL_SCOs.Include (SCO);
            end if;
         end Output_Element;

         -------------------
         -- Output_Header --
         -------------------

         procedure Output_Header
           (T      : Character;
            E      : Expr'Class;
            New_SI : aliased in out Default_MCDC_State_Inserter)
         is
            Loc : Source_Location := No_Source_Location;
            --  Node whose Sloc is used for the decision

            Nam : Name_Id := Namet.No_Name;
            --  For the case of an aspect, aspect name

            Is_Contract : Boolean := T in 'a' | 'A' | 'P';
            --  Is the decision that of a contract

         begin
            case T is
               when 'I' | 'E' | 'W' | 'a' | 'A' =>

                  --  For IF, EXIT, WHILE, or aspects, the token SLOC is that
                  --  of the parent of the expression.

                  Loc := Sloc (Parent (E));

                  if T in 'a' | 'A' then
                     Nam := Aspect_Assoc_Name (E.Parent.As_Aspect_Assoc);
                  end if;

               when 'G'                         =>

                  --  For an entry body guard, use the location of the entry
                  --  body. For the guard on a select alternative, we do not
                  --  have access to the token location for the WHEN, so we use
                  --  the sloc of the condition itself.

                  declare
                     Par : constant Ada_Node := E.Parent;
                  begin
                     if Par.Kind = Ada_Entry_Body then
                        Loc := Sloc (Par);
                     else
                        Loc := Sloc (E);
                     end if;
                  end;

               when 'P'                         =>

                  --  For PRAGMA, we must get the location from the pragma
                  --  node. Argument E is the pragma argument.

                  declare
                     PN : Ada_Node := E.As_Ada_Node;
                  begin
                     while PN.Kind /= Ada_Pragma_Node loop
                        PN := PN.Parent;
                     end loop;
                     Loc := Sloc (PN);
                     Is_Contract :=
                       Pragma_Name (PN.As_Pragma_Node)
                       /= Precomputed_Symbols (Debug);
                  end;

               when 'X'                         =>

                  --  For an expression, we will use the sloc of the first
                  --  condition. This is done afterwards, when processing the
                  --  low level scos in sc_obligations.adb.
                  --
                  --  cf. the Update_Decision_Sloc procedure.

                  null;

               --  No other possibilities

               when others                      =>
                  raise Program_Error;
            end case;

            Append_SCO
              (C1                 => T,
               C2                 => ' ',
               From               => +Loc,
               To                 => Slocs.No_Local_Location,
               SFI                => UIC.SFI,
               Last               => False,
               Pragma_Aspect_Name => Nam);
            if UIC.Disable_Instrumentation then
               UIC.Non_Instr_LL_SCOs.Include (SCO_Id (SCOs.SCO_Table.Last));
            end if;

            Current_Decision := SCOs.SCO_Table.Last;

            --  Do not instrument this decision if we have already determined
            --  from the context that instrumenting it could produce invalid
            --  code.
            --
            --  Instrumenting static decisions would make them non-static by
            --  wrapping them in a Witness call. This transformation would
            --  trigger legality checks on the originally non-evaluated branch,
            --  which could result in compilation errors specific to the
            --  instrumented code, e.g. on:
            --
            --   X := (if <config.static-False>
            --         then <out-of-range-static>
            --         else <value>);
            --
            --  For this reason, also refrain from instrumenting static
            --  decisions.

            if not UIC.Disable_Instrumentation
              and then not Is_Static_Expr (E)
              and then
                (Coverage.Enabled (Decision)
                 or else MCDC_Coverage_Enabled
                 or else Assertion_Condition_Coverage_Enabled)
            then
               if MCDC_Coverage_Enabled
                 or else
                   (Is_Contract and then Assertion_Condition_Coverage_Enabled)
               then
                  Condition_Count := 0;

                  --  We need to instrument for MCDC, so we need an MCDC state
                  --  inserter. If there is none, try to create one based on a
                  --  decl expr.

                  if Require_MCDC_State_Inserter (UIC, E, New_SI) then
                     Conditions_State :=
                       To_Unbounded_String
                         (UIC.MCDC_State_Inserter.Insert_MCDC_State
                            (UIC, Make_MCDC_State_Name (SCOs.SCO_Table.Last)));
                  end if;
               end if;

               UIC.Source_Decisions.Append
                 (Source_Decision'
                    (LL_SCO      => Current_Decision,
                     Decision    => E.As_Expr,
                     State       => Conditions_State,
                     Is_Contract => Is_Contract));
            end if;

            --  For an aspect specification, which will be rewritten into a
            --  pragma, enter a hash table entry now.

            if T in 'a' | 'A' then
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
            --  Test for the two cases where N is the root node of some
            --  decision:

            Decision_Root : constant Boolean :=

            --  Simple decision at outer level: a boolean expression (which
            --  is not a logical operator or short circuit form) appearing
            --  as the operand of an IF, WHILE, EXIT WHEN, or special PRAGMA
            --  construct.

               (N = Process_Decisions.N and then T /= 'X')

              --  Complex decision, whether at outer level or nested: a boolean
              --  expression involving a logical operator.

              or else
                (N.Kind in Ada_Expr
                 and then Is_Complex_Decision (UIC, N.As_Expr));

         begin
            if Decision_Root then
               declare
                  EN : constant Expr := N.As_Expr;
                  T  : Character;

                  --  If there is none, Output_Header may create a MCDC local
                  --  state variable inserter.

                  New_SI    : aliased Default_MCDC_State_Inserter;
                  Saved_SI  : constant Any_MCDC_State_Inserter :=
                    UIC.MCDC_State_Inserter;
                  Saved_IDE : constant Boolean := UIC.In_Decl_Expr;
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
                  Mark := SCOs.SCO_Table.Last;
                  Mark_Hash := Hash_Entries.Last;
                  Output_Header (T, EN, New_SI);

                  --  Output the decision (recursively traversing operands)

                  Output_Decision_Operand (EN, Is_Static_Expr (N.As_Expr));

                  --  If the decision was in an expression context (T =
                  --  'X') and contained only NOT operators, then we don't
                  --  output it, so delete the associated SCO entries. As a
                  --  consequence, no instrumentation will be emitted.

                  if X_Not_Decision then
                     SCOs.SCO_Table.Set_Last (Mark);
                     Hash_Entries.Set_Last (Mark_Hash);

                  --  Otherwise, set Last in last table entry to mark end

                  else
                     SCOs.SCO_Table.Table (SCOs.SCO_Table.Last).Last := True;
                  end if;

                  --  Process any embedded decisions. For the sake of
                  --  simplicity the coverage of nested decisions in contract
                  --  decisions should not be checked. Therefore they should be
                  --  instrumented.

                  if T not in 'P' | 'A' | 'a' then
                     Find_Nested_Decisions (EN);
                  end if;

                  UIC.MCDC_State_Inserter := Saved_SI;
                  UIC.In_Decl_Expr := Saved_IDE;
                  return Over;
               end;
            end if;

            --  Here for cases that are known to not be logical operators

            case N.Kind is
               --  CASE expression: processed in Process_Case_Expr

               when Ada_Case_Expr       =>
                  return (if Enabled (GExpr) then Over else Into);

               --  IF expression: processed like an if statement

               when Ada_If_Expr         =>
                  declare
                     IEN : constant If_Expr := N.As_If_Expr;
                     Alt : constant Elsif_Expr_Part_List := IEN.F_Alternatives;

                  begin
                     Process_Decisions (UIC, IEN.F_Cond_Expr, 'I');
                     Process_Decisions (UIC, IEN.F_Then_Expr, 'X');
                     if Enabled (GExpr) then
                        Instrument_GExpr
                          (IEN.F_Then_Expr,
                           IEN.F_Then_Expr.Sloc_Range,
                           Handle (IEN),
                           Member_Refs.If_Expr_F_Then_Expr);
                     end if;

                     for J in 1 .. Alt.Children_Count loop
                        declare
                           EIN : constant Elsif_Expr_Part :=
                             Alt.Child (J).As_Elsif_Expr_Part;
                        begin
                           Process_Decisions (UIC, EIN.F_Cond_Expr, 'I');
                           Process_Decisions (UIC, EIN.F_Then_Expr, 'X');
                           if Enabled (GExpr) then
                              Instrument_GExpr
                                (EIN.F_Then_Expr,
                                 EIN.F_Then_Expr.Sloc_Range,
                                 Handle (EIN),
                                 Member_Refs.Elsif_Expr_Part_F_Then_Expr);
                           end if;
                        end;
                     end loop;

                     Process_Decisions (UIC, IEN.F_Else_Expr, 'X');
                     if Enabled (GExpr) then
                        Instrument_GExpr
                          (IEN.F_Else_Expr,
                           IEN.F_Else_Expr.Sloc_Range,
                           Handle (IEN),
                           Member_Refs.If_Expr_F_Else_Expr);
                     end if;
                     return Over;
                  end;

               when Ada_Quantified_Expr =>
                  Process_Decisions
                    (UIC, N.As_Quantified_Expr.F_Loop_Spec, 'X');
                  Process_Decisions (UIC, N.As_Quantified_Expr.F_Expr, 'W');

                  if Enabled (GExpr) then
                     Instrument_GExpr
                       (N.As_Quantified_Expr.F_Expr,
                        N.As_Quantified_Expr.F_Expr.Sloc_Range,
                        Handle (N),
                        Member_Refs.Quantified_Expr_F_Expr);
                  end if;
                  return Over;

               when Ada_For_Loop_Spec   =>
                  declare
                     LS : constant For_Loop_Spec := N.As_For_Loop_Spec;
                  begin
                     Process_Decisions (UIC, LS.F_Var_Decl, 'X');
                     Process_Decisions (UIC, LS.F_Iter_Expr, 'X');
                     if not LS.F_Iter_Filter.Is_Null then
                        Process_Decisions (UIC, LS.F_Iter_Filter.F_Expr, 'W');
                     end if;
                  end;
                  return Over;

               --  Aspects for which we don't want to instrument the decision

               when Ada_Aspect_Assoc    =>
                  declare
                     AN : constant Aspect_Assoc := N.As_Aspect_Assoc;
                  begin
                     if Aspect_Assoc_Name (AN)
                        in As_Symbol (Dynamic_Predicate)
                         | As_Symbol (Invariant)
                         | As_Symbol (Ghost_Predicate)
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

               --  Declare expressions: do not process the nested decisions
               --  in the declarations, as those will be processed when
               --  instrumenting them, but do process the final expression.

               when Ada_Decl_Expr       =>
                  Process_Decisions (UIC, N.As_Decl_Expr.F_Expr, 'X');
                  return Over;

               --  All other cases, continue scan

               when others              =>
                  return Into;
            end case;
         end Process_Node;

         --  Start of processing for Process_Decisions

      begin
         if N.Is_Null
           or else UIC.Disable_Coverage
           or else UIC.Is_Disabled_Region ((UIC.SFI, +Sloc (N)))
         then
            return;
         end if;
         Hash_Entries.Init;
         N.Traverse (Process_Node'Access);
         Hash_Entries.Free;
      end Process_Decisions;

      --  Start of processing for Process_Expressions

   begin
      if N.Is_Null then
         return;
      end if;
      Process_Decisions (UIC, N, T);

      --  Then, look for all call expressions to instrument them. However,
      --  avoid instrumenting calls that are in subprogram specifications.
      --  If this subprogram has a previous declaration and calls for default
      --  parameter values, the buffer indices passed to the two witness calls
      --  inserted in the declaration and the body specifications will be
      --  different, and the declaration and body will not be conformant
      --  with each other anymore.

      if Enabled (Fun_Call) and then N.Parent.Kind /= Ada_Subp_Spec then
         if N.Kind = Ada_Call_Stmt then
            for Child of N.Children loop
               if Child.Kind = Ada_Call_Expr then
                  Process_Call_Expression (Child.As_Call_Expr.F_Suffix);
               end if;
            end loop;
         else
            Process_Call_Expression (N);
         end if;
      end if;

      N.Traverse (Process_Decl_Expr'Access);
      N.Traverse (Process_Raise_Expr'Access);
      if Enabled (GExpr) then
         N.Traverse (Process_Case_Expr'Access);
      end if;
   end Process_Expression;

   -----------------------------------
   -- Process_Standalone_Expression --
   -----------------------------------

   procedure Process_Standalone_Expression
     (UIC : in out Ada_Unit_Inst_Context; N : Ada_Node'Class; T : Character)
   is
      Saved_SI : constant Any_MCDC_State_Inserter := UIC.MCDC_State_Inserter;
   begin
      --  The purpose of this Process_Expression wrapper is to avoid creating
      --  local MCDC state variables outside of the expression itself: hide the
      --  current state inserter during the call.

      UIC.MCDC_State_Inserter := null;
      Process_Expression (UIC, N, T);
      UIC.MCDC_State_Inserter := Saved_SI;
   end Process_Standalone_Expression;

   ----------------------------------
   -- Process_Formal_Default_Exprs --
   ----------------------------------

   procedure Process_Formal_Default_Exprs
     (UIC : in out Ada_Unit_Inst_Context; N_Spec : Subp_Spec)
   is
      Saved_Disable_Instrumentation : constant Boolean :=
        UIC.Disable_Instrumentation;
   begin
      if N_Spec.Is_Null then
         return;
      end if;

      UIC.Disable_Instrumentation := True;
      Process_Expression (UIC, N_Spec.F_Subp_Params, 'X');
      UIC.Disable_Instrumentation := Saved_Disable_Instrumentation;
   end Process_Formal_Default_Exprs;

   ------------------
   -- Is_Call_Leaf --
   ------------------

   function Is_Call_Leaf (Node : Ada_Node'Class) return Boolean is
   begin
      case Node.Kind is
         when LALCO.Ada_Name                         =>

            --  The suffix of a Dotted name will designate the same call

            if Node.Kind in Ada_Dotted_Name then
               return False;
            end if;

            --  The prefix of a call expr (that is actually a call) will
            --  designate the same call.

            if Node.Kind in Ada_Call_Expr
              and then Node.As_Call_Expr.P_Kind = Call
            then
               return False;
            end if;

            return
              (Node.As_Name.P_Is_Call
               or else
                 (Node.Parent.Kind = Ada_Call_Expr
                  and then Node.Parent.As_Call_Expr.P_Kind = Call));

         when Ada_Un_Op | Ada_Bin_Op | Ada_Concat_Op =>
            return False;

         when others                                 =>
            return False;
      end case;
   end Is_Call_Leaf;

   ---------------
   -- Full_Call --
   ---------------

   function Full_Call (Node : Ada_Node'Class) return Ada_Node is
      Call : Ada_Node;
   begin

      pragma Assert (Is_Call_Leaf (Node));

      Call := Node.As_Ada_Node;

      --  From a node that is a call, get:
      --
      --  * its parent if it is a Dotted_Name or an operator;
      --  * its parent's parent if it is an operator called via its string
      --    litteral name;
      --  * the operand for concatenation operators (it will instrumented to
      --    hold the witness statement for the operator).

      if Call.Kind in Ada_String_Literal
        and then Call.Parent.Kind in Ada_Dotted_Name
      then
         Call := Call.Parent.Parent;
      end if;

      if Call.Parent.Kind in Ada_Dotted_Name
        or else
          (Call.Kind in Ada_Op
           and then
             Call.Parent.Kind in Ada_Bin_Op | Ada_Un_Op | Ada_Relation_Op)
      then
         Call := Call.Parent;

      elsif Call.Kind = Ada_Op_Concat then
         Call := Call.Parent.As_Concat_Operand.F_Operand.As_Ada_Node;
      end if;

      --  Then, if Call's parent is a Call_Expr, it means that the call has
      --  arguments. We want to take them into account. Otherwise the call
      --  has no arguments and Call is already the outter-most node
      --  representing this call.

      if Call.Parent.Kind in Ada_Call_Expr then
         Call := Call.Parent;
      end if;

      return Call;
   end Full_Call;

   ------------------------------------
   -- Fill_Expression_Insertion_Info --
   ------------------------------------

   procedure Fill_Expression_Insertion_Info
     (UIC : in out Ada_Unit_Inst_Context; Bit : Any_Bit_Id)
   is
      Formal_Name   : constant Node_Rewriting_Handle :=
        Make_Identifier (UIC.Rewriting_Context, "Dummy_Witness_Result");
      Formal_Def_Id : constant Node_Rewriting_Handle :=
        Create_Regular_Node
          (UIC.Rewriting_Context,
           Ada_Defining_Name_List,
           Children =>
             (1 => Create_Defining_Name (UIC.Rewriting_Context, Formal_Name)));
   begin
      --  Create both the witness call and a formal parameter to
      --  accept it as an actual.

      UIC.Current_Insertion_Info.Get.Witness_Actual :=
        Make_Statement_Witness
          (UIC,
           Bit          => Bit,
           Flavor       => Function_Call,
           In_Generic   => UIC.In_Generic,
           In_Decl_Expr => UIC.In_Decl_Expr);

      UIC.Current_Insertion_Info.Get.Witness_Formal :=
        Create_Param_Spec
          (UIC.Rewriting_Context,
           F_Ids          => Formal_Def_Id,
           F_Has_Aliased  => No_Node_Rewriting_Handle,
           F_Mode         => No_Node_Rewriting_Handle,
           F_Type_Expr    => Make_Std_Ref (UIC, "Boolean"),
           F_Default_Expr => No_Node_Rewriting_Handle,
           F_Aspects      => No_Node_Rewriting_Handle);

   end Fill_Expression_Insertion_Info;

   -------------------------
   -- Is_Logical_Operator --
   -------------------------

   function Is_Logical_Operator
     (UIC : Ada_Unit_Inst_Context; N : Ada_Node'Class) return Boolean is
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
            when Ada_Op_Not                       =>
               return True;

            when Ada_Op_And_Then | Ada_Op_Or_Else =>
               return True;

            when Ada_Op_And | Ada_Op_Or           =>

               --  Only consider Op_N as logical operators if we are told "and"
               --  and "or" have short circuit semantics, and that it is a
               --  Standard.Boolean operator.

               return
                 UIC.Short_Circuit_And_Or
                 and then Is_Standard_Boolean_And_Or (Op_N);

            when others                           =>
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
         when Ada_Op_Not                       =>

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

         when Ada_Op_And | Ada_Op_Or           =>

            --  Only consider these as decisions if we are told the operators
            --  have short circuit semantics, and that it is a Standard.Boolean
            --  operator.

            return
              UIC.Short_Circuit_And_Or
              and then Is_Standard_Boolean_And_Or (Op_N);

         when others                           =>
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

      return
        N.P_Referenced_Decl.Is_Null
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
      function Strip_Quotes (WWS : Wide_Wide_String) return Wide_Wide_String
      is (WWS (WWS'First + 1 .. WWS'Last - 1))
      with Pre => WWS (WWS'First) = '"' and WWS (WWS'Last) = '"';

      Op_Sym : constant Wide_Wide_String := Strip_Quotes (Text (Op));
   begin
      if Op_Sym = "+" then
         return "add";
      elsif Op_Sym = "-" then
         return "sub";
      elsif Op_Sym = "*" then
         return "mul";
      elsif Op_Sym = "/" then
         return "div";
      elsif Op_Sym = "**" then
         return "pow";
      elsif Op_Sym = "&" then
         return "concat";
      elsif Op_Sym = "<" then
         return "lt";
      elsif Op_Sym = "<=" then
         return "le";
      elsif Op_Sym = ">" then
         return "gt";
      elsif Op_Sym = ">=" then
         return "ge";
      elsif Op_Sym = "=" then
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
         when Ada_Un_Op  =>
            return N.As_Un_Op.F_Op;

         when Ada_Bin_Op =>
            return N.As_Bin_Op.F_Op;

         when others     =>
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

   function As_Symbol (Id : Identifier) return Symbol_Type
   is (Find (Symbols, Canonicalize (Id.Text).Symbol));

   -----------------
   -- Pragma_Name --
   -----------------

   function Pragma_Name (P : Pragma_Node) return Symbol_Type
   is (As_Symbol (P.F_Id));
   function Pragma_Name (P : Pragma_Node) return Name_Id
   is (As_Name (P.F_Id));

   -------------------
   -- Safe_Is_Ghost --
   -------------------

   function Safe_Is_Ghost (N : Basic_Decl'Class) return Boolean is
   begin
      return not (for all DN of N.P_Defining_Names => not DN.P_Is_Ghost_Code);
   exception
      when E : Property_Error =>
         Report
           (Node => N,
            Msg  =>
              "Could not determine if decl is ghost: "
              & Switches.Exception_Info (E),
            Kind => Low_Warning);
         return False;

   end Safe_Is_Ghost;

   function Safe_Is_Ghost (N : LAL.Stmt'Class) return Boolean is
   begin
      return N.P_Is_Ghost_Code;
   exception
      when E : Property_Error =>
         Report
           (Node => N,
            Msg  =>
              "Could not determine if stmt is ghost: "
              & Switches.Exception_Info (E),
            Kind => Low_Warning);
         return False;

   end Safe_Is_Ghost;

   ---------------------------------
   -- Safe_Previous_Part_For_Decl --
   ---------------------------------

   function Safe_Previous_Part_For_Decl
     (N : Basic_Decl'Class) return Basic_Decl is
   begin
      return N.P_Previous_Part_For_Decl;
   exception
      when E : Property_Error =>
         Report
           (Node => N,
            Msg  =>
              "Could not resolve the previous declaration: "
              & Switches.Exception_Info (E),
            Kind => Warning);
         return No_Basic_Decl;
   end Safe_Previous_Part_For_Decl;

   -------------
   -- Matches --
   -------------

   function Matches
     (P : Pragma_Node; Matchers : Pragma_Matcher_Array) return Boolean
   is
      function As_Symbol (E : Expr) return Symbol_Type;
      --  If E is an identifier or a dotted name involving identifiers only,
      --  return a symbol that represents it. Return null otherwise.

      ---------------
      -- As_Symbol --
      ---------------

      function As_Symbol (E : Expr) return Symbol_Type is
      begin
         if E.Is_Null then
            return No_Symbol;
         end if;

         case Ada_Expr (E.Kind) is
            when Libadalang.Common.Ada_Identifier =>
               return As_Symbol (E.As_Identifier);

            when Ada_Dotted_Name                  =>
               declare
                  DN     : constant Dotted_Name := E.As_Dotted_Name;
                  Prefix : constant Symbol_Type :=
                    As_Symbol (DN.F_Prefix.As_Expr);
               begin
                  return
                    (if Prefix = No_Symbol
                     then No_Symbol
                     else
                       Find
                         (Symbols,
                          Image (Prefix)
                          & "."
                          & Image (As_Symbol (DN.F_Suffix.As_Expr))));
               end;

            when others                           =>
               return No_Symbol;
         end case;
      end As_Symbol;

      Pragma_Name : constant Symbol_Type := As_Symbol (P.F_Id);

      --  Start of processing for Matches

   begin
      for Matcher of Matchers loop

         --  If this matcher accepts the pragma name, inspect its arguments

         if Matcher.Pragma_Name = Pragma_Name then
            for Assoc of P.F_Args loop
               declare
                  A : constant Pragma_Argument_Assoc :=
                    Assoc.As_Pragma_Argument_Assoc;
               begin
                  if Matcher.Assoc_Name = No_Symbol then

                     --  We expect an "Expr_Name" expression

                     if A.F_Name.Is_Null
                       and then As_Symbol (A.F_Expr) = Matcher.Expr_Name
                     then
                        return True;
                     end if;

                  --  We expect a "Assoc_Name => Expr_Name" association

                  elsif As_Symbol (A.F_Name.As_Expr) = Matcher.Assoc_Name
                    and then As_Symbol (A.F_Expr) = Matcher.Expr_Name
                  then
                     return True;
                  end if;
               end;
            end loop;
         end if;
      end loop;

      return False;
   end Matches;

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

   function Aspect_Assoc_Name (A : Aspect_Assoc) return Symbol_Type
   is (As_Symbol (Aspect_Assoc_Name (A)));
   function Aspect_Assoc_Name (A : Aspect_Assoc) return Name_Id
   is (As_Name (Aspect_Assoc_Name (A)));

   --------------
   -- To_Nodes --
   --------------

   function To_Nodes
     (Handle : Rewriting_Handle; Name : Ada_Qualified_Name)
      return Node_Rewriting_Handle
   is
      Result : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
   begin
      for Id of Name loop
         declare
            Id_Node : constant Node_Rewriting_Handle :=
              Make_Identifier (Handle, To_Text (To_String (Id)));
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
      while Unwrapped_N.Kind = Ada_Paren_Expr loop
         Unwrapped_N := Unwrapped_N.As_Paren_Expr.F_Expr;
      end loop;

      return Unwrapped_N;
   end Unwrap;

   ---------------------
   -- In_Package_Spec --
   ---------------------

   function In_Package_Spec (N : Ada_Node'Class) return Boolean is
      Decl_Part : Declarative_Part;
   begin
      if N.Is_Null
        or else N.Parent.Is_Null
        or else N.Parent.Kind /= Ada_Ada_Node_List
        or else N.Parent.Parent.Is_Null
        or else N.Parent.Parent.Kind not in Ada_Declarative_Part_Range
      then
         return False;
      end if;

      Decl_Part := N.Parent.Parent.As_Declarative_Part;
      return
        not Decl_Part.Parent.Is_Null
        and then Decl_Part.Parent.Kind = Ada_Package_Decl;
   end In_Package_Spec;

   ------------------------
   -- Inclusive_End_Sloc --
   ------------------------

   function Inclusive_End_Sloc
     (SL : Source_Location_Range) return Source_Location is
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
     (UIC : Ada_Unit_Inst_Context; E : Expr) return Base_Type_Decl
   is
      ET : Base_Type_Decl;
   begin
      begin
         ET := E.P_Expression_Type;

         if ET.Is_Null then
            Report
              (UIC,
               E,
               "failed to determine expression type (got null type)",
               Warning);
         end if;

      exception
         when Exc : Property_Error =>
            Report
              (UIC,
               E,
               "failed to determine expression type: "
               & Switches.Exception_Info (Exc),
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

   function Is_Static_Expr (E : Expr'Class) return Boolean is
   begin
      return E.P_Is_Static_Expr;
   exception
      when Exc : Property_Error =>
         Report
           (E,
            "failed to determine whether this is a static expression: "
            & Switches.Exception_Info (Exc),
            Low_Warning);
         return False;
   end Is_Static_Expr;

   --------------------
   -- Bool_Expr_Eval --
   --------------------

   function Bool_Expr_Eval (E : Expr) return String is
   begin
      return To_String (Libadalang.Expr_Eval.Expr_Eval (E).Enum_Result.Text);
   exception
      when Exc : Property_Error =>
         Report
           (E,
            "failed to evaluate the expression: "
            & Switches.Exception_Info (Exc),
            Low_Warning);
         return "";
   end Bool_Expr_Eval;

   --------------
   -- Is_Ghost --
   --------------

   function Is_Ghost
     (UIC : Ada_Unit_Inst_Context; D : Basic_Decl) return Boolean
   is
      Decl : Basic_Decl := D;
   begin
      --  We are looking for a Ghost aspect for the given expression function.
      --  If this expression function has a declaration, the aspect must be
      --  there.

      begin
         Decl := Decl.P_Canonical_Part;
      exception
         when Exc : Property_Error =>
            Report
              (UIC,
               Decl,
               "Failed to look for a previous declaration of this expression"
               & " function: "
               & Switches.Exception_Info (Exc),
               Warning);
      end;

      begin
         return Decl.P_Has_Aspect (T_Ghost);
      exception
         when Exc : Property_Error =>
            Report
              (UIC,
               Decl,
               "Failed to look for a Ghost aspect for this declaration: "
               & Switches.Exception_Info (Exc),
               Warning);
            return False;
      end;
   end Is_Ghost;

   ----------------
   -- Is_Generic --
   ----------------

   function Is_Generic
     (UIC : Ada_Unit_Inst_Context; Decl : Basic_Decl'Class) return Boolean
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
              (UIC,
               Decl,
               "Failed to look for a canonical part of this declaration: "
               & Switches.Exception_Info (Exc),
               Warning);
            return False;
      end;

      return
        Canonical_Decl.Kind
        in Ada_Generic_Subp_Decl | Ada_Generic_Package_Decl;
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
     (UIC  : in out Ada_Unit_Inst_Context;
      N    : Ada_Node'Class;
      Decl : Basic_Decl)
   is
      function Local_Sloc
        (Sloc : Source_Location) return Slocs.Local_Source_Location
      is ((Line => Natural (Sloc.Line), Column => Natural (Sloc.Column)));

      Decl_SFI      : constant Source_File_Index :=
        Get_Index_From_Generic_Name
          (Decl.Unit.Get_Filename, Kind => Files_Table.Source_File);
      New_Scope_Ent : constant Scope_Entity :=
        (Source_Range =>
           Slocs.Source_Location_Range'
             (Source_File => UIC.SFI,
              L           =>
                Slocs.Local_Source_Location_Range'
                  (First_Sloc => Local_Sloc (Start_Sloc (N.Sloc_Range)),
                   Last_Sloc  => Local_Sloc (End_Sloc (N.Sloc_Range)))),
         Name         =>
           +Langkit_Support.Text.To_UTF8 (Decl.P_Defining_Name.F_Name.Text),
         Sloc         => Local_Sloc (Sloc (N)),
         Identifier   =>
           (Decl_SFI  => Decl_SFI,
            Decl_Line => Natural (Decl.Sloc_Range.Start_Line)),
         Start_SCO    => SCO_Id (SCOs.SCO_Table.Last + 1));
      Inserted      : Scope_Entities_Trees.Cursor;
   begin
      UIC.Scope_Entities.Insert_Child
        (Parent   => UIC.Current_Scope_Entity,
         Before   => Scope_Entities_Trees.No_Element,
         New_Item => New_Scope_Ent,
         Position => Inserted);
      UIC.Current_Scope_Entity := Inserted;
   end Enter_Scope;

   ----------------
   -- Exit_Scope --
   ----------------

   procedure Exit_Scope (UIC : in out Ada_Unit_Inst_Context) is
      use Scope_Entities_Trees;
      Cur_Scope    : Scope_Entity renames
        Scope_Entities_Trees.Element (UIC.Current_Scope_Entity);
      Parent_Scope : constant Cursor := Parent (UIC.Current_Scope_Entity);
      --  Latch the parent value before UIC.Current_Scope_Entity is freed

   begin
      --  If the scope has no SCO (it could be possible for a package spec with
      --  only subprogram declarations for instance), discard it.

      if SCO_Id (SCOs.SCO_Table.Last) < Cur_Scope.Start_SCO then
         UIC.Scope_Entities.Delete_Leaf (UIC.Current_Scope_Entity);
      end if;

      --  If this is not the top-level scope (we want to keep its reference
      --  after having traversed the AST), go up the scope tree.

      UIC.Current_Scope_Entity := Parent_Scope;
   end Exit_Scope;

   ---------------------------
   -- Start_Statement_Block --
   ---------------------------

   procedure Start_Statement_Block (UIC : in out Ada_Unit_Inst_Context) is
   begin
      if not Switches.Instrument_Block then
         return;
      end if;
      UIC.Block_Stack.Append
        (Block_Information'
           (Block => SCO_Id_Vectors.Empty_Vector, others => <>));
   end Start_Statement_Block;

   -------------------------
   -- End_Statement_Block --
   -------------------------

   procedure End_Statement_Block (UIC : in out Ada_Unit_Inst_Context) is
      Bit : Any_Bit_Id;
   begin
      if not Switches.Instrument_Block then
         return;
      end if;

      declare
         Current_Block : constant Block_Information :=
           UIC.Block_Stack.Last_Element;
      begin
         --  Delete the block if it is empty

         if Current_Block.Block.Is_Empty then
            UIC.Block_Stack.Delete_Last;
            return;
         end if;

         --  Allocate a single statement bit for the last statement (arbitrary)
         --  of the block.

         Bit :=
           Allocate_Statement_Bit
             (UIC.Unit_Bits, Nat (Current_Block.Block.Last_Element));
         Insert_Stmt_Witness
           (UIC             => UIC,
            Stmt_Instr_Info => Current_Block.Last_Stmt_Instr_Info,
            Bit             => Bit);
         UIC.Blocks.Append (Current_Block.Block);
      end;
      UIC.Block_Stack.Delete_Last;
   end End_Statement_Block;

   -------------------------
   -- Insert_Stmt_Witness --
   -------------------------

   procedure Insert_Stmt_Witness
     (UIC             : in out Ada_Unit_Inst_Context;
      Stmt_Instr_Info : Stmt_Instr_Info_Type;
      Bit             : Any_Bit_Id)
   is
      RC                  : Rewriting_Handle renames UIC.Rewriting_Context;
      Insertion_N         : Node_Rewriting_Handle renames
        Stmt_Instr_Info.Insertion_N;
      Instrument_Location : Instrument_Location_Type renames
        Stmt_Instr_Info.Instrument_Location;
      Insert_Info         : Insertion_Info_Ref renames
        Stmt_Instr_Info.Insert_Info_Ref;
   begin
      --  Create an artificial internal error, if requested

      Raise_Stub_Internal_Error_For (Ada_Instrument_Insert_Stmt_Witness);

      case Insert_Info.Get.Method is

         when Statement | Declaration =>

            if Instrument_Location = Inside_Expr then
               declare
                  Old_Cond : Node_Rewriting_Handle := Insertion_N;
                  New_Cond : constant Node_Rewriting_Handle :=
                    Create_Regular_Node
                      (RC,
                       Ada_Bin_Op,
                       Children =>
                         (1 =>
                            Make_Statement_Witness
                              (UIC,
                               Bit          => Bit,
                               Flavor       => Function_Call,
                               In_Generic   => UIC.In_Generic,
                               In_Decl_Expr => Stmt_Instr_Info.In_Decl_Expr),

                          2 => Make (UIC, Ada_Op_Or_Else),

                          --  Placeholder for relocation of old condition
                          --  after it is detached from the tree.

                          3 => No_Node_Rewriting_Handle));

               begin
                  --  Detach old condition from tree and replace it with
                  --  OR ELSE node.

                  Replace (Old_Cond, New_Cond);

                  --  Now reattach old condition in new OR ELSE node. If
                  --  it is AND, OR, XOR, AND THEN binary operation or an
                  --  IF expression, or a quantified expression (FOR ALL,
                  --  FOR SOME), we need to wrap it in parens to generate
                  --  valid code.

                  --  Now reattach old condition in the new OR ELSE node.
                  --  We will wrap it in parens to preserve the semantics
                  --  of the condition.
                  --
                  --  The two operands of the OR ELSE may not have the
                  --  same type (Standard.Boolean for the Witness return
                  --  type). We could convert the result of the witness
                  --  call to the actual type of the expression, but this
                  --  requires "withing" the package in which the derived
                  --  boolean type is defined in case it is not visible.
                  --  Instead, as this is a top-level boolean expression,
                  --  we can simply convert the original expression to
                  --  Standard.Boolean.

                  if UIC.Language_Version in If_Expr_Supported_Versions then
                     Wrap_In_If_Expr (RC, Old_Cond);
                  else
                     Wrap_In_Call_Expr
                       (RC,
                        Prefix => To_Text ("GNATcov_RTS.Std.Boolean"),
                        Node   => Old_Cond);
                  end if;

                  Set_Child (New_Cond, Member_Refs.Bin_Op_F_Right, Old_Cond);
               end;

            else
               declare
                  Ref_Node       : Node_Rewriting_Handle;
                  Insert_Sibling : Boolean;
                  Is_Before      : Boolean;
                  Witness_Call   : Node_Rewriting_Handle;
               begin
                  --  In the case of an accept_statement containing a
                  --  sequence of statements, if Instrument_Location
                  --  is After, we want to call the witness after the
                  --  entry has been accepted, but before the enclosed
                  --  statements are executed, so insert the witness
                  --  call in the inner statement list at first position.

                  if Kind (Insertion_N) = Ada_Accept_Stmt_With_Stmts
                    and then Instrument_Location = After
                  then
                     Ref_Node :=
                       Child
                         (Insertion_N,
                          (Member_Refs.Accept_Stmt_With_Stmts_F_Stmts,
                           Member_Refs.Handled_Stmts_F_Stmts));
                     Insert_Sibling := False;

                  else
                     Ref_Node := Insertion_N;
                     Insert_Sibling := True;

                     --  Set Is_Before according to the requested
                     --  insertion mode.
                     --
                     --  Also update Ref_Node so that it is the
                     --  "reference" node to use for insertion, i.e. the
                     --  sibling in the target insertion list (Ref_List,
                     --  below).
                     --
                     --  Note that the witness is inserted at the current
                     --  location of the statement, so that it will occur
                     --  immediately *before* it in the instrumented
                     --  sources. This is necessary because we want to
                     --  mark a statement as executed anytime it has
                     --  commenced execution (including in cases it
                     --  raises an exception or otherwise transfers
                     --  control). However in some special cases we have
                     --  to insert after the statement, see the comment
                     --  for Instrument_Location_Type.

                     declare
                        Ref_List : Node_Rewriting_Handle;
                     begin
                        case Instrument_Location is
                           when Before        =>
                              Is_Before := True;
                              Ref_List := Insert_Info.Get.RH_List;

                           when Before_Parent =>
                              Is_Before := True;
                              Ref_List := Insert_Info.Get.Parent.RH_List;

                           when After         =>
                              Is_Before := False;
                              Ref_List := Insert_Info.Get.RH_List;

                           --  The cases where we need to instrument
                           --  inside an expression are handled before,
                           --  as they do not trigger the insertion of a
                           --  new statement in a statement list.

                           when Inside_Expr   =>
                              raise Program_Error;
                        end case;

                        while Parent (Ref_Node) /= Ref_List loop
                           Ref_Node := Parent (Ref_Node);
                        end loop;
                     end;
                  end if;

                  --  Insert witness statement or declaration

                  Witness_Call :=
                    Make_Statement_Witness
                      (UIC,
                       Bit          => Bit,
                       Flavor       =>
                         (case Insert_Info.Get.Method is
                            when Statement                  => Procedure_Call,
                            when Declaration                => Declaration,
                            when Expression_Function | None =>
                              raise Program_Error),
                       In_Generic   => UIC.In_Generic,
                       In_Decl_Expr => Stmt_Instr_Info.In_Decl_Expr);

                  if Insert_Sibling then
                     if Is_Before then
                        Insert_Before (Ref_Node, Witness_Call);
                     else
                        Insert_After (Ref_Node, Witness_Call);
                     end if;
                  else
                     Insert_First (Ref_Node, Witness_Call);
                  end if;
               end;
            end if;

         when Expression_Function     =>

            --  Create both the witness call and a formal parameter to
            --  accept it as an actual.

            declare
               RC : constant Rewriting_Handle := UIC.Rewriting_Context;

               Formal_Name   : constant Node_Rewriting_Handle :=
                 Make_Identifier
                   (UIC.Rewriting_Context, "Dummy_Witness_Result");
               Formal_Def_Id : constant Node_Rewriting_Handle :=
                 Create_Regular_Node
                   (RC,
                    Ada_Defining_Name_List,
                    Children => (1 => Create_Defining_Name (RC, Formal_Name)));
            begin
               Insert_Info.Get.Witness_Actual :=
                 Make_Statement_Witness
                   (UIC,
                    Bit          => Bit,
                    Flavor       => Function_Call,
                    In_Generic   => UIC.In_Generic,
                    In_Decl_Expr => Stmt_Instr_Info.In_Decl_Expr);

               Insert_Info.Get.Witness_Formal :=
                 Create_Param_Spec
                   (RC,
                    F_Ids          => Formal_Def_Id,
                    F_Has_Aliased  => No_Node_Rewriting_Handle,
                    F_Mode         => No_Node_Rewriting_Handle,
                    F_Type_Expr    => Make_Std_Ref (UIC, "Boolean"),
                    F_Default_Expr => No_Node_Rewriting_Handle,
                    F_Aspects      => No_Node_Rewriting_Handle);
            end;

         when None                    =>
            raise Program_Error;
      end case;
   end Insert_Stmt_Witness;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   procedure Start_Rewriting
     (Self           : out Ada_Source_Rewriter'Class;
      Instrumenter   : in out Ada_Instrumenter_Type'Class;
      Prj            : Prj_Desc;
      Input_Filename : String)
   is
      Unit : constant Analysis_Unit :=
        Get_From_File (Instrumenter, Input_Filename);
   begin
      if Unit.Has_Diagnostics then
         Outputs.Error ("instrumentation failed for " & Input_Filename);
         Outputs.Error
           ("please make sure the original project can be " & "compiled");
         for D of Unit.Diagnostics loop
            Outputs.Error (Unit.Format_GNU_Diagnostic (D));
         end loop;
         raise Xcov_Exit_Exc;
      end if;
      Start_Rewriting (Self, Instrumenter, Prj, Unit);
   end Start_Rewriting;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   procedure Start_Rewriting
     (Self         : out Ada_Source_Rewriter'Class;
      Instrumenter : in out Ada_Instrumenter_Type'Class;
      Prj          : Prj_Desc;
      Unit         : Analysis_Unit)
   is
      Base_Filename   : constant String :=
        Ada.Directories.Simple_Name (Unit.Get_Filename);
      Output_Filename : constant String := New_File (Prj, Base_Filename);
   begin
      Self.Input_Filename := +Unit.Get_Filename;
      Self.Output_Filename := +Output_Filename;
      Self.Unit := Unit;
      Self.Handle := Start_Rewriting (Instrumenter.Context);
   end Start_Rewriting;

   --------------------
   -- Rewritten_Unit --
   --------------------

   function Rewritten_Unit
     (Self : Ada_Source_Rewriter'Class)
      return Libadalang.Analysis.Analysis_Unit is
   begin
      return Self.Unit;
   end Rewritten_Unit;

   -----------------------------
   -- Unit_Requested_Callback --
   -----------------------------

   overriding
   procedure Unit_Requested_Callback
     (Self               : in out Missing_Src_Reporter;
      Context            : Libadalang.Analysis.Analysis_Context'Class;
      Name               : Langkit_Support.Text.Text_Type;
      From               : Libadalang.Analysis.Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean) is
   begin
      --  We need to warn about sources that we could not find *and* whose
      --  presence is mandated by Ada.

      if Found or else not Is_Not_Found_Error then
         return;
      end if;

      --  Warn for a given source file only once, as Libadalang can invoke this
      --  callback several times. For clarity, only mention the base name,
      --  which should be unique in Ada projects anyway.

      declare
         N : constant Unbounded_String :=
           +Ada.Directories.Simple_Name (Langkit_Support.Text.Image (Name));
      begin
         if Self.Reported_Files.Contains (N) then
            return;
         end if;

         Self.Reported_Files.Include (N);

         --  Warn only if the file does not reside in the GNATcov_RTS
         --  hierarchy.

         declare
            Source_Name : constant String := +N;
         begin
            if not (GNATCOLL.Utils.Starts_With (Source_Name, "gnatcov_rts")
                    or else GNATCOLL.Utils.Starts_With (Source_Name, "gcvrt"))
            then
               --  If we have not done it yet, clarify which file we were
               --  instrumenting when we noticed that the source file N was
               --  missing.

               if Self.Instrumented_File /= "" then
                  Warn
                    ("While instrumenting "
                     & (+Self.Instrumented_File)
                     & "...");
                  Self.Instrumented_File := Null_Unbounded_String;
               end if;

               Warn ("Cannot find required source file: " & Source_Name);
            end if;
         end;
      end;
   end Unit_Requested_Callback;

   ----------------------------------
   -- Create_Missing_File_Reporter --
   ----------------------------------

   function Create_Missing_File_Reporter
      return Libadalang.Analysis.Event_Handler_Reference is
   begin
      return
        Create_Event_Handler_Reference (Missing_Src_Reporter'(others => <>));
   end Create_Missing_File_Reporter;

   ------------------------
   -- Create_LAL_Context --
   ------------------------

   procedure Create_LAL_Context
     (Instrumenter : in out Ada_Instrumenter_Type'Class) is
   begin
      Instrumenter.Context :=
        Create_Context
          (Unit_Provider =>
             Create_Unit_Provider_Reference (Instrumenter.Provider),
           Event_Handler => Instrumenter.Event_Handler,
           File_Reader   => Instrumenter.File_Reader);
      Instrumenter.Get_From_File_Count := 0;

      declare
         Mapping : Config_Pragmas_Mapping;
      begin
         Load_Config_Pragmas_Mapping
           (Mapping, +Instrumenter.Config_Pragmas_Mapping);
         Set_Mapping (Instrumenter.Context, Mapping);
      end;
   end Create_LAL_Context;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Instrumenter : in out Ada_Instrumenter_Type'Class;
      Filename     : String;
      Reparse      : Boolean := False) return Libadalang.Analysis.Analysis_Unit
   is
   begin
      --  If we exceeded the maximum number of calls to Get_From_File, start
      --  with a new context.

      if Instrumenter.Get_From_File_Count >= Max_Get_From_File_Count then
         Create_LAL_Context (Instrumenter);
      end if;
      Instrumenter.Get_From_File_Count := Instrumenter.Get_From_File_Count + 1;

      return Instrumenter.Context.Get_From_File (Filename, Reparse => Reparse);
   end Get_From_File;

   -------------------------------------------
   -- Put_Warnings_And_Style_Checks_Pragmas --
   -------------------------------------------

   procedure Put_Warnings_And_Style_Checks_Pragmas
     (File : in out Text_Files.File_Type) is
   begin
      File.Put_Line ("pragma Style_Checks (Off); pragma Warnings (Off);");
   end Put_Warnings_And_Style_Checks_Pragmas;

   --------------------------
   -- Initialize_Rewriting --
   --------------------------

   procedure Initialize_Rewriting
     (UIC          : in out Ada_Unit_Inst_Context;
      Instrumenter : Ada_Instrumenter_Type'Class) is
   begin
      UIC.Rewriting_Context := Handle (Instrumenter.Context);

      declare
         RH : constant Rewriting_Handle := UIC.Rewriting_Context;
         E  : Instrumentation_Entities renames UIC.Entities;

         function Indexed_Buffer
           (Buffer_Kind : Ada_Qualified_Name) return Node_Rewriting_Handle;
         --  Suffix Buffer_Kind with the buffer index for the currently
         --  instrumented source file. See the comment for the package
         --  Allocated_Bits_Vectors in Instrument.Common for more information.

         --------------------
         -- Indexed_Buffer --
         --------------------

         function Indexed_Buffer
           (Buffer_Kind : Ada_Qualified_Name) return Node_Rewriting_Handle
         is
            Buffer : constant Ada_Qualified_Name :=
              To_Qualified_Name ("Buffers_" & Img (E.Buffers_Index))
              & Buffer_Kind;
         begin
            return
              Create_From_Template
                (Handle    => RH,
                 Template  =>
                   To_Text (To_Ada (UIC.Pure_Buffer_Unit.Unit & Buffer)),
                 Arguments => (1 .. 0 => No_Node_Rewriting_Handle),
                 Rule      => Expr_Rule);
         end Indexed_Buffer;

      begin
         E.Common_Buffers := To_Nodes (RH, Sys_Buffers);
         E.Unit_Buffers := To_Nodes (RH, UIC.Pure_Buffer_Unit.Unit);
         E.Statement_Buffer := Indexed_Buffer (Statement_Buffer_Name);

         if Coverage.Enabled (Decision)
           or else MCDC_Coverage_Enabled
           or else Assertion_Condition_Coverage_Enabled
         then
            E.Decision_Buffer := Indexed_Buffer (Decision_Buffer_Name);

            if MCDC_Coverage_Enabled
              or else Assertion_Condition_Coverage_Enabled
            then
               E.MCDC_Buffer := Indexed_Buffer (MCDC_Buffer_Name);
            end if;
         end if;
      end;
   end Initialize_Rewriting;

   ----------------
   -- Probe_Main --
   ----------------

   function Probe_Main
     (Prj         : Prj_Desc;
      Dump_Config : Any_Dump_Config;
      Rewriter    : Ada_Source_Rewriter'Class)
      return Main_Instrumentation_Description
   is
      U   : Analysis_Unit;
      CU  : LAL.Compilation_Unit;
      Tmp : LAL.Ada_Node;

      Controlled_Types_Available : Boolean;
      Actual_Auto_Dump_Trigger   : Auto_Dump_Trigger;

      Main : Compilation_Unit_Part (Unit_Based_Language) :=
        (Language_Kind => Unit_Based_Language,
         Part          => GPR2.S_Body,
         others        => <>);
      --  Note that we can't get the compilation unit name using the
      --  To_Compilation_Unit_Name overload taking a File_Info parameter,
      --  as the main we are instrumenting there may be the instrumented
      --  version of the original version, in which case it won't belong
      --  to the root project as it will be in the <prj>-gnatcov-instr
      --  directory.

      function Remove_Pragma_Ada_83_Nodes
        (N : Ada_Node'Class) return Visit_Status;

      function Remove_Pragma_Ada_83_Nodes
        (N : Ada_Node'Class) return Visit_Status is
      begin
         if N.Kind in Ada_Pragma_Node
           and then Pragma_Name (N.As_Pragma_Node) = Name_Ada_83
         then
            declare
               H : constant Node_Rewriting_Handle := Handle (N);
            begin
               Remove_Child (H);
            end;
            return Over;
         else
            return Into;
         end if;
      end Remove_Pragma_Ada_83_Nodes;

   begin
      --  Make sure this main source has the expected structure:
      --
      --  * a simple subprogram body in a compilation unit;
      --  * a generic subprogram instantiation
      --
      --  If this source matches none of the above, emit a warning and raise a
      --  Cannot_Instrument_Main_Error exception.

      U := Unit (Handle (Rewriter.Rewritten_Unit));

      Tmp := U.Root;
      if Tmp.Kind /= Ada_Compilation_Unit then
         Stop_Probe_Main (U, "compilation unit expected");
      else
         CU := Tmp.As_Compilation_Unit;
      end if;

      Tmp := CU.F_Body;
      if Tmp.Kind /= Ada_Library_Item then
         Stop_Probe_Main (U, "library item expected");
      end if;

      Controlled_Types_Available :=
        not Finalization_Restricted_In_Unit (U.Context, CU);

      Actual_Auto_Dump_Trigger :=
        (if Dump_Config.Auto_Trigger = Main_End
           and then not Controlled_Types_Available
           and then not Task_Termination_Restricted (U.Context, CU)
         then Ravenscar_Task_Termination
         else Dump_Config.Auto_Trigger);

      Main.Unit := To_Qualified_Name (CU.P_Syntactic_Fully_Qualified_Name);

      Tmp := Tmp.As_Library_Item.F_Item.As_Ada_Node;

      --  Make sure there is no pragma Ada_83 in the instrumented code.

      Traverse (U.Root, Remove_Pragma_Ada_83_Nodes'Access);

      case Tmp.Kind is
         when Ada_Subp_Body                  =>
            declare
               Subp_Body : constant LAL.Subp_Body := Tmp.As_Subp_Body;
            begin
               return
                 (Synthetic                  => False,
                  Main                       => Main,
                  Controlled_Types_Available => Controlled_Types_Available,
                  Actual_Auto_Dump_Trigger   => Actual_Auto_Dump_Trigger,
                  Prelude                    => Handle (CU.F_Prelude),
                  Main_Decls                 =>
                    Handle (Subp_Body.F_Decls.F_Decls),
                  Main_Stmts                 =>
                    Handle (Subp_Body.F_Stmts.F_Stmts),
                  Subp_Body                  => Subp_Body);
            end;

         when Ada_Generic_Subp_Instantiation =>
            declare
               Generic_Wrapper_Body_Filename : Unbounded_String;
               Generic_Wrapper_Body          : Node_Rewriting_Handle;
               Prelude                       : Node_Rewriting_Handle;
               Main_Decls                    : Node_Rewriting_Handle;
               Main_Stmts                    : Node_Rewriting_Handle;
            begin
               Expand_Main_Generic_Instantiation
                 (Tmp.As_Generic_Subp_Instantiation,
                  Prj,
                  Generic_Wrapper_Body_Filename,
                  Generic_Wrapper_Body,
                  Prelude,
                  Main_Decls,
                  Main_Stmts);
               return
                 (Synthetic                     => True,
                  Main                          => Main,
                  Controlled_Types_Available    => Controlled_Types_Available,
                  Actual_Auto_Dump_Trigger      => Actual_Auto_Dump_Trigger,
                  Prelude                       => Prelude,
                  Main_Decls                    => Main_Decls,
                  Main_Stmts                    => Main_Stmts,
                  Generic_Wrapper_Body_Filename =>
                    Generic_Wrapper_Body_Filename,
                  Generic_Wrapper_Body          => Generic_Wrapper_Body);
            end;

         --  Note that a renaming ("procedure X renames ...") cannot be used as
         --  a main program.

         when others                         =>
            Stop_Probe_Main (U, "subprogram body expected");
      end case;
   end Probe_Main;

   ---------------------
   -- Stop_Probe_Main --
   ---------------------

   procedure Stop_Probe_Main (Unit : Analysis_Unit; Message : String) is
      Filename : constant String :=
        Ada.Directories.Simple_Name (Unit.Get_Filename);
   begin
      --  TODO??? Ideally, we would like to display the source location in
      --  Filename that led to abort the instrumentation of this main. This
      --  is not possible today since we are possibly rewriting a source
      --  file that was already instrumented, so slocs do not reflect the
      --  sources that users see.

      Warn ("cannot dump coverage buffers in " & Filename & ": " & Message);
      raise Cannot_Instrument_Main_Error;
   end Stop_Probe_Main;

   ---------------------------------------
   -- Expand_Main_Generic_Instantiation --
   ---------------------------------------

   procedure Expand_Main_Generic_Instantiation
     (Main                          : Generic_Subp_Instantiation;
      Prj                           : Prj_Desc;
      Generic_Wrapper_Body_Filename : out Unbounded_String;
      Generic_Wrapper_Body          : out Node_Rewriting_Handle;
      Prelude                       : out Node_Rewriting_Handle;
      Main_Decls                    : out Node_Rewriting_Handle;
      Main_Stmts                    : out Node_Rewriting_Handle)
   is
      Unit : constant Analysis_Unit := Main.Unit;
      UH   : constant Unit_Rewriting_Handle := Handle (Unit);
      RH   : constant Rewriting_Handle := Handle (Unit.Context);

      function Wrap_Name
        (Orig_Name : Defining_Name; Prefix : String)
         return Node_Rewriting_Handle;
      --  Return a node that is a copy of Orig_Name but with an additional
      --  prefix for the identifier of the designated entity.
      --
      --  For instance:
      --
      --    Wrap_Name (<Foo>, "Prefix") = <Prefix_Foo>
      --    Wrap_Name (<Foo.Bar>, "Prefix") = <Foo.Prefix_Bar>

      Output_Dir : constant String := +Prj.Output_Dir;

      function Filename
        (Unit_Name : Node_Rewriting_Handle; Extension : String) return String;
      --  Return the name of the source file in Output_Dir that contains a unit
      --  of the given name and with the given extension.

      ---------------
      -- Wrap_Name --
      ---------------

      function Wrap_Name
        (Orig_Name : Defining_Name; Prefix : String)
         return Node_Rewriting_Handle
      is
         Result : constant Node_Rewriting_Handle :=
           Clone (Handle (Orig_Name.F_Name));

         --  Get the identifier to rewrite

         Id   : LAL.Name := Orig_Name.F_Name;
         R_Id : Node_Rewriting_Handle := Result;
      begin
         case Id.Kind is
            when LALCO.Ada_Dotted_Name =>
               Id := Id.As_Dotted_Name.F_Suffix.As_Name;
               R_Id := Child (R_Id, Member_Refs.Dotted_Name_F_Suffix);

            when LALCO.Ada_Identifier  =>
               null;

            when others                =>
               Stop_Probe_Main
                 (Unit, "unexpected unit name component: " & Id.Kind'Image);
         end case;

         --  Rewrite it and return the name

         Set_Text (R_Id, To_Text (Prefix) & Id.Text);
         return Result;
      end Wrap_Name;

      --------------
      -- Filename --
      --------------

      function Filename
        (Unit_Name : Node_Rewriting_Handle; Extension : String) return String
      is
         Result : Unbounded_String;

         procedure Visit (N : Node_Rewriting_Handle);
         --  Append to Result N's contribution to the requested filename

         -----------
         -- Visit --
         -----------

         procedure Visit (N : Node_Rewriting_Handle) is
         begin
            case Kind (N) is
               when LALCO.Ada_Identifier  =>
                  Append (Result, To_UTF8 (Text (N)));

               when LALCO.Ada_Dotted_Name =>
                  Visit (Child (N, Member_Refs.Dotted_Name_F_Prefix));
                  Append (Result, '-');
                  Visit (Child (N, Member_Refs.Dotted_Name_F_Suffix));

               when others                =>

                  --  Since we abort rewriting in Wrap_Name above for such
                  --  cases, the following should be unreachable.

                  raise Program_Error
                    with "invalid unit name component: " & Kind (N)'Image;
            end case;
         end Visit;

         --  Start of processing for Filename

      begin
         Visit (Unit_Name);
         Append (Result, Extension);
         return Output_Dir / Ada.Characters.Handling.To_Lower (+Result);
      end Filename;

      Wrapped_Prefix         : constant String := "Xcov_Wrapped_";
      Generic_Wrapper_Prefix : constant String := "Xcov_Genwrap_";
      --  Prefixes used to create names for the additional units created while
      --  expanding this main.

      Generic_Wrapper_Spec_Template : constant Text_Type :=
        "generic" & Chars.LF & "procedure {};" & Chars.LF;
      Generic_Wrapper_Body_Template : constant Text_Type :=
        "with {};"
        & Chars.LF
        & Chars.LF
        & "procedure {} is"
        & Chars.LF
        & "begin"
        & Chars.LF
        & "   {};"
        & Chars.LF
        & "end {};"
        & Chars.LF;
      Main_Template                 : constant Text_Type :=
        "with {};"
        & Chars.LF
        & Chars.LF
        & "procedure {} is new {};"
        & Chars.LF;
      --  Templates used to produce the synthetic sources created while
      --  expanding this main.

      --  Step 1: rename the instantiation and move it to a new unit

      Orig_Name : constant Defining_Name := Main.F_Subp_Name;

      Main_Name            : constant Node_Rewriting_Handle :=
        Clone (Handle (Orig_Name));
      Wrapped_Name         : constant Node_Rewriting_Handle :=
        Wrap_Name (Orig_Name, Wrapped_Prefix);
      Generic_Wrapper_Name : constant Node_Rewriting_Handle :=
        Wrap_Name (Orig_Name, Generic_Wrapper_Prefix);

      Has_Error : Boolean := False;

      --  Start of processing for Expand_Main_Generic_Instantiation

   begin
      --  In order for the renaming to be valid, update all references to this
      --  instantiation in this unit. Emit a warning and skip the renaming if
      --  for some reason we cannot get the references to update.

      begin
         for R of Orig_Name.P_Find_All_References (Units => (1 => Unit)) loop
            case Kind (R) is
               when Precise            =>
                  Replace (Handle (Ref (R)), Clone (Wrapped_Name));

               when No_Ref | Imprecise =>
                  null;

               when LALCO.Error        =>
                  Has_Error := True;
            end case;
         end loop;
      exception
         when Property_Error =>
            Has_Error := True;
      end;
      if Has_Error then
         Report
           (Node => Orig_Name,
            Msg  => "Could not find all references to this subprogram",
            Kind => Low_Warning);
      end if;

      --  Rename the defining name itself and write the resulting unit in a new
      --  source file (it will not change after this).

      Replace (Handle (Orig_Name.F_Name), Clone (Wrapped_Name));
      Write_To_File (UH, Filename (Wrapped_Name, ".ads"));

      --  Step 2: create a generic procedure (in which to insert dumps) wrapper
      --  for it. Do not write the body to a file right now since main
      --  instrumentation may modify it.

      Set_Root
        (UH,
         Create_From_Template
           (RH,
            Generic_Wrapper_Spec_Template,
            (1 => Generic_Wrapper_Name),
            Rule => Compilation_Unit_Rule));
      Write_To_File (UH, Filename (Generic_Wrapper_Name, ".ads"));

      Generic_Wrapper_Body :=
        Create_From_Template
          (RH,
           Generic_Wrapper_Body_Template,
           (1 => Wrapped_Name,
            2 => Generic_Wrapper_Name,
            3 => Wrapped_Name,
            4 => Generic_Wrapper_Name),
           Rule => Compilation_Unit_Rule);
      Generic_Wrapper_Body_Filename :=
        +Filename (Generic_Wrapper_Name, ".adb");

      --  Code that is inserted to dump coverage buffers will land in this
      --  wrapper: set Prelude, Main_Decls and Main_Stmts accordingly.

      Prelude :=
        Child (Generic_Wrapper_Body, Member_Refs.Compilation_Unit_F_Prelude);
      declare
         Subp_Body : constant Node_Rewriting_Handle :=
           Child
             (Generic_Wrapper_Body,
              (Member_Refs.Compilation_Unit_F_Body,
               Member_Refs.Library_Item_F_Item));
      begin
         Main_Decls :=
           Child
             (Subp_Body,
              (Member_Refs.Subp_Body_F_Decls,
               Member_Refs.Declarative_Part_F_Decls));
         Main_Stmts :=
           Child
             (Subp_Body,
              (Member_Refs.Subp_Body_F_Stmts,
               Member_Refs.Handled_Stmts_F_Stmts));
      end;

      --  Step 3: replace the original main spec with the following
      --  instantiation. No need to manually write it to a file: applying the
      --  current rewriting session will do it.

      Set_Root
        (UH,
         Create_From_Template
           (RH,
            Main_Template,
            (1 => Generic_Wrapper_Name,
             2 => Main_Name,
             3 => Generic_Wrapper_Name),
            Rule => Compilation_Unit_Rule));
   end Expand_Main_Generic_Instantiation;

   ---------------------------
   -- Simple_Dump_Proc_Call --
   ---------------------------

   function Simple_Dump_Proc_Call
     (RH : Rewriting_Handle; Helper_Unit : Ada_Qualified_Name)
      return Node_Rewriting_Handle
   is
      Dump_Procedure : Ada_Qualified_Name := Helper_Unit;
   begin
      Dump_Procedure.Append (Dump_Procedure_Name);
      return
        Create_Regular_Node
          (RH, Ada_Call_Stmt, (1 => To_Nodes (RH, Dump_Procedure)));
   end Simple_Dump_Proc_Call;

   -----------------------------------
   -- Insert_Simple_Dump_Proc_Calls --
   -----------------------------------

   procedure Insert_Simple_Dump_Proc_Calls
     (RH          : Rewriting_Handle;
      Helper_Unit : Ada_Qualified_Name;
      Subp_Body   : LAL.Subp_Body)
   is
      --  Simply add a call to the buffer dump procedure at the end of
      --  the top level statement list, as well as at the end of the
      --  statement list of each top level exception handler, and
      --  right before each return statment that applies to the main.

      Call_Stmt : constant Node_Rewriting_Handle :=
        Simple_Dump_Proc_Call (RH, Helper_Unit);
      --  Call invoking the dump procedure

      function Process_Returns (Node : Ada_Node'Class) return Visit_Status;
      --  Helper for LAL's Traverse procedure. If node is a return
      --  statement that returns from the main, insert a call to the
      --  dump buffer procedure right before.

      ---------------------
      -- Process_Returns --
      ---------------------

      function Process_Returns (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind in Ada_Return_Stmt
           and then Return_From_Subp_Body (Node.As_Return_Stmt, Subp_Body)
         then
            --  Child_Index is 0 based whereas Insert_Child is 1 based

            Insert_Before (Handle (Node), Clone (Call_Stmt));
            return Over;
         else
            return Into;
         end if;
      end Process_Returns;

      Handled_Stmt_List : constant Node_Rewriting_Handle :=
        Handle (Subp_Body.F_Stmts.F_Stmts);

      --  Start of processing for Insert_Simple_Dump_Proc_Calls

   begin
      --  Add a Dump_Buffer call at the end of the main's handeled statements

      Insert_Last (Handled_Stmt_List, Call_Stmt);

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
               Insert_Last (Exn_Stmt_List, Clone (Call_Stmt));
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
      Decls       : Node_Rewriting_Handle)
   is
      --  Declare an object of the type <helper unit>.Dump_Controlled_Type
      --  which will call the procedure Dump_Buffers when finalized.

      Controlled_Type_Name : Ada_Qualified_Name := Helper_Unit;
      Dump_Object_Decl     : Node_Rewriting_Handle;
   begin
      Controlled_Type_Name.Append
        (To_Unbounded_String ("Dump_Controlled_Type"));
      Dump_Object_Decl :=
        Create_From_Template
          (RH,
           "GNATcov_Dump_Object : {};",
           (1 => To_Nodes (RH, Controlled_Type_Name)),
           Object_Decl_Rule);

      --  Insert the declaration as the first declaration in the
      --  list to ensure it is finalized last.

      Insert_First (Decls, Dump_Object_Decl);
   end Insert_Controlled_Dump_Object_Decl;

   ----------------------------------
   -- Has_Matching_Pragma_For_Unit --
   ----------------------------------

   function Has_Matching_Pragma_For_Unit
     (Context  : Analysis_Context;
      Unit     : LAL.Compilation_Unit;
      Matchers : Pragma_Matcher_Array) return Boolean
   is
      System_Unit  : constant Analysis_Unit :=
        Context.Get_From_Provider ("System", Unit_Specification);
      Unit_Pragmas : constant Pragma_Node_Array :=
        Unit.P_All_Config_Pragmas
        & (if System_Unit.Has_Diagnostics
           then Pragma_Node_Array'(1 .. 0 => No_Pragma_Node)
           else System_Unit.Root.As_Compilation_Unit.P_All_Config_Pragmas);
      --  Configuration pragmas that apply to Unit. Also append the list of
      --  configuration pragmas defined in System as they often define the set
      --  of restrictions associated with the runtime.

   begin
      if not Unusable_System_Reported and then System_Unit.Has_Diagnostics then
         Diagnostics.Report
           (Msg  =>
              "Could not parse the System unit for the runtime,"
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
         if Matches (Prag_Node, Matchers) then
            return True;
         end if;
      end loop;
      return False;

   end Has_Matching_Pragma_For_Unit;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context : Analysis_Context; Unit : String; Part : Analysis_Unit_Kind)
      return Boolean is
   begin
      return
        Context.Unit_Provider.Get.Get_Unit_Filename (To_Text (Unit), Part)
        /= "";
   end Has_Unit;

   -------------------------------------
   -- Finalization_Restricted_In_Unit --
   -------------------------------------

   function Finalization_Restricted_In_Unit
     (Context : Analysis_Context; Unit : LAL.Compilation_Unit) return Boolean
   is
   begin
      --  GNATCOLL.Projects does not adequately load the runtime for the aamp
      --  target, which means that we end up assuming that finalization is not
      --  restricted. It actually is restricted using the aamp5-small runtime.
      --  The GNATCOLL.Projects bug was fixed, but also workaround the issue
      --  to make it work for non-edge gnatcov, and revert this at next bump.
      --  Ref: eng/shared/anod#300.

      if Switches.Target_Family not in null
        and then Switches.Target_Family.all = "aamp"
      then
         return True;
      end if;
      return
        not Has_Unit (Context, "Ada.Finalization", Unit_Specification)
        or else
          Has_Matching_Pragma_For_Unit
            (Context, Unit, Pragma_Restricts_Finalization_Matchers);
   end Finalization_Restricted_In_Unit;

   ---------------------------------
   -- Task_Termination_Restricted --
   ---------------------------------

   function Task_Termination_Restricted
     (Context : Analysis_Context; Unit : LAL.Compilation_Unit) return Boolean
   is
   begin
      --  See above. Revert this at the next bump as well (eng/shared/anod#300)

      if Switches.Target_Family not in null
        and then Switches.Target_Family.all = "aamp"
      then
         return True;
      end if;
      return
        not Has_Unit (Context, "Ada.Task.Termination", Unit_Specification)
        or else
          not Has_Unit (Context, "Ada.Task.Identification", Unit_Specification)
        or else
          Has_Matching_Pragma_For_Unit
            (Context, Unit, Pragma_Prevents_Task_Termination_Matchers);
   end Task_Termination_Restricted;

   -----------------------------
   -- Entry_Guards_Restricted --
   -----------------------------

   function Entry_Guards_Restricted
     (Context : Analysis_Context; Unit : LAL.Compilation_Unit) return Boolean
   is
   begin
      return
        Has_Matching_Pragma_For_Unit
          (Context, Unit, Pragma_Restricts_Entry_Guards_Matchers);
   end Entry_Guards_Restricted;

   -----------
   -- Apply --
   -----------

   procedure Apply (Self : in out Ada_Source_Rewriter'Class) is
   begin
      Write_To_File (Handle (Self.Unit), +Self.Output_Filename);
      Abort_Rewriting (Self.Handle);
      Self.Finalize;
   end Apply;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out Ada_Source_Rewriter) is
   begin
      if Self.Handle /= No_Rewriting_Handle then
         Abort_Rewriting (Self.Handle);
      end if;
      Self.Unit := No_Analysis_Unit;
   end Finalize;

   -------------------
   -- Write_To_File --
   -------------------

   procedure Write_To_File (Unit : Unit_Rewriting_Handle; Filename : String) is
      use Ada.Streams.Stream_IO;
      use Ada.Strings.Wide_Wide_Unbounded.Aux;

      Source : constant Unbounded_Wide_Wide_String := Unparse (Unit);

      --  To avoid copying the potentially big string for sources on the
      --  secondary stack (and reduce the amount of copies anyway), use the
      --  internal GNAT API to retreive the internal string access and process
      --  it by chunks.

      Source_Access : Big_Wide_Wide_String_Access;
      Length        : Natural;

      Chunk_Size : constant := 4096;
      Position   : Natural;

      --  Note: we need to open and write the instrumented source as a binary
      --  file, to be consistent with Libadalang which retrieves the tokens
      --  from a file opened in binary mode. It provides us with line
      --  terminators when unparsing the user-defined tokens and we must be
      --  careful to output these unaltered. On Windows for example, we get
      --  CRLF line terminators in the LAL tokens, as CR is not automatically
      --  stripped when opening a file in binary mode. If these line
      --  terminators (CRLF) were written in a text file, calls to fwrite
      --  would replace LF with CRLF, resulting in CRCRLF, which is incorrect.

      File : Ada.Streams.Stream_IO.File_Type;
      S    : Stream_Access;
   begin
      Ada.Streams.Stream_IO.Create (File, Out_File, Filename);
      S := Stream (File);

      --  Automatically insert pragmas to disable style checks and warnings
      --  in generated code: it is not our goal to make instrumentation
      --  generate warning-free or well-formatted code.

      String'Write (S, "pragma Style_Checks (Off); pragma Warnings (Off);");

      Get_Wide_Wide_String (Source, Source_Access, Length);
      Position := Source_Access.all'First;

      while Position <= Length loop
         declare
            Chunk_First : constant Natural := Position;
            Chunk_Last  : constant Natural :=
              Natural'Min (Chunk_First + Chunk_Size - 1, Length);

            Chunk         : Wide_Wide_String renames
              Source_Access.all (Chunk_First .. Chunk_Last);
            Encoded_Chunk : constant String :=
              Ada.Characters.Conversions.To_String (Chunk);
         begin
            String'Write (S, Encoded_Chunk);
            Position := Chunk_Last + 1;
         end;
      end loop;

      Close (File);
      if Switches.Pretty_Print then
         Text_Files.Run_GNATformat (Filename);
      end if;
   end Write_To_File;

   -------------------------------
   -- Auto_Dump_Buffers_In_Main --
   -------------------------------

   overriding
   procedure Auto_Dump_Buffers_In_Main
     (Self        : in out Ada_Instrumenter_Type;
      Filename    : String;
      Dump_Config : Any_Dump_Config;
      Prj         : Prj_Desc)
   is
      Rewriter : Ada_Source_Rewriter;
      RH       : Rewriting_Handle renames Rewriter.Handle;

      Helper_Unit : Ada_Qualified_Name;
      --  Name of unit to contain helpers implementing the buffers dump

      Desc : Main_Instrumentation_Description;
   begin
      Start_Rewriting (Rewriter, Self, Prj, Filename);

      --  Try to detect the structure of this main, to determine how to insert
      --  the dump of coverage buffers. In case of failure, let Probe_Main emit
      --  a warning and do nothing.

      begin
         Desc := Probe_Main (Prj, Dump_Config, Rewriter);
      exception
         when Cannot_Instrument_Main_Error =>
            return;
      end;

      --  Emit the helper unit and add a WITH clause for it

      Emit_Dump_Helper_Unit_For_Trigger
        (Dump_Config,
         Dump_Trigger   => Desc.Actual_Auto_Dump_Trigger,
         Instrumenter   => Self,
         Prj            => Prj,
         Main           => Desc.Main,
         Helper_Unit    => Helper_Unit,
         Has_Controlled => Desc.Controlled_Types_Available);

      declare
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

         With_Buffers_Clause : constant Node_Rewriting_Handle :=
           Create_From_Template
             (RH,
              Template  => "with GNATcov_RTS.Buffers;",
              Arguments => (1 .. 0 => No_Node_Rewriting_Handle),
              Rule      => With_Clause_Rule);

         Runtime_Version_Check_Node : constant Node_Rewriting_Handle :=
           Create_From_Template
             (RH,
              Template  => To_Wide_Wide_String (Ada_Runtime_Version_Check),
              Arguments => (1 .. 0 => No_Node_Rewriting_Handle),
              Rule      => Pragma_Rule);

      begin
         Insert_Last (Desc.Prelude, With_Clause);
         Insert_Last (Desc.Prelude, With_RTS_Clause);
         Insert_Last (Desc.Prelude, With_Buffers_Clause);
         Insert_Last (Desc.Prelude, Runtime_Version_Check_Node);
      end;

      --  Depending on the chosen coverage buffers dump trigger, insert the
      --  appropriate code.

      case Desc.Actual_Auto_Dump_Trigger is

         when At_Exit | Ravenscar_Task_Termination =>

            --  Build the call to the registration function and insert it in
            --  the main's declarative part, right before the first
            --  declaration.

            declare
               Register_Function : Ada_Qualified_Name;
               --  Name of the function to register the coverage buffers dump
               --  routine.

               Call_Expr : Node_Rewriting_Handle;
               Call_Decl : Node_Rewriting_Handle;
            begin
               Register_Function := Helper_Unit;
               Register_Function.Append (Register_Dump_Function_Name);
               Call_Expr := To_Nodes (RH, Register_Function);

               Call_Decl :=
                 Create_From_Template
                   (RH,
                    "Autodump_Dummy : {} := {};",
                    (1 => To_Nodes (RH, Witness_Dummy_Type_Name),
                     2 => Call_Expr),
                    Object_Decl_Rule);
               Insert_First (Desc.Main_Decls, Call_Decl);
            end;

         when Main_End                             =>

            --  For Main_End we have three instrumentation schemes depending on
            --  the availability of tasks and controlled types:
            --
            --  - If controlled types are available use a controlled object to
            --    dump the buffers as part of its finalization.
            --
            --  - If controlled types are unavailable but task types are, then
            --    use the Ravenscar_Task_Termination dump trigger as we
            --    otherwise have no instrumentation method that works with all
            --    code constructs. In that case, Actuall_Dump_Trigger is set to
            --    Ravenscar_Task_Termination and we should not reach this part
            --    of the code.
            --
            --  - If we do not have finalization or tasks then simply insert
            --    calls right before all the exit points of the main.

            if Desc.Controlled_Types_Available then
               Insert_Controlled_Dump_Object_Decl
                 (RH, Helper_Unit, Desc.Main_Decls);

            --  Past this, we know that we have to insert a call to the dump
            --  procedure at points where to dump traces.

            elsif Desc.Synthetic then

               --  The code to instrument is synthetic: we do not have regular
               --  nodes, and thus we cannot call
               --  Insert_Simple_Dump_Proc_Calls. Fortunately, in this
               --  situation we know there there is only one place where we
               --  want to dump coverage buffers: at the end of the wrapper
               --  procedure body.

               Insert_Last
                 (Desc.Main_Stmts, Simple_Dump_Proc_Call (RH, Helper_Unit));

            else
               Insert_Simple_Dump_Proc_Calls (RH, Helper_Unit, Desc.Subp_Body);
            end if;

         when None                                 =>
            null;
      end case;

      --  In case we created synthetic sources, write them down before calling
      --  Rewriter.Apply since these sources rely on the rewriting session.

      if Desc.Synthetic then
         declare
            UH         : constant Unit_Rewriting_Handle :=
              Handle (Rewriter.Unit);
            Saved_Root : constant Node_Rewriting_Handle := Root (UH);
         begin
            Set_Root (UH, Desc.Generic_Wrapper_Body);
            Write_To_File (UH, +Desc.Generic_Wrapper_Body_Filename);
            Set_Root (UH, Saved_Root);
         end;
      end if;

      Rewriter.Apply;
   end Auto_Dump_Buffers_In_Main;

   --------------------------------
   -- Replace_Manual_Indications --
   --------------------------------

   overriding
   procedure Replace_Manual_Indications
     (Self                 : in out Ada_Instrumenter_Type;
      Prj                  : in out Prj_Desc;
      Source               : Virtual_File;
      Has_Dump_Indication  : out Boolean;
      Has_Reset_Indication : out Boolean)
   is
      Instrumented_Filename : constant String :=
        +(Prj.Output_Dir & "/" & (+Source.Base_Name));
      Source_Filename       : constant String := +Source.Full_Name;
      Instrumented_Exists   : constant Boolean :=
        Ada.Directories.Exists (Instrumented_Filename);
      File_To_Search        : constant String :=
        (if Instrumented_Exists
         then Instrumented_Filename
         else Source_Filename);
      Unit                  : constant Libadalang.Analysis.Analysis_Unit :=
        Get_From_File (Self, File_To_Search, Reparse => True);
      Rewriter              : Ada_Source_Rewriter;
      External_Annotations  : Instr_Annotation_Map;

      Dummy_Ctx : constant Context_Handle :=
        Create_Context ("Searching manual indications in " & Source_Filename);

      function Find_And_Replace_Pragma
        (N : Ada_Node'Class) return Visit_Status;
      --  If N is the expected pragma statement, replace it with the actual
      --  call.

      -----------------------------
      -- Find_And_Replace_Pragma --
      -----------------------------

      function Find_And_Replace_Pragma (N : Ada_Node'Class) return Visit_Status
      is
         function Is_Expected_Argument
           (Args : Base_Assoc_List; Idx : Positive; Arg : All_Symbols)
            return Boolean
         is ((As_Symbol
                (Args.Child (Idx)
                   .As_Pragma_Argument_Assoc
                   .F_Expr
                   .As_Identifier)
              = As_Symbol (Arg)));
         --  Check if the argument of Prag_N at Index matches Arg

      begin
         if N.Kind = Ada_Pragma_Node then
            declare
               Prag_N    : constant Pragma_Node := N.As_Pragma_Node;
               Prag_Args : constant Base_Assoc_List := Prag_N.F_Args;
            begin
               if Pragma_Name (Prag_N) = Name_Annotate
                 and then Prag_Args.Children_Count in 2 .. 3
                 and then Is_Expected_Argument (Prag_Args, 1, Xcov)
                 and then
                   (Is_Expected_Argument (Prag_Args, 2, Dump_Buffers)
                    or else Is_Expected_Argument (Prag_Args, 2, Reset_Buffers))
               then
                  --  First, check that we are in a statement list, no point in
                  --  generating invalid code.

                  if Prag_N.Parent.Kind not in Ada_Stmt_List then
                     Report
                       (Prag_N,
                        "Incorrect placement for a buffer dump/reset"
                        & " annotation, the pragma should be placed in a"
                        & " statement sequence.",
                        Warning);
                     return Over;
                  end if;

                  --  The pragma statement to be replaced by the actual call
                  --  to Dump_Buffers / Reset_Buffers has been found.

                  if not (Has_Dump_Indication or else Has_Reset_Indication)
                  then
                     Start_Rewriting (Rewriter, Self, Prj, Unit);
                  end if;

                  declare
                     RH         : constant Rewriting_Handle := Rewriter.Handle;
                     With_Unit  : constant Node_Rewriting_Handle :=
                       To_Nodes (RH, Create_Manual_Helper_Unit_Name (Prj));
                     Dump_Call  : constant Node_Rewriting_Handle :=
                       To_Nodes
                         (RH,
                          To_Qualified_Name (To_String (Dump_Procedure_Name)));
                     Dump_Args  : constant Node_Rewriting_Handle :=
                       (if Prag_Args.Children_Count = 3
                        then Detach (Prag_Args.Child (3))
                        else
                          Create_Token_Node
                            (RH,
                             Kind => Ada_String_Literal,
                             Text =>
                               To_Text
                                 ("""" & (To_Ada (Prj.Prj_Name)) & """")));
                     Reset_Call : constant Node_Rewriting_Handle :=
                       To_Nodes
                         (RH,
                          To_Qualified_Name
                            (To_String (Reset_Procedure_Name)));
                  begin
                     --  Add the with clause only once in the file

                     if not (Has_Dump_Indication or else Has_Reset_Indication)
                     then
                        Insert_Last
                          (Handle (Unit.Root.As_Compilation_Unit.F_Prelude),
                           Create_From_Template
                             (RH,
                              Template  => "with {};",
                              Arguments => (1 => With_Unit),
                              Rule      => With_Clause_Rule));
                     end if;

                     if Is_Expected_Argument (Prag_Args, 2, Dump_Buffers) then

                        --  Insert the call to the dump procedure

                        if Switches.Misc_Trace.Is_Active then
                           Switches.Misc_Trace.Trace
                             ("Found buffer dump indication at "
                              & Image (N.Full_Sloc_Image));
                        end if;

                        Replace
                          (Handle (N),
                           Create_From_Template
                             (RH,
                              "{}.{} ({});",
                              Arguments =>
                                (1 => With_Unit,
                                 2 => Dump_Call,
                                 3 => Dump_Args),
                              Rule      => Call_Stmt_Rule));
                        Has_Dump_Indication := True;
                     else

                        --  Insert the call to the reset procedure

                        if Switches.Misc_Trace.Is_Active then
                           Switches.Misc_Trace.Trace
                             ("Found buffer reset indication at "
                              & Image (N.Full_Sloc_Image));
                        end if;

                        Replace
                          (Handle (N),
                           Create_From_Template
                             (RH,
                              "{}.{};",
                              Arguments => (1 => With_Unit, 2 => Reset_Call),
                              Rule      => Call_Stmt_Rule));
                        Has_Reset_Indication := True;
                     end if;
                  end;
                  return Over;
               end if;
            end;
         end if;
         return Into;
      end Find_And_Replace_Pragma;

   begin
      --  If no pragma is found in this file then Start_Rewriting will not be
      --  called. In this case, Rewriter.Handle will not be properly
      --  initialized which will lead to finalization issues. To avoid this,
      --  make sure it is set to No_Rewriting_Handle.

      Has_Dump_Indication := False;
      Has_Reset_Indication := False;
      Rewriter.Handle := No_Rewriting_Handle;

      --  Import external annotations if this file was not already processed by
      --  the regular instrumentation process.

      if not Instrumented_Exists then
         External_Annotations :=
           SS_Annotations.Get_Buffer_Annotations (Source_Filename);
         if not External_Annotations.Is_Empty then
            Start_Rewriting (Rewriter, Self, Prj, Unit);
            Insert_External_Annotations
              (Rewriter.Handle, Unit, External_Annotations);
         end if;
      end if;

      Unit.Root.Traverse (Find_And_Replace_Pragma'Access);

      if Has_Dump_Indication or else Has_Reset_Indication then
         Ada.Directories.Create_Path (+Prj.Output_Dir);
         Rewriter.Apply;
      end if;
   end Replace_Manual_Indications;

   ---------------------------------
   -- Insert_External_Annotations --
   ---------------------------------

   procedure Insert_External_Annotations
     (RH          : in out Rewriting_Handle;
      Unit        : Analysis_Unit;
      Annotations : Instr_Annotation_Map)
   is
      procedure Insert_One (Cur : Instr_Annotation_Maps.Cursor);
      --  Insert a single annotation into the rewritten unit.

      ----------------
      -- Insert_One --
      ----------------

      procedure Insert_One (Cur : Instr_Annotation_Maps.Cursor) is
         Loc      : constant Slocs.Local_Source_Location :=
           Instr_Annotation_Maps.Key (Cur);
         LAL_Loc  : constant Source_Location :=
           (Line   => Langkit_Support.Slocs.Line_Number (Loc.Line),
            Column => Langkit_Support.Slocs.Column_Number (Loc.Column));
         Annot    : constant Instr_Annotation :=
           Instr_Annotation_Maps.Element (Cur);
         Cur_Node : Ada_Node := Unit.Root.Lookup (LAL_Loc);

         Insert_Child : Node_Rewriting_Handle;
         --  Handle of the node before/after which we will insert the pragma

      begin
         --  Exit early if the annotation location does not lie within the AST

         if Cur_Node = No_Ada_Node then
            Warning_Or_Error
              ("Could not find node for source location "
               & Slocs.Image (Loc)
               & " in file "
               & Unit.Get_Filename
               & ", external "
               & Annot.Kind'Image
               & "annotation ignored.");
            return;
         end if;
         while Cur_Node /= No_Ada_Node
           and then Cur_Node.Kind not in Ada_Stmt_List | Ada_Decl_List
         loop
            Cur_Node := Cur_Node.Parent;
         end loop;

         --  Warn if we haven't found a statement list in which to insert the
         --  annotation.

         if Cur_Node = No_Ada_Node then
            Warning_Or_Error
              ("Could not find statement or declaration list enclosing "
               & Slocs.Image (Loc)
               & " in file "
               & Unit.Get_Filename
               & ", external "
               & Annot.Kind'Image
               & " annotation ignored.");
            return;
         end if;

         --  Find the Decl or statment index which closest matches the
         --  annotation location.

         declare
            Child : Ada_Node := Cur_Node.First_Child;
         begin
            while Child /= No_Ada_Node
              and then Compare (Child, LAL_Loc) = After
            loop
               Child := Child.Next_Sibling;
            end loop;

            --  We cannot have an empty list, otherwise there would be no
            --  source location range associated to it.

            if Child = No_Ada_Node then
               Insert_Child := Last_Child (Handle (Cur_Node));
            else
               Insert_Child := Handle (Child);
            end if;
         end;

         declare
            Prag_Stmt : Node_Rewriting_Handle;
         begin
            --  Create the proper pragma based on the annotation

            case Annot.Kind is
               when Dump_Buffers     =>
                  Prag_Stmt :=
                    Create_From_Template
                      (RH,
                       "pragma Annotate (Xcov, Dump_Buffers"
                       & (if Length (Annot.Trace_Prefix) /= 0
                          then To_Text (", " & (+Annot.Trace_Prefix))
                          else "")
                       & ");",
                       (1 .. 0 => No_Node_Rewriting_Handle),
                       Pragma_Rule);

               when Reset_Buffers    =>
                  Prag_Stmt :=
                    Create_From_Template
                      (RH,
                       "pragma Annotate (Xcov, Reset_Buffers);",
                       (1 .. 0 => No_Node_Rewriting_Handle),
                       Pragma_Rule);

               when Cov_Off | Cov_On =>
                  raise Program_Error with "Unreachable";
            end case;
            --  Insert the proper annotation pragma in the statement list, in
            --  the right location.

            if Annot.Insert_After then
               Insert_After (Insert_Child, New_Sibling => Prag_Stmt);
            else
               Insert_Before (Insert_Child, New_Sibling => Prag_Stmt);
            end if;
         end;
      end Insert_One;

      --  Start of processing for Insert_External_Annotations

   begin
      Annotations.Iterate (Insert_One'Access);

      --  Apply the rewriting **in the LAL tree** to not mess with rewriting
      --  indices in the instrumentation process, and to make the pragmas
      --  visible in the analysis tree.

      declare
         Apply_Res : constant Apply_Result := Libadalang.Rewriting.Apply (RH);
      begin
         if not Apply_Res.Success then
            Warning_Or_Error
              ("Failed to import external buffer annotations for "
               & Unit.Get_Filename
               & ", they will be ignored.");
            for D of Apply_Res.Diagnostics loop
               Warning_Or_Error (Unit.Format_GNU_Diagnostic (D));
            end loop;
            Abort_Rewriting (RH);
         end if;
      end;
   end Insert_External_Annotations;

   ------------------------------
   -- Insert_With_Dump_Helper --
   ------------------------------

   procedure Insert_With_Dump_Helper
     (Self   : in out Ada_Instrumenter_Type;
      Source : GNATCOLL.VFS.Virtual_File;
      Prj    : in out Prj_Desc)
   is
      Instrumented_Filename : constant String :=
        +(Prj.Output_Dir & "/" & GNATCOLL.VFS."+" (Source.Base_Name));
      Source_Filename       : constant String :=
        GNATCOLL.VFS."+" (Source.Full_Name);
      Instrumented_Exists   : constant Boolean :=
        Ada.Directories.Exists (Instrumented_Filename);
      File_To_Search        : constant String :=
        (if Instrumented_Exists
         then Instrumented_Filename
         else Source_Filename);
      Unit                  : constant Libadalang.Analysis.Analysis_Unit :=
        Get_From_File (Self, File_To_Search, Reparse => True);

      Rewriter : Ada_Source_Rewriter;

      Dummy_Ctx : constant Context_Handle :=
        Create_Context ("Inserting with dump helper in " & Source_Filename);

   begin
      Ada.Directories.Create_Path (+Prj.Output_Dir);

      Start_Rewriting (Rewriter, Self, Prj, Unit);
      Insert_Last
        (Handle (Unit.Root.As_Compilation_Unit.F_Prelude),
         Create_From_Template
           (Rewriter.Handle,
            Template  => "with {};",
            Arguments =>
              (1 =>
                 To_Nodes
                   (Rewriter.Handle, Create_Manual_Helper_Unit_Name (Prj))),
            Rule      => With_Clause_Rule));
      Rewriter.Apply;
   end Insert_With_Dump_Helper;

   ----------------------------
   -- Instrument_Source_File --
   ----------------------------

   procedure Instrument_Source_File
     (UIC          : in out Ada_Unit_Inst_Context;
      Filename     : String;
      Instrumenter : in out Ada_Instrumenter_Type;
      Prj          : Prj_Desc)
   is
      CU_Name   : Compilation_Unit_Part (Language_Kind => Unit_Based_Language);
      Rewriter  : Ada_Source_Rewriter;
      Dummy_Ctx : constant Context_Handle :=
        Create_Context ("Instrumenting " & Filename);

      Root_Analysis_Unit : Analysis_Unit;

      Preelab : Boolean := False;
      --  Set to True if Unit is required to be preelaborable, i.e. it is
      --  either preelaborated, or the declaration of a remote types or remote
      --  call interface library unit. In this case, do not generate any
      --  witness calls for elaboration of library-level declarations: they
      --  would be pointless (there is no elaboration code anyway) and illegal
      --  (because of the No_Elaboration_Code restriction).

      Has_Pragma_SCAO : Boolean;
      --  Whether there is a pragma Short_Circuit_And_Or that applies to this
      --  unit.
   begin
      --  Reset the language version to the default of the program to avoid
      --  a 'pragma <Ada_Version>' to be propagated from the spec to body.

      UIC.Language_Version := Switches.Global_Language_Version;

      Start_Rewriting (Rewriter, Instrumenter, Prj, Filename);

      Root_Analysis_Unit := Rewriter.Rewritten_Unit;

      --  Insert pragmas for the external annotations before doing anything.
      --  This will close the rewriting session so we'll need to re start it
      --  again if there were any annotations.

      declare
         Buffer_Annots : constant Instr_Annotation_Map :=
           Get_Buffer_Annotations (Filename);
      begin
         if not Buffer_Annots.Is_Empty then
            Insert_External_Annotations
              (Rewriter.Handle, Root_Analysis_Unit, Buffer_Annots);
            Rewriter.Handle := Start_Rewriting (Instrumenter.Context);
         end if;
      end;

      UIC.Root_Unit := Root_Analysis_Unit.Root.As_Compilation_Unit;
      UIC.Current_Scope_Entity := UIC.Scope_Entities.Root;

      --  Determine whether Unit is required to be preelaborable, and whether
      --  we can insert witness calls (which are not preelaborable).

      CU_Name.Part := +UIC.Root_Unit.P_Unit_Kind;

      --  P_Unit_Kind does not detect separates, so manually inspect the
      --  compilation unit's contents.

      if UIC.Root_Unit.F_Body.Kind = Ada_Subunit then
         CU_Name.Part := GPR2.S_Separate;
      end if;

      CU_Name.Unit :=
        To_Qualified_Name (UIC.Root_Unit.P_Decl.P_Fully_Qualified_Name_Array);

      begin
         --  Library level declarations can be instrumented only when
         --  elaboration code is allowed.

         Preelab :=
           Decls_Are_Library_Level (UIC.Root_Unit)
           and then
             (UIC.Root_Unit.P_Is_Preelaborable
              or else
                UIC.Root_Unit.P_Has_Restriction
                  (To_Unbounded_Text ("No_Elaboration_Code"))
              or else UIC.Has_No_Elaboration_Code_All);
      exception
         when Libadalang.Common.Property_Error =>
            Report
              (Msg  =>
                 "failed to determine preelaboration constraint for "
                 & Filename,
               Kind => Warning);
            Preelab := False;
      end;

      --  Reset the unit inst context for the currently instrumented source
      --  file.

      UIC.Annotations.Clear;
      UIC.Scope_Entities := Scope_Entities_Trees.Empty_Tree;
      UIC.Current_Scope_Entity := UIC.Scope_Entities.Root;
      UIC.Source_Decisions := Source_Decision_Vectors.Empty;
      UIC.Source_Conditions := Source_Condition_Vectors.Empty;
      UIC.Unit_Bits.Last_Statement_Bit := No_Bit_Id;
      UIC.Unit_Bits.Last_Outcome_Bit := No_Bit_Id;
      UIC.Unit_Bits.Last_Path_Bit := No_Bit_Id;
      UIC.Unit_Bits.Decision_Bits := LL_Decision_SCO_Bit_Allocs.Empty;
      UIC.Unit_Bits.Statement_Bits := LL_Statement_SCO_Bit_Allocs.Empty;
      UIC.Block_Stack := Block_Stacks.Empty_Vector;
      UIC.Blocks := SCO_Id_Vector_Vectors.Empty_Vector;

      --  Instrumentation of another source file in the same unit may have
      --  produced non-instrumented low-level SCOs in UIC.Non_Instr_LL_SCOs,
      --  which were later converted to high-level SCOS, and the low-level SCO
      --  table was since then cleared, so UIC.Non_Instr_LL_SCOs may contain
      --  stale entries: clear it.

      UIC.Non_Instr_LL_SCOs.Clear;

      UIC.True_Static_LL_SCOs.Clear;
      UIC.False_Static_LL_SCOs.Clear;

      Initialize_Rewriting (UIC, Instrumenter);
      UIC.Instrumented_Unit := CU_Name;

      begin
         Has_Pragma_SCAO :=
           UIC.Root_Unit.P_Config_Pragmas
             (To_Unbounded_Text ("Short_Circuit_And_Or"))'Length
           /= 0;
      exception
         when Exc : Property_Error =>
            Report
              (UIC,
               UIC.Root_Unit,
               "Could not determine pragmas of the compilation unit: "
               & Switches.Exception_Info (Exc),
               Kind => Low_Warning);
            Has_Pragma_SCAO := False;
      end;

      UIC.Short_Circuit_And_Or :=
        Switches.Short_Circuit_And_Or or else Has_Pragma_SCAO;

      --  Create an artificial internal error, if requested

      Raise_Stub_Internal_Error_For (Ada_Instrument_Start_File);

      --  Make sure that the simple name of the instrumented source file is
      --  registered in our tables. This is required to properly detect when we
      --  try to load SCOs for the same unit from an ALI file, as ALI files
      --  only provide simple names.

      UIC.SFI :=
        Get_Index_From_Generic_Name
          (Filename, Kind => Files_Table.Source_File);
      UIC.Fullname := +Filename;
      UIC.Unit_Bits.SFI := UIC.SFI;

      --  Then run SCOs generation. This inserts calls to witness
      --  procedures/functions in the same pass.

      SCOs.Initialize;

      --  Append an entry in the SCO_Unit_Table for this unit to preserve
      --  invariants in our data structures and subsequently avoiding to have
      --  UIC.CU set to No_CU_Id.

      Append_Unit (UIC.SFI);

      declare
         Annots : constant Instr_Annotation_Map :=
           Get_Disabled_Cov_Annotations (Filename);
      begin
         UIC.Populate_Ext_Disabled_Cov (Annots, UIC.SFI);
      end;

      Traverse_Declarations_Or_Statements
        (UIC      => UIC,
         L        => No_Ada_List,
         Preelab  => Preelab,
         P        => Rewriter.Rewritten_Unit.Root,
         Is_Block => False);

      --  Check that there are no open statement blocks at the end of the
      --  instrumentation processs.

      pragma Assert (UIC.Block_Stack.Is_Empty);

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
            Count_Paths   => True,
            Attached_Ctx  =>
              Instr_Attached_Ctx'
                (True_Static_SCOs  => UIC.True_Static_LL_SCOs,
                 False_Static_SCOs => UIC.False_Static_LL_SCOs));

         --  In the instrumentation case, the origin of SCO information is
         --  the original source file.

         UIC.CU := Created_Units.Element (UIC.SFI);

         --  Import annotations in our internal tables

         UIC.Import_Annotations (Created_Units);

         --  Import external exemption annotations

         for Cur in Created_Units.Iterate loop
            Import_External_Exemptions (Created_Unit_Maps.Key (Cur));
         end loop;

         --  Import non-instrumented SCOs in the internal tables

         Import_Non_Instrumented_LL_SCOs (UIC, SCO_Map);

         --  Insert calls to condition/decision witnesses

         if Coverage.Enabled (Decision)
           or else MCDC_Coverage_Enabled
           or else Assertion_Condition_Coverage_Enabled
         then
            for SD of UIC.Source_Decisions loop
               declare
                  HL_SCO            : constant SCO_Id := SCO_Map (SD.LL_SCO);
                  Should_Instrument : constant Boolean :=
                    ((not SD.Is_Contract
                      and then
                        (Coverage.Enabled (Decision)
                         or else MCDC_Coverage_Enabled))
                     or else
                       (SD.Is_Contract and then Assertion_Coverage_Enabled));
               begin
                  if Should_Instrument then
                     Insert_Decision_Witness (UIC, SD, Path_Count (HL_SCO));
                  end if;
               end;
            end loop;

            if MCDC_Coverage_Enabled
              or else Assertion_Condition_Coverage_Enabled
            then
               --  As high-level SCO tables have been populated, we have built
               --  BDDs for each decision, and we can now set the correct MC/DC
               --  path offset for each condition.
               --
               --  We do not include a witness call for conditions which appear
               --  in a decision with a path count exceeding the limit to avoid
               --  generating overly large traces / run out of memory.
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
                     --  If the number of paths in the decision binary diagram
                     --  exceeds the path count limit or if we could not find a
                     --  way to create a MC/DC state local variable to track
                     --  the path, we do not instrument the condition.

                     if SC.State = ""
                       or else Path_Count (Decision) > Get_Path_Count_Limit
                     then
                        Set_Decision_SCO_Non_Instr_For_MCDC (Decision);
                     else
                        Insert_Condition_Witness
                          (UIC, SC, Offset_For_True (Condition));
                     end if;
                  end;
               end loop;
            end if;
         end if;

         --  Witnesses have now been inserted, and bit indices allocated: build
         --  bit maps.

         Bit_Maps :=
           (Statement_Bits =>
              new Statement_Bit_Map'
                (Bit_Id'First .. UIC.Unit_Bits.Last_Statement_Bit =>
                   No_SCO_Id),
            Decision_Bits  =>
              new Decision_Bit_Map'
                (Bit_Id'First .. UIC.Unit_Bits.Last_Outcome_Bit =>
                   (No_SCO_Id, False)),
            MCDC_Bits      =>
              new MCDC_Bit_Map'
                (Bit_Id'First .. UIC.Unit_Bits.Last_Path_Bit =>
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

               if (MCDC_Coverage_Enabled
                   or else Assertion_Condition_Coverage_Enabled)
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
         Set_Scope_Entities (UIC.CU, UIC.Scope_Entities);

         --  Remap low level SCOs in blocks to high level SCOs

         Remap_Blocks (UIC.Blocks, SCO_Map);
         Set_Blocks (UIC.CU, UIC.Blocks);
      end;

      --  Emit the instrumented source file

      Rewriter.Apply;

      --  Track which CU_Id maps to which instrumented unit

      Instrumented_Unit_CUs.Insert (CU_Name, UIC.CU);

      --  Update the Ignore_Status of the CU we instrumented

      Files_Table.Consolidate_Ignore_Status
        (Index  =>
           Files_Table.Get_Index_From_Generic_Name
             (Name => Filename, Kind => Files_Table.Source_File),
         Status => Files_Table.Never);
   end Instrument_Source_File;

   -----------------------
   -- Buffers_List_Unit --
   -----------------------

   function Buffers_List_Unit
     (Project_Name : Ada_Qualified_Name) return Ada_Qualified_Name
   is
      Project_Name_Slug : constant String :=
        Qualified_Name_Slug (Project_Name, Use_Hash => False);
   begin
      return
        Ada_Identifier_Vectors."&"
          (Sys_Prefix, Ada_Identifier (Unbounded_String'(+Project_Name_Slug)));
   end Buffers_List_Unit;

   -----------------
   -- Buffer_Unit --
   -----------------

   function Buffer_Unit
     (Unit_Name : Ada_Qualified_Name) return Ada_Qualified_Name
   is
      Simple_Name : Ada_Identifier;
   begin
      Append (Simple_Name, "B");
      Append
        (Simple_Name,
         Ada_Identifier (Unbounded_String'(+Qualified_Name_Slug (Unit_Name))));
      return CU_Name : Ada_Qualified_Name := Sys_Prefix do
         CU_Name.Append (Simple_Name);
      end return;
   end Buffer_Unit;

   ----------------------
   -- Pure_Buffer_Unit --
   ----------------------

   function Pure_Buffer_Unit
     (Unit_Name : Ada_Qualified_Name) return Ada_Qualified_Name
   is
      Simple_Name : Instrument.Ada_Identifier;
   begin
      Append (Simple_Name, 'P');
      Append
        (Simple_Name,
         Ada_Identifier (Unbounded_String'(+Qualified_Name_Slug (Unit_Name))));
      return CU_Name : Ada_Qualified_Name := Sys_Prefix do
         CU_Name.Append (Simple_Name);
      end return;
   end Pure_Buffer_Unit;

   ----------------------
   -- Emit_Buffer_Unit --
   ----------------------

   procedure Emit_Buffer_Unit
     (Buffer_Unit : Compilation_Unit_Part;
      Prj         : Prj_Desc;
      Unit        : Files_Table.Compilation_Unit;
      Unit_Bits   : Allocated_Bits_Vectors.Vector;
      CU_Names    : CU_Name_Vectors.Vector;
      CUs         : CU_Id_Vectors.Vector)
   is
      Pkg_Name : constant String := To_Ada (Buffer_Unit.Unit);
      --  Package name for the buffer unit

      Filename : constant String := To_Filename (Prj, Buffer_Unit);

      File              : Text_Files.File_Type;
      Last_Buffer_Index : constant Natural := Natural (Unit_Bits.Length);

      T : Translate_Set :=
        Assoc ("UNIT_NAME", Pkg_Name)
        & Assoc ("UNIT_BUFFERS_NAME", Unit_Buffers_Name (Unit))
        & Assoc ("LAST_INDEX", Last_Buffer_Index'Image);
   begin
      Trace_Buffer_Unit (Pkg_Name, Filename, Prj, CU_Names, Is_Pure => False);
      Create_File (Prj, File, Filename);

      declare
         Buffer_Indexes    : Vector_Tag;
         Buffer_Unit_Names : Vector_Tag;
         Buffer_Unit_Parts : Vector_Tag;

         SCOs_Fingerprints        : Vector_Tag;
         Bit_Maps_Fingerprints    : Vector_Tag;
         Annotations_Fingerprints : Vector_Tag;

         Statement_Last_Bits      : Vector_Tag;
         Statement_Buffer_Symbols : Vector_Tag;

         Decision_Last_Bits      : Vector_Tag;
         Decision_Buffer_Symbols : Vector_Tag;

         MCDC_Last_Bits      : Vector_Tag;
         MCDC_Buffer_Symbols : Vector_Tag;
      begin
         for I in 1 .. Last_Buffer_Index loop
            declare
               Unit_Bit : constant Allocated_Bits := Unit_Bits.Element (I);
               CU_Name  : constant Compilation_Unit_Part :=
                 CU_Names.Element (I);
               CU       : constant CU_Id := CUs.Element (I);

               Unit_Name : constant String :=
                 Ada.Characters.Handling.To_Lower (To_Ada (CU_Name.Unit));
               --  Lower-case name for the instrumented unit

               Unit_Part : constant String :=
                 (case CU_Name.Part is
                    when GPR2.S_Spec     => "Unit_Spec",
                    when GPR2.S_Body     => "Unit_Body",
                    when GPR2.S_Separate => "Unit_Separate");
               --  Do not use 'Image so that we use the original casing for the
               --  enumerators, and thus avoid compilation warnings/errors.

               SCOs_Fingerprint : constant SC_Obligations.Fingerprint_Type :=
                 SC_Obligations.Fingerprint (CU);

            begin
               Append (Buffer_Indexes, Img (I));
               Append (Buffer_Unit_Names, Unit_Name);
               Append (Buffer_Unit_Parts, Unit_Part);

               Append
                 (SCOs_Fingerprints, Format_Fingerprint (SCOs_Fingerprint));
               Append
                 (Bit_Maps_Fingerprints,
                  Format_Fingerprint
                    (SC_Obligations.Bit_Maps_Fingerprint
                       (CU, SCOs_Fingerprint)));
               Append
                 (Annotations_Fingerprints,
                  Format_Fingerprint
                    (SC_Obligations.Annotations_Fingerprint
                       (CU, SCOs_Fingerprint)));

               Append (Statement_Last_Bits, Img (Unit_Bit.Last_Statement_Bit));
               Append
                 (Statement_Buffer_Symbols, Statement_Buffer_Symbol (CU_Name));

               Append (Decision_Last_Bits, Img (Unit_Bit.Last_Outcome_Bit));
               Append
                 (Decision_Buffer_Symbols, Decision_Buffer_Symbol (CU_Name));

               Append (MCDC_Last_Bits, Img (Unit_Bit.Last_Path_Bit));
               Append (MCDC_Buffer_Symbols, MCDC_Buffer_Symbol (CU_Name));
            end;
         end loop;

         T :=
           T
           & Assoc ("BUFFER_IDX", Buffer_Indexes)
           & Assoc ("BUF_UNIT_NAME", Buffer_Unit_Names)
           & Assoc ("BUF_UNIT_PART", Buffer_Unit_Parts)
           & Assoc ("SCOS_FINGERPRINT", SCOs_Fingerprints)
           & Assoc ("BIT_MAPS_FINGERPRINT", Bit_Maps_Fingerprints)
           & Assoc ("ANNOTATIONS_FINGERPRINT", Annotations_Fingerprints)

           & Assoc ("STMT_LAST", Statement_Last_Bits)
           & Assoc ("STMT_BUF_SYM", Statement_Buffer_Symbols)

           & Assoc ("DECISION_LAST", Decision_Last_Bits)
           & Assoc ("DECISION_BUF_SYM", Decision_Buffer_Symbols)

           & Assoc ("MCDC_LAST", MCDC_Last_Bits)
           & Assoc ("MCDC_BUF_SYM", MCDC_Buffer_Symbols);
      end;

      File.Put (Render_Template ("buffer_unit.ads.tmplt", T));
   end Emit_Buffer_Unit;

   ---------------------------
   -- Emit_Pure_Buffer_Unit --
   ---------------------------

   procedure Emit_Pure_Buffer_Unit
     (PB_Unit                        : Compilation_Unit_Part;
      Prj                            : Prj_Desc;
      CU_Names                       : CU_Name_Vectors.Vector;
      Language_Version               : Unbounded_Wide_Wide_String;
      Degenerate_Subprogram_Generics : Generic_Subp_Vectors.Vector;
      Has_No_Elaboration_Code_All    : Boolean)
   is
      Last_Buffer_Index : constant Natural := Natural (CU_Names.Length);
      Pkg_Name          : constant String := To_Ada (PB_Unit.Unit);
      Filename          : constant String :=
        New_File (Prj, To_Filename (Prj, PB_Unit));
      File              : Text_Files.File_Type;

      T : Translate_Set :=
        Assoc ("UNIT_NAME", Pkg_Name)
        & Assoc
            ("ADA_VERSION", To_String (To_Wide_Wide_String (Language_Version)))
        & Assoc ("ADA_RUNTIME_VERSION_CHECK", Ada_Runtime_Version_Check)
        & Assoc ("HAS_NO_ELABORATION_CODE_ALL", Has_No_Elaboration_Code_All);

      --  Start of processing for Emit_Pure_Buffer_Unit

   begin
      Trace_Buffer_Unit (Pkg_Name, Filename, Prj, CU_Names, Is_Pure => True);

      declare
         Buffer_Indexes       : Vector_Tag;
         Stmt_Buf_Symbols     : Vector_Tag;
         Decision_Buf_Symbols : Vector_Tag;
         MCDC_Buf_Symbols     : Vector_Tag;
      begin
         for I in 1 .. Last_Buffer_Index loop
            declare
               CU_Name : constant Compilation_Unit_Part :=
                 CU_Names.Element (I);
            begin
               Append (Buffer_Indexes, Img (I));
               Append (Stmt_Buf_Symbols, Statement_Buffer_Symbol (CU_Name));
               Append (Decision_Buf_Symbols, Decision_Buffer_Symbol (CU_Name));
               Append (MCDC_Buf_Symbols, MCDC_Buffer_Symbol (CU_Name));
            end;
         end loop;

         T :=
           T
           & Assoc ("BUFFER_IDX", Buffer_Indexes)
           & Assoc ("STMT_BUF_SYM", Stmt_Buf_Symbols)
           & Assoc ("DECISION_BUF_SYM", Decision_Buf_Symbols)
           & Assoc ("MCDC_BUF_SYM", MCDC_Buf_Symbols);
      end;

      declare
         Degenerate_Subp_Generics : Vector_Tag;
      begin
         for G of Degenerate_Subprogram_Generics loop
            Append
              (Degenerate_Subp_Generics,
               To_String (To_Wide_Wide_String (G.Generic_Subp_Decl)));
         end loop;

         T := T & Assoc ("DEGENERATE_SUBP_GENERIC", Degenerate_Subp_Generics);
      end;

      File.Create (Filename);
      File.Put (Render_Template ("pure_buffer_unit.ads.tmplt", T));
      Text_Files.Close (File);
      if Switches.Pretty_Print then
         Text_Files.Run_GNATformat (Filename);
      end if;

      if not Degenerate_Subprogram_Generics.Is_Empty then
         declare
            PB_Unit_Body             : constant Compilation_Unit_Part :=
              (Language_Kind => Unit_Based_Language,
               Unit          => PB_Unit.Unit,
               Part          => GPR2.S_Body);
            PB_Filename              : constant String :=
              New_File (Prj, To_Filename (Prj, PB_Unit_Body));
            Degenerate_Subp_Generics : Vector_Tag;
         begin
            for G of Degenerate_Subprogram_Generics loop
               Append
                 (Degenerate_Subp_Generics,
                  To_String (To_Wide_Wide_String (G.Generic_Subp_Body)));
            end loop;

            T :=
              T & Assoc ("DEGENERATE_SUBP_GENERIC", Degenerate_Subp_Generics);

            File.Create (PB_Filename);
            File.Put (Render_Template ("pure_buffer_unit.adb.tmplt", T));
            Text_Files.Close (File);
            if Switches.Pretty_Print then
               Text_Files.Run_GNATformat (PB_Filename);
            end if;
         end;
      end if;
   end Emit_Pure_Buffer_Unit;

   -----------------------------
   -- Dump_Manual_Helper_Unit --
   -----------------------------

   overriding
   function Dump_Manual_Helper_Unit
     (Self : Ada_Instrumenter_Type; Prj : Prj_Desc)
      return Files_Table.Compilation_Unit
   is
      pragma Unreferenced (Self);
   begin
      return
        (Language  => Unit_Based_Language,
         Unit_Name => +To_Ada (Create_Manual_Helper_Unit_Name (Prj)));
   end Dump_Manual_Helper_Unit;

   ------------------------------------
   -- Create_Manual_Helper_Unit_Name --
   ------------------------------------

   function Create_Manual_Helper_Unit_Name
     (Prj : Prj_Desc) return Ada_Qualified_Name
   is
      Helper_Unit : Ada_Qualified_Name;
   begin
      Helper_Unit := Sys_Prefix;
      Helper_Unit.Append
        (To_Unbounded_String
           ("D"
            & "B_manual_"
            & Qualified_Name_Slug (Prj.Prj_Name, Use_Hash => False)));
      return Helper_Unit;
   end Create_Manual_Helper_Unit_Name;

   ---------------------------------------
   -- Emit_Dump_Helper_Unit_For_Trigger --
   ---------------------------------------

   procedure Emit_Dump_Helper_Unit_For_Trigger
     (Dump_Config    : Any_Dump_Config;
      Dump_Trigger   : Valid_Dump_Trigger;
      Instrumenter   : Ada_Instrumenter_Type'Class;
      Prj            : Prj_Desc;
      Main           : Compilation_Unit_Part;
      Helper_Unit    : out Ada_Qualified_Name;
      Has_Controlled : Boolean := False)
   is
      File : Text_Files.File_Type;

      Output_Unit, Output_Proc : Ada_Qualified_Name;
      --  Qualified names for the unit that contains the buffer output
      --  procedure, and for the procedure itself.

      --  Start of processing for Emit_Dump_Helper_Unit

   begin
      --  Create the name of the helper unit
      if Dump_Trigger = Manual then
         Helper_Unit := Create_Manual_Helper_Unit_Name (Prj);
      else
         Helper_Unit := Sys_Prefix;
         Helper_Unit.Append
           (To_Unbounded_String ("D" & Instrumented_Unit_Slug (Main)));
      end if;

      --  Compute the qualified names we need for instrumentation

      declare
         use type Ada_Qualified_Name;
         Unit : constant String :=
           (case Dump_Config.Channel is
              when Binary_File            => "Files",
              when Base64_Standard_Output => "Base64");
      begin
         Output_Unit.Append (To_Unbounded_String ("GNATcov_RTS"));
         Output_Unit.Append (To_Unbounded_String ("Traces"));
         Output_Unit.Append (To_Unbounded_String ("Output." & Unit));

         Output_Proc := Output_Unit & To_Unbounded_String ("Write_Trace_File");
      end;

      declare
         Spec_Filename : constant String :=
           To_Filename (Prj, CU_Name_For_Unit (Helper_Unit, GPR2.S_Spec));
         Body_Filename : constant String :=
           To_Filename (Prj, CU_Name_For_Unit (Helper_Unit, GPR2.S_Body));

         Helper_Unit_Name : constant String := To_Ada (Helper_Unit);
         Dump_Procedure   : constant String := To_String (Dump_Procedure_Name);
         Reset_Procedure  : constant String :=
           To_String (Reset_Procedure_Name);
         Output_Unit_Str  : constant String := To_Ada (Output_Unit);
         Project_Name_Str : constant String :=
           """" & To_Ada (Prj.Prj_Name) & """";
         Sys_Lists        : Ada_Qualified_Name := Sys_Buffers;

         T : Translate_Set :=
           Assoc ("UNIT_NAME", Helper_Unit_Name)
           & Assoc ("DUMP_PROCEDURE", Dump_Procedure)
           & Assoc
               ("REGISTER_DUMP_FCT", To_String (Register_Dump_Function_Name))
           & Assoc ("RESET_PROCEDURE", Reset_Procedure)
           & Assoc ("WITNESS_DUMMY_TYPE", To_Ada (Witness_Dummy_Type_Name))
           & Assoc ("MAIN_END", Dump_Trigger = Main_End)
           & Assoc
               ("RAVENSCAR_TASK_TERM",
                Dump_Trigger = Ravenscar_Task_Termination)
           & Assoc ("AT_EXIT", Dump_Trigger = At_Exit)
           & Assoc ("MANUAL", Dump_Trigger = Manual)
           & Assoc ("HAS_CONTROLLED", Has_Controlled)
           & Assoc ("CHANNEL_IMG", Image (Dump_Config.Channel))
           & Assoc ("OUTPUT_UNIT", Output_Unit_Str);
         --  Holds the data passed to the templating engine.

      begin
         Sys_Lists.Append (To_Unbounded_String ("Lists"));

         if Sources_Trace.Is_Active then
            Sources_Trace.Increase_Indent
              ("Writing "
               & Instrumenter.Language_Name
               & " dump helper unit "
               & Helper_Unit_Name);
            Sources_Trace.Trace ("Project: " & To_Ada (Prj.Prj_Name));
            Sources_Trace.Trace ("Spec filename: " & Spec_Filename);
            Sources_Trace.Trace ("Body filename: " & Body_Filename);
            Sources_Trace.Trace ("For main: " & Image (Main));
            Sources_Trace.Decrease_Indent;
         end if;

         --  Emit the package spec. This includes one Dump_Buffers procedure,
         --  which dumps all coverage buffers in Main's closure to the source
         --  trace file, and in the case of manual dump trigger, a
         --  Reset_Buffers procedure which will resets all coverage buffers in
         --  the project tree rooted at the project to which Main belongs.

         Create_File (Prj, File, Spec_Filename);

         declare
            Withed_Unit_Vec : Vector_Tag := +To_Ada (Sys_Buffers);
         begin
            if Dump_Trigger = Main_End and then Has_Controlled then
               Append (Withed_Unit_Vec, "Ada.Finalization");
            end if;

            T := T & Assoc ("WITHED_UNIT", Withed_Unit_Vec);
         end;

         File.Put (Render_Template ("dump_helper.ads.tmplt", T));
         File.Close;

         --  Emit the package body

         Create_File (Prj, File, Body_Filename);

         declare
            Withed_Unit_Vec : Vector_Tag :=
              +Output_Unit_Str & To_Ada (Sys_Lists);
         begin
            case Dump_Trigger is
               when Ravenscar_Task_Termination =>
                  Append (Withed_Unit_Vec, "Ada.Task_Identification");
                  Append (Withed_Unit_Vec, "Ada.Task_Termination");

               when At_Exit                    =>
                  Append (Withed_Unit_Vec, "Interfaces.C");

               when others                     =>
                  null;
            end case;

            if Dump_Config.Channel = Binary_File then
               Append (Withed_Unit_Vec, "Interfaces.C.Strings");
            end if;

            Append
              (Withed_Unit_Vec, To_Ada (Buffers_List_Unit (Prj.Prj_Name)));

            T := T & Assoc ("WITHED_UNIT", Withed_Unit_Vec);
         end;

         if Dump_Config.Channel = Binary_File then
            T :=
              T
              & Assoc
                  ("FILENAME_ENV_VAR",
                   (if Dump_Config.Filename_Env_Var = ""
                    then Output_Unit_Str & ".Default_Trace_Filename_Env_Var"
                    else """" & (+Dump_Config.Filename_Env_Var) & """"))
              & Assoc ("FILENAME_SIMPLE", Dump_Config.Filename_Simple)
              & Assoc ("FILENAME_PREFIX", Dump_Config.Filename_Prefix);
         end if;
         T :=
           T
           & Assoc ("TAG", Instrumenter.Tag)
           & Assoc ("OUTPUT_PROCEDURE", To_Ada (Output_Proc))
           & Assoc
               ("BUFFER_LIST_UNIT", To_Ada (Buffers_List_Unit (Prj.Prj_Name)))
           & Assoc ("PROGRAM_NAME", Project_Name_Str)
           & Assoc ("SYS_LISTS", To_Ada (Sys_Lists));

         File.Put (Render_Template ("dump_helper.adb.tmplt", T));
         File.Close;
      end;
   end Emit_Dump_Helper_Unit_For_Trigger;

   ----------------------------------
   -- Emit_Dump_Helper_Unit_Manual --
   ----------------------------------

   procedure Emit_Dump_Helper_Unit_Manual
     (Self        : in out Ada_Instrumenter_Type;
      Dump_Config : Any_Dump_Config;
      Prj         : Prj_Desc)
   is
      Main : Compilation_Unit_Part;
      --  Since the dump trigger is "manual" and there is no main to be given,
      --  the Main argument in the following call to Emit_Dump_Helper_Unit will
      --  not be used.

      Dummy_Ada_Helper_Unit : Ada_Qualified_Name;
   begin
      Emit_Dump_Helper_Unit_For_Trigger
        (Dump_Config  => Dump_Config,
         Dump_Trigger => Manual,
         Instrumenter => Self,
         Prj          => Prj,
         Main         => Main,
         Helper_Unit  => Dummy_Ada_Helper_Unit);
   end Emit_Dump_Helper_Unit_Manual;

   ----------------------------
   -- Emit_Buffers_List_Unit --
   ----------------------------

   overriding
   procedure Emit_Buffers_List_Unit
     (Self        : Ada_Instrumenter_Type;
      Instr_Units : Unit_Sets.Set;
      Prj         : Prj_Desc)
   is
      Buffers_CU_Name : constant Compilation_Unit_Part :=
        CU_Name_For_Unit (Buffers_List_Unit (Prj.Prj_Name), GPR2.S_Spec);
      Unit_Name       : constant String := To_Ada (Buffers_CU_Name.Unit);
      Filename        : constant String := To_Filename (Prj, Buffers_CU_Name);
      File            : Text_Files.File_Type;
   begin
      if Sources_Trace.Is_Active then
         Sources_Trace.Increase_Indent
           ("Writing "
            & Self.Language_Name
            & " buffer list unit "
            & Unit_Name);
         Sources_Trace.Trace ("Project: " & To_Ada (Prj.Prj_Name));
         Sources_Trace.Trace ("Filename: " & Filename);
         Sources_Trace.Decrease_Indent;
      end if;

      --  Emit the unit to contain the list of buffers

      Create_File (Prj, File, Filename);
      Put_Warnings_And_Style_Checks_Pragmas (File);
      File.Put_Line
        ("with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;");

      for Instr_Unit of Instr_Units loop

         --  Even though Ada buffer units are not explicitly referenced in the
         --  generated code (we import all the coverage buffers from their C
         --  symbol, as we want common processing for all languages), we still
         --  need to "with" them. Otherwise, gprbuild would not include them in
         --  the link as they would not be in the dependency closure.

         if Instr_Unit.Language = Unit_Based_Language then
            File.Put_Line
              ("with "
               & To_Ada
                   (Buffer_Unit (To_Qualified_Name (+Instr_Unit.Unit_Name)))
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
              ("   "
               & Buffer_Name
               & " : aliased constant GNATcov_RTS_Coverage_Buffers_Group;");
            File.Put_Line
              ("   pragma Import (C, "
               & Buffer_Name
               & ","""
               & Buffer_Name
               & """);");
         end;
      end loop;

      if Instr_Units.Is_Empty then
         File.Put_Line
           ("   Dummy : aliased GNATcov_RTS_Coverage_Buffers_Group;");
      end if;

      --  Create the list of coverage buffers

      File.Put_Line
        ("   List : constant GNATcov_RTS.Buffers.Lists"
         & ".Coverage_Buffers_Group_Array := (");
      declare
         Index : Positive := 1;
         Last  : constant Natural := Natural (Instr_Units.Length);
      begin
         for Instr_Unit of Instr_Units loop
            File.Put
              ("      "
               & Img (Index)
               & " => "
               & Unit_Buffers_Name (Instr_Unit)
               & "'Access");
            if Index = Last then
               File.Put_Line (");");
            else
               File.Put_Line (",");
            end if;
            Index := Index + 1;
         end loop;
      end;

      if Instr_Units.Is_Empty then
         File.Put ("      1 => Dummy'Access);");
      end if;

      File.Put_Line
        ("   C_List : constant GNATcov_RTS.Buffers.Lists"
         & ".GNATcov_RTS_Coverage_Buffers_Group_Array :=");
      File.Put_Line
        ("      (" & Instr_Units.Length'Image & ", List'Address);");

      File.Put_Line
        ("   pragma Export (C, C_List, """
         & Unit_Buffers_Array_Name (Prj.Prj_Name)
         & """);");

      File.New_Line;
      File.Put_Line ("end " & Unit_Name & ";");
   end Emit_Buffers_List_Unit;

   -----------------------------
   -- Emit_Observability_Unit --
   -----------------------------

   overriding
   procedure Emit_Observability_Unit
     (Self : in out Ada_Instrumenter_Type; Prj : in out Prj_Desc)
   is

      pragma Unreferenced (Self);

      Buf_List_Unit      : constant Ada_Qualified_Name :=
        CU_Name_For_Unit (Buffers_List_Unit (Prj.Prj_Name), GPR2.S_Spec).Unit;
      Buf_List_Unit_Name : constant String := To_Ada (Buf_List_Unit);

      Obs_Unit      : constant Ada_Qualified_Name :=
        Buf_List_Unit
        & Ada_Identifier_Vectors.To_Vector
            (To_Unbounded_String ("Observe"), 1);
      Obs_Unit_Name : constant String := To_Ada (Obs_Unit);

      Obs_Spec_Filename : constant String :=
        To_Filename (Prj, CU_Name_For_Unit (Obs_Unit, GPR2.S_Spec));
      Obs_Body_Filename : constant String :=
        To_Filename (Prj, CU_Name_For_Unit (Obs_Unit, GPR2.S_Body));

      Spec_File : Text_Files.File_Type;
      Body_File : Text_Files.File_Type;

      T : constant Translate_Set :=
        Assoc ("UNIT_NAME", Obs_Unit_Name)
        & Assoc ("BUF_LIST_UNIT_NAME", Buf_List_Unit_Name);
   begin
      Create_File (Prj, Spec_File, Obs_Spec_Filename);
      Create_File (Prj, Body_File, Obs_Body_Filename);

      Spec_File.Put (Render_Template ("observability.ads.tmplt", T));
      Body_File.Put (Render_Template ("observability.adb.tmplt", T));
   end Emit_Observability_Unit;

   ---------------------------------
   -- Save_Config_Pragmas_Mapping --
   ---------------------------------

   procedure Save_Config_Pragmas_Mapping (Filename : String) is

      --  First, compute the configuration pragmas mapping for all sources in
      --  the loaded project.

      Context : constant Analysis_Context := Create_Context;
      Mapping : constant Config_Pragmas_Mapping :=
        Import_From_Project (Context, Project.Project);

      --  Then, turn this mapping into a JSON description

      Desc  : constant JSON_Value := Create_Object;
      Local : constant JSON_Value := Create_Object;
   begin
      Desc.Set_Field ("local_pragmas", Local);
      Desc.Set_Field ("global_pragmas", +Mapping.Global_Pragmas);

      for Cur in Mapping.Local_Pragmas.Iterate loop
         declare
            Source_File  : constant Unbounded_String := Unit_Maps.Key (Cur);
            Pragmas_File : constant Unbounded_String :=
              Unit_Maps.Element (Cur);
         begin
            Local.Set_Field (+Source_File, Pragmas_File);
         end;
      end loop;

      --  Finally, write that JSON description to the requested file.
      --
      --  No need to be space-efficient here, and a non-compact form will be
      --  easier for debugging.

      Write (Filename, Desc, Compact => False);
   end Save_Config_Pragmas_Mapping;

   ---------------------------------
   -- Load_Config_Pragmas_Mapping --
   ---------------------------------

   procedure Load_Config_Pragmas_Mapping
     (Mapping : out Config_Pragmas_Mapping; Filename : String)
   is
      --  Parse the JSON description

      Result : constant Read_Result := JSON.Read (Filename);
   begin
      if not Result.Success then
         Outputs.Error ("Cannot read " & Filename);
         Outputs.Fatal_Error (Format_Parsing_Error (Result.Error));
      end if;

      --  Decode it into the final mapping

      declare
         Global : constant JSON_Value := Result.Value.Get ("global_pragmas");
      begin
         if Global.Kind /= JSON_Null_Type then
            Mapping.Global_Pragmas := Global.Get;
         end if;
      end;

      declare
         procedure Process (Name : String; Value : JSON_Value);
         --  Insert an entry to Mapping.Local_Pragmas corresponding to the
         --  source file at Name and the configuration pragmas file at Value (a
         --  JSON string).

         -------------
         -- Process --
         -------------

         procedure Process (Name : String; Value : JSON_Value) is
         begin
            Mapping.Local_Pragmas.Insert (+Name, Value.Get);
         end Process;
      begin
         Result.Value.Get ("local_pragmas").Map_JSON_Object (Process'Access);
      end;
   end Load_Config_Pragmas_Mapping;

   -----------------------------
   -- Create_Ada_Instrumenter --
   -----------------------------

   function Create_Ada_Instrumenter
     (Tag                        : Unbounded_String;
      Config_Pragmas_Mapping     : String;
      Mapping_Filename           : String;
      Preprocessor_Data_Filename : String) return Ada_Instrumenter_Type
   is
      Instrumenter : Ada_Instrumenter_Type;
   begin
      Instrumenter.Tag := Tag;

      --  First create the context for Libadalang

      Instrumenter.Provider :=
        Instrument.Ada_Unit_Provider.Create_Provider (Mapping_Filename);

      --  Create a file reader, to let Libadalang preprocess source files that
      --  need it.

      Instrumenter.File_Reader :=
        Instrument.Ada_Preprocessing.Create_Preprocessor
          (Preprocessor_Data_Filename);

      --  Create the event handler, to report when Libadalang cannot read a
      --  required source file.

      Instrumenter.Event_Handler := Create_Missing_File_Reporter;

      --  Save the location of the file holding the configuration pragmas

      Instrumenter.Config_Pragmas_Mapping := +Config_Pragmas_Mapping;

      --  Then, create the analysis context

      Create_LAL_Context (Instrumenter);
      return Instrumenter;
   end Create_Ada_Instrumenter;

   ---------------------
   -- Instrument_Unit --
   ---------------------

   procedure Instrument_Unit
     (Self              : in out Ada_Instrumenter_Type;
      Unit_Name         : String;
      Prj               : Prj_Desc;
      Files_Of_Interest : File_Sets.Set)
   is
      Allocated_Bits    : Allocated_Bits_Vectors.Vector;
      Last_Buffer_Index : Natural := 0;
      CU_Names          : CU_Name_Vectors.Vector;
      CUs               : CU_Id_Vectors.Vector;
      Unit              : constant Files_Table.Compilation_Unit :=
        (Language => Unit_Based_Language, Unit_Name => +Unit_Name);

      UIC : Ada_Unit_Inst_Context;

      procedure Instrument_Source_File_Wrapper (Filename : String);
      --  Wrapper around Instrument_Source_File. Instrument the given filename
      --  if it is a file of interest.

      ------------------------------------
      -- Instrument_Source_File_Wrapper --
      ------------------------------------

      procedure Instrument_Source_File_Wrapper (Filename : String) is
         Event_Handler : Missing_Src_Reporter renames
           Missing_Src_Reporter_Access (Self.Event_Handler.Unchecked_Get).all;
         --  Handle to the event handler we use to report missing source files;

         Basename : constant String := Ada.Directories.Simple_Name (Filename);
      begin
         --  Instrument the file only if it is a file of interest

         if Files_Of_Interest.Contains (Create_Normalized (Filename)) then

            --  In the corresponding trace is active, always print a notice for
            --  the source file that we are about to instrument. In non-verbose
            --  mode, just get prepared to print it in case we emit a "source
            --  file missing" warning through Libadalang's event handler.

            if Sources_Trace.Is_Active then
               Sources_Trace.Trace ("Instrumenting " & Basename);
            else
               Event_Handler.Instrumented_File := +Basename;
            end if;

            Last_Buffer_Index := Last_Buffer_Index + 1;
            UIC.Entities.Buffers_Index := Last_Buffer_Index;
            Instrument_Source_File
              (UIC          => UIC,
               Filename     => Filename,
               Instrumenter => Self,
               Prj          => Prj);
            CU_Names.Append (UIC.Instrumented_Unit);
            CUs.Append (UIC.CU);
            Allocated_Bits.Append (UIC.Unit_Bits);
         end if;
      end Instrument_Source_File_Wrapper;
   begin
      --  Initialize the buffer unit to match the unit name. It will contain
      --  coverage buffers for all parts of the unit: specification, body,
      --  and separates.

      UIC.Buffer_Unit :=
        CU_Name_For_Unit
          (Buffer_Unit (To_Qualified_Name (Unit_Name)), GPR2.S_Spec);
      UIC.Pure_Buffer_Unit :=
        CU_Name_For_Unit
          (Pure_Buffer_Unit (To_Qualified_Name (Unit_Name)), GPR2.S_Spec);

      --  We consider that theer is no No_Elaboration_Code_All pragma/aspect
      --  until we see one.

      UIC.Has_No_Elaboration_Code_All := False;

      --  Try to find the spec and / or the body for this compilation unit
      --  using the unit provider. Then retrieve the separate unit from the
      --  spec / body / both.

      for Part in Analysis_Unit_Kind loop
         if Self.Provider.Has_Unit (Unit_Name, +Part) then
            for Filename of
              Find_Ada_Units
                (Self,
                 Self.Provider.Get_Unit_Filename
                   (Langkit_Support.Text.From_UTF8 (Unit_Name), Part))
            loop
               Instrument_Source_File_Wrapper (+Filename);
            end loop;
         end if;
      end loop;

      --  Once the unit was instrumented, emit the coverage buffer units

      Emit_Buffer_Unit
        (Buffer_Unit => UIC.Buffer_Unit,
         Prj         => Prj,
         Unit        => Unit,
         Unit_Bits   => Allocated_Bits,
         CU_Names    => CU_Names,
         CUs         => CUs);
      Emit_Pure_Buffer_Unit
        (PB_Unit                        => UIC.Pure_Buffer_Unit,
         Prj                            => Prj,
         CU_Names                       => CU_Names,
         Language_Version               => UIC.Language_Version_Pragma,
         Degenerate_Subprogram_Generics => UIC.Degenerate_Subprogram_Generics,
         Has_No_Elaboration_Code_All    => UIC.Has_No_Elaboration_Code_All);
   end Instrument_Unit;

   --------------------
   -- Find_Ada_Units --
   --------------------

   function Find_Ada_Units
     (Instrumenter : in out Ada_Instrumenter_Type; Filename : String)
      return String_Vectors.Vector
   is

      function Process_Node (N : LAL.Ada_Node'Class) return Visit_Status;

      Dependent_Comp_Units : String_Vectors.Vector;

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node (N : LAL.Ada_Node'Class) return Visit_Status is
      begin
         if N.Kind in Ada_Body_Stub then
            begin
               declare
                  Stub        : constant LAL.Body_Stub := N.As_Body_Stub;
                  Subunit_FQN : constant Text_Type :=
                    Stub.P_Fully_Qualified_Name;
               begin
                  if Subunit_FQN'Length = 0 then
                     raise Property_Error;
                  else
                     Dependent_Comp_Units.Append
                       (Find_Ada_Units
                          (Instrumenter,
                           Instrumenter
                             .Context
                             .Unit_Provider
                             .Get
                             .Get_Unit_Filename
                                (Subunit_FQN, LALCO.Unit_Body)));
                  end if;
               end;
            exception
               when Property_Error =>
                  --  TODO: location
                  Warn ("failed to locate the subunit for this stub");
            end;
            return Over;
         end if;

         return Into;
      end Process_Node;

      Unit : constant LAL.Analysis_Unit :=
        Get_From_File (Instrumenter, Filename);
   begin
      --  Abort if the input project is not compilable

      if Unit.Has_Diagnostics then
         Outputs.Error ("instrumentation failed for " & Filename);
         Outputs.Error
           ("please make sure the original project can be compiled");
         for D of Unit.Diagnostics loop
            Outputs.Error (Unit.Format_GNU_Diagnostic (D));
         end loop;
         raise Xcov_Exit_Exc;
      end if;

      --  We cannot instrument files that contain only pragmas, but they are
      --  still valid sources in the context of the GNAT runtime (for instance
      --  g-os_lib.adb), so just ignore them.

      if Unit.Root.Kind = Ada_Pragma_Node_List then
         return String_Vectors.Empty_Vector;

      --  Abort if a source file does not contain exactly one compilation
      --  unit.

      elsif Unit.Root.Kind = Ada_Compilation_Unit_List then
         Outputs.Error ("instrumentation failed for " & Filename);
         Outputs.Error
           ("source files containing multiple compilation units"
            & " are not supported");
         raise Xcov_Exit_Exc;
      end if;

      pragma Assert (Unit.Root.Kind = Ada_Compilation_Unit);
      declare
         CU : constant LAL.Compilation_Unit := Unit.Root.As_Compilation_Unit;
      begin
         Dependent_Comp_Units :=
           String_Vectors.To_Vector (+Filename, Length => 1);
         CU.Traverse (Process_Node'Access);
         return Dependent_Comp_Units;
      end;
   end Find_Ada_Units;

end Instrument.Ada_Unit;
