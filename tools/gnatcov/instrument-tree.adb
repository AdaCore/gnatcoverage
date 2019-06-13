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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GNATCOLL.VFS;

with Langkit_Support;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Symbols;
with Libadalang.Common; use Libadalang.Common;
with Libadalang.Rewriting; use Libadalang.Rewriting;
with Libadalang.Sources; use Libadalang.Sources;

with Namet; use Namet;
with SCOs;
with Snames; use Snames;
with Table;

with ALI_Files;
with Coverage; use Coverage;
with Diagnostics; use Diagnostics;
with Instrument.Common; use Instrument.Common;
with SC_Obligations; use SC_Obligations;
with Strings; use Strings;
with Types; use Types;

package body Instrument.Tree is

   function Create_Identifier
     (RH : Rewriting_Handle; Text : Text_Type) return Node_Rewriting_Handle
   is (Create_Token_Node (RH, Libadalang.Common.Ada_Identifier, Text));

   function To_Nodes
     (Handle : Rewriting_Handle;
      Name   : Ada_Qualified_Name) return Node_Rewriting_Handle
      with Pre => not Name.Is_Empty;
   --  Turn the given qualified name into a name tree for rewriting

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

   function Precomputed_Symbol_Text (S : All_Symbols) return Text_Type is
      (Canonicalize (To_Wide_Wide_String (S'Img)).Symbol);

   package Symbols_Pkg is
     new Langkit_Support.Symbols
       (All_Symbols, Precomputed_Symbol_Text);

   use Symbols_Pkg;

   Symbols : constant Symbol_Table := Create_Symbol_Table;
   --  Holder for name singletons

   function As_Symbol (S : All_Symbols) return Symbol_Type is
      (Precomputed_Symbol (Symbols, S));

   function As_Symbol (Id : Identifier) return Symbol_Type;
   function As_Name (Id : Identifier) return Name_Id;
   --  Canonicalize Id and return a corresponding Name_Id/Symbol_Type

   function Pragma_Name (P : Pragma_Node) return Symbol_Type;
   function Pragma_Name (P : Pragma_Node) return Name_Id;
   --  Return a symbol from Symbols corresponding to the name of the given
   --  P pragma.

   function Aspect_Assoc_Name (A : Aspect_Assoc) return Identifier;
   function Aspect_Assoc_Name (A : Aspect_Assoc) return Symbol_Type;
   function Aspect_Assoc_Name (A : Aspect_Assoc) return Name_Id;
   --  Return a symbol from Symbols corresponding to the name of the given
   --  A aspect association.

   procedure Append_SCO
     (C1, C2             : Character;
      From, To           : Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name);
   --  Append a new entry to the low-level SCO table

   package Ada_Qualified_Name_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Ada_Qualified_Name,
      "="          => Ada_Identifier_Vectors."=");

   function Sloc (N : Ada_Node'Class) return Source_Location is
     (Start_Sloc (N.Sloc_Range));

   function Unwrap (N : Expr) return Expr;
   --  Strip Paren_Expr from N

   -----------------
   -- Diagnostics --
   -----------------

   procedure Report
     (IC   : Unit_Inst_Context;
      Node : Ada_Node'Class;
      Msg  : String;
      Kind : Report_Kind := Diagnostics.Error);

   -------------------------------------
   -- Generation of witness fragments --
   -------------------------------------

   type Statement_Witness_Flavor is
     (Procedure_Call, Function_Call, Declaration);
   function Make_Statement_Witness
     (IC     : Unit_Inst_Context;
      Bit    : Bit_Id;
      Flavor : Statement_Witness_Flavor) return Node_Rewriting_Handle;
   --  Create a procedure call statement or object declaration to witness
   --  execution of the low level SCO with the given bit id.

   function Make_MCDC_State_Name (LL_SCO_Id : Nat) return String is
      ("MCDC_State_" & Img (Integer (LL_SCO_Id)));
   --  Return the name of the MC/DC state local variable for the given
   --  decision SCO.

   function Make_MCDC_State
     (IC   : Unit_Inst_Context;
      Name : String) return Node_Rewriting_Handle;
   --  Create the declaration of the MC/DC state local variable

   ------------
   -- Report --
   ------------

   procedure Report
     (IC   : Unit_Inst_Context;
      Node : Ada_Node'Class;
      Msg  : String;
      Kind : Report_Kind := Diagnostics.Error)
   is
      LAL_Loc : constant Source_Location := Sloc (Node);
   begin
      Report ((Source_File => IC.SFI,
               L           => (Line   => Integer (LAL_Loc.Line),
                               Column => Integer (LAL_Loc.Column))),
              Msg,
              Kind);
   end Report;

   ----------------------------
   -- Make_Statement_Witness --
   ----------------------------

   function Make_Statement_Witness
     (IC     : Unit_Inst_Context;
      Bit    : Bit_Id;
      Flavor : Statement_Witness_Flavor) return Node_Rewriting_Handle
   is
      Bit_Img : constant String  := Img (Bit);
      E       : Instrumentation_Entities renames IC.Entities;

      function Call_Img return String is
        ("{}.Witness ({}, " & Bit_Img & ")"
         & (if Flavor = Function_Call then "" else ";"));

      --  Note: package spec and package body are instrumented separately,
      --  so we need to make sure that variables declared in a body can't
      --  clash with those from the corresponding spec, hence the inclusion
      --  of the unit part in the variable name.

      function Decl_Img return String is
        ("Discard_" & IC.Instrumented_Unit.Part'Img & Bit_Img
         & " : {}.Witness_Dummy_Type := "
         & Call_Img);

   --  Start of processing for Make_Statement_Witness

   begin
      if Flavor = Declaration then
         return Create_From_Template
           (IC.Rewriting_Context,
            Template  => To_Wide_Wide_String (Decl_Img),
            Arguments => (1 | 2 => E.Common_Buffers, 3 => E.Statement_Buffer),
            Rule      => Object_Decl_Rule);
      else
         return Create_From_Template
           (IC.Rewriting_Context,
            Template  => To_Wide_Wide_String (Call_Img),
            Arguments => (E.Common_Buffers, E.Statement_Buffer),
            Rule      =>
              (if Flavor = Procedure_Call then Call_Stmt_Rule else Name_Rule));
      end if;
   end Make_Statement_Witness;

   ---------------------
   -- Make_MCDC_State --
   ---------------------

   function Make_MCDC_State
     (IC   : Unit_Inst_Context;
      Name : String) return Node_Rewriting_Handle
   is
      E        : Instrumentation_Entities renames IC.Entities;
      Decl_Img : constant String :=
        Name & " : aliased {}.MCDC_State_Type;";
   begin
      return Create_From_Template
        (IC.Rewriting_Context,
         Template  => To_Wide_Wide_String (Decl_Img),
         Arguments => (1 => E.Common_Buffers),
         Rule      => Object_Decl_Rule);
   end Make_MCDC_State;

   ----------------
   -- Append_SCO --
   ----------------

   procedure Append_SCO
     (C1, C2             : Character;
      From, To           : Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name)
   is
   begin
      SCOs.SCO_Table.Append
        ((From =>
              (Line => Logical_Line_Number (From.Line),
               Col  => Types.Column_Number (From.Column)),
          To   =>
              (Line => Logical_Line_Number (To.Line),
               Col  => Types.Column_Number (To.Column)),
          C1   => C1,   C2 => C2,
          Last => Last,
          Pragma_Sloc        => No_Location,
          Pragma_Aspect_Name => Pragma_Aspect_Name));
   end Append_SCO;

   -----------------------------------------
   -- Traverse_Declarations_Or_Statements --
   -----------------------------------------

   function Traverse_Declarations_Or_Statements
     (IC      : in out Unit_Inst_Context;
      L       : Ada_Node_List;
      Preelab : Boolean       := False;
      D       : Dominant_Info := No_Dominant;
      P       : Ada_Node      := No_Ada_Node) return Dominant_Info;
   --  Same as above, and returns dominant information corresponding to the
   --  last node with SCO in L.

   --  The following Traverse_* routines perform appropriate calls to
   --  Traverse_Declarations_Or_Statements to traverse specific node kinds.
   --  Parameter D, when present, indicates the dominant of the first
   --  declaration or statement within N.

   --  Why is Traverse_Sync_Definition commented specifically, whereas
   --  the others are not???

   procedure Traverse_Generic_Package_Declaration
     (IC      : in out Unit_Inst_Context;
      N       : Generic_Package_Decl;
      Preelab : Boolean);

   procedure Traverse_Handled_Statement_Sequence
     (IC      : in out Unit_Inst_Context;
      N : Handled_Stmts;
      D : Dominant_Info := No_Dominant);

   procedure Traverse_Package_Body
     (IC      : in out Unit_Inst_Context;
      N       : Package_Body;
      Preelab : Boolean);

   procedure Traverse_Package_Declaration
     (IC      : in out Unit_Inst_Context;
      N       : Base_Package_Decl;
      Preelab : Boolean;
      D       : Dominant_Info := No_Dominant);

   procedure Traverse_Subprogram_Or_Task_Body
     (IC : in out Unit_Inst_Context;
      N  : Ada_Node;
      D  : Dominant_Info := No_Dominant);

   procedure Traverse_Sync_Definition
     (IC : in out Unit_Inst_Context; N : Ada_Node);
   --  Traverse a protected definition or task definition

   --  Note regarding traversals: In a few cases where an Alternatives list is
   --  involved, pragmas such as "pragma Page" may show up before the first
   --  alternative. We skip them because we're out of statement or declaration
   --  context, so these can't be pragmas of interest for SCO purposes, and
   --  the regular alternative processing typically involves attribute queries
   --  which aren't valid for a pragma.

   procedure Process_Decisions
     (IC : in out Unit_Inst_Context;
      N  : Ada_Node'Class;
      T  : Character);
   --  If N is Empty, has no effect. Otherwise scans the tree for the node N,
   --  to output any decisions it contains. T is one of IEGPWX (for context of
   --  expression: if/exit when/entry guard/pragma/while/expression). If T is
   --  other than X, the node N is the if expression involved, and a decision
   --  is always present (at the very least a simple decision is present at the
   --  top level).

   --------------------------
   -- Internal Subprograms --
   --------------------------

   function Has_Decision (T : Ada_Node'Class) return Boolean;
   --  T is the node for a subtree. Returns True if any (sub)expression in T
   --  contains a nested decision (i.e. either is a logical operator, or
   --  contains a logical operator in its subtree).

   function Operator (N : Expr) return Op;
   --  Return the operator node of an unary or binary expression, or No_Op if
   --  not an operator.

   function Is_Logical_Operator (N : Ada_Node'Class) return Tristate;
   --  False for any node that isn't an Expr. For an Expr, determines whether N
   --  is a logical operator: True for short circuit conditions, Unknown for OR
   --  and AND (the Short_Circuit_And_Or pragma may be used) and False
   --  otherwise.

   -----------------------------------------
   -- Traverse_Declarations_Or_Statements --
   -----------------------------------------

   --  Tables used by Traverse_Declarations_Or_Statements for temporarily
   --  holding statement and decision entries. These are declared globally
   --  since they are shared by recursive calls to this procedure.

   type SC_Entry is record
      N    : Ada_Node;
      From : Source_Location;
      To   : Source_Location;
      Typ  : Character;

      Index : Nat := 0;
      --  1-based index of N in enclosing list, if any
   end record;
   --  Used to store a single entry in the following table, From:To represents
   --  the range of entries in the CS line entry, and typ is the type, with
   --  space meaning that no type letter will accompany the entry.

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
   --  be searched for decisions for the case of Process_Decisions_Defer with a
   --  node argument (with Lst set to No_Ada_Node. Lst is the list to be
   --  searched for decisions for the case of Process_Decisions_Defer with a
   --  List argument (in which case Nod is set to No_Ada_Node).

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
     (IC      : in out Unit_Inst_Context;
      L       : Ada_Node_List;
      Preelab : Boolean       := False;
      D       : Dominant_Info := No_Dominant;
      P       : Ada_Node      := No_Ada_Node)
   is
      Discard_Dom : Dominant_Info;
      pragma Warnings (Off, Discard_Dom);
   begin
      Discard_Dom := Traverse_Declarations_Or_Statements
                       (IC, L, Preelab, D, P);
   end Traverse_Declarations_Or_Statements;

   function Traverse_Declarations_Or_Statements
     (IC      : in out Unit_Inst_Context;
      L       : Ada_Node_List;
      Preelab : Boolean       := False;
      D       : Dominant_Info := No_Dominant;
      P       : Ada_Node      := No_Ada_Node)
      return Dominant_Info
   is
      Current_Dominant : Dominant_Info := D;
      --  Dominance information for the current basic block

      Current_Test : Ada_Node;
      --  Conditional node (IF statement or ELSIF part) being processed

      SC_First : constant Nat := SC.Last + 1;
      SD_First : constant Nat := SD.Last + 1;
      --  Record first entries used in SC/SD at this recursive level

      Current_Index : Nat := 0;
      --  If traversing a list, 1-based index of the current element

      Insertion_Count : Nat := 0;
      --  Count of nodes inserted in current list so far

      RH_Enclosing_List     : Node_Rewriting_Handle;
      --  If traversing a list, rewriting handle for the list

      Witness_Use_Statement : Boolean;
      --  Set True if traversing a list of statements, in which case inserted
      --  statement witnesses must be statements.

      procedure Extend_Statement_Sequence
        (N : Ada_Node'Class; Typ : Character);
      --  Extend the current statement sequence to encompass the node N. Typ is
      --  the letter that identifies the type of statement/declaration that is
      --  being added to the sequence.

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

      procedure Traverse_Aspects (AS : Aspect_Spec);
      --  Helper for Traverse_One: traverse an Aspect_Spec

      procedure Traverse_Degenerate_Subprogram (N : Ada_Node'Class);
      --  Common code to handle null procedures and expression functions. Emit
      --  a SCO of the given Kind and N outside of the dominance flow.

      -------------------------------
      -- Extend_Statement_Sequence --
      -------------------------------

      procedure Extend_Statement_Sequence
        (N : Ada_Node'Class; Typ : Character)
      is
         SR      : constant Source_Location_Range := N.Sloc_Range;

         F       : constant Source_Location := Start_Sloc (SR);
         T       : Source_Location := End_Sloc (SR);
         --  Source location bounds used to produre a SCO statement. By
         --  default, this should cover the same source location range as N,
         --  however for nodes that can contain themselves other statements
         --  (for instance IN statements), we select an end bound that appear
         --  before the first nested statement (see To_Node below).

         To_Node : Ada_Node := No_Ada_Node;
         --  In the case of simple statements, set to No_Ada_Node and unused.
         --  Othewrise, use F and this node's end sloc for the emitted
         --  statement source location ranage.

      begin
         case Kind (N) is
            when Ada_Accept_Stmt =>
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
                  Decl : constant Protected_Type_Decl :=
                     N.As_Protected_Type_Decl;
               begin
                  if not Decl.F_Aspects.Is_Null then
                     To_Node := Decl.F_Aspects.As_Ada_Node;

                  elsif not Decl.F_Discriminants.Is_Null then
                     To_Node := Decl.F_Discriminants.As_Ada_Node;

                  else
                     To_Node := Decl.F_Name.As_Ada_Node;
                  end if;
               end;

            when Ada_Expr =>
               To_Node := N.As_Ada_Node;

            when others =>
               null;
         end case;

         if not To_Node.Is_Null then
            T := End_Sloc (To_Node.Sloc_Range);
         end if;

         SC.Append ((Ada_Node (N), F, T, Typ, Current_Index));
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
           with Pre => RH_Enclosing_List /= No_Node_Rewriting_Handle;
         --  Insert statement witness call for the given SCE

         ------------------------------
         -- Insert_Statement_Witness --
         ------------------------------

         procedure Insert_Statement_Witness
           (SCE : SC_Entry; LL_SCO_Id : Nat)
         is
         begin
            --  Allocate a bit in the statement coverage buffer, and record
            --  its id in the bitmap.

            IC.Unit_Bits.Last_Statement_Bit :=
              IC.Unit_Bits.Last_Statement_Bit + 1;
            IC.Unit_Bits.Statement_Bits.Append
              ((LL_SCO_Id, Executed => IC.Unit_Bits.Last_Statement_Bit));

            if SCE.N.Kind = Ada_Elsif_Stmt_Part then
               declare
                  Old_Cond : constant Node_Rewriting_Handle :=
                    Handle (SCE.N.As_Elsif_Stmt_Part.F_Cond_Expr);
                  New_Cond : constant Node_Rewriting_Handle :=
                    Create_Regular_Node
                      (IC.Rewriting_Context,
                       Ada_Bin_Op,
                       Children =>
                         (1 => Make_Statement_Witness
                            (IC,
                             Bit    => IC.Unit_Bits.Last_Statement_Bit,
                             Flavor => Function_Call),

                          2 => Create_Node
                            (IC.Rewriting_Context, Ada_Op_And_Then),

                          --  Placeholder for relocation of old condition after
                          --  it is detached from the tree.

                          3 => No_Node_Rewriting_Handle));

               begin
                  --  Detach old condition from tree and replace it with
                  --  AND THEN node.

                  Replace (Old_Cond, New_Cond);

                  --  Now reattach old condition in new AND THEN node

                  Set_Child (New_Cond, 3, Old_Cond);
               end;

            else
               --  Insert witness statement or declaration

               Insert_Child
                 (Handle => RH_Enclosing_List,

                  Index  =>
                  --  The witness is inserted at the current location of the
                  --  statement, so that it will occur immediately *before*
                  --  it in the instrumented sources. This is necessary because
                  --  we want to mark a statement as executed anytime it has
                  --  commenced execution (including in cases it raises an
                  --  exception or otherwise transfers control).

                    Integer (SCE.Index + Insertion_Count),

                  Child  =>
                    Make_Statement_Witness
                      (IC,
                       Bit    => IC.Unit_Bits.Last_Statement_Bit,
                       Flavor => (if Witness_Use_Statement
                                  then Procedure_Call
                                  else Declaration)));

               Insertion_Count := Insertion_Count + 1;
            end if;
         end Insert_Statement_Witness;

      --  Start of processing for Set_Statement_Entry

      begin
         --  Output statement entries from saved entries in SC table

         for J in SC_First .. SC_Last loop
            --  If there is a pending dominant for this statement sequence,
            --  emit a SCO for it.

            if J = SC_First and then Current_Dominant /= No_Dominant then
               declare
                  SR   : constant Source_Location_Range :=
                     Current_Dominant.N.Sloc_Range;
                  From : constant Source_Location := Start_Sloc (SR);
                  To   : Source_Location := End_Sloc (SR);

               begin
                  if Current_Dominant.K /= 'E' then
                     To := No_Source_Location;
                  end if;

                  Append_SCO
                    (C1   => '>',
                     C2   => Current_Dominant.K,
                     From => From,
                     To   => To,
                     Last => False);
               end;
            end if;

            declare
               SCE                : SC_Entry renames SC.Table (J);
               Pragma_Aspect_Name : Name_Id := Namet.No_Name;

            begin
               if SCE.Typ = 'P' then
                  Pragma_Aspect_Name := Pragma_Name (SCE.N.As_Pragma_Node);
               end if;

               Append_SCO
                 (C1                 => 'S',
                  C2                 => SCE.Typ,
                  From               => SCE.From,
                  To                 => SCE.To,
                  Last               => (J = SC_Last),
                  Pragma_Aspect_Name => Pragma_Aspect_Name);

               --  Do not attempt to instrument a pragma that we know for
               --  certain will not generate code (such as Annotate or
               --  elaboration control pragmas).

               if SCE.Typ /= 'P'
                 or else Pragma_Might_Generate_Code
                           (Case_Insensitive_Get_Pragma_Id
                              (Pragma_Aspect_Name))
               then
                  Insert_Statement_Witness (SCE, SCOs.SCO_Table.Last);
               end if;
            end;
         end loop;

         --  Last statement of basic block, if present, becomes new current
         --  dominant.

         if SC_Last >= SC_First then
            Current_Dominant := ('S', SC.Table (SC_Last).N);
         end if;

         --  Clear out used section of SC table

         SC.Set_Last (SC_First - 1);

         --  Output any embedded decisions

         for J in SD_First .. SD_Last loop
            declare
               SDE : SD_Entry renames SD.Table (J);

            begin
               Process_Decisions (IC, SDE.Nod, SDE.Typ);
            end;
         end loop;

         --  Clear out used section of SD table

         SD.Set_Last (SD_First - 1);
      end Set_Statement_Entry;

      ----------------------
      -- Traverse_Aspects --
      ----------------------

      procedure Traverse_Aspects (AS : Aspect_Spec) is
         AL : Aspect_Assoc_List;
         AN : Aspect_Assoc;
         AE : Expr;
         C1 : Character;

      begin
         AL := AS.F_Aspect_Assocs;
         for I in 1 .. AL.Children_Count loop
            AN := AL.Child (I).As_Aspect_Assoc;
            AE := AN.F_Expr;

            C1 := ASCII.NUL;

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
               C1 := 'A';

            else
               --  Other aspects: just process any decision nested in the
               --  aspect expression.

               if Has_Decision (AE) then
                  C1 := 'X';
               end if;
            end if;

            if C1 /= ASCII.NUL then
               Process_Decisions_Defer (AE, C1);
            end if;
         end loop;
      end Traverse_Aspects;

      ------------------------------------
      -- Traverse_Degenerate_Subprogram --
      ------------------------------------

      procedure Traverse_Degenerate_Subprogram (N : Ada_Node'Class) is
      begin
         --  Complete current sequence of statements

         Set_Statement_Entry;

         declare
            Saved_Dominant : constant Dominant_Info := Current_Dominant;
            --  Save last statement in current sequence as dominant

         begin
            --  Output statement SCO for degenerate subprogram body (null
            --  statement or freestanding expression) outside of the dominance
            --  chain.

            Current_Dominant := No_Dominant;
            Extend_Statement_Sequence (N, Typ => ' ');

            --  For the case of an expression-function, collect decisions
            --  embedded in the expression now.

            if N.Kind in Ada_Expr then
               Process_Decisions_Defer (N, 'X');
            end if;

            Set_Statement_Entry;

            --  Restore current dominant information designating last statement
            --  in previous sequence (i.e. make the dominance chain skip over
            --  the degenerate body).

            Current_Dominant := Saved_Dominant;
         end;
      end Traverse_Degenerate_Subprogram;

      ------------------
      -- Traverse_One --
      ------------------

      procedure Traverse_One (N : Ada_Node) is
      begin
         --  Initialize or extend current statement sequence. Note that for
         --  special cases such as IF and Case statements we will modify
         --  the range to exclude internal statements that should not be
         --  counted as part of the current statement sequence.

         case N.Kind is
            --  Top of the tree: Compilation unit

            when Ada_Compilation_Unit =>
               declare
                  CUN    : constant Compilation_Unit := N.As_Compilation_Unit;
                  Item_N : constant Basic_Decl :=
                    CUN.F_Body.As_Library_Item.F_Item;
               begin
                  --  Note: we do not traverse the context clause or generate
                  --  any SCOs for it, as nothing there can generate any code.

                  case Item_N.Kind is
                     when Ada_Generic_Instantiation
                        | Ada_Generic_Package_Decl
                        | Ada_Package_Body
                        | Ada_Package_Decl
                        | Ada_Protected_Body
                        | Ada_Subp_Body
                        | Ada_Subp_Decl
                        | Ada_Task_Body
                     =>
                        Traverse_Declarations_Or_Statements
                          (IC, P => Item_N.As_Ada_Node, L => No_Ada_Node_List);

                     --  All other cases of compilation units (e.g. renamings),
                     --  generate no SCO information.

                     when others =>
                        null;
                  end case;

                  --  All instrumented units need to reference the
                  --  corresponding unit that contains coverage buffers.

                  declare
                     Buffers_Unit : constant Node_Rewriting_Handle := To_Nodes
                       (IC.Rewriting_Context, IC.Pure_Buffer_Unit.Unit);
                     With_Clause  : constant Node_Rewriting_Handle :=
                        Create_From_Template
                          (IC.Rewriting_Context, "with {};",
                           (1 => Buffers_Unit), With_Clause_Rule);
                  begin
                     Append_Child (Handle (CUN.F_Prelude), With_Clause);
                  end;
               end;

            --  Package declaration

            when Ada_Package_Decl =>
               Set_Statement_Entry;
               Traverse_Package_Declaration
                 (IC, N.As_Base_Package_Decl, Preelab, Current_Dominant);

            --  Generic package declaration

            when Ada_Generic_Package_Decl =>
               Set_Statement_Entry;
               Traverse_Generic_Package_Declaration
                 (IC, N.As_Generic_Package_Decl, Preelab);

            --  Package body

            when Ada_Package_Body =>
               Set_Statement_Entry;
               Traverse_Package_Body (IC, N.As_Package_Body, Preelab);

            --  Subprogram declaration or subprogram body stub

            when Ada_Expr_Function
               | Ada_Subp_Body_Stub
               | Ada_Subp_Decl
            =>
               declare
                  Spec : constant Subp_Spec :=
                    As_Subp_Spec (As_Basic_Decl (N).P_Subp_Spec_Or_Null);
               begin
                  Process_Decisions_Defer (Spec.F_Subp_Params, 'X');

                  --  Case of a null procedure: generate SCO for fictitious
                  --  NULL statement located at the NULL keyword in the
                  --  procedure specification.

                  if N.Kind = Ada_Null_Subp_Decl
                    and then Spec.F_Subp_Kind.Kind = Ada_Subp_Kind_Procedure
                  then
                     --  Traverse_Degenerate_Subprogram
                     --    (Null_Statement (Spec));
                     --  LAL??? No such fictitious node. But it doesn't really
                     --  matter, just pass Spec to provide the sloc.
                     Traverse_Degenerate_Subprogram (Spec);

                  --  Case of an expression function: generate a statement SCO
                  --  for the expression (and then decision SCOs for any nested
                  --  decisions).

                  elsif N.Kind = Ada_Expr_Function then
                     Traverse_Degenerate_Subprogram
                       (N.As_Expr_Function.F_Expr);
                  end if;
               end;

            --  Entry declaration

            when Ada_Entry_Decl =>
               Process_Decisions_Defer
                 (As_Entry_Decl (N).F_Spec.F_Entry_Params, 'X');

            --  Generic subprogram declaration

            when Ada_Generic_Subp_Decl =>
               declare
                  GSD : constant Generic_Subp_Decl := As_Generic_Subp_Decl (N);
               begin
                  Process_Decisions_Defer
                    (GSD.F_Formal_Part.F_Decls, 'X');
                  Process_Decisions_Defer
                    (GSD.F_Subp_Decl.F_Subp_Spec.F_Subp_Params, 'X');
               end;

            --  Task or subprogram body

            when Ada_Subp_Body
               | Ada_Task_Body
            =>
               Set_Statement_Entry;
               Traverse_Subprogram_Or_Task_Body (IC, N);

            --  Entry body

            when Ada_Entry_Body =>
               declare
                  Cond : constant Expr := As_Entry_Body (N).F_Barrier;

                  Inner_Dominant : Dominant_Info := No_Dominant;

               begin
                  Set_Statement_Entry;

                  if not Cond.Is_Null then
                     Process_Decisions_Defer (Cond, 'G');

                     --  For an entry body with a barrier, the entry body
                     --  is dominanted by a True evaluation of the barrier.

                     Inner_Dominant := ('T', N);
                  end if;

                  Traverse_Subprogram_Or_Task_Body (IC, N, Inner_Dominant);
               end;

            --  Protected body

            when Ada_Protected_Body =>
               Set_Statement_Entry;
               Traverse_Declarations_Or_Statements
                 (IC, L => As_Protected_Body (N).F_Decls.F_Decls);

            --  Exit statement, which is an exit statement in the SCO sense,
            --  so it is included in the current statement sequence, but
            --  then it terminates this sequence. We also have to process
            --  any decisions in the exit statement expression.

            when Ada_Exit_Stmt =>
               Extend_Statement_Sequence (N, 'E');
               declare
                  Cond : constant Expr := As_Exit_Stmt (N).F_Cond_Expr;
               begin
                  Process_Decisions_Defer (Cond, 'E');
                  Set_Statement_Entry;

                  --  If condition is present, then following statement is
                  --  only executed if the condition evaluates to False.

                  if not Cond.Is_Null then
                     Current_Dominant := ('F', N);
                  else
                     Current_Dominant := No_Dominant;
                  end if;
               end;

            --  Label, which breaks the current statement sequence, but the
            --  label itself is not included in the next statement sequence,
            --  since it generates no code.

            when Ada_Label =>
               Set_Statement_Entry;
               Current_Dominant := No_Dominant;

            --  Block statement, which breaks the current statement sequence

            when Ada_Decl_Block | Ada_Begin_Block =>
               Set_Statement_Entry;

               if N.Kind = Ada_Decl_Block then
                  --  The first statement in the handled sequence of statements
                  --  is dominated by the elaboration of the last declaration.

                  Current_Dominant := Traverse_Declarations_Or_Statements
                    (IC,
                     L => As_Decl_Block (N).F_Decls.F_Decls,
                     D => Current_Dominant);
               end if;

               Traverse_Handled_Statement_Sequence
                 (IC,
                  N => (case N.Kind is
                           when Ada_Decl_Block  => As_Decl_Block (N).F_Stmts,
                           when Ada_Begin_Block => As_Begin_Block (N).F_Stmts,
                           when others          => raise Program_Error),
                  D => Current_Dominant);

            --  If statement, which breaks the current statement sequence,
            --  but we include the condition in the current sequence.

            when Ada_If_Stmt =>
               Current_Test := N;
               Extend_Statement_Sequence (N, 'I');

               declare
                  If_N : constant If_Stmt := N.As_If_Stmt;
                  Alt  : constant Elsif_Stmt_Part_List := If_N.F_Alternatives;
               begin
                  Process_Decisions_Defer (If_N.F_Cond_Expr, 'I');
                  Set_Statement_Entry;

                  --  Now we traverse the statements in the THEN part

                  Traverse_Declarations_Or_Statements
                    (IC,
                     L => If_N.F_Then_Stmts.As_Ada_Node_List,
                     D => ('T', N));

                  --  Loop through ELSIF parts if present

                  declare
                     Saved_Dominant : constant Dominant_Info :=
                       Current_Dominant;

                  begin
                     for J in 1 .. If_N.F_Alternatives.Children_Count loop
                        declare
                           Elif : constant Elsif_Stmt_Part :=
                             Alt.Child (J).As_Elsif_Stmt_Part;
                        begin

                           --  An Elsif is executed only if the previous test
                           --  got a FALSE outcome.

                           Current_Dominant := ('F', Current_Test);

                           --  Now update current test information

                           Current_Test := Ada_Node (Elif);

                           --  We generate a statement sequence for the
                           --  construct "ELSIF condition", so that we have
                           --  a statement for the resulting decisions.

                           Extend_Statement_Sequence (Ada_Node (Elif), 'I');
                           Process_Decisions_Defer (Elif.F_Cond_Expr, 'I');
                           Set_Statement_Entry;

                           --  An ELSIF part is never guaranteed to have
                           --  been executed, following statements are only
                           --  dominated by the initial IF statement.

                           Current_Dominant := Saved_Dominant;

                           --  Traverse the statements in the ELSIF

                           Traverse_Declarations_Or_Statements
                             (IC,
                              L => Elif.F_Stmts.As_Ada_Node_List,
                              D => ('T', Ada_Node (Elif)));
                        end;
                     end loop;
                  end;

                  --  Finally traverse the ELSE statements if present

                  Traverse_Declarations_Or_Statements
                    (IC,
                     L => If_N.F_Else_Stmts.As_Ada_Node_List,
                     D => ('F', Current_Test));
               end;

            --  CASE statement, which breaks the current statement sequence,
            --  but we include the expression in the current sequence.

            when Ada_Case_Stmt =>
               Extend_Statement_Sequence (N, 'C');
               declare
                  Case_N : constant Case_Stmt := N.As_Case_Stmt;
                  Alt_L  : constant Case_Stmt_Alternative_List :=
                    Case_N.F_Alternatives;
               begin
                  Process_Decisions_Defer (Case_N.F_Expr, 'X');
                  Set_Statement_Entry;

                  --  Process case branches, all of which are dominated by the
                  --  CASE statement.

                  for J in 1 .. Alt_L.Children_Count loop
                     declare
                        Alt : constant Case_Stmt_Alternative :=
                          Alt_L.Child (J).As_Case_Stmt_Alternative;
                     begin
                        Traverse_Declarations_Or_Statements
                          (IC,
                           L => Alt.F_Stmts.As_Ada_Node_List,
                           D => Current_Dominant);
                     end;
                  end loop;
               end;

            --  ACCEPT statement

            when Ada_Accept_Stmt | Ada_Accept_Stmt_With_Stmts =>
               Extend_Statement_Sequence (N, 'A');
               Set_Statement_Entry;

               if N.Kind = Ada_Accept_Stmt_With_Stmts then
                  --  Process sequence of statements, dominant is the ACCEPT
                  --  statement.

                  Traverse_Handled_Statement_Sequence
                    (IC,
                     N => N.As_Accept_Stmt_With_Stmts.F_Stmts,
                     D => Current_Dominant);
               end if;

               --  SELECT statement

            --  (all 4 non-terminals: selective_accept, timed_entry_call,
            --  conditional_entry_call, and asynchronous_select).

            when Ada_Select_Stmt =>
               Extend_Statement_Sequence (N, 'S');
               Set_Statement_Entry;

               declare
                  Sel_N : constant Select_Stmt := As_Select_Stmt (N);
                  S_Dom : Dominant_Info;
               begin
                  for J in 1 .. Sel_N.F_Guards.Children_Count loop
                     declare
                        Alt : constant Select_When_Part :=
                          Sel_N.F_Guards.Child (J).As_Select_When_Part;
                        Guard : Expr;
                     begin
                        S_Dom := Current_Dominant;
                        Guard := Alt.F_Cond_Expr;

                        if not Guard.Is_Null then
                           Process_Decisions (IC, Guard, 'G');
                           Current_Dominant := ('T', Ada_Node (Guard));
                        end if;

                        Traverse_Declarations_Or_Statements
                          (IC,
                           L => Alt.F_Stmts.As_Ada_Node_List,
                           D => Current_Dominant);

                        Current_Dominant := S_Dom;
                     end;
                  end loop;

                  Traverse_Declarations_Or_Statements
                    (IC,
                     L => Sel_N.F_Else_Stmts.As_Ada_Node_List,
                     D => Current_Dominant);
                  Traverse_Declarations_Or_Statements
                    (IC,
                     L => Sel_N.F_Abort_Stmts.As_Ada_Node_List,
                     D => Current_Dominant);
               end;

            when Ada_Terminate_Alternative =>

               --  It is dubious to emit a statement SCO for a TERMINATE
               --  alternative, since no code is actually executed if the
               --  alternative is selected -- the tasking runtime call just
               --  never returns???

               Extend_Statement_Sequence (N, ' ');
               Set_Statement_Entry;

            --  Unconditional exit points, which are included in the current
            --  statement sequence, but then terminate it

            when Ada_Goto_Stmt
               | Ada_Raise_Stmt
               | Ada_Requeue_Stmt
            =>
               Extend_Statement_Sequence (N, ' ');
               Set_Statement_Entry;
               Current_Dominant := No_Dominant;

            --  Simple return statement. which is an exit point, but we
            --  have to process the return expression for decisions.

            when Ada_Return_Stmt =>
               Extend_Statement_Sequence (N, ' ');
               Process_Decisions_Defer
                 (N.As_Return_Stmt.F_Return_Expr, 'X');
               Set_Statement_Entry;
               Current_Dominant := No_Dominant;

            --  Extended return statement

            when Ada_Extended_Return_Stmt =>
               Extend_Statement_Sequence (N, 'R');
               declare
                  ER_N : constant Extended_Return_Stmt :=
                    N.As_Extended_Return_Stmt;
               begin
                  Process_Decisions_Defer (ER_N.F_Decl, 'X');
                  Set_Statement_Entry;

                  Traverse_Handled_Statement_Sequence
                    (IC,
                     N => ER_N.F_Stmts,
                     D => Current_Dominant);
               end;
               Current_Dominant := No_Dominant;

            --  Loop ends the current statement sequence, but we include
            --  the iteration scheme if present in the current sequence.
            --  But the body of the loop starts a new sequence, since it
            --  may not be executed as part of the current sequence.

            when Ada_Base_Loop_Stmt =>
               declare
                  Loop_S         : constant Base_Loop_Stmt :=
                    N.As_Base_Loop_Stmt;
                  ISC            : constant Loop_Spec := Loop_S.F_Spec;
                  Inner_Dominant : Dominant_Info     := No_Dominant;

               begin
                  if not ISC.Is_Null then

                     --  If iteration scheme present, extend the current
                     --  statement sequence to include the iteration scheme
                     --  and process any decisions it contains.

                     --  WHILE loop

                     if ISC.Kind = Ada_While_Loop_Spec then
                        Extend_Statement_Sequence (N, 'W');
                        Process_Decisions_Defer
                          (ISC.As_While_Loop_Spec.F_Expr, 'W');

                        --  Set more specific dominant for inner statements
                        --  (the control sloc for the decision is that of
                        --  the WHILE token).

                        Inner_Dominant := ('T', Ada_Node (ISC));

                     --  FOR loop

                     else
                        Extend_Statement_Sequence (N, 'F');

                        declare
                           ISC_For : constant For_Loop_Spec :=
                             ISC.As_For_Loop_Spec;
                           For_Param : constant For_Loop_Var_Decl :=
                             ISC_For.F_Var_Decl;
                        begin
                           --  loop_parameter_specification case

                           if not For_Param.Is_Null then
                              Process_Decisions_Defer
                                (Ada_Node (For_Param), 'X');

                           --  iterator_specification case

                           else
                              Process_Decisions_Defer
                                (ISC_For.F_Loop_Type, 'X');
                              Process_Decisions_Defer
                                (ISC_For.F_Iter_Expr, 'X');
                           end if;
                        end;
                     end if;
                  end if;

                  Set_Statement_Entry;

                  if Inner_Dominant = No_Dominant then
                     Inner_Dominant := Current_Dominant;
                  end if;

                  Traverse_Declarations_Or_Statements
                    (IC,
                     L => Loop_S.F_Stmts.As_Ada_Node_List,
                     D => Inner_Dominant);
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
                        | Name_Postcondition
                        | Name_Precondition
                     =>
                        --  For Assert/Check/Precondition/Postcondition, we
                        --  must generate a P entry for the decision. Note
                        --  that this is done unconditionally at this stage.
                        --  Output for disabled pragmas is suppressed later
                        --  on when we output the decision line in Put_SCOs,
                        --  depending on setting by Set_SCO_Pragma_Enabled.

                        if Nam = Name_Check then

                           --  Skip check name

                           Arg := 2;
                        end if;

                        Process_Decisions_Defer (Prag_Arg_Expr (Arg), 'P');
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
                        Typ := 'p';

                     when Name_Annotate =>
                        --  If this is a coverage exemption, record it

                        if Prag_Args.Children_Count >= 2
                          and then As_Symbol (Prag_Arg_Expr (1).As_Identifier)
                                     = As_Symbol (Xcov)
                        then
                           declare
                              use ALI_Files;

                              Ann_Sloc : constant Source_Location := Sloc (N);
                              Ann_Kind : constant Symbol_Type :=
                                As_Symbol (Prag_Arg_Expr (2).As_Identifier);
                              Ann : ALI_Annotation;
                           begin
                              Ann.Kind :=
                                ALI_Annotation_Kind'Value (Image (Ann_Kind));

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

                              Ann.CU := IC.CU;
                              ALI_Annotations.Insert
                                (Key => (Source_File => IC.SFI,
                                         L => (Line   =>
                                                 Positive (Ann_Sloc.Line),
                                               Column =>
                                                 Positive (Ann_Sloc.Column))),
                                 New_Item => Ann);

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
                     --  related pragmas: [Type_]Invariant,
                     --  [{Static,Dynamic}_]Predicate???

                     when others =>
                        Process_Decisions_Defer (N, 'X');
                        Typ := 'P';

                  end case;

                  --  Add statement SCO

                  Extend_Statement_Sequence (N, Typ);
               end;

            --  Aspects specification

            when Ada_Aspect_Spec =>
               Traverse_Aspects (N.As_Aspect_Spec);

            --  Object or named number declaration
            --  Generate a single SCO even if multiple defining identifiers
            --  are present.

            when Ada_Number_Decl
               | Ada_Object_Decl
            =>
               Extend_Statement_Sequence (N, 'o');

               if Has_Decision (N) then
                  Process_Decisions_Defer (N, 'X');
               end if;

            --  All other cases, which extend the current statement sequence
            --  but do not terminate it, even if they have nested decisions.

            when Ada_Protected_Type_Decl
               | Ada_Task_Type_Decl
            =>
               Extend_Statement_Sequence (N, 't');
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

               Traverse_Sync_Definition (IC, N);

            when Ada_Single_Protected_Decl
               | Ada_Single_Task_Decl
            =>
               Extend_Statement_Sequence (N, 'o');
               Set_Statement_Entry;

               Traverse_Sync_Definition (IC, N);

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

                     when Ada_Call_Stmt =>
                        Typ := ' ';

                     when others =>
                        if N.Kind in Ada_Stmt then
                           Typ := ' ';
                        else
                           Typ := 'd';
                        end if;
                  end case;

                  if Typ /= ASCII.NUL then
                     Extend_Statement_Sequence (N, Typ);
                  end if;
               end;

               --  Process any embedded decisions

               if Has_Decision (N) then
                  Process_Decisions_Defer (N, 'X');
               end if;
         end case;
      end Traverse_One;

      Items_Count : constant Natural :=
        (if L.Is_Null then 0 else L.Children_Count);

   --  Start of processing for Traverse_Declarations_Or_Statements

   begin
      --  Process single prefixed node

      if not P.Is_Null then
         Traverse_One (P);
      end if;

      --  Set up rewriting for the list case

      if not L.Is_Null then
         RH_Enclosing_List := Handle (L);
         Witness_Use_Statement := L.Kind = Ada_Stmt_List;
      end if;

      --  Loop through statements or declarations

      for J in 1 .. Items_Count loop
         declare
            N : constant Ada_Node := L.Child (J);
         begin
            Current_Index := Int (J);
            Traverse_One (N);
         end;
      end loop;

      --  End sequence of statements and flush deferred decisions

      if not P.Is_Null or else Items_Count > 0 then
         Set_Statement_Entry;
      end if;

      return Current_Dominant;
   end Traverse_Declarations_Or_Statements;

   ------------------------------------------
   -- Traverse_Generic_Package_Declaration --
   ------------------------------------------

   procedure Traverse_Generic_Package_Declaration
     (IC      : in out Unit_Inst_Context;
      N       : Generic_Package_Decl;
      Preelab : Boolean)
   is
   begin
      Process_Decisions (IC, N.F_Formal_Part, 'X');
      Traverse_Package_Declaration
        (IC, N.F_Package_Decl.As_Base_Package_Decl, Preelab);
   end Traverse_Generic_Package_Declaration;

   -----------------------------------------
   -- Traverse_Handled_Statement_Sequence --
   -----------------------------------------

   procedure Traverse_Handled_Statement_Sequence
     (IC : in out Unit_Inst_Context;
      N  : Handled_Stmts;
      D  : Dominant_Info := No_Dominant)
   is
   begin
      if N.Is_Null then
         return;
      end if;

      Traverse_Declarations_Or_Statements
        (IC, L => N.F_Stmts.As_Ada_Node_List, D => D);

      for J in 1 .. N.F_Exceptions.Children_Count loop
         declare
            Handler : constant Ada_Node := N.F_Exceptions.Child (J);
         begin
            --  Note: the exceptions list can also contain pragmas

            if Handler.Kind = Ada_Exception_Handler then
               Traverse_Declarations_Or_Statements
                 (IC,
                  L => Handler.As_Exception_Handler.F_Stmts.As_Ada_Node_List,
                  D => ('E', Handler));
            end if;
         end;
      end loop;
   end Traverse_Handled_Statement_Sequence;

   ---------------------------
   -- Traverse_Package_Body --
   ---------------------------

   procedure Traverse_Package_Body
     (IC      : in out Unit_Inst_Context;
      N       : Package_Body;
      Preelab : Boolean)
   is
   begin
      --  The first statement in the handled sequence of statements is
      --  dominated by the elaboration of the last declaration.

      Traverse_Handled_Statement_Sequence
        (IC,
         N => N.F_Stmts,
         D => Traverse_Declarations_Or_Statements
                (IC, N.F_Decls.F_Decls, Preelab));
   end Traverse_Package_Body;

   ----------------------------------
   -- Traverse_Package_Declaration --
   ----------------------------------

   procedure Traverse_Package_Declaration
     (IC      : in out Unit_Inst_Context;
      N       : Base_Package_Decl;
      Preelab : Boolean;
      D       : Dominant_Info := No_Dominant)
   is
      Private_Part_Dominant : constant Dominant_Info :=
         Traverse_Declarations_Or_Statements
           (IC, N.F_Public_Part.F_Decls, Preelab, D);
   begin
      if not N.F_Private_Part.Is_Null then

         --  First private declaration is dominated by last visible declaration

         Traverse_Declarations_Or_Statements
           (IC,
            L       => N.F_Private_Part.F_Decls,
            Preelab => Preelab,
            D       => Private_Part_Dominant);
      end if;
   end Traverse_Package_Declaration;

   ------------------------------
   -- Traverse_Sync_Definition --
   ------------------------------

   procedure Traverse_Sync_Definition
     (IC : in out Unit_Inst_Context;
      N  : Ada_Node)
   is
      Dom_Info : Dominant_Info := ('S', N);
      --  The first declaration is dominated by the protected or task [type]
      --  declaration.

      Vis_Decl  : Public_Part;
      Priv_Decl : Private_Part;
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
               Vis_Decl := T_Def.F_Public_Part;
               Priv_Decl := T_Def.F_Private_Part;
            end;

         when Ada_Task_Type_Decl =>
            declare
               T_Def : constant Task_Def :=
                 N.As_Task_Type_Decl.F_Definition;
            begin
               Vis_Decl := T_Def.F_Public_Part;
               Priv_Decl := T_Def.F_Private_Part;
            end;

         when others =>
            raise Program_Error;
      end case;

      --  Vis_Decl and Priv_Decl may be Empty at least for empty task type
      --  declarations. Querying F_Decls is invalid in this case.

      if not Vis_Decl.Is_Null then
         Dom_Info := Traverse_Declarations_Or_Statements
           (IC, L => Vis_Decl.F_Decls, D => Dom_Info);
      end if;

      if not Priv_Decl.Is_Null then
         --  If visible declarations are present, the first private declaration
         --  is dominated by the last visible declaration.

         Traverse_Declarations_Or_Statements
           (IC, L => Priv_Decl.F_Decls, D => Dom_Info);
      end if;
   end Traverse_Sync_Definition;

   --------------------------------------
   -- Traverse_Subprogram_Or_Task_Body --
   --------------------------------------

   procedure Traverse_Subprogram_Or_Task_Body
     (IC : in out Unit_Inst_Context;
      N  : Ada_Node;
      D  : Dominant_Info := No_Dominant)
   is
      Decls    : Declarative_Part;
      HSS      : Handled_Stmts;
      Dom_Info : Dominant_Info    := D;

      Saved_Local_Decls : constant Node_Rewriting_Handle := IC.Local_Decls;

   begin
      case Kind (N) is
         when Ada_Subp_Body =>
            declare
               SBN : constant Subp_Body := N.As_Subp_Body;
            begin
               Decls := SBN.F_Decls;
               HSS   := SBN.F_Stmts;
            end;

         when Ada_Task_Body =>
            declare
               TBN : constant Task_Body := N.As_Task_Body;
            begin
               Decls := TBN.F_Decls;
               HSS   := TBN.F_Stmts;
            end;

         when others =>
            raise Program_Error;
      end case;

      IC.Local_Decls := Handle (Decls.F_Decls);

      --  If declarations are present, the first statement is dominated by the
      --  last declaration.

      Dom_Info := Traverse_Declarations_Or_Statements
        (IC, L => Decls.F_Decls, D => Dom_Info);

      Traverse_Handled_Statement_Sequence (IC, N => HSS, D => Dom_Info);

      IC.Local_Decls := Saved_Local_Decls;
   end Traverse_Subprogram_Or_Task_Body;

   -----------------------
   -- Process_Decisions --
   -----------------------

   procedure Process_Decisions
     (IC : in out Unit_Inst_Context;
      N  : Ada_Node'Class;
      T  : Character)
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

      procedure Output_Decision_Operand (Operand : Expr);
      --  The node Operand is the top level logical operator of a decision, or
      --  it is one of the operands of a logical operator belonging to a single
      --  complex decision. This (recursive) routine outputs the sequence of
      --  table entries corresponding to the node. Note that we do not process
      --  the sub- operands to look for further decisions, that processing is
      --  done in Find_Nested_Decisions, because we can't get decisions mixed
      --  up in the global table. Call has no effect if Operand is Empty.
      --  Increments Condition_Count (recursively) for each condition.

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

      procedure Output_Decision_Operand (Operand : Expr) is
         C1 : Character;
         C2 : Character;
         --  C1 holds a character that identifies the operation while C2
         --  indicates whether we are sure (' ') or not ('?') this operation
         --  belongs to the decision. '?' entries will be filtered out in the
         --  second (SCO_Record_Filtered) pass.

         N : constant Expr := Unwrap (Operand);

         L, R : Expr;
         T    : Tristate;

         Op_N  : Op;
         Op_NK : Ada_Node_Kind_Type;

      begin
         T := Is_Logical_Operator (N);

         --  Logical operator

         if T /= False then
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

            if T = True then
               C2 := ' ';
            else
               C2 := '?';
            end if;

            Append_SCO
              (C1   => C1,
               C2   => C2,
               From => Sloc (Op_N),
               To   => No_Source_Location,
               Last => False);

            Hash_Entries.Append ((Sloc (N), SCOs.SCO_Table.Last));

            if not L.Is_Null then
               Output_Decision_Operand (L);
            end if;
            Output_Decision_Operand (R);

         --  Not a logical operator -> condition

         else
            Output_Element (N.As_Ada_Node);

            if MCDC_Coverage_Enabled then
               IC.Source_Conditions.Append
                 ((LL_SCO    => SCOs.SCO_Table.Last,
                   Condition => N.As_Expr,
                   State     => MCDC_State,
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
      begin
         Append_SCO
           (C1   => ' ',
            C2   => 'c',
            From => Start_Sloc (N_SR),
            To   => End_Sloc (N_SR),
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
               --  Argument N is the pragma argument, and we have to go up
               --  two levels (through the pragma argument association) to
               --  get to the pragma node itself.

               Loc := Sloc (Parent (Parent (N)));

            when 'X' =>

               --  For an expression, no Sloc???

               null;

            --  No other possibilities

            when others =>
               raise Program_Error;
         end case;

         Append_SCO
           (C1                 => T,
            C2                 => ' ',
            From               => Loc,
            To                 => No_Source_Location,
            Last               => False,
            Pragma_Aspect_Name => Nam);

         Current_Decision := SCOs.SCO_Table.Last;

         if Coverage.Enabled (Coverage.Decision)
           or else MCDC_Coverage_Enabled
         then
            if MCDC_Coverage_Enabled then
               Condition_Count := 0;

               if IC.Local_Decls = No_Node_Rewriting_Handle then
                  Report (IC, N,
                          "gnatcov limitation: "
                          & "cannot find local declarative part for MC/DC",
                          Kind => Diagnostics.Error);
               else
                  MCDC_State :=
                    To_Unbounded_String
                      (Make_MCDC_State_Name (SCOs.SCO_Table.Last));
                  Insert_Child
                    (IC.Local_Decls, 1,
                     Make_MCDC_State (IC, To_String (MCDC_State)));
               end if;
            end if;

            IC.Source_Decisions.Append
              ((LL_SCO   => Current_Decision,
                Decision => N.As_Expr,
                State    => MCDC_State));
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
         if Is_Logical_Operator (N) /= False then
            if N.Kind = Ada_Un_Op then
               Find_Nested_Decisions (N.As_Un_Op.F_Expr);

            else
               Find_Nested_Decisions (N.As_Bin_Op.F_Left);
               Find_Nested_Decisions (N.As_Bin_Op.F_Right);
               X_Not_Decision := False;
            end if;

         else
            Process_Decisions (IC, N, 'X');
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
            and then Is_Logical_Operator (N.As_Expr) /= False);

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

               Output_Decision_Operand (EN);

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
                  Process_Decisions (IC, IEN.F_Cond_Expr, 'I');
                  Process_Decisions (IC, IEN.F_Then_Expr, 'X');

                  for J in 1 .. Alt.Children_Count loop
                     declare
                        EIN : constant Elsif_Expr_Part :=
                          Alt.Child (J).As_Elsif_Expr_Part;
                     begin
                        Process_Decisions (IC, EIN.F_Cond_Expr, 'I');
                        Process_Decisions (IC, EIN.F_Then_Expr, 'X');
                     end;
                  end loop;

                  Process_Decisions (IC, IEN.F_Else_Expr, 'X');
                  return Over;
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
   end Process_Decisions;

   ------------------
   -- Has_Decision --
   ------------------

   function Has_Decision (T : Ada_Node'Class) return Boolean is
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
           and then (Is_Logical_Operator (N) /= False
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

   function Is_Logical_Operator (N : Ada_Node'Class) return Tristate is
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
               --  Ada_Op_Not should be Unkwown???

            when Ada_Op_And_Then | Ada_Op_Or_Else =>
               return True;

            when Ada_Op_And | Ada_Op_Or =>
               return Unknown;

            when others =>
               return False;
         end case;
      end;
   end Is_Logical_Operator;

   --------------
   -- Operator --
   --------------

   function Operator (N : Expr) return Op is
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

end Instrument.Tree;
