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

--  Source instrumentation

with Langkit_Support.Slocs;   use Langkit_Support.Slocs;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Libadalang.Analysis;     use Libadalang.Analysis;
with Libadalang.Common;       use Libadalang.Common;
with Libadalang.Lexer;        use Libadalang.Lexer;
with Libadalang.Sources;      use Libadalang.Sources;

with Types; use Types;
with Table;

package body Instrument is

   Symbols : Symbol_Table := Create_Symbol_Table;
   --  Holder for name singletons

   Aspect_Dynamic_Predicate : constant Symbol_Type := Find
     (Symbols, Canonicalize ("Dynamic_Predicate").Symbol);
   Aspect_Invariant         : constant Symbol_Type := Find
     (Symbols, Canonicalize ("Invariant").Symbol);
   Aspect_Post              : constant Symbol_Type := Find
     (Symbols, Canonicalize ("Post").Symbol);
   Aspect_Postcondition     : constant Symbol_Type := Find
     (Symbols, Canonicalize ("Postcondition").Symbol);
   Aspect_Pre               : constant Symbol_Type := Find
     (Symbols, Canonicalize ("Pre").Symbol);
   Aspect_Precondition      : constant Symbol_Type := Find
     (Symbols, Canonicalize ("Precondition").Symbol);
   Aspect_Predicate         : constant Symbol_Type := Find
     (Symbols, Canonicalize ("Predicate").Symbol);
   Aspect_Static_Predicate  : constant Symbol_Type := Find
     (Symbols, Canonicalize ("Static_Predicate").Symbol);
   Aspect_Type_Invariant    : constant Symbol_Type := Find
     (Symbols, Canonicalize ("Type_Invariant").Symbol);

   function Pragma_Name (P : Pragma_Node) return Symbol_Type;
   --  Return a symbol from Symbols corresponding to the name of the given
   --  P pragma.

   function Aspect_Assoc_Name (A : Aspect_Assoc) return Symbol_Type;
   --  Return a symbol from Symbols corresponding to the name of the given
   --  A aspect association.

   procedure Append_SCO
     (C1, C2             : Character;
      From, To           : Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Symbol_Type := null) is null;
   --  ???

   Current_Pragma_Sloc : Source_Location := No_Source_Location;
   --  Start location for the currently enclosing pragma, if any

   type Dominant_Info is record
      K : Character;
      --  F/T/S/E for a valid dominance marker, or ' ' for no dominant

      N : Ada_Node;
      --  Node providing the Sloc(s) for the dominance marker
   end record;
   No_Dominant : constant Dominant_Info := (' ', No_Ada_Node);

   procedure Traverse_Declarations_Or_Statements
     (L : Ada_Node;
      D : Dominant_Info := No_Dominant;
      P : Ada_Node      := No_Ada_Node);
   --  Process L, a list of statements or declarations dominated by D. If P is
   --  present, it is processed as though it had been prepended to L.

   function Traverse_Declarations_Or_Statements
     (L : Ada_Node;
      D : Dominant_Info := No_Dominant;
      P : Ada_Node      := No_Ada_Node) return Dominant_Info;
   --  Same as above, and returns dominant information corresponding to the
   --  last node with SCO in L.

   --  The following Traverse_* routines perform appropriate calls to
   --  Traverse_Declarations_Or_Statements to traverse specific node kinds.
   --  Parameter D, when present, indicates the dominant of the first
   --  declaration or statement within N.

   --  Why is Traverse_Sync_Definition commented specifically, whereas
   --  the others are not???

   procedure Traverse_Generic_Package_Declaration (N : Ada_Node);

   procedure Traverse_Handled_Statement_Sequence
     (N : Ada_Node;
      D : Dominant_Info := No_Dominant);

   procedure Traverse_Package_Body (N : Ada_Node);

   procedure Traverse_Package_Declaration
     (N : Ada_Node;
      D : Dominant_Info := No_Dominant);

   procedure Traverse_Subprogram_Or_Task_Body
     (N : Ada_Node;
      D : Dominant_Info := No_Dominant);

   procedure Traverse_Sync_Definition (N : Ada_Node);
   --  Traverse a protected definition or task definition

   --  Note regarding traversals: In a few cases where an Alternatives list is
   --  involved, pragmas such as "pragma Page" may show up before the first
   --  alternative. We skip them because we're out of statement or declaration
   --  context, so these can't be pragmas of interest for SCO purposes, and
   --  the regular alternative processing typically involves attribute queries
   --  which aren't valid for a pragma.

   procedure Process_Decisions
     (N           : Ada_Node;
      T           : Character;
      Pragma_Sloc : Source_Location);
   --  If N is Empty, has no effect. Otherwise scans the tree for the node N,
   --  to output any decisions it contains. T is one of IEGPWX (for context of
   --  expression: if/exit when/entry guard/pragma/while/expression). If T is
   --  other than X, the node N is the if expression involved, and a decision
   --  is always present (at the very least a simple decision is present at the
   --  top level).

   --------------------------
   -- Internal Subprograms --
   --------------------------

   function Has_Decision (E : Expr) return Boolean;
   --  E is the node for a subexpression. Returns True if the subexpression
   --  contains a nested decision (i.e. either is a logical operator, or
   --  contains a logical operator in its subtree).

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
      Plo : Source_Location;
   end record;
   --  Used to store a single entry in the following table. Nod is the node to
   --  be searched for decisions for the case of Process_Decisions_Defer with a
   --  node argument (with Lst set to No_Ada_Node. Lst is the list to be
   --  searched for decisions for the case of Process_Decisions_Defer with a
   --  List argument (in which case Nod is set to No_Ada_Node). Plo is the sloc
   --  of the enclosing pragma, if any.

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
     (L : Ada_Node;
      D : Dominant_Info := No_Dominant;
      P : Ada_Node      := No_Ada_Node)
   is
      Discard_Dom : Dominant_Info;
      pragma Warnings (Off, Discard_Dom);
   begin
      Discard_Dom := Traverse_Declarations_Or_Statements (L, D, P);
   end Traverse_Declarations_Or_Statements;

   function Traverse_Declarations_Or_Statements
     (L : Ada_Node;
      D : Dominant_Info := No_Dominant;
      P : Ada_Node      := No_Ada_Node) return Dominant_Info
   is
      Current_Dominant : Dominant_Info := D;
      --  Dominance information for the current basic block

      Current_Test : Ada_Node;
      --  Conditional node (N_If_Statement or N_Elsiif being processed

      N : Ada_Node;

      SC_First : constant Nat := SC.Last + 1;
      SD_First : constant Nat := SD.Last + 1;
      --  Record first entries used in SC/SD at this recursive level

      procedure Extend_Statement_Sequence (N : Ada_Node; Typ : Character);
      --  Extend the current statement sequence to encompass the node N. Typ is
      --  the letter that identifies the type of statement/declaration that is
      --  being added to the sequence.

      procedure Process_Decisions_Defer (N : Ada_Node; T : Character);
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

      procedure Traverse_Aspects (N : Aspect_Spec);
      --  Helper for Traverse_One: traverse N's aspect specifications

      procedure Traverse_Degenerate_Subprogram (N : Ada_Node);
      --  Common code to handle null procedures and expression functions. Emit
      --  a SCO of the given Kind and N outside of the dominance flow.

      -------------------------------
      -- Extend_Statement_Sequence --
      -------------------------------

      procedure Extend_Statement_Sequence (N : Ada_Node; Typ : Character) is
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
               To_Node := N;

            when others =>
               null;
         end case;

         if not To_Node.Is_Null then
            T := End_Sloc (To_Node.Sloc_Range);
         end if;

         SC.Append ((N, F, T, Typ));
      end Extend_Statement_Sequence;

      -----------------------------
      -- Process_Decisions_Defer --
      -----------------------------

      procedure Process_Decisions_Defer (N : Ada_Node; T : Character) is
      begin
         SD.Append ((N, T, Current_Pragma_Sloc));
      end Process_Decisions_Defer;

      -------------------------
      -- Set_Statement_Entry --
      -------------------------

      procedure Set_Statement_Entry is
         SC_Last : constant Int := SC.Last;
         SD_Last : constant Int := SD.Last;

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
                    (C1                 => '>',
                     C2                 => Current_Dominant.K,
                     From               => From,
                     To                 => To,
                     Last               => False,
                     Pragma_Aspect_Name => null);
               end;
            end if;

            declare
               SCE                : SC_Entry renames SC.Table (J);
               Pragma_Aspect_Name : Symbol_Type := null;

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
               Process_Decisions (SDE.Nod, SDE.Typ, SDE.Plo);
            end;
         end loop;

         --  Clear out used section of SD table

         SD.Set_Last (SD_First - 1);
      end Set_Statement_Entry;

      ----------------------
      -- Traverse_Aspects --
      ----------------------

      procedure Traverse_Aspects (N : Aspect_Spec) is
         AL : constant Aspect_Assoc_List := N.F_Aspect_Assocs;
         AN : Aspect_Assoc;
         AE : Expr;
         C1 : Character;

      begin
         for I in 1 .. AL.Children_Count loop
            AN := AL.Child (I).As_Aspect_Assoc;
            AE := AN.F_Expr;

            C1 := ASCII.NUL;

            if Aspect_Assoc_Name (AN) in Aspect_Dynamic_Predicate
                                       | Aspect_Invariant
                                       | Aspect_Post
                                       | Aspect_Postcondition
                                       | Aspect_Pre
                                       | Aspect_Precondition
                                       | Aspect_Predicate
                                       | Aspect_Static_Predicate
                                       | Aspect_Type_Invariant
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
               pragma Assert (Current_Pragma_Sloc = No_Source_Location);

               if C1 = 'A' then
                  Current_Pragma_Sloc := Start_Sloc (AN.Sloc_Range);
               end if;

               Process_Decisions_Defer (AE.As_Ada_Node, C1);

               Current_Pragma_Sloc := No_Source_Location;
            end if;
         end loop;
      end Traverse_Aspects;

      ------------------------------------
      -- Traverse_Degenerate_Subprogram --
      ------------------------------------

      procedure Traverse_Degenerate_Subprogram (N : Ada_Node) is
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

            --  Package declaration

            when Ada_Package_Decl =>
               Set_Statement_Entry;
               Traverse_Package_Declaration (N, Current_Dominant);

            --  Generic package declaration

            when Ada_Generic_Package_Decl =>
               Set_Statement_Entry;
               Traverse_Generic_Package_Declaration (N);

            --  Package body

            when Ada_Package_Body =>
               Set_Statement_Entry;
               Traverse_Package_Body (N);

            --  Subprogram declaration or subprogram body stub

            when Ada_Expr_Function
               | Ada_Subp_Body_Stub
               | Ada_Subp_Decl
            =>
               declare
                  Spec : constant Subp_Spec :=
                    (case N.Kind is
                        when Ada_Expr_Function  =>
                          As_Expr_Function (N).F_Subp_Spec,
                        when Ada_Subp_Body_Stub =>
                           As_Subp_Body_Stub (N).F_Subp_Spec,
                        when Ada_Subp_Decl | Ada_Null_Subp_Decl =>
                          As_Classic_Subp_Decl (N).F_Subp_Spec,
                        when others             => raise Program_Error);
               begin
                  Process_Decisions_Defer (Ada_Node (Spec.F_Subp_Params), 'X');

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
                     Traverse_Degenerate_Subprogram (Ada_Node (Spec));

                  --  Case of an expression function: generate a statement SCO
                  --  for the expression (and then decision SCOs for any nested
                  --  decisions).

                  elsif N.Kind = Ada_Expr_Function then
                     Traverse_Degenerate_Subprogram
                       (Ada_Node (N.As_Expr_Function.F_Expr));
                  end if;
               end;

            --  Entry declaration

            when Ada_Entry_Declaration =>
               Process_Decisions_Defer (Parameter_Specifications (N), 'X');

            --  Generic subprogram declaration

            when Ada_Generic_Subprogram_Declaration =>
               Process_Decisions_Defer
                 (Generic_Formal_Declarations (N), 'X');
               Process_Decisions_Defer
                 (Parameter_Specifications (Specification (N)), 'X');

            --  Task or subprogram body

            when Ada_Subprogram_Body
               | Ada_Task_Body
            =>
               Set_Statement_Entry;
               Traverse_Subprogram_Or_Task_Body (N);

            --  Entry body

            when Ada_Entry_Body =>
               declare
                  Cond : constant Ada_Node :=
                           Condition (Entry_Body_Formal_Part (N));

                  Inner_Dominant : Dominant_Info := No_Dominant;

               begin
                  Set_Statement_Entry;

                  if Present (Cond) then
                     Process_Decisions_Defer (Cond, 'G');

                     --  For an entry body with a barrier, the entry body
                     --  is dominanted by a True evaluation of the barrier.

                     Inner_Dominant := ('T', N);
                  end if;

                  Traverse_Subprogram_Or_Task_Body (N, Inner_Dominant);
               end;

            --  Protected body

            when Ada_Protected_Body =>
               Set_Statement_Entry;
               Traverse_Declarations_Or_Statements (Declarations (N));

            --  Exit statement, which is an exit statement in the SCO sense,
            --  so it is included in the current statement sequence, but
            --  then it terminates this sequence. We also have to process
            --  any decisions in the exit statement expression.

            when Ada_Exit_Statement =>
               Extend_Statement_Sequence (N, 'E');
               Process_Decisions_Defer (Condition (N), 'E');
               Set_Statement_Entry;

               --  If condition is present, then following statement is
               --  only executed if the condition evaluates to False.

               if Present (Condition (N)) then
                  Current_Dominant := ('F', N);
               else
                  Current_Dominant := No_Dominant;
               end if;

            --  Label, which breaks the current statement sequence, but the
            --  label itself is not included in the next statement sequence,
            --  since it generates no code.

            when Ada_Label =>
               Set_Statement_Entry;
               Current_Dominant := No_Dominant;

            --  Block statement, which breaks the current statement sequence

            when Ada_Block_Statement =>
               Set_Statement_Entry;

               --  The first statement in the handled sequence of statements
               --  is dominated by the elaboration of the last declaration.

               Current_Dominant := Traverse_Declarations_Or_Statements
                                     (L => Declarations (N),
                                      D => Current_Dominant);

               Traverse_Handled_Statement_Sequence
                 (N => Handled_Statement_Sequence (N),
                  D => Current_Dominant);

            --  If statement, which breaks the current statement sequence,
            --  but we include the condition in the current sequence.

            when Ada_If_Statement =>
               Current_Test := N;
               Extend_Statement_Sequence (N, 'I');
               Process_Decisions_Defer (Condition (N), 'I');
               Set_Statement_Entry;

               --  Now we traverse the statements in the THEN part

               Traverse_Declarations_Or_Statements
                 (L => Then_Statements (N),
                  D => ('T', N));

               --  Loop through ELSIF parts if present

               if Present (Elsif_Parts (N)) then
                  declare
                     Saved_Dominant : constant Dominant_Info :=
                                        Current_Dominant;

                     Elif : Ada_Node := First (Elsif_Parts (N));

                  begin
                     while Present (Elif) loop

                        --  An Elsif is executed only if the previous test
                        --  got a FALSE outcome.

                        Current_Dominant := ('F', Current_Test);

                        --  Now update current test information

                        Current_Test := Elif;

                        --  We generate a statement sequence for the
                        --  construct "ELSIF condition", so that we have
                        --  a statement for the resulting decisions.

                        Extend_Statement_Sequence (Elif, 'I');
                        Process_Decisions_Defer (Condition (Elif), 'I');
                        Set_Statement_Entry;

                        --  An ELSIF part is never guaranteed to have
                        --  been executed, following statements are only
                        --  dominated by the initial IF statement.

                        Current_Dominant := Saved_Dominant;

                        --  Traverse the statements in the ELSIF

                        Traverse_Declarations_Or_Statements
                          (L => Then_Statements (Elif),
                           D => ('T', Elif));
                        Next (Elif);
                     end loop;
                  end;
               end if;

               --  Finally traverse the ELSE statements if present

               Traverse_Declarations_Or_Statements
                 (L => Else_Statements (N),
                  D => ('F', Current_Test));

            --  CASE statement, which breaks the current statement sequence,
            --  but we include the expression in the current sequence.

            when Ada_Case_Statement =>
               Extend_Statement_Sequence (N, 'C');
               Process_Decisions_Defer (Expression (N), 'X');
               Set_Statement_Entry;

               --  Process case branches, all of which are dominated by the
               --  CASE statement.

               declare
                  Alt : Ada_Node;
               begin
                  Alt := First_Non_Pragma (Alternatives (N));
                  while Present (Alt) loop
                     Traverse_Declarations_Or_Statements
                       (L => Statements (Alt),
                        D => Current_Dominant);
                     Next (Alt);
                  end loop;
               end;

            --  ACCEPT statement

            when Ada_Accept_Statement =>
               Extend_Statement_Sequence (N, 'A');
               Set_Statement_Entry;

               --  Process sequence of statements, dominant is the ACCEPT
               --  statement.

               Traverse_Handled_Statement_Sequence
                 (N => Handled_Statement_Sequence (N),
                  D => Current_Dominant);

            --  SELECT

            when Ada_Selective_Accept =>
               Extend_Statement_Sequence (N, 'S');
               Set_Statement_Entry;

               --  Process alternatives

               declare
                  Alt   : Ada_Node;
                  Guard : Ada_Node;
                  S_Dom : Dominant_Info;

               begin
                  Alt := First (Select_Alternatives (N));
                  while Present (Alt) loop
                     S_Dom := Current_Dominant;
                     Guard := Condition (Alt);

                     if Present (Guard) then
                        Process_Decisions
                          (Guard,
                           'G',
                           Pragma_Sloc => No_Location);
                        Current_Dominant := ('T', Guard);
                     end if;

                     Traverse_One (Alt);

                     Current_Dominant := S_Dom;
                     Next (Alt);
                  end loop;
               end;

               Traverse_Declarations_Or_Statements
                 (L => Else_Statements (N),
                  D => Current_Dominant);

            when Ada_Conditional_Entry_Call
               | Ada_Timed_Entry_Call
            =>
               Extend_Statement_Sequence (N, 'S');
               Set_Statement_Entry;

               --  Process alternatives

               Traverse_One (Entry_Call_Alternative (N));

               if Nkind (N) = Ada_Timed_Entry_Call then
                  Traverse_One (Delay_Alternative (N));
               else
                  Traverse_Declarations_Or_Statements
                    (L => Else_Statements (N),
                     D => Current_Dominant);
               end if;

            when Ada_Asynchronous_Select =>
               Extend_Statement_Sequence (N, 'S');
               Set_Statement_Entry;

               Traverse_One (Triggering_Alternative (N));
               Traverse_Declarations_Or_Statements
                 (L => Statements (Abortable_Part (N)),
                  D => Current_Dominant);

            when Ada_Accept_Alternative =>
               Traverse_Declarations_Or_Statements
                 (L => Statements (N),
                  D => Current_Dominant,
                  P => Accept_Statement (N));

            when Ada_Entry_Call_Alternative =>
               Traverse_Declarations_Or_Statements
                 (L => Statements (N),
                  D => Current_Dominant,
                  P => Entry_Call_Statement (N));

            when Ada_Delay_Alternative =>
               Traverse_Declarations_Or_Statements
                 (L => Statements (N),
                  D => Current_Dominant,
                  P => Delay_Statement (N));

            when Ada_Triggering_Alternative =>
               Traverse_Declarations_Or_Statements
                 (L => Statements (N),
                  D => Current_Dominant,
                  P => Triggering_Statement (N));

            when Ada_Terminate_Alternative =>

               --  It is dubious to emit a statement SCO for a TERMINATE
               --  alternative, since no code is actually executed if the
               --  alternative is selected -- the tasking runtime call just
               --  never returns???

               Extend_Statement_Sequence (N, ' ');
               Set_Statement_Entry;

            --  Unconditional exit points, which are included in the current
            --  statement sequence, but then terminate it

            when Ada_Goto_Statement
               | Ada_Raise_Statement
               | Ada_Requeue_Statement
            =>
               Extend_Statement_Sequence (N, ' ');
               Set_Statement_Entry;
               Current_Dominant := No_Dominant;

            --  Simple return statement. which is an exit point, but we
            --  have to process the return expression for decisions.

            when Ada_Simple_Return_Statement =>
               Extend_Statement_Sequence (N, ' ');
               Process_Decisions_Defer (Expression (N), 'X');
               Set_Statement_Entry;
               Current_Dominant := No_Dominant;

            --  Extended return statement

            when Ada_Extended_Return_Statement =>
               Extend_Statement_Sequence (N, 'R');
               Process_Decisions_Defer (Return_Object_Declarations (N), 'X');
               Set_Statement_Entry;

               Traverse_Handled_Statement_Sequence
                 (N => Handled_Statement_Sequence (N),
                  D => Current_Dominant);

               Current_Dominant := No_Dominant;

            --  Loop ends the current statement sequence, but we include
            --  the iteration scheme if present in the current sequence.
            --  But the body of the loop starts a new sequence, since it
            --  may not be executed as part of the current sequence.

            when Ada_Loop_Statement =>
               declare
                  ISC            : constant Ada_Node := Iteration_Scheme (N);
                  Inner_Dominant : Dominant_Info     := No_Dominant;

               begin
                  if Present (ISC) then

                     --  If iteration scheme present, extend the current
                     --  statement sequence to include the iteration scheme
                     --  and process any decisions it contains.

                     --  While loop

                     if Present (Condition (ISC)) then
                        Extend_Statement_Sequence (N, 'W');
                        Process_Decisions_Defer (Condition (ISC), 'W');

                        --  Set more specific dominant for inner statements
                        --  (the control sloc for the decision is that of
                        --  the WHILE token).

                        Inner_Dominant := ('T', ISC);

                     --  For loop

                     else
                        Extend_Statement_Sequence (N, 'F');
                        Process_Decisions_Defer
                          (Loop_Parameter_Specification (ISC), 'X');
                     end if;
                  end if;

                  Set_Statement_Entry;

                  if Inner_Dominant = No_Dominant then
                     Inner_Dominant := Current_Dominant;
                  end if;

                  Traverse_Declarations_Or_Statements
                    (L => Statements (N),
                     D => Inner_Dominant);
               end;

            --  Pragma

            when Ada_Pragma =>

               --  Record sloc of pragma (pragmas don't nest)

               pragma Assert (Current_Pragma_Sloc = No_Location);
               Current_Pragma_Sloc := Sloc (N);

               --  Processing depends on the kind of pragma

               declare
                  Nam : constant Name_Id := Pragma_Name_Unmapped (N);
                  Arg : Ada_Node         :=
                          First (Pragma_Argument_Associations (N));
                  Typ : Character;

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
                           Next (Arg);
                        end if;

                        Process_Decisions_Defer (Expression (Arg), 'P');
                        Typ := 'p';

                        --  Pre/postconditions can be inherited so SCO should
                        --  never be deactivated???

                     when Name_Debug =>
                        if Present (Arg) and then Present (Next (Arg)) then

                           --  Case of a dyadic pragma Debug: first argument
                           --  is a P decision, any nested decision in the
                           --  second argument is an X decision.

                           Process_Decisions_Defer (Expression (Arg), 'P');
                           Next (Arg);
                        end if;

                        Process_Decisions_Defer (Expression (Arg), 'X');
                        Typ := 'p';

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

                  Current_Pragma_Sloc := No_Location;
               end;

            --  Object declaration. Ignored if Prev_Ids is set, since the
            --  parser generates multiple instances of the whole declaration
            --  if there is more than one identifier declared, and we only
            --  want one entry in the SCOs, so we take the first, for which
            --  Prev_Ids is False.

            when Ada_Number_Declaration
               | Ada_Object_Declaration
            =>
               if not Prev_Ids (N) then
                  Extend_Statement_Sequence (N, 'o');

                  if Has_Decision (N) then
                     Process_Decisions_Defer (N, 'X');
                  end if;
               end if;

            --  All other cases, which extend the current statement sequence
            --  but do not terminate it, even if they have nested decisions.

            when Ada_Protected_Type_Declaration
               | Ada_Task_Type_Declaration
            =>
               Extend_Statement_Sequence (N, 't');
               Process_Decisions_Defer (Discriminant_Specifications (N), 'X');
               Set_Statement_Entry;

               Traverse_Sync_Definition (N);

            when Ada_Single_Protected_Declaration
               | Ada_Single_Task_Declaration
            =>
               Extend_Statement_Sequence (N, 'o');
               Set_Statement_Entry;

               Traverse_Sync_Definition (N);

            when others =>

               --  Determine required type character code, or ASCII.NUL if
               --  no SCO should be generated for this node.

               declare
                  NK  : constant Node_Kind := Nkind (N);
                  Typ : Character;

               begin
                  case NK is
                     when Ada_Full_Type_Declaration
                        | Ada_Incomplete_Type_Declaration
                        | Ada_Private_Extension_Declaration
                        | Ada_Private_Type_Declaration
                     =>
                        Typ := 't';

                     when Ada_Subtype_Declaration =>
                        Typ := 's';

                     when Ada_Renaming_Declaration =>
                        Typ := 'r';

                     when Ada_Generic_Instantiation =>
                        Typ := 'i';

                     when Ada_Package_Body_Stub
                        | Ada_Protected_Body_Stub
                        | Ada_Representation_Clause
                        | Ada_Task_Body_Stub
                        | Ada_Use_Package_Clause
                        | Ada_Use_Type_Clause
                     =>
                        Typ := ASCII.NUL;

                     when Ada_Procedure_Call_Statement =>
                        Typ := ' ';

                     when others =>
                        if NK in Ada_Statement_Other_Than_Procedure_Call then
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

         --  Process aspects if present

         Traverse_Aspects (N);
      end Traverse_One;

   --  Start of processing for Traverse_Declarations_Or_Statements

   begin
      --  Process single prefixed node

      if Present (P) then
         Traverse_One (P);
      end if;

      --  Loop through statements or declarations

      if Is_Non_Empty_List (L) then
         N := First (L);
         while Present (N) loop

            --  Note: For separate bodies, we see the tree after Par.Labl has
            --  introduced implicit labels, so we need to ignore those nodes.

            if Nkind (N) /= Ada_Implicit_Label_Declaration then
               Traverse_One (N);
            end if;

            Next (N);
         end loop;

      end if;

      --  End sequence of statements and flush deferred decisions

      if Present (P) or else Is_Non_Empty_List (L) then
         Set_Statement_Entry;
      end if;

      return Current_Dominant;
   end Traverse_Declarations_Or_Statements;

   ------------------------------------------
   -- Traverse_Generic_Package_Declaration --
   ------------------------------------------

   procedure Traverse_Generic_Package_Declaration (N : Ada_Node) is
   begin
      Process_Decisions (Generic_Formal_Declarations (N), 'X', No_Location);
      Traverse_Package_Declaration (N);
   end Traverse_Generic_Package_Declaration;

   -----------------------------------------
   -- Traverse_Handled_Statement_Sequence --
   -----------------------------------------

   procedure Traverse_Handled_Statement_Sequence
     (N : Ada_Node;
      D : Dominant_Info := No_Dominant)
   is
      Handler : Ada_Node;

   begin
      --  For package bodies without a statement part, the parser adds an empty
      --  one, to normalize the representation. The null statement therein,
      --  which does not come from source, does not get a SCO.

      if Present (N) and then Comes_From_Source (N) then
         Traverse_Declarations_Or_Statements (Statements (N), D);

         if Present (Exception_Handlers (N)) then
            Handler := First_Non_Pragma (Exception_Handlers (N));
            while Present (Handler) loop
               Traverse_Declarations_Or_Statements
                 (L => Statements (Handler),
                  D => ('E', Handler));
               Next (Handler);
            end loop;
         end if;
      end if;
   end Traverse_Handled_Statement_Sequence;

   ---------------------------
   -- Traverse_Package_Body --
   ---------------------------

   procedure Traverse_Package_Body (N : Ada_Node) is
      Dom : Dominant_Info;
   begin
      --  The first statement in the handled sequence of statements is
      --  dominated by the elaboration of the last declaration.

      Dom := Traverse_Declarations_Or_Statements (Declarations (N));

      Traverse_Handled_Statement_Sequence
        (Handled_Statement_Sequence (N), Dom);
   end Traverse_Package_Body;

   ----------------------------------
   -- Traverse_Package_Declaration --
   ----------------------------------

   procedure Traverse_Package_Declaration
     (N : Ada_Node;
      D : Dominant_Info := No_Dominant)
   is
      Spec : constant Ada_Node := Specification (N);
      Dom  : Dominant_Info;

   begin
      Dom :=
        Traverse_Declarations_Or_Statements (Visible_Declarations (Spec), D);

      --  First private declaration is dominated by last visible declaration

      Traverse_Declarations_Or_Statements (Private_Declarations (Spec), Dom);
   end Traverse_Package_Declaration;

   ------------------------------
   -- Traverse_Sync_Definition --
   ------------------------------

   procedure Traverse_Sync_Definition (N : Ada_Node) is
      Dom_Info : Dominant_Info := ('S', N);
      --  The first declaration is dominated by the protected or task [type]
      --  declaration.

      Sync_Def : Ada_Node;
      --  N's protected or task definition

      Priv_Decl : List_Id;
      Vis_Decl  : List_Id;
      --  Sync_Def's Visible_Declarations and Private_Declarations

   begin
      case Nkind (N) is
         when N_Protected_Type_Declaration
            | N_Single_Protected_Declaration
         =>
            Sync_Def := Protected_Definition (N);

         when N_Single_Task_Declaration
            | N_Task_Type_Declaration
         =>
            Sync_Def := Task_Definition (N);

         when others =>
            raise Program_Error;
      end case;

      --  Sync_Def may be Empty at least for empty Task_Type_Declarations.
      --  Querying Visible or Private_Declarations is invalid in this case.

      if Present (Sync_Def) then
         Vis_Decl  := Visible_Declarations (Sync_Def);
         Priv_Decl := Private_Declarations (Sync_Def);
      else
         Vis_Decl  := No_List;
         Priv_Decl := No_List;
      end if;

      Dom_Info := Traverse_Declarations_Or_Statements
                    (L => Vis_Decl,
                     D => Dom_Info);

      --  If visible declarations are present, the first private declaration
      --  is dominated by the last visible declaration.

      Traverse_Declarations_Or_Statements
        (L => Priv_Decl,
         D => Dom_Info);
   end Traverse_Sync_Definition;

   --------------------------------------
   -- Traverse_Subprogram_Or_Task_Body --
   --------------------------------------

   procedure Traverse_Subprogram_Or_Task_Body
     (N : Ada_Node;
      D : Dominant_Info := No_Dominant)
   is
      Decls    : constant List_Id := Declarations (N);
      Dom_Info : Dominant_Info    := D;

   begin
      --  If declarations are present, the first statement is dominated by the
      --  last declaration.

      Dom_Info := Traverse_Declarations_Or_Statements
                    (L => Decls, D => Dom_Info);

      Traverse_Handled_Statement_Sequence
        (N => Handled_Statement_Sequence (N),
         D => Dom_Info);
   end Traverse_Subprogram_Or_Task_Body;

   -----------------------
   -- Process_Decisions --
   -----------------------

   procedure Process_Decisions
     (N           : Ada_Node;
      T           : Character;
      Pragma_Sloc : Source_Location)
   is
      Mark : Nat;
      --  This is used to mark the location of a decision sequence in the SCO
      --  table. We use it for backing out a simple decision in an expression
      --  context that contains only NOT operators.

      Mark_Hash : Nat;
      --  Likewise for the putative SCO_Raw_Hash_Table entries: see below

      type Hash_Entry is record
         Sloc      : Source_Ptr;
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

      X_Not_Decision : Boolean;
      --  This flag keeps track of whether a decision sequence in the SCO table
      --  contains only NOT operators, and is for an expression context (T=X).
      --  The flag will be set False if T is other than X, or if an operator
      --  other than NOT is in the sequence.

      procedure Output_Decision_Operand (N : Ada_Node);
      --  The node N is the top level logical operator of a decision, or it is
      --  one of the operands of a logical operator belonging to a single
      --  complex decision. This routine outputs the sequence of table entries
      --  corresponding to the node. Note that we do not process the sub-
      --  operands to look for further decisions, that processing is done in
      --  Process_Decision_Operand, because we can't get decisions mixed up in
      --  the global table. Call has no effect if N is Empty.

      procedure Output_Element (N : Ada_Node);
      --  Node N is an operand of a logical operator that is not itself a
      --  logical operator, or it is a simple decision. This routine outputs
      --  the table entry for the element, with C1 set to ' '. Last is set
      --  False, and an entry is made in the condition hash table.

      procedure Output_Header (T : Character);
      --  Outputs a decision header node. T is I/W/E/P for IF/WHILE/EXIT WHEN/
      --  PRAGMA, and 'X' for the expression case.

      procedure Process_Decision_Operand (N : Ada_Node);
      --  This is called on node N, the top level node of a decision, or on one
      --  of its operands or suboperands after generating the full output for
      --  the complex decision. It process the suboperands of the decision
      --  looking for nested decisions.

      function Process_Node (N : Ada_Node) return Traverse_Result;
      --  Processes one node in the traversal, looking for logical operators,
      --  and if one is found, outputs the appropriate table entries.

      -----------------------------
      -- Output_Decision_Operand --
      -----------------------------

      procedure Output_Decision_Operand (N : Ada_Node) is
         C1 : Character;
         C2 : Character;
         --  C1 holds a character that identifies the operation while C2
         --  indicates whether we are sure (' ') or not ('?') this operation
         --  belongs to the decision. '?' entries will be filtered out in the
         --  second (SCO_Record_Filtered) pass.

         L : Ada_Node;
         T : Tristate;

      begin
         if No (N) then
            return;
         end if;

         T := Is_Logical_Operator (N);

         --  Logical operator

         if T /= False then
            if Nkind (N) = N_Op_Not then
               C1 := '!';
               L := Empty;

            else
               L := Left_Opnd (N);

               if Nkind_In (N, N_Op_Or, N_Or_Else) then
                  C1 := '|';
               else pragma Assert (Nkind_In (N, N_Op_And, N_And_Then));
                  C1 := '&';
               end if;
            end if;

            if T = True then
               C2 := ' ';
            else
               C2 := '?';
            end if;

            Append_SCO
              (C1   => C1,
               C2   => C2,
               From => Sloc (N),
               To   => No_Location,
               Last => False);

            Hash_Entries.Append ((Sloc (N), SCO_Raw_Table.Last));

            Output_Decision_Operand (L);
            Output_Decision_Operand (Right_Opnd (N));

         --  Not a logical operator

         else
            Output_Element (N);
         end if;
      end Output_Decision_Operand;

      --------------------
      -- Output_Element --
      --------------------

      procedure Output_Element (N : Ada_Node) is
         FSloc : Source_Ptr;
         LSloc : Source_Ptr;
      begin
         Sloc_Range (N, FSloc, LSloc);
         Append_SCO
           (C1   => ' ',
            C2   => 'c',
            From => FSloc,
            To   => LSloc,
            Last => False);
         Hash_Entries.Append ((FSloc, SCO_Raw_Table.Last));
      end Output_Element;

      -------------------
      -- Output_Header --
      -------------------

      procedure Output_Header (T : Character) is
         Loc : Source_Ptr := No_Location;
         --  Node whose Sloc is used for the decision

         Nam : Name_Id := No_Name;
         --  For the case of an aspect, aspect name

      begin
         case T is
            when 'I' | 'E' | 'W' | 'a' | 'A' =>

               --  For IF, EXIT, WHILE, or aspects, the token SLOC is that of
               --  the parent of the expression.

               Loc := Sloc (Parent (N));

               if T = 'a' or else T = 'A' then
                  Nam := Chars (Identifier (Parent (N)));
               end if;

            when 'G' | 'P' =>

               --  For entry guard, the token sloc is from the N_Entry_Body.
               --  For PRAGMA, we must get the location from the pragma node.
               --  Argument N is the pragma argument, and we have to go up
               --  two levels (through the pragma argument association) to
               --  get to the pragma node itself. For the guard on a select
               --  alternative, we do not have access to the token location for
               --  the WHEN, so we use the first sloc of the condition itself
               --  (note: we use First_Sloc, not Sloc, because this is what is
               --  referenced by dominance markers).

               --  Doesn't this requirement of using First_Sloc need to be
               --  documented in the spec ???

               if Nkind_In (Parent (N), N_Accept_Alternative,
                                        N_Delay_Alternative,
                                        N_Terminate_Alternative)
               then
                  Loc := First_Sloc (N);
               else
                  Loc := Sloc (Parent (Parent (N)));
               end if;

            when 'X' =>

               --  For an expression, no Sloc

               null;

            --  No other possibilities

            when others =>
               raise Program_Error;
         end case;

         Append_SCO
           (C1                 => T,
            C2                 => ' ',
            From               => Loc,
            To                 => No_Location,
            Last               => False,
            Pragma_Aspect_Name => Nam);

         --  For an aspect specification, which will be rewritten into a
         --  pragma, enter a hash table entry now.

         if T = 'a' then
            Hash_Entries.Append ((Loc, SCO_Raw_Table.Last));
         end if;
      end Output_Header;

      ------------------------------
      -- Process_Decision_Operand --
      ------------------------------

      procedure Process_Decision_Operand (N : Ada_Node) is
      begin
         if Is_Logical_Operator (N) /= False then
            if Nkind (N) /= N_Op_Not then
               Process_Decision_Operand (Left_Opnd (N));
               X_Not_Decision := False;
            end if;

            Process_Decision_Operand (Right_Opnd (N));

         else
            Process_Decisions (N, 'X', Pragma_Sloc);
         end if;
      end Process_Decision_Operand;

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node (N : Ada_Node) return Traverse_Result is
      begin
         case Nkind (N) is

            --  Logical operators, output table entries and then process
            --  operands recursively to deal with nested conditions.

            when N_And_Then
               | N_Op_And
               | N_Op_Not
               | N_Op_Or
               | N_Or_Else
            =>
               declare
                  T : Character;

               begin
                  --  If outer level, then type comes from call, otherwise it
                  --  is more deeply nested and counts as X for expression.

                  if N = Process_Decisions.N then
                     T := Process_Decisions.T;
                  else
                     T := 'X';
                  end if;

                  --  Output header for sequence

                  X_Not_Decision := T = 'X' and then Nkind (N) = N_Op_Not;
                  Mark      := SCO_Raw_Table.Last;
                  Mark_Hash := Hash_Entries.Last;
                  Output_Header (T);

                  --  Output the decision

                  Output_Decision_Operand (N);

                  --  If the decision was in an expression context (T = 'X')
                  --  and contained only NOT operators, then we don't output
                  --  it, so delete it.

                  if X_Not_Decision then
                     SCO_Raw_Table.Set_Last (Mark);
                     Hash_Entries.Set_Last (Mark_Hash);

                  --  Otherwise, set Last in last table entry to mark end

                  else
                     SCO_Raw_Table.Table (SCO_Raw_Table.Last).Last := True;
                  end if;

                  --  Process any embedded decisions

                  Process_Decision_Operand (N);
                  return Skip;
               end;

            --  Case expression

            --  Really hard to believe this is correct given the special
            --  handling for if expressions below ???

            when N_Case_Expression =>
               return OK; -- ???

            --  If expression, processed like an if statement

            when N_If_Expression =>
               declare
                  Cond : constant Ada_Node := First (Expressions (N));
                  Thnx : constant Ada_Node := Next (Cond);
                  Elsx : constant Ada_Node := Next (Thnx);

               begin
                  Process_Decisions (Cond, 'I', Pragma_Sloc);
                  Process_Decisions (Thnx, 'X', Pragma_Sloc);
                  Process_Decisions (Elsx, 'X', Pragma_Sloc);
                  return Skip;
               end;

            --  All other cases, continue scan

            when others =>
               return OK;
         end case;
      end Process_Node;

      procedure Traverse is new Traverse_Proc (Process_Node);

   --  Start of processing for Process_Decisions

   begin
      if No (N) then
         return;
      end if;

      Hash_Entries.Init;

      --  See if we have simple decision at outer level and if so then
      --  generate the decision entry for this simple decision. A simple
      --  decision is a boolean expression (which is not a logical operator
      --  or short circuit form) appearing as the operand of an IF, WHILE,
      --  EXIT WHEN, or special PRAGMA construct.

      if T /= 'X' and then Is_Logical_Operator (N) = False then
         Output_Header (T);
         Output_Element (N);

         --  Change Last in last table entry to True to mark end of
         --  sequence, which is this case is only one element long.

         SCO_Raw_Table.Table (SCO_Raw_Table.Last).Last := True;
      end if;

      Traverse (N);

      --  Now we have the definitive set of SCO entries, register them in the
      --  corresponding hash table.

      for J in 1 .. Hash_Entries.Last loop
         SCO_Raw_Hash_Table.Set
           (Hash_Entries.Table (J).Sloc,
            Hash_Entries.Table (J).SCO_Index);
      end loop;

      Hash_Entries.Free;
   end Process_Decisions;

   ---------------------
   -- Instrument_Unit --
   ---------------------

   procedure Instrument_Unit (Unit_Name : String) is
      Ctx  : Analysis_Context := Create;
      Unit : Analysis_Unit := Get_From_File (Ctx, Unit_Name);
   begin
      Traverse_Declarations_Or_Statements (Root (Unit));
      Destroy (Ctx);
   end Instrument_Unit;

   ------------------
   -- Has_Decision --
   ------------------

   function Has_Decision (E : Expr) return Boolean is
      function Visit (N : Ada_Node) return Visit_Status;
      --  If N's kind indicates the presence of a decision, return Stop,
      --  otherwise return Into.
      --
      --  We know have a decision as soon as we have a logical operator (by
      --  definition) or an IF-expression (its condition is a decision).

      -----------
      -- Visit --
      -----------

      function Visit (N : Ada_Node) return Visit_Status is
      begin
         if Is_Logical_Operator (N) /= False
           or else Nkind (N) = N_If_Expression
         then
            return Stop;
         else
            return Into;
         end if;
      end Visit;

   --  Start of processing for Has_Decision

   begin
      return E.Traverse (Visit'Access) = Stop;
   end Has_Decision;

   -----------------
   -- Pragma_Name --
   -----------------

   function Pragma_Name (P : Pragma_Node) return Symbol_Type is
      Raw_Name   : constant Text_Type := P.F_Id.Text;
      Canon_Name : constant Canonicalization_Result := Canonicalize (Raw_Name);
   begin
      pragma Assert (Canon_Name.Success);
      return Find (Symbols, Canon_Name.Symbol);
   end Pragma_Name;

   -----------------------
   -- Aspect_Assoc_Name --
   -----------------------

   function Aspect_Assoc_Name (A : Aspect_Assoc) return Symbol_Type is
      Raw_Name   : constant Text_Type := A.F_Id.Text;
      Canon_Name : constant Canonicalization_Result := Canonicalize (Raw_Name);
   begin
      pragma Assert (Canon_Name.Success);
      return Find (Symbols, Canon_Name.Symbol);
   end Aspect_Assoc_Name;

end Instrument;
