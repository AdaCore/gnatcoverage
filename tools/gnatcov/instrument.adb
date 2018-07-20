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

with Libadalang.Analysis; use Libadalang.Analysis;
--  with Project;
--  with SCOs;
with Types; use Types;
with Table;

package body Instrument is

   subtype List_Id is Ada_Node_List;
   subtype Node_Id is Ada_Node;
   Empty   : constant Node_Id := No_Ada_Node;
   No_List : constant List_Id := No_Ada_Node_List;

   type Dominant_Info is record
      K : Character;
      --  F/T/S/E for a valid dominance marker, or ' ' for no dominant

      N : Node_Id;
      --  Node providing the Sloc(s) for the dominance marker
   end record;
   No_Dominant : constant Dominant_Info := (' ', Empty);

   procedure Traverse_Declarations_Or_Statements
     (L : List_Id;
      D : Dominant_Info := No_Dominant;
      P : Node_Id       := Empty);
   --  Process L, a list of statements or declarations dominated by D. If P is
   --  present, it is processed as though it had been prepended to L.

   function Traverse_Declarations_Or_Statements
     (L : List_Id;
      D : Dominant_Info := No_Dominant;
      P : Node_Id       := Empty) return Dominant_Info;
   --  Same as above, and returns dominant information corresponding to the
   --  last node with SCO in L.

   -----------------------------------------
   -- Traverse_Declarations_Or_Statements --
   -----------------------------------------

   --  Tables used by Traverse_Declarations_Or_Statements for temporarily
   --  holding statement and decision entries. These are declared globally
   --  since they are shared by recursive calls to this procedure.

   type SC_Entry is record
      N    : Node_Id;
      From : Source_Ptr;
      To   : Source_Ptr;
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
      Nod : Node_Id;
      Lst : List_Id;
      Typ : Character;
      Plo : Source_Ptr;
   end record;
   --  Used to store a single entry in the following table. Nod is the node to
   --  be searched for decisions for the case of Process_Decisions_Defer with a
   --  node argument (with Lst set to No_List. Lst is the list to be searched
   --  for decisions for the case of Process_Decisions_Defer with a List
   --  argument (in which case Nod is set to Empty). Plo is the sloc of the
   --  enclosing pragma, if any.

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
     (L : List_Id;
      D : Dominant_Info := No_Dominant;
      P : Node_Id       := Empty)
   is
      Discard_Dom : Dominant_Info;
      pragma Warnings (Off, Discard_Dom);
   begin
      Discard_Dom := Traverse_Declarations_Or_Statements (L, D, P);
   end Traverse_Declarations_Or_Statements;

   function Traverse_Declarations_Or_Statements
     (L : List_Id;
      D : Dominant_Info := No_Dominant;
      P : Node_Id       := Empty) return Dominant_Info
   is
      Current_Dominant : Dominant_Info := D;
      --  Dominance information for the current basic block

      Current_Test : Node_Id;
      --  Conditional node (N_If_Statement or N_Elsiif being processed

      N : Node_Id;

      SC_First : constant Nat := SC.Last + 1;
      SD_First : constant Nat := SD.Last + 1;
      --  Record first entries used in SC/SD at this recursive level

      procedure Extend_Statement_Sequence (N : Node_Id; Typ : Character);
      --  Extend the current statement sequence to encompass the node N. Typ is
      --  the letter that identifies the type of statement/declaration that is
      --  being added to the sequence.

      procedure Process_Decisions_Defer (N : Node_Id; T : Character);
      pragma Inline (Process_Decisions_Defer);
      --  This routine is logically the same as Process_Decisions, except that
      --  the arguments are saved in the SD table for later processing when
      --  Set_Statement_Entry is called, which goes through the saved entries
      --  making the corresponding calls to Process_Decision. Note: the
      --  enclosing statement must have already been added to the current
      --  statement sequence, so that nested decisions are properly
      --  identified as such.

      procedure Process_Decisions_Defer (L : List_Id; T : Character);
      pragma Inline (Process_Decisions_Defer);
      --  Same case for list arguments, deferred call to Process_Decisions

      procedure Set_Statement_Entry;
      --  Output CS entries for all statements saved in table SC, and end the
      --  current CS sequence. Then output entries for all decisions nested in
      --  these statements, which have been deferred so far.

      procedure Traverse_One (N : Node_Id);
      --  Traverse one declaration or statement

      procedure Traverse_Aspects (N : Node_Id);
      --  Helper for Traverse_One: traverse N's aspect specifications

      procedure Traverse_Degenerate_Subprogram (N : Node_Id);
      --  Common code to handle null procedures and expression functions. Emit
      --  a SCO of the given Kind and N outside of the dominance flow.

      -------------------------------
      -- Extend_Statement_Sequence --
      -------------------------------

      procedure Extend_Statement_Sequence (N : Node_Id; Typ : Character) is
         Dummy   : Source_Ptr;
         F       : Source_Ptr;
         T       : Source_Ptr;
         To_Node : Node_Id := Empty;

      begin
         Sloc_Range (N, F, T);

         case Kind (N) is
            when Ada_Accept_Stmt =>
               if Present (F_Params (N)) then
                  To_Node := Last (F_Params (N));

               elsif Present (Entry_Index (N)) then
                  To_Node := Entry_Index (N);

               else
                  To_Node := Entry_Direct_Name (N);
               end if;

            when Ada_Case_Statement =>
               To_Node := Expression (N);

            when Ada_Elsif_Part
               | Ada_If_Statement
            =>
               To_Node := Condition (N);

            when Ada_Extended_Return_Statement =>
               To_Node := Last (Return_Object_Declarations (N));

            when Ada_Loop_Statement =>
               To_Node := Iteration_Scheme (N);

            when Ada_Asynchronous_Select
               | Ada_Conditional_Entry_Call
               | Ada_Selective_Accept
               | Ada_Single_Protected_Declaration
               | Ada_Single_Task_Declaration
               | Ada_Timed_Entry_Call
            =>
               T := F;

            when Ada_Protected_Type_Declaration
               | Ada_Task_Type_Declaration
            =>
               if Has_Aspects (N) then
                  To_Node := Last (Aspect_Specifications (N));

               elsif Present (Discriminant_Specifications (N)) then
                  To_Node := Last (Discriminant_Specifications (N));

               else
                  To_Node := Defining_Identifier (N);
               end if;

            when Ada_Subexpr =>
               To_Node := N;

            when others =>
               null;
         end case;

         if Present (To_Node) then
            Sloc_Range (To_Node, Dummy, T);
         end if;

         SC.Append ((N, F, T, Typ));
      end Extend_Statement_Sequence;

      -----------------------------
      -- Process_Decisions_Defer --
      -----------------------------

      procedure Process_Decisions_Defer (N : Node_Id; T : Character) is
      begin
         SD.Append ((N, No_List, T, Current_Pragma_Sloc));
      end Process_Decisions_Defer;

      procedure Process_Decisions_Defer (L : List_Id; T : Character) is
      begin
         SD.Append ((Empty, L, T, Current_Pragma_Sloc));
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
            if J = SC_First then

               if Current_Dominant /= No_Dominant then
                  declare
                     From : Source_Ptr;
                     To   : Source_Ptr;

                  begin
                     Sloc_Range (Current_Dominant.N, From, To);

                     if Current_Dominant.K /= 'E' then
                        To := No_Location;
                     end if;

                     Set_Raw_Table_Entry
                       (C1                 => '>',
                        C2                 => Current_Dominant.K,
                        From               => From,
                        To                 => To,
                        Last               => False,
                        Pragma_Sloc        => No_Location,
                        Pragma_Aspect_Name => No_Name);
                  end;
               end if;
            end if;

            declare
               SCE                : SC_Entry renames SC.Table (J);
               Pragma_Sloc        : Source_Ptr := No_Location;
               Pragma_Aspect_Name : Name_Id    := No_Name;

            begin
               --  For the case of a statement SCO for a pragma controlled by
               --  Set_SCO_Pragma_Enabled, set Pragma_Sloc so that the SCO (and
               --  those of any nested decision) is emitted only if the pragma
               --  is enabled.

               if SCE.Typ = 'p' then
                  Pragma_Sloc := SCE.From;
                  SCO_Raw_Hash_Table.Set
                    (Pragma_Sloc, SCO_Raw_Table.Last + 1);
                  Pragma_Aspect_Name := Pragma_Name_Unmapped (SCE.N);
                  pragma Assert (Pragma_Aspect_Name /= No_Name);

               elsif SCE.Typ = 'P' then
                  Pragma_Aspect_Name := Pragma_Name_Unmapped (SCE.N);
                  pragma Assert (Pragma_Aspect_Name /= No_Name);
               end if;

               Set_Raw_Table_Entry
                 (C1                 => 'S',
                  C2                 => SCE.Typ,
                  From               => SCE.From,
                  To                 => SCE.To,
                  Last               => (J = SC_Last),
                  Pragma_Sloc        => Pragma_Sloc,
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
               if Present (SDE.Nod) then
                  Process_Decisions (SDE.Nod, SDE.Typ, SDE.Plo);
               else
                  Process_Decisions (SDE.Lst, SDE.Typ, SDE.Plo);
               end if;
            end;
         end loop;

         --  Clear out used section of SD table

         SD.Set_Last (SD_First - 1);
      end Set_Statement_Entry;

      ----------------------
      -- Traverse_Aspects --
      ----------------------

      procedure Traverse_Aspects (N : Node_Id) is
         AE : Node_Id;
         AN : Node_Id;
         C1 : Character;

      begin
         AN := First (Aspect_Specifications (N));
         while Present (AN) loop
            AE := Expression (AN);

            --  SCOs are generated before semantic analysis/expansion:
            --  PPCs are not split yet.

            pragma Assert (not Split_PPC (AN));

            C1 := ASCII.NUL;

            case Get_Aspect_Id (AN) is

               --  Aspects rewritten into pragmas controlled by a Check_Policy:
               --  Current_Pragma_Sloc must be set to the sloc of the aspect
               --  specification. The corresponding pragma will have the same
               --  sloc. Note that Invariant, Pre, and Post will be enabled if
               --  the policy is Check; on the other hand, predicate aspects
               --  will be enabled for Check and Ignore (when Add_Predicate
               --  is called) because the actual checks occur in client units.
               --  When the assertion policy for Predicate is Disable, the
               --  SCO remains disabled, because Add_Predicate is never called.

               --  Pre/post can have checks in client units too because of
               --  inheritance, so should they receive the same treatment???

               when Aspect_Dynamic_Predicate
                  | Aspect_Invariant
                  | Aspect_Post
                  | Aspect_Postcondition
                  | Aspect_Pre
                  | Aspect_Precondition
                  | Aspect_Predicate
                  | Aspect_Static_Predicate
                  | Aspect_Type_Invariant
               =>
                  C1 := 'a';

               --  Other aspects: just process any decision nested in the
               --  aspect expression.

               when others =>
                  if Has_Decision (AE) then
                     C1 := 'X';
                  end if;
            end case;

            if C1 /= ASCII.NUL then
               pragma Assert (Current_Pragma_Sloc = No_Location);

               if C1 = 'a' or else C1 = 'A' then
                  Current_Pragma_Sloc := Sloc (AN);
               end if;

               Process_Decisions_Defer (AE, C1);

               Current_Pragma_Sloc := No_Location;
            end if;

            Next (AN);
         end loop;
      end Traverse_Aspects;

      ------------------------------------
      -- Traverse_Degenerate_Subprogram --
      ------------------------------------

      procedure Traverse_Degenerate_Subprogram (N : Node_Id) is
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

            if Nkind (N) in Ada_Subexpr then
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

      procedure Traverse_One (N : Node_Id) is
      begin
         --  Initialize or extend current statement sequence. Note that for
         --  special cases such as IF and Case statements we will modify
         --  the range to exclude internal statements that should not be
         --  counted as part of the current statement sequence.

         case Nkind (N) is

            --  Package declaration

            when Ada_Package_Declaration =>
               Set_Statement_Entry;
               Traverse_Package_Declaration (N, Current_Dominant);

            --  Generic package declaration

            when Ada_Generic_Package_Declaration =>
               Set_Statement_Entry;
               Traverse_Generic_Package_Declaration (N);

            --  Package body

            when Ada_Package_Body =>
               Set_Statement_Entry;
               Traverse_Package_Body (N);

            --  Subprogram declaration or subprogram body stub

            when Ada_Expression_Function
               | Ada_Subprogram_Body_Stub
               | Ada_Subprogram_Declaration
            =>
               declare
                  Spec : constant Node_Id := Specification (N);
               begin
                  Process_Decisions_Defer
                    (Parameter_Specifications (Spec), 'X');

                  --  Case of a null procedure: generate SCO for fictitious
                  --  NULL statement located at the NULL keyword in the
                  --  procedure specification.

                  if Nkind (N) = Ada_Subprogram_Declaration
                    and then Nkind (Spec) = Ada_Procedure_Specification
                    and then Null_Present (Spec)
                  then
                     Traverse_Degenerate_Subprogram (Null_Statement (Spec));

                  --  Case of an expression function: generate a statement SCO
                  --  for the expression (and then decision SCOs for any nested
                  --  decisions).

                  elsif Nkind (N) = Ada_Expression_Function then
                     Traverse_Degenerate_Subprogram (Expression (N));
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
                  Cond : constant Node_Id :=
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

                     Elif : Node_Id := First (Elsif_Parts (N));

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
                  Alt : Node_Id;
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
                  Alt   : Node_Id;
                  Guard : Node_Id;
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
                  ISC            : constant Node_Id := Iteration_Scheme (N);
                  Inner_Dominant : Dominant_Info    := No_Dominant;

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
                  Arg : Node_Id          :=
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

   ---------------------
   -- Instrument_Unit --
   ---------------------

   procedure Instrument_Unit (Unit_Name : String) is
      Ctx : Analysis_Context := Create;
      Unit : Analysis_Unit := Get_From_File (Ctx, Unit_Name);
   begin
      Traverse (Unit);
   end Instrument_Unit;

end Instrument;
