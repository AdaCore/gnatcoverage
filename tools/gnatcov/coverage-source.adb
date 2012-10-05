------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

with Interfaces;

with Coverage.Tags;  use Coverage.Tags;
with Decision_Map;      use Decision_Map;
with Diagnostics;       use Diagnostics;
with Elf_Disassemblers; use Elf_Disassemblers;
with MC_DC;             use MC_DC;
with Slocs;             use Slocs;
with Strings;           use Strings;
with Switches;          use Switches;
with Traces_Elf;        use Traces_Elf;

package body Coverage.Source is

   use Ada.Containers;

   --  For each source coverage obligation, we maintain a corresponding source
   --  coverage information record, which denotes the coverage state of the
   --  SCO. Default initialization denotes a completely uncovered state.

   package Evaluation_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Evaluation);

   package Evaluation_Sets is new Ada.Containers.Ordered_Sets (Evaluation);

   type Outcome_Taken_Type is array (Boolean) of Boolean;

   type Line_States is array (Coverage_Level) of Line_State;

   type Source_Coverage_Info (Kind  : SCO_Kind := Statement) is record
      Tag : SC_Tag := No_SC_Tag;
      --  Tag identifying one among multiple coverage analyses being performed
      --  for a given SCO.

      State : Line_States := (others => No_Code);
      --  Line state for this SCO. The following invariant should hold:
      --  At the same coverage level, a merge of all SCO's states for a given
      --  line should be equal to this line's cumulative state.

      case Kind is
         when Statement =>
            Basic_Block_Has_Code : Boolean := False;
            --  Set True when code is present for this or any following SCO in
            --  basic block.

            Executed : Boolean := False;
            --  Set True when the statement has been executed

         when Decision =>
            Outcome_Taken : Outcome_Taken_Type := (others => False);
            --  Each of these components is set True when the corresponding
            --  outcome has been exercised.

            Evaluations : Evaluation_Sets.Set;
            --  Set of all distinct evaluations of this decision (computed for
            --  MC/DC only).

         when others =>
            null;
      end case;
   end record;

   package SCI_Vectors is new Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Source_Coverage_Info);

   package SCI_Vector_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_SCO_Id,
      Element_Type => SCI_Vectors.Vector,
      "="          => SCI_Vectors."=");

   SCI_Vector : SCI_Vector_Vectors.Vector;

   function Get_SCI (SCO : SCO_Id; Tag : SC_Tag) return Source_Coverage_Info;
   --  Return the SCI for the given SCO and tag

   procedure Update_SCI
     (SCO     : SCO_Id;
      Tag     : SC_Tag;
      Process : access procedure (SCI : in out Source_Coverage_Info));
   --  Execute Process on the SCI for the given SCO and tag

   --  MC/DC evaluation stack

   Evaluation_Stack : Evaluation_Vectors.Vector;

   procedure Condition_Evaluated
     (Exe     : Exe_File_Acc;
      PC      : Pc_Type;
      C_SCO   : SCO_Id;
      C_Value : Boolean);
   --  Record evaluation of condition C_SCO with the given C_Value in the
   --  current decision evaluation.

   function Compute_MCDC_State
     (SCO : SCO_Id;
      SCI : Source_Coverage_Info) return Line_State;
   --  Compute the MC/DC state of SCO, which is already covered for DC

   function Decision_Requires_Coverage (SCO : SCO_Id) return Boolean;
   --  Always True for all decisions that are part of a control structure;
   --  for other decisions, True if All_Decisions is set, or if the decision
   --  is complex and MC/DC is enabled.

   procedure Update_State
     (Prev_State : in out Line_State;
      SCO        : SCO_Id;
      Tag        : SC_Tag;
      Level      : Coverage_Level;
      State      : Line_State);
   --  Merge State into Prev_State and record State as the coverage state of
   --  SCO for Level.

   procedure Update_Line_State
     (Line  : Line_Info_Access;
      SCO   : SCO_Id;
      Tag   : SC_Tag;
      Level : Coverage_Level;
      State : Line_State);
   --  Merge State into Line's state for Level, and update SCO's state for
   --  the same level so that Source_Coverage_Info.State's invariant holds.

   --------------------------
   -- Basic_Block_Has_Code --
   --------------------------

   function Basic_Block_Has_Code (SCO : SCO_Id; Tag : SC_Tag) return Boolean is
   begin
      return Get_SCI (SCO, Tag).Basic_Block_Has_Code;
   end Basic_Block_Has_Code;

   ------------------------
   -- Compute_Line_State --
   ------------------------

   procedure Compute_Line_State
     (Line_Num  : Positive;
      Line_Info : Line_Info_Access)
   is
      Multiple_Statements_Reported : Boolean := False;
      --  Set True when a diagnosis has been emitted for multiple statements

   begin
      if Line_Info.SCOs.Length = 0 then
         --  No SCOs associated with this source line

         --  ??? Have a debug mode to warn if there is object code with
         --  this line ?

         return;
      end if;

      --  Examine each SCO associated with line

      for J in Line_Info.SCOs.First_Index .. Line_Info.SCOs.Last_Index loop
         SCOs_Of_Line : declare
            SCO         : constant SCO_Id := Line_Info.SCOs.Element (J);
            SCO_State   : Line_State := No_Code;

            procedure Ensure_SCI (SCIV : in out SCI_Vectors.Vector);
            --  If SCIV is empty, add a SCI with no tag

            ----------------
            -- Ensure_SCI --
            ----------------

            procedure Ensure_SCI (SCIV : in out SCI_Vectors.Vector) is
            begin
               if SCIV.Length = 0 then
                  declare
                     New_SCI : Source_Coverage_Info (Kind => Kind (SCO));
                     pragma Unmodified (New_SCI);
                     --  Used for default initialization
                  begin
                     SCIV.Append (New_SCI);
                  end;
               end if;
            end Ensure_SCI;

         --  Start of processing for SCOs_Of_Line

         begin
            --  Make sure we have at least one SCI for this SCO

            SCI_Vector.Update_Element (SCO, Ensure_SCI'Access);

            --  Iterate over all SCIs for this SCO

            for SCI of SCI_Vector.Element (SCO) loop
               if Kind (SCO) = Statement then
                  --  Statement coverage: line is covered if any associated
                  --  statement is executed.

                  if not Basic_Block_Has_Code (SCO, SCI.Tag) then

                     --  A SCO is marked as covered or not covered if there is
                     --  code for it, or for a subsequent SCO in the same basic
                     --  block, else we leave it as No_Code, so that a line
                     --  ends up marked as No_Code only if no code execution
                     --  can ever cause it to be marked as covered.

                     null;

                  elsif Is_Disabled_Statement (SCO) then

                     --  A disabled statement is never covered, nor not-covered
                     --  (it has no code and is not subject to coverage
                     --  analysis).

                     null;

                  elsif SCI.Executed then
                     if Is_Multistatement_Line (Line_Info.all) then
                        --  There is more than one statement SCO for this line.
                        --  Statements do not have full column numbers in debug
                        --  information, which prevents discriminating between
                        --  multiple statement SCOs on the same line. We
                        --  therefore conservatively mark this SCO (and hence
                        --  the complete line) as partially, rather than fully,
                        --  covered, and we report a coverage violation on the
                        --  first SCO on the line.

                        if not Multiple_Statements_Reported then
                           Multiple_Statements_Reported := True;
                           Report_Violation
                             (SCO,
                              SCI.Tag,
                              Msg => "^multiple statements on line, unable to "
                                     & "establish full statement coverage");
                        end if;
                        SCO_State := Partially_Covered;

                     else
                        SCO_State := Covered;

                     end if;

                  else
                     SCO_State := Not_Covered;

                     --  Generate violation message on first line of SCO

                     if Line_Num = First_Sloc (SCO).Line then
                        Report_Violation (SCO, SCI.Tag, "not executed");
                     end if;
                  end if;

                  Update_Line_State (Line_Info, SCO, SCI.Tag, Stmt, SCO_State);

               elsif Kind (SCO) = Decision
                 and then First_Sloc (SCO).Line /= Line_Num
               then

                  --  For a decision that spans multiple lines, SCO state is
                  --  computed for the first line, and then cached in the SCI
                  --  and reused for subsequent lines.

                  if Enabled (Decision) then
                     SCO_State := SCI.State (Decision);
                     Update_Line_State
                       (Line_Info, SCO, SCI.Tag, Decision, SCO_State);
                  end if;

                  if MCDC_Coverage_Enabled then
                     SCO_State := SCI.State (MCDC_Level);
                     Update_Line_State
                       (Line_Info, SCO, SCI.Tag, MCDC_Level, SCO_State);
                  end if;

               elsif Kind (SCO) = Decision
                 and then (Enabled (Decision) or else MCDC_Coverage_Enabled)
                 and then Decision_Requires_Coverage (SCO)
               then

                  --  Compute decision coverage state for this decision. Note
                  --  that the decision coverage information is also included
                  --  in MC/DC coverage.

                  if SCI.Outcome_Taken (False)
                    and then SCI.Outcome_Taken (True)
                  then
                     SCO_State := Covered;

                  elsif SCI.Outcome_Taken (False)
                    or else SCI.Outcome_Taken (True)
                  then
                     SCO_State := Partially_Covered;

                     --  Indicate which outcome has never been taken: if FALSE
                     --  has been taken then this is outcome TRUE, else FALSE.

                     if Degraded_Origins (SCO) then
                        Report_Violation
                          (SCO, SCI.Tag, "not exercised in both directions");

                     else
                        Report_Violation
                          (SCO,
                           SCI.Tag,
                           "outcome "
                           & SCI.Outcome_Taken (False)'Img
                           & " never exercised");
                     end if;

                  elsif Enclosing_Statement (SCO) = No_SCO_Id
                    or else Basic_Block_Has_Code
                              (Enclosing_Statement (SCO), SCI.Tag)
                  then
                     --  Similar to the above for statement coverage: a
                     --  decision that cannot ever be executed is reported as
                     --  No_Code, not Not_Covered. Note: the enclosing
                     --  statement may be covered even though the decision has
                     --  never been evaluated (case e.g. of an exception being
                     --  raised before any outcome is reached, or of a
                     --  condition for which we fail to identify the
                     --  corresponding conditional branch instruction). We
                     --  report the coverage failure for the decision in that
                     --  case only; if the statement was not executed, we
                     --  report only the statement failure. If there is no
                     --  enclosing statement then we always report the coverage
                     --  status.

                     if Enclosing_Statement (SCO) = No_SCO_Id
                          or else
                        Get_SCI (Enclosing_Statement (SCO), SCI.Tag).Executed
                     then
                        Report_Violation (SCO, SCI.Tag, "never evaluated");
                     end if;
                     SCO_State := Not_Covered;
                  end if;

                  Update_Line_State
                    (Line_Info, SCO, SCI.Tag, Decision, SCO_State);

                  if MCDC_Coverage_Enabled then
                     if SCO_State = Covered then

                        --  Complete computation of MC/DC coverage state if SCO
                        --  is covered for decision coverage.

                        Update_Line_State
                          (Line_Info,
                           SCO,
                           SCI.Tag,
                           MCDC_Level,
                           Compute_MCDC_State (SCO, SCI));

                     elsif SCO_State /= No_Code then

                        --  Case of MC/DC enabled but at least one outcome
                        --  never taken: do not report details regarding MC/DC
                        --  coverage, just record that MC/DC is not achieved.

                        Update_Line_State
                          (Line_Info, SCO, SCI.Tag, MCDC_Level, Not_Covered);
                     end if;
                  end if;
               end if;
            end loop;
         end SCOs_Of_Line;
      end loop;
   end Compute_Line_State;

   ------------------------
   -- Compute_MCDC_State --
   ------------------------

   function Compute_MCDC_State
     (SCO : SCO_Id;
      SCI : Source_Coverage_Info) return Line_State
   is
      use Evaluation_Sets;

      Indep : array (0 .. Last_Cond_Index (SCO)) of Boolean :=
                (others => False);
      --  Indicates whether independent influence of each condition has been
      --  shown.

      Influent_Condition : Any_Condition_Index;
      --  Condition whose independent influence is shown by the vector pair
      --  being considered.

      SCO_State : Line_State := No_Code;

      E1, E2 : Cursor;

   begin
      E1 := SCI.Evaluations.First;
      while E1 /= No_Element loop
         E2 := Next (E1);
         while E2 /= No_Element loop
            Influent_Condition := Is_MC_DC_Pair
              (Element (E1),
               Element (E2),
               Unique_Cause => MCDC_Level = UC_MCDC);

            --  Record and report the first eval pair that shows independent
            --  influence of Influent_Condition.

            if Influent_Condition /= No_Condition_Index
                 and then
               not Indep (Influent_Condition)
            then

               Indep (Influent_Condition) := True;
               Report
                 ("C" & Img (Integer (Influent_Condition))
                  & " independent influence shown by eval pair: "
                  & Image (Element (E1)) & " / " & Image (Element (E2)),
                  Sloc => First_Sloc (SCO),
                  SCO  => Condition (SCO, Influent_Condition),
                  Kind => Notice);
            end if;
            Next (E2);
         end loop;
         Next (E1);
      end loop;

      --  Iterate over conditions and report

      for J in 0 .. Indep'Last loop
         if not Indep (J) then
            Update_State
              (SCO_State,
               Condition (SCO, J), SCI.Tag,
               MCDC_Level, Not_Covered);
            Report_Violation
              (SCO => Condition (SCO, J),
               Tag => SCI.Tag,
               Msg => "has no independent influence pair, MC/DC not achieved");
         else
            Update_State
              (SCO_State,
               Condition (SCO, J), SCI.Tag,
               MCDC_Level, Covered);
         end if;
      end loop;

      --  If we have degraded origins for SCO but we computed MC/DC coverage
      --  state then this means that DC is achieved, and so MC/DC must be
      --  achieved as well (because this is a single condition decision).

      pragma Assert (SCO_State = Covered or else not Degraded_Origins (SCO));
      return SCO_State;
   end Compute_MCDC_State;

   -----------------------------
   -- Compute_Source_Coverage --
   -----------------------------

   procedure Compute_Source_Coverage
     (Subp_Name : String_Access;
      Subp_Info : Subprogram_Info;
      T         : Trace_Entry)
   is
      pragma Unreferenced (Subp_Name);

      use type Interfaces.Unsigned_32;

      Exe        : Exe_File_Acc renames Subp_Info.Exec;
      PC         : Pc_Type;
      Insn_Len   : Natural;
      SCO, S_SCO : SCO_Id;
      Tag        : SC_Tag;

      procedure Discharge_SCO (SCO : SCO_Id; Empty_Range : Boolean);
      --  Discharge the coverage obligation denoted by SCO using the current
      --  execution trace for an instruction at PC. Empty_Range is True if
      --  the sloc for PC that is associated with SCO has an empty PC range.

      -------------------
      -- Discharge_SCO --
      -------------------

      procedure Discharge_SCO (SCO : SCO_Id; Empty_Range : Boolean) is
         Propagating, No_Propagation : Boolean;
         Dom_SCO : SCO_Id;
         Dom_Val : Boolean;

         procedure Set_Executed (SCI : in out Source_Coverage_Info);
         --  Set Executed to True

         procedure Set_Outcome_Taken (SCI : in out Source_Coverage_Info);
         --  Set SCI.Outcome_Taken (Dom_Val) to True

         ------------------
         -- Set_Executed --
         ------------------

         procedure Set_Executed (SCI : in out Source_Coverage_Info) is
         begin
            SCI.Executed := True;
         end Set_Executed;

         -----------------------
         -- Set_Outcome_Taken --
         -----------------------

         procedure Set_Outcome_Taken (SCI : in out Source_Coverage_Info) is
         begin
            SCI.Outcome_Taken (Dom_Val) := True;
         end Set_Outcome_Taken;

      --  Start of processing for Discharge_SCO

      begin
         --  Find enclosing statement SCO (if any) and mark it as executed

         S_SCO := Enclosing_Statement (SCO);
         Propagating := False;
         while S_SCO /= No_SCO_Id loop
            exit when Get_SCI (S_SCO, Tag).Executed;

            --  For pragma Pre/Postcondition, no propagation: the statement
            --  is never marked as executed by propagation, and marking it
            --  does not cause propagation to other statements.

            No_Propagation := Is_Pragma_Pre_Post_Condition (S_SCO);

            if not (Propagating and No_Propagation) then

               --  Mark S_SCO as executed

               Update_SCI (S_SCO, Tag, Set_Executed'Access);
            end if;

            exit when not Propagating and No_Propagation;

            --  Propagate back to beginning of basic block

            Propagating := True;

            Dominant (S_SCO, Dom_SCO, Dom_Val);
            if Dom_SCO /= No_SCO_Id
                 and then Kind (Dom_SCO) = Decision
                 and then not Degraded_Origins (Dom_SCO)
            then
               Report
                 (Msg  => "propagating from " & Image (S_SCO) & " to "
                          & Image (Dom_SCO) & "=" & Dom_Val'Img,
                  Kind => Notice);
               Update_SCI (Dom_SCO, Tag, Set_Outcome_Taken'Access);
            end if;

            S_SCO := Enclosing_Statement (Dom_SCO);
         end loop;

         if not (Enabled (Decision) or else MCDC_Coverage_Enabled)
           or else Kind (SCO) /= Condition
           or else not Cond_Branch_Map.Contains ((Subp_Info.Exec, PC))
           or else Empty_Range
         then
            return;
         end if;

         --  Here we have a condition SCO and the PC for a conditional branch
         --  instruction.

         Process_Conditional_Branch : declare
            D_SCO : constant SCO_Id := Enclosing_Decision (SCO);
            --  Enclosing decision

            CBI : constant Cond_Branch_Info :=
                    Cond_Branch_Map.Element ((Subp_Info.Exec, PC));
            pragma Assert (CBI.Condition = SCO);

            procedure Edge_Taken (E : Edge_Kind);
            --  Record that edge E for the conditional branch at PC has been
            --  taken.

            ----------------
            -- Edge_Taken --
            ----------------

            procedure Edge_Taken (E : Edge_Kind) is
               CBE : constant Cond_Edge_Info := CBI.Edges (E);

               procedure Set_Outcome_Taken
                 (SCI : in out Source_Coverage_Info);
               --  Mark as taken the decision outcome corresponding to CBE

               -----------------------
               -- Set_Outcome_Taken --
               -----------------------

               procedure Set_Outcome_Taken
                 (SCI : in out Source_Coverage_Info)
               is
                  use Condition_Evaluation_Vectors;

                  Eval : Evaluation;

                  Inferred_Values : Vector;
                  --  Inferred condition values, for the case of a D_SCO with
                  --  no diamond.

                  function Pop_Eval return Evaluation;
                  --  Pop the top element from the evaluation stack

                  --------------
                  -- Pop_Eval --
                  --------------

                  function Pop_Eval return Evaluation is
                  begin
                     return ES_Top : constant Evaluation :=
                                       Evaluation_Stack.Last_Element
                     do
                        Evaluation_Stack.Delete_Last;
                     end return;
                  end Pop_Eval;

               --  Start of processing for Set_Outcome_Taken

               begin
                  --  If for some reason we failed to identify which value
                  --  of the outcome this edge represents, then we silently
                  --  ignore it, and do not mark any outcome of the decision
                  --  as known to have been taken.

                  if CBE.Outcome = Unknown then
                     return;
                  end if;

                  --  Mark outcome taken

                  SCI.Outcome_Taken (To_Boolean (CBE.Outcome)) := True;

                  Report (Exe, PC,
                    Msg  => "outcome " & CBE.Outcome'Img & " of "
                            & Image (D_SCO) & " reached by " & E'Img,
                    Kind => Notice);

                  --  Processing full evaluation history is costly, and
                  --  requires full traces of conditional branches, so we
                  --  do it only when actually required.

                  if not MCDC_Coverage_Enabled then
                     return;
                  end if;

                  if Has_Diamond (D_SCO) then
                     Eval := Pop_Eval;

                  else
                     --  Decision has no diamond: each condition is reachable
                     --  through only one path, and we can infer the complete
                     --  condition vector from just the last condition being
                     --  tested.

                     Inferred_Values := Infer_Values (SCO);
                     Inferred_Values.Append (CBE.Origin);

                     if Debug_Full_History then
                        --  In full history debugging mode, we record the
                        --  evaluation history and check it against the
                        --  inferred vector.

                        Eval := Pop_Eval;

                        if Eval.Values /= Inferred_Values then
                           Report_Violation
                             (D_SCO,
                              SCI.Tag,
                              "^inferred values mismatch: expected "
                              & Image (Inferred_Values)
                              & ", got " & Image (Eval.Values));
                        end if;

                     else
                        --  Reconstruct complete evaluation information from
                        --  just the outcome.

                        Eval.Decision       := D_SCO;
                        Eval.Values         := Inferred_Values;
                        Eval.Next_Condition := No_Condition_Index;
                     end if;
                  end if;

                  pragma Assert (Eval.Next_Condition = No_Condition_Index);

                  Eval.Outcome := CBE.Outcome;
                  SCI.Evaluations.Include (Eval);
               end Set_Outcome_Taken;

            --  Start of processing for Edge_Taken

            begin
               if CBE.Dest_Kind = Unknown then
                  Report
                    (Exe, PC,
                     "unlabeled edge " & E'Img & " taken",
                     Kind => Error);

               else
                  --  Record value of condition for this evaluation

                  if CBE.Origin = Unknown then
                     if CBE.Dest_Kind = Condition
                       and then SCO = Condition (D_SCO, CBE.Next_Condition)
                     then
                        --  This is an intermediate branch that remains within
                        --  the currently tested condition.

                        null;

                     elsif CBE.Dest_Kind = Raise_Exception then

                        --  Exception raised: abandon current evaluation

                        Report (Exe, PC,
                          "edge " & E'Img & " raised an exception, "
                          & "abandoning evaluation", Kind => Notice);

                        if Has_Diamond (D_SCO) or else Debug_Full_History then
                           Evaluation_Stack.Delete_Last;
                        end if;

                     else
                        Report
                          (Exe, PC,
                           "edge " & E'Img & " with unlabeled origin taken",
                           Kind => Error);
                     end if;

                  else
                     Condition_Evaluated
                       (Exe, PC, SCO, To_Boolean (CBE.Origin));
                  end if;

                  --  If the destination is an outcome, process completed
                  --  evaluation.

                  if CBE.Dest_Kind = Outcome then
                     Update_SCI
                       (D_SCO, Tag, Set_Outcome_Taken'Access);
                  end if;
               end if;
            end Edge_Taken;

            use type Interfaces.Unsigned_8;

         --  Start of processing for Process_Conditional_Branch

         begin
            Report
              (Exe, PC,
               "processing cond branch trace op" & T.Op'Img,
               Kind => Notice);

            case T.Op and 3 is
               when 1 =>
                  Edge_Taken (Branch);

               when 2 =>
                  Edge_Taken (Fallthrough);

               when 3 =>
                  if MCDC_Coverage_Enabled
                       and then (Has_Diamond (D_SCO)
                                   or else Debug_Full_History)
                  then
                     --  For MC/DC we need full historical traces, not just
                     --  accumulated traces.

                     Report
                       (Exe, PC,
                        "missing full traces of conditional branch for MC/DC");
                  else
                     Edge_Taken (Branch);
                     Edge_Taken (Fallthrough);
                  end if;

               when others =>
                  Report
                    (Exe, PC,
                     "unexpected cond branch trace state " & T.State'Img,
                     Kind => Warning);

            end case;
         end Process_Conditional_Branch;
      end Discharge_SCO;

   --  Start of processing for Compute_Source_Coverage

   begin
      --  Set current subprogram for per-routine tagging

      Traces_Names.Enter_Routine (Subp_Info);

      --  Iterate over trace for this routine

      PC := T.First + Subp_Info.Offset;

      Trace_Insns :
      while PC <= T.Last + Subp_Info.Offset loop
         Tag := Tag_Repository.Get_Tag (Exe, PC);

         Insn_Len :=
           Disa_For_Machine (Machine).
           Get_Insn_Length (Subp_Info.Insns (PC .. Subp_Info.Insns'Last));

         --  Discharge each SCO for source locations associated with this
         --  instruction.

         declare
            SL : constant Source_Locations :=
                   Get_Slocs (Subp_Info.Exec.all, PC);
         begin
            for J in SL'Range loop
               SCO := Sloc_To_SCO (SL (J));

               --  All but the last sloc in SL correspond to an empty PC range

               if SCO /= No_SCO_Id then
                  Discharge_SCO (SCO, Empty_Range => J < SL'Last);
               end if;
            end loop;
         end;

         PC := PC + Pc_Type (Insn_Len);

         --  Handle case where PC wraps

         exit Trace_Insns when PC = 0;
      end loop Trace_Insns;
   end Compute_Source_Coverage;

   -------------------------
   -- Condition_Evaluated --
   -------------------------

   procedure Condition_Evaluated
     (Exe     : Exe_File_Acc;
      PC      : Pc_Type;
      C_SCO   : SCO_Id;
      C_Value : Boolean)
   is
      D_SCO : constant SCO_Id := Enclosing_Decision (C_SCO);

      function In_Current_Evaluation return Boolean;
      --  True when this evaluation is the expected next condition in the
      --  evaluation at the top of the evaluation stack.

      procedure Update_Current_Evaluation (ES_Top : in out Evaluation);
      --  Record the value of condition C_SCO in the current evaluation, and
      --  set the next expected condition.

      ---------------------------
      -- In_Current_Evaluation --
      ---------------------------

      function In_Current_Evaluation return Boolean is
      begin
         if Evaluation_Stack.Length = 0 then
            return False;
         end if;

         declare
            ES_Top : Evaluation renames Evaluation_Stack.Last_Element;
         begin
            return ES_Top.Decision = D_SCO
              and then ES_Top.Next_Condition = Index (C_SCO);
         end;
      end In_Current_Evaluation;

      -------------------------------
      -- Update_Current_Evaluation --
      -------------------------------

      procedure Update_Current_Evaluation (ES_Top : in out Evaluation) is
         Next_C_SCO : SCO_Id;
      begin
         --  Add Unknown markers for masked conditions

         while ES_Top.Values.Last_Index < Index (C_SCO) - 1 loop
            ES_Top.Values.Append (Unknown);
         end loop;

         --  Record value for this condition

         ES_Top.Values.Append (To_Tristate (C_Value));

         --  Set index of next expected condition

         Next_C_SCO := Next_Condition (C_SCO, C_Value);
         if Next_C_SCO /= No_SCO_Id then
            ES_Top.Next_Condition := Index (Next_C_SCO);
         else
            ES_Top.Next_Condition := No_Condition_Index;
         end if;
      end Update_Current_Evaluation;

   --  Start of processing for Condition_Evaluated

   begin
      --  No-op unless doing MC/DC analysis

      if not MCDC_Coverage_Enabled then
         return;
      end if;

      --  No-op if decision has no diamond and not debugging

      if not (Has_Diamond (D_SCO) or else Debug_Full_History) then
         return;
      end if;

      if not In_Current_Evaluation then
         Evaluation_Stack.Append
           (Evaluation'(Decision       => D_SCO,
                        Next_Condition => 0,
                        Outcome        => Unknown,
                        others         => <>));
      end if;

      if not In_Current_Evaluation then
         Report
           (Exe, PC,
            "expected condition index"
            & Evaluation_Stack.Last_Element.Next_Condition'Img
            & ", got " & Index (C_SCO)'Img,
            Kind => Warning);
      end if;

      Evaluation_Stack.Update_Element
        (Evaluation_Stack.Last_Index, Update_Current_Evaluation'Access);
   end Condition_Evaluated;

   --------------------
   -- Get_Line_State --
   --------------------

   function Get_Line_State
     (SCO   : SCO_Id;
      Level : Coverage_Level) return SCO_State
   is
      Result : SCO_State := No_Code;
   begin
      --  Aggregate SCI state for each SCI of the SCO

      for SCI of SCI_Vector.Element (SCO) loop
         Result := Result * SCI.State (Level);
      end loop;
      return Result;
   end Get_Line_State;

   -------------
   -- Get_SCI --
   -------------

   function Get_SCI (SCO : SCO_Id; Tag : SC_Tag) return Source_Coverage_Info is
      Default_SCI : Source_Coverage_Info (Kind => Kind (SCO));
      pragma Warnings (Off, Default_SCI);
      --  Used for default initialization value

   begin
      if SCO in SCI_Vector.First_Index .. SCI_Vector.Last_Index then
         for SCI of SCI_Vector.Element (SCO) loop
            if SCI.Tag = Tag then
               return SCI;
            end if;
         end loop;
      end if;
      return Default_SCI;
   end Get_SCI;

   --------------------
   -- Initialize_SCI --
   --------------------

   procedure Initialize_SCI is
      procedure Add_SCI (SCO : SCO_Id);
      --  Add SCI for SCO

      -------------
      -- Add_SCI --
      -------------

      procedure Add_SCI (SCO : SCO_Id) is
      begin
         pragma Assert (SCO = SCI_Vector.Last_Index + 1);
         SCI_Vector.Append (SCI_Vectors.Empty_Vector);
      end Add_SCI;

      --  Start of processing for Initialize_SCI

   begin
      SC_Obligations.Iterate (Add_SCI'Access);
   end Initialize_SCI;

   --------------------------------
   -- Decision_Requires_Coverage --
   --------------------------------

   function Decision_Requires_Coverage (SCO : SCO_Id) return Boolean is
   begin
      pragma Assert (Kind (SCO) = Decision);

      return Switches.All_Decisions
        or else not Is_Expression (SCO)
        or else (MCDC_Coverage_Enabled and then Last_Cond_Index (SCO) > 0);
   end Decision_Requires_Coverage;

   ------------------------------
   -- Set_Basic_Block_Has_Code --
   ------------------------------

   procedure Set_Basic_Block_Has_Code (SCO : SCO_Id; Tag : SC_Tag) is

      S_SCO : SCO_Id := SCO;

      procedure Set_SCI_BB_Has_Code (SCI : in out Source_Coverage_Info);
      --  Set SCI.Basic_Block_Has_Code

      -------------------------
      -- Set_SCI_BB_Has_Code --
      -------------------------

      procedure Set_SCI_BB_Has_Code (SCI : in out Source_Coverage_Info) is
      begin
         if Kind (S_SCO) = Statement
              and then Is_Pragma_Annotate_Xcov (S_SCO)
         then
            --  This is a statement SCO for a pragma Annotate (Xcov): do not
            --  set Basic_Block_Has_Code, in order to avoid generating a bogus
            --  violation for the pragma SCO.

            null;

         else
            SCI.Basic_Block_Has_Code := True;
         end if;
      end Set_SCI_BB_Has_Code;

      Propagating, No_Propagation : Boolean;

   --  Start of processing for Set_Basic_Block_Has_Code

   begin
      Propagating := False;
      loop
         --  Pragma Pre/Post-condition SCOs are not taken into account while
         --  setting Basic_Block_Has_Code, because they are out of the normal
         --  sequence of the enclosing list of declarations (the corresponding
         --  code is instead generated in the subprogram to which they apply).
         --  See also similar processing in the back propagation circuitry in
         --  Coverage.Source.Compute_Source_Coverage.

         No_Propagation := Is_Pragma_Pre_Post_Condition (S_SCO);

         if not (Propagating and No_Propagation) then
            Update_SCI (S_SCO, Tag, Set_SCI_BB_Has_Code'Access);
         end if;

         exit when not Propagating and No_Propagation;

         Propagating := True;
         S_SCO := Previous (S_SCO);
         exit when S_SCO = No_SCO_Id
                     or else Get_SCI (S_SCO, Tag).Basic_Block_Has_Code;
      end loop;
   end Set_Basic_Block_Has_Code;

   -----------------------
   -- Update_Line_State --
   -----------------------

   procedure Update_Line_State
     (Line  : Line_Info_Access;
      SCO   : SCO_Id;
      Tag   : SC_Tag;
      Level : Coverage_Level;
      State : Line_State)
   is
   begin
      Update_State (Line.State (Level), SCO, Tag, Level, State);
   end Update_Line_State;

   ----------------
   -- Update_SCI --
   ----------------

   procedure Update_SCI
     (SCO     : SCO_Id;
      Tag     : SC_Tag;
      Process : access procedure (SCI : in out Source_Coverage_Info))
   is
      procedure Update_SCIV (SCIV : in out SCI_Vectors.Vector);
      --  Call Process on the relevant element of SCIV

      -----------------
      -- Update_SCIV --
      -----------------

      procedure Update_SCIV (SCIV : in out SCI_Vectors.Vector) is
      begin
         for Cur in SCIV.Iterate loop
            if SCI_Vectors.Element (Cur).Tag = Tag then
               SCIV.Update_Element (Cur, Process);
               return;
            end if;
         end loop;

         --  Here if no SCI exists yet for this SCO and tag

         declare
            New_SCI : Source_Coverage_Info (Kind (SCO));
         begin
            New_SCI.Tag := Tag;
            Process (New_SCI);
            SCIV.Append (New_SCI);
         end;
      end Update_SCIV;

   --  Start of processing for Update_SCI

   begin
      SCI_Vector.Update_Element (SCO, Update_SCIV'Access);
   end Update_SCI;

   ------------------
   -- Update_State --
   ------------------

   procedure Update_State
     (Prev_State : in out Line_State;
      SCO        : SCO_Id;
      Tag        : SC_Tag;
      Level      : Coverage_Level;
      State      : Line_State)
   is
      procedure Update_SCO_Line_State (SCI : in out Source_Coverage_Info);
      --  Set SCI's coverage state for Level to State

      ---------------------------
      -- Update_SCO_Line_State --
      ---------------------------

      procedure Update_SCO_Line_State (SCI : in out Source_Coverage_Info) is
      begin
         SCI.State (Level) := State;
      end Update_SCO_Line_State;

   --  Start of processing for Update_State

   begin
      Update_SCI (SCO, Tag, Update_SCO_Line_State'Access);
      Prev_State := Prev_State * State;
   end Update_State;

end Coverage.Source;
