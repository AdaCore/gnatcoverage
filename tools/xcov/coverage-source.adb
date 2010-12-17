------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2009-2010, AdaCore                      --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;

with Interfaces;

with Decision_Map;      use Decision_Map;
with Diagnostics;       use Diagnostics;
with Elf_Disassemblers; use Elf_Disassemblers;
with MC_DC;             use MC_DC;
with Strings;           use Strings;
with Switches;          use Switches;

package body Coverage.Source is

   use Ada.Containers;

   --  For each source coverage obligation, we maintain a corresponding source
   --  coverage information record, which denotes the coverage state of the
   --  SCO. Default initialization denotes a completely uncovered state.

   package Evaluation_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => MC_DC.Evaluation);

   type Outcome_Taken_Type is array (Boolean) of Boolean;

   type Line_States is array (Coverage_Level) of Line_State;

   type Source_Coverage_Info (Kind  : SCO_Kind := Statement) is record
      State : Line_States := (others => No_Code);
      --  Line state for this SCO. The following invariant should hold:
      --  At the same coverage level, a merge of all SCO's states for a given
      --  line should be equal to this line's cumulative state.

      case Kind is
         when Statement =>
            Executed : Boolean := False;
            --  Set True when the statement has been executed

         when Decision =>
            Outcome_Taken : Outcome_Taken_Type := (others => False);
            --  Each of these components is set True when the corresponding
            --  outcome has been exercised.

            Evaluations : Evaluation_Vectors.Vector;
            --  History of all evaluations of this decision (computed for MC/DC
            --  only).

         when others =>
            null;
      end case;
   end record;

   procedure Set_Executed (SCI : in out Source_Coverage_Info);
   --  Set Executed to True

   package SCI_Vectors is new Ada.Containers.Vectors
       (Index_Type   => Valid_SCO_Id,
        Element_Type => Source_Coverage_Info);
   SCI_Vector : SCI_Vectors.Vector;

   --  MC/DC evaluation stack

   Evaluation_Stack : Evaluation_Vectors.Vector;

   procedure Condition_Evaluated
     (Exe     : Exe_File_Acc;
      PC      : Pc_Type;
      C_SCO   : SCO_Id;
      C_Value : Boolean);
   --  Record evaluation of condition C_SCO with the given C_Value in the
   --  current decision evaluation.

   function Compute_MCDC_State (SCO : SCO_Id) return Line_State;
   --  Compute the MC/DC state of SCO, which is already covered for DC

   procedure Update_State
     (Prev_State : in out Line_State;
      SCO        : SCO_Id;
      Level      : Coverage_Level;
      State      : Line_State);
   --  Merge State into Prev_State and record State as the coverage state of
   --  SCO for Level.

   procedure Update_Line_State
     (Line  : Line_Info_Access;
      SCO   : SCO_Id;
      Level : Coverage_Level;
      State : Line_State);
   --  Merge State into Line's state for Level, and update SCO's state for
   --  the same level so that Source_Coverage_Info.State's invariant holds.

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
         declare
            SCO         : constant SCO_Id := Line_Info.SCOs.Element (J);
            SCI         : Source_Coverage_Info;
            Default_SCI : Source_Coverage_Info (Kind => Kind (SCO));
            pragma Warnings (Off, Default_SCI);
            --  Used for default initialization value

            SCO_State   : Line_State := No_Code;

         begin
            if SCO in SCI_Vector.First_Index .. SCI_Vector.Last_Index then
               SCI := SCI_Vector.Element (SCO);
            else
               SCI := Default_SCI;
            end if;

            if Kind (SCO) = Statement then
               --  Statement coverage: line is covered if any associated
               --  statement is executed.

               if not Basic_Block_Has_Code (SCO) then

                  --  A SCO is marked as covered or not covered if there is
                  --  code for it, or for a subsequent SCO in the same basic
                  --  block, else we leave it as No_Code, so that a line ends
                  --  up marked as No_Code only if no code execution can ever
                  --  cause it to be marked as covered.

                  null;

               elsif SCI.Executed then
                  if Line_Info.State (Stmt) = No_Code then
                     --  This is the first statement SCO for this line

                     SCO_State := Covered;

                  else
                     --  A previous statement SCO has been seen for this line.
                     --  Statements do not have full column numbers in debug
                     --  information, which prevents discriminating between
                     --  multiple statement SCOs on the same line. We therefore
                     --  conservatively mark this SCO (and hence the complete
                     --  line) as partially, rather than fully, covered.

                     if not Multiple_Statements_Reported then
                        Multiple_Statements_Reported := True;
                        Report
                          (First_Sloc (SCO),
                           "multiple statement SCOs on line, unable to "
                           & "establish full statement coverage",
                           Kind => Warning);
                     end if;
                     SCO_State := Partially_Covered;
                  end if;

               else
                  SCO_State := Not_Covered;

                  --  Generate violation message on first line of SCO

                  if Line_Num = First_Sloc (SCO).Line then
                     Report (SCO, "not executed");
                  end if;
               end if;

               Update_Line_State (Line_Info, SCO, Stmt, SCO_State);

            elsif Kind (SCO) = Decision
              and then First_Sloc (SCO).Line /= Line_Num
            then

               --  For a decision that spans multiple lines, line state is
               --  computed for the first line, and then cached in the SCI and
               --  reused for subsequent lines.

               if Enabled (Decision) then
                  SCO_State := SCI.State (Decision);
                  Update_Line_State (Line_Info, SCO, Decision, SCO_State);
               end if;

               if MCDC_Coverage_Enabled then
                  SCO_State := SCI.State (MCDC_Level);
                  Update_Line_State (Line_Info, SCO, MCDC_Level, SCO_State);
               end if;

            elsif Kind (SCO) = Decision
              and then (Enabled (Decision) or else MCDC_Coverage_Enabled)
            then

               --  Compute decision coverage state for this decision. Note that
               --  the decision coverage information is also included in MC/DC
               --  coverage.

               if SCI.Outcome_Taken (False)
                 and then SCI.Outcome_Taken (True)
               then
                  SCO_State := Covered;

               elsif SCI.Outcome_Taken (False)
                 or else SCI.Outcome_Taken (True)
               then
                  SCO_State := Partially_Covered;

                  --  Indicate which outcome has never been taken: if FALSE has
                  --  been taken then this is outcome TRUE, else FALSE.

                  if Degraded_Origins (SCO) then
                     Report (SCO, "not exercised in both directions");
                  else
                     Report
                       (SCO,
                        "outcome "
                        & SCI.Outcome_Taken (False)'Img
                        & " never exercised");
                  end if;

               elsif Basic_Block_Has_Code (Enclosing_Statement (SCO)) then

                  --  Similar to the above for statement coverage: a decision
                  --  that cannot ever be executed is reported as No_Code, not
                  --  Not_Covered. Note: the enclosing statement may be covered
                  --  even though the decision has never been evaluated (case
                  --  e.g. of an exception being raised before any outcome is
                  --  reached, or of a condition for which we fail to identify
                  --  the corresponding conditional branch instruction).

                  Report (SCO, "never evaluated");
                  SCO_State := Not_Covered;
               end if;

               Update_Line_State (Line_Info, SCO, Decision, SCO_State);

               if MCDC_Coverage_Enabled then
                  if SCO_State = Covered then
                     --  Complete computation of MC/DC coverage state if SCO is
                     --  covered for decision coverage.

                     Update_Line_State
                       (Line_Info, SCO, MCDC_Level, Compute_MCDC_State (SCO));

                  elsif SCO_State /= No_Code then
                     --  Case of MC/DC enabled but at least one outcome never
                     --  taken: do not report details regarding MC/DC coverage,
                     --  just record that MC/DC is not achieved.

                     Update_Line_State
                       (Line_Info, SCO, MCDC_Level, Not_Covered);
                  end if;
               end if;
            end if;
         end;
      end loop;
   end Compute_Line_State;

   ------------------------
   -- Compute_MCDC_State --
   ------------------------

   function Compute_MCDC_State (SCO : SCO_Id) return Line_State is
      SCI : Source_Coverage_Info renames SCI_Vector.Element (SCO);

      Indep : array (0 .. Last_Cond_Index (SCO)) of Boolean :=
                (others => False);
      --  Indicates whether independent influence of each condition has been
      --  shown.

      Influent_Condition : Any_Condition_Index;
      --  Condition whose independent influence is shown by the vector pair
      --  being considered.

      SCO_State : Line_State := No_Code;

   begin
      for E1 in SCI.Evaluations.First_Index
             .. SCI.Evaluations.Last_Index - 1
      loop
         for E2 in E1 + 1 .. SCI.Evaluations.Last_Index loop
            Influent_Condition := Is_MC_DC_Pair
                 (SCI.Evaluations.Element (E1),
                  SCI.Evaluations.Element (E2),
                  Unique_Cause => MCDC_Level = UC_MCDC);

            --  Record and report the first eval pair that shows independent
            --  influence of Influent_Condition.

            if Influent_Condition /= No_Condition_Index
                 and then
               not Indep (Influent_Condition)
            then

               Indep (Influent_Condition) := True;
               Report
                 ("c" & Img (Integer (Influent_Condition))
                  & " independent influence shown by eval pair: "
                  & Image (SCI.Evaluations.Element (E1))
                  & " / "
                  & Image (SCI.Evaluations.Element (E2)),
                  Sloc => First_Sloc (SCO),
                  SCO  => Condition (SCO, Influent_Condition),
                  Kind => Notice);
            end if;
         end loop;
      end loop;

      --  Iterate over conditions and report

      for J in 0 .. Indep'Last loop
         if not Indep (J) then
            Update_State
              (SCO_State, Condition (SCO, J), MCDC_Level, Not_Covered);
            Report
              ("has no independent influence pair, MC/DC not achieved",
               Sloc => First_Sloc (SCO),
               SCO  => Condition (SCO, J),
               Kind => Warning);
         else
            Update_State (SCO_State, Condition (SCO, J), MCDC_Level, Covered);
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

   begin
      --  Iterate over trace for this routine

      PC := T.First + Subp_Info.Offset;

      Trace_Insns :
      while PC <= T.Last + Subp_Info.Offset loop
         Insn_Len :=
           Disa_For_Machine (Machine).
           Get_Insn_Length (Subp_Info.Insns (PC .. Subp_Info.Insns'Last));

         --  Find SCO for this instruction

         SCO := Sloc_To_SCO (Get_Sloc (Subp_Info.Exec.all, PC));
         if SCO = No_SCO_Id then
            goto Continue_Trace_Insns;
         end if;

         --  Ensure there is a coverage information entry for this SCO

         while SCI_Vector.Last_Index < SCO loop
            declare
               New_Index : constant SCO_Id := SCI_Vector.Last_Index + 1;
               New_SCI   : Source_Coverage_Info
                 (Kind  => Kind (New_Index));
               pragma Warnings (Off, New_SCI);
               --  Used for default initialization value only
            begin
               SCI_Vector.Append (New_SCI);
            end;
         end loop;

         --  Find enclosing statement SCO and mark it as executed

         S_SCO := SCO;
         while Kind (S_SCO) /= Statement loop
            S_SCO := Parent (S_SCO);
            pragma Assert (S_SCO /= No_SCO_Id);
         end loop;

         loop
            --  Mark S_SCO as executed

            SCI_Vector.Update_Element (S_SCO, Set_Executed'Access);

            --  Propagate back to beginning of basic block

            S_SCO := Previous (S_SCO);
            exit when S_SCO = No_SCO_Id
              or else SCI_Vector.Element (S_SCO).Executed;
            SCI_Vector.Update_Element (S_SCO, Set_Executed'Access);
         end loop;

         if not (Enabled (Decision) or else MCDC_Coverage_Enabled)
           or else Kind (SCO) /= Condition
           or else not Cond_Branch_Map.Contains ((Subp_Info.Exec, PC))
         then
            goto Continue_Trace_Insns;
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
                           Report
                             (D_SCO,
                              "inferred values mismatch: expected "
                              & Image (Inferred_Values)
                              & ", got " & Image (Eval.Values),
                              Kind => Error);
                        end if;

                     else
                        --  Reconstruct complete evaluation information from
                        --  just the outcome.

                        Eval.Decision       := D_SCO;
                        Eval.Values         := Inferred_Values;
                        Eval.Next_Condition := No_Condition_Index;
                     end if;
                  end if;

                  pragma Assert
                    (Eval.Next_Condition = No_Condition_Index);

                  Eval.Outcome := CBE.Outcome;
                  SCI.Evaluations.Append (Eval);
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
                     SCI_Vector.Update_Element
                       (D_SCO, Set_Outcome_Taken'Access);
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
                       and then (Has_Diamond (SCO) or else Debug_Full_History)
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

         <<Continue_Trace_Insns>>
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
      Level : Coverage_Level) return Line_State is
   begin
      if SCO in SCI_Vector.First_Index .. SCI_Vector.Last_Index then
         return SCI_Vector.Element (SCO).State (Level);
      else
         return No_Code;
      end if;
   end Get_Line_State;

   -----------------------
   -- Has_Been_Executed --
   -----------------------

   function Has_Been_Executed (SCO : SCO_Id) return Boolean is
   begin
      pragma Assert (Kind (SCO) = Statement);

      if SCO in SCI_Vector.First_Index .. SCI_Vector.Last_Index then
         return SCI_Vector.Element (SCO).Executed;
      else
         return False;
      end if;
   end Has_Been_Executed;

   ------------------
   -- Set_Executed --
   ------------------

   procedure Set_Executed (SCI : in out Source_Coverage_Info) is
   begin
      SCI.Executed := True;
   end Set_Executed;

   -----------------------
   -- Update_Line_State --
   -----------------------

   procedure Update_Line_State
     (Line  : Line_Info_Access;
      SCO   : SCO_Id;
      Level : Coverage_Level;
      State : Line_State)
   is
   begin
      Update_State (Line.State (Level), SCO, Level, State);
   end Update_Line_State;

   ------------------
   -- Update_State --
   ------------------

   procedure Update_State
     (Prev_State : in out Line_State;
      SCO        : SCO_Id;
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
      if SCO in SCI_Vector.First_Index .. SCI_Vector.Last_Index then
         SCI_Vector.Update_Element (SCO, Update_SCO_Line_State'Access);
      end if;

      Prev_State := Prev_State * State;
   end Update_State;

end Coverage.Source;
