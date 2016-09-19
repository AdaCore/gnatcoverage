------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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
with Ada.Tags;
with Ada.Unchecked_Deallocation;

with Interfaces;

with Binary_Files;      use Binary_Files;
with Coverage.Tags;     use Coverage.Tags;
with Decision_Map;      use Decision_Map;
with Diagnostics;       use Diagnostics;
with Elf_Disassemblers; use Elf_Disassemblers;
with MC_DC;             use MC_DC;
with Outputs;           use Outputs;
with Slocs;             use Slocs;
with Strings;           use Strings;
with Switches;          use Switches;
with Traces_Elf;        use Traces_Elf;
with Types;

package body Coverage.Source is

   use Ada.Containers;

   function Report_If_Excluded (SCO : SCO_Id) return Boolean;
   --  If True, mention in output that SCO cannot be covered (due to absence of
   --  any object code whose traces might discharge the SCO).

   --  For each source coverage obligation, we maintain a corresponding source
   --  coverage information record, which denotes the coverage state of the
   --  SCO. Default initialization denotes a completely uncovered state.

   package Evaluation_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Evaluation);

   package Evaluation_Sets is new Ada.Containers.Ordered_Sets (Evaluation);

   type Outcome_Taken_Type is array (Boolean) of Boolean;
   No_Outcome_Taken    : constant Outcome_Taken_Type := (others => False);
   Both_Outcomes_Taken : constant Outcome_Taken_Type := (others => True);

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
            --  Set True when the statement is known to have been executed

            Line_Executed : Boolean := False;
            --  Set True when some code on a line intersected by the statement
            --  has been executed.

         when Decision =>
            Outcome_Taken, Known_Outcome_Taken : Outcome_Taken_Type :=
                                                   No_Outcome_Taken;
            --  Each of these components is set True when the corresponding
            --  outcome has been exercised. Outcome_Taken is set depending
            --  on conditional branch instructions, and might be reversed
            --  (if the decision has degraded origin). Known_Outcome_Taken
            --  is set from dominance information, and is always accurate when
            --  set (but may be unset for an outcome that does not dominate
            --  any statement).

            Evaluations : Evaluation_Sets.Set;
            --  Set of all distinct evaluations of this decision (computed for
            --  MC/DC only).

         when others =>
            null;
      end case;
   end record;
   type Source_Coverage_Info_Access is access constant Source_Coverage_Info;
   type RW_Source_Coverage_Info_Access is access Source_Coverage_Info;
   procedure Read_SCI
     (S   : access Root_Stream_Type'Class;
      SCI : out RW_Source_Coverage_Info_Access);
   --  Allocate a new SCI initialized from S

   procedure Write_SCI
     (S   : access Root_Stream_Type'Class;
      SCI : RW_Source_Coverage_Info_Access);
   --  Output SCI.all to S

   for RW_Source_Coverage_Info_Access'Read use Read_SCI;
   for RW_Source_Coverage_Info_Access'Write use Write_SCI;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Source_Coverage_Info, RW_Source_Coverage_Info_Access);

   package SCI_Vectors is new Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => RW_Source_Coverage_Info_Access);

   package SCI_Vector_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_SCO_Id,
      Element_Type => SCI_Vectors.Vector,
      "="          => SCI_Vectors."=");

   SCI_Vector : SCI_Vector_Vectors.Vector;

   Default_SCIs : array (SCO_Kind) of Source_Coverage_Info_Access;
   --  Default SCI structures returned by Get_SCI when no specific one has
   --  been allocated for a given SCO.

   function Get_SCI
     (SCO : SCO_Id; Tag : SC_Tag) return Source_Coverage_Info_Access;
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
   --  Always True for all decisions that are part of a control structure; for
   --  other decisions, True if All_Decisions is set, or if the decision is
   --  complex and MC/DC is enabled. Note: this can be True even for decisions
   --  that are not Decision_Coverable.

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

   procedure Merge_Checkpoint_SCI
     (SCO    : SCO_Id;
      Tag    : SC_Tag;
      CP_SCI : Source_Coverage_Info);
   --  Merge the given checkpointed coverage information with current coverage
   --  info for SCO.

   --------------------------
   -- Basic_Block_Has_Code --
   --------------------------

   function Basic_Block_Has_Code (SCO : SCO_Id; Tag : SC_Tag) return Boolean is
   begin
      return Get_SCI (SCO, Tag).Basic_Block_Has_Code;
   end Basic_Block_Has_Code;

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save (S : access Root_Stream_Type'Class) is
   begin
      Ada.Tags.Tag'Write (S, Tag_Provider'Tag);
      SCI_Vector_Vectors.Vector'Write (S, SCI_Vector);
   end Checkpoint_Save;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load
     (S  : access Root_Stream_Type'Class;
      CS : access Checkpoint_State)
   is
      use type Ada.Tags.Tag;
      use SCI_Vector_Vectors;

      CP_Tag_Provider : Ada.Tags.Tag;
      CP_SCI_Vector   : SCI_Vector_Vectors.Vector;

   begin
      --  Checkpointed coverage information can only be loaded if the current
      --  tag provider is the default (i.e. no coverage separation), or same
      --  as checkpoint.

      Ada.Tags.Tag'Read (S, CP_Tag_Provider);
      if Tag_Provider.all not in Default_Tag_Provider_Type
        and then Tag_Provider'Tag /= CP_Tag_Provider
      then
         Warn ("cannot merge coverage information separated by "
               & Tag_Providers.Name (CP_Tag_Provider));
         return;
      end if;

      --  Extend SCI vector to accomodate any supplementary SCOs loaded from
      --  the checkpoint.

      Initialize_SCI;

      SCI_Vector_Vectors.Vector'Read (S, CP_SCI_Vector);
      for SCO_Cur in CP_SCI_Vector.Iterate loop
         Process_One_SCO : declare
            CP_SCO : constant SCO_Id := To_Index (SCO_Cur);
            SCO    : constant SCO_Id := CS.SCO_Map (CP_SCO);

            procedure Free_SCIs (SCIV : in out SCI_Vectors.Vector);
            --  Deallocate all elements in SCIV

            ---------------
            -- Free_SCIs --
            ---------------

            procedure Free_SCIs (SCIV : in out SCI_Vectors.Vector) is
            begin
               for CP_SCI of SCIV loop
                  Free (CP_SCI);
               end loop;
            end Free_SCIs;

         --  Start of processing for Process_One_SCO

         begin
            if SCO /= No_SCO_Id then
               for CP_SCI of Element (SCO_Cur) loop
                  if CP_SCI /= null then
                     Merge_Checkpoint_SCI
                       (SCO,
                        Tag_Provider.Map_Tag (CS.all, CP_SCI.Tag),
                        CP_SCI.all);
                  end if;
               end loop;
            end if;

            --  Deallocate checkpoint SCIs for this SCO once they have been
            --  merged into the main SCI vector.

            CP_SCI_Vector.Update_Element (SCO_Cur, Free_SCIs'Access);
         end Process_One_SCO;
      end loop;
   end Checkpoint_Load;

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
      if Line_Info.SCOs = null then
         --  No SCOs associated with this source line

         --  ??? Have a debug mode to warn if there is object code with
         --  this line ?

         return;
      end if;

      --  Examine each SCO associated with line

      for SCO of Line_Info.SCOs.all loop
         SCOs_Of_Line : declare
            SCO_State : Line_State := No_Code;

            procedure Ensure_SCI (SCIV : in out SCI_Vectors.Vector);
            --  If SCIV is empty, add a SCI with no tag

            ----------------
            -- Ensure_SCI --
            ----------------

            procedure Ensure_SCI (SCIV : in out SCI_Vectors.Vector) is
            begin
               if SCIV.Length = 0 then
                  SCIV.Append (new Source_Coverage_Info (Kind => Kind (SCO)));
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

                  if Is_Pragma_Annotate (SCO)
                    or else Is_Disabled_Statement (SCO)
                  then
                     --  A disabled statement (i.e. a pragma Assert/Debug/PPC
                     --  that is not enabled) or a pragma Annotate are known
                     --  and intended to not generate any executable code. They
                     --  are treated as documentation items in source, and are
                     --  not subject to coverage discussion: they are neither
                     --  covered nor not-covered, and need not be reported as
                     --  bona fide statements excluded from coverage analysis
                     --  either (see below case).

                     null;

                  elsif Unit_Has_Code (SCO)
                          and then not Basic_Block_Has_Code (SCO, SCI.Tag)
                  then

                     --  If a unit has any code at all, then a SCO is marked
                     --  as covered or not covered if there is code for it, or
                     --  for a subsequent SCO in the same basic block, else
                     --  we leave it as No_Code because it won't ever possibly
                     --  be covered anyway, so that a line ends up marked as
                     --  No_Code only if no code execution can ever cause it
                     --  to be marked as covered. However, if no code at all
                     --  has been seen for the entire unit, this means that
                     --  the user probably omitted required tests for that
                     --  unit, so in that case we conservatively mark all
                     --  statements in the unit as not covered (on the basis
                     --  that they might end up having code, and be marked
                     --  as not covered, when the code for the unit is actually
                     --  loaded).

                     --  The distinction of the two cases of no code being
                     --  present for a SCO is that in the first case, the
                     --  code for the surrounding unit is present, so we know
                     --  the compiler definitely did not generate code for
                     --  that SCO, whereas in the second case the entire object
                     --  for the unit was generated by the compiler but then
                     --  omitted at link time, so we don't know for sure
                     --  whether or not the compiler emitted code for that SCO,
                     --  so we conservatively assume that it might have.

                     if Report_If_Excluded (SCO) then
                        SCO_State := Not_Coverable;
                        Report_Exclusion (SCO, SCI.Tag, "has no object code");
                     end if;

                  elsif SCI.Executed then
                     SCO_State := Covered;

                  elsif SCI.Line_Executed then
                     if Is_Multistatement_Line (Line_Info.all) then

                        --  There is more than one statement SCO for this line.
                        --  When statements do not have full column numbers in
                        --  debug information, one cannot discriminate between
                        --  code for multiple statement SCOs on the same line.
                        --  We therefore conservatively mark each SCO (and
                        --  hence the complete line) as partially, rather than
                        --  fully, covered, and we report a coverage violation
                        --  on the first SCO on the line.

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
                        --  There is just one statement for this line, so we
                        --  know for certain that it has been executed.
                        --  Note: Ensure_SCI above guarantees that SCI is an
                        --  actual specific SCI, not one of the default ones.

                        SCI.Executed := True;
                        SCO_State := Covered;
                     end if;

                  else
                     SCO_State := Not_Covered;

                     --  Generate violation message on first line of SCO

                     if Line_Num = First_Sloc (SCO).L.Line then
                        Report_Violation (SCO, SCI.Tag, "not executed");
                     end if;
                  end if;

                  Update_Line_State (Line_Info, SCO, SCI.Tag, Stmt, SCO_State);

               elsif Kind (SCO) = Decision
                 and then First_Sloc (SCO).L.Line /= Line_Num
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

                  if Decision_Outcome (SCO) /= Unknown then
                     --  Case of a compile time known decision: exclude from
                     --  coverage analysis.

                     if Report_If_Excluded (SCO) then
                        SCO_State := Not_Coverable;

                        --  Note: we do not report the exclusion of this SCO,
                        --  because if it is in an IF statement, then the IF
                        --  statement could be covered by back propagation, and
                        --  it would be confusing to see a line marked + in
                        --  annotated sources in conjunction with a message
                        --  mentioning an uncoverable construct in the
                        --  report output.
                     end if;

                  elsif SCI.Outcome_Taken = Both_Outcomes_Taken
                          or else
                        SCI.Known_Outcome_Taken = Both_Outcomes_Taken
                  then
                     --  Here for a decision whose both outcomes have been
                     --  exercised.

                     SCO_State := Covered;

                  elsif SCI.Outcome_Taken /= No_Outcome_Taken
                          or else
                        SCI.Known_Outcome_Taken /= No_Outcome_Taken
                  then
                     --  Here if at least one outcome has been exercised,
                     --  determined either by conditional branch instructions
                     --  (Outcome_Taken) or dominance (Known_Outcome_Taken).

                     SCO_State := Partially_Covered;

                     declare
                        Missing_Outcome : Tristate := Unknown;
                     begin
                        --  Indicate which outcome has never been taken: if
                        --  FALSE has been taken then this is outcome TRUE,
                        --  else FALSE.

                        if SCI.Known_Outcome_Taken (False)
                          /= SCI.Known_Outcome_Taken (True)
                        then
                           Missing_Outcome :=
                             To_Tristate (SCI.Known_Outcome_Taken (False));

                        elsif not Degraded_Origins (SCO) then
                           Missing_Outcome :=
                             To_Tristate (SCI.Outcome_Taken (False));
                        end if;

                        if Missing_Outcome = Unknown then
                           Report_Violation
                             (SCO, SCI.Tag,
                              "not exercised in both directions");

                        else
                           Report_Violation
                             (SCO,
                              SCI.Tag,
                              "outcome "
                              & Missing_Outcome'Img & " never exercised");
                        end if;
                     end;

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

                     declare
                        S_SCO : constant SCO_Id := Enclosing_Statement (SCO);
                        S_SCI : constant Source_Coverage_Info_Access :=
                          (if S_SCO = No_SCO_Id
                             then null
                             else Get_SCI (S_SCO, SCI.Tag));
                     begin
                        if S_SCI = null
                          or else S_SCI.Executed
                          or else S_SCI.Line_Executed
                        then
                           Report_Violation (SCO, SCI.Tag, "never evaluated");
                        end if;
                     end;
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
                           Compute_MCDC_State (SCO, SCI.all));

                     elsif SCO_State /= No_Code then

                        --  Case of MC/DC enabled, and decision is coverable
                        --  but at least one outcome was never taken: do not
                        --  report details regarding MC/DC coverage, just
                        --  record that MC/DC is not achieved.

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
     (Subp_Key  : Subprogram_Key;
      Subp_Info : Subprogram_Info;
      T         : Trace_Entry)
   is
      pragma Unreferenced (Subp_Key);

      use type Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_64;

      Exe      : Exe_File_Acc renames Subp_Info.Exec;
      PC       : Pc_Type;
      Insn_Len : Natural;
      Tag      : SC_Tag;

      procedure Discharge_SCO
        (SCO                 : SCO_Id;
         Tsloc               : Tagged_Sloc;
         Empty_Range         : Boolean;
         Multistatement_Line : Boolean);
      --  Discharge the coverage obligation denoted by SCO using the current
      --  execution trace for an instruction at PC, with the given tagged
      --  sloc. Empty_Range is True if the sloc for PC that is associated with
      --  SCO has an empty PC range.

      -------------------
      -- Discharge_SCO --
      -------------------

      procedure Discharge_SCO
        (SCO                 : SCO_Id;
         Tsloc               : Tagged_Sloc;
         Empty_Range         : Boolean;
         Multistatement_Line : Boolean)
      is
         Propagating, No_Propagation : Boolean;

         S_SCO   : SCO_Id;
         Dom_SCO : SCO_Id;
         Dom_Val : Boolean;

         Precise : constant Boolean := Tsloc.Sloc.L.Column /= 0;

         Line_Executed : Boolean;
         --  Set True if we are discharging from a trace with imprecise sloc
         --  that has line information only (column unknown).

         Tag_Suffix : constant String :=
                        (if Tag = No_SC_Tag
                         then ""
                         else ", tag=" & Tag_Provider.Tag_Name (Tag));
         --  Suffix identifying tag for sloc in debug message

         procedure Set_Executed (SCI : in out Source_Coverage_Info);
         --  Set Executed (if Line_Executed is False) or Line_Executed (if it
         --  is True) to True.

         procedure Set_Known_Outcome_Taken (SCI : in out Source_Coverage_Info);
         --  Set SCI.Known_Outcome_Taken (Dom_Val) to True

         ------------------
         -- Set_Executed --
         ------------------

         procedure Set_Executed (SCI : in out Source_Coverage_Info) is
         begin
            if Line_Executed then
               SCI.Line_Executed := True;
            else
               SCI.Executed := True;
            end if;
         end Set_Executed;

         -----------------------------
         -- Set_Known_Outcome_Taken --
         -----------------------------

         procedure Set_Known_Outcome_Taken
           (SCI : in out Source_Coverage_Info)
         is
         begin
            SCI.Known_Outcome_Taken (Dom_Val) := True;
         end Set_Known_Outcome_Taken;

      --  Start of processing for Discharge_SCO

      begin
         --  Find enclosing statement SCO (if any) and mark it as executed

         S_SCO := Enclosing_Statement (SCO);
         Propagating := False;
         while S_SCO /= No_SCO_Id loop

            --  If we are discharging a SCO from an imprecise sloc within
            --  its line range, only mark it Line_Executed (else we
            --  are propagating, and execution is certain even if the
            --  originating trace is imprecise).

            declare
               use Types;

               S_SCO_First : constant Source_Location := First_Sloc (S_SCO);
               S_SCO_Last  : constant Source_Location := Last_Sloc (S_SCO);

               Cur_SCI     : constant Source_Coverage_Info_Access :=
                               Get_SCI (S_SCO, Tag);
            begin
               Line_Executed := not Precise
                 and then Tsloc.Sloc.Source_File = S_SCO_First.Source_File
                 and then Tsloc.Sloc.L.Line
                            in S_SCO_First.L.Line .. S_SCO_Last.L.Line;

               exit when Cur_SCI.Executed
                 or else (Line_Executed and Cur_SCI.Line_Executed);
            end;

            --  For pragma Pre/Postcondition, no propagation: the statement
            --  is never marked as executed by propagation, and marking it
            --  does not cause propagation to other statements. We also
            --  cannot propagate from an imprecise sloc if the line has
            --  multiple statements.

            No_Propagation := Is_Pragma_Pre_Post_Condition (S_SCO)
              or else (not Precise and then Multistatement_Line);

            if not (Propagating and No_Propagation) then

               --  Mark S_SCO as executed

               Report
                 (Msg  => (if Line_Executed then "line " else "")
                            & "executed" & Tag_Suffix
                            & (if Propagating then " (propagating)" else ""),
                  SCO  => S_SCO,
                  Exe  => Exe,
                  PC   => PC,
                  Kind => Notice);

               Update_SCI (S_SCO, Tag, Set_Executed'Access);
            end if;

            exit when not Propagating and No_Propagation;

            --  Propagate back to beginning of basic block, and possibly to
            --  upper decisions.

            Propagating := True;

            Dominant (S_SCO, Dom_SCO, Dom_Val);
            if Dom_SCO /= No_SCO_Id
              and then Kind (Dom_SCO) = Decision
              and then not Get_SCI (Dom_SCO, Tag).Known_Outcome_Taken (Dom_Val)
            then
               Report
                 (Msg  => "outcome " & Dom_Val'Img & " taken" & Tag_Suffix
                            & " (propagating)",
                  SCO  => Dom_SCO,
                  Exe  => Exe,
                  PC   => PC,
                  Kind => Notice);

               Update_SCI (Dom_SCO, Tag, Set_Known_Outcome_Taken'Access);
            end if;

            S_SCO := Enclosing_Statement (Dom_SCO);
         end loop;

         if not (Enabled (Decision) or else MCDC_Coverage_Enabled)
           or else Kind (SCO) /= Condition
           or else not Cond_Branch_Map.Contains ((Subp_Info.Exec, PC))
           or else Empty_Range
           or else not Precise
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

                  Report
                    (Msg  => "outcome " & CBE.Outcome'Img
                             & (if Degraded_Origins (D_SCO)
                                then " (degraded)"
                                else "")
                             & " taken by " & E'Img,
                     SCO  => D_SCO,
                     Exe  => Exe,
                     PC   => PC,
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
                     Kind => Diagnostics.Error);

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
                           Kind => Diagnostics.Error);
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
               "processing cond branch trace op" & T.Op'Img & " (" &
               (case T.Op and 3 is
                     when 1      => "branch",
                     when 2      => "fallthrough",
                     when 3      => "both",
                     when others => "???") & " taken)",
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

      I_Ranges : Insn_Set_Ranges renames
        Get_Insn_Set_Ranges (Subp_Info.Exec.all, Subp_Info.Section).all;
      Cache    : Insn_Set_Cache := Empty_Cache;
      Insn_Set : Insn_Set_Type;

   --  Start of processing for Compute_Source_Coverage

   begin
      --  Set current subprogram for separated source coverage analysis

      Tag_Provider.Enter_Routine (Subp_Info);

      --  Iterate over trace for this routine

      PC := T.First + Subp_Info.Offset;

      Trace_Insns :
      while Iterate_Over_Insns
        (I_Ranges, Cache, T.Last + Subp_Info.Offset, PC, Insn_Set)
      loop
         Insn_Len := Disa_For_Machine (Machine, Insn_Set).
           Get_Insn_Length (Slice (Subp_Info.Insns, PC, Subp_Info.Insns.Last));

         --  Discharge each SCO for source locations associated with this
         --  instruction.

         declare
            SL         : constant Tagged_Slocs :=
                           Tag_Provider.Get_Slocs_And_Tags (PC);
            SCOs       : access SCO_Id_Array;
            Single_SCO : aliased SCO_Id_Array := (0 => No_SCO_Id);

            Multistatement_Line : Boolean;
            --  Set True if discharging from an imprecise sloc on a line with
            --  multiple statement SCOs.

         begin
            for J in SL'Range loop
               Tag := SL (J).Tag;

               Multistatement_Line := False;
               Single_SCO (Single_SCO'First) := No_SCO_Id;
               SCOs := null;

               if SL (J).Sloc.L.Column = 0 then
                  declare
                     LI : constant Line_Info_Access := Get_Line (SL (J).Sloc);
                  begin
                     if LI /= null and then LI.SCOs /= null then
                        SCOs := LI.SCOs.all'Access;
                        Multistatement_Line := Is_Multistatement_Line (LI.all);

                     else
                        --  No SCO at all on line: leave SCOs uninitialized,
                        --  will be set below to point to Single_SCO (set to
                        --  No_SCO_Id).

                        null;
                     end if;
                  end;
               else
                  --  If we have column-accurate sloc information, then there
                  --  is at most a single SCO to discharge.

                  Single_SCO (Single_SCO'First) := Sloc_To_SCO (SL (J).Sloc);
               end if;

               if SCOs = null then

                  --  Case where line has no SCO at all, or we have column-
                  --  accurate sloc information.

                  SCOs := Single_SCO'Unchecked_Access;
               end if;

               for SCO of SCOs.all loop
                  if SCO /= No_SCO_Id then
                     --  All but the first sloc in SL correspond to an empty PC
                     --  range (Address_Infos with shorter PC ranges sort
                     --  higher).

                     Discharge_SCO
                       (SCO,
                        Empty_Range         => J > SL'First,
                        Tsloc               => SL (J),
                        Multistatement_Line => Multistatement_Line);
                  end if;
               end loop;
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
            "unexpected condition" & Index (C_SCO)'Img & " in trace, expected"
            & Evaluation_Stack.Last_Element.Next_Condition'Img,
            Kind => Warning);
      end if;

      Evaluation_Stack.Update_Element
        (Evaluation_Stack.Last_Index, Update_Current_Evaluation'Access);
   end Condition_Evaluated;

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

   function Get_SCI
     (SCO : SCO_Id; Tag : SC_Tag) return Source_Coverage_Info_Access
   is
      Result : RW_Source_Coverage_Info_Access;

      procedure Q (SCI : RW_Source_Coverage_Info_Access);
      --  Set Result to SCI if SCI.Tag = Tag

      procedure QV (SCIV : SCI_Vectors.Vector);
      --  Run Q for each element of SCIV, stopping if Result is set

      -------
      -- Q --
      -------

      procedure Q (SCI : RW_Source_Coverage_Info_Access) is
      begin
         if SCI.Tag = Tag then
            Result := SCI;
         end if;
      end Q;

      --------
      -- QV --
      --------

      procedure QV (SCIV : SCI_Vectors.Vector) is
      begin
         for J in SCIV.First_Index .. SCIV.Last_Index loop
            SCIV.Query_Element (J, Q'Access);
            exit when Result /= null;
         end loop;
      end QV;

   --  Start of processing for Get_SCI

   begin
      if SCO in SCI_Vector.First_Index .. SCI_Vector.Last_Index then
         SCI_Vector.Query_Element (SCO, QV'Access);
      end if;

      return
        (if Result /= null
         then Source_Coverage_Info_Access (Result)
         else Default_SCIs (Kind (SCO)));
   end Get_SCI;

   --------------------
   -- Initialize_SCI --
   --------------------

   Default_SCIs_Initialized : Boolean := False;

   procedure Initialize_SCI is
      Last_SCO : constant SCO_Id := SC_Obligations.Last_SCO;
   begin
      if Last_SCO > SCI_Vector.Last_Index then
         SCI_Vector.Set_Length (Ada.Containers.Count_Type (Last_SCO));
         pragma Assert (SCI_Vector.Last_Index = Last_SCO);
      end if;

      if not Default_SCIs_Initialized then
         for K in Default_SCIs'Range loop
            Default_SCIs (K) :=
              Source_Coverage_Info_Access
                (RW_Source_Coverage_Info_Access'
                   (new Source_Coverage_Info (Kind => K)));
         end loop;
         Default_SCIs_Initialized := True;
      end if;
   end Initialize_SCI;

   --------------------------
   -- Merge_Checkpoint_SCI --
   --------------------------

   procedure Merge_Checkpoint_SCI
     (SCO    : SCO_Id;
      Tag    : SC_Tag;
      CP_SCI : Source_Coverage_Info)
   is
      procedure Merge_SCI (SCI : in out Source_Coverage_Info);
      --  Merge coverage information from checkpointed CP_SCI into SCI

      ---------------
      -- Merge_SCI --
      ---------------

      procedure Merge_SCI (SCI : in out Source_Coverage_Info) is
      begin
         pragma Assert (SCI.Kind = CP_SCI.Kind);

         --  Merge raw coverage information from checkpoint. SCI.Line_State
         --  will be recomputed later on, once traces for this increment have
         --  been processed.

         case SCI.Kind is
            when Statement =>
               SCI.Basic_Block_Has_Code :=
                 SCI.Basic_Block_Has_Code or CP_SCI.Basic_Block_Has_Code;
               SCI.Executed      := SCI.Executed or CP_SCI.Executed;
               SCI.Line_Executed := SCI.Line_Executed or CP_SCI.Line_Executed;

            when Decision =>
               SCI.Known_Outcome_Taken :=
                 SCI.Known_Outcome_Taken or CP_SCI.Known_Outcome_Taken;

               --  Note: if checkpoint has only one Outcome_Taken, and the SCO
               --  has degraded origins, then we can't take advantage of it,
               --  because it might be negated compared to the current context.

               if not Degraded_Origins (SCO)
                    or else
                  CP_SCI.Outcome_Taken (False) = CP_SCI.Outcome_Taken (True)
               then
                  SCI.Outcome_Taken :=
                    SCI.Outcome_Taken or CP_SCI.Outcome_Taken;
               end if;

               SCI.Evaluations.Union (CP_SCI.Evaluations);

            when others =>
               null;
         end case;
      end Merge_SCI;

   --  Start of processing for Merge_Checkpoint_SCI

   begin
      Update_SCI (SCO, Tag, Merge_SCI'Access);
   end Merge_Checkpoint_SCI;

   --------------
   -- Read_SCI --
   --------------

   procedure Read_SCI
     (S   : access Root_Stream_Type'Class;
      SCI : out RW_Source_Coverage_Info_Access)
   is
      CP_SCI : constant Source_Coverage_Info :=
        Source_Coverage_Info'Input (S);
   begin
      if CP_SCI = Default_SCIs (CP_SCI.Kind).all then
         SCI := null;
      else
         SCI := new Source_Coverage_Info'(CP_SCI);
      end if;
   end Read_SCI;

   ------------------------
   -- Report_If_Excluded --
   ------------------------

   function Report_If_Excluded (SCO : SCO_Id) return Boolean is
   begin
      if Excluded_SCOs then
         return Kind (SCO) = Decision
           or else (Kind (SCO) = Statement
                      and then S_Kind (SCO) in Ada_Statement_Kind);
      else
         return False;
      end if;
   end Report_If_Excluded;

   ------------------------------
   -- Set_Basic_Block_Has_Code --
   ------------------------------

   procedure Set_Basic_Block_Has_Code (SCO : SCO_Id; Tag : SC_Tag) is

      S_SCO : SCO_Id := SCO;
      pragma Assert (Kind (S_SCO) = Statement);

      procedure Set_SCI_BB_Has_Code (SCI : in out Source_Coverage_Info);
      --  Set SCI.Basic_Block_Has_Code

      -------------------------
      -- Set_SCI_BB_Has_Code --
      -------------------------

      procedure Set_SCI_BB_Has_Code (SCI : in out Source_Coverage_Info) is
      begin
         SCI.Basic_Block_Has_Code := True;
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
      Cell : constant Line_State_Cell := Coverage_Level_To_Cell (Level);
   begin
      Update_State (Line.State (Cell), SCO, Tag, Level, State);
   end Update_Line_State;

   ----------------
   -- Update_SCI --
   ----------------

   procedure Update_SCI
     (SCO     : SCO_Id;
      Tag     : SC_Tag;
      Process : access procedure (SCI : in out Source_Coverage_Info))
   is
      procedure Deref_Process (SCIA : RW_Source_Coverage_Info_Access);
      --  Call Process (SCIA.all) and set Processed to True if SCIA.Tag = Tag

      procedure Update_SCIV (SCIV : in out SCI_Vectors.Vector);
      --  Call Process on the relevant element of SCIV

      Processed : Boolean;

      -------------------
      -- Deref_Process --
      -------------------

      procedure Deref_Process (SCIA : RW_Source_Coverage_Info_Access) is
      begin
         if SCIA.Tag = Tag then
            Process (SCIA.all);
            Processed := True;
         end if;
      end Deref_Process;

      -----------------
      -- Update_SCIV --
      -----------------

      procedure Update_SCIV (SCIV : in out SCI_Vectors.Vector) is
      begin
         Processed := False;
         for J in SCIV.First_Index .. SCIV.Last_Index loop
            SCIV.Query_Element (J, Deref_Process'Access);
            if Processed then
               return;
            end if;
         end loop;

         --  Here if no SCI exists yet for this SCO and tag

         declare
            New_SCI : constant RW_Source_Coverage_Info_Access :=
                        new Source_Coverage_Info (Kind (SCO));
         begin
            New_SCI.Tag := Tag;
            Process (New_SCI.all);
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

   ---------------
   -- Write_SCI --
   ---------------

   procedure Write_SCI
     (S   : access Root_Stream_Type'Class;
      SCI : RW_Source_Coverage_Info_Access)
   is
   begin
      Source_Coverage_Info'Output (S, SCI.all);
   end Write_SCI;

end Coverage.Source;
