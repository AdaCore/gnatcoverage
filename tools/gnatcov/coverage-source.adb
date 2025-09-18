------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
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

with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Unchecked_Deallocation;

with Interfaces;

with GPR2.Build.Source;
with GPR2.Project.View;

with Binary_Files;          use Binary_Files;
with Decision_Map;          use Decision_Map;
with Diagnostics;           use Diagnostics;
with Elf_Disassemblers;     use Elf_Disassemblers;
with LLVM_JSON_Checkpoints; use LLVM_JSON_Checkpoints;
with Outputs;               use Outputs;
with Project;               use Project;
with Slocs;                 use Slocs;
with Switches;              use Switches;
with Traces_Elf;            use Traces_Elf;
with Traces_Files;          use Traces_Files;
with Traces_Source;         use Traces_Source;
with Types;

package body Coverage.Source is

   --  This unit instantiates containers and we want to avoid too much
   --  performance cost when using references to their elements, so suppress
   --  tampering checks.

   pragma Suppress (Tampering_Check);

   use Ada.Containers;

   function Report_If_Excluded (SCO : SCO_Id) return Boolean;
   --  If True, mention in output that SCO cannot be covered (due to absence of
   --  any object code whose traces might discharge the SCO).

   --  For each source coverage obligation, we maintain a corresponding source
   --  coverage information record, which denotes the coverage state of the
   --  SCO. Default initialization denotes a completely uncovered state.

   procedure Read is new
     Read_Set
       (Element_Type => Evaluation,
        Set_Type     => Evaluation_Sets.Set,
        Clear        => Evaluation_Sets.Clear,
        Insert       => Evaluation_Sets.Insert,
        Read_Element => Read);

   procedure Write is new
     Write_Set
       (Element_Type  => Evaluation,
        Set_Type      => Evaluation_Sets.Set,
        Cursor_Type   => Evaluation_Sets.Cursor,
        Length        => Evaluation_Sets.Length,
        Iterate       => Evaluation_Sets.Iterate,
        Query_Element => Evaluation_Sets.Query_Element,
        Write_Element => Write);

   type Outcome_Taken_Type is array (Boolean) of Boolean;
   No_Outcome_Taken    : constant Outcome_Taken_Type := (others => False);
   Both_Outcomes_Taken : constant Outcome_Taken_Type := (others => True);

   type Line_States is array (Coverage_Level) of Line_State;

   type Source_Coverage_Info (Kind : SCO_Kind := Statement) is record
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
            --  Each of these components is set to True when the corresponding
            --  outcome has been exercised. Outcome_Taken is set depending
            --  on conditional branch instructions, and might be reversed
            --  (if the decision has degraded origin). Known_Outcome_Taken
            --  is set from dominance information, and is always accurate when
            --  set (but may be unset for an outcome that does not dominate
            --  any statement).

            Evaluations : Evaluation_Sets.Set;
            --  Set of all distinct evaluations of this decision (computed for
            --  MC/DC only).

         when Fun_Call_SCO_Kind =>
            Fun_Call_Executed : Boolean := False;
            --  Set to True if this call or function was executed at least once

         when Guarded_Expr =>
            GExpr_Executed : Boolean := False;
            --  Set to True if this was executed at least once

         when others =>
            null;
      end case;
   end record;

   package SCI_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Valid_SCO_Id,
        Element_Type => Source_Coverage_Info);

   procedure Read
     (CLS : in out Checkpoint_Load_State; Value : out Source_Coverage_Info);
   --  Read a SCI initialized from CLS

   procedure Write
     (CSS : in out Checkpoint_Save_State; Value : Source_Coverage_Info);
   --  Write a SCI to CSS

   procedure Read is new
     Read_Vector
       (Index_Type   => Valid_SCO_Id,
        Element_Type => Source_Coverage_Info,
        Vectors      => SCI_Vectors,
        Read_Element => Read);

   procedure Write is new
     Write_Vector
       (Index_Type    => Valid_SCO_Id,
        Element_Type  => Source_Coverage_Info,
        Vectors       => SCI_Vectors,
        Write_Element => Write);

   SCI_Vector : SCI_Vectors.Vector;

   --  MC/DC evaluation stack

   Evaluation_Stack : Evaluation_Vectors.Vector;

   procedure Condition_Evaluated
     (Exe : Exe_File_Acc; PC : Pc_Type; C_SCO : SCO_Id; C_Value : Boolean);
   --  Record evaluation of condition C_SCO with the given C_Value in the
   --  current decision evaluation.

   function Compute_MCDC_State
     (SCO : SCO_Id; SCI : Source_Coverage_Info) return Line_State;
   --  Compute the MC/DC state of SCO, which is already covered for DC

   function Compute_ATCC_State
     (SCO : SCO_Id; SCI : Source_Coverage_Info) return Line_State;
   --  Compute the ATCC state of SCO, which is already covered for ATC

   function Decision_Requires_Coverage (SCO : SCO_Id) return Boolean;
   --  Always True for all decisions that are part of a control structure; for
   --  other decisions, True if All_Decisions is set, or if the decision is
   --  complex and MC/DC is enabled. This function only checks for decisions
   --  not belonging to assertions. Note: this can be True even for decisions
   --  that are not Decision_Coverable.

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

   procedure Merge_Checkpoint_SCI
     (SCO    : SCO_Id;
      CP_SCI : Source_Coverage_Info;
      Relocs : Checkpoint_Relocations);
   --  Merge the given checkpointed coverage information with current coverage
   --  info for SCO.

   Unit_List_Invalidated : Boolean := False;
   --  Keeps track of whether Invalidate_Unit_List was called

   Unit_List : Unit_Sets.Set;
   --  List of names for units of interest. Store it as an ordered set so that
   --  the order of dump depends on its content, not on the way it was created.

   package Unit_To_Ignored_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Compilation_Unit,
        Element_Type => Ignored_Sources_Vector_Access);

   Ignored_SF_Map : Unit_To_Ignored_Maps.Map;
   --  Map units of interest to the list of associated ignored source files

   procedure Initialize_SCI_For_SID
     (CU : CU_Id; SCOs_Fingerprint : SC_Obligations.Fingerprint_Type);
   --  Initialize source coverage information vectors for obligations in CU,
   --  for SCOs_Fingerprint version, assumed to come from source
   --  instrumentation.

   --------------------------
   -- Basic_Block_Has_Code --
   --------------------------

   function Basic_Block_Has_Code (SCO : SCO_Id) return Boolean is
   begin
      return SCI_Vector.Constant_Reference (SCO).Basic_Block_Has_Code;
   end Basic_Block_Has_Code;

   ------------------------
   -- Unit_List_Is_Valid --
   ------------------------

   function Unit_List_Is_Valid return Boolean is
   begin
      return not Unit_List_Invalidated;
   end Unit_List_Is_Valid;

   --------------------------
   -- Invalidate_Unit_List --
   --------------------------

   procedure Invalidate_Unit_List (Reason : String) is
   begin
      --  Log that we can't dump the list of units of interest only the first
      --  time.

      if Dump_Units and then not Unit_List_Invalidated then
         Put_Line
           ("We will not be able to dump the list of units of interest: "
            & Reason);
      end if;

      Unit_List_Invalidated := True;
      Unit_List := Unit_Sets.Empty_Set;
   end Invalidate_Unit_List;

   --------------
   -- Add_Unit --
   --------------

   procedure Add_Unit (Unit : Compilation_Unit) is
   begin
      if not Unit_List_Invalidated then
         Unit_List.Include (Unit);
      end if;
   end Add_Unit;

   -------------------------------------------
   -- Compute_Unit_Name_For_Ignored_Sources --
   -------------------------------------------

   procedure Compute_Unit_Name_For_Ignored_Sources is
      use Types;

      procedure Callback
        (Project : GPR2.Project.View.Object; File : GPR2.Build.Source.Object);
      --  If the file is a (sometimes) ignored file, compute its unit name and
      --  store it in the file table.

      --------------
      -- Callback --
      --------------

      procedure Callback
        (Project : GPR2.Project.View.Object; File : GPR2.Build.Source.Object)
      is
         pragma Unreferenced (Project);
         SFI : constant Source_File_Index :=
           Get_Index_From_Generic_Name
             (String (File.Path_Name.Value), Source_File, Insert => False);
         FI  : constant File_Info_Access :=
           (if SFI /= No_Source_File then Get_File (SFI) else null);
      begin
         if FI /= null and then not FI.Unit.Known then
            declare
               Unit : constant Compilation_Unit := To_Compilation_Unit (File);
            begin
               Consolidate_Source_File_Unit (SFI, Unit);
            end;
         end if;
      end Callback;

      --  Start of processing for Compute_Unit_Name_For_Ignored_Sources

   begin
      Enumerate_Sources
        (Callback'Access, Language => All_Languages, Only_UOIs => True);
   end Compute_Unit_Name_For_Ignored_Sources;

   -------------------------
   -- Fill_Ignored_SF_Map --
   -------------------------

   procedure Fill_Ignored_SF_Map is
      use Unit_To_Ignored_Maps;

      procedure Callback (SFI : Types.Source_File_Index);
      --  If SFI is a source file that is ignored, register it in
      --  Ignored_SF_Map under its unit name. Do nothing otherwise.

      --------------
      -- Callback --
      --------------

      procedure Callback (SFI : Types.Source_File_Index) is
         FI  : constant File_Info_Access := Get_File (SFI);
         Vec : Ignored_Sources_Vector_Access;
      begin
         if FI.Kind = Source_File
           and then FI.Ignore_Status in Always .. Sometimes
         then
            if not Ignored_SF_Map.Contains (FI.Unit.Name) then
               Vec := new Ignored_Sources_Vector.Vector;
               Ignored_SF_Map.Insert (FI.Unit.Name, Vec);
            else
               Vec := Ignored_SF_Map.Element (FI.Unit.Name);
            end if;
            Vec.Append (FI);
         end if;
      end Callback;

      --  Start of processing for Fill_Ignored_SF_Map

   begin
      Files_Table_Iterate (Callback'Access);
   end Fill_Ignored_SF_Map;

   --------------------------
   -- Iterate_On_Unit_List --
   --------------------------

   procedure Iterate_On_Unit_List
     (Process_Unit        :
        not null access procedure (Name : Compilation_Unit);
      Process_Source_File : not null access procedure (FI : File_Info)) is
   begin
      for S of Unit_List loop
         Process_Unit.all (S);

         if Ignored_SF_Map.Contains (S) then
            for FI of Ignored_SF_Map.Element (S).all loop
               Process_Source_File (FI.all);
            end loop;
         end if;
      end loop;
   end Iterate_On_Unit_List;

   ------------------
   -- Report_Units --
   ------------------

   procedure Report_Units (File : File_Type) is
      procedure Print_Ignored_File (FI : Files_Table.File_Info);
      --  Print the name of the file and its ignore status

      procedure Print_Unit_Name (Unit : Compilation_Unit);
      --  Print the unit name

      ------------------------
      -- Print_Ignored_File --
      ------------------------

      procedure Print_Ignored_File (FI : Files_Table.File_Info) is
      begin
         if FI.Ignore_Status = Files_Table.Sometimes then
            Put_Line (File, "   " & FI.Unique_Name.all & " sometimes ignored");
         elsif FI.Ignore_Status = Files_Table.Always then
            Put_Line (File, "   " & FI.Unique_Name.all & " always ignored");
         end if;
      end Print_Ignored_File;

      ---------------------
      -- Print_Unit_Name --
      ---------------------

      procedure Print_Unit_Name (Unit : Compilation_Unit) is
      begin
         case Unit.Language is
            when File_Based_Language =>
               Put_Line (File, Ada.Directories.Simple_Name (+Unit.Unit_Name));

            when Unit_Based_Language =>
               Put_Line (File, +Unit.Unit_Name);
         end case;
      end Print_Unit_Name;

      --  Start of processing for Report_Units

   begin
      Iterate_On_Unit_List (Print_Unit_Name'Access, Print_Ignored_File'Access);
   end Report_Units;

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save (CSS : access Checkpoint_Save_State) is
   begin
      Write (CSS.all, SCI_Vector);

      --  For checkpoints only, stream the list of names for units of interest

      if CSS.Purpose = Consolidation then
         CSS.Write (Unit_List_Invalidated);
         if not Unit_List_Invalidated then
            CSS.Write_Count (Unit_List.Length);
            for N of Unit_List loop
               Write (CSS.all, N);
            end loop;
         end if;
      end if;
   end Checkpoint_Save;

   ----------------------
   -- Checkpoint_Clear --
   ----------------------

   procedure Checkpoint_Clear is
   begin
      SCI_Vector.Clear;
      Unit_List_Invalidated := False;
      Unit_List := Unit_Sets.Empty_Set;
   end Checkpoint_Clear;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load (CLS : in out Checkpoint_Load_State) is
      use SCI_Vectors;

      CP_SCI_Vector : SCI_Vectors.Vector;
      Relocs        : Checkpoint_Relocations renames CLS.Relocations;
   begin
      --  Extend SCI vector to accomodate any supplementary SCOs loaded from
      --  the checkpoint.

      Initialize_SCI;

      --  Even if we cannot merge coverage information, we must read it in
      --  order to be able to decode the rest of the checkpoint.

      Read (CLS, CP_SCI_Vector);

      for SCO_Cur in CP_SCI_Vector.Iterate loop
         Process_One_SCO :
         declare
            CP_SCO  : constant SCO_Id := To_Index (SCO_Cur);
            Removed : constant Boolean := SCO_Ignored (Relocs, CP_SCO);
            SCO     : constant SCO_Id :=
              (if Removed then No_SCO_Id else Remap_SCO_Id (Relocs, CP_SCO));

            procedure Insert_Extra_Decision_SCI
              (S_Eval : Static_Decision_Evaluation_Sets.Set);
            --  Add a set of static evaluations to the rest of the Decision's
            --  evaluation set.

            procedure Insert_Extra_Decision_SCI
              (S_Eval : Static_Decision_Evaluation_Sets.Set)
            is
               Inserted_SCI : Source_Coverage_Info :=
                 (Kind => Decision, others => <>);

               function To_Evaluation
                 (SCO : SCO_Id; Static_Eval : Static_Decision_Evaluation)
                  return Evaluation;
               --  Create an `Evaluation` entry from a
               --  Static_Decision_Evaluation.

               function To_Evaluation
                 (SCO : SCO_Id; Static_Eval : Static_Decision_Evaluation)
                  return Evaluation
               is
                  Eval : Evaluation :=
                    (Decision       => SCO,
                     Outcome        => To_Tristate (Static_Eval.Outcome),
                     Values         => Condition_Evaluation_Vectors.Empty,
                     Next_Condition => No_Condition_Index);
               begin
                  Populate_From_Static_Eval_Vector
                    (SCO, Static_Eval.Values, Eval.Values);
                  return Eval;
               end To_Evaluation;

            begin
               if Kind (SCO) /= Decision then
                  raise Program_Error
                    with
                      "Unexpected "
                      & Kind (SCO)'Image
                      & " SCO kind registered as a static"
                      & " decision.";
               end if;

               SCOs_Trace.Trace
                 ("Inserting "
                  & S_Eval.Length'Image
                  & " static SCOs for "
                  & Image (CP_SCO));

               for J in S_Eval.Iterate loop
                  declare
                     Eval : Static_Decision_Evaluation renames
                       S_Eval.Element (J);
                  begin
                     Inserted_SCI.Evaluations.Include
                       (To_Evaluation (CP_SCO, Eval));
                     Inserted_SCI.Known_Outcome_Taken (Eval.Outcome) := True;
                  end;
               end loop;

               Merge_Checkpoint_SCI (SCO, Inserted_SCI, Relocs);
            end Insert_Extra_Decision_SCI;
         begin
            if CLS.Static_Decision_Evaluations.Contains (CP_SCO) then

               --  Check if the current SCO has static evaluations, and
               --  merge them as an extra SCI if yes.

               Insert_Extra_Decision_SCI
                 (CLS.Static_Decision_Evaluations.Element (CP_SCO));
            end if;

            if not Removed then
               declare
                  CP_SCI : Source_Coverage_Info renames
                    CP_SCI_Vector.Reference (SCO_Cur);
               begin
                  Merge_Checkpoint_SCI (SCO, CP_SCI, Relocs);
               end;
            end if;
         end Process_One_SCO;
      end loop;

      --  For checkpoints only (not SID files), load the list of names for
      --  units of interest.

      if CLS.Purpose = Consolidation then
         declare
            Invalidated : constant Boolean := CLS.Read_Boolean;
            Dummy       : Unbounded_String;
         begin
            if Invalidated then
               Invalidate_Unit_List
                 (+CLS.Filename
                  & " does not contain the list of units (produced with"
                  & " --scos or --sid)");
            else
               for I in 1 .. CLS.Read_Integer loop
                  Unit_List.Include (CLS.Read_Compilation_Unit);
               end loop;
            end if;
         end;
      end if;
   end Checkpoint_Load;

   --------------------
   -- LLVM_JSON_Load --
   --------------------

   procedure LLVM_JSON_Load (Ckpt : access constant LLVM_Coverage_Ckpt) is
      Rep_SCI : Source_Coverage_Info;
   begin
      LLVM_Trace.Trace ("Coverage.Source.LLVM_JSON_Load");

      --  Extend the SCI vector to accomodate for the SCOs.

      Initialize_SCI;

      for File_Report of Ckpt.File_Reports loop
         for Fct_Report of File_Report.Functions loop
            for Region of Fct_Report.Regions loop
               if Region.Kind = Statement then
                  Rep_SCI :=
                    (Kind                 => Statement,
                     Basic_Block_Has_Code => True,
                     Executed             => Region.Execution_Count > 0,
                     others               => <>);
               elsif Region.Kind = Decision then
                  declare
                     Eval_Set   : Evaluation_Sets.Set;
                     T_Executed : Boolean := False;
                     F_Executed : Boolean := False;
                  begin

                     --  Convert an evaluation vector to a set,
                     --  and check True/False_executed accordingly.

                     for Eval of Region.Test_Vectors loop
                        declare
                           Eval_Cpy : Evaluation := Eval;
                        begin
                           --  Reassign the Decision to set it to the right
                           --  SCO, and insert it in the set.

                           Eval_Cpy.Decision := Region.SCO;
                           if not Eval_Set.Contains (Eval_Cpy) then
                              Eval_Set.Insert (Eval_Cpy);
                           end if;
                        end;
                        case Eval.Outcome is
                           when True   =>
                              T_Executed := True;

                           when False  =>
                              F_Executed := True;

                           when others =>
                              null;
                        end case;
                     end loop;

                     Rep_SCI :=
                       (Kind          => Decision,
                        Outcome_Taken =>
                          (True => T_Executed, False => F_Executed),
                        Evaluations   => Eval_Set,
                        others        => <>);
                  end;
               end if;

               if Region.Kind /= Condition then
                  SCI_Vector (Region.SCO) := Rep_SCI;
               end if;
            end loop;
         end loop;
      end loop;

   end LLVM_JSON_Load;

   ------------------------
   -- Compute_Line_State --
   ------------------------

   procedure Compute_Line_State
     (Line_Num  : Positive;
      Line_Info : Line_Info_Access;
      ST        : in out Scope_Traversal_Type)
   is
      procedure Compute_Condition_Level_Line_State
        (SCO       : SCO_Id;
         SCO_State : Line_State;
         Line_Info : Line_Info_Access;
         SCI       : in out Source_Coverage_Info;
         Level     : Coverage_Level)
      with Pre => Level in MCDC | UC_MCDC | ATCC;
      --  Complete computation of Level coverage state if SCO is covered for
      --  the previous less strict coverage level. The coverage status for
      --  decision coverage is SCO_State.
      --
      --  This function is useful for the levels that require to compute
      --  the coverage of conditions, namely MCDC and ATCC. Their previous
      --  less strict coverage levels are respectively Decision and ATC.

      procedure Report_Insufficiently_Instrumented
        (SCO : SCO_Id; Level : Coverage_Level; Line_Info : Line_Info_Access);
      --  Appropriately report the case in which a SCO is not sufficiently
      --  instrumented to compute its coverage for MCDC or ATCC level.

      ----------------------------------------
      -- Compute_Condition_Level_Line_State --
      ----------------------------------------

      procedure Compute_Condition_Level_Line_State
        (SCO       : SCO_Id;
         SCO_State : Line_State;
         Line_Info : Line_Info_Access;
         SCI       : in out Source_Coverage_Info;
         Level     : Coverage_Level) is
      begin
         if SCO_State = Covered then

            --  Complete computation of MC/DC/ATCC coverage state if SCO
            --  is covered for decision/ATC coverage.

            if not Decision_SCO_Instrumented_For_MCDC (SCO) then
               Report_Insufficiently_Instrumented (SCO, Level, Line_Info);
            else
               Update_Line_State
                 (Line_Info,
                  SCO,
                  Level,
                  (if Level in MCDC_Coverage_Level
                   then Compute_MCDC_State (SCO, SCI)
                   else Compute_ATCC_State (SCO, SCI)));
            end if;

         elsif SCO_State not in No_Code | Undetermined_Coverage then

            --  Case of MC/DC or ATCC enabled, and decision / ATC is coverable
            --  but at least one outcome was never taken: do not report details
            --  regarding MC/DC / ATCC coverage, just record that MC/DC / ATCC
            --  is not achieved.

            Update_Line_State (Line_Info, SCO, Level, Not_Covered);
         end if;
      end Compute_Condition_Level_Line_State;

      -------------------------------------
      -- Check_Sufficiently_Instrumented --
      -------------------------------------

      procedure Report_Insufficiently_Instrumented
        (SCO : SCO_Id; Level : Coverage_Level; Line_Info : Line_Info_Access) is
      begin
         --  This decision was not instrumented for Level, so report only
         --  once for the whole decision, but still mark each condition
         --  as not instrumented.

         for Cond_Index in 0 .. Last_Cond_Index (SCO) loop
            Update_Line_State
              (Line_Info,
               Condition (SCO, Cond_Index),
               Level,
               Undetermined_Coverage);
         end loop;

         Update_Line_State (Line_Info, SCO, Level, Covered);

         Report_Coverage
           (SCO,
            "was not instrumented for " & Image (Level),
            Undetermined_Cov);
      end Report_Insufficiently_Instrumented;

      --  Local variables

      Multiple_Statements_Reported : Boolean := False;
      --  Set True when a diagnosis has been emitted for multiple statements

      --  Start of processing for Compute_Line_State

   begin
      if Line_Info.Coverage_Processed then

         --  Recomputing the coverage state for this line has no influence over
         --  the resulting coverage state, but will lead to eventual violation
         --  messages being emitted multiple times.

         return;
      end if;

      if Line_Info.SCOs = null then
         --  No SCOs associated with this source line

         --  ??? Have a debug mode to warn if there is object code with
         --  this line ?

         --  Record that this line has been processed

         Line_Info.Coverage_Processed := True;

         return;
      end if;

      --  Examine each SCO associated with line

      for SCO of Line_Info.SCOs.all loop

         --  Skip the discarded SCOs and those not in a subprogram of interest

         if Kind (SCO) = Removed
           or else (not Subps_Of_Interest.Is_Empty
                    and then not In_Scope_Of_Interest (ST, SCO))
         then
            goto Next_SCO;
         end if;

         SCOs_Of_Line :
         declare
            SCO_State : Line_State := No_Code;
            SCI       : Source_Coverage_Info renames
              SCI_Vector.Reference (SCO);
         begin
            if Kind (SCO) = Statement then

               --  Statement coverage: line is covered if any associated
               --  statement is executed.

               if Ignore_SCO (SCO) then

                  --  They are neither covered nor not-covered, and need not be
                  --  reported as bona fide statements excluded from coverage
                  --  analysis either (see below case).

                  null;

               elsif Unit_Has_Code (SCO)
                 and then not Basic_Block_Has_Code (SCO)
               then
                  --  If a unit has any code at all, then a SCO is marked as
                  --  covered or not covered if there is code for it, or for a
                  --  subsequent SCO in the same basic block, else we leave it
                  --  as No_Code because it won't ever possibly be covered
                  --  anyway, so that a line ends up marked as No_Code only if
                  --  no code execution can ever cause it to be marked as
                  --  covered. However, if no code at all has been seen for the
                  --  entire unit, this means that the user probably omitted
                  --  required tests for that unit, so in that case we do not
                  --  enter this branch (because Unit_Has_Code is False), and
                  --  so we end up conservatively marking all statements in the
                  --  unit as not covered (on the basis that they might end up
                  --  having code, and be marked  as not covered, when the code
                  --  for the unit is actually loaded).
                  --
                  --  The distinction of the two cases of no code being present
                  --  for a SCO is that in the first case, the code for the
                  --  surrounding unit is present, so we know the compiler
                  --  definitely did not generate code for that SCO, whereas in
                  --  the second case the entire object for the unit was
                  --  generated by the compiler but then omitted at link time,
                  --  so we don't know for sure whether or not the compiler
                  --  emitted code for that SCO, so we conservatively assume
                  --  that it might have.
                  --
                  --  Stmt_SCO_Instrumented (SCO) returns false iff the unit
                  --  was instrumented, but not the particular SCO.  In that
                  --  case, report the SCO as undetermined coverage.

                  if not Stmt_SCO_Instrumented (SCO)
                    and then S_Kind (SCO) in Ada_Statement_Kind
                  then

                     SCO_State := Undetermined_Coverage;
                     Report_Coverage
                       (SCO, "was not instrumented", Kind => Undetermined_Cov);

                  elsif Report_If_Excluded (SCO) then
                     SCO_State := Not_Coverable;
                     Report_Exclusion (SCO, "has no object code");
                  end if;

               elsif SCI.Executed then
                  SCO_State := Covered;

               elsif SCI.Line_Executed then
                  if Is_Multistatement_Line (Line_Info.all) then

                     --  There is more than one statement SCO for this line.
                     --  When statements do not have full column numbers in
                     --  debug information, one cannot discriminate between
                     --  code for multiple statement SCOs on the same line.  We
                     --  therefore conservatively mark each SCO (and hence the
                     --  complete line) as partially, rather than fully,
                     --  covered, and we report a coverage violation on the
                     --  first SCO on the line.

                     if not Multiple_Statements_Reported then
                        Multiple_Statements_Reported := True;
                        Report_Violation
                          (SCO,
                           Msg =>
                             "^multiple statements on line, unable to "
                             & "establish full statement coverage");
                     end if;
                     SCO_State := Partially_Covered;

                  else
                     --  There is just one statement for this line, so we know
                     --  for certain that it has been executed.  Note:
                     --  Ensure_SCI above guarantees that SCI is an actual
                     --  specific SCI, not one of the default ones.

                     SCI.Executed := True;
                     SCO_State := Covered;
                  end if;

               else
                  SCO_State := Not_Covered;

                  --  Generate violation message on first line of SCO

                  if Line_Num = First_Sloc (SCO).L.Line then
                     Report_Violation (SCO, "not executed");
                  end if;
               end if;

               Update_Line_State (Line_Info, SCO, Stmt, SCO_State);

            elsif Kind (SCO) = Decision
              and then First_Sloc (SCO).L.Line /= Line_Num
            then
               --  For a decision that spans multiple lines, SCO state is
               --  computed for the first line, and then cached in the SCI and
               --  reused for subsequent lines.

               if Decision_Requires_Assertion_Coverage (SCO) then

                  SCO_State := SCI.State (ATC);
                  Update_Line_State (Line_Info, SCO, ATC, SCO_State);

                  if Assertion_Condition_Coverage_Enabled then
                     SCO_State := SCI.State (ATCC);
                     Update_Line_State (Line_Info, SCO, ATCC, SCO_State);
                  end if;
               else
                  if Enabled (Decision) then
                     SCO_State := SCI.State (Decision);
                     Update_Line_State (Line_Info, SCO, Decision, SCO_State);
                  end if;

                  if MCDC_Coverage_Enabled then
                     SCO_State := SCI.State (MCDC_Level);
                     Update_Line_State (Line_Info, SCO, MCDC_Level, SCO_State);
                  end if;
               end if;

            elsif Kind (SCO) = Decision
              and then ((Decision_Requires_Coverage (SCO)
                         and then (Enabled (Decision)
                                   or else MCDC_Coverage_Enabled))
                        or else Decision_Requires_Assertion_Coverage (SCO))
            then
               --  Compute decision coverage state for this decision. Note that
               --  the decision coverage information is also included in MC/DC
               --  coverage. The same goes for ATC and ATCC information.

               if Decision_Outcome (SCO) /= Unknown then

                  --  Case of a compile time known decision exclude from
                  --  coverage analysis.

                  if SCI.Evaluations.Length > 1 then

                     --  Case of a compile time known decision that was
                     --  consolidated with several checkpoints in which the
                     --  decision had different static conditions, but kept the
                     --  same outcome anyway.
                     --
                     --  In this case, we chose to report the violation,
                     --  because if you have a static decision in your code
                     --  that may change depending on the build context, then
                     --  you SHOULD get it covered

                     SCO_State := Not_Covered;
                     Report_Violation
                       (SCO,
                        "outcome "
                        & To_Boolean (Decision_Outcome (SCO))'Image
                        & " never exercised");
                     Update_Line_State (Line_Info, SCO, Decision, SCO_State);

                  elsif Report_If_Excluded (SCO) then
                     SCO_State := Not_Coverable;

                  --  Note: we do not report the exclusion of this SCO,
                  --  because if it is in an IF statement, then the IF
                  --  statement could be covered by back propagation, and it
                  --  would be confusing to see a line marked + in annotated
                  --  sources in conjunction with a message mentioning an
                  --  uncoverable construct in the report output.
                  end if;

               elsif SCI.Outcome_Taken = Both_Outcomes_Taken
                 or else SCI.Known_Outcome_Taken = Both_Outcomes_Taken
               then
                  --  Here for a decision whose both outcomes have been
                  --  exercised.

                  SCO_State := Covered;

               elsif SCI.Outcome_Taken /= No_Outcome_Taken
                 or else SCI.Known_Outcome_Taken /= No_Outcome_Taken
               then
                  --  Assertion coverage

                  if Decision_Requires_Assertion_Coverage (SCO) then
                     --  Contract coverage level "Assertion True Coverage"

                     --  Assertions are never supposed to be evaluated to
                     --  False. Therefore once they have been exercised and
                     --  found to be True, they are covered.

                     if SCI.Outcome_Taken (True)
                       or else SCI.Known_Outcome_Taken (True)
                     then
                        SCO_State := Covered;
                     else
                        SCO_State := Not_Covered;
                        Report_Violation (SCO, "outcome TRUE never exercised");
                     end if;

                  else
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
                             (SCO, "not exercised in both directions");

                        else
                           Report_Violation
                             (SCO,
                              "outcome "
                              & Missing_Outcome'Img
                              & " never exercised");
                        end if;
                     end;
                  end if;

               elsif Enclosing_Statement (SCO) = No_SCO_Id
                 or else (Basic_Block_Has_Code (Enclosing_Statement (SCO))
                          and then Stmt_SCO_Instrumented
                                     (Enclosing_Statement (SCO)))
               then
                  --  Similar to the above for statement coverage: a decision
                  --  that cannot ever be executed is reported as No_Code, not
                  --  Not_Covered. Note: the enclosing statement may be covered
                  --  even though the decision has never been evaluated (case
                  --  e.g. of an exception being raised before any outcome is
                  --  reached, or of a condition for which we fail to identify
                  --  the corresponding conditional branch instruction). We
                  --  report the coverage failure for the decision in that case
                  --  only; if the statement was not executed, we report only
                  --  the statement failure. If there is no enclosing
                  --  statement, or there is an ignored SCO (e.g. case of a
                  --  pragma that generates freestanding decisions) then we
                  --  always report the coverage status.

                  declare
                     S_SCO : constant SCO_Id := Enclosing_Statement (SCO);
                  begin
                     SCO_State := Not_Covered;
                     if S_SCO = No_SCO_Id
                       or else SCI_Vector (S_SCO).Executed
                       or else SCI_Vector (S_SCO).Line_Executed
                       or else Ignore_SCO (S_SCO)
                     then
                        --  Decision_SCO_Instrumented (SCO) is False iff the
                        --  unit was instrumented, but not that particular
                        --  decision.

                        if not Decision_SCO_Instrumented (SCO) then
                           SCO_State := Undetermined_Coverage;
                           Report_Coverage
                             (SCO,
                              "was not instrumented for decision coverage",
                              Kind => Undetermined_Cov);

                        --  If the decision has not conditional branches at
                        --  all, mark it as uncoverable and report it. We
                        --  should already have leveraged back-propagation, at
                        --  this point, so a decision with no outcomes taken is
                        --  either never evaluated, or has no branches to track
                        --  the evaluation.

                        elsif not Decision_Has_Influence (SCO) then
                           if Report_If_Excluded (SCO) then
                              SCO_State := Not_Coverable;
                              Report_Exclusion
                                (SCO, Msg => "has no object code");
                           else
                              --  Mark the SCO as no code if not reporting it,
                              --  to avoid having a violation in the reports.

                              SCO_State := No_Code;
                           end if;
                        else
                           Report_Violation (SCO, "never evaluated");
                        end if;
                     end if;
                  end;
               end if;

               --  Update the state of the line for all enabled source coverage
               --  levels.

               if Decision_Requires_Assertion_Coverage (SCO) then

                  --  If the SCO is in an assertion, update its state for the
                  --  relevant assertion coverage levels...

                  Update_Line_State (Line_Info, SCO, ATC, SCO_State);

                  if Enabled (ATCC) then
                     Compute_Condition_Level_Line_State
                       (SCO, SCO_State, Line_Info, SCI, ATCC);
                  end if;

               elsif Decision_Requires_Coverage (SCO) then

                  --  ...otherwise update the SCO state for the regular source
                  --  coverage levels.

                  --  Update the SCO state for decision level

                  Update_Line_State (Line_Info, SCO, Decision, SCO_State);

                  --  Compute and update the SCO state for MCDC level

                  if MCDC_Coverage_Enabled
                    and then not Decision_Requires_Assertion_Coverage (SCO)
                  then
                     Compute_Condition_Level_Line_State
                       (SCO, SCO_State, Line_Info, SCI, MCDC_Level);
                  end if;
               end if;

            --  For fun_call coverage, only compute the SCO state on the first
            --  line, and re-use the SCI cache to set the line state on
            --  subprograms or calls spanning multiple lines.

            elsif Kind (SCO) in Fun_Call_SCO_Kind
              and then Enabled (Fun_Call)
              and then First_Sloc (SCO).L.Line = Line_Num
            then
               if not Fun_Call_SCO_Instrumented (SCO) then
                  SCO_State := Undetermined_Coverage;
                  Report_Coverage
                    (SCO, "was not instrumented", Kind => Undetermined_Cov);

               --  For call statements, we only need to rely on the state of
               --  the enclosing statement.

               elsif Is_Call_Stmt (SCO) then
                  declare
                     S_SCO : constant SCO_Id := Enclosing_Statement (SCO);
                  begin
                     SCO_State :=
                       (if S_SCO /= No_SCO_Id
                          and then SCI_Vector (S_SCO).Executed
                        then Covered
                        else Not_Covered);
                  end;
               else
                  SCO_State :=
                    (if SCI.Fun_Call_Executed then Covered else Not_Covered);
               end if;

               if SCO_State = Not_Covered then
                  Report_Violation (SCO, "not executed");
               end if;

               Update_Line_State (Line_Info, SCO, Fun_Call, SCO_State);

            elsif Kind (SCO) in Fun_Call_SCO_Kind and then Enabled (Fun_Call)
            then
               SCO_State := SCI.State (Fun_Call);
               Update_Line_State (Line_Info, SCO, Fun_Call, SCO_State);
            elsif Kind (SCO) = Guarded_Expr and then Enabled (GExpr) then
               if not GExpr_SCO_Instrumented (SCO) then
                  SCO_State := Undetermined_Coverage;
                  Report_Coverage
                    (SCO, "was not instrumented", Kind => Undetermined_Cov);
               elsif SCI.GExpr_Executed then
                  SCO_State := Covered;
               else
                  SCO_State := Not_Covered;

                  --  Report a violation on the first line of the SCO to avoid
                  --  duplicating violations in the report.

                  if Line_Num = First_Sloc (SCO).L.Line then
                     Report_Violation (SCO, "not executed");
                  end if;
               end if;

               Update_Line_State (Line_Info, SCO, GExpr, SCO_State);
            end if;
         end SCOs_Of_Line;

         <<Next_SCO>>
         null;
      end loop;
      --  Record that this line has been processed

      Line_Info.Coverage_Processed := True;
   end Compute_Line_State;

   ------------------------
   -- Compute_MCDC_State --
   ------------------------

   function Compute_MCDC_State
     (SCO : SCO_Id; SCI : Source_Coverage_Info) return Line_State
   is
      use Evaluation_Sets;

      Indep : array (0 .. Last_Cond_Index (SCO)) of Boolean :=
        (others => False);
      --  Indicates whether independent influence of each condition has been
      --  shown.

      Influent_Condition : Any_Condition_Index;
      --  Condition whose independent influence is shown by the vector pair
      --  being considered.

      package Condition_Set is new
        Ada.Containers.Ordered_Sets (Any_Condition_Index);

      type Cond_Set_Access is access Condition_Set.Set;

      procedure Free is new
        Ada.Unchecked_Deallocation (Condition_Set.Set, Cond_Set_Access);

      package Evaluation_to_Cond_Set_Map is new
        Ada.Containers.Ordered_Maps
          (Key_Type     => Evaluation,
           Element_Type => Cond_Set_Access);

      Eval_Cond_Set_Map : Evaluation_to_Cond_Set_Map.Map;
      --  Map between evaluations and the conditions for which they are in a
      --  pair demonstrating independent influence.

      SCO_State : Line_State := No_Code;

      E1, E2 : Cursor;

      Last_Cond_No_Pair : Condition_Index;
      --  Condition index for which to dump the message. As the DHTML output
      --  only displays the last registered violation for a line, we must
      --  emit the evaluation vectors on the last violation to report.

      function Emit_Evaluation_Vector_Message return String;
      --  List all the evaluation vectors, along with the conditions for which
      --  they are in a pair demonstrating independent influence.

      ------------------------------------
      -- Emit_Evaluation_Vector_Message --
      ------------------------------------

      function Emit_Evaluation_Vector_Message return String is
         Msg         : Unbounded_String;
         No_Pair_Msg : Unbounded_String;
      begin
         Msg := +"Decision of the form " & Expression_Image (SCO) & ASCII.LF;
         Append (Msg, "Evaluation vectors found:" & ASCII.LF);

         for Eval of SCI.Evaluations loop
            declare
               Cond_Set : Condition_Set.Set renames
                 Eval_Cond_Set_Map.Element (Eval).all;
            begin
               if not Cond_Set.Is_Empty then
                  --  This evaluation is in a pair
                  Append (Msg, "    " & Image (Eval) & "  In a pair for ");
                  for Cond of Cond_Set loop
                     Append (Msg, 'C' & Img (Integer (Cond)));
                     if Cond < Cond_Set.Last_Element then
                        Append (Msg, ", ");
                     end if;
                  end loop;
                  Append (Msg, ASCII.LF);
               else
                  Append
                    (No_Pair_Msg,
                     "    "
                     & Image (Eval)
                     & "  Not part of any pair"
                     & ASCII.LF);
               end if;
            end;
         end loop;

         return +(Msg & No_Pair_Msg);
      end Emit_Evaluation_Vector_Message;

      --  Start of processing for Compute_MCDC_State

   begin
      for Cur of SCI.Evaluations loop
         Eval_Cond_Set_Map.Include (Cur, new Condition_Set.Set);
      end loop;

      E1 := SCI.Evaluations.First;
      while E1 /= No_Element loop
         E2 := Next (E1);
         while E2 /= No_Element loop
            Influent_Condition :=
              Is_MC_DC_Pair
                (Element (E1),
                 Element (E2),
                 Unique_Cause => MCDC_Level = UC_MCDC);
            --  Record and report the first eval pair that shows independent
            --  influence of Influent_Condition.

            if (Switches.Show_MCDC_Vectors
                or else Switches.Show_Condition_Vectors)
              and then Influent_Condition /= No_Condition_Index
            then
               Eval_Cond_Set_Map.Element (Element (E1)).Include
                 (Influent_Condition);
               Eval_Cond_Set_Map.Element (Element (E2)).Include
                 (Influent_Condition);
            end if;

            if Influent_Condition /= No_Condition_Index
              and then not Indep (Influent_Condition)
            then

               Indep (Influent_Condition) := True;
               Report
                 ("C"
                  & Img (Integer (Influent_Condition))
                  & " independent influence shown by eval pair: "
                  & Image (Element (E1))
                  & " / "
                  & Image (Element (E2)),
                  Sloc => First_Sloc (SCO),
                  SCO  => Condition (SCO, Influent_Condition),
                  Kind => Notice);
            end if;
            Next (E2);
         end loop;
         Next (E1);
      end loop;

      --  Find the last condition which has a violation

      if Switches.Show_MCDC_Vectors or else Switches.Show_Condition_Vectors
      then
         for J in reverse Indep'Range loop
            if not Indep (J) then
               Last_Cond_No_Pair := J;
               exit;
            end if;
         end loop;
      end if;

      --  Iterate over conditions and report

      for J in Indep'Range loop
         if not Indep (J) then
            Update_State
              (SCO_State, Condition (SCO, J), MCDC_Level, Not_Covered);
            Report_Violation
              (SCO => Condition (SCO, J),
               Msg => "has no independent influence pair, MC/DC not achieved");

            if (Switches.Show_MCDC_Vectors
                or else Switches.Show_Condition_Vectors)
              and then J = Last_Cond_No_Pair
            then

               --  We want the MC/DC vector to be displayed with the MC/DC
               --  violations, after the last MC/DC violation of the decision.
               --
               --  As only messages attached to SCO conditions appear in the
               --  "MCDC COVERAGE" report section, we thus need to attach this
               --  message to the SCO of the last condition (to have it be
               --  displayed last).
               --
               --  Note that the sloc for the message will still be the
               --  decision sloc, because Report_Coverage initializes the
               --  message sloc to the one of the enclosing decision when the
               --  SCO is a condition.

               Report_Coverage
                 (SCO  => Condition (SCO, J),
                  Msg  => Emit_Evaluation_Vector_Message,
                  Kind => Info);
            end if;

         else
            Update_State (SCO_State, Condition (SCO, J), MCDC_Level, Covered);
         end if;
      end loop;

      for Cur of SCI.Evaluations loop
         Free (Eval_Cond_Set_Map (Cur));
      end loop;

      --  If we have degraded origins for SCO but we computed MC/DC coverage
      --  state then this means that DC is achieved, and so MC/DC must be
      --  achieved as well (because this is a single condition decision).

      pragma Assert (SCO_State = Covered or else not Degraded_Origins (SCO));
      return SCO_State;
   end Compute_MCDC_State;

   ------------------------
   -- Compute_ATCC_State --
   ------------------------

   function Compute_ATCC_State
     (SCO : SCO_Id; SCI : Source_Coverage_Info) return Line_State
   is

      function Emit_Evaluation_Vector_Message return String;
      --  List all the evaluation vectors

      ------------------------------------
      -- Emit_Evaluation_Vector_Message --
      ------------------------------------

      function Emit_Evaluation_Vector_Message return String is
         Msg : Unbounded_String;
      begin
         Msg := +"Decision of the form " & Expression_Image (SCO) & ASCII.LF;
         Append (Msg, "Evaluation vectors found:" & ASCII.LF);

         for Eval of SCI.Evaluations loop
            Append (Msg, "    " & Image (Eval) & ASCII.LF);
         end loop;

         return +Msg;
      end Emit_Evaluation_Vector_Message;

      SCO_State : Line_State := No_Code;

      Last_Cond_Not_Evaluated : Condition_Index;
      --  Condition index for which to dump the message. As the DHTML output
      --  only displays the last registered violation for a line, we must
      --  emit the evaluation vectors of the last reported violation.

      Last_Cond_Idx : constant Condition_Index := Last_Cond_Index (SCO);
      --  Index of last condition in decision

      type Condition_Evaluated_Array is
        array (Condition_Index'First .. Last_Cond_Idx) of Boolean;

      Condition_Evaluated : Condition_Evaluated_Array := (others => False);

      --  Start of processing for Compute_ATCC_State

   begin

      --  Record if a condition has been evaluated during the overall
      --  evaluation to true of its corresponding decision.

      for Eval of SCI.Evaluations loop
         if Eval.Outcome = True then
            for I in Condition_Evaluated_Array'Range loop
               Condition_Evaluated (I) :=
                 Condition_Evaluated (I) or else Eval.Values (I) /= Unknown;
            end loop;
         end if;
      end loop;

      --  Find the last condition which has a violation

      for I in reverse Condition_Evaluated'Range loop
         if not Condition_Evaluated (I) then
            Last_Cond_Not_Evaluated := I;
            exit;
         end if;
      end loop;

      --  Iterate over conditions and report

      for I in Condition_Evaluated_Array'Range loop
         if not Condition_Evaluated (I) then
            Update_State (SCO_State, Condition (SCO, I), ATCC, Not_Covered);
            Report_Violation
              (SCO => Condition (SCO, I),
               Msg =>
                 "was never evaluated during an evaluation of the "
                 & "decision to True, ATCC not achieved");

            if Switches.Show_Condition_Vectors
              and then I = Last_Cond_Not_Evaluated
            then
               --  In much the same way is in Compute_MCDC_State, display the
               --  vector with ATCC violations and after the last ATCC
               --  violation of the decision.

               Report_Coverage
                 (SCO  => Condition (SCO, I),
                  Msg  => Emit_Evaluation_Vector_Message,
                  Kind => Info);
            end if;
         else
            Update_State (SCO_State, Condition (SCO, I), ATCC, Covered);
         end if;
      end loop;

      --  If we have degraded origins for SCO but we computed ATCC coverage
      --  state then this means that ATC is achieved, and so ATCC must be
      --  achieved as well (because this is a single condition decision).

      pragma Assert (SCO_State = Covered or else not Degraded_Origins (SCO));
      return SCO_State;
   end Compute_ATCC_State;

   -----------------------------
   -- Compute_Source_Coverage --
   -----------------------------

   procedure Compute_Source_Coverage
     (Subp_Key : Subprogram_Key; Subp_Info : Subprogram_Info; T : Trace_Entry)
   is
      pragma Unreferenced (Subp_Key);

      use type Pc_Type;

      Exe      : Exe_File_Acc renames Subp_Info.Exec;
      PC       : Pc_Type;
      Insn_Len : Natural;

      procedure Discharge_SCO
        (SCO                 : SCO_Id;
         Sloc                : Source_Location;
         Empty_Range         : Boolean;
         Multistatement_Line : Boolean);
      --  Discharge the coverage obligation denoted by SCO using the current
      --  execution trace for an instruction at PC, with the given sloc.
      --  Empty_Range is True if the sloc for PC that is associated with SCO
      --  has an empty PC range.

      -------------------
      -- Discharge_SCO --
      -------------------

      procedure Discharge_SCO
        (SCO                 : SCO_Id;
         Sloc                : Source_Location;
         Empty_Range         : Boolean;
         Multistatement_Line : Boolean)
      is
         Propagating, No_Propagation : Boolean;

         S_SCO   : SCO_Id;
         Dom_SCO : SCO_Id;
         Dom_Val : Boolean;

         Precise : constant Boolean := Sloc.L.Column /= 0;

         Line_Executed : Boolean;
         --  Set True if we are discharging from a trace with imprecise sloc
         --  that has line information only (column unknown).
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

               Cur_SCI : Source_Coverage_Info renames
                 SCI_Vector.Constant_Reference (S_SCO);
            begin
               Line_Executed :=
                 not Precise
                 and then Sloc.Source_File = S_SCO_First.Source_File
                 and then Sloc.L.Line
                          in S_SCO_First.L.Line .. S_SCO_Last.L.Line;

               exit when
                 Cur_SCI.Executed
                 or else (Line_Executed and Cur_SCI.Line_Executed);
            end;

            --  For pragma Pre/Postcondition, no propagation: the statement
            --  is never marked as executed by propagation, and marking it
            --  does not cause propagation to other statements. We also
            --  cannot propagate from an imprecise sloc if the line has
            --  multiple statements.

            No_Propagation :=
              Is_Pragma_Pre_Post_Condition (S_SCO)
              or else (not Precise and then Multistatement_Line);

            if not (Propagating and No_Propagation) then
               --  Mark S_SCO as executed

               Report
                 ((if Line_Executed then "line " else "")
                  & "executed"
                  & (if Propagating then " (propagating)" else ""),
                  SCO  => S_SCO,
                  Exe  => Exe,
                  PC   => PC,
                  Kind => Notice);

               declare
                  SCI : Source_Coverage_Info renames
                    SCI_Vector.Reference (S_SCO);
               begin
                  if Line_Executed then
                     SCI.Line_Executed := True;
                  else
                     SCI.Executed := True;
                  end if;
               end;
            end if;

            exit when not Propagating and No_Propagation;

            --  Propagate back to beginning of basic block, and possibly to
            --  upper decisions.

            Propagating := True;

            Dominant (S_SCO, Dom_SCO, Dom_Val);
            if Dom_SCO /= No_SCO_Id
              and then Kind (Dom_SCO) = Decision
              and then not SCI_Vector (Dom_SCO).Known_Outcome_Taken (Dom_Val)
            then
               Report
                 ("outcome " & Dom_Val'Img & " taken (propagating)",
                  SCO  => Dom_SCO,
                  Exe  => Exe,
                  PC   => PC,
                  Kind => Notice);

               SCI_Vector.Reference (Dom_SCO).Known_Outcome_Taken (Dom_Val) :=
                 True;
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

         Process_Conditional_Branch :
         declare
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

               procedure Set_Outcome_Taken (SCI : in out Source_Coverage_Info);
               --  Mark as taken the decision outcome corresponding to CBE

               -----------------------
               -- Set_Outcome_Taken --
               -----------------------

               procedure Set_Outcome_Taken (SCI : in out Source_Coverage_Info)
               is
                  use Condition_Evaluation_Vectors;

                  Eval : Evaluation;

                  Inferred_Values : Vector;
                  --  Inferred condition values, for the case of a D_SCO with
                  --  no condition reachable through multile paths.

                  function Pop_Eval return Evaluation;
                  --  Pop the top element from the evaluation stack

                  --------------
                  -- Pop_Eval --
                  --------------

                  function Pop_Eval return Evaluation is
                  begin
                     return
                        ES_Top : constant Evaluation :=
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
                    ("outcome "
                     & CBE.Outcome'Img
                     & (if Degraded_Origins (D_SCO) then " (degraded)" else "")
                     & " taken by "
                     & E'Img,
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

                  if Has_Multipath_Condition (D_SCO) then
                     Eval := Pop_Eval;

                  else
                     --  Each condition is reachable through only one path,
                     --  and we can infer the complete condition vector from
                     --  just the last condition tested.

                     Inferred_Values := Infer_Values (SCO);
                     Inferred_Values.Append (CBE.Origin);

                     if Full_History_Trace.Is_Active then
                        --  In full history debugging mode, we record the
                        --  evaluation history and check it against the
                        --  inferred vector.

                        Eval := Pop_Eval;

                        if Eval.Values /= Inferred_Values then
                           Report_Violation
                             (D_SCO,
                              "^inferred values mismatch: expected "
                              & Image (Inferred_Values)
                              & ", got "
                              & Image (Eval.Values));
                        end if;

                     else
                        --  Reconstruct complete evaluation information from
                        --  just the outcome.

                        Eval.Decision := D_SCO;
                        Eval.Values := Inferred_Values;
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
                    (Exe,
                     PC,
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

                        Report
                          (Exe,
                           PC,
                           "edge "
                           & E'Img
                           & " raised an exception, "
                           & "abandoning evaluation",
                           Kind => Notice);

                        if Has_Multipath_Condition (D_SCO)
                          or else Full_History_Trace.Is_Active
                        then
                           Evaluation_Stack.Delete_Last;
                        end if;

                     else
                        Report
                          (Exe,
                           PC,
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
                     Set_Outcome_Taken (SCI_Vector.Reference (D_SCO));
                  end if;
               end if;
            end Edge_Taken;

            use type Interfaces.Unsigned_8;

            --  Start of processing for Process_Conditional_Branch

         begin
            Report
              (Exe,
               PC,
               "processing cond branch trace op"
               & T.Op'Img
               & " ("
               & (case T.Op and 3 is
                    when 1      => "branch",
                    when 2      => "fallthrough",
                    when 3      => "both",
                    when others => "???")
               & " taken)",
               Kind => Notice);

            case T.Op and 3 is
               when 1      =>
                  Edge_Taken (Branch);

               when 2      =>
                  Edge_Taken (Fallthrough);

               when 3      =>
                  if MCDC_Coverage_Enabled
                    and then (Has_Multipath_Condition (D_SCO)
                              or else Full_History_Trace.Is_Active)
                  then
                     --  For MC/DC we need full historical traces, not just
                     --  accumulated traces.

                     Report
                       (Exe,
                        PC,
                        "missing full traces of conditional branch for MC/DC");
                  else
                     Edge_Taken (Branch);
                     Edge_Taken (Fallthrough);
                  end if;

               when others =>
                  Report
                    (Exe,
                     PC,
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
      --  Iterate over trace for this routine

      PC := T.First + Subp_Info.Offset;

      Trace_Insns :
      while Iterate_Over_Insns
              (I_Ranges, Cache, T.Last + Subp_Info.Offset, PC, Insn_Set)
      loop
         Insn_Len :=
           Disa_For_Machine (Machine, Insn_Set).Get_Insn_Length
             (Slice (Subp_Info.Insns, PC, Subp_Info.Insns.Last));

         --  Discharge each SCO for source locations associated with this
         --  instruction.

         declare
            Routine    : constant Address_Info_Acc :=
              Get_Address_Info
                (Exec => Subp_Info.Exec.all,
                 Kind => Subprogram_Addresses,
                 PC   => Subp_Info.Insns.First);
            SL         : constant Source_Locations :=
              Get_Slocs (Routine.Lines, PC);
            SCOs       : access SCO_Id_Array;
            Single_SCO : aliased SCO_Id_Array := (0 => No_SCO_Id);

            Multistatement_Line : Boolean;
            --  Set True if discharging from an imprecise sloc on a line with
            --  multiple statement SCOs.

         begin
            for J in SL'Range loop
               Multistatement_Line := False;
               Single_SCO (Single_SCO'First) := No_SCO_Id;
               SCOs := null;

               if SL (J).L.Column = 0 then
                  declare
                     LI : constant Line_Info_Access := Get_Line (SL (J));
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

                  Single_SCO (Single_SCO'First) := Sloc_To_SCO (SL (J));
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
                        Sloc                => SL (J),
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

   ----------------------------
   -- Refine_Source_Coverage --
   ----------------------------

   procedure Refine_Source_Coverage is
      --  Refine the outcome taken information. See the documentation of
      --  Process_SCO below.

      procedure Process_SCO (SCO : SCO_Id);
      --  Infer that an outcome has been taken for the following specific case:
      --    * The given SCO is a statement SCO#1 dominated by a decision SCO#2
      --      valuation V.
      --    * The decision SCO#2 has an Outcome_Taken (*) set.
      --
      --  This configuration allows us to conclude that the "not V" outcome of
      --  SCO#2 is covered: set Known_Outcome_Taken accordingly.

      -----------------
      -- Process_SCO --
      -----------------

      procedure Process_SCO (SCO : SCO_Id) is
         Dom_SCO : SCO_Id;
         Dom_Val : Boolean;
      begin
         --  If this is not a statement SCO, it does not have dominant
         --  information: skip it.

         if Kind (SCO) /= Statement then
            return;
         end if;

         --  Also skip it if it has no associated SCI, or if this is not a
         --  coverable SCO.

         declare
            SCI : Source_Coverage_Info renames
              SCI_Vector.Constant_Reference (SCO);
         begin
            if not SCI.Basic_Block_Has_Code or else SCI.Executed then
               return;
            end if;
         end;

         Dominant (SCO, Dom_SCO, Dom_Val);
         if Dom_SCO = No_SCO_Id or else Kind (Dom_SCO) /= Decision then
            return;
         end if;

         declare
            SCI : Source_Coverage_Info renames
              SCI_Vector.Constant_Reference (Dom_SCO);
         begin
            if SCI.Outcome_Taken (True) or else SCI.Outcome_Taken (False) then
               declare
                  SCI : Source_Coverage_Info renames
                    SCI_Vector.Reference (Dom_SCO);
               begin
                  SCI.Known_Outcome_Taken (not Dom_Val) := True;
               end;
            end if;
         end;
      end Process_SCO;
   begin
      SC_Obligations.Iterate (Process_SCO'Access);
   end Refine_Source_Coverage;

   -----------------------------
   -- Compute_Source_Coverage --
   -----------------------------

   procedure Compute_Source_Coverage
     (Filename                : String;
      Fingerprint             : SC_Obligations.Fingerprint_Type;
      CU_Name                 : Compilation_Unit_Part;
      Bit_Maps_Fingerprint    : SC_Obligations.Fingerprint_Type;
      Annotations_Fingerprint : SC_Obligations.Fingerprint_Type;
      Stmt_Buffer             : Coverage_Buffer;
      Decision_Buffer         : Coverage_Buffer;
      MCDC_Buffer             : Coverage_Buffer)
   is
      CU : CU_Id;
      BM : CU_Bit_Maps;
      ST : Scope_Traversal_Type;

      procedure Set_Executed (SCI : in out Source_Coverage_Info);
      --  Mark SCI as executed

      function Part_Image (Part : GPR2.Valid_Unit_Kind) return String;
      --  Helper to include Part in an error message

      function Unit_Image return String
      is (case CU_Name.Language_Kind is
            when Unit_Based_Language =>
              (Part_Image (CU_Name.Part) & " " & To_Ada (CU_Name.Unit)),
            when File_Based_Language => +CU_Name.Filename);
      --  Helper to refer to the instrumented unit in an error message

      procedure Update_SCI
        (SCO     : SCO_Id;
         Process : access procedure (SCI : in out Source_Coverage_Info));
      --  Execute Process on the SCI for the given SCO

      ------------------
      -- Set_Executed --
      ------------------

      procedure Set_Executed (SCI : in out Source_Coverage_Info) is
      begin
         if SCI.Kind = Statement then
            SCI.Executed := True;
         elsif SCI.Kind in Fun_Call_SCO_Kind then
            SCI.Fun_Call_Executed := True;
         elsif SCI.Kind = Guarded_Expr then
            SCI.GExpr_Executed := True;
         end if;

      end Set_Executed;

      ----------------
      -- Part_Image --
      ----------------

      function Part_Image (Part : GPR2.Valid_Unit_Kind) return String is
      begin
         return
           (case Part is
              when GPR2.S_Body     => "body of",
              when GPR2.S_Spec     => "spec of",
              when GPR2.S_Separate => "separate");
      end Part_Image;

      ----------------
      -- Update_SCI --
      ----------------

      procedure Update_SCI
        (SCO     : SCO_Id;
         Process : access procedure (SCI : in out Source_Coverage_Info)) is
      begin
         if In_Scope_Of_Interest (ST, SCO) then
            Process.all (SCI_Vector.Reference (SCO));
         end if;
      end Update_SCI;

      --  Start of processing for Compute_Source_Coverage

   begin
      Misc_Trace.Trace ("processing traces for unit " & Unit_Image);

      CU := Find_Instrumented_Unit (CU_Name);

      if CU = No_CU_Id then

         --  When using a single instrumented program to compute separate
         --  coverage for all units (common in unit testing), it is legitimate
         --  to process source trace files that contain entries relating to
         --  units not of interest. So in this case, do not even warn about it:
         --  just log the fact that we skip this trace entry in the verbose
         --  about, just in case.

         Misc_Trace.Trace
           ("discarding source trace entry for unknown instrumented unit: "
            & Unit_Image);
         return;

      elsif Provider (CU) /= Instrumenter then

         --  We loaded compiler-generated SCOs for this unit before processing
         --  its source trace buffer, so we have inconsistent information. Just
         --  ignore this coverage information and proceed.

         Warn
           ("inconsistent coverage method, ignoring coverage information"
            & " for "
            & Unit_Image);
         return;
      end if;

      --  Sanity check that Fingerprint is consistent with what the
      --  instrumenter recorded in the CU info.

      if not Has_Fingerprint (CU, Fingerprint)
        or else Bit_Maps_Fingerprint
                /= SC_Obligations.Bit_Maps_Fingerprint (CU, Fingerprint)
        or else Annotations_Fingerprint
                /= SC_Obligations.Annotations_Fingerprint (CU, Fingerprint)
      then
         Warn
           ("traces for "
            & Unit_Image
            & " (from "
            & Filename
            & ") are"
            & " inconsistent with the corresponding Source Instrumentation"
            & " Data");
         return;
      end if;

      --  Mark unit as present in closure

      Set_Unit_Has_Code (CU);

      --  Discharge SCOs based on source traces

      BM := Bit_Maps (CU, Fingerprint);

      ST := Scope_Traversal (CU);
      for J in Stmt_Buffer'Range loop

         --  If bit is set, statement has been executed

         if Stmt_Buffer (J) then
            Update_SCI (BM.Statement_Bits (J), Set_Executed'Access);
         end if;
      end loop;

      --  If there are blocks, discharge all of the blocks statement SCOs
      --  when one of them (which actually is the last) is covered.

      declare
         Stmt_Blocks : SCO_Id_Vector_Vector renames Blocks (CU, Fingerprint);
         Block_Index : Positive;
      begin
         if not Stmt_Blocks.Is_Empty then
            Block_Index := Stmt_Blocks.First_Index;
            for J in Stmt_Buffer'Range loop

               --  Skip buffer bits corresponding to fun_call SCOs
               --
               --  TODO??? Investigate if having a separate buffer for calls
               --  makes more sense, and/or yields a cleaner implementation.

               if Kind (BM.Statement_Bits (J)) not in Statement then
                  goto Continue;
               end if;

               if Stmt_Buffer (J) then
                  for SCO of Stmt_Blocks.Element (Block_Index) loop
                     Update_SCI (SCO => SCO, Process => Set_Executed'Access);
                  end loop;
               end if;
               Block_Index := Block_Index + 1;
               <<Continue>>
            end loop;
         end if;
      end;

      for J in Decision_Buffer'Range loop
         if Decision_Buffer (J) then
            declare
               Outcome_Info : Decision_Bit_Info renames BM.Decision_Bits (J);

               procedure Set_Known_Outcome_Taken
                 (SCI : in out Source_Coverage_Info);
               --  Mark Outcome_Info.Outcome as taken

               -----------------------------
               -- Set_Known_Outcome_Taken --
               -----------------------------

               procedure Set_Known_Outcome_Taken
                 (SCI : in out Source_Coverage_Info) is
               begin
                  SCI.Known_Outcome_Taken (Outcome_Info.Outcome) := True;
               end Set_Known_Outcome_Taken;

            begin
               Update_SCI (Outcome_Info.D_SCO, Set_Known_Outcome_Taken'Access);
            end;
         end if;
      end loop;

      for J in MCDC_Buffer'Range loop
         if MCDC_Buffer (J) then
            declare
               MCDC_Info   : MCDC_Bit_Info renames BM.MCDC_Bits (J);
               Outcome     : Boolean;
               Cond_Values : constant Condition_Values_Array :=
                 Condition_Values
                   (MCDC_Info.D_SCO, MCDC_Info.Path_Index, Outcome);

               procedure Add_Evaluation (SCI : in out Source_Coverage_Info);
               --  Add evaluation to SCI

               --------------------
               -- Add_Evaluation --
               --------------------

               procedure Add_Evaluation (SCI : in out Source_Coverage_Info) is
               begin
                  SCI.Evaluations.Include
                    ((Decision       => MCDC_Info.D_SCO,
                      Values         => To_Vector (Cond_Values),
                      Outcome        => To_Tristate (Outcome),
                      Next_Condition => No_Condition_Index));
               end Add_Evaluation;

            begin
               Update_SCI (MCDC_Info.D_SCO, Add_Evaluation'Access);
            end;
         end if;
      end loop;
   end Compute_Source_Coverage;

   -------------------------
   -- Condition_Evaluated --
   -------------------------

   procedure Condition_Evaluated
     (Exe : Exe_File_Acc; PC : Pc_Type; C_SCO : SCO_Id; C_Value : Boolean)
   is
      D_SCO : constant SCO_Id := Enclosing_Decision (C_SCO);

      function In_Current_Evaluation return Boolean;
      --  True when this evaluation is the expected next condition in the
      --  evaluation at the top of the evaluation stack.

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
            return
              ES_Top.Decision = D_SCO
              and then ES_Top.Next_Condition = Index (C_SCO);
         end;
      end In_Current_Evaluation;

      --  Start of processing for Condition_Evaluated

   begin
      --  No-op unless doing MC/DC analysis

      if not MCDC_Coverage_Enabled then
         return;
      end if;

      --  No-op if decision has no multi-path condition and not debugging

      if not (Has_Multipath_Condition (D_SCO)
              or else Full_History_Trace.Is_Active)
      then
         return;
      end if;

      if not In_Current_Evaluation then
         Evaluation_Stack.Append
           (Evaluation'
              (Decision       => D_SCO,
               Next_Condition => 0,
               Outcome        => Unknown,
               others         => <>));
      end if;

      if not In_Current_Evaluation then
         Report
           (Exe,
            PC,
            "unexpected condition"
            & Index (C_SCO)'Img
            & " in trace, expected"
            & Evaluation_Stack.Last_Element.Next_Condition'Img,
            Kind => Warning);
      end if;

      --  Record the value of condition C_SCO in the current evaluation, and
      --  set the next expected condition.

      declare
         ES_Top     : Evaluation renames
           Evaluation_Stack.Reference (Evaluation_Stack.Last_Index);
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
      end;
   end Condition_Evaluated;

   --------------------------------
   -- Decision_Requires_Coverage --
   --------------------------------

   function Decision_Requires_Coverage (SCO : SCO_Id) return Boolean is
   begin
      pragma Assert (Kind (SCO) = Decision);

      return
        Switches.All_Decisions
        or else not Is_Expression (SCO)
        or else (MCDC_Coverage_Enabled and then Last_Cond_Index (SCO) > 0);
   end Decision_Requires_Coverage;

   ------------------------------------------
   -- Decision_Requires_Assertion_Coverage --
   ------------------------------------------

   function Decision_Requires_Assertion_Coverage (SCO : SCO_Id) return Boolean
   is
   begin
      pragma Assert (Kind (SCO) in Decision | Condition);

      return
        Currently_Accepted_Trace_Kind /= Binary_Trace_File
        and then Assertion_Coverage_Enabled
        and then Is_Assertion_To_Cover (SCO);
   end Decision_Requires_Assertion_Coverage;

   --------------------
   -- Get_Line_State --
   --------------------

   function Get_Line_State
     (SCO : SCO_Id; Level : Coverage_Level) return SCO_State is
   begin
      return SCI_Vector (SCO).State (Level);
   end Get_Line_State;

   --------------------
   -- Initialize_SCI --
   --------------------

   procedure Initialize_SCI is
      Last_SCO : constant SCO_Id := SC_Obligations.Last_SCO;
      Last_SCI : constant SCO_Id := SCI_Vector.Last_Index;
   begin
      if Last_SCO > SCI_Vector.Last_Index then
         SCI_Vector.Reserve_Capacity (Ada.Containers.Count_Type (Last_SCO));
         for SCO in Last_SCI + 1 .. Last_SCO loop
            declare
               SCI : Source_Coverage_Info (Kind (SCO));
            begin
               SCI_Vector.Append (SCI);
            end;
         end loop;

         --  Make sure the above logic got index computations correct: we
         --  should have exactly one SCI per SCO.

         pragma Assert (SCI_Vector.Last_Index = Last_SCO);
      end if;
   end Initialize_SCI;

   ----------------------------
   -- Initialize_SCI_For_SID --
   ----------------------------

   procedure Initialize_SCI_For_SID
     (CU : CU_Id; SCOs_Fingerprint : SC_Obligations.Fingerprint_Type)
   is

      procedure Set_Has_Code (SCO : SCO_Id);
      --  If SCO is a statement, mark it as having code

      ------------------
      -- Set_Has_Code --
      ------------------

      procedure Set_Has_Code (SCO : SCO_Id) is
         SCI : Source_Coverage_Info renames SCI_Vector.Reference (SCO);
      begin
         if SCI.Kind = Statement then
            SCI.Basic_Block_Has_Code := True;
         end if;
      end Set_Has_Code;

      Stmt_Bit_Map : Statement_Bit_Map renames
        Bit_Maps (CU, SCOs_Fingerprint).Statement_Bits.all;

      --  Start of processing for Initialize_SCI_For_Instrumented_CU

   begin
      Initialize_SCI;
      for Bit in Stmt_Bit_Map'Range loop
         Set_Has_Code (Stmt_Bit_Map (Bit));
      end loop;

      --  If statements were instrumented as blocks, also process the non-
      --  instrumented statement SCOs in blocks.

      declare
         Stmt_Blocks : SCO_Id_Vector_Vector renames
           Blocks (CU, SCOs_Fingerprint);
         Block_Index : Positive;
      begin
         if not Stmt_Blocks.Is_Empty then
            Block_Index := Stmt_Blocks.First_Index;

            for Bit in Stmt_Bit_Map'Range loop

               --  Skip bits corresponding to fun_call obligations

               if Kind (Stmt_Bit_Map (Bit)) /= Statement then
                  goto Continue;
               end if;

               --  Assert that the SCO corresponding to the current bit
               --  corresponds to the last statement SCO of the current block.

               if Stmt_Blocks.Element (Block_Index).Last_Element
                 /= Stmt_Bit_Map (Bit)
               then
                  Outputs.Fatal_Error
                    ("Contents of statement blocks is inconsistent with source"
                     & " coverage obligations");
               end if;

               for SCO of Stmt_Blocks.Element (Block_Index) loop
                  Set_Has_Code (SCO);
               end loop;
               Block_Index := Block_Index + 1;
               <<Continue>>
            end loop;
         end if;
      end;
   end Initialize_SCI_For_SID;

   ----------------------------------------
   -- Initialize_SCI_For_Instrumented_CU --
   ----------------------------------------

   procedure Initialize_SCI_For_Instrumented_CU (CU : CU_Id) is
   begin
      for Fingerprint of SC_Obligations.Fingerprints (CU) loop
         Initialize_SCI_For_SID (CU, Fingerprint);
      end loop;
   end Initialize_SCI_For_Instrumented_CU;

   --------------------------
   -- Merge_Checkpoint_SCI --
   --------------------------

   procedure Merge_Checkpoint_SCI
     (SCO    : SCO_Id;
      CP_SCI : Source_Coverage_Info;
      Relocs : Checkpoint_Relocations)
   is
      SCI : Source_Coverage_Info renames SCI_Vector.Reference (SCO);
   begin
      pragma Assert (SCI.Kind = CP_SCI.Kind);

      --  Merge raw coverage information from checkpoint. SCI.Line_State will
      --  be recomputed later on, once traces for this increment have been
      --  processed.

      case SCI.Kind is
         when Statement =>
            SCI.Basic_Block_Has_Code :=
              SCI.Basic_Block_Has_Code or CP_SCI.Basic_Block_Has_Code;
            SCI.Executed := SCI.Executed or CP_SCI.Executed;
            SCI.Line_Executed := SCI.Line_Executed or CP_SCI.Line_Executed;

         when Decision  =>
            SCI.Known_Outcome_Taken :=
              SCI.Known_Outcome_Taken or CP_SCI.Known_Outcome_Taken;

            --  Note: if checkpoint has only one Outcome_Taken, and the SCO has
            --  degraded origins, then we can't take advantage of it, because
            --  it might be negated compared to the current context.

            if not Degraded_Origins (SCO)
              or else CP_SCI.Outcome_Taken (False)
                      = CP_SCI.Outcome_Taken (True)
            then
               SCI.Outcome_Taken := SCI.Outcome_Taken or CP_SCI.Outcome_Taken;
            end if;

            --  Merge evaluation vectors from checkpoint

            for Cur in CP_SCI.Evaluations.Iterate loop
               declare
                  E : Evaluation := Evaluation_Sets.Element (Cur);
               begin
                  E.Decision := Remap_SCO_Id (Relocs, E.Decision);
                  SCI.Evaluations.Include (E);
               end;
            end loop;

         when others    =>
            null;
      end case;
   end Merge_Checkpoint_SCI;

   ----------
   -- Read --
   ----------

   procedure Read
     (CLS : in out Checkpoint_Load_State; Value : out Source_Coverage_Info)
   is
      CP_SCI : Source_Coverage_Info (SCO_Kind'Val (CLS.Read_U8));
   begin
      declare
         States : array (1 .. 10) of Line_State;
      begin
         for I in States'Range loop
            States (I) := CLS.Read_Line_State;
         end loop;
         CP_SCI.State (Insn) := States (1);
         CP_SCI.State (Branch) := States (2);
         CP_SCI.State (Stmt) := States (3);
         CP_SCI.State (Decision) := States (4);
         CP_SCI.State (MCDC) := States (5);
         CP_SCI.State (UC_MCDC) := States (6);
         CP_SCI.State (ATC) := States (7);
         CP_SCI.State (ATCC) := States (8);
         CP_SCI.State (Fun_Call) := States (9);
         CP_SCI.State (GExpr) := States (10);
      end;

      case CP_SCI.Kind is
         when Statement         =>
            CP_SCI.Basic_Block_Has_Code := CLS.Read_Boolean;
            CP_SCI.Executed := CLS.Read_Boolean;
            CP_SCI.Line_Executed := CLS.Read_Boolean;

         when Decision          =>
            CP_SCI.Outcome_Taken (False) := CLS.Read_Boolean;
            CP_SCI.Outcome_Taken (True) := CLS.Read_Boolean;

            CP_SCI.Known_Outcome_Taken (False) := CLS.Read_Boolean;
            CP_SCI.Known_Outcome_Taken (True) := CLS.Read_Boolean;

            Read (CLS, CP_SCI.Evaluations);

         when Fun_Call_SCO_Kind =>
            CP_SCI.Fun_Call_Executed := CLS.Read_Boolean;

         when Guarded_Expr      =>
            CP_SCI.GExpr_Executed := CLS.Read_Boolean;

         when others            =>
            null;
      end case;

      Value := Source_Coverage_Info'(CP_SCI);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (CSS : in out Checkpoint_Save_State; Value : Source_Coverage_Info) is
   begin
      CSS.Write_U8 (SCO_Kind'Pos (Value.Kind));
      for S of Value.State loop
         CSS.Write (S);
      end loop;

      case Value.Kind is
         when Statement         =>
            CSS.Write (Value.Basic_Block_Has_Code);
            CSS.Write (Value.Executed);
            CSS.Write (Value.Line_Executed);

         when Decision          =>
            CSS.Write (Value.Outcome_Taken (False));
            CSS.Write (Value.Outcome_Taken (True));

            CSS.Write (Value.Known_Outcome_Taken (False));
            CSS.Write (Value.Known_Outcome_Taken (True));

            Write (CSS, Value.Evaluations);

         when Fun_Call_SCO_Kind =>
            CSS.Write (Value.Fun_Call_Executed);

         when Guarded_Expr      =>
            CSS.Write (Value.GExpr_Executed);

         when others            =>
            null;
      end case;
   end Write;

   ------------------------
   -- Report_If_Excluded --
   ------------------------

   function Report_If_Excluded (SCO : SCO_Id) return Boolean is
   begin
      if Excluded_SCOs then
         return
           Kind (SCO) = Decision
           or else (Kind (SCO) = Statement
                    and then S_Kind (SCO) in Ada_Statement_Kind);
      else
         return False;
      end if;
   end Report_If_Excluded;

   ------------------------------
   -- Set_Basic_Block_Has_Code --
   ------------------------------

   procedure Set_Basic_Block_Has_Code (SCO : SCO_Id) is

      S_SCO : SCO_Id := SCO;
      pragma Assert (Kind (S_SCO) = Statement);

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
            SCI_Vector.Reference (S_SCO).Basic_Block_Has_Code := True;
         end if;

         exit when not Propagating and No_Propagation;

         Propagating := True;
         S_SCO := Previous (S_SCO);
         exit when
           S_SCO = No_SCO_Id or else SCI_Vector (S_SCO).Basic_Block_Has_Code;
      end loop;
   end Set_Basic_Block_Has_Code;

   -----------------------
   -- Update_Line_State --
   -----------------------

   procedure Update_Line_State
     (Line  : Line_Info_Access;
      SCO   : SCO_Id;
      Level : Coverage_Level;
      State : Line_State)
   is
      Cell : constant Line_State_Cell := Coverage_Level_To_Cell (Level);
   begin
      Update_State (Line.State (Cell), SCO, Level, State);
   end Update_Line_State;

   ------------------
   -- Update_State --
   ------------------

   procedure Update_State
     (Prev_State : in out Line_State;
      SCO        : SCO_Id;
      Level      : Coverage_Level;
      State      : Line_State) is
   begin
      SCI_Vector.Reference (SCO).State (Level) := State;
      Prev_State := Prev_State * State;
   end Update_State;

end Coverage.Source;
