------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Strings; use GNAT.Strings;
with GNAT.Regexp;  use GNAT.Regexp;

with Interfaces; use Interfaces;

with System.Storage_Elements;

with Coverage.Source;   use Coverage.Source;
with Coverage.Tags;     use Coverage.Tags;
with Diagnostics;       use Diagnostics;
with Elf_Disassemblers; use Elf_Disassemblers;
with Execs_Dbase;       use Execs_Dbase;
with Files_Table;       use Files_Table;
with Hex_Images;        use Hex_Images;
with Qemu_Traces;
with Slocs;             use Slocs;
with Strings;           use Strings;
with Switches;          use Switches;
with Traces_Dbase;      use Traces_Dbase;
with Traces_Files;      use Traces_Files;
with Traces_Names;      use Traces_Names;

package body Decision_Map is

   use Ada.Containers;
   use Coverage;

   Decision_Map_Base : Traces_Base;
   --  The decision map is a list of code addresses, so we manage it as a
   --  trace database.

   package Decision_Occurrence_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Decision_Occurrence_Access);
   use type Decision_Occurrence_Vectors.Vector;
   --  A list of decision occurrences, used for Decision_Occurrence_Maps below,
   --  and also to maintain the stack of open decision occurrences while
   --  analysing object code.

   package Decision_Occurrence_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => SCO_Id,
      Element_Type => Decision_Occurrence_Vectors.Vector);
   Decision_Occurrence_Map : Decision_Occurrence_Maps.Map;
   --  The decision occurrence map lists all object code occurrences of each
   --  source decision (identified by its SCO_Id).

   package SCO_Sets is new Ada.Containers.Ordered_Sets (SCO_Id);
   use type SCO_Sets.Set;

   package Statement_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Pc_Type,
      Element_Type => SCO_Sets.Set);

   --  A basic block in object code

   type Basic_Block is record
      From, To_PC, To  : Pc_Type := No_PC;
      --  Start and end addresses (note: To is not necessarily a valid PC value
      --  but instead the address of the last byte in the last instruction of
      --  the BB, whose first byte is at To_PC).

      --  Properties of the branch instruction at the end of the basic block:

      Branch_Dest            : Dest := (Target => No_PC, Delay_Slot => No_PC);
      --  Destination

      Branch                 : Branch_Kind := Br_None;
      --  Branch kind

      Cond                   : Boolean;
      --  True if conditional branch

      Condition              : SCO_Id := No_SCO_Id;
      --  If this is a conditional branch testing a condition, identifies it

      Excluded_From_Decision : Boolean;
      --  True when there is no doubt that this basic block is *not* part of a
      --  decision (nor a condition, hence). This is the case for basic blocks
      --  that contain a call to a finalizer and for the basic blocks that
      --  follow them until the next conditional branch.

      Statements             : Statement_Maps.Map;
      --  Statement SCOs associated with each PC range within the basic block
   end record;

   No_Basic_Block : constant Basic_Block := (others => <>);

   function "<" (L, R : Basic_Block) return Boolean;
   --  Order by From

   function Is_Finalizer_Symbol (Name : String_Access) return Boolean;
   --  Return whether Name corresponds to a finalizer symbol name

   Finalizer_Symbol_Pattern : constant Regexp := Compile
     (".*___finalizer\.[0-9]+");

   package Pc_Sets is new Ada.Containers.Ordered_Sets (Pc_Type);

   package Basic_Block_Sets is new Ada.Containers.Ordered_Sets (Basic_Block);

   function Find_Basic_Block
     (Basic_Blocks : Basic_Block_Sets.Set;
      PC           : Pc_Type) return Basic_Block;
   --  Return the basic block containing PC from the given set, or
   --  No_Basic_Block if none.

   type Branch_Count_Array is
     array (Branch_Kind, Any_Statement_Kind, Boolean) of Natural;
   --  Branch counts by branch kind and, for branches associated with a
   --  statement SCO, statement kind. The third dimension discriminates
   --  between conditional and non-conditional branches.

   type Cond_Branch_Kind is (None, Statement, Condition, Check);
   --  Statistics category for a conditional branch instruction:
   --    * no SCO
   --    * statement SCO
   --    * condition SCO, non-exception
   --    * condition SCO, exception

   type Cond_Branch_Count_Array is array (Cond_Branch_Kind) of Natural;

   type Branch_Statistics is record
      Branch_Counts      : Branch_Count_Array      :=
                             (others => (others => (others => 0)));
      Cond_Branch_Counts : Cond_Branch_Count_Array := (others => 0);
      Non_Traceable      : Natural := 0;
   end record;

   type Cond_Branch_Context is limited record
      Decision_Stack : Decision_Occurrence_Vectors.Vector;
      --  The stack of open decision occurrences

      Basic_Blocks   : Basic_Block_Sets.Set;
      --  All basic blocks in the routine being analyzed

      Stats          : Branch_Statistics;
      --  Statistics on conditional branches in the routine being analyzed
   end record;

   procedure Analyze_Routine
     (Name  : String_Access;
      Exec  : Exe_File_Acc;
      Insns : Binary_Content);
   --  Build decision map for the given subprogram

   procedure Analyze_Conditional_Branch
     (Exec        : Exe_File_Acc;
      Insn        : Binary_Content;
      Tag         : SC_Tag;
      C_SCO       : SCO_Id;
      Branch_Dest : Dest;
      FT_Dest     : Dest;
      Ctx         : in out Cond_Branch_Context;
      BB          : in out Basic_Block);
   --  Process one conditional branch instruction for the given condition SCO.
   --  Sets BB.Condition to C_SCO, if applicable.

   procedure Analyze_Decision_Occurrence
     (Exe   : Exe_File_Acc;
      Ctx   : Cond_Branch_Context;
      D_Occ : Decision_Occurrence_Access);
   --  Perform logical structure analysis of the given decision occurrence

   procedure Append_Decision_Occurrence (D_Occ : Decision_Occurrence_Access);
   --  Record association of D_Occ with its decision

   function Image (BB : Basic_Block) return String;
   pragma Unreferenced (Image);
   --  For debugging purposes

   procedure Write_Map (Filename : String);
   --  Write the contents of the decision map to the named file

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Basic_Block) return Boolean is
   begin
      return L.From < R.From;
   end "<";

   function "<" (L, R : Cond_Branch_Loc) return Boolean is
      use System.Storage_Elements;
   begin
      return To_Integer (L.Exe.all'Address) < To_Integer (R.Exe.all'Address)
        or else (L.Exe = R.Exe and then L.PC < R.PC);
   end "<";

   -------------------------
   -- Is_Finalizer_Symbol --
   -------------------------

   function Is_Finalizer_Symbol (Name : String_Access) return Boolean is
      Result : Boolean;
   begin
      if Name = null then
         return False;
      else
         Result := Match (Name.all, Finalizer_Symbol_Pattern);
         return Result;
      end if;
   end Is_Finalizer_Symbol;

   -------------
   -- Analyze --
   -------------

   procedure Analyze (Exe_File : Exe_File_Acc) is
      Sym_It : Addresses_Iterator;
      Sym    : Addresses_Info_Acc;
      Sec    : Addresses_Info_Acc;

      First_Symbol_Occurrence : Boolean;
      Subp_Key                : Subprogram_Key;
      Subp_Info               : Subprogram_Info;
   begin
      Build_Debug_Lines (Exe_File.all);

      --  Add routine names of interest to routines database

      Routine_Names_From_Lines (Exe_File, Has_SCO'Access);

      --  Analyze control flow graph

      Init_Iterator (Exe_File.all, Symbol_Addresses, Sym_It);
      loop
         Next_Iterator (Sym_It, Sym);
         exit when Sym = null;

         --  Only process symbols we are interested in

         if Is_Routine_Of_Interest (Sym.Symbol_Name.all) then
            Sec := Sym.Parent;
            Load_Section_Content (Exe_File.all, Sec);

            Key_From_Symbol (Exe_File, Sym, Subp_Key);

            Add_Code
              (Subp_Key,
               Exe_File,
               Sec.Section_Content (Sym.First .. Sym.Last),
               First_Symbol_Occurrence,
               Subp_Info);

            --  Build decision map for routine based on referenced instance

            if First_Symbol_Occurrence then
               Tag_Provider.Enter_Routine (Subp_Info);

               Analyze_Routine
                 (Sym.Symbol_Name,
                  Exe_File,
                  Sec.Section_Content (Sym.First .. Sym.Last));

               --  Load sloc information

               Build_Source_Lines_For_Section
                 (Exe_File,
                  null,
                  Sec.Section_Content (Sym.First .. Sym.Last));
            end if;
         end if;
      end loop;
   end Analyze;

   --------------------------------
   -- Analyze_Conditional_Branch --
   --------------------------------

   procedure Analyze_Conditional_Branch
     (Exec        : Exe_File_Acc;
      Insn        : Binary_Content;
      Tag         : SC_Tag;
      C_SCO       : SCO_Id;
      Branch_Dest : Dest;
      FT_Dest     : Dest;
      Ctx         : in out Cond_Branch_Context;
      BB          : in out Basic_Block)
   is
      pragma Assert (Kind (C_SCO) = Condition);

      D_SCO : constant SCO_Id := Enclosing_Decision (C_SCO);

   begin
      --  Mark instruction address for full (historical) traces collection (for
      --  MC/DC source coverage analysis) if required by decision structure
      --  (presence of multiple paths) or if Debug_Full_History is set.

      if Has_Diamond (D_SCO) or else Debug_Full_History then
         Add_Entry
           (Base  => Decision_Map_Base,
            First => Insn'First,
            Last  => Insn'Last,
            Op    => 0);
      end if;

      --  Record address in SCO descriptor

      Add_Address (C_SCO, Insn'First);

      --  Update control flow information

      Process_Condition :
      declare
         Parent_SCO : SCO_Id;
         --  Parent SCO of D_SCO, if appropriate

         Enclosing_D_SCO : SCO_Id;
         --  For a nested decision, the enclosing decision

         DS_Top : Decision_Occurrence_Access;
         --  Innermost currently open decision

         Cond_Index : constant Condition_Index := Index (C_SCO);
         --  Index of C_SCO in D_SCO

         function Is_Expected_Condition
           (CI                   : Condition_Index;
            Report_If_Unexpected : Boolean := False) return Boolean;
         --  Check whether we expect to evaluate CI: either we remain in the
         --  current condition (case of a condition that requires multiple
         --  branches), or we move to the next one.

         procedure Check_Condition_Index (CI : Condition_Index);
         --  Check whether CI is an expected condition, and if not, report an
         --  error.

         ---------------------------
         -- Is_Expected_Condition --
         ---------------------------

         function Is_Expected_Condition
           (CI                   : Condition_Index;
            Report_If_Unexpected : Boolean := False) return Boolean
         is
            Current_CI : Condition_Index renames DS_Top.Seen_Condition;
            Last_CI    : Condition_Index renames DS_Top.Last_Cond_Index;

         begin
            if CI in Condition_Index'Max (Current_CI,     0)
                  .. Condition_Index'Min (Current_CI + 1, Last_CI)
            then
               return True;
            end if;

            if Report_If_Unexpected then
               declare
                  use Ada.Strings.Unbounded;

                  Msg : Unbounded_String;
                  Expect_Next_CI : Boolean;
               begin
                  Msg := To_Unbounded_String
                    ("unexpected condition" & CI'Img & " (expected");

                  Expect_Next_CI := Current_CI < DS_Top.Last_Cond_Index;

                  if Current_CI >= 0 then
                     Append (Msg, Condition_Index'Image (Current_CI));

                     if Expect_Next_CI then
                        Append (Msg, " or");
                     end if;
                  end if;

                  if Expect_Next_CI then
                     Append (Msg, Condition_Index'Image (Current_CI + 1));
                  end if;

                  Append (Msg, ") in decision " & Image (DS_Top.Decision));

                  if Tag /= No_SC_Tag then
                     Append (Msg, ", tag=" & Tag_Provider.Tag_Name (Tag));
                  end if;

                  Report (Exec, Insn'First, To_String (Msg));
               end;
            end if;
            return False;
         end Is_Expected_Condition;

         ---------------------------
         -- Check_Condition_Index --
         ---------------------------

         procedure Check_Condition_Index (CI : Condition_Index) is
            Dummy : Boolean;
            pragma Unreferenced (Dummy);
         begin
            Dummy := Is_Expected_Condition (CI, Report_If_Unexpected => True);
         end Check_Condition_Index;

      --  Start of processing for Process_Condition

      begin
         Parent_SCO := Parent (D_SCO);

         if Parent_SCO /= No_SCO_Id
           and then Kind (Parent_SCO) = Condition
         then
            Enclosing_D_SCO := Enclosing_Decision (Parent_SCO);
         else
            Enclosing_D_SCO := No_SCO_Id;
         end if;

         --  Flush completed decisions from the Decision_Stack

         while Ctx.Decision_Stack.Length > 0 loop
            DS_Top := Ctx.Decision_Stack.Last_Element;
            exit when DS_Top.Decision = D_SCO
              and then Is_Expected_Condition (Cond_Index);

            if DS_Top.Decision = Enclosing_D_SCO then
               --  Here if the parent of our decision is part of a
               --  condition in another decision, and DS_Top is that
               --  enclosing decision.

               Check_Condition_Index (Index (Parent_SCO));
               DS_Top := null;
               exit;
            end if;

            --  If the condition being evaluated is the first one in its
            --  decision, assume that we are starting a new nested (or
            --  successive) evaluation.

            exit when Cond_Index = 0;

            --  Otherwise pop completed evaluations from the stack until
            --  we find the relevant pending one.

            Analyze_Decision_Occurrence (Exec, Ctx, DS_Top);
            Ctx.Decision_Stack.Delete_Last;
            DS_Top := null;
         end loop;

         if

           --  No pending evaluation

           DS_Top = null

           --  Evaluating a new, different decision than the enclosing one

           or else DS_Top.Decision /= D_SCO

           --  Nested/successive evaluation of the same decision

           or else (DS_Top.Seen_Condition = DS_Top.Last_Cond_Index
                    and then DS_Top.Last_Cond_Index > 0
                    and then Cond_Index = 0)

         then
            --  Push new context

            DS_Top := new Decision_Occurrence'
              (Last_Cond_Index => Last_Cond_Index (D_SCO),
               Decision        => D_SCO,
               others          => <>);
            Ctx.Decision_Stack.Append (DS_Top);
         end if;

         --  Here after pushing context for current decision, if needed

         pragma Assert (DS_Top.Decision = D_SCO);
         Check_Condition_Index (Cond_Index);

         --  Record condition occurrence

         Report
           (Exec, Insn'First,
            "cond branch for " & Image (C_SCO)
            & " (" & Img (Integer (Index (C_SCO))) & ")",
            Kind => Notice);
         pragma Assert (BB.Condition = No_SCO_Id);
         BB.Condition := C_SCO;

         if Cond_Index > DS_Top.Seen_Condition then
            DS_Top.Seen_Condition := Cond_Index;
         end if;

         DS_Top.Conditional_Branches.Append (Insn'First);

         Cond_Branch_Map.Insert
           ((Exec, Insn'First),
            Cond_Branch_Info'
              (Decision_Occurrence => DS_Top,
               Condition           => C_SCO,
               Edges               =>
                 (Branch      =>
                    (Destination => Branch_Dest,
                     Dest_Kind   =>
                       (if BB.Branch = Br_Ret then Outcome else Unknown),
                     others      => <>),
                  Fallthrough =>
                    (Destination => FT_Dest,
                     others      => <>))));
      end Process_Condition;
   end Analyze_Conditional_Branch;

   ---------------------------------
   -- Analyze_Decision_Occurrence --
   ---------------------------------

   procedure Analyze_Decision_Occurrence
     (Exe   : Exe_File_Acc;
      Ctx   : Cond_Branch_Context;
      D_Occ : Decision_Occurrence_Access)
   is
      Last_Seen_Condition_PC : constant Pc_Type :=
                                 D_Occ.Conditional_Branches.Last_Element;

      --  Note: all the analysis is done under control of an initial check that
      --    D_Occ.Seen_Condition = D_Occ.Last_Condition_Index

      Last_CBI : constant Cond_Branch_Info :=
                   Cond_Branch_Map.Element ((Exe, Last_Seen_Condition_PC));

      --  For destinations for which we have identified origin information,
      --  reference to the conditional branch and edge having that destination,
      --  which carries the known information. This information can be used to
      --  copy edge information for edges that share the same destination.
      --  Note that this assumes that control flow changes fully capture the
      --  values of short-circuit operators. This still holds in cases where
      --  such a value is captured in a temporary variable, because:
      --    - for the case of branches corresponding to non-short-circuit value
      --      this is trivial (the destination corresponds to the single point
      --      in code where the value is assigned True).
      --    - if two instructions branch to the same destination past the
      --      assignment, then both must bypass it (there can't be one
      --      bypassing it and one occurring after the assignment has been
      --      actually evaluated).

      type Known_Destination is record
         Cond_Branch_PC : Pc_Type;
         Edge           : Edge_Kind;
      end record;

      package Known_Destination_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type     => Dest,
         Element_Type => Known_Destination);

      Known_Destinations : Known_Destination_Maps.Map;

      package Dest_Sets is new Ada.Containers.Ordered_Sets (Dest);
      Known_Outcome : array (Boolean) of Dest_Sets.Set;
      --  When set, each element of this array is a set of edge destinations
      --  known to correspond to the respective outcome of the decision.

      procedure Trace_Destination
        (CBI       : Cond_Branch_Info;
         Edge      : Edge_Kind;
         Edge_Info : in out Cond_Edge_Info);
      --  Inspect the basic block containing Edge's destination, and if
      --  necessary any basic block we unconditionally branch to from there,
      --  until we find a conditional branch or a call to an exception-raising
      --  routine.

      procedure Label_Destination
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info;
         Edge           : Edge_Kind);
      --  First pass of control flow analysis: test if Edge's destination
      --  matches either of Last_CBI's edges' destination, and if so mark it as
      --  an outcome destination.

      procedure Label_From_Opposite
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info;
         Edge           : Edge_Kind);
      --  Second pass of control flow analysis: if Edge is not qualified yet,
      --  but the opposite destination of CBI is, deduce qualification for Edge
      --  from that information.

      procedure Label_From_Other
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info;
         Edge           : Edge_Kind);
      --  Third pass of control flow analysis: if Edge is not qualified yet,
      --  but the another edge with the same destination is, copy its
      --  information.

      procedure Set_Known_Origin
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info;
         Edge           : Edge_Kind;
         Origin         : Boolean);
      --  Qualify CBE as corresponding to valuation Origin of the condition
      --  tested by the conditional branch at Cond_Branch_PC, described by CBI.

      procedure Record_Known_Destination
        (Cond_Branch_PC : Pc_Type;
         CBI            : Cond_Branch_Info;
         Edge           : Edge_Kind);
      --  Once this destination has been fully qualified, remember it so
      --  that further tests with the same destination can reuse the
      --  information.

      procedure Label_Destinations
        (CB_Loc : Cond_Branch_Loc;
         CBI    : in out Cond_Branch_Info);
      --  Identify destination kind of each edge of CBI using information local
      --  to CBI.

      procedure Fixup_Finalizer_Edges
        (CB_Loc : Cond_Branch_Loc;
         CBI    : in out Cond_Branch_Info);
      --  Fix up edges not to be unknown outcomes when they are post-dominated
      --  by calls to finalizers.

      procedure Report_Unlabeled_Destinations
        (CB_Loc : Cond_Branch_Loc;
         CBI    : in out Cond_Branch_Info);
      --  Report remaining unqualified edges

      Has_Valuation : array (Condition_Index'First .. D_Occ.Last_Cond_Index,
                             Boolean range False .. True) of Boolean :=
                               (others => (others => False));
      --  For each valuation of each condition, indicates whether there is
      --  one edge corresponding to each possible valuation of the condition.

      -----------------------
      -- Label_Destination --
      -----------------------

      procedure Label_Destination
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info;
         Edge           : Edge_Kind)
      is
         Edge_Name : constant String := Edge'Img;
         Edge_Info : Cond_Edge_Info renames CBI.Edges (Edge);

         function Check_Possible_Successor
           (Next_Condition : Condition_Index) return Tristate;
         --  Determine whether Next_Condition is a valid successor for the
         --  currently tested condition (CBI.Condition), and if so, return
         --  the associated origin.

         ------------------------------
         -- Check_Possible_Successor --
         ------------------------------

         function Check_Possible_Successor
           (Next_Condition : Condition_Index) return Tristate
         is
         begin
            for J in Boolean'Range loop
               declare
                  Next_C : constant SCO_Id :=
                             SC_Obligations.Next_Condition (CBI.Condition, J);
               begin
                  if Next_C /= No_SCO_Id
                    and then Index (Next_C) = Next_Condition
                  then
                     return To_Tristate (J);
                  end if;
               end;
            end loop;
            return Unknown;
         end Check_Possible_Successor;

      --  Start of processing for Label_Destination

      begin
         --  Check for known edge with this destination. Destination info will
         --  be set upon return if destination is known.

         Label_From_Other (Cond_Branch_PC, CBI, Edge);

         if Edge_Info.Dest_Kind = Unknown then
            --  Here for the first occurrence of a destination

            Trace_Destination (CBI, Edge, Edge_Info);
         end if;

         --  Here the destination kind has been identified, now compute the
         --  corresponding origin, i.e. the value of the condition being tested
         --  that causes this destination to be reached.

         --  Check for outcome destination

         --  HYP: a branch destination is an outcome when it is equal to one
         --  of the branch destinations of the last conditional branch in
         --  the decision. Note: some outcome destinations do not satisfy
         --  this property (but there is no indication that any non-outcome
         --  destination does satisfy it).

         --  HYP: a branch destination is an outcome when it branches past
         --  the last conditional branch instruction for the last condition.

         if (Edge_Info.Dest_Kind = Outcome and then Edge_Info.Origin = Unknown)
              or else
            (Edge_Info.Dest_Kind = Unknown
               and then
             (Edge_Info.Destination
                = Last_CBI.Edges (Branch).Destination
                or else Edge_Info.Destination
                          = Last_CBI.Edges (Fallthrough).Destination
                or else Edge_Info.Destination.Target > Last_Seen_Condition_PC))
         then
            Edge_Info.Dest_Kind := Outcome;

            --  Check that there is a possible outcome from this condition

            declare
               Outcome_Seen   : Boolean;
               --  Set True when there is a value of the condition that
               --  determines an outcome of the decision.

               Outcome_Origin : Tristate := Unknown;
               --  The value of the condition that determines this outcome,
               --  if there is only one such value. Reset to Unknown when
               --  both values of the condition determine outcomes (case of the
               --  last condition in a decision).

            begin
               --  Check whether either valuation of the current condition
               --  determines the decision outcome.

               for J in Boolean'Range loop
                  declare
                     Candidate_Outcome : constant Tristate :=
                                           Outcome (CBI.Condition, J);
                  begin
                     if Candidate_Outcome /= Unknown then
                        Outcome_Seen := True;

                        if Edge_Info.Outcome /= Unknown then
                           --  Case where we know what outcome is reached
                           --  by this edge.

                           if Candidate_Outcome = Edge_Info.Outcome then
                              Outcome_Origin := To_Tristate (J);
                              exit;
                           end if;

                        else
                           --  Case where we do not know what outcome is
                           --  reached: if there is only one reachable outcome,
                           --  then we found it.

                           if Outcome_Origin = Unknown then
                              --  J becomes candidate origin

                              Outcome_Origin := To_Tristate (J);

                           else
                              --  This is the second iteration, and we have
                              --  another candidate origin: this means that
                              --  the current condition gives an outcome either
                              --  way, so we can't determine the proper origin
                              --  at this point.

                              Outcome_Origin := Unknown;
                           end if;
                        end if;
                     end if;
                  end;
               end loop;

               if not Outcome_Seen then
                  --  Case of a destination that we identified as an outcome
                  --  but whose condition cannot determine the outcome of the
                  --  decision according to the BDD.

                  Report
                    (Exe, Cond_Branch_PC,
                     Edge_Name & " destination unexpectedly out of decision");

               elsif Outcome_Origin /= Unknown then
                  Set_Known_Origin
                    (Cond_Branch_PC, CBI, Edge, To_Boolean (Outcome_Origin));
                  Known_Outcome (To_Boolean (Edge_Info.Outcome)).
                    Include (Edge_Info.Destination);

               else
                  --  In the case of a decision with only one condition (but
                  --  possibly multiple branches for each condition), assign
                  --  an arbitrary origin to the last outcome destination if no
                  --  previous information allowed to identify it (the other
                  --  destination will be labeled by Label_From_Opposite).

                  --  Note: we don't degrade origins for the first edge
                  --  immediately because dominance information might allow
                  --  precise labeling on the second edge.

                  if D_Occ.Last_Cond_Index = Condition_Index'First
                    and then Edge_Info.Dest_Kind = Outcome
                    and then Edge = Edge_Kind'Last
                    and then Known_Outcome (False).Is_Empty
                    and then Known_Outcome (True).Is_Empty
                  then
                     Set_Degraded_Origins (D_Occ.Decision);
                     Known_Outcome (True).Include (Edge_Info.Destination);

                     --  Both edges from this conditional branch instruction
                     --  are (distinct) outcomes, and we arbitrarily decide
                     --  that this one is the one for outcome True. So, if the
                     --  outcome for origin True is True, then the origin for
                     --  for this edge is True, else it is False.

                     Set_Known_Origin
                       (Cond_Branch_PC, CBI, Edge,
                        Outcome (CBI.Condition, True) = To_Tristate (True));
                  end if;
               end if;
            end;

         --  Check for internal destination

         elsif Edge_Info.Dest_Kind = Condition then

            --  Check that the next condition is a possible successor, and
            --  label edge origin (that is, the valuation of the tested
            --  condition that causes the successor in question to be evaluated
            --  next).

            declare
               Condition_Origin : constant Tristate :=
                                    Check_Possible_Successor
                                      (Edge_Info.Next_Condition);
            begin
               if Condition_Origin /= Unknown then
                  Set_Known_Origin
                    (Cond_Branch_PC, CBI, Edge, To_Boolean (Condition_Origin));

               elsif Edge_Info.Next_Condition /= Index (CBI.Condition) then
                  --  Report failure to identify successor if neither going to
                  --  a possible successor nor remaining in the same condition.

                  Report
                    (Exe, Cond_Branch_PC,
                     Edge_Name
                     & " does not branch to a possible successor condition");
               end if;
            end;
         end if;

         --  Look for a previous edge with the same destination

         --  Label_From_Other was already called at the beginning of
         --  Label_Destination, is this redundant???

         if Edge_Info.Dest_Kind = Unknown then
            Label_From_Other (Cond_Branch_PC, CBI, Edge);
         end if;

         --  Destination may still be unlabeled at this point, which is not
         --  a problem if we can label it later on by inference from the
         --  opposite edge.
      end Label_Destination;

      ------------------------
      -- Label_Destinations --
      ------------------------

      procedure Label_Destinations
        (CB_Loc : Cond_Branch_Loc;
         CBI    : in out Cond_Branch_Info)
      is
         Cond_Branch_PC : Pc_Type renames CB_Loc.PC;
      begin
         --  Label each destination

         for Edge in Edge_Kind loop
            Label_Destination (Cond_Branch_PC, CBI, Edge);
         end loop;

         --  So far we have looked at each destination in isolation. Now try
         --  to further qualify each destination by deducing its properties
         --  from those known on the other, and record known valuations.

         for Edge in Edge_Kind loop
            Label_From_Opposite (Cond_Branch_PC, CBI, Edge);

            if CBI.Edges (Edge).Origin /= Unknown then
               Has_Valuation
                 (Index (CBI.Condition), To_Boolean (CBI.Edges (Edge).Origin))
                   := True;
            end if;
         end loop;

      end Label_Destinations;

      -------------------------
      -- Label_From_Opposite --
      -------------------------

      procedure Label_From_Opposite
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info;
         Edge           : Edge_Kind)
      is
         This_CBE     : Cond_Edge_Info renames CBI.Edges (Edge);
         Opposite_CBE : Cond_Edge_Info renames
                          CBI.Edges (Edge_Kind'Val (1 - Edge_Kind'Pos (Edge)));
      begin
         if This_CBE.Origin /= Unknown
           or else This_CBE.Dest_Kind = Condition
           or else This_CBE.Dest_Kind = Raise_Exception
         then
            --  This_CBE is already labeled (either known origin, or unknown
            --  origin but known to be part of a runtime check or remain in
            --  the same condition): nothing to do.

            return;
         end if;

         --  Here when This_CBE is either Unknown or Outcome, and has Unknown
         --  Origin.

         if Opposite_CBE.Dest_Kind = Raise_Exception then
            --  Opposite branch is for a compiler-generated check, so this one
            --  remains in the current condition.

            This_CBE.Dest_Kind := Condition;
            This_CBE.Next_Condition := Index (CBI.Condition);

         elsif Opposite_CBE.Origin /= Unknown then
            --  Opposite branch is associated with a known valuation of the
            --  condition, so this edge must have the opposite valuation. If
            --  that opposite valuation determines a known outcome, check that
            --  this edge does not have an inconsistent destination before
            --  setting origin.

            declare
               Candidate_Val     : constant Boolean :=
                                     not To_Boolean (Opposite_CBE.Origin);
               Candidate_Outcome : constant Tristate :=
                                     Outcome (CBI.Condition, Candidate_Val);
            begin
               if Candidate_Outcome = Unknown
                 or else not Known_Outcome
                                (not To_Boolean (Candidate_Outcome)).
                              Contains (CBI.Edges (Edge).Destination)
               then
                  Set_Known_Origin
                    (Cond_Branch_PC,
                     CBI,
                     Edge,
                     not To_Boolean (Opposite_CBE.Origin));
               end if;
            end;
         end if;
      end Label_From_Opposite;

      ----------------------
      -- Label_From_Other --
      ----------------------

      procedure Label_From_Other
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info;
         Edge           : Edge_Kind)
      is
         pragma Unreferenced (Cond_Branch_PC);

         use Known_Destination_Maps;
         CBE : Cond_Edge_Info renames CBI.Edges (Edge);
         Cur : constant Cursor :=
                 Known_Destinations.Find (CBE.Destination);
         KD  : Known_Destination;
      begin
         if CBE.Origin = Unknown and then Cur /= No_Element then
            KD := Element (Cur);
            declare
               Other_CBE : Cond_Edge_Info renames
                             Cond_Branch_Map.Element
                               ((Exe, KD.Cond_Branch_PC)).Edges (KD.Edge);
            begin
               --  Assert consistency of dest kind, if known

               if CBE.Dest_Kind /= Unknown then
                  pragma Assert (CBE.Dest_Kind = Other_CBE.Dest_Kind);
                  null;
               end if;

               --  Copy edge destination information (but not edge origin,
               --  as Other_CBE may be testing another condition).

               CBE.Dest_Kind      := Other_CBE.Dest_Kind;
               CBE.Outcome        := Other_CBE.Outcome;
               CBE.Next_Condition := Other_CBE.Next_Condition;
            end;
         end if;
      end Label_From_Other;

      ------------------------------
      -- Record_Known_Destination --
      ------------------------------

      procedure Record_Known_Destination
        (Cond_Branch_PC : Pc_Type;
         CBI            : Cond_Branch_Info;
         Edge           : Edge_Kind)
      is
         Edge_Info : Cond_Edge_Info renames CBI.Edges (Edge);
      begin
         if not Known_Destinations.Contains (Edge_Info.Destination) then
            Known_Destinations.Insert
              (Edge_Info.Destination, (Cond_Branch_PC, Edge));
         end if;
      end Record_Known_Destination;

      ---------------------------
      -- Fixup_Finalizer_Edges --
      ---------------------------

      procedure Fixup_Finalizer_Edges
        (CB_Loc : Cond_Branch_Loc;
         CBI    : in out Cond_Branch_Info)
      is
         pragma Unreferenced (CB_Loc);
      begin
         for Kind in Edge_Kind loop
            declare
               Edge : Cond_Edge_Info renames CBI.Edges (Kind);
            begin
               if Edge.Dest_Kind = Outcome
                    and then
                  Edge.Outcome = Unknown
                    and then
                  Edge.Reaches_Finalizer
               then
                  Edge.Dest_Kind := Raise_Exception;
               end if;
            end;
         end loop;
      end Fixup_Finalizer_Edges;

      -----------------------------------
      -- Report_Unlabeled_Destinations --
      -----------------------------------

      procedure Report_Unlabeled_Destinations
        (CB_Loc : Cond_Branch_Loc;
         CBI    : in out Cond_Branch_Info)
      is
         function Dest_Image
           (Edge      : Edge_Kind;
            Edge_Info : Cond_Edge_Info) return String;
         --  Return string representation of the given edge of CBI

         ----------------
         -- Dest_Image --
         ----------------

         function Dest_Image
           (Edge      : Edge_Kind;
            Edge_Info : Cond_Edge_Info) return String
         is
            function Additional_Info_Image return String;
            --  Edge_Info.Next_Condition if Dest_Kind is Condition,
            --  Edge_Info.Outcome image if Dest_Kind is Outcome,
            --  else an empty string.

            ---------------------------
            -- Additional_Info_Image --
            ---------------------------

            function Additional_Info_Image return String is
            begin
               case Edge_Info.Dest_Kind is
                  when Condition =>
                     return " ("
                       & Img (Integer (Edge_Info.Next_Condition))
                       & ")";

                  when Outcome =>
                     return " (" & Edge_Info.Outcome'Img & ")";

                  when others =>
                     return "";
               end case;
            end Additional_Info_Image;

         --  Start of processing for Dest_Image

         begin
            return Edge_Info.Origin'Img & "->" & Edge'Img
              & " = " & Hex_Image (Edge_Info.Destination.Target)
              & " " & Edge_Info.Dest_Kind'Img & Additional_Info_Image;
         end Dest_Image;

      --  Start of processing for Report_Unlabeled_Destinations

      begin
         for Edge in Edge_Kind loop
            --  Finally report destinations we still can't label

            if CBI.Edges (Edge).Dest_Kind = Unknown
                 or else
               (CBI.Edges (Edge).Dest_Kind = Outcome
                  and then CBI.Edges (Edge).Origin = Unknown)
            then
               Report (Exe, CB_Loc.PC,
                       "unable to label " & Edge'Img
                       & " destination "
                       & Hex_Image (CBI.Edges (Edge).Destination.Target),
                       Kind => Warning);
            end if;
         end loop;

         Report
           (Exe, CB_Loc.PC,
            Dest_Image (Branch, CBI.Edges (Branch))
            & " / "
            & Dest_Image (Fallthrough, CBI.Edges (Fallthrough)),
            Kind => Notice);
      end Report_Unlabeled_Destinations;

      ----------------------
      -- Set_Known_Origin --
      ----------------------

      procedure Set_Known_Origin
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info;
         Edge           : Edge_Kind;
         Origin         : Boolean)
      is
         CBE        : Cond_Edge_Info renames CBI.Edges (Edge);
         Next_C_SCO : SCO_Id;
         BB         : constant Basic_Block :=
                        Find_Basic_Block (Ctx.Basic_Blocks, Cond_Branch_PC);
      begin
         CBE.Origin  := To_Tristate (Origin);
         CBE.Outcome := Outcome (CBI.Condition, Origin);

         if CBE.Outcome /= Unknown then
            CBE.Dest_Kind := Outcome;

            if BB.Branch /= Br_Ret then
               Known_Outcome (To_Boolean (CBE.Outcome)).Include
                 (CBE.Destination);
            end if;

         else
            Next_C_SCO := Next_Condition (CBI.Condition, Origin);
            if Next_C_SCO /= No_SCO_Id then
               CBE.Dest_Kind := Condition;
               CBE.Next_Condition := Index (Next_C_SCO);
            end if;
         end if;

         if BB.Branch /= Br_Ret then
            Record_Known_Destination (Cond_Branch_PC, CBI, Edge);
         end if;
      end Set_Known_Origin;

      -----------------------
      -- Trace_Destination --
      -----------------------

      procedure Trace_Destination
        (CBI       : Cond_Branch_Info;
         Edge      : Edge_Kind;
         Edge_Info : in out Cond_Edge_Info)
      is
         pragma Unreferenced (Edge);

         Visited_BB : Pc_Sets.Set;
         --  Set of BB.To_Pc for all visited basic blocks, used to avoid
         --  infinite loops.

         Next_PC : Pc_Type := Edge_Info.Destination.Target;
         BB      : Basic_Block;

         Next_PC_Sloc : Source_Location;
         Next_PC_SCO  : SCO_Id;
         --  Statement at Next_PC

         SCO_For_Jump, D_SCO_For_Jump : SCO_Id;
         --  Statement and decision SCOs for jump instruction at end of BB

         D_SCO : constant SCO_Id := Enclosing_Decision (CBI.Condition);
         S_SCO : constant SCO_Id := Enclosing_Statement (D_SCO);
         --  SCOs for the decision being evaluated, and its enclosing statement

         Leaves_Statement : Boolean := False;
         --  True if the edge is known to branch to code that has a different
         --  statement SCO than the condition being evaluated.

         function Is_Visited_BB (Pc : Pc_Type) return Boolean;
         --  Return if the given address starts an already visited basic block

         -------------------
         -- Is_Visited_BB --
         -------------------

         function Is_Visited_BB (Pc : Pc_Type) return Boolean
         is
            use Pc_Sets;
         begin
            return Visited_BB.Find (Pc) /= Pc_Sets.No_Element;
         end Is_Visited_BB;

      begin
         <<Follow_Jump>>
         if Next_PC = No_PC then
            --  We cannot follow indirect jumps.  If we end up there, we can
            --  only stop the analysis of the current edge.

            return;
         end if;

         BB := Find_Basic_Block (Ctx.Basic_Blocks, Next_PC);
         if BB = No_Basic_Block then
            --  This should *never* happen: we are walking through the CFG of
            --  the current routine, so all jumps must target an existing basic
            --  block.

            raise Program_Error with
              "Cannot find a basic block for the instruction at "
              & Hex_Image (Next_PC);

         elsif Is_Visited_BB (BB.From) then
            --  Stop when coming across an already visited basic block, in
            --  order to avoid infinite loops.

            return;
         end if;
         Visited_BB.Insert (BB.From);

         Next_PC_Sloc := Get_Sloc (Exe.all, Next_PC);

         --  Check for exception or outcome using dominance information.
         --  Note that this relies on an accurate mapping of slocs to
         --  SCO for statements, not conditions. Since statements slocs have
         --  only only line granularity (not column granularity), this must be
         --  disabled in the case of multiple statements occurring on the same
         --  line.

         declare
            use Statement_Maps;
            Cur : constant Statement_Maps.Cursor :=
                    BB.Statements.Ceiling (Next_PC);
         begin
            if Cur /= No_Element then
               Next_PC_SCO := Element (Cur).First_Element;
            else
               Next_PC_SCO := No_SCO_Id;
            end if;
         end;

         if Next_PC_SCO /= No_SCO_Id
           and then not Is_Multistatement_Line (Next_PC_Sloc)
         then
            declare
               Dom_SCO : SCO_Id;
               Dom_Val : Boolean;
               --  Dominance information for statement SCO at Next_PC

            begin
               --  Walk back through all statement dominants, stop on decision
               --  dominant or when no dominant information is available.

               Dom_SCO := Next_PC_SCO;
               loop
                  Dominant (Dom_SCO, Dom_SCO, Dom_Val);
                  exit when Dom_SCO = No_SCO_Id;

                  if Dom_SCO = Enclosing_Decision (CBI.Condition) then

                     --  Here if this edge branches to a statement dominated by
                     --  CBI's decision being evaluated to Dom_Val.

                     Edge_Info.Dest_Kind := Outcome;
                     Edge_Info.Outcome := To_Tristate (Dom_Val);
                     return;
                  end if;

                  --  Here if we reached a decision other than the one being
                  --  evaluated.

                  exit when Kind (Dom_SCO) /= Statement;
               end loop;
            end;
         end if;

         --  Determine whether the jump is known to branch to another statement

         Leaves_Statement :=
           Leaves_Statement
             or else
               (S_SCO /= No_SCO_Id
                  and then Next_PC_SCO /= No_SCO_Id
                  and then S_SCO /= Enclosing_Statement (Next_PC_SCO));

         --  Here if we remain within the current decision: continue tracing
         --  object control flow: find SCOs for jump at end of basic block.

         --  Condition or Statement

         SCO_For_Jump := Sloc_To_SCO (Get_Sloc (Exe.all, BB.To_PC));

         --  Decision

         D_SCO_For_Jump := Sloc_To_SCO (Get_Sloc (Exe.all, BB.To_PC),
                                        Include_Decisions => True);
         if D_SCO_For_Jump /= No_SCO_Id
              and then Kind (D_SCO_For_Jump) = Condition
         then
            D_SCO_For_Jump := Enclosing_Decision (D_SCO_For_Jump);
         end if;

         --  Note: there are cases (e.g. for Pre/Post aspects) where there
         --  is a decision SCO with no enslosing statement SCO, and we have
         --  code associated with the sloc of the decision, but not with the
         --  sloc ranges of any of the conditions. In these cases we need to
         --  look at D_SCO_For_Jump to identify that the code is still part of
         --  the decision.

         case BB.Branch is
            when Br_Jmp =>
               if BB.Cond then
                  if BB.Condition /= No_SCO_Id
                    and then Enclosing_Decision (BB.Condition) = D_Occ.Decision
                    and then not Leaves_Statement
                  then
                     --  Edge proceeds to evaluate a condition in the current
                     --  decision.

                     Edge_Info.Dest_Kind := Condition;
                     Edge_Info.Next_Condition := Index (BB.Condition);
                  end if;

               else
                  --  Note that we follow unconditional jumps only when they
                  --  remain within the current condition (or its enclosing
                  --  statement, because some intermediate insns might be
                  --  decorated with just the statement sloc).

                  Next_PC := BB.Branch_Dest.Target;
                  goto Follow_Jump;
               end if;

            when Br_Call =>
               declare
                  Sym : constant Addresses_Info_Acc :=
                          Get_Symbol (Exe.all, BB.Branch_Dest.Target);
                  Sym_Name : String_Access;
               begin
                  if Sym /= null then
                     Sym_Name := Sym.Symbol_Name;
                  end if;

                  --  If the call's sloc is within the condition, and it is
                  --  a call to a runtime routine raising an exception, then
                  --  assume it is a check.

                  if SCO_For_Jump = CBI.Condition
                       and then
                     Sym_Name /= null
                       and then
                     (Sym_Name.all = "__gnat_last_chance_handler"
                        or else
                      Has_Prefix (Sym_Name.all, Prefix => "__gnat_rcheck_"))
                  then
                     Edge_Info.Dest_Kind := Raise_Exception;

                  --  Call to Raise_Assert_Failure within the decision (with
                  --  either a condition or a decision SCO), if if it is an
                  --  assert or PPC): False outcome.

                  elsif D_SCO_For_Jump = D_SCO
                    and then Is_Assertion (D_SCO)
                    and then
                      Sym_Name /= null
                    and then
                      Sym_Name.all = "system__assertions__raise_assert_failure"
                  then
                     Edge_Info.Dest_Kind := Outcome;
                     Edge_Info.Outcome   := False;

                  --  Edges that are post-dominated by calls to finalizers are
                  --  outcomes or exceptions: generated procedures that call
                  --  finalizers for some block are never called inside a
                  --  decision. Thus, if we come across one, we know the
                  --  decision evaluation is over. For now, me tag this edge as
                  --  an unknown outcome, but we might realize later (in
                  --  Fixup_Finalizer_Edges) that this is an exception edge.

                  elsif Is_Finalizer_Symbol (Sym_Name) then
                     Edge_Info.Dest_Kind         := Outcome;
                     Edge_Info.Outcome           := Unknown;
                     Edge_Info.Reaches_Finalizer := True;

                  --  Else assume call returns, continue tracing at next PC

                  else
                     Next_PC := BB.To + 1;
                     goto Follow_Jump;

                  end if;
               end;

            when Br_Ret =>

               --  Edge returning from the current function can only be an
               --  outcome.

               Edge_Info.Dest_Kind := Outcome;
               return;

            when others =>
               null;
         end case;
      end Trace_Destination;

   --  Start of processing for Analyze_Decision_Occurrence

   begin
      if D_Occ.Last_Cond_Index /= D_Occ.Seen_Condition then
         --  Report PC of last seen condition in decision occurrence, if it is
         --  not the final condition of the decision (indicates a decision
         --  occurrence being flushed before it was completely seen).

         Report (Exe, Last_Seen_Condition_PC,
                 "incomplete occurrence of " & Image (D_Occ.Decision));
         return;
      end if;

      --  Label edge destinations. Perform two passes so that the second can
      --  reuse known destinations identified by the first.

      for Pass in 1 .. 2 loop
         for J in D_Occ.Conditional_Branches.First_Index
               .. D_Occ.Conditional_Branches.Last_Index
         loop
            declare
               use Cond_Branch_Maps;
               Cur : constant Cond_Branch_Maps.Cursor :=
                 Cond_Branch_Map.Find
                   ((Exe, D_Occ.Conditional_Branches.Element (J)));
            begin
               Cond_Branch_Map.Update_Element (Cur, Label_Destinations'Access);
            end;
         end loop;
      end loop;

      --  Use finalizer tags to fix up edges that have been incorrectly tagged
      --  as outcomes.

      for J in D_Occ.Conditional_Branches.First_Index
            .. D_Occ.Conditional_Branches.Last_Index
      loop
         declare
            use Cond_Branch_Maps;
            Cur : constant Cond_Branch_Maps.Cursor :=
                    Cond_Branch_Map.Find
                      ((Exe, D_Occ.Conditional_Branches.Element (J)));
         begin
            Cond_Branch_Map.Update_Element
              (Cur, Fixup_Finalizer_Edges'Access);
         end;
      end loop;

      --  Report remaining unlabeled destinations

      for J in D_Occ.Conditional_Branches.First_Index
            .. D_Occ.Conditional_Branches.Last_Index
      loop
         declare
            use Cond_Branch_Maps;
            Cur : constant Cond_Branch_Maps.Cursor :=
                    Cond_Branch_Map.Find
                      ((Exe, D_Occ.Conditional_Branches.Element (J)));
         begin
            Cond_Branch_Map.Update_Element
              (Cur, Report_Unlabeled_Destinations'Access);
         end;
      end loop;

      --  Report conditions for which no edge provides a valuation

      for J in Condition_Index'First .. D_Occ.Last_Cond_Index loop
         for Val in Boolean'Range loop
            if not Has_Valuation (J, Val) then

               --  Static analysis failed???

               Report (First_Sloc (Condition (D_Occ.Decision, J)),
                       Msg  => "condition lacks edge for " & Val'Img,
                       Kind => Warning);
            end if;
         end loop;
      end loop;

      --  Record decision occurrence

      Append_Decision_Occurrence (D_Occ);
   end Analyze_Decision_Occurrence;

   ---------------------
   -- Analyze_Routine --
   ---------------------

   procedure Analyze_Routine
     (Name  : String_Access;
      Exec  : Exe_File_Acc;
      Insns : Binary_Content)
   is
      PC       : Pc_Type;
      Insn_Len : Natural;

      Current_Basic_Block_Start : Pc_Type;
      --  Start of current basic block

      Current_Basic_Block_S_Map : Statement_Maps.Map;
      --  Statements map of current basic block

      Prev_Insn_S_SCOs, Cur_Insn_S_SCOs : SCO_Sets.Set;
      --  Statement SCOs of the previous and current instructions

      Context : Cond_Branch_Context;

      procedure New_Basic_Block;
      --  Reset state to start processing a new basic block

      procedure Put_Line (S : String; Underline : Character);
      --  Output S, underline with the given character

      ---------------------
      -- New_Basic_Block --
      ---------------------

      procedure New_Basic_Block is
      begin
         Current_Basic_Block_Start := PC;
         Current_Basic_Block_S_Map := Statement_Maps.Empty_Map;
      end New_Basic_Block;

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (S : String; Underline : Character) is
      begin
         Put_Line (S);
         Put_Line (String'(S'Range => Underline));
         New_Line;
      end Put_Line;

      Subp_Name : constant String := Get_Filename (Exec.all) & ":" & Name.all;

      Call_Excluded : Boolean := False;
      --  True for a call to a generated procedure that calls finalizers for
      --  some block.

      Next_Branch_Excluded : Boolean := False;
      --  True for the conditional branch instruction that follows this kind of
      --  call.

      --  These two flags enable to prevent some finalisation-related basic
      --  blocks from being considered as being part of a decision.

      type Pending_Cond_Branch is record
         Insn_First, Insn_Last : Pc_Type;
         --  Conditional branch instruction PC range in Insns

         Tag                   : SC_Tag;
         C_SCO                 : SCO_Id;
         --  SCO and sloc information for this instruction

         Branch_Dest, FT_Dest  : Dest;
         --  Machine properties for this conditional branch

         BB_From               : Pc_Type;
         --  Fisrt byte of the basic block that contain this instruction
      end record;
      --  Information needed to analyze a conditional branch

      package Pending_Cond_Branch_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Pending_Cond_Branch);
      use Pending_Cond_Branch_Vectors;

      Pending_Cond_Branches : Pending_Cond_Branch_Vectors.Vector;
      --  Conditional branches analysis needs to access to all basic blocks.
      --  Thus, it has to be done after having built basic blocks. Conditional
      --  branches are stored in this vector as we discover them when building
      --  basic blocks.

      Cond_Branch_Cur       : Pending_Cond_Branch_Vectors.Cursor;

   --  Start of processing for Analyze_Routine

   begin
      if Branch_Stats then
         Put_Line
           ("Branch statistics for " & Subp_Name, '=');
      elsif Verbose then
         Put_Line ("Building decision map for " & Subp_Name);
      end if;

      --  Iterate over instructions, looking for conditional branches

      PC := Insns'First;
      New_Basic_Block;
      Prev_Insn_S_SCOs.Clear;

      while PC <= Insns'Last loop
         Insn_Len :=
           Disa_For_Machine (Machine).
             Get_Insn_Length (Insns (PC .. Insns'Last));

         declare
            LI   : Line_Info_Access;
            Insn : Binary_Content renames
                     Insns (PC .. PC + Pc_Type (Insn_Len) - 1);

            Branch      : Branch_Kind;
            Flag_Indir  : Boolean;
            Flag_Cond   : Boolean;
            Branch_Dest : Dest;
            FT_Dest     : Dest;
            Tslocs      : constant Tagged_Slocs :=
                            Tag_Provider.Get_Slocs_And_Tags (Exec, PC);
            --  Properties of Insn

         begin
            --  Find lines for this PC, and mark all corresponding statement
            --  SCOs as having code.

            Cur_Insn_S_SCOs.Clear;
            for Tsloc of Tslocs loop
               LI := Get_Line (Tsloc.Sloc);
               if LI /= null then
                  for SCO of LI.SCOs loop
                     if Kind (SCO) = Statement then
                        Set_Basic_Block_Has_Code (SCO, Tsloc.Tag);
                        Cur_Insn_S_SCOs.Include (SCO);
                     end if;
                  end loop;
               end if;
            end loop;

            --  If this instruction has a distinct set of statements from the
            --  previous one, add an entry to the statements map.

            if not Cur_Insn_S_SCOs.Is_Empty
              and then Prev_Insn_S_SCOs /= Cur_Insn_S_SCOs
            then
               Current_Basic_Block_S_Map.Insert (PC, Cur_Insn_S_SCOs);
               Prev_Insn_S_SCOs := Cur_Insn_S_SCOs;
            end if;

            --  Disassemble instruction

            Disa_For_Machine (Machine).Get_Insn_Properties
              (Insn_Bin    => Insn,
               Pc          => PC,
               Branch      => Branch,
               Flag_Indir  => Flag_Indir,
               Flag_Cond   => Flag_Cond,
               Branch_Dest => Branch_Dest,
               FT_Dest     => FT_Dest);

            --  Update decision exclusion flags for this basic block

            --  We expect the next control flow instruction to be a conditional
            --  branch. Reset decision exclusion flags if this is not the case
            --  (i.e. we do not match the expectded pattern: finalization at
            --  the end of a decision).

            Next_Branch_Excluded :=
               Next_Branch_Excluded and then
                  (Branch = Br_None
                     or else
                   (Branch = Br_Jmp and then Flag_Cond));

            Call_Excluded := False;
            if Branch = Br_Call then
               declare
                  Called_Sym : constant Addresses_Info_Acc :=
                    Get_Symbol (Exec.all, Branch_Dest.Target);
                  Sym_Name : String_Access;
               begin
                  if Called_Sym /= null then
                     Sym_Name := Called_Sym.Symbol_Name;
                  else
                     Sym_Name := null;
                  end if;

                  Call_Excluded := Is_Finalizer_Symbol (Sym_Name);
               end;
            end if;

            --  If both edges have the same delay slot address, then said delay
            --  slot is always executed, whether or not we branch, so we
            --  ignore it for the purpose of edge destination equivalence.
            --  We thus treat:

            --     cond-branch tgt
            --     insn (in delay slot)

            --  as equivalent to:

            --     insn
            --     cond-branch tgt
            --     nop (in delay slot)

            if Branch_Dest.Delay_Slot = FT_Dest.Delay_Slot then
               Branch_Dest.Delay_Slot := No_PC;
               FT_Dest.Delay_Slot     := No_PC;
            end if;

            if Branch /= Br_None then
               Analyze_Branch : declare
                  BB : constant Basic_Block :=
                         (From                   => Current_Basic_Block_Start,
                          To_PC                  => Insn'First,
                          To                     => Insn'Last,
                          Branch_Dest            => Branch_Dest,
                          Branch                 => Branch,
                          Cond                   => Flag_Cond,
                          Statements             => Current_Basic_Block_S_Map,
                          Excluded_From_Decision =>
                             Call_Excluded or Next_Branch_Excluded,
                          others      => <>);

                  SCO        : SCO_Id;
                  Tag        : SC_Tag;
                  Branch_SCO : SCO_Id := No_SCO_Id;
                  --  Condition or Statement SCO associated with BB.To_PC, for
                  --  statistics purposes.
                  --  If multiple SCOs are associated with this PC:
                  --    - if one of them is a Condition, it is selected,
                  --    - else an arbitrary statement SCO is selected.
                  --  Note that no two condition SCOs may be associated with
                  --  a given PC.

                  procedure Report_Non_Traceable (Reason : String);
                  --  Emit a diagnostic for a non-traceable
                  --  conditional branch instruction at PC.

                  --------------------------
                  -- Report_Non_Traceable --
                  --------------------------

                  procedure Report_Non_Traceable (Reason : String) is
                  begin
                     Context.Stats.Non_Traceable :=
                       Context.Stats.Non_Traceable + 1;

                     Report
                       ("non-traceable: " & Reason,
                        Exe  => Exec,
                        PC   => BB.To_PC,
                        Sloc => First_Sloc (Branch_SCO),
                        Tag  => Tag,
                        Kind => Warning);
                  end Report_Non_Traceable;

               --  Start of processing for Analyze_Branch

               begin
                  for Tsloc of Tslocs loop
                     SCO := Sloc_To_SCO (Tsloc.Sloc);
                     Tag := Tsloc.Tag;

                     if Flag_Cond and then not BB.Excluded_From_Decision then
                        if Branch = Br_Jmp or else Branch = Br_Ret then
                           if SCO /= No_SCO_Id
                                and then Kind (SCO) = Condition
                           then
                              Pending_Cond_Branches.Append
                                ((Insn_First  => Insn'First,
                                  Insn_Last   => Insn'Last,
                                  Tag         => Tsloc.Tag,
                                  C_SCO       => SCO,
                                  Branch_Dest => Branch_Dest,
                                  FT_Dest     => FT_Dest,
                                  BB_From     => BB.From));

                              --  Assumption: a given conditional branch
                              --  instruction tests at most 1 source condition.

                              if Branch_SCO /= No_SCO_Id
                                and then Kind (Branch_SCO) = Condition
                              then
                                 Report_Non_Traceable
                                   ("multiple conditions for conditional "
                                    & "branch");
                              end if;
                              Branch_SCO := SCO;
                           end if;

                        else
                           --  Warn if conditional call or conditional return
                           --  (such combinations are not supported).

                           Report_Non_Traceable
                             ("unexpected conditional branch of type "
                              & Branch'Img);
                        end if;
                     end if;

                     if Branch_SCO = No_SCO_Id and then SCO /= No_SCO_Id then
                        Branch_SCO := SCO;
                     end if;
                  end loop;

                  Context.Basic_Blocks.Insert (BB);

                  --  Update statistics

                  if Branch_Stats then
                     declare
                        SK : constant Any_Statement_Kind :=
                          S_Kind (Enclosing_Statement (Branch_SCO));
                     begin
                        Context.Stats.Branch_Counts (Branch, SK, Flag_Cond) :=
                          Context.Stats.Branch_Counts (Branch, SK, Flag_Cond)
                          + 1;
                     end;

                     if Flag_Cond then
                        declare
                           CBK : Cond_Branch_Kind;

                        begin
                           if Branch_SCO = No_SCO_Id then
                              CBK := None;
                              Report_Non_Traceable ("no SCO");

                           elsif BB.Condition = No_SCO_Id then
                              CBK := Statement;

                              if S_Kind (Branch_SCO) /= For_Loop_Statement then
                                 Report_Non_Traceable
                                   ("cond branch for "
                                    & S_Kind (Branch_SCO)'Img);
                              end if;
                           else
                              declare
                                 CBI : constant Cond_Branch_Info :=
                                   Cond_Branch_Map.Element ((Exec, BB.To_PC));
                              begin
                                 if CBI.Edges (Decision_Map.Branch).Dest_Kind
                                   = Raise_Exception
                                   or else CBI.Edges (Fallthrough).Dest_Kind
                                   = Raise_Exception
                                 then
                                    CBK := Check;
                                 else
                                    CBK := Condition;

                                    for J in CBI.Edges'Range loop
                                       if CBI.Edges (J).Next_Condition
                                         = Index (BB.Condition)
                                       then
                                          Report_Non_Traceable
                                            (J'Img
                                             & " edge remains in condition");
                                       end if;
                                    end loop;
                                 end if;
                              end;
                           end if;
                           Context.Stats.Cond_Branch_Counts (CBK) :=
                             Context.Stats.Cond_Branch_Counts (CBK) + 1;

                        end;
                     end if;
                  end if;
               end Analyze_Branch;
            end if;

            --  Propagate the flag (when True) to the next instruction unless
            --  it is a branch (i.e. until the end of the next basic block).

            Next_Branch_Excluded :=
               (Next_Branch_Excluded and then Branch = Br_None)
                  or else
               Call_Excluded;

            PC := PC + Pc_Type (Insn_Len);

            --  Handle case where PC wraps

            exit when PC = 0;

            if Branch /= Br_None then
               New_Basic_Block;
            end if;
         end;
      end loop;

      --  Analyze pending conditional branches

      Cond_Branch_Cur := Pending_Cond_Branches.First;
      while Cond_Branch_Cur /= Pending_Cond_Branch_Vectors.No_Element loop
         declare
            Cond_Branch : Pending_Cond_Branch renames
               Element (Cond_Branch_Cur);

            BB_Cur      : constant Basic_Block_Sets.Cursor :=
               Context.Basic_Blocks.Find
                 ((Cond_Branch.BB_From, others => <>));
            BB          : Basic_Block := Basic_Block_Sets.Element (BB_Cur);
            --  Given that this conditional branch has been found when building
            --  basic blocks, there *must* be one and only one basic block for
            --  it.

         begin
            Analyze_Conditional_Branch
              (Exec        => Exec,
               Insn        => Insns
                                (Cond_Branch.Insn_First ..
                                 Cond_Branch.Insn_Last),
               Tag         => Cond_Branch.Tag,
               C_SCO       => Cond_Branch.C_SCO,
               Branch_Dest => Cond_Branch.Branch_Dest,
               FT_Dest     => Cond_Branch.FT_Dest,
               Ctx         => Context,
               BB          => BB);

            --  Update the basic block in the context: BB is an "out" argument
            --  for Analyze_Conditional_Branch, so fields that are not part of
            --  the key may have changed.

            Context.Basic_Blocks.Replace_Element (BB_Cur, BB);
            Cond_Branch_Cur := Next (Cond_Branch_Cur);
         end;
      end loop;

      --  Flush pending decisions

      for J in reverse Context.Decision_Stack.First_Index
                    .. Context.Decision_Stack.Last_Index
      loop
         Analyze_Decision_Occurrence
           (Exec, Context, Context.Decision_Stack.Element (J));
      end loop;

      --  Report statistics

      if Branch_Stats then
         declare
            First : Boolean;
            Count : Natural;
         begin
            --  Add blank lines if diagnosis about non traceable cond branches
            --  have been emitted.

            if Context.Stats.Non_Traceable > 0 then
               New_Line;
            end if;

            Put_Line ("Summary by branch kind", '-');
            for J in Context.Stats.Branch_Counts'Range (1) loop
               First := True;
               for K in Context.Stats.Branch_Counts'Range (2) loop
                  Count := Context.Stats.Branch_Counts (J, K, False)
                         + Context.Stats.Branch_Counts (J, K, True);
                  if Count > 0 then
                     if First then
                        Put_Line ("  "
                                  & (case J is
                                       when Br_None => "not a branch     ",
                                       when Br_Call => "subprogram call  ",
                                       when Br_Ret  => "subprogram return",
                                       when Br_Jmp  => "simple branch    "));
                        First := False;
                     end if;

                     Put ("    " & K'Img & Count'Img);
                     if Context.Stats.Branch_Counts (J, K, True) > 0 then
                        Put
                          (" ("
                           & Img (Context.Stats.Branch_Counts (J, K, True))
                           & " conditional)");
                     end if;
                     New_Line;

                  end if;
               end loop;
            end loop;
            New_Line;

            Put_Line ("Conditional branches", '-');
            for J in Context.Stats.Cond_Branch_Counts'Range loop
               if Context.Stats.Cond_Branch_Counts (J) > 0 then
                  Put_Line
                    (" "
                     & (case J is
                       when None      => "no SCO       ",
                       when Statement => "statement    ",
                       when Condition => "condition    ",
                       when Check     => "runtime check")
                     & Context.Stats.Cond_Branch_Counts (J)'Img);
               end if;
            end loop;
            New_Line;

            Put_Line
              (Img (Context.Stats.Non_Traceable)
               & " non-traceable conditional branches reported");
         end;
      end if;
   end Analyze_Routine;

   --------------------------------
   -- Append_Decision_Occurrence --
   --------------------------------

   procedure Append_Decision_Occurrence (D_Occ : Decision_Occurrence_Access) is

      procedure Update_Element
        (SCO : SCO_Id;
         V   : in out Decision_Occurrence_Vectors.Vector);
      --  Append D_Occ to V

      --------------------
      -- Update_Element --
      --------------------

      procedure Update_Element
        (SCO : SCO_Id;
         V   : in out Decision_Occurrence_Vectors.Vector)
      is
         pragma Unreferenced (SCO);
      begin
         V.Append (D_Occ);
      end Update_Element;

      use Decision_Occurrence_Maps;
      Cur : constant Cursor := Decision_Occurrence_Map.Find (D_Occ.Decision);

   --  Start of processing for Append_Decision_Occurrence

   begin
      if Cur = Decision_Occurrence_Maps.No_Element then
         Decision_Occurrence_Map.Insert
           (Key      => D_Occ.Decision,
            New_Item => Decision_Occurrence_Vectors.To_Vector (D_Occ, 1));
      else
         Decision_Occurrence_Map.Update_Element (Cur, Update_Element'Access);
      end if;
   end Append_Decision_Occurrence;

   ------------------------
   -- Build_Decision_Map --
   ------------------------

   procedure Build_Decision_Map (Exec_Name    : String;
                                 Text_Start   : Pc_Type;
                                 Map_Filename : String)
   is
      Exec : Exe_File_Acc;

   --  Start of processing for Build_Decision_Map

   begin
      Open_Exec (Exec_Name, Text_Start, Exec);

      Init_Base (Decision_Map_Base);
      Analyze (Exec);
      Decision_Map.Write_Map (Map_Filename);

      Close_File (Exec.all);
   end Build_Decision_Map;

   ----------------------
   -- Find_Basic_Block --
   ----------------------

   function Find_Basic_Block
     (Basic_Blocks : Basic_Block_Sets.Set;
      PC           : Pc_Type) return Basic_Block
   is
      use Basic_Block_Sets;
      PC_Block : constant Basic_Block := (From => PC, others => <>);
      Cur : constant Cursor := Basic_Blocks.Floor (PC_Block);
   begin
      if Cur /= No_Element and then PC <= Element (Cur).To then
         return Element (Cur);
      else
         return No_Basic_Block;
      end if;
   end Find_Basic_Block;

   -----------
   -- Image --
   -----------

   function Image (BB : Basic_Block) return String is
      Cond_Char : constant array (Boolean) of Character :=
                    (False => ' ', True => '?');
   begin
      return Hex_Image (BB.From) & "-" & Hex_Image (BB.To)
               & " " & BB.Branch'Img & Cond_Char (BB.Cond) & " "
               & Hex_Image (BB.Branch_Dest.Target);
   end Image;

   ---------------
   -- Write_Map --
   ---------------

   procedure Write_Map (Filename : String) is
      Trace_File : Trace_File_Type;
   begin
      Create_Trace_File (Qemu_Traces.Decision_Map, Trace_File);
      Write_Trace_File (Filename, Trace_File, Decision_Map_Base);
   end Write_Map;

end Decision_Map;
