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

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Regexp;  use GNAT.Regexp;
with GNAT.Strings; use GNAT.Strings;

with Interfaces; use Interfaces;

with System.Storage_Elements;

with Binary_Files;      use Binary_Files;
with Coverage.Source;   use Coverage.Source;
with Coverage_Options;  use Coverage_Options;
with Diagnostics;       use Diagnostics;
with Elf_Common;
with Elf_Disassemblers; use Elf_Disassemblers;
with Execs_Dbase;       use Execs_Dbase;
with Disa_Symbolize;
with Files_Table;       use Files_Table;
with Hex_Images;        use Hex_Images;
with Highlighting;
with Qemu_Traces;
with Slocs;             use Slocs;
with Strings;           use Strings;
with Switches;          use Switches;
with Traces_Dbase;      use Traces_Dbase;
with Traces_Files;      use Traces_Files;
with Traces_Names;      use Traces_Names;

package body Decision_Map is

   --  This unit instantiates containers and we want to avoid too much
   --  performance cost when using references to their elements, so suppress
   --  tampering checks.

   pragma Suppress (Tampering_Check);

   use Ada.Containers;
   use Coverage;
   use all type Unbounded_String;

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

   function First_CBI_PC (D_Occ : Decision_Occurrence_Access) return Pc_Type;
   --  Return the PC of the first conditional branch instruction in D_Occ.
   --  Used as unique identifier for occurrences.

   type Call_Kind is (Normal, Raise_Exception, Finalizer, Finalization_Exp);
   --  Classification of calls within a decision:
   --    - normal calls to subprograms
   --    - calls that are known to raise an exception
   --    - calls to generated block finalizers / cleanup code
   --    - calls to finalization expansion symbols - excluding finalizers
   --
   --  A basic block in object code

   package Outcome_Reached_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Pc_Type,
      Element_Type => Boolean);
   --  Map of decision occurrence (identified by PC of first conditional
   --  branch instruction) to Outcome_Reached status (see below).

   type Basic_Block is record
      From, To_PC, To  : Pc_Type := No_PC;
      --  Start and end addresses (note: To is not necessarily a valid PC value
      --  but instead the address of the last byte in the last instruction of
      --  the BB, whose first byte is at To_PC).

      --  Properties of the branch instruction at the end of the basic block:

      Branch_Dest, FT_Dest   : Dest := (Target => No_PC, Delay_Slot => No_PC);
      --  Branch and fallthrough destinations

      Branch                 : Branch_Kind := Br_None;
      --  Branch kind

      Cond                   : Boolean := False;
      --  True if conditional branch

      First_Cond             : Boolean := False;
      --  True if Cond and this is the first conditional branch in the
      --  enclosing decision occurrence.

      Call                   : Call_Kind := Normal;
      Called_Sym             : String_Access;
      --  If Branch = Br_Call, information about the called subprogram

      Condition              : SCO_Id := No_SCO_Id;
      --  If this is a conditional branch testing a condition, identifies it

      Branch_SCO             : SCO_Id := No_SCO_Id;
      --  Condition or Statement SCO for To_PC, for statistics purposes

      --  If multiple SCOs are associated with this PC:
      --    - if one of them is a Condition, it is selected (in which case
      --      BB.Branch_SCO = BB.Condition)
      --    - else an arbitrary statement SCO is selected.

      --  Note that no two condition SCOs may be associated with a given PC.

      Outcome_Reached        : Outcome_Reached_Maps.Map;
      --  Set True for basic blocks that are reached after the outcome of the
      --  enclosing decision is determined: subsequent conditional branch
      --  instructions in the decision occurrence must be excluded from
      --  coverage analysis. Set False for basic blocks that are known to be
      --  reachable while the outcome is not determined yet. Note that this
      --  is computed per decision occurrence, because if two occurrences
      --  of the same decision appear in close succession (e.g. inlined one
      --  after the other, or possibly as part of an unrolled loop), the
      --  initial (pre-outcome) basic block of the second occurrence might
      --  need to be marked as a post-outcome block for the first one.

      Jump_Only              : Boolean := False;
      --  Set True for basic blocks that are a singleton unconditional branch.
      --  The compiler creates such basic blocks only to hold source locations:
      --  they can be useful for decision mapping heuristics, but can also be
      --  skipped to analyze the destination BB if needed (see
      --  Label_From_Other).
   end record;

   No_Basic_Block : constant Basic_Block := (others => <>);

   function "<" (L, R : Basic_Block) return Boolean;
   --  Order by From

   Finalizer_Symbol_Pattern : constant Regexp := Compile
     (".*___finalizer\.[0-9]+");
   Pre_Finalizer_Symbol_Pattern : constant Regexp := Compile
     ("system__finalization_primitives__.*");

   package Pc_Sets is new Ada.Containers.Ordered_Sets (Pc_Type);

   package Basic_Block_Sets is new Ada.Containers.Ordered_Sets (Basic_Block);

   function Find_Basic_Block
     (Basic_Blocks : Basic_Block_Sets.Set;
      PC           : Pc_Type) return Basic_Block_Sets.Cursor;
   function Find_Basic_Block
     (Basic_Blocks : Basic_Block_Sets.Set;
      PC           : Pc_Type) return Basic_Block;
   --  Return the basic block containing PC from the given set, or
   --  No_Element / No_Basic_Block if none.

   type Branch_Count_Array is
     array (Branch_Kind, Any_Statement_Kind, Boolean) of Natural;
   --  Branch counts by branch kind and, for branches associated with a
   --  statement SCO, statement kind. The third dimension discriminates
   --  between conditional and non-conditional branches.

   type Cond_Branch_Kind is (None, Statement, Condition, Check, Cleanup);
   --  Statistics category for a conditional branch instruction:
   --    * no SCO
   --    * statement SCO
   --    * condition SCO, non-exception
   --    * condition SCO, exception
   --    * cleanup actions after outcome has been determined

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

      Subprg         : Address_Info_Acc;
      --  Info of enclosing subprogram
   end record;

   procedure Analyze_Routine
     (Name  : String_Access;
      Exec  : Exe_File_Acc;
      Insns : Binary_Content);
   --  Build decision map for the given subprogram

   procedure Analyze_Conditional_Branch
     (Exec        : Exe_File_Acc;
      Insn        : Binary_Content;
      C_SCO       : SCO_Id;
      Branch_Dest : Dest;
      FT_Dest     : Dest;
      Ctx         : in out Cond_Branch_Context;
      BB          : in out Basic_Block);
   --  Process one conditional branch instruction for the given condition SCO.
   --  Sets BB.Condition to C_SCO, if applicable.

   procedure Analyze_Call (Exe : Exe_File_Acc; BB : in out Basic_Block);
   --  Set information about the call/ret instruction at the end of BB

   procedure Skip_Constant_Conditions
     (Cond    : in out SCO_Id;
      Outcome : out Tristate;
      Skipped : access SCO_Sets.Set);
   --  Set Cond to the next runtime condition starting at Cond (included) and
   --  Outcome to Unknown. If there is no runtime condition before reaching an
   --  outcome, set Cond to No_SCO_Id and Outcome to the known outcome. Store
   --  the SCO of skipped conditions in Skipped.

   function Is_Expected_First_Condition
     (Decision  : SCO_Id;
      Condition : SCO_Id) return Boolean;
   --  Return whether Condition can be the first condition to be evaluated at
   --  runtime for Decision.

   function Is_Last_Runtime_Condition
     (D_Occ : Decision_Occurrence_Access) return Boolean;
   --  Return whether the last condition seen after analyzing conditional
   --  branches is the last one to be evaluated at runtime.

   procedure Analyze_Decision_Occurrence
     (Exe   : Exe_File_Acc;
      Ctx   : in out Cond_Branch_Context;
      D_Occ : Decision_Occurrence_Access);
   --  Perform logical structure analysis of the given decision occurrence

   procedure Append_Decision_Occurrence (D_Occ : Decision_Occurrence_Access);
   --  Record association of D_Occ with its decision

   function Image (BB : Basic_Block) return String;
   pragma Unreferenced (Image);
   --  For debugging purposes

   procedure Write_Map (Filename : String);
   --  Write the contents of the decision map to the named file

   function Check_Possible_Successor
     (D_SCO          : SCO_Id;
      This_Condition : Any_Condition_Index;
      Next_Condition : Condition_Index) return Tristate;
   --  Determine whether Next_Condition is a valid successor of This_Condition
   --  in the given decision, and if so, return the associated origin (i.e.
   --  the associated valuation of This_Condition). If not, return Unknown.
   --  This_Condition may be No_Condition_Index, in which case we check
   --  whether Next_Condition is a valid first condition to be tested
   --  (and return Unknown iff it's not).

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

   -------------
   -- Analyze --
   -------------

   procedure Analyze (Exe_File : Exe_File_Acc) is
      Sym_It : Addresses_Iterator;
      Sym    : Address_Info_Acc;
      Sec    : Address_Info_Acc;

      First_Symbol_Occurrence : Boolean;
      Subp_Key                : Subprogram_Key;
      Subp_Info               : Subprogram_Info;
   begin
      --  Do not map decisions more than once: although it is correct to run
      --  this more than once per executable, this is a time consuming
      --  operation.

      if Exe_File.Has_Decision_Mapped then
         return;
      end if;

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
               Sec.Section_Sec_Idx,
               Slice (Sec.Section_Content, Sym.First, Sym.Last),
               First_Symbol_Occurrence,
               Subp_Info);

            --  Process the reference instance of the routine

            if First_Symbol_Occurrence then

               --  Map routine instructions to SCOs

               Analyze_Routine
                 (Sym.Symbol_Name,
                  Exe_File,
                  Slice (Sec.Section_Content, Sym.First, Sym.Last));
            end if;
         end if;
      end loop;

      Exe_File.Set_Decision_Mapped;
   end Analyze;

   ---------------------------
   -- Subp_Raises_Exception --
   ---------------------------

   function Subp_Raises_Exception (Symbol_Name : String) return Boolean is
   begin
      return
        (Symbol_Name = "__gnat_last_chance_handler"
         or else
         Symbol_Name = "system__assertions__raise_assert_failure"
         or else
         Has_Prefix (Symbol_Name, Prefix => "__gnat_rcheck_"));
   end Subp_Raises_Exception;

   ------------------
   -- Analyze_Call --
   ------------------

   procedure Analyze_Call (Exe : Exe_File_Acc; BB : in out Basic_Block) is
      pragma Assert (BB.Branch = Br_Call);

      Sym : constant Address_Info_Acc :=
        Get_Symbol (Exe.all, BB.Branch_Dest.Target);

   begin
      if Sym /= null then
         BB.Called_Sym := Sym.Symbol_Name;
      end if;

      if BB.Called_Sym = null then
         return;
      end if;

      declare
         Sym_Name : constant String :=
            Platform_Independent_Symbol (BB.Called_Sym.all, Exe.all);

      begin
         if Sym_Name = "ada__exceptions__triggered_by_abort" then
            BB.Call := Finalizer;

         elsif Subp_Raises_Exception (Sym_Name) then
            BB.Call := Raise_Exception;

         elsif Is_Finalizer_Symbol (Sym_Name) then
            BB.Call := Finalizer;

         elsif Is_Finalization_Expansion_Symbol (Sym_Name) then
            BB.Call := Finalization_Exp;
         end if;
      end;

      --  If call is known to never return, clear out falltrhough destination

      if BB.Call = Raise_Exception then
         BB.FT_Dest := (No_PC, No_PC);
      end if;
   end Analyze_Call;

   --------------------------------
   -- Analyze_Conditional_Branch --
   --------------------------------

   procedure Analyze_Conditional_Branch
     (Exec        : Exe_File_Acc;
      Insn        : Binary_Content;
      C_SCO       : SCO_Id;
      Branch_Dest : Dest;
      FT_Dest     : Dest;
      Ctx         : in out Cond_Branch_Context;
      BB          : in out Basic_Block)
   is
      pragma Assert (Kind (C_SCO) = Condition);

      D_SCO : constant SCO_Id := Enclosing_Decision (C_SCO);

      function Is_Begin_Handler_Call (D : Dest) return Boolean;
      --  True if Dest branches to a basic block ending in a
      --  __gnat_begin_handler call.

      ---------------------------
      -- Is_Begin_Handler_Call --
      ---------------------------

      function Is_Begin_Handler_Call (D : Dest) return Boolean is
         BB : constant Basic_Block :=
           Find_Basic_Block (Ctx.Basic_Blocks, D.Target);
      begin
         return BB /= No_Basic_Block
           and then BB.Branch = Br_Call
           and then BB.Called_Sym /= null
           and then Platform_Independent_Symbol (BB.Called_Sym.all, Exec.all)
                      in "__gnat_begin_handler" | "__gnat_begin_handler_v1";
      end Is_Begin_Handler_Call;

   --  Start of processing for Analyze_Conditional_Branch

   begin
      --  If one of the edges branches to a __gnat_begin_handler
      --  call, then this conditional branch is an exception
      --  dispatch test, and does not contribute to any decision.

      if Is_Begin_Handler_Call (Branch_Dest)
           or else
         Is_Begin_Handler_Call (FT_Dest)
      then
         Report
           (Exec, Insn.First, "exception dispatch", Kind => Notice);
         return;
      end if;

      --  Record address in SCO descriptor

      Add_Address (C_SCO, Insn.First);

      --  Update control flow information

      Process_Condition :
      declare
         Parent_SCO : SCO_Id;
         --  Parent SCO of D_SCO, if appropriate

         Enclosing_D_SCO : SCO_Id;
         --  For a nested decision, the enclosing decision

         Enclosing_Inlined_Body : Address_Info_Acc;
         --  For a condition occurring in an inlined body, descriptor of that
         --  body.

         Cond_Index : constant Condition_Index := Index (C_SCO);
         --  Index of C_SCO in D_SCO

         Starting_Evaluation : constant Boolean :=
           Is_Expected_First_Condition
             (D_SCO, Condition (D_SCO, Cond_Index));
         --  True if this condition can be the first one evaluated in
         --  its decision.

         DS_Top : Decision_Occurrence_Access;
         --  Innermost currently open decision evaluation

         function Is_Expected_Condition
           (CI                   : Condition_Index;
            Report_If_Unexpected : Boolean := False) return Boolean;
         --  Check whether we expect to evaluate CI: either we remain in the
         --  current condition (case of a condition that requires multiple
         --  branches), or we move to the next one..

         ---------------------------
         -- Is_Expected_Condition --
         ---------------------------

         function Is_Expected_Condition
           (CI                   : Condition_Index;
            Report_If_Unexpected : Boolean := False) return Boolean
         is
            Current_CI  : Condition_Index renames DS_Top.Seen_Condition;

         begin
            if
              --  Case of remaining in the current evaluation, or starting a
              --  new one if there's none in progress.

              CI = Condition_Index'Max (Current_CI, 0)

              --  Else the next condition is reachable through the fallthrough
              --  edge of the current condition, so it must be a possible
              --  successor.

                or else
              Check_Possible_Successor
                (DS_Top.Decision,
                 This_Condition => Current_CI,
                 Next_Condition => CI) /= Unknown

            then
               return True;
            end if;

            if Report_If_Unexpected then
               declare
                  Msg : Unbounded_String;
               begin
                  Msg :=
                    +("unexpected condition" & CI'Img
                      & " in decision " & Image (DS_Top.Decision));

                  --  This could correspond to some finalization code, that has
                  --  a debug info code location corresponding to a condition.
                  --  We will silence it unless explicitely requested with a
                  --  verbose mode.

                  Report (Exec, Insn.First, +Msg, Kind => Notice);
               end;
            end if;
            return False;
         end Is_Expected_Condition;

      --  Start of processing for Process_Condition

      begin
         --  Determine enclosing SCO

         Parent_SCO := Parent (D_SCO);

         if Parent_SCO /= No_SCO_Id
           and then Kind (Parent_SCO) = Condition
         then
            Enclosing_D_SCO := Enclosing_Decision (Parent_SCO);
         else
            Enclosing_D_SCO := No_SCO_Id;
         end if;

         --  Determine innermost enclosing inlined body

         Enclosing_Inlined_Body :=
           Get_Address_Info
             (Exec.all, Inlined_Subprogram_Addresses, Insn.First);

         --  Flush completed decisions from the Decision_Stack

         while Ctx.Decision_Stack.Length > 0 loop
            DS_Top := Ctx.Decision_Stack.Last_Element;
            exit when DS_Top.Decision = D_SCO
              and then DS_Top.Inlined_Body = Enclosing_Inlined_Body
              and then Is_Expected_Condition (Cond_Index);

            if DS_Top.Decision = Enclosing_D_SCO then
               --  Here if the parent of our decision is part of a
               --  condition in another decision, and DS_Top is that
               --  enclosing decision.

               if not Is_Expected_Condition (Index (Parent_SCO),
                                             Report_If_Unexpected => True)
               then
                  return;
               end if;

               DS_Top := null;
               exit;
            end if;

            --  If the condition being tested is the first of its decision,
            --  then we may be starting a new decision occurrence: determine
            --  whether it is nested in the current one.

            if Starting_Evaluation then
               if DS_Top.Seen_Condition = DS_Top.Last_Cond_Index
                 and then DS_Top.Last_Cond_Index > 0
               then
                  --  Previous evaluation is complete: pop it

                  null;

               elsif DS_Top.Inlined_Body /= null
                 and then Insn.First not in DS_Top.Inlined_Body.First
                                         .. DS_Top.Inlined_Body.Last
               then
                  --  Exited inlined body of previous evaluation: pop it

                  null;

               else
                  --  Nested decision: remain in current evaluation

                  exit;
               end if;

            elsif (DS_Top.Inlined_Body = null
                   and then Enclosing_Inlined_Body /= null)
              or else
                (DS_Top.Inlined_Body /= null
                 and then Enclosing_Inlined_Body /= DS_Top.Inlined_Body
                 and then Insn.First in DS_Top.Inlined_Body.First
                                     .. DS_Top.Inlined_Body.Last)
            then
               --  Entering an inlined body: do not presume that the current
               --  evaluation is completed.

               exit;

               --  Check whether call site loc is within current decision???
               --  Difficulty: case of nested inlined calls, we need to find
               --  the call site for the outermost call in that case???

            end if;

            --  Otherwise pop completed evaluations from the stack until
            --  we find the relevant pending one.

            Analyze_Decision_Occurrence (Exec, Ctx, DS_Top);
            Ctx.Decision_Stack.Delete_Last;
            DS_Top := null;
         end loop;

         --  Check if we are evaluating part of a parent decision already on
         --  the decision stack, and point DS_Top at that decision if so. This
         --  is not necessarily the immediately enclosing decision, in
         --  particular with complex CFGs resulting from the use of quantified
         --  expressions.
         --
         --  Typically, in the example sketched below:
         --
         --                                <  D5  >
         --       --------------------------------------------------------
         --  R := (for all x ... => P(x)) and then (for all x ... => Q(x))
         --                         ^^^^
         --                         D9
         --                         C10
         --       ----------------------           -----------------------
         --        C7 (1st cond of D5)              C8 (2nd cond of D5)
         --
         --  We could well see
         --  * A first branch for C7, starting D5
         --  * A branch for C10, starting D9, then
         --  * Another branch for C7, still for D5, implementing
         --    part of the first for-all control flow.
         --
         --  If we overlook ancestors and start a new decision occurrence
         --  everytime we encounter a condition not part of the being-analyzed
         --  decision, then we will start two occurrences of the decision 5,
         --  which is not what we want.

         for D_Occ of Ctx.Decision_Stack loop
            if D_Occ.Decision = D_SCO then
               DS_Top := D_Occ;
            end if;
         end loop;

         --  Push a new occurrence on the evaluation stack, if needed

         if
           --  No pending evaluation

           DS_Top = null

           --  Evaluating a new, different decision than an enclosing one

           or else DS_Top.Decision /= D_SCO

           --  Start of evaluation in a new inlined body: cannot be the same
           --  decision occurrence.

           or else (Starting_Evaluation
                    and then DS_Top.Inlined_Body /= Enclosing_Inlined_Body)

         then
            declare
               function Enclosing_Inlined_Body_Image return String;
               --  Return information about enclosing inlined body if in one,
               --  else null string.

               ----------------------------------
               -- Enclosing_Inlined_Body_Image --
               ----------------------------------

               function Enclosing_Inlined_Body_Image return String is
               begin
                  if Enclosing_Inlined_Body /= null then
                     return " in inlined call from "
                       & Image (Enclosing_Inlined_Body.Call_Sloc)
                       & " ("
                       & Hex_Image (Enclosing_Inlined_Body.First)
                       & ".." & Hex_Image (Enclosing_Inlined_Body.Last) & ")";
                  else
                     return "";
                  end if;
               end Enclosing_Inlined_Body_Image;

            begin
               Report (Exec, Insn.First,
                       "starting occurrence"
                       & Enclosing_Inlined_Body_Image,
                       SCO  => D_SCO,
                       Kind => Notice);
            end;

            DS_Top := new Decision_Occurrence'
              (Last_Cond_Index => Last_Cond_Index (D_SCO),
               Decision        => D_SCO,
               Inlined_Body    => Enclosing_Inlined_Body,
               others          => <>);

            if not Is_Expected_Condition (Cond_Index,
                                          Report_If_Unexpected => True)
            then
               return;
            end if;
            Ctx.Decision_Stack.Append (DS_Top);
         end if;

         --  Here after pushing context for current decision, if needed

         pragma Assert (DS_Top.Decision = D_SCO);

         --  Record condition occurrence

         Report
           (Exec, Insn.First,
            "cond branch for " & Image (C_SCO)
            & " (" & Img (Integer (Index (C_SCO))) & ")",
            Kind => Notice);
         pragma Assert (BB.Condition = No_SCO_Id);
         BB.Condition := C_SCO;

         if Cond_Index > DS_Top.Seen_Condition then
            DS_Top.Seen_Condition := Cond_Index;
         end if;

         DS_Top.Conditional_Branches.Append (Insn.First);

         Cond_Branch_Map.Insert
           ((Exec, Insn.First),
            Cond_Branch_Info'
              (Last_PC             => Insn.Last,
               Decision_Occurrence => DS_Top,
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

   ------------------------------
   -- Skip_Constant_Conditions --
   ------------------------------

   procedure Skip_Constant_Conditions
     (Cond    : in out SCO_Id;
      Outcome : out Tristate;
      Skipped : access SCO_Sets.Set)
   is
      Next_Cond  : SCO_Id;
      Cond_Value : Tristate;

   begin
      if Cond = No_SCO_Id then
         Outcome := Unknown;
         return;
      end if;

      loop
         Cond_Value := SC_Obligations.Value (Cond);
         if Cond_Value = Unknown then

            --  Condition tested at run time

            Outcome := Unknown;
            return;

         else
            --  Condition with compile time known value: skip

            if Skipped /= null then
               Skipped.Include (Cond);
            end if;

            Next_Cond := Next_Condition (Cond, To_Boolean (Cond_Value));
            if Next_Cond = No_SCO_Id then

               --  No successor: outcome reached

               Outcome := SC_Obligations.Outcome
                 (Cond, To_Boolean (Cond_Value));
               Cond := No_SCO_Id;
               return;

            else
               --  Continue by jumping to its (only) successor

               Cond := Next_Cond;
            end if;
         end if;
      end loop;
   end Skip_Constant_Conditions;

   ---------------------------------
   -- Is_Expected_First_Condition --
   ---------------------------------

   function Is_Expected_First_Condition
     (Decision  : SCO_Id;
      Condition : SCO_Id) return Boolean
   is
      use SCO_Sets;

      First_Condition           : SCO_Id :=
        SC_Obligations.Condition (Decision, 0);
      Outcome                   : Tristate;
      Possible_First_Conditions : aliased SCO_Sets.Set;

   begin
      Skip_Constant_Conditions
        (First_Condition, Outcome, Possible_First_Conditions'Access);
      return Condition = First_Condition
                or else
             Possible_First_Conditions.Contains (Condition);
   end Is_Expected_First_Condition;

   -------------------------------
   -- Is_Last_Runtime_Condition --
   -------------------------------

   function Is_Last_Runtime_Condition
     (D_Occ : Decision_Occurrence_Access) return Boolean
   is
      CI_SCO  : SCO_Id;
      Outcome : Tristate;
   begin
      --  If this is the last condition in the whole decision, so it is
      --  obviously acceptable.

      if D_Occ.Last_Cond_Index = D_Occ.Seen_Condition then
         return True;
      end if;

      --  When the last conditions are constant, the last seen condition can be
      --  different that the last decision condition. Return whether there is
      --  at least one runtime condition between the last seen condition and
      --  skipping constants conditions.

      for Value in Boolean'Range loop
         CI_SCO := Next_Condition
           (Condition (D_Occ.Decision, D_Occ.Seen_Condition),
            Value);
         Skip_Constant_Conditions (CI_SCO, Outcome, null);
         if CI_SCO /= No_SCO_Id then
            return False;
         end if;
      end loop;

      --  We reached the outcome by both original's condition outgoing
      --  edges without meeting any runtime condition, thus the last seen
      --  condition was the last runtime one.

      return True;
   end Is_Last_Runtime_Condition;

   ---------------------------------
   -- Analyze_Decision_Occurrence --
   ---------------------------------

   procedure Analyze_Decision_Occurrence
     (Exe   : Exe_File_Acc;
      Ctx   : in out Cond_Branch_Context;
      D_Occ : Decision_Occurrence_Access)
   is
      First_Seen_Condition_PC : constant Pc_Type :=
                                  D_Occ.Conditional_Branches.First_Element;
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

      function Get_CBE (KD : Known_Destination) return Cond_Edge_Info;
      --  Return the edge information associated to KD

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
      --  but another edge with the same destination is, copy its information.

      function Label_From_BB
        (Cond_Branch_PC : Pc_Type;
         CBI            : Cond_Branch_Info;
         Edge           : Edge_Kind) return Tristate;
      --  Helper for the third pass of control flow analysis: Edge must be an
      --  unknown outcome. If there is another edge that is a known outcome and
      --  that points to some previous instruction in the same basic block,
      --  return the value of its outcome. Return Unknown otherwise.

      procedure Set_Known_Origin
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info;
         Edge           : Edge_Kind;
         Origin         : Boolean);
      --  Qualify CBE as corresponding to valuation Origin of the condition
      --  tested by the conditional branch at Cond_Branch_PC, described by CBI.

      procedure Record_Known_Destination
        (Cond_Branch_PC : Pc_Type;
         Destination    : Dest;
         Edge           : Edge_Kind);
      --  Once this destination has been fully qualified, remember it so
      --  that further tests with the same destination can reuse the
      --  information.

      procedure Label_Destinations
        (CB_Loc : Cond_Branch_Loc;
         CBI    : in out Cond_Branch_Info);
      --  Identify destination kind of each edge of CBI using information local
      --  to CBI.

      procedure Output_Cond_Branch
        (Exe   : Exe_File_Acc;
         CB_PC : Pc_Type);
      --  Output decision map entry for the conditional branch instruction at
      --  CB_PC in Exe. Report unlabeled destinations as we go. Remove the
      --  instruction from the Cond_Branch_Map if not contributive to coverage
      --  analysis.

      function Decision_Of_Jump (Jump_PC : Pc_Type) return SCO_Id;
      --  Return the SCO for the decision containing Jump_PC, if any

      Has_Valuation : array (Condition_Index'First .. D_Occ.Last_Cond_Index,
                             Boolean range False .. True) of Boolean :=
                               (others => (others => False));
      --  For each valuation of each condition, indicates whether there is
      --  one edge corresponding to each possible valuation of the condition.

      First_Cond_Branch : Boolean;
      --  Set True when Label_Destinations is called for the first conditional
      --  branch instruction in the decision occurrence.

      ----------------------
      -- Decision_Of_Jump --
      ----------------------

      function Decision_Of_Jump (Jump_PC : Pc_Type) return SCO_Id is
         D_SCO_For_Jump : SCO_Id;
      begin
         D_SCO_For_Jump :=
           Sloc_To_SCO (Get_Sloc (Ctx.Subprg.Lines, Jump_PC),
                        Include_Decisions => True);
         if D_SCO_For_Jump /= No_SCO_Id
              and then Kind (D_SCO_For_Jump) = Condition
         then
            D_SCO_For_Jump := Enclosing_Decision (D_SCO_For_Jump);
         end if;
         return D_SCO_For_Jump;
      end Decision_Of_Jump;

      -------------
      -- Get_CBE --
      -------------

      function Get_CBE (KD : Known_Destination) return Cond_Edge_Info is
      begin
         return Cond_Branch_Map.Element
           ((Exe, KD.Cond_Branch_PC)).Edges (KD.Edge);
      end Get_CBE;

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
               Outcome_Seen : Boolean := False;
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
                           --  Case where we know what outcome is reached by
                           --  this edge.

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
                   (D_Occ.Decision,
                    This_Condition => Index (CBI.Condition),
                    Next_Condition => Edge_Info.Next_Condition);
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
         Cond_Branch_PC   : Pc_Type renames CB_Loc.PC;
         Cond_Branch_Sloc : constant Source_Location :=
                              Get_Sloc (Ctx.Subprg.Lines, Cond_Branch_PC);

         procedure Mark_Successors
           (Dest_PC         : Pc_Type;
            Outcome_Reached : Boolean);
         --  Mark basic blocks within the decision that are reachable
         --  from Dest_PC as having known/unknown outcome. If known, do so
         --  recursively.

         ---------------------
         -- Mark_Successors --
         ---------------------

         procedure Mark_Successors
           (Dest_PC         : Pc_Type;
            Outcome_Reached : Boolean)
         is
            use Basic_Block_Sets;

            Next_PC : Pc_Type;

            Cur : Cursor;

            BB       : Basic_Block;
            BB_D_SCO : SCO_Id;

         begin
            Next_PC := Dest_PC;

            <<Tail_Recurse>>
            Cur := Find_Basic_Block (Ctx.Basic_Blocks, Next_PC);
            if Cur = No_Element then
               return;
            end if;

            BB := Element (Cur);
            BB_D_SCO := Decision_Of_Jump (BB.To_PC);
            if BB_D_SCO /= D_Occ.Decision  then
               return;
            end if;

            --  Here BB is a basic block reachable from the current decision,
            --  and whose last PC is also in the decision. Note however that
            --  it might be the first BB in a distinct inlined occurrence.

            if BB.Outcome_Reached.Contains (First_Seen_Condition_PC) then
               if BB.Outcome_Reached.Element (First_Seen_Condition_PC) then
                  null;

               elsif Outcome_Reached and then not BB.First_Cond then
                  --  Reject attempt to exclude (i.e. mark as post-outcome)
                  --  a basic block that is already known to be pre-outcome.
                  --  Generate a warning in that case, except in the case where
                  --  the basic block is the first one in the decision, in
                  --  which case this edge is a loop, and we are really
                  --  branching to a new evaluation of the decision (for the
                  --  next loop iteration).

                  Report (Exe, Cond_Branch_PC,
                          "tried to exclude pre-outcome basic block at "
                          & Hex_Image (Next_PC),
                          Kind => Diagnostics.Error);
               end if;

            else
               BB.Outcome_Reached.Include
                 (First_Seen_Condition_PC, Outcome_Reached);
               Ctx.Basic_Blocks.Replace_Element (Cur, BB);

               --  Recurse on target if unconditional branch, or if
               --  outcome known.

               if Outcome_Reached or else not BB.Cond then
                  Mark_Successors (BB.Branch_Dest.Target, Outcome_Reached);
               end if;

               --  Fallthrough is also excluded if branch is conditional,
               --  or if the branch is a call that returns.

               if (Outcome_Reached and then BB.Cond)
                 or else
                   (BB.Branch = Br_Call and then BB.Call /= Raise_Exception)
               then
                  Next_PC := BB.FT_Dest.Target;
                  goto Tail_Recurse;
               end if;
            end if;
         end Mark_Successors;

         Labeling_Complete : Boolean;

      --  Start of processing for Label_Destinations

      begin
         --  Skip branch if already identified as not contributing to the
         --  decision outcome.

         if CBI.Condition = No_SCO_Id then
            return;
         end if;

         --  Skip branch if outcome is already known when we reach it. For the
         --  first cond branch in the occurrence, we know for certain that the
         --  outcome has not been reached (the first cond branch is always
         --  contributive). We need to record this in order to handle the
         --  case of a tight loop where an outcome appears to branch back to
         --  the beginning of the decision.

         declare
            use Basic_Block_Sets;

            BB_Cur : constant Cursor :=
                       Find_Basic_Block (Ctx.Basic_Blocks, Cond_Branch_PC);
            BB     : Basic_Block := Element (BB_Cur);

            function BB_Outcome_Reached return Tristate;
            --  Return outcome reached status recorded in BB for this decision
            --  occurrence.

            ------------------------
            -- BB_Outcome_Reached --
            ------------------------

            function BB_Outcome_Reached return Tristate is
               use Outcome_Reached_Maps;

               OR_Cur : constant Outcome_Reached_Maps.Cursor :=
                 BB.Outcome_Reached.Find (First_Seen_Condition_PC);
            begin
               if OR_Cur = Outcome_Reached_Maps.No_Element then
                  return Unknown;
               else
                  return To_Tristate (Element (OR_Cur));
               end if;
            end BB_Outcome_Reached;

         begin
            if First_Cond_Branch then
               if BB_Outcome_Reached = Unknown then
                  --  Mark this BB as being the first one in the decision
                  --  occurrence, and therefore necessarily pre-outcome.

                  BB.First_Cond := True;
                  BB.Outcome_Reached.Include (First_Seen_Condition_PC, False);
                  Ctx.Basic_Blocks.Replace_Element (BB_Cur, BB);
               end if;
               pragma Assert (BB_Outcome_Reached = False);
            end if;

            if BB_Outcome_Reached = True then
               CBI.Condition := No_SCO_Id;
               Report (Exe, Cond_Branch_PC,
                       "skipping post-outcome branch"
                       & " in BB @" & Hex_Image (BB.From),
                       Kind => Notice);
               return;
            end if;
         end;

         --  Label each destination

         Labeling_Complete := True;
         for Edge in Edge_Kind loop
            if CBI.Edges (Edge).Origin /= Unknown
                 or else
               CBI.Edges (Edge).Dest_Kind = Raise_Exception
                 or else
               (CBI.Edges (Edge).Dest_Kind = Condition
                  and then
                CBI.Edges (Edge).Next_Condition = Index (CBI.Condition))
            then
               --  Labeling already complete for this edge, nothing else to do

               null;

            else
               Labeling_Complete := False;
               Label_Destination (Cond_Branch_PC, CBI, Edge);
            end if;
         end loop;

         --  Return early if both edges are already labeled

         if Labeling_Complete then
            return;
         end if;

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

         --  If both targets are still unlabeled, and have the same sloc as the
         --  cond branch, treat as intra-condition jump. This pattern occurs
         --  when the value of a condition is saved into a temporary, which
         --  will be tested later on (e.g. after some cleanup actions have been
         --  taken).

         if CBI.Edges (Branch).Dest_Kind = Unknown
              and then
            Get_Sloc
              (Ctx.Subprg.Lines, CBI.Edges (Branch).Destination.Target)
              = Cond_Branch_Sloc
              and then
            CBI.Edges (Fallthrough).Dest_Kind = Unknown
              and then
            Get_Sloc
              (Ctx.Subprg.Lines,
               CBI.Edges (Fallthrough).Destination.Target)
              = Cond_Branch_Sloc
         then
            for Edge of CBI.Edges loop
               Edge.Dest_Kind := Condition;
               Edge.Next_Condition := Index (CBI.Condition);
            end loop;
            return;
         end if;

         --  If either destination is now known to be an outcome, mark further
         --  conditional branch instructions in the decision occurrence as
         --  cleanup actions that play no role in outcome determination, else
         --  mark successors as still contributing to the decision outcome.
         --  (Do nothing if we don't know whether or not a given destination
         --  is an outcome or not!)

         --  Note: this is primarily targetting edges that correspond to
         --  front-end generated cleanup code for controlled objects. For
         --  back-end exception handling infrastructure, a different approach
         --  is used: GIGI knows to set locations without column information on
         --  those nodes, so that we do not attempt to associate the generated
         --  conditional branch instructions with any source conditions.

         for CBE of CBI.Edges loop
            if CBE.Dest_Kind /= Unknown then
               Mark_Successors
                 (CBE.Destination.Target,
                  Outcome_Reached => CBE.Dest_Kind = Outcome);
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
         use Known_Destination_Maps;

         CBE_Dest : Dest;

         CBE : Cond_Edge_Info renames CBI.Edges (Edge);
         Cur : Cursor;
         BB  : Basic_Block;

      begin
         if CBE.Origin /= Unknown then
            return;
         end if;

         CBE_Dest := CBE.Destination;

         loop
            Cur := Known_Destinations.Find (CBE_Dest);
            BB := Find_Basic_Block (Ctx.Basic_Blocks, CBE_Dest.Target);

            --  First look for an edge that has the same destination

            if Cur /= No_Element then
               declare
                  Other_CBE : Cond_Edge_Info renames Get_CBE (Element (Cur));
               begin
                  --  Assert consistency of dest kind, if known

                  pragma Assert
                    (CBE.Dest_Kind = Unknown
                     or else
                     CBE.Dest_Kind = Other_CBE.Dest_Kind);

                  CBE.Dest_Kind      := Other_CBE.Dest_Kind;
                  CBE.Outcome        := Other_CBE.Outcome;
                  CBE.Next_Condition := Other_CBE.Next_Condition;
               end;
               exit;

            --  If edge is not found but branches immediately to a further
            --  unconditional jump, then follow that jump.

            elsif BB.Jump_Only
            then
               CBE_Dest := BB.Branch_Dest;
            else
               exit;
            end if;
         end loop;

         --  Test whether to enable an additional heuristic for control-flow
         --  topologies thare are specific to the use of stack manipulation
         --  x87-FPU instructions (applies only for x86)...

         if Get_Machine (Exe.all) /= Elf_Common.EM_386 then
            return;
         end if;

         --  ...and it is used to label only edges that target a basic block
         --  which starts with a x87-FPU instruction.

         declare
            Sec      : constant Address_Info_Acc := Get_Address_Info
              (Exe.all, Section_Addresses, BB.From);
            Buffer   : Highlighting.Buffer_Type (1);
            Line_Len : Natural;
         begin
            Disa_For_Machine (Machine, Default).Disassemble_Insn
              (Slice (Sec.Section_Content, BB.From, BB.To),
               BB.From,
               Buffer,
               Line_Len,
               Disa_Symbolize.Nul_Symbolizer);

            --  x87-FPU instructions mnemonics are those starting with "f"

            if Buffer.Get_Raw /= "f" then
               return;
            end if;
         end;

         --  Here, for the case of x87-FPU instructions only (see above): if
         --  we still have an unqualified outcome, try to qualify it from
         --  another edge branching somewhere before in the same basic
         --  block.

         if CBE.Dest_Kind = Outcome and then CBE.Outcome = Unknown then
            declare
               Opposite_Edge : constant Edge_Kind :=
                 Edge_Kind'Val (1 - Edge_Kind'Pos (Edge));
               CBE_Outcome : constant Tristate := Label_From_BB
                 (Cond_Branch_PC, CBI, Edge);
               Opposite_Outcome : constant Tristate := Label_From_BB
                 (Cond_Branch_PC, CBI, Opposite_Edge);
            begin
               --  Accept the estimated outcome value only if the opposite
               --  one is different.

               if CBE_Outcome /= Opposite_Outcome then
                  CBE.Outcome := CBE_Outcome;
               end if;
            end;
         end if;
      end Label_From_Other;

      -------------------
      -- Label_From_BB --
      -------------------

      function Label_From_BB
        (Cond_Branch_PC : Pc_Type;
         CBI            : Cond_Branch_Info;
         Edge           : Edge_Kind) return Tristate
      is
         use Known_Destination_Maps;
         BB     : Basic_Block;
         CBE    : Cond_Edge_Info renames CBI.Edges (Edge);
         Cur    : Cursor;
         Branch : Branch_Kind;
      begin
         --  First, look at the kind of the current conditional branch
         --  instruction.

         BB := Find_Basic_Block (Ctx.Basic_Blocks, Cond_Branch_PC);
         if BB /= No_Basic_Block then
            Branch := BB.Branch;
         else
            --  This should *never* happen: for every edge, there must be a
            --  basic block whose last instruction is the origin of the
            --  edge.

            raise Program_Error with
               "Cannot find a basic block for the branch instruction at "
               & Hex_Image (Cond_Branch_PC);
         end if;

         --  If this branch is a conditional return, there is nothing to
         --  infer: other returning edges are probably unrelated to this
         --  one.

         if Branch /= Br_Ret then
            --  Otherwise, get for the basic block that contain CBE's
            --  destination.

            BB := Find_Basic_Block
              (Ctx.Basic_Blocks, CBE.Destination.Target);
            if BB /= No_Basic_Block then
               --  Then look at edges whose destination is before CBEs in the
               --  same basic block and with the same kind of destination.
               --  Return the value of the outcome of the first known one
               --  we met, if any.

               Cur := Known_Destinations.Ceiling ((BB.From - 1, 0));
               while Cur /= No_Element
                        and then
                     Key (Cur) < CBE.Destination
               loop
                  declare
                     Other_CBE : Cond_Edge_Info renames
                        Get_CBE (Element (Cur));
                  begin
                     if Other_CBE.Dest_Kind = Outcome
                           and then
                        Other_CBE.Outcome /= Unknown
                     then
                        return Other_CBE.Outcome;
                     end if;
                  end;
                  Cur := Next (Cur);
               end loop;

            else
               --  This should *never* happen: for every edge, there must be a
               --  basic block that contains the destination of the edge.

               raise Program_Error with
                  "Cannot find a basic block for the edge "
                  & Hex_Image (Cond_Branch_PC)
                  & " -> "
                  & Hex_Image (CBE.Destination.Target);
            end if;
         end if;

         --  If we could not deduce anything, return that we do not known what
         --  outcome this edge is.

         return Unknown;
      end Label_From_BB;

      ------------------------------
      -- Record_Known_Destination --
      ------------------------------

      procedure Record_Known_Destination
        (Cond_Branch_PC : Pc_Type;
         Destination    : Dest;
         Edge           : Edge_Kind)
      is
      begin
         if not Known_Destinations.Contains (Destination) then
            Known_Destinations.Insert
              (Destination, (Cond_Branch_PC, Edge));
         end if;
      end Record_Known_Destination;

      ------------------------
      -- Output_Cond_Branch --
      ------------------------

      procedure Output_Cond_Branch
        (Exe   : Exe_File_Acc;
         CB_PC : Pc_Type)
      is
         use Cond_Branch_Maps;

         Cur : Cursor := Cond_Branch_Map.Find ((Exe, CB_PC));

         CBI : Cond_Branch_Info renames Element (Cur);

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

      --  Start of processing for Output_Cond_Branch

      begin
         if CBI.Condition = No_SCO_Id
           or else (CBI.Edges (Branch).Origin = Unknown
                      and then
                    CBI.Edges (Fallthrough).Origin = Unknown)
         then
            Report (Exe, CB_PC,
                    "omitted non-contributive branch", Kind => Notice);
            Cond_Branch_Map.Delete (Cur);
            return;
         end if;

         --  Mark instruction address for full (historical) traces collection
         --  (for MC/DC source coverage analysis) if required by decision
         --  structure (presence of multiple paths) or if Full_History_Trace is
         --  enabled.

         if Has_Multipath_Condition (Enclosing_Decision (CBI.Condition))
           or else Full_History_Trace.Is_Active
         then
            Add_Entry
              (Base  => Decision_Map_Base,
               First => CB_PC,
               Last  => CBI.Last_PC,
               Op    => 0);
         end if;

         --  Report remaining unlabeled destinations

         for Edge in Edge_Kind loop
            if CBI.Edges (Edge).Dest_Kind = Unknown
                 or else
               (CBI.Edges (Edge).Dest_Kind = Outcome
                  and then CBI.Edges (Edge).Origin = Unknown)
            then
               Report (Exe, CB_PC,
                       "unable to label " & Edge'Img
                       & " destination "
                       & Hex_Image (CBI.Edges (Edge).Destination.Target),
                       Kind => Warning);
            end if;
         end loop;

         Report
           (Exe, CB_PC,
            Dest_Image (Branch, CBI.Edges (Branch))
            & " / "
            & Dest_Image (Fallthrough, CBI.Edges (Fallthrough)),
            Kind => Notice);
      end Output_Cond_Branch;

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

         if CBE.Outcome = Unknown then
            --  The outcome may be reached after having skipped constant
            --  conditions.

            declare
               Cond : SCO_Id := Next_Condition (CBI.Condition, Origin);
            begin
               Skip_Constant_Conditions (Cond, CBE.Outcome, Skipped => null);
            end;
         end if;

         if CBE.Outcome /= Unknown then
            CBE.Dest_Kind := Outcome;

            if BB.Branch /= Br_Ret then
               Known_Outcome (To_Boolean (CBE.Outcome)).Include
                 (CBE.Destination);
            end if;

         else
            Next_C_SCO := Next_Condition (CBI.Condition, Origin);
            declare
               Outcome : Tristate;
            begin
               Skip_Constant_Conditions (Next_C_SCO, Outcome, Skipped => null);
            end;
            if Next_C_SCO /= No_SCO_Id then
               CBE.Dest_Kind := Condition;
               CBE.Next_Condition := Index (Next_C_SCO);
            end if;
         end if;

         if BB.Branch /= Br_Ret then
            declare
               Destination : Dest := CBI.Edges (Edge).Destination;
               BB          : Basic_Block :=
                 Find_Basic_Block (Ctx.Basic_Blocks, Destination.Target);
            begin

               loop
                  --  Keep following branches as long as the BB is a one
                  --  instruction unconditional branching.

                  exit when not BB.Jump_Only;

                  Destination := BB.Branch_Dest;
                  BB := Find_Basic_Block (Ctx.Basic_Blocks,
                                          Destination.Target);
               end loop;
               Record_Known_Destination (Cond_Branch_PC, Destination, Edge);
            end;

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

         Next_Dest     : Dest := Edge_Info.Destination;
         Next_PC       : Pc_Type;
         In_Delay_Slot : Boolean := False;

         BB            : Basic_Block;

         After_Call : Boolean := False;
         --  Whether the previous basic block ends with a call instruction.
         --  In the reguler case, we expect there is a basic block after all
         --  "fallback instructions". However, calls can be "no-return", so we
         --  must deal with situations when there is no basic block after them.

         Next_PC_Sloc : Source_Location;
         Next_PC_SCO  : SCO_Id;
         --  Statement at Next_PC

         D_SCO_For_Jump : SCO_Id;
         --  Decision SCO for jump instruction at end of BB

         D_SCO : constant SCO_Id := Enclosing_Decision (CBI.Condition);
         S_SCO : constant SCO_Id := Enclosing_Statement (D_SCO);
         --  SCOs for the decision being evaluated, and its enclosing statement

         Leaves_Statement : Boolean := False;
         --  True if the edge is known to branch to code outside of the
         --  current statement.

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

         if Next_Dest.Delay_Slot = No_PC then
            Next_PC := Next_Dest.Target;
            In_Delay_Slot := False;
         else
            Next_PC := Next_Dest.Delay_Slot;
            In_Delay_Slot := True;
         end if;

         --  We cannot follow indirect jumps. If we end up there, we can only
         --  stop the analysis of the current edge.

         if Next_PC = No_PC then
            return;
         end if;

         BB := Find_Basic_Block (Ctx.Basic_Blocks, Next_PC);

         --  If we are branching into an inlined subprogram, we can't deduce
         --  anything from it.
         --
         --  TODO??? The above is a very conservative guess. If needed in
         --  practice, we could refine this to handle various manageable cases.

         declare
            BB_Inline_Info : constant Address_Info_Acc :=
              Get_Address_Info
                (Exe.all, Inlined_Subprogram_Addresses, BB.To_PC);
         begin
            if BB_Inline_Info /= null
              and then BB_Inline_Info /= D_Occ.Inlined_Body
            then
               return;
            end if;
         end;

         if BB = No_Basic_Block then
            if After_Call
               or else Next_PC not in Ctx.Subprg.First .. Ctx.Subprg.Last
            then
               --  If After_Call is true, the previously processed basic block
               --  is probably a non-returning call (although there is no way
               --  for us to be sure about this).
               --
               --  If Next_PC is out of the current routine, this is either a
               --  tail call (unlikely at -O0 or -O1) or a JUMP padding (likely
               --  present in the routine because PE executables on Windows
               --  lack symbol size information).
               --
               --  In both case, just ignore, so do not complain about the
               --  control flow and consider the decision will never have an
               --  outcome when executing it.

               Edge_Info.Dest_Kind := Unknown;
               return;

            else
               --  This should *never* happen: we are walking through the CFG
               --  of the current routine, so all jumps must target an existing
               --  basic block.

               raise Program_Error with
                 "Cannot find a basic block for the instruction at "
                 & Hex_Image (Next_PC);
            end if;

         elsif Is_Visited_BB (BB.From) then
            --  Stop when coming across an already visited basic block, in
            --  order to avoid infinite loops.

            return;
         end if;

         Visited_BB.Insert (BB.From);
         After_Call := False;

         Next_PC_Sloc := Get_Sloc (Ctx.Subprg.Lines, Next_PC);

         --  Check for exception or outcome using dominance information.
         --  Note that this relies on an accurate mapping of slocs to SCO for
         --  statements, not conditions. For a statement sloc that has only
         --  line granularity (no column info), this must be disabled if the
         --  line has multiple statements.

         Next_PC_SCO := Sloc_To_SCO (Next_PC_Sloc, Include_Decisions => False);
         if Next_PC_SCO /= No_SCO_Id then
            Next_PC_SCO := Enclosing_Statement (Next_PC_SCO);
         end if;

         if Next_PC_SCO /= No_SCO_Id
           and then not (Next_PC_Sloc.L.Column = 0
                           and then
                         Is_Multistatement_Line (Next_PC_Sloc))
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

         --  If we have processed a delay slot instruction, proceed with the
         --  branch target.

         if In_Delay_Slot then
            Next_Dest.Delay_Slot := No_PC;
            goto Follow_Jump;
         end if;

         --  Here if we remain within the current decision: continue tracing
         --  object control flow: find SCOs for jump at end of basic block.

         --  Determine whether the jump is known to branch to another statement

         --  Note: we first check the Next_PC_SCO, then we fall back to testing
         --  whether Next_PC_Sloc falls outside of the sloc range of S_SCO,
         --  to handle the case where Next_PC_Sloc is present but does not
         --  correspond to any SCO (this may be the case e.g. for a NOP
         --  generated to carry the sloc of an END).

         Leaves_Statement :=
           Leaves_Statement
             or else
               (S_SCO /= No_SCO_Id
                  and then
                (if Next_PC_SCO /= No_SCO_Id then
                   S_SCO /= Next_PC_SCO
                 elsif Next_PC_Sloc /= No_Location then
                   not In_Range (Next_PC_Sloc, Sloc_Range (S_SCO))
                 else
                   False));

         --  If this is a call to a finalization symbol (besides the finalizer
         --  itself), assume that we remain in the decision. Otherwise, look
         --  for the decision for the jump at the end of BB (if any).

         if BB.Branch = Br_Call and then BB.Call = Finalization_Exp then
            D_SCO_For_Jump := D_SCO;
         else
            D_SCO_For_Jump := Decision_Of_Jump (BB.To_PC);
         end if;

         --  If in an IF-expression, and we're branching into a decision other
         --  than a nested one (or outside of any decision), then have reached
         --  an outcome of the current decision. To determine this, check
         --  whether D_SCO_For_Jump is equal to D_SCO or any nested decision.

         declare
            Nested_D_SCO : SCO_Id := D_SCO_For_Jump;
         begin
            while Nested_D_SCO /= D_SCO and then Nested_D_SCO /= No_SCO_Id loop
               Nested_D_SCO := Parent (Nested_D_SCO);
            end loop;

            if Is_If_Expression (D_SCO) and then Nested_D_SCO /= D_SCO then
               Edge_Info.Dest_Kind := Outcome;
               return;
            end if;
         end;

         --  Note: there are cases (e.g. for Pre/Post aspects) where there
         --  is a decision SCO with no enclosing statement SCO, and we have
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

                  Next_Dest := BB.Branch_Dest;
                  goto Follow_Jump;
               end if;

            when Br_Call =>
               --  Edges that are post-dominated by calls to block finalizers
               --  are outcomes: finalization of transient blocks only occurs
               --  after a complete expression has been evaluated (or an
               --  exception was raised).
               --
               --  This heuristic is not valid for quantified expressions, so
               --  we will deactivate it in this case.

               if BB.Call = Finalizer
                  and then not Is_Quantified_Expression (CBI.Condition)
               then
                  Edge_Info.Dest_Kind := Outcome;

               --  Case of a call emitted within the same decision, and raising
               --  an exception: the edge is either a failed run time check
               --  or a False outcome for an assertion/pre/post-condition.

               elsif BB.Call = Raise_Exception
                     and then D_SCO_For_Jump = D_SCO
               then
                  --  Call to Raise_Assert_Failure in an Assert/PPC decision:
                  --  False outcome. Note that more than one condition within
                  --  the decision may generate such an outcome, and share the
                  --  call insn. In this case, the call may be labeled with the
                  --  sloc of any of them: it can be for some other condition
                  --  than SCO (but always within the same enclosing decision).

                  if Is_Assertion (D_SCO)
                     and then Platform_Independent_Symbol (BB.Called_Sym.all,
                                                           Exe.all)
                              = "system__assertions__raise_assert_failure"
                  then
                     Edge_Info.Dest_Kind := Outcome;
                     Edge_Info.Outcome   := False;

                  else
                     Edge_Info.Dest_Kind := Raise_Exception;
                  end if;

               elsif BB.Call in Normal | Finalization_Exp then
                  Next_Dest := (BB.To + 1, Delay_Slot => No_PC);
                  After_Call := True;
                  goto Follow_Jump;
               end if;

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
      Report (Exe, First_CBI_PC (D_Occ),
              "analyzing occurrence",
              SCO  => D_Occ.Decision,
              Kind => Notice);

      --  Detect and report incomplete occurrences (i.e. occurrence where the
      --  last seen condition is not the last run-time-evaluated condition of
      --  the decision). In this case we bail out.

      if not Is_Last_Runtime_Condition (D_Occ) then
         return;
      end if;

      --  Label edge destinations. Perform two passes so that the second can
      --  reuse known destinations identified by the first. First_Cond_Branch
      --  is set while labeling the destinations for the first conditional
      --  branch insn in the decision.

      for Pass in 1 .. 2 loop
         First_Cond_Branch := True;
         for CB_PC of D_Occ.Conditional_Branches loop
            Cond_Branch_Map.Update_Element
              (Cond_Branch_Map.Find ((Exe, CB_PC)), Label_Destinations'Access);
            First_Cond_Branch := False;
         end loop;
      end loop;

      --  Generate decision map, reporting labeling failures as we go

      for CB_PC of D_Occ.Conditional_Branches loop
         Output_Cond_Branch (Exe, CB_PC);
      end loop;

      --  Report non-constant conditions for which no edge provides a valuation

      for J in Condition_Index'First .. D_Occ.Last_Cond_Index loop
         for Val in Boolean'Range loop
            if not Has_Valuation (J, Val)
              and then
                Value (Condition (D_Occ.Decision, J)) = Unknown
            then
               D_Occ.Missing_Valuations.Insert
                 (Valuation_Type'(CI  => J, Val => Val));
            end if;
         end loop;
      end loop;

      --  Record decision occurrence

      Append_Decision_Occurrence (D_Occ);
   end Analyze_Decision_Occurrence;

   ---------------------
   -- Analyze_Routine --
   ---------------------

   --  Code for interface thunks must be ignored altogether for coverage
   --  purposes. It is identified by matching the symbol name against a regular
   --  expression, and looking at whether the first instruction in the symbol
   --  has the sloc of a type declaration.

   Thunk_Matcher : constant GNAT.Regexp.Regexp := Compile (".*__T[0-9]+[bs]");

   procedure Analyze_Routine
     (Name  : String_Access;
      Exec  : Exe_File_Acc;
      Insns : Binary_Content)
   is
      PC       : Pc_Type;
      Insn_Len : Natural;

      BB : Basic_Block;
      --  Current basic block information

      Context : Cond_Branch_Context;

      procedure New_Basic_Block;
      --  Reset state to start processing a new basic block

      procedure Put_Line (S : String; Underline : Character);
      --  Output S, underline with the given character

      procedure Report_Non_Traceable
        (BB     : Basic_Block;
         Reason : String);
      --  Emit a diagnostic for a non-traceable conditional branch instruction
      --  at BB.To_PC.

      ---------------------
      -- New_Basic_Block --
      ---------------------

      procedure New_Basic_Block is
      begin
         --  Initialize for new basic block starting at PC

         BB := (From => PC, others => <>);
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

      --------------------------
      -- Report_Non_Traceable --
      --------------------------

      procedure Report_Non_Traceable
        (BB     : Basic_Block;
         Reason : String)
      is
      begin
         Context.Stats.Non_Traceable :=
           Context.Stats.Non_Traceable + 1;

         Report
           ("non-traceable: " & Reason,
            Exe  => Exec,
            PC   => BB.To_PC,
            Sloc => First_Sloc (BB.Branch_SCO),
            Kind => Warning);
      end Report_Non_Traceable;

      Subp_Name : constant String := Get_Filename (Exec.all) & ":" & Name.all;

      --  The following records describes a conditional branch instruction
      --  found during the initial code scan, which needs to be analyzed later
      --  on (see below for complete description of the two-pass scan).

      type Pending_Cond_Branch is record
         Insn_First, Insn_Last : Pc_Type;
         --  Conditional branch instruction PC range in Insns

         C_SCO                 : SCO_Id;
         --  SCO and sloc information for this instruction

         Branch_Dest, FT_Dest  : Dest;
         --  Machine properties for this conditional branch

         BB_From               : Pc_Type;
         --  First PC of the basic block that contains this instruction
      end record;

      package Pending_Cond_Branch_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Pending_Cond_Branch);
      use Pending_Cond_Branch_Vectors;

      Pending_Cond_Branches : Pending_Cond_Branch_Vectors.Vector;
      --  Conditional branches analysis needs to have information about all
      --  basic blocks. All conditional branch instructions to be analyzeAd are
      --  therefore queued in this vector while performing an initial code
      --  scan (first pass), during which all basic blocks are identified.
      --  They are actually processed after this scan is completed (second
      --  pass).

      Disas    : access Disassembler'Class;
      I_Ranges : Insn_Set_Ranges_Cst_Acc;
      Cache    : Insn_Set_Cache := Empty_Cache;
      Insn_Set : Insn_Set_Type;

   --  Start of processing for Analyze_Routine

   begin
      if Branch_Stats_Trace.Is_Active then
         Put_Line ("Branch statistics for " & Subp_Name, '^');
      else
         Misc_Trace.Trace ("building decision map for " & Subp_Name);
      end if;

      Context.Subprg :=
        Get_Address_Info (Exec.all, Subprogram_Addresses, Insns.First);
      I_Ranges :=
        Get_Insn_Set_Ranges (Exec.all, Context.Subprg.Parent.Section_Sec_Idx);

      --  First pass: instruction scan

      --  In this pass, we record all basic blocks, and we make a note of any
      --  conditional branch instruction that needs to be analyzed.

      PC := Insns.First;
      New_Basic_Block;

      while Iterate_Over_Insns
        (I_Ranges.all, Cache, Insns.Last, PC, Insn_Set)
      loop
         Disas := Disa_For_Machine (Machine, Insn_Set);
         Insn_Len := Disas.Get_Insn_Length (Slice (Insns, PC, Insns.Last));

         declare
            LI   : Line_Info_Access;
            Insn : constant Binary_Content :=
              Slice (Insns, PC, PC + Pc_Type (Insn_Len) - 1);

            Branch      : Branch_Kind;
            Flag_Indir  : Boolean;
            Flag_Cond   : Boolean;
            Branch_Dest : Dest;
            FT_Dest     : Dest;
            Slocs       : constant Source_Locations :=
                            Get_Slocs (Context.Subprg.Lines, PC);
            --  Properties of Insn

         begin
            --  Check if this is a thunk

            if PC = Insns.First and then Match (Subp_Name, Thunk_Matcher) then
               for Sloc of Slocs loop
                  LI := Get_Line (Sloc);
                  if LI /= null and then LI.SCOs /= null then
                     for SCO of LI.SCOs.all loop
                        if Kind (SCO) = Statement
                          and then S_Kind (SCO) = Type_Declaration
                        then
                           Report (Exec, PC,
                                   Msg  => "ignoring thunk " & Subp_Name,
                                   SCO  => SCO,
                                   Kind => Notice);
                           return;
                        end if;
                     end loop;
                  end if;
               end loop;
            end if;

            --  Find lines for this PC, and mark relevant statement SCOs as
            --  as having code: if the PC has no column information, this
            --  is done for all SCOs on the line, else only for those that
            --  contain that column number.

            for Sloc of Slocs loop
               LI := Get_Line (Sloc);
               if LI /= null and then LI.SCOs /= null then

                  --  Record presence of code for all Statement SCOs on line

                  for SCO of LI.SCOs.all loop

                     if Kind (SCO) = Statement
                          and then
                        (Sloc.L.Column = 0
                           or else In_Range (Sloc, Sloc_Range (SCO)))
                     then
                        Set_Basic_Block_Has_Code (SCO);
                     end if;
                  end loop;

                  --  Record presence of code for compilation unit

                  Set_Unit_Has_Code (Comp_Unit (Sloc.Source_File));
               end if;
            end loop;

            --  Disassemble instruction

            Disas.Get_Insn_Properties
              (Insn_Bin    => Insn,
               Pc          => PC,
               Branch      => Branch,
               Flag_Indir  => Flag_Indir,
               Flag_Cond   => Flag_Cond,
               Branch_Dest => Branch_Dest,
               FT_Dest     => FT_Dest);

            --  Use debug information to complete properties

            if Branch = Br_Call and then Flag_Indir then
               Branch_Dest.Target := Get_Call_Target
                 (Exec.all, PC, Pc_Type (Insn_Len));
            end if;

            --  If both edges have the same delay slot address, then said delay
            --  slot is always executed, whether or not we branch, so we ignore
            --  it for the purpose of edge destination equivalence. We thus
            --  treat:
            --
            --     cond-branch tgt
            --     insn (in delay slot)
            --
            --  as equivalent to:
            --
            --     insn
            --     cond-branch tgt
            --     nop (in delay slot)
            --
            --  If the delay slot is a NOP with the same sloc as the branch, we
            --  will skip it, as it just means that the compiler was unable to
            --  insert a meaningful instruction in the delay slot.

            if Branch_Dest.Delay_Slot /= No_PC
              and then Branch_Dest.Delay_Slot = FT_Dest.Delay_Slot
            then
               declare
                  Delay_Slot_PC   : constant Pc_Type :=
                    PC + Pc_Type (Insn_Len);
                  Delay_Slot_Len  : constant Natural :=
                    Disas.Get_Insn_Length
                      (Slice (Insns, Delay_Slot_PC, Insns.Last));
                  Delay_Slot_Insn : constant Binary_Content :=
                    Slice (Insns,
                           Delay_Slot_PC,
                           Delay_Slot_PC + Pc_Type (Delay_Slot_Len) - 1);
                  Delay_Slot_Loc  : constant  Source_Location :=
                    Get_Sloc (Context.Subprg.Lines, Delay_Slot_PC);
                  Branch_Sloc     : constant Source_Location :=
                    Get_Sloc (Context.Subprg.Lines, PC);
               begin
                  pragma Assert (Branch_Dest.Delay_Slot = Delay_Slot_PC);

                  --  If the delay slot is a NOP with the same sloc as the
                  --  branch, make the next iteration work on the instruction
                  --  that follows the delay slot.

                  if Disas.Is_Nop (Delay_Slot_Insn, Delay_Slot_PC)
                    and then Delay_Slot_Loc = Branch_Sloc
                  then
                     PC := Delay_Slot_PC;
                     Insn_Len := Delay_Slot_Len;
                  end if;

                  --  Edge destination equivalence (see comment above)

                  Branch_Dest.Delay_Slot := No_PC;
                  FT_Dest.Delay_Slot := No_PC;
               end;
            end if;

            if Branch /= Br_None then
               Analyze_Branch : declare
                  SCO            : SCO_Id;
                  Branch_SCO     : SCO_Id renames BB.Branch_SCO;

               --  Start of processing for Analyze_Branch

               begin
                  --  Update BB info

                  BB.To_PC       := Insn.First;
                  BB.To          := Insn.Last;
                  BB.Branch_Dest := Branch_Dest;
                  BB.FT_Dest     := FT_Dest;
                  BB.Branch      := Branch;
                  BB.Cond        := Flag_Cond;

                  if Branch = Br_Call then
                     Analyze_Call (Exec, BB);
                  end if;

                  --  Keep track of whether, for our analysis, we can consider
                  --  this basic block to be only an unconditional jump.

                  BB.Jump_Only :=
                    not BB.Cond
                    and then BB.Branch = Br_Jmp
                    and then BB.From = BB.To_PC;

                  for Sloc of Slocs loop
                     SCO := Sloc_To_SCO (Sloc);

                     if Flag_Cond then
                        if Branch = Br_Jmp or else Branch = Br_Ret then
                           if SCO /= No_SCO_Id
                             and then Kind (SCO) = Condition
                           then
                              --  Assumption: a given conditional branch
                              --  instruction tests at most 1 source condition.

                              if Branch_SCO /= No_SCO_Id
                                and then Kind (Branch_SCO) = Condition
                              then
                                 if Branch_SCO /= SCO then
                                    Report_Non_Traceable
                                      (BB,
                                       "multiple conditions for conditional "
                                       & "branch");
                                 else
                                    --  Duplicate sloc info denoting the
                                    --  same SCO as the one seen previously:
                                    --  nothing to do.

                                    null;
                                 end if;

                              else
                                 Branch_SCO := SCO;

                                 --  Queue for later processing

                                 Pending_Cond_Branches.Append
                                   (Pending_Cond_Branch'
                                      (Insn_First  => Insn.First,
                                       Insn_Last   => Insn.Last,
                                       C_SCO       => SCO,
                                       Branch_Dest => Branch_Dest,
                                       FT_Dest     => FT_Dest,
                                       BB_From     => BB.From));
                              end if;
                           end if;

                        else
                           --  Warn if conditional call or conditional return
                           --  (such combinations are not supported).

                           Report_Non_Traceable
                             (BB, "unexpected conditional branch of type "
                              & Branch'Img);
                        end if;
                     end if;

                     if Branch_SCO = No_SCO_Id and then SCO /= No_SCO_Id then
                        Branch_SCO := SCO;
                     end if;
                  end loop;

                  Context.Basic_Blocks.Insert (BB);
               end Analyze_Branch;
            end if;

            PC := PC + Pc_Type (Insn_Len);

            --  Handle case where PC wraps

            exit when PC = 0;

            if Branch /= Br_None then
               New_Basic_Block;
            end if;
         end;
      end loop;

      --  On targets with delay slots, a subprogram may end with a delay slot,
      --  in which case we need to create an additional basic block for it.

      if BB.From <= Insns.Last then
         BB.To_PC := Insns.Last - Pc_Type (Insn_Len) + 1;
         BB.To    := Insns.Last;
         Context.Basic_Blocks.Insert (BB);
      end if;

      --  All done if doing only statement coverage

      if not (Branch_Stats_Trace.Is_Active
                or else Enabled (Decision)
                or else MCDC_Coverage_Enabled)
      then
         return;
      end if;

      --  Second pass: analyze queued conditional branch instructions

      for Cond_Branch of Pending_Cond_Branches loop
         declare
            BB_Cur      : constant Basic_Block_Sets.Cursor :=
              Find_Basic_Block (Context.Basic_Blocks, Cond_Branch.BB_From);
            BB          : Basic_Block := Basic_Block_Sets.Element (BB_Cur);
            --  This conditional branch has been found when building the list
            --  of basic blocks, so we know there is exactly one basic block
            --  ending with it.

         begin
            Analyze_Conditional_Branch
              (Exec        => Exec,
               Insn        =>
                 Slice (Insns, Cond_Branch.Insn_First, Cond_Branch.Insn_Last),
               C_SCO       => Cond_Branch.C_SCO,
               Branch_Dest => Cond_Branch.Branch_Dest,
               FT_Dest     => Cond_Branch.FT_Dest,
               Ctx         => Context,
               BB          => BB);

            --  Update the basic block in the context: BB is an "out" argument
            --  for Analyze_Conditional_Branch, so fields that are not part of
            --  the key may have changed.

            Context.Basic_Blocks.Replace_Element (BB_Cur, BB);
         end;
      end loop;

      --  Flush pending decisions

      for J in reverse Context.Decision_Stack.First_Index
                    .. Context.Decision_Stack.Last_Index
      loop
         Analyze_Decision_Occurrence
           (Exec, Context, Context.Decision_Stack.Element (J));
      end loop;

      --  Report non-constant conditions for which no edge provides a
      --  valuation.

      for C in Decision_Occurrence_Map.Iterate loop
         declare
            D_Occs : constant Decision_Occurrence_Vectors.Vector :=
              Decision_Occurrence_Maps.Element (C);

            Missing_Valuations : Valuation_Sets.Set :=
              D_Occs.First_Element.Missing_Valuations;
            --  For each valuation of each condition, indicates whether
            --  there is one edge corresponding to each possible valuation
            --  of the condition.

            Incomplete_Occurrence : Boolean := True;
         begin
            for D_Occ of D_Occs loop
               Missing_Valuations.Intersection (D_Occ.Missing_Valuations);
               Incomplete_Occurrence := Incomplete_Occurrence and then
                 not Is_Last_Runtime_Condition (D_Occ);
            end loop;

            --  Detect and report incomplete occurrences (i.e. occurrence where
            --  the last seen condition is not the last run-time-evaluated
            --  condition of the decision).

            if Incomplete_Occurrence then
               Report (Exec,
                       D_Occs.First_Element.Conditional_Branches.Last_Element,
                       "incomplete occurrence",
                       SCO => D_Occs.First_Element.Decision);
            end if;

            for Valuation of Missing_Valuations loop
               declare
                  D_Occ         : constant Decision_Occurrence_Access :=
                    D_Occs.First_Element;
                  Condition_SCO : constant SCO_Id :=
                    Condition (D_Occ.Decision, Valuation.CI);
               begin
                  Report (First_Sloc (Condition_SCO),
                          Msg  => Image (Condition_SCO)
                          & " lacks edge for "
                          & Valuation.Val'Img
                          & " in decision occurrence starting @"
                          & Hex_Image
                            (D_Occs.First_Element.Conditional_Branches
                             .First_Element),
                          Kind => Warning);
               end;
            end loop;
         end;
      end loop;
      Decision_Occurrence_Map.Clear;

      --  Statistics processing

      if Branch_Stats_Trace.Is_Active then

         --  Update statistics

         for BB of Context.Basic_Blocks loop
            declare
               SK : constant Any_Statement_Kind :=
                 S_Kind (Enclosing_Statement (BB.Branch_SCO));
            begin
               Context.Stats.Branch_Counts (BB.Branch, SK, BB.Cond) :=
                 Context.Stats.Branch_Counts (BB.Branch, SK, BB.Cond)
                 + 1;
            end;

            if BB.Cond then
               declare
                  CBK : Cond_Branch_Kind;

                  function Is_Cleanup_CBI return Boolean;
                  --  A conditional branch instruction that is post-outcome
                  --  for at least one decision occurrence, and pre-outcome
                  --  for none, is part of cleanup actions.

                  --------------------
                  -- Is_Cleanup_CBI --
                  --------------------

                  function Is_Cleanup_CBI return Boolean is
                     Pre_Outcome, Post_Outcome : Boolean := False;
                  begin
                     for Outcome_Reached of BB.Outcome_Reached loop
                        if Outcome_Reached then
                           Post_Outcome := True;
                        else
                           Pre_Outcome := True;
                        end if;
                     end loop;
                     return Post_Outcome and not Pre_Outcome;
                  end Is_Cleanup_CBI;

               begin
                  if BB.Branch_SCO = No_SCO_Id then
                     CBK := None;
                     Report_Non_Traceable (BB, "no SCO");

                  elsif BB.Condition = No_SCO_Id then
                     CBK := Statement;

                     if S_Kind (BB.Branch_SCO) /= For_Loop_Statement then
                        Report_Non_Traceable
                          (BB, "cond branch for "
                           & S_Kind (BB.Branch_SCO)'Img);
                     end if;

                  elsif Is_Cleanup_CBI then
                     CBK := Cleanup;

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
                                   (BB, J'Img & " edge remains in condition");
                              end if;
                           end loop;
                        end if;
                     end;
                  end if;

                  Context.Stats.Cond_Branch_Counts (CBK) :=
                    Context.Stats.Cond_Branch_Counts (CBK) + 1;
               end;
            end if;
         end loop;

         declare
            First : Boolean;
            Count : Natural;
         begin
            --  Add blank lines if any diagnosis about non-traceable cond
            --  branches has been emitted.

            if Context.Stats.Non_Traceable > 0 then
               New_Line;
            end if;

            Put_Line ("Summary by branch kind", '"');
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

            First := True;
            for J in Context.Stats.Cond_Branch_Counts'Range loop
               if Context.Stats.Cond_Branch_Counts (J) > 0 then
                  if First then
                     New_Line;
                     Put_Line ("Conditional branches", '"');
                     First := False;
                  end if;

                  Put_Line
                    (" "
                     & (case J is
                       when None      => "no SCO       ",
                       when Statement => "statement    ",
                       when Condition => "condition    ",
                       when Check     => "runtime check",
                       when Cleanup   => "cleanup")
                     & Context.Stats.Cond_Branch_Counts (J)'Img);
               end if;
            end loop;

            New_Line;
            Put_Line
              (Img (Context.Stats.Non_Traceable)
               & " non-traceable conditional branches reported");
            New_Line;
         end;
      end if;
   end Analyze_Routine;

   --------------------------------
   -- Append_Decision_Occurrence --
   --------------------------------

   procedure Append_Decision_Occurrence (D_Occ : Decision_Occurrence_Access) is
      use Decision_Occurrence_Maps;
      Cur : constant Cursor := Decision_Occurrence_Map.Find (D_Occ.Decision);
   begin
      if Cur = Decision_Occurrence_Maps.No_Element then
         Decision_Occurrence_Map.Insert
           (Key      => D_Occ.Decision,
            New_Item => Decision_Occurrence_Vectors.To_Vector (D_Occ, 1));
      else
         Decision_Occurrence_Map.Reference (Cur).Append (D_Occ);
      end if;
   end Append_Decision_Occurrence;

   ------------------------
   -- Build_Decision_Map --
   ------------------------

   procedure Build_Decision_Map
     (Exec_Name    : String;
      Text_Start   : Pc_Type;
      Map_Filename : String)
   is
      Exec : Exe_File_Acc;
   begin
      Open_Exec (Exec_Name, Text_Start, Exec);

      Init_Base (Decision_Map_Base);
      Analyze (Exec);
      Decision_Map.Write_Map (Map_Filename);
   end Build_Decision_Map;

   ------------------------------
   -- Check_Possible_Successor --
   ------------------------------

   function Check_Possible_Successor
     (D_SCO          : SCO_Id;
      This_Condition : Any_Condition_Index;
      Next_Condition : Condition_Index) return Tristate
   is
   begin
      --  If This_Condition is No_Condition_Index, iterate just once with an
      --  arbitrary for J, else check both valuations of the current condition.

      for J in False .. (This_Condition /= No_Condition_Index) loop
         declare
            use SCO_Sets;

            Next_Condition_SCO  : constant SCO_Id :=
              Condition (D_SCO, Next_Condition);

            Next_C              : SCO_Id;
            Outcome             : Tristate;
            Possible_Successors : aliased SCO_Sets.Set;
         begin
            if This_Condition = No_Condition_Index then
               --  Initial condition

               Next_C := Condition (D_SCO, 0);
            else
               --  Go to the next condition with the J valuation for the
               --  current condition, and skip constant conditions if any.

               Next_C := SC_Obligations.Next_Condition
                           (Condition (D_SCO, This_Condition), J);
            end if;

            Skip_Constant_Conditions
              (Next_C, Outcome, Possible_Successors'Access);

            --  If there is a match between the resulting condition and
            --  the given Next_Condition, return the tried valuation.

            if (Next_C /= No_SCO_Id
                and then Index (Next_C) = Next_Condition)
              or else Possible_Successors.Contains (Next_Condition_SCO)
            then
               return To_Tristate (J);
            end if;
         end;
      end loop;

      --  If we end up here, no valuation enabled us to reach
      --  Next_Condition.

      return Unknown;
   end Check_Possible_Successor;

   ----------------------
   -- Find_Basic_Block --
   ----------------------

   function Find_Basic_Block
     (Basic_Blocks : Basic_Block_Sets.Set;
      PC           : Pc_Type) return Basic_Block_Sets.Cursor
   is
      use Basic_Block_Sets;

      Cur : constant Cursor := Basic_Blocks.Floor ((From => PC, others => <>));

   begin
      if Cur /= No_Element and then PC <= Element (Cur).To then
         return Cur;
      else
         return No_Element;
      end if;
   end Find_Basic_Block;

   function Find_Basic_Block
     (Basic_Blocks : Basic_Block_Sets.Set;
      PC           : Pc_Type) return Basic_Block
   is
      use Basic_Block_Sets;

      Cur : constant Cursor := Find_Basic_Block (Basic_Blocks, PC);
   begin
      if Cur /= No_Element then
         return Element (Cur);
      else
         return No_Basic_Block;
      end if;
   end Find_Basic_Block;

   ------------------
   -- First_CBI_PC --
   ------------------

   function First_CBI_PC (D_Occ : Decision_Occurrence_Access) return Pc_Type is
   begin
      return D_Occ.Conditional_Branches.First_Element;
   end First_CBI_PC;

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

   -------------------------
   -- Is_Finalizer_Symbol --
   -------------------------

   function Is_Finalizer_Symbol (Name : String) return Boolean is
   begin
      return Match (Name, Finalizer_Symbol_Pattern);
   end Is_Finalizer_Symbol;

   --------------------------------------
   -- Is_Finalization_Expansion_Symbol --
   --------------------------------------

   function Is_Finalization_Expansion_Symbol (Name : String) return Boolean is
   begin
      return Match (Name, Pre_Finalizer_Symbol_Pattern);
   end Is_Finalization_Expansion_Symbol;

   ---------------
   -- Write_Map --
   ---------------

   procedure Write_Map (Filename : String) is
      Trace_File : Trace_File_Type;
   begin
      Create_Trace_File (Filename, Qemu_Traces.Decision_Map, Trace_File);
      Write_Trace_File (Trace_File, Decision_Map_Base);
   end Write_Map;

end Decision_Map;
