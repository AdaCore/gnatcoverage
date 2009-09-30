------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Ada.Directories;   use Ada.Directories;
with Ada.Text_IO;       use Ada.Text_IO;
with Interfaces;        use Interfaces;

with Elf_Disassemblers; use Elf_Disassemblers;
with Hex_Images;        use Hex_Images;
with SC_Obligations;    use SC_Obligations;
with Qemu_Traces;
with Sources;           use Sources;
with Files_Table;       use Files_Table;
with Strings;           use Strings;
with Switches;          use Switches;
with Traces;            use Traces;
with Traces_Dbase;      use Traces_Dbase;
with Traces_Elf;        use Traces_Elf;
with Traces_Files;      use Traces_Files;
with Traces_Names;      use Traces_Names;
with Types;             use Types;

package body Decision_Map is

   use Ada.Containers;

   Decision_Map_Base : Traces_Base;
   --  The decision map is a list of code addresses, so we manage it as a
   --  trace database.

   ---------------------------------
   -- Control flow graph analysis --
   ---------------------------------

   --  Conditional branch instructions that correspond to conditions in the
   --  sources are annotated with information relating the corresponding
   --  edges of the control flow graph with the logical structure of the
   --  decision.

   type Edge_Dest_Kind is (Unknown, Condition, Outcome);
   --  Destination of an edge in the control flow graph within an occurrence
   --  of a decision: not determined yet, test another condition, final
   --  decision outcome reached.

   --  Cond_Edge_Info is the information associated with each edge of the
   --  control flow graph.

   type Cond_Edge_Info is record
      Origin         : Tristate := Unknown;
      --  If not Unknown, indicate which value of the tested condition causes
      --  this edge to be taken.

      Destination    : Pc_Type;
      --  Edge destination

      Dest_Kind      : Edge_Dest_Kind := Unknown;
      --  Edge destination classification, if known

      Next_Condition : Integer := -1;
      --  For the case where Dest_Kind is Condition, index within decision of
      --  the next tested condition.

      Outcome        : Tristate       := Unknown;
      --  For the case where Dest_Kind is Outcome, corresponding valuation of
      --  the decision, if known.
   end record;

   --  Cond_Branch_Info is the information associated with each conditional
   --  branch instruction.

   type Edge_Kind is (Branch, Fallthrough);
   type Edges_Type is array (Edge_Kind) of Cond_Edge_Info;

   type Cond_Branch_Info is record
      Condition         : SCO_Id;
      --  Condition being tested by the conditional branch instruction

      Basic_Block_Start : Pc_Type;
      --  First PC of the basic block containing conditional branch instruction

      Edges : Edges_Type;
      --  Edge information for the branch case and fallthrough case
   end record;

   package Cond_Branch_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Pc_Type,
      Element_Type => Cond_Branch_Info,
      "<"          => Interfaces."<");

   Cond_Branch_Map : Cond_Branch_Maps.Map;

   type Condition_Occurrence_Array is array (Natural range <>) of Pc_Type;
   --  In a decision occurrence, each tested condition is represented by
   --  a conditional branch instruction.

   type Decision_Occurrence (Last_Cond_Index : Natural) is limited record
      Decision              : SCO_Id;
      --  The decision being evaluated

      Condition_Occurrences : Condition_Occurrence_Array
                                (0 .. Last_Cond_Index) := (others => No_PC);
      --  The corresponding evaluations of the conditions in the decision

      Seen_Condition : Integer := -1;
   end record;
   type Decision_Occurrence_Access is access all Decision_Occurrence;

   package Decision_Occurrence_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Nat,
      Element_Type => Decision_Occurrence_Access);
   use type Decision_Occurrence_Vectors.Vector;
   --  A list of decision occurrences, used for Decision_Occurrence_Maps below,
   --  and also to maintain the stack of open decision occurrences while
   --  analysing object code.

   package Decision_Occurrence_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => SCO_Id,
      Element_Type => Decision_Occurrence_Vectors.Vector);
   Decision_Occurrence_Map : Decision_Occurrence_Maps.Map;
   pragma Unreferenced (Decision_Occurrence_Map);
   --  The decision occurrence map lists all object code occurrences of each
   --  source decision (identified by its SCO_Id).

   type Cond_Branch_Context is limited record
      Decision_Stack : Decision_Occurrence_Vectors.Vector;
      --  The stack of open decision occurrences
   end record;

   procedure Analyze;
   --  Build the decision map from the executable, debug information and
   --  the Source Coverage Obligations (which must have been loaded already).

   procedure Analyze_Routine
     (Name : String_Acc;
      Info : in out Subprogram_Info);
   --  Build decision map for the given subprogram

   procedure Analyze_Conditional_Branch
     (Exe               : Exe_File_Acc;
      Insn              : Binary_Content;
      Branch_Dest       : Pc_Type;
      Fallthrough_Dest  : Pc_Type;
      Basic_Block_Start : Pc_Type;
      Ctx               : in out Cond_Branch_Context);
   --  Process one conditional branch instruction: identify relevant source
   --  coverable construct, and record association in the decision map.

   procedure Analyze_Decision_Occurrence
     (D_Occ : Decision_Occurrence_Access);
   --  Perform logical structure analysis of the given decision occurrence

   type Report_Kind is (Notice, Warning, Error);
   procedure Report
     (PC   : Pc_Type;
      Msg  : String;
      Kind : Report_Kind := Error);
   --  Output diagnostic message during control flow analysis. Messages with
   --  Notice kind are omitted unless global flag Verbose is set. A prefix is
   --  prepended depending On message kind:
   --     *** notice
   --     ??? warning
   --     !!! error

   -------------
   -- Analyze --
   -------------

   procedure Analyze is
   begin
      Init_Base (Decision_Map_Base, Full_History => False);
      Traces_Names.Iterate (Analyze_Routine'Access);

      if Verbose then
         Report_SCOs_Without_Code;
      end if;
   end Analyze;

   --------------------------------
   -- Analyze_Conditional_Branch --
   --------------------------------

   procedure Analyze_Conditional_Branch
     (Exe               : Exe_File_Acc;
      Insn              : Binary_Content;
      Branch_Dest       : Pc_Type;
      Fallthrough_Dest  : Pc_Type;
      Basic_Block_Start : Pc_Type;
      Ctx               : in out Cond_Branch_Context)
   is
      Sloc : Source_Location := Get_Sloc (Exe.all, Insn'First);
      --  Source location of Insn

      SCO : SCO_Id;

   begin
      if Sloc = Sources.No_Location then
         --  No associated source, so no further processing required for source
         --  coverage analysis.

         return;
      end if;

      --  Normalize source file name

      Sloc.Source_File :=
        Get_Index (Simple_Name (Get_Name (Sloc.Source_File)));

      --  Look up SCO

      SCO := Sloc_To_SCO (Sloc);

      if Verbose then
         Put_Line ("cond branch at " & Hex_Image (Insn'First)
                   & " (from " & Hex_Image (Basic_Block_Start) & ")"
                   & " " & Image (Sloc)
                   & ": " & Image (SCO));

      end if;

      if SCO = No_SCO_Id or else Kind (SCO) /= Condition then
         return;
      end if;

      --  Here for conditional branches that have an associated Condition SCO

      --  Mark instruction address for full (historical) traces collection
      --  (for MC/DC source coverage analysis).

      Add_Entry
        (Base  => Decision_Map_Base,
         First => Insn'First,
         Last  => Insn'Last,
         Op    => 0);

      --  Record address in SCO descriptor

      Add_Address (SCO, Insn'First);

      --  Update control flow information

      Process_Condition :
      declare
         D_SCO : constant SCO_Id := Parent (SCO);
         --  Corresponding decision

         Parent_SCO : SCO_Id;
         --  Parent SCO of D_SCO, if appropriate

         DS_Top : Decision_Occurrence_Access;
         --  Innermost currently open decision

         Cond_Index : constant Natural := Index (SCO);
         --  Index of SCO in D_SCO

         procedure Check_Condition_Index (CI : Natural);
         --  Check whether we expect to evaluate CI

         procedure Check_Condition_Index (CI : Natural) is
            Expected_CI : constant Natural := DS_Top.Seen_Condition + 1;
         begin
            if Expected_CI /= CI then
               Report
                 (Insn'First,
                  "evaluation of unexpected condition" & CI'Img
                  & " (expected" & Expected_CI'Img & ")"
                  & " in decision " & Image (DS_Top.Decision));
            end if;
         end Check_Condition_Index;

      --  Start of processing for Process_Condition

      begin
         if Ctx.Decision_Stack.Length > 0 then
            DS_Top := Ctx.Decision_Stack.Element
                        (Ctx.Decision_Stack.Last_Index);

            if DS_Top.Decision /= D_SCO then
               --  Check that the condition containing the nested decision is
               --  the expected one.

               Parent_SCO := Parent (D_SCO);

               if Kind (Parent_SCO) /= Condition
                 or else Parent (Parent_SCO) /= DS_Top.Decision
               then
                  Report
                    (Insn'First,
                     "evaluation of unexpected"
                     & " nested decision " & Image (D_SCO)
                     & " within " & Image (DS_Top.Decision));

                  --  If D_SCO is a decision higher on the stack, pop (and
                  --  discard) all pending evaluations.

                  Unwind_Loop :
                  for J in reverse Ctx.Decision_Stack.First_Index
                    .. Ctx.Decision_Stack.Last_Index - 1
                  loop
                     if Ctx.Decision_Stack.Element (J).Decision
                       = D_SCO
                     then
                        DS_Top := Ctx.Decision_Stack.Element (J);
                        declare
                           Next : Decision_Occurrence_Vectors.Cursor
                             := Ctx.Decision_Stack.To_Cursor
                               (J + 1);
                        begin
                           Ctx.Decision_Stack.Delete
                             (Position => Next,
                              Count    =>
                                Count_Type
                                  (Ctx.Decision_Stack.Last_Index
                                   - J));
                        end;
                        exit Unwind_Loop;
                     end if;
                  end loop Unwind_Loop;

               else
                  --  OK, we are in the correct enclosing decision, now check
                  --  the enclosing condition of the nested decision is the one
                  --  being evaluated.

                  Check_Condition_Index (Index (Parent_SCO));
               end if;

               --  The following causes a new context to be pushed for this
               --  decision occurrence.

               DS_Top := null;
            end if;
         end if;

         if DS_Top = null then
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

         if DS_Top.Condition_Occurrences (Cond_Index) /= No_PC then
            Report (Insn'First,
                    "duplicate evaluation of condition"
                    & Cond_Index'Img
                    & " in decision " & Image (DS_Top.Decision));
         else
            --  Record condition occurrence

            if Cond_Index > DS_Top.Seen_Condition then
               DS_Top.Seen_Condition := Cond_Index;
            end if;
            DS_Top.Condition_Occurrences (Cond_Index) := Insn'First;
            Cond_Branch_Map.Insert
              (Insn'First,
               Cond_Branch_Info'
                 (Condition         => SCO,
                  Basic_Block_Start => Basic_Block_Start,
                  Edges             =>
                    (Branch =>
                       (Destination => Branch_Dest,
                        others      => <>),
                     Fallthrough =>
                       (Destination => Fallthrough_Dest,
                        others      => <>))));
         end if;

         if Cond_Index = Last_Cond_Index (D_SCO) then
            --  Evaluated last condition: analyze & pop top decision

            Analyze_Decision_Occurrence (Ctx.Decision_Stack.Last_Element);
            Ctx.Decision_Stack.Delete_Last;
         end if;
      end Process_Condition;
   end Analyze_Conditional_Branch;

   ---------------------------------
   -- Analyze_Decision_Occurrence --
   ---------------------------------

   procedure Analyze_Decision_Occurrence
     (D_Occ : Decision_Occurrence_Access)
   is
      Last_Seen_Condition_PC : constant Pc_Type :=
                                 D_Occ.Condition_Occurrences
                                   (D_Occ.Seen_Condition);
      pragma Assert (Last_Seen_Condition_PC /= No_PC);

      --  Note: all the analysis is done under control of an initial check that
      --    D_Occ.Seen_Condition = D_Occ.Last_Condition_Index

      Last_CBI : constant Cond_Branch_Info :=
                   Cond_Branch_Map.Element (Last_Seen_Condition_PC);

      function Find_Condition_Basic_Block (PC : Pc_Type) return Integer;
      --  Return the index of the condition occurrence within D_Occ whose
      --  enclosing basic block contains PC, if any, or -1 if there is no such
      --  condition occurrence.

      procedure Label_Destination
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info;
         Edge           : Edge_Kind);
      --  Test if Edge's destination matches either of Last_CBI's edges'
      --  destination, and if so mark it as an outcome destination.

      procedure Label_Destinations
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info);
      --  Identify destination kind of each edge of CBI

      --------------------------------
      -- Find_Condition_Basic_Block --
      --------------------------------

      function Find_Condition_Basic_Block (PC : Pc_Type) return Integer is
      begin
         for J in D_Occ.Condition_Occurrences'Range loop
            declare
               Cond_Branch_PC : Pc_Type renames
                                  D_Occ.Condition_Occurrences (J);
            begin
               if Cond_Branch_PC /= No_PC
                    and then
                  PC in Cond_Branch_Map.Element (Cond_Branch_PC).
                          Basic_Block_Start .. Cond_Branch_PC
               then
                  return J;
               end if;
            end;
         end loop;

         return -1;
      end Find_Condition_Basic_Block;

      -----------------------
      -- Label_Destination --
      -----------------------

      procedure Label_Destination
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info;
         Edge           : Edge_Kind)
      is
         Edge_Name         : constant String := Edge'Img;
         Edge_Info         : Cond_Edge_Info renames CBI.Edges (Edge);
         Destination_Index : constant Integer :=
                               Find_Condition_Basic_Block
                                 (Edge_Info.Destination);

      begin
         pragma Assert (Edge_Info.Dest_Kind = Unknown);

         --  Check for outcome destination

         --  HYP: a branch destination is an outcome when it is equal to one
         --  of the branch destinations of the last conditional branch in
         --  the decision. Note: some outcome destinations do not satisfy
         --  this property (but there is no indication that any non-outcome
         --  destination does satisfy it).

         --  HYP: a branch destination is an outcome when it branches past
         --  the conditional branch instruction for the last condition.

         if Edge_Info.Destination
              = Last_CBI.Edges (Branch).Destination
           or else Edge_Info.Destination
                     = Last_CBI.Edges (Fallthrough).Destination
           or else Edge_Info.Destination > Last_Seen_Condition_PC
         then
            Edge_Info.Dest_Kind := Outcome;

            --  Check that there is a possible outcome from this condition

            declare
               Outcome_Seen   : Boolean;
               Outcome_Origin : Tristate := Unknown;
            begin
               for J in Boolean'Range loop
                  if Outcome (CBI.Condition, J) /= Unknown then
                     Outcome_Seen := True;
                     if Outcome_Origin = Unknown then
                        Outcome_Origin := To_Tristate (J);
                     else
                        Outcome_Origin := Unknown;
                     end if;
                  end if;
               end loop;

               if not Outcome_Seen then
                  Report
                    (Cond_Branch_PC,
                     Edge_Name & " destination unexpectedly out of condition");

               elsif Outcome_Origin /= Unknown then
                  --  If there is only one outcome edge (and the other is a
                  --  condition), then Outcome_Origin is the valuation of the
                  --  condition that causes it to be taken.

                  Edge_Info.Origin := Outcome_Origin;
                  Edge_Info.Outcome :=
                    Outcome (CBI.Condition, To_Boolean (Outcome_Origin));
                  pragma Assert (Edge_Info.Outcome /= Unknown);
               end if;
            end;
         end if;

         --  Check for internal destination

         if Destination_Index in 0 .. D_Occ.Last_Cond_Index then
            --  Destination is a basic block that tests a condition within this
            --  decision occurrence.

            if Edge_Info.Dest_Kind /= Unknown then
               Report
                 (Cond_Branch_PC,
                  Edge_Name & " destination is both final and intermediate");

            else
               Edge_Info.Dest_Kind := Condition;
               Edge_Info.Next_Condition := Destination_Index;

               --  Check that the next condition is a possible successor, and
               --  label edge origin (that is, the valuation of the tested
               --  condition that causes the successor in question to be
               --  evaluated next).

               for J in Boolean'Range loop
                  declare
                     Next_C : constant SCO_Id :=
                                Next_Condition (CBI.Condition, J);
                  begin
                     if Next_C /= No_SCO_Id
                       and then Index (Next_C) = Destination_Index
                     then
                        pragma Assert (Next_C =
                                         Cond_Branch_Map.Element
                                           (D_Occ.Condition_Occurrences
                                             (Destination_Index)).Condition);
                        Edge_Info.Origin := To_Tristate (J);
                        exit;
                     end if;
                  end;
               end loop;

               if Edge_Info.Origin = Unknown then
                  Report
                    (Cond_Branch_PC,
                     Edge_Name
                     & " does not branch to a possible successor"
                     & " condition");
               end if;
            end if;
         end if;

         --  Report destinations we can't label (warning only, maybe we can
         --  infer branch kind later).

         if Edge_Info.Dest_Kind = Unknown then
            Report (Cond_Branch_PC,
                    "unable to label " & Edge_Name
                    & " destination " & Hex_Image (Edge_Info.Destination),
                    Kind => Warning);
         end if;
      end Label_Destination;

      ------------------------
      -- Label_Destinations --
      ------------------------

      procedure Label_Destinations
        (Cond_Branch_PC : Pc_Type;
         CBI            : in out Cond_Branch_Info)
      is
         function Dest_Image (Edge : Edge_Kind) return String;
         --  Return string representation of the given edge of CBI

         ----------------
         -- Dest_Image --
         ----------------

         function Dest_Image (Edge : Edge_Kind) return String is
            Edge_Info : Cond_Edge_Info renames CBI.Edges (Edge);
         begin
            return Edge'Img
              & " = " & Hex_Image (Edge_Info.Destination)
              & " " & Edge_Info.Dest_Kind'Img;
         end Dest_Image;

      begin
         --  Label each destination

         Label_Destination (Cond_Branch_PC, CBI, Branch);
         Label_Destination (Cond_Branch_PC, CBI, Fallthrough);

         Report
           (Cond_Branch_PC,
            "destination kinds: " & Dest_Image (Branch)
            & " / " & Dest_Image (Fallthrough),
            Kind => Notice);

      end Label_Destinations;

   begin
      if D_Occ.Last_Cond_Index /= D_Occ.Seen_Condition then
         --  Report PC of last seen condition in decision occurrence, if it is
         --  not the final condition of the decision.

         Report (Last_Seen_Condition_PC,
                 "last seen condition for decision "
                 & Image (D_Occ.Decision) & " is not final one");
         return;
      end if;

      --  Label edge destinations

      for J in D_Occ.Condition_Occurrences'Range loop
         declare
            use Cond_Branch_Maps;
            Cur : constant Cond_Branch_Maps.Cursor :=
                    Cond_Branch_Map.Find (D_Occ.Condition_Occurrences (J));
         begin
            if D_Occ.Condition_Occurrences (J) /= No_PC then
               Cond_Branch_Map.Update_Element (Cur, Label_Destinations'Access);
            end if;
         end;
      end loop;
   end Analyze_Decision_Occurrence;

   ---------------------
   -- Analyze_Routine --
   ---------------------

   procedure Analyze_Routine
     (Name : String_Acc;
      Info : in out Subprogram_Info)
   is
      PC       : Pc_Type;
      Insn_Len : Natural;

      Current_Basic_Block_Start : Pc_Type;
      Context : Cond_Branch_Context;

   --  Start of processing for Analyze_Routine

   begin
      if Verbose then
         Put_Line ("Building decision map for " & Name.all);
      end if;
      Build_Debug_Lines (Info.Exec.all);

      if Info.Insns = null then
         Put_Line ("No instructions for " & Name.all);
         return;
      end if;

      --  Iterate over instructions, looking for conditional branches

      PC := Info.Insns'First;
      Current_Basic_Block_Start := PC;
      while PC < Info.Insns'Last loop
         Insn_Len :=
           Disa_For_Machine (Machine).
             Get_Insn_Length (Info.Insns (PC .. Info.Insns'Last));

         declare
            Insn : Binary_Content renames
                     Info.Insns (PC .. PC + Pc_Type (Insn_Len) - 1);

            Branch     : Branch_Kind;
            Flag_Indir : Boolean;
            Flag_Cond  : Boolean;
            Dest       : Pc_Type;
            --  Properties of Insn

         begin
            Disa_For_Machine (Machine).Get_Insn_Properties
              (Insn_Bin   => Insn,
               Pc         => PC,
               Branch     => Branch,
               Flag_Indir => Flag_Indir,
               Flag_Cond  => Flag_Cond,
               Dest       => Dest);

            if Branch = Br_Jmp and then Flag_Cond then
               Analyze_Conditional_Branch
                 (Info.Exec,
                  Insn              => Insn,
                  Branch_Dest       => Dest,
                  Fallthrough_Dest  => Insn'Last + 1,
                  Basic_Block_Start => Current_Basic_Block_Start,
                  Ctx               => Context);
            end if;

            PC := PC + Pc_Type (Insn_Len);

            --  Handle case where PC wraps

            exit when PC = 0;

            if Branch /= Br_None then
               Current_Basic_Block_Start := PC;
            end if;
         end;
      end loop;
   end Analyze_Routine;

   ------------------------
   -- Build_Decision_Map --
   ------------------------

   procedure Build_Decision_Map (Exec_Name : String)
   is
      Exec : aliased Exe_File_Type;

      Text_Start : constant Pc_Type := 0;
      --  Should be a global option???

      Decision_Map_Filename     : String_Acc := null;
      Decision_Map_Suffix       : constant String := ".dmap";
      --  Decision map filename is constructed by appending the suffix to the
      --  executable image name.
   begin
      if Routine_List_Filename /= null then
         Traces_Names.Read_Routines_Name_From_Text
           (Routine_List_Filename.all);
      else
         Traces_Elf.Read_Routines_Name
           (Exec_Name,
            Exclude   => False,
            Keep_Open => False);
      end if;

      Decision_Map_Filename :=
        new String'(Exec_Name & Decision_Map_Suffix);
      Open_File (Exec, Exec_Name, Text_Start);
      Build_Sections (Exec);
      Build_Symbols (Exec'Unchecked_Access);
      Load_Code_And_Traces (Exec'Unchecked_Access, Base => null);
      Decision_Map.Analyze;
      Decision_Map.Write_Map (Decision_Map_Filename.all);
      Close_File (Exec);
   end Build_Decision_Map;

   ------------
   -- Report --
   ------------

   procedure Report
     (PC   : Pc_Type;
      Msg  : String;
      Kind : Report_Kind := Error)
   is
      subtype Prefix_Str is String (1 .. 3);
      Prefix : constant array (Report_Kind) of Prefix_Str :=
                 (Notice  => "***",
                  Warning => "???",
                  Error   => "!!!");
   begin
      if Verbose or else Kind > Notice then
         Put_Line (Prefix (Kind) & " " & Hex_Image (PC) & ": " & Msg);
      end if;
   end Report;

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
