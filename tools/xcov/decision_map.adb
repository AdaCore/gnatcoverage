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
   pragma Unreferenced (Condition, Outcome);
   --  Destination of an edge in the control flow graph within an occurrence
   --  of a decision: not determined yet, test another condition, final
   --  decision outcome reached.

   --  Cond_Edge_Info is the information associated with each edge of the
   --  control flow graph.

   type Cond_Edge_Info is record
      Origin      : Tristate := Unknown;
      --  If not Unknown, indicate which value of the tested condition causes
      --  this edge to be taken.

      Destination : Pc_Type;
      --  Edge destination

      Dest_Kind   : Edge_Dest_Kind := Unknown;
      --  Edge destination classification, if known

      Outcome     : Tristate       := Unknown;
      --  For the case where Dest_Kind is Outcome, corresponding valuation of
      --  the decision, if known.
   end record;

   --  Cond_Branch_Info is the information associated with each conditional
   --  branch instruction.

   type Cond_Branch_Info is record
      Condition        : SCO_Id;
      --  Condition being tested by the conditional branch instruction

      Branch_Edge,
      Fallthrough_Edge : Cond_Edge_Info;
      --  Edge information for the branch case and fallthrough case
   end record;

   package Cond_Branch_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Pc_Type,
      Element_Type => Cond_Branch_Info,
      "<"          => Interfaces."<");

   Cond_Branch_Map : Cond_Branch_Maps.Map;

   type Condition_Occurrence_Array is array (Positive range <>) of Pc_Type;
   --  In a decision occurrence, each tested condition is represented by
   --  a conditional branch instruction.

   type Decision_Occurrence (Last_Cond_Index : Natural) is limited record
      Decision              : SCO_Id;
      --  The decision being evaluated

      Condition_Occurrences : Condition_Occurrence_Array
                                (0 .. Last_Cond_Index);
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
     (Exe              : Exe_File_Acc;
      Insn             : Binary_Content;
      Branch_Dest      : Pc_Type;
      Fallthrough_Dest : Pc_Type;
      Ctx              : in out Cond_Branch_Context);
   --  Process one conditional branch instruction: identify relevant source
   --  coverable construct, and record association in the decision map.

   procedure Report (Msg : String) renames Put_Line;
   --  Output diagnostic message when an anomaly in the control flow graph
   --  is encountered.

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
     (Exe              : Exe_File_Acc;
      Insn             : Binary_Content;
      Branch_Dest      : Pc_Type;
      Fallthrough_Dest : Pc_Type;
      Ctx              : in out Cond_Branch_Context)
   is
      First_Sloc, Last_Sloc : Source_Location;
      --  Source location range of Insn

      SCO : SCO_Id;

   begin
      Get_Sloc_Range (Exe.all, Insn'First, First_Sloc, Last_Sloc);

      if First_Sloc = Sources.No_Location then
         --  No associated source, so no further processing required for source
         --  coverage analysis.

         return;
      end if;

      --  Normalize source file name

      pragma Assert (First_Sloc.Source_File = Last_Sloc.Source_File);

      First_Sloc.Source_File :=
        Get_Index (Simple_Name (Get_Name (First_Sloc.Source_File)));
      Last_Sloc.Source_File := First_Sloc.Source_File;

      --  Look up SCO

      SCO := Slocs_To_SCO (First_Sloc, Last_Sloc);

      if Verbose then
         Put_Line ("cond branch at " & Hex_Image (Insn'First)
                   & " " & Image (First_Sloc) & "-" & Image (Last_Sloc)
                   & ": " & Image (SCO));

      end if;

      if SCO /= No_SCO_Id then
         case Kind (SCO) is
            when Condition =>
               --  For conditions, we need full (historical) traces in order to
               --  provide MC/DC source coverage analysis.

               Add_Entry
                 (Base  => Decision_Map_Base,
                  First => Insn'First,
                  Last  => Insn'Last,
                  Op    => 0);

               Add_Address (SCO, Insn'First);

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
                     Expected_CI : constant Natural :=
                                     DS_Top.Seen_Condition + 1;
                  begin
                     if Expected_CI /= CI then
                        Report
                          (Hex_Image (Insn'First)
                           & ": evaluation of unexpected condition" & CI'Img
                           & " (expected" & Expected_CI'Img & ")"
                           & " in decision SCO#"
                           & Image (DS_Top.Decision));
                     end if;
                  end Check_Condition_Index;

               --  Start of processing for Process_Condition

               begin
                  if Ctx.Decision_Stack.Length > 0 then
                     DS_Top := Ctx.Decision_Stack.Element
                                 (Ctx.Decision_Stack.Last_Index);

                     if DS_Top.Decision /= D_SCO then
                        --  Check that the condition containing the nested
                        --  decision is the expected one.

                        Parent_SCO := Parent (D_SCO);

                        if Kind (Parent_SCO) /= Condition
                          or else Parent (Parent_SCO) /= DS_Top.Decision
                        then
                           Report
                             (Hex_Image (Insn'First)
                              & ": evaluation of unexpected"
                              & " nested decision SCO#" & Image (D_SCO)
                              & " within SCO#" & Image (DS_Top.Decision));

                           --  If D_SCO is a decision higher on the stack,
                           --  pop (and discard) all pending evaluations.

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
                           --  OK, we are in the correct enclosing decision,
                           --  now check the enclosing condition of the
                           --  nested decision is the one being evaluated.

                           Check_Condition_Index (Index (Parent_SCO));
                        end if;

                        --  Push new context for this decision occurrence

                        DS_Top := new Decision_Occurrence
                          (Last_Cond_Index (D_SCO));
                        Ctx.Decision_Stack.Append (DS_Top);
                     end if;
                  end if;

                  --  Here after context for current decision has been pushed,
                  --  if needed.

                  pragma Assert (DS_Top.Decision = D_SCO);
                  Check_Condition_Index (Cond_Index);

                  if DS_Top.Condition_Occurrences (Cond_Index) /= No_PC then
                     Report (Hex_Image (Insn'First)
                             & ": duplicate evaluation of condition"
                             & Cond_Index'Img
                             & " in decision SCO#"
                             & Image (DS_Top.Decision));
                  else
                     --  Record condition occurrence

                     DS_Top.Condition_Occurrences (Cond_Index) := Insn'First;
                     Cond_Branch_Map.Insert
                       (Insn'First,
                        Cond_Branch_Info'
                          (Condition        => SCO,
                           Branch_Edge      =>
                             (Destination => Branch_Dest,
                              others      => <>),
                           Fallthrough_Edge =>
                             (Destination => Fallthrough_Dest,
                              others      => <>)));
                  end if;

                  if Cond_Index = Last_Cond_Index (D_SCO) then
                     --  Evaluated last condition: pop top decision

                     Ctx.Decision_Stack.Delete_Last;
                  end if;
               end Process_Condition;

            when others =>
               null;
         end case;
      end if;
   end Analyze_Conditional_Branch;

   ---------------------
   -- Analyze_Routine --
   ---------------------

   procedure Analyze_Routine
     (Name : String_Acc;
      Info : in out Subprogram_Info)
   is
      PC       : Pc_Type;
      Insn_Len : Natural;

      Context : Cond_Branch_Context;

   --  Start of processing for Analyze_Routine

   begin
      Put_Line ("Building decision map for " & Name.all);
      Build_Debug_Lines (Info.Exec.all);

      if Info.Insns = null then
         Put_Line ("No instructions for " & Name.all);
         return;
      end if;

      --  Iterate over instructions, looking for conditional branches

      PC := Info.Insns'First;
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
                  Insn             => Insn,
                  Branch_Dest      => Dest,
                  Fallthrough_Dest => Insn'Last + 1,
                  Ctx              => Context);
            end if;
         end;

         PC := PC + Pc_Type (Insn_Len);

         --  Handle case where PC wraps

         exit when PC = 0;
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
