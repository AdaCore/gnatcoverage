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

with Ada.Text_IO; use Ada.Text_IO;

with Sources; use Sources;
with Traces; use Traces;
with Traces_Elf; use Traces_Elf;
with Strings; use Strings;
with Traces_Dbase; use Traces_Dbase;
with Disa_Symbolize; use Disa_Symbolize;

package Traces_Sources is

   type DO178B_Level_Type is (Level_Raw, Level_A, Level_C);
   --  Control the output line state.
   --  Level_Raw gives the most detailed state.

   DO178B_Level : DO178B_Level_Type := Level_Raw;
   --  DO178B level for output state

   --  Coverage state of a source line of code.
   type Line_State is
     (Not_Covered,
      --  No instructions executed

      Partially_Covered,
      --  Some instructions not covered in the line

      Covered, -- Branch_Partially_Covered
      --  Covered at instruction level but some branches partially covered

      Branch_Taken,
      Branch_Fallthrough,
      --  All instructions executed, only one side of branch taken

      Branch_Covered,
      --  Covered at branch level

      Covered_No_Branch,
      --  Same as covered but no branches

      No_Code
      --  Initial state: no code for this line
     );

   subtype Known_Trace_State is
     Trace_State range Not_Covered .. Trace_State'Last;
   type State_Update_Table_Type is array (Line_State, Known_Trace_State)
     of Line_State;

   Update_Table : constant State_Update_Table_Type;
   --  Table to merge trace_states.
   --  The initial Line_State must be No_Code.
   --  Update the line_state using:
   --    New_State := Update_Table (Old_State, Trace_State);

   type State_Char_Array is array (Line_State) of Character;
   State_Char : constant State_Char_Array;
   --  Characters identifying a Line_State.

   type State_Map_Array is array (DO178B_Level_Type, Line_State) of Line_State;
   State_Map : constant State_Map_Array;
   --  Degradation of a state according to the level.

   function Find_File (Filename : String_Acc) return Source_File_Index;
   --  Find or create a new source file.

   procedure Add_Line (File : Source_File_Index;
                       Line : Natural;
                       Info : Addresses_Info_Acc;
                       Base : Traces_Base_Acc;
                       Exec : Exe_File_Acc);
   --  Lets know File that Line exists and add the addresses range for Info.
   --  (This knowledge comes from debugging informations).

   procedure Add_Line_State (File : Source_File_Index;
                             Line : Natural;
                             State : Traces.Trace_State);
   --  Same as Add_Line but with a State.
   --  (The knowledge comes from execution traces).

   Flag_Show_Missing : Boolean := False;
   --  If True, Disp_Line_State displays info for files that are not found
   --  Why isn't this a parameter of Disp_Line_State???

   procedure Disp_File_Summary;
   --  Display per-file summary

   procedure Add_Source_Rebase (Old_Prefix : String;
                                New_Prefix : String);
   --  Needs comment???

   procedure Add_Source_Search (Prefix : String);
   --  Needs comment???

   procedure Dump_Routines_Traces;
   --  Needs comment???

   procedure Dump_Uncovered_Routines (Report : File_Access);
   --  Go through the routine database and dump the list of uncovered
   --  routines into Report.

private
   type Line_Chain;
   type Line_Chain_Acc is access Line_Chain;

   type Line_Chain is record
      Line : Addresses_Info_Acc;
      Base : Traces_Base_Acc;
      Exec : Exe_File_Acc;
      Link : Line_Chain_Acc;
   end record;

   --  Data associated with a SLOC
   --  To a Sloc or to a line???

   type Line_Info is record
      State : Line_State;
      --  Coverage state

      First_Line, Last_Line : Line_Chain_Acc;
      --  Object code for this line.
      --  Confusing names, this record is related to a single line so why
      --  does it have a first line and a last line???
   end record;

   --  Tables below need comments???

   Update_Table : constant State_Update_Table_Type :=
     (
      No_Code =>
        (Not_Covered => Not_Covered,
         Covered => Covered_No_Branch,
         Branch_Taken => Branch_Taken,
         Fallthrough_Taken => Branch_Fallthrough,
         Both_Taken => Branch_Covered),
      Not_Covered =>
        (Not_Covered => Not_Covered,
         others => Partially_Covered),
      Partially_Covered =>
        (others => Partially_Covered),
      Covered =>
        (Not_Covered => Partially_Covered,
         others => Covered),
      Covered_No_Branch =>
        (Not_Covered => Partially_Covered,
         Covered => Covered_No_Branch,
         Branch_Taken => Branch_Taken,
         Fallthrough_Taken => Branch_Fallthrough,
         Both_Taken => Branch_Covered),
      Branch_Taken =>
        (Not_Covered => Partially_Covered,
         Covered => Branch_Taken,
         Branch_Taken => Covered,
         Fallthrough_Taken => Covered,
         Both_Taken => Covered),
      Branch_Fallthrough =>
        (Not_Covered => Partially_Covered,
         Covered => Branch_Fallthrough,
         Branch_Taken => Covered,
         Fallthrough_Taken => Covered,
         Both_Taken => Covered),
      Branch_Covered =>
        (Not_Covered => Partially_Covered,
         Branch_Taken | Fallthrough_Taken => Covered,
         Covered | Both_Taken => Branch_Covered)
      );

   State_Char : constant State_Char_Array :=
     (No_Code => '.',
      Not_Covered => '-',
      Partially_Covered => '!',
      Covered => '?',
      Covered_No_Branch => '+',
      Branch_Taken => '>',
      Branch_Fallthrough => 'v',
      Branch_Covered => '*');

   State_Map : constant State_Map_Array :=
     (Level_Raw => (No_Code => No_Code,
                    Not_Covered => Not_Covered,
                    Partially_Covered => Partially_Covered,
                    Covered => Covered,
                    Covered_No_Branch => Covered_No_Branch,
                    Branch_Taken => Branch_Taken,
                    Branch_Fallthrough => Branch_Fallthrough,
                    Branch_Covered => Branch_Covered),
      Level_A   => (No_Code => No_Code,
                    Not_Covered => Not_Covered,
                    Partially_Covered => Partially_Covered,
                    Covered => Partially_Covered,
                    Covered_No_Branch => Covered_No_Branch,
                    Branch_Taken => Partially_Covered,
                    Branch_Fallthrough => Partially_Covered,
                    Branch_Covered => Covered_No_Branch),
      Level_C   => (No_Code => No_Code,
                    Not_Covered => Not_Covered,
                    Partially_Covered => Covered_No_Branch,
                    Covered => Covered_No_Branch,
                    Covered_No_Branch => Covered_No_Branch,
                    Branch_Taken => Covered_No_Branch,
                    Branch_Fallthrough => Covered_No_Branch,
                    Branch_Covered => Covered_No_Branch));

   type Stat_Array is array (Line_State) of Natural;
   function Get_Stat_String (Stats : Stat_Array) return String;

   type Counters is record
      Fully   : Natural;
      Partial : Natural;
      Total   : Natural;
   end record;

   function Get_Counters (Stats : Stat_Array) return Counters;

   type Pretty_Printer is abstract tagged limited record
      Need_Sources : Boolean;
      Global_Stats : Stat_Array;
      Show_Asm     : Boolean;
   end record;

   procedure Pretty_Print_Start (Pp : in out Pretty_Printer) is null;
   --  Called once at the beginning of the process.

   procedure Pretty_Print_Finish (Pp : in out Pretty_Printer) is null;
   --  Called once at the end of the process.

   procedure Pretty_Print_File (Pp : in out Pretty_Printer;
                                Source_Filename : String;
                                Stats : Stat_Array;
                                Has_Source : Boolean;
                                Skip : out Boolean) is abstract;
   --  Called at the beginning of a source file display.

   --  Subprograms below need comments???

   procedure Pretty_Print_Line (Pp : in out Pretty_Printer;
                                Line_Num : Natural;
                                State : Line_State;
                                Line : String) is abstract;

   procedure Pretty_Print_Label (Pp : in out Pretty_Printer;
                                 Label : String) is null;

   procedure Pretty_Print_Insn (Pp : in out Pretty_Printer;
                                Pc : Pc_Type;
                                State : Trace_State;
                                Insn : Binary_Content;
                                Sym : Symbolizer'Class) is null;

   procedure Pretty_Print_End_File (Pp : in out Pretty_Printer) is abstract;

   procedure Disp_Line_State (Pp       : in out Pretty_Printer'Class;
                              Show_Asm : Boolean);

end Traces_Sources;
