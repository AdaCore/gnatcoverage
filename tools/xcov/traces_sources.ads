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
with GNAT.Dynamic_Tables;
with Ada.Containers.Hashed_Maps;
with Ada.Text_IO; use Ada.Text_IO;

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
   --  DO178B level for output state.

   --  Coverage state of a source line of code.
   type Line_State is
     (
      --  No instructions executed.
      Not_Covered,

      --  Some instructions not covered in the line.
      Partially_Covered,

      --  Covered at instructions level but there are branches partially
      --  covered.
      Covered, -- Branch_Partially_Covered

      --  All instructions executed.
      --  Only one branch and one decision taken.
      Branch_Taken,
      Branch_Fallthrough,

      --  Covered at decision level.
      Branch_Covered,

      --  Same as covered but no branches.
      Covered_No_Branch,

      --  Initial state: no code for this line.
      No_Code
      );

   subtype Known_Trace_State is
     Trace_State range Not_Covered .. Trace_State'Last;
   type State_Update_Table_Type is array (Line_State, Known_Trace_State)
     of Line_State;

   --  Table to merge trace_states.
   --  The initial Line_State must be No_Code.
   --  Update the line_state using:
   --    New_State := Update_Table (Old_State, Trace_State);
   Update_Table : constant State_Update_Table_Type;

   --  Characters identifying a Line_State.
   type State_Char_Array is array (Line_State) of Character;
   State_Char : constant State_Char_Array;

   --  Degradation of a state according to the level.
   type State_Map_Array is array (DO178B_Level_Type, Line_State) of Line_State;
   State_Map : constant State_Map_Array;

   type Source_File is private;

   --  Find or create a new source file.
   function Find_File (Filename : String_Acc) return Source_File;

   --  Lets know File that Line exists and add the addresses range for Info.
   --  (This knowledge comes from debugging informations).
   procedure Add_Line (File : Source_File;
                       Line : Natural;
                       Info : Addresses_Info_Acc;
                       Base : Traces_Base_Acc;
                       Exec : Exe_File_Acc);

   --  Same as Add_Line but with a State.
   --  (The knowledge comes from execution traces).
   procedure Add_Line_State (File : Source_File;
                             Line : Natural;
                             State : Traces.Trace_State);

   --  If True, Disp_Line_State will also display info for files that are not
   --  found.
   Flag_Show_Missing : Boolean := False;

   --  Display a per file summary.
   procedure Disp_File_Summary;

   procedure Add_Source_Rebase (Old_Prefix : String;
                                New_Prefix : String);

   procedure Add_Source_Search (Prefix : String);

   procedure Dump_Routines_Traces;

   procedure Dump_Uncovered_Routines (Report : File_Access);
   --  Go through the routine database and dump the list of uncovered
   --  routines into Report.
   --  Should expose an iterator on routines and move this to Traces_xxx???

private
   type Line_Chain;
   type Line_Chain_Acc is access Line_Chain;

   type Line_Chain is record
      Line : Addresses_Info_Acc;
      Base : Traces_Base_Acc;
      Exec : Exe_File_Acc;
      Link : Line_Chain_Acc;
   end record;

   --  Data associated with a SLOC.
   type Line_Info is record
      --  The coverage state.
      State : Line_State;

      --  Object code for this line.
      First_Line, Last_Line : Line_Chain_Acc;
   end record;

   --  Describe a source file - one element per line.
   package Source_Lines_Vectors is new GNAT.Dynamic_Tables
     (Table_Component_Type => Line_Info,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 16,
      Table_Increment => 100);

   subtype Source_Lines is Source_Lines_Vectors.Instance;

   --  Containers helper.
   function Equal (L, R : Source_Lines) return Boolean;

   --  Describe all the source files.
   package Filenames_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => String_Acc,
      Element_Type => Source_Lines,
      Hash => Hash,
      Equivalent_Keys => Equal,
      "=" => Equal);

   type Source_File is record
      Cur : Filenames_Maps.Cursor;
   end record;

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

   type Pourcentage is record
      Fully : Natural;
      Partial : Natural;
      Total : Natural;
   end record;

   function Get_Pourcentage (Stats : Stat_Array) return Pourcentage;

   type Pretty_Printer is abstract tagged limited record
      Need_Sources : Boolean;
      Global_Stats : Stat_Array;
      Show_Asm     : Boolean;
   end record;

   --  Called once at the beginning of the process.
   procedure Pretty_Print_Start (Pp : in out Pretty_Printer) is null;

   --  Called once at the end of the process.
   procedure Pretty_Print_Finish (Pp : in out Pretty_Printer) is null;

   --  Called at the beginning of a source file display.
   procedure Pretty_Print_File (Pp : in out Pretty_Printer;
                                Source_Filename : String;
                                Stats : Stat_Array;
                                Has_Source : Boolean;
                                Skip : out Boolean) is abstract;
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
