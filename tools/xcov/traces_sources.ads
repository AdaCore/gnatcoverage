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

with Traces;         use Traces;
with Traces_Elf;     use Traces_Elf;
with Traces_Dbase;   use Traces_Dbase;
with Disa_Symbolize; use Disa_Symbolize;
with Traces_Stats;   use Traces_Stats;
with Traces_Lines;   use Traces_Lines;
with Types;          use Types;

package Traces_Sources is

   procedure New_Source_File (File : Source_File_Index);
   --  Initialize entry for File in source files table

   procedure Add_Line
     (File : Source_File_Index;
      Line : Natural;
      Info : Addresses_Info_Acc;
      Base : Traces_Base_Acc;
      Exec : Exe_File_Acc);
   --  Lets know File that Line exists and add the addresses range for Info.
   --  (This knowledge comes from debugging informations).

   procedure Add_Line_State
     (File  : Source_File_Index;
      Line  : Natural;
      State : Traces.Trace_State);
   --  Same as Add_Line but with a State.
   --  (The knowledge comes from execution traces).

   Flag_Show_Missing : Boolean := False;
   --  If True, Disp_Line_State displays info for files that are not found
   --  Why isn't this a parameter of Disp_Line_State???

   procedure Disp_File_Summary;
   --  Display per-file summary

   procedure Add_Source_Rebase (Old_Prefix : String; New_Prefix : String);
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

   Global_Stats : Stat_Array := (others => 0);
   --  Stats associated to the whole set of source files that this package
   --  considers (i.e. total numbers of lines, of partially covered /
   --  not covered / fully covered lines...)

   type Pretty_Printer is abstract tagged limited record
      Need_Sources : Boolean;
      Show_Asm     : Boolean;
   end record;

   procedure Pretty_Print_Start
     (Pp : in out Pretty_Printer) is null;
   --  Called once at the beginning of the process

   procedure Pretty_Print_Finish
     (Pp : in out Pretty_Printer) is null;
   --  Called once at the end of the process

   procedure Pretty_Print_File
     (Pp              : in out Pretty_Printer;
      Source_Filename : String;
      Stats           : Stat_Array;
      Has_Source      : Boolean;
      Skip            : out Boolean) is abstract;
   --  Called at the beginning of a source file display

   --  Subprograms below need comments???

   procedure Pretty_Print_Line
     (Pp       : in out Pretty_Printer;
      Line_Num : Natural;
      State    : Line_State;
      Line     : String) is abstract;

   procedure Pretty_Print_Label
     (Pp    : in out Pretty_Printer;
      Label : String) is null;

   procedure Pretty_Print_Insn
     (Pp    : in out Pretty_Printer;
      Pc    : Pc_Type;
      State : Trace_State;
      Insn  : Binary_Content;
      Sym   : Symbolizer'Class) is null;

   procedure Pretty_Print_End_File (Pp : in out Pretty_Printer) is abstract;

   procedure Disp_Line_State
     (Pp       : in out Pretty_Printer'Class;
      Show_Asm : Boolean);

end Traces_Sources;
