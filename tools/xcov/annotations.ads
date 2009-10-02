------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

with Disa_Symbolize; use Disa_Symbolize;
with Traces; use Traces;
with Traces_Lines;   use Traces_Lines;
with Traces_Stats;   use Traces_Stats;
with Traces_Elf;    use Traces_Elf;
with Traces_Dbase; use Traces_Dbase;
with Files_Table; use Files_Table;

package Annotations is

   type Annotation_Format is
     (Annotate_Asm,
      Annotate_Xcov,
      Annotate_Html,
      Annotate_Xcov_Asm,
      Annotate_Html_Asm,
      Annotate_Xml,
      Annotate_Report,
      Annotate_Unknown);

   function To_Annotation_Format (Option : String) return Annotation_Format;
   --  Convert annotation format option to Annotation_Format value

   Flag_Show_Missing : Boolean := False;
   --  If True, Disp_Line_State displays info for files that are not found
   --  Why isn't this a parameter of Disp_Line_State???

   procedure Disp_File_Summary;
   --  Display per-file summary

private
   Global_Stats : Stat_Array := (others => 0);
   --  Stats associated with the whole set of source files that this package
   --  considers (i.e. total numbers of lines, of partially covered /
   --  not covered / fully covered lines...)

   type Pretty_Printer is abstract tagged limited record
      Need_Sources : Boolean;
      Show_Asm     : Boolean;
   end record;

   procedure Pretty_Print_Start
     (Pp : in out Pretty_Printer) is null;
   --  Called once at the beginning of the process

   procedure Pretty_Print_End
     (Pp : in out Pretty_Printer) is null;
   --  Called once at the end of the process

   procedure Pretty_Print_Start_File
     (Pp              : in out Pretty_Printer;
      Source_Filename : String;
      Stats           : Stat_Array;
      Has_Source      : Boolean;
      Skip            : out Boolean) is abstract;
   --  Called at the beginning of a source file display

   procedure Pretty_Print_End_File (Pp : in out Pretty_Printer) is abstract;

   --  Subprograms below need comments???

   procedure Pretty_Print_Start_Line
     (Pp       : in out Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String) is abstract;

   procedure Pretty_Print_End_Line (Pp : in out Pretty_Printer) is null;

   procedure Pretty_Print_Start_Instruction_Set
     (Pp    : in out Pretty_Printer;
      State : Line_State) is null;

   procedure Pretty_Print_End_Instruction_Set
     (Pp : in out Pretty_Printer) is null;

   procedure Pretty_Print_Start_Symbol
     (Pp     : in out Pretty_Printer;
      Name   : String;
      Offset : Pc_Type;
      State  : Line_State) is null;

   procedure Pretty_Print_End_Symbol (Pp : in out Pretty_Printer) is null;

   procedure Pretty_Print_Insn
     (Pp    : in out Pretty_Printer;
      Pc    : Pc_Type;
      State : Insn_State;
      Insn  : Binary_Content;
      Sym   : Symbolizer'Class) is null;

   procedure Generate_Report
     (Pp       : in out Pretty_Printer'Class;
      Show_Asm : Boolean);

end Annotations;
