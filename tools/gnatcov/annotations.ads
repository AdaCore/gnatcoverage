------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

with GNAT.Strings; use GNAT.Strings;

with Binary_Files;   use Binary_Files;
with Coverage;
with Diagnostics;    use Diagnostics;
with Disa_Symbolize; use Disa_Symbolize;
with Elf_Disassemblers; use Elf_Disassemblers;
with Files_Table;    use Files_Table;
with SC_Obligations; use SC_Obligations;
with Slocs;          use Slocs;
with Traces;         use Traces;
with Traces_Elf;     use Traces_Elf;
with Traces_Files;   use Traces_Files;
with Traces_Lines;   use Traces_Lines;
with Traces_Stats;   use Traces_Stats;
with Types;          use Types;

package Annotations is

   type Annotation_Format is
     (Annotate_Asm,
      Annotate_Xcov,
      Annotate_Html,
      Annotate_Xcov_Plus,
      Annotate_Html_Plus,
      Annotate_Dynamic_Html,
      Annotate_Xml,
      Annotate_Report,
      Annotate_Unknown);

   function To_Annotation_Format (Option : String) return Annotation_Format;
   --  Convert annotation format option to Annotation_Format value

   Annotation : Annotation_Format := Annotate_Unknown;
   --  The kind of output being generated

private

   function Get_Unique_Filename
     (File      : Source_File_Index;
      Extension : String) return String;
   --  Return an unique filename to hold coverage annotations for File

   function SCO_Text (SCO : SCO_Id; Length : Natural := 8) return String;
   --  Extract the text of SCO from source file, truncating it to the
   --  first source line and the first Length characters. If it has been
   --  truncated, the returned value will end with "...".

   function Message_Annotation (M : Message) return String;
   --  Return a representation of M to be associated with an annotated line

   Global_Stats : Stat_Array := (others => 0);
   --  Stats associated with the whole set of source files that this package
   --  considers (i.e. total numbers of lines, of partially covered /
   --  not covered / fully covered lines...)

   type Pretty_Printer is abstract tagged limited record
      Need_Sources : Boolean;
      Show_Details : Boolean;
      Context      : Coverage.Context_Access;
   end record;

   procedure Pretty_Print_Start
     (Pp : in out Pretty_Printer) is null;
   --  Called once at the beginning of the process

   procedure Pretty_Print_End
     (Pp : in out Pretty_Printer) is null;
   --  Called once at the end of the process

   procedure Pretty_Print_Start_File
     (Pp   : in out Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean) is abstract;
   --  Called at the beginning of a source file display

   procedure Pretty_Print_End_File (Pp : in out Pretty_Printer) is abstract;
   --  Called at the end of a source file display

   procedure Pretty_Print_Start_Line
     (Pp       : in out Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String) is abstract;
   --  Let Pp start the pretty printing of line at Line_Num in current file

   procedure Pretty_Print_End_Line (Pp : in out Pretty_Printer) is null;
   --  Let Pp end the pretty printing of the current line

   procedure Pretty_Print_Start_Instruction_Set
     (Pp    : in out Pretty_Printer;
      State : Any_Line_State) is null;
   --  Let Pp start the pretty printing of a set of instructions, State
   --  being the merged state of all its instructions.

   procedure Pretty_Print_End_Instruction_Set
     (Pp : in out Pretty_Printer) is null;
   --  Let Pp end the pretty printing of a set of instructions

   procedure Pretty_Print_Start_Symbol
     (Pp     : in out Pretty_Printer;
      Name   : String;
      Offset : Pc_Type;
      State  : Line_State) is null;

   procedure Pretty_Print_End_Symbol (Pp : in out Pretty_Printer) is null;

   procedure Pretty_Print_Insn
     (Pp       : in out Pretty_Printer;
      Pc       : Pc_Type;
      State    : Insn_State;
      Insn     : Binary_Content;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class) is null;
   --  Let Pp print the instruction at Pc using Sym as a symbolizer. State
   --  should be the coverage state of this instruction and Insn its binary
   --  content.

   procedure Pretty_Print_Message
     (Pp : in out Pretty_Printer;
      M  : Message) is null;
   --  Let Pp print the message M, attached to the current file:line

   procedure Pretty_Print_Statement
     (Pp    : in out Pretty_Printer;
      SCO   : SCO_Id;
      State : Line_State) is null;
   --  Let Pp print the statement whose id is SCO

   procedure Pretty_Print_Start_Decision
     (Pp    : in out Pretty_Printer;
      SCO   : SCO_Id;
      State : Line_State) is null;
   --  Let Pp start the display of the decision whose id is SCO

   procedure Pretty_Print_End_Decision (Pp : in out Pretty_Printer) is null;
   --  Let Pp close the display of the current decision

   procedure Pretty_Print_Condition
     (Pp    : in out Pretty_Printer;
      SCO   : SCO_Id;
      State : Line_State) is null;
   --  Let Pp print the condition whose id is SCO

   procedure Generate_Report
     (Pp           : in out Pretty_Printer'Class;
      Show_Details : Boolean);
   --  Let Pp generate the annotated sources. If Show_Details is False, only
   --  a line state will be displayed. If Show_Details is True, a justification
   --  is associated to this line state.

   function Aggregated_State (Info : Line_Info) return Any_Line_State;
   --  Return synthetic indication of coverage state for all computed criteria

   function Original_Processing_Context (TF : Trace_File_Type) return String;
   --  If TF was processed in a previous Gnatcov execution whose coverage
   --  information was then reloadad through a checkpoint, provide information
   --  about the original processing context. Empty string for traces processed
   --  in the current Gnatcov execution.

   function Get_Exemption (Sloc : Source_Location) return Source_Location;
   --  If the given sloc is covered by an exemption, return the source location
   --  of the corresponding exemption annotation, else return No_Location.

   function Get_Exemption_Message
     (Sloc : Source_Location) return String_Access;
   --  For a sloc denoting an Exempt_On annotation, return the descriptive
   --  message justifying the exemption.

   function Get_Exemption_Count
     (Sloc : Source_Location) return Natural;
   --  Return the exempted line/message counter for exemption at sloc

   procedure Inc_Exemption_Count (Sloc : Source_Location);
   --  Increment the exempted line/message counter for exemption at sloc

end Annotations;
