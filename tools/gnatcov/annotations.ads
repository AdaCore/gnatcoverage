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

with Ada.Text_IO;

with GNAT.Strings; use GNAT.Strings;

with Binary_Files;          use Binary_Files;
with Coverage;
with Coverage_Options;      use Coverage_Options;
with Diagnostics;           use Diagnostics;
with Disa_Symbolize;        use Disa_Symbolize;
with Elf_Disassemblers;     use Elf_Disassemblers;
with Files_Table;           use Files_Table;
with SC_Obligations;        use SC_Obligations;
with Strings;               use Strings;
with Slocs;                 use Slocs;
with Traces;                use Traces;
with Traces_Elf;            use Traces_Elf;
with Traces_Files_Registry; use Traces_Files_Registry;
with Traces_Lines;          use Traces_Lines;
with Traces_Stats;          use Traces_Stats;
with Types;                 use Types;

package Annotations is

   use all type Unbounded_String;

   type Annotation_Format is
     (Annotate_Asm,
      Annotate_Xcov,
      Annotate_Static_Html,
      Annotate_Xcov_Plus,
      Annotate_Static_Html_Plus,
      Annotate_Html,
      Annotate_Cobertura,
      Annotate_Xml,
      Annotate_Report,
      Annotate_Sarif,
      Annotate_Unknown);
   subtype Annotation_Format_Family is Annotation_Format
   with Static_Predicate =>
     Annotation_Format_Family in
       Annotate_Asm
       | Annotate_Xcov
       | Annotate_Static_Html
       | Annotate_Html
       | Annotate_Cobertura
       | Annotate_Xml
       | Annotate_Report
       | Annotate_Sarif;

   function To_Annotation_Format (Option : String) return Annotation_Format;
   --  Convert annotation format option to Annotation_Format value

   type Annotation_Formats_Arr is array (Annotation_Format) of Boolean;

   Annotation : Annotation_Formats_Arr :=
     (Annotate_Unknown => True, others => False);
   --  For each kind of output, a True value indicates that the corresponding
   --  report annotation should be generated. During an execution,
   --  Annotation (Annotate_Unknown) should be True only in the following
   --  situations:
   --  - We haven't seen any --annotate option on the command line,
   --  - We are parsing the --annotate options and one of the values for the
   --    option does not correspond to any annotation format. In that case
   --    the execution will terminate before finishing to parse the command
   --    line options.

   Multiple_Reports : Boolean := False;
   --  True if more than one report annotation format which requires writing
   --  the report to the output directory is requested.

   type Report_Section is
   range Coverage_Level'Pos (Coverage_Level'First)
     .. Coverage_Level'Pos (Coverage_Level'Last) + 3;
   --  For report and SARIF formats. There is one report section for each
   --  coverage level, plus the following three special sections:

   Coverage_Exclusions   : constant Report_Section := Report_Section'Last - 2;
   Undet_Coverage        : constant Report_Section := Report_Section'Last - 1;
   Other_Errors          : constant Report_Section := Report_Section'Last;

   function Section_Of_Message (M : Message) return Report_Section;
   --  Indicate the coverage criterion a given SCO/message pertains to (by its
   --  'Pos), or Other_Errors if SCO has no related section/M is not a
   --  violation message.
   function Section_Of_SCO (SCO : SCO_Id) return Report_Section;

private

   function SCO_Text
     (SCO    : SCO_Id;
      Length : Natural := 9;
      UTF8   : Boolean := False) return String;
   --  Extract the text of SCO from source file, truncating it to the first
   --  source line and the first Length characters. If it has been truncated,
   --  the returned value will end with "...".
   --
   --  If UTF8, the source file text is decoded (see Files_Table.Get_Line) and
   --  the result is valid UTF-8.

   function SCO_Annotations (SCO : SCO_Id) return String_Vectors.Vector;
   --  Return annotations for the SCO. For instance, this will return a list
   --  of strings showing the expansion stack if the SCO comes from a macro
   --  expansion.

   procedure Output_Annotations
     (Output      : Ada.Text_IO.File_Type;
      Annotations : String_Vectors.Vector);
   --  Print annotations

   function SCO_Image (SCO : SCO_Id; Length : Natural := 9) return String;
   --  Return a string representation of the annotated SCO

   function Message_Annotation (M : Message) return String;
   --  Return a representation of M to be associated with an annotated line

   Global_Stats    : Li_Stat_Array := (others => 0);
   Global_Ob_Stats : Ob_Stat_Array;
   --  Stats associated with the whole set of source files that this package
   --  considers (i.e. total numbers of lines / obligations, of partially
   --  covered / not covered / fully covered lines...)

   type Pretty_Printer is abstract tagged limited record
      Need_Sources : Boolean;
      Show_Details : Boolean;

      Use_UTF8 : Boolean := False;
      --  Whether source excerpts need to be converted to UTF-8 for this format

      Context : Coverage.Context_Access;
   end record;

   function Format (Pp : Pretty_Printer) return Annotation_Format_Family
   is abstract;
   --  Return the family of annotation formats that Pp generates

   function SCO_Text
     (Pp : Pretty_Printer; SCO : SCO_Id; Length : Natural := 9) return String
   is (SCO_Text (SCO, Length, UTF8 => Pp.Use_UTF8));

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
   --  Let Pp print the statement SCO whose id is SCO

   procedure Pretty_Print_Fun
     (Pp    : in out Pretty_Printer;
      SCO   : SCO_Id;
      State : Line_State);
   --  Let Pp print the function SCO whose id is SCO

   procedure Pretty_Print_Call
     (Pp    : in out Pretty_Printer;
      SCO   : SCO_Id;
      State : Line_State);
   --  Let Pp print the call SCO whose id is SCO

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

   procedure Pretty_Print_Scope_Entities
     (Pp             : in out Pretty_Printer;
      File           : Source_File_Index;
      Scope_Entities : Scope_Entities_Tree) is null;
   --  Let Pp print the given body entity

   No_Cleaning : constant String := "DO NOT CLEAN";
   --  Constant used to indicate that we should not attempt to clean the output
   --  dir prior to emitting the report. This is used instead of the empty
   --  string as the empty string pattern can be used as a cleaning pattern
   --  that will match every file in the report directory. We should probably
   --  never need to clean a report dir with the above string as a pattern.

   procedure Generate_Report
     (Pp               : in out Pretty_Printer'Class;
      Show_Details     : Boolean;
      Subdir           : String := "";
      Clean_Pattern    : String := No_Cleaning);
   --  Let Pp generate the annotated sources. If Show_Details is False, only a
   --  line state will be displayed. If Show_Details is True, a justification
   --  is associated to this line state. Subp_Of_Interest contains the list
   --  of subprograms of interest, empty if no subprogram of interest was
   --  specified. If Subdir is not empty, this will be subdirectory in which
   --  the report will be created if multiple reports are requested. If
   --  Clean_Pattern is not No_Cleaning, all the files matching Clean_Pattern
   --  in Output_Dir as well as in Output_Dir/Subdir will be removed.

   function Aggregated_State
     (Info              : Line_Info;
      Ignore_Exemptions : Boolean := False) return Any_Line_State;
   --  Return synthetic indication of coverage state for all computed criteria.
   --  If Ignore_Exemptions is True, any exemption information attached to the
   --  line is ignored during the state aggregation.
   --
   --  We'll consider Exempted_With_Violation to be more important than
   --  Exempted_With_Undetermined_Cov in case there are exemptions of both
   --  kinds.

   function Original_Processing_Context
     (TF : Trace_File_Element) return String;
   --  If TF was processed in a previous Gnatcov execution whose coverage
   --  information was then reloadad through a checkpoint, provide information
   --  about the original processing context. Empty string for traces processed
   --  in the current Gnatcov execution.

   function Get_Exemption_Message
     (Sloc : Source_Location) return String_Access;
   --  For a Sloc denoting an Exempt_On annotation, return the descriptive
   --  message justifying the exemption.

   function Get_Exemption_Violation_Count
     (Sloc : Source_Location) return Natural;
   --  Return the exempted line/message violation counter for exemption at Sloc

   function Get_Exemption_Undet_Cov_Count
     (Sloc : Source_Location) return Natural;
   --  Return the exempted line/message undetermined coverage items counter for
   --  exemption at Sloc.

   procedure Output_Multiline_Msg
     (Output : Ada.Text_IO.File_Type;
      Text   : String);
   --  Output the given Text on Output, replacing all
   --  Ada.Characters.Latin_1.LF by a call to Ada.Text_IO.Newline.

   function Line_Metrics
     (FI       : File_Info_Access;
      From, To : Natural) return Li_Stat_Array;
   --  Return line metrics for the given line range

   function Obligation_Metrics (From, To : SCO_Id) return Ob_Stat_Array;
   --  Return obligation metrics for the given SCO range

   function SCO_Kind_Image (SCO : SCO_Id) return String;
   --  Get the string representation of the SCO_Kind of SCO. A special
   --  treatment is needed for assertions. There is no assertion SCO_Kind but
   --  some decision SCOs correspond to assertion decisions. The difference
   --  needs to be made clear in the reports by not calling them the same.

end Annotations;
