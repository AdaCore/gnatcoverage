------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2022, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with Interfaces;

with ALI_Files;        use ALI_Files;
with Calendar_Utils;
with Coverage;
with Coverage_Options; use Coverage_Options;
with Coverage.Object;
with Coverage.Source;  use Coverage.Source;
with Coverage.Tags;
with Instrument;
with Outputs;          use Outputs;
with Subprocesses;
with Switches;         use Switches;
with Traces_Disa;

package body Annotations is

   package US renames Ada.Strings.Unbounded;

   -------------------------
   -- Get_Unique_Filename --
   -------------------------

   function Get_Unique_Filename
     (File      : Source_File_Index;
      Extension : String) return String
   is
      --  Just take the unique name and turn path separators into dashes.
      --  It should be enough...

      Result : String := Get_Unique_Name (File);
   begin
      for C of Result loop
         if C in '/' | '\' then
            C := '-';
         end if;
      end loop;
      return Result & '.' & Extension;
   end Get_Unique_Filename;

   procedure Disp_File_Line_State
     (Pp         : in out Pretty_Printer'Class;
      File_Index : Source_File_Index;
      FI         : File_Info_Access);
   --  Go through file and and let Pp annotate its lines with coverage
   --  information

   procedure Disp_Messages
     (Pp : in out Pretty_Printer'Class;
      LI : Line_Info);
   --  Go through LI's messages and let Pp display them

   procedure Disp_Instruction_Sets
     (Pp : in out Pretty_Printer'Class;
      LI : Line_Info);
   --  In object coverage, go through instructions at line LI
   --  and let Pp display them

   procedure Disp_SCOs
     (Pp   : in out Pretty_Printer'Class;
      Line : Natural;
      LI   : Line_Info);
   --  In source coverage, go through source coverage information at line LI
   --  and let Pp display them

   ----------------------
   -- Aggregated_State --
   ----------------------

   function Aggregated_State
     (Info              : Line_Info;
      Ignore_Exemptions : Boolean := False) return Any_Line_State is
      Result : Line_State := No_Code;
   begin
      --  Exempted case

      if Info.Exemption /= Slocs.No_Location
        and then not Ignore_Exemptions
      then
         if Get_Exemption_Violation_Count (Info.Exemption) = 0
            and then Get_Exemption_Undet_Cov_Count (Info.Exemption) = 0
         then
            return Exempted_No_Violation;
         elsif Get_Exemption_Violation_Count (Info.Exemption) = 0 then
            return Exempted_With_Undetermined_Cov;
         else
            return Exempted_With_Violation;
         end if;
      end if;

      --  Non-exempted case

      for J in Info.State'Range loop
         Result := Result * Info.State (J);
      end loop;
      return Result;
   end Aggregated_State;

   --------------------------
   -- Disp_File_Line_State --
   --------------------------

   procedure Disp_File_Line_State
     (Pp         : in out Pretty_Printer'Class;
      File_Index : Source_File_Index;
      FI         : File_Info_Access)
   is
      procedure Process_One_Line (Index : Positive);
      --  Let Pp annotate the line at Index in File, including all the
      --  information attached to this line in the file table, if relevant
      --  (e.g. annotate the assembly code attached to this line in object
      --  coverage; or report the lack of evidence of the independant
      --  influence of a condition located on this line, in MCDC).

      ----------------------
      -- Process_One_Line --
      ----------------------

      procedure Process_One_Line (Index : Positive) is
         LI : constant Line_Info_Access := Get_Line (FI, Index);
      begin
         Pretty_Print_Start_Line (Pp, Index, LI, Get_Line (FI, Index));

         if Pp.Show_Details then
            Disp_Instruction_Sets (Pp, LI.all);
            Disp_SCOs (Pp, Index, LI.all);
            Disp_Messages (Pp, LI.all);
         end if;

         Pretty_Print_End_Line (Pp);
      end Process_One_Line;

      Skip : Boolean;

   --  Start of processing for Disp_File_Line_State

   begin
      Files_Table.Fill_Line_Cache (FI);

      if not FI.Has_Source and then Pp.Need_Sources then
         Warn_File_Missing (FI.all);
      end if;

      --  If there is no coverage information to display in the annotated
      --  sources (i.e. if the total number of line with a Line_State is null),
      --  then there is no useful information to add (case in particular of
      --  sources with SCOs but no associated code, e.g. generics that are
      --  not instantiated).

      if Get_Total (FI.Li_Stats) = 0 and then not FI.Has_Source then
         return;
      end if;

      Pretty_Print_Start_File (Pp, File_Index, Skip);

      if Skip then
         return;
      end if;

      Pretty_Print_Scope_Entities
        (Pp, File_Index, Get_Scope_Entities (Comp_Unit (File_Index)));
      Iterate_On_Lines (FI, Process_One_Line'Access);
      Pretty_Print_End_File (Pp);
   end Disp_File_Line_State;

   ---------------------------
   -- Disp_Instruction_Sets --
   ---------------------------

   procedure Disp_Instruction_Sets
     (Pp : in out Pretty_Printer'Class;
      LI : Line_Info)
   is
      use Traces_Disa;

      procedure Pretty_Print_Insn
        (Addr     : Pc_Type;
         State    : Insn_State;
         Insn     : Binary_Content;
         Insn_Set : Insn_Set_Type;
         Sym      : Symbolizer'Class);
      --  Call Pp.Pretty_Print_Insn with the corresponding parameters; this
      --  procedure is meant to be used as a callback in an iterator over
      --  assembly lines (Traces_Disa.Disp_Assembly_Lines).

      -----------------------
      -- Pretty_Print_Insn --
      -----------------------

      procedure Pretty_Print_Insn
        (Addr     : Pc_Type;
         State    : Insn_State;
         Insn     : Binary_Content;
         Insn_Set : Insn_Set_Type;
         Sym      : Symbolizer'Class) is
      begin
         Pretty_Print_Insn (Pp, Addr, State, Insn, Insn_Set, Sym);
      end Pretty_Print_Insn;

      --  Local variables

      Instruction_Set : Address_Info_Acc;
      Sec_Info        : Address_Info_Acc;
      Ls              : constant Any_Line_State := Aggregated_State (LI);
      In_Symbol       : Boolean;

   --  Start of processing for Disp_Instruction_Sets

   begin
      if not Coverage.Object_Coverage_Enabled
            or else
         LI.Obj_Infos = null
      then
         return;
      end if;

      Pretty_Print_Start_Instruction_Set (Pp, Ls);

      --  Iterate over each insn block for the source line

      for Info of LI.Obj_Infos.all loop
         Instruction_Set := Info.Instruction_Set;
         declare
            use Interfaces;

            Label  : constant String :=
              Get_Label (Info.Exec.all, Instruction_Set);
            Symbol : constant Address_Info_Acc :=
              Get_Symbol (Info.Exec.all, Instruction_Set.First);
         begin
            if Label'Length > 0 and then Symbol /= null then
               In_Symbol := True;
               Pretty_Print_Start_Symbol
                 (Pp,
                  Symbol.Symbol_Name.all,
                  Instruction_Set.First - Symbol.First,
                  Info.State);
            else
               In_Symbol := False;
            end if;
         end;

         Sec_Info := Instruction_Set.Parent;

         while Sec_Info /= null
           and then Sec_Info.Kind /= Section_Addresses
         loop
            Sec_Info := Sec_Info.Parent;
         end loop;

         Disp_Assembly_Lines
           (Slice (Sec_Info.Section_Content,
                   Instruction_Set.First,
                   Instruction_Set.Last),
            Get_Insn_Set_Ranges (Info.Exec.all, Sec_Info.Section_Sec_Idx).all,
            Info.Base.all, Pretty_Print_Insn'Access, Info.Exec.all);

         if In_Symbol then
            Pretty_Print_End_Symbol (Pp);
         end if;
      end loop;

      Pretty_Print_End_Instruction_Set (Pp);
   end Disp_Instruction_Sets;

   -------------------
   -- Disp_Messages --
   -------------------

   procedure Disp_Messages
     (Pp : in out Pretty_Printer'Class;
      LI : Line_Info)
   is
   begin
      if LI.Messages /= null then
         for Message of LI.Messages.all loop
            Pretty_Print_Message (Pp, Message);
         end loop;
      end if;
   end Disp_Messages;

   ---------------
   -- Disp_SCOs --
   ---------------

   procedure Disp_SCOs
     (Pp   : in out Pretty_Printer'Class;
      Line : Natural;
      LI   : Line_Info)
   is

      SCO_State : Line_State;
   begin
      if not Coverage.Source_Coverage_Enabled or else LI.SCOs = null then
         return;
      end if;

      for SCO of LI.SCOs.all loop
         --  Process a given SCO exactly once; to do so, only process a
         --  SCO when its first sloc is at the current line. Otherwise, it
         --  should have been processed earlier.

         if Kind (SCO) /= Removed and then First_Sloc (SCO).L.Line = Line then
            case SCO_Kind (Kind (SCO)) is
               when Statement =>
                  if Coverage.Enabled (Stmt) then
                     SCO_State := Get_Line_State (SCO, Stmt);
                     Pretty_Print_Statement (Pp, SCO, SCO_State);
                  end if;

               when Decision =>
                  if Coverage.Enabled (Decision)
                    or else Coverage.MCDC_Coverage_Enabled
                  then
                     if Coverage.MCDC_Coverage_Enabled then
                        SCO_State := Get_Line_State (SCO, Coverage.MCDC_Level);

                     elsif Coverage.Enabled (Decision) then
                        SCO_State := Get_Line_State (SCO, Decision);
                     end if;

                     Pretty_Print_Start_Decision (Pp, SCO, SCO_State);

                     if Coverage.MCDC_Coverage_Enabled then
                        for J in Condition_Index'First .. Last_Cond_Index (SCO)
                        loop
                           Pretty_Print_Condition
                             (Pp,
                              Condition (SCO, J),
                              Get_Line_State (Condition (SCO, J),
                                              Coverage.MCDC_Level));
                        end loop;
                     end if;

                     Pretty_Print_End_Decision (Pp);
                  end if;

               when Condition =>
                  --  Condition without a parent decision. This should never
                  --  happen; fatal error.

                  Fatal_Error ("no decision attached to " & Image (SCO));

               when Operator =>
                  null;

            end case;
         end if;
      end loop;
   end Disp_SCOs;

   ---------------------
   -- Generate_Report --
   ---------------------

   procedure Generate_Report
     (Pp               : in out Pretty_Printer'Class;
      Show_Details     : Boolean;
      Subp_Of_Interest : Scope_Id_Set;
      Subdir           : String := "";
      Clean_Pattern    : String := No_Cleaning)
   is
      use Coverage;

      procedure Compute_File_State (File_Index : Source_File_Index);
      --  For all lines in file at Index, compute line state and
      --  update file table accordingly. Compute FI's coverage stats
      --  in the process and update the global stats with FI's
      --  information.

      procedure Process_One_File (File_Index : Source_File_Index);
      --  Process file at index and let Pp annotate it if it has some
      --  coverage info to display. Update FI's coverage stats with
      --  this line information.

      ------------------------
      -- Compute_File_State --
      ------------------------

      procedure Compute_File_State (File_Index : Source_File_Index)
      is
         ST : Scope_Traversal_Type;
         FI : constant File_Info_Access :=
           Get_File (File_Index);

         procedure Compute_Line_State (L : Positive);
         --  Given a source line located in FI's source file, at line L,
         --  compute its line state and record it into the file table.

         ------------------------
         -- Compute_Line_State --
         ------------------------

         procedure Compute_Line_State (L : Positive) is
            LI : constant Line_Info_Access := Get_Line (FI, L);
            S  : Line_State;
         begin
            --  Check that this is a SCO for a subprogram of interest

            if not Subp_Of_Interest.Is_Empty and then LI.SCOs /= null then
               for SCO of LI.SCOs.all loop
                  Traverse_SCO (ST, SCO);
               end loop;
               if not In_Scope_Of_Interest (ST) then
                  return;
               end if;
            end if;

            --  Compute state for each coverage objective

            if Object_Coverage_Enabled then
               Object.Compute_Line_State (LI);
            else
               Source.Compute_Line_State (L, LI);
            end if;

            --  Compute aggregated line state before exemption

            S := Aggregated_State (LI.all, Ignore_Exemptions => True);

            --  If exempted, bump exemption hit counter if generating annotated
            --  sources (or HTML). Note that for the Report case, we count
            --  exempted messages, not lines, and we manage the counter
            --  specifically in Annotation.Report.

            if LI.Exemption /= Slocs.No_Location
              and then not Annotation (Annotate_Report)
            then
               if S in Not_Covered .. Partially_Covered then
                  Inc_Violation_Exemption_Count (LI.Exemption);
               elsif S = Undetermined_Coverage then
                  Inc_Undet_Cov_Exemption_Count (LI.Exemption);
               end if;
            end if;
         end Compute_Line_State;

      --  Start of processing for Compute_File_State
      begin
         if FI.Kind /= Source_File or else FI.Ignore_Status = Always then
            return;
         end if;

         Populate_Exemptions (File_Index);
         ST := Scope_Traversal (Comp_Unit (File_Index));
         Iterate_On_Lines (FI, Compute_Line_State'Access);

         --  Update file statistics for line L. Note that this can be done
         --  only once Compute_Line_State for each line has been computed,
         --  because this depends on violation count for each exempted region.

         FI.Li_Stats := Line_Metrics (FI, 1, Last_Line (FI));
         FI.Ob_Stats :=
           Obligation_Metrics
             (First_SCO (Comp_Unit (File_Index)),
              Last_SCO (Comp_Unit (File_Index)));

         for J in Global_Stats'Range loop
            Global_Stats (J) := Global_Stats (J) + FI.Li_Stats (J);
            for Level in Global_Ob_Stats'Range loop
               Global_Ob_Stats (Level).Stats (J) :=
                 Global_Ob_Stats (Level).Stats (J)
                 + FI.Ob_Stats (Level).Stats (J);
               Global_Ob_Stats (Level).Total :=
                 Global_Ob_Stats (Level).Total + FI.Ob_Stats (Level).Total;
            end loop;
         end loop;
      end Compute_File_State;

      ----------------------
      -- Process_One_File --
      ----------------------

      procedure Process_One_File (File_Index : Source_File_Index) is
         FI : constant File_Info_Access := Get_File (File_Index);
      begin
         if FI.Kind = Source_File and then To_Display (FI) then
            Disp_File_Line_State (Pp, File_Index, FI);
         end if;
      end Process_One_File;

      Previous_Output_Dir : constant String := Get_Output_Dir;
      --  Current output dir, needed to restore it after we change to the
      --  requested subdirectory for this report.

   --  Start of processing for Generate_Report

   begin

      if Clean_Pattern /= No_Cleaning then
         Clean_Dir (Get_Output_Dir, Clean_Pattern);
      end if;

      --  Clean the output subdir in all cases, and then switch to that subdir
      --  to generate the report, if needed.

      if Subdir'Length > 0 then
         declare
            Output_Subdir : constant String :=
              Previous_Output_Dir & GNAT.OS_Lib.Directory_Separator & Subdir;
         begin
            if Clean_Pattern /= No_Cleaning then
               Clean_Dir (Output_Subdir, Clean_Pattern);
            end if;
            if Multiple_Reports then
               Set_Output_Dir (Output_Subdir);
            end if;
         end;
      end if;

      Pp.Show_Details := Show_Details;

      --  Compute lines state, files and global statistics

      Global_Stats := (others => 0);
      Files_Table_Iterate (Compute_File_State'Access);

      --  Print

      Pretty_Print_Start (Pp);
      Files_Table_Iterate (Process_One_File'Access);
      Pretty_Print_End (Pp);

      --  Restore the output dir if we modified it

      if Subdir'Length > 0 and then Multiple_Reports then
         Set_Output_Dir (Previous_Output_Dir);
      end if;

   end Generate_Report;

   -------------------
   -- Get_Exemption --
   -------------------

   function Get_Exemption (Sloc : Source_Location) return Source_Location is
      use ALI_Annotation_Maps;

      Cur : constant Cursor := ALI_Annotations.Floor (Sloc);
   begin
      if not Ignore_Exemptions_Trace.Is_Active
        and then Cur /= No_Element
        and then Key (Cur).Source_File = Sloc.Source_File
      then
         declare
            A : constant ALI_Annotation := Element (Cur);
         begin
            if A.Kind = Exempt_On then
               return Key (Cur);
            end if;
         end;
      end if;

      return Slocs.No_Location;
   end Get_Exemption;

   -----------------------------------
   -- Get_Exemption_Violation_Count --
   -----------------------------------

   function Get_Exemption_Violation_Count
     (Sloc : Source_Location) return Natural
   is
   begin
      return ALI_Annotations.Element (Sloc).Violation_Count;
   end Get_Exemption_Violation_Count;

   -----------------------------------
   -- Get_Exemption_Undet_Cov_Count --
   -----------------------------------

   function Get_Exemption_Undet_Cov_Count
     (Sloc : Source_Location) return Natural
   is
   begin
      return ALI_Annotations.Element (Sloc).Undetermined_Cov_Count;
   end Get_Exemption_Undet_Cov_Count;

   ---------------------------
   -- Get_Exemption_Message --
   ---------------------------

   function Get_Exemption_Message
     (Sloc : Source_Location) return String_Access
   is
   begin
      return ALI_Annotations.Element (Sloc).Message;
   end Get_Exemption_Message;

   -----------------------------------
   -- Inc_Violation_Exemption_Count --
   -----------------------------------

   procedure Inc_Violation_Exemption_Count (Sloc : Source_Location) is
      use ALI_Annotation_Maps;

      Cur : constant Cursor := ALI_Annotations.Find (Sloc);
      E   : ALI_Annotation renames ALI_Annotations.Reference (Cur);
   begin
      E.Violation_Count := E.Violation_Count + 1;
   end Inc_Violation_Exemption_Count;

   -----------------------------------
   -- Inc_Undet_Cov_Exemption_Count --
   -----------------------------------

   procedure Inc_Undet_Cov_Exemption_Count (Sloc : Source_Location) is
      use ALI_Annotation_Maps;

      Cur : constant Cursor := ALI_Annotations.Find (Sloc);
      E   : ALI_Annotation renames ALI_Annotations.Reference (Cur);
   begin
      E.Undetermined_Cov_Count := E.Undetermined_Cov_Count + 1;
   end Inc_Undet_Cov_Exemption_Count;

   ------------------------
   -- Message_Annotation --
   ------------------------

   function Message_Annotation (M : Message) return String is
      use Coverage, Coverage.Tags;
      use Ada.Strings.Unbounded;
   begin
      if M.SCO /= No_SCO_Id then
         return SCO_Kind_Image (M.SCO)
           & (if (Switches.Show_MCDC_Vectors
              or else Switches.Show_Condition_Vectors)
              and then Kind (M.SCO) = Condition
              then Index (M.SCO)'Image & " ("
              else " ")
           & SCO_Image (M.SCO)
           & " at "
           & Img (First_Sloc (M.SCO).L.Line) & ":"
           & Img (First_Sloc (M.SCO).L.Column)
           & (if (Switches.Show_MCDC_Vectors
              or else Switches.Show_Condition_Vectors)
              and then Kind (M.SCO) = Condition
              then ")"
              else "")
           & (if M.Tag = No_SC_Tag
              then ""
              else " (from " & Tag_Provider.Tag_Name (M.Tag) & ")")
           & " " & To_String (M.Msg);
      else
         return Image (M.Sloc, Unique_Name => True) & ": " & To_String (M.Msg);
      end if;
   end Message_Annotation;

   ---------------------------------
   -- Original_Processing_Context --
   ---------------------------------

   function Original_Processing_Context (TF : Trace_File_Element) return String
   is
      use Calendar_Utils;
      use Ada.Strings.Unbounded;
   begin
      if Length (TF.Context) = 0 then
         return "";

      else
         declare
            Orig_Context : constant Coverage.Context :=
              Coverage.From_String (To_String (TF.Context));
         begin
            return To_String (Orig_Context.Command)
              & " @ " & Image (Orig_Context.Timestamp);
         end;
      end if;
   end Original_Processing_Context;

   --------------
   -- SCO_Text --
   --------------

   function SCO_Text (SCO : SCO_Id; Length : Natural := 8) return String is

      Sloc_Start : Source_Location := First_Sloc (SCO);
      Sloc_End   : Source_Location := End_Lex_Element (Last_Sloc (SCO));
      Sloc_Bound : Source_Location;
      Line       : US.Unbounded_String;
      Col_Start  : Natural;
      Col_End    : Natural;

      Desc : US.Unbounded_String;
      --  SCO description: shortened view of the SCO tokens, with a macro
      --  expansion annotation if the SCO comes from a macro expansion.

   begin
      --  First, if this is a SCO inside a macro expansion, get the
      --  preprocessed excerpt for reporting purposes.

      if Has_PP_Info (SCO) and then Get_PP_Info (SCO).Kind = In_Expansion then
         declare
            SFI : constant Source_File_Index := Sloc_Start.Source_File;

            Info                   : constant PP_Info := Get_PP_Info (SCO);
            Preprocessed_Filename  : constant String :=
              Get_PP_Filename (SFI) & ".prepro";
            Postprocessed_Filename : constant String := Get_PP_Filename (SFI);
            Postprocessed_SFI      : Source_File_Index :=
              Get_Index_From_Generic_Name
                (Postprocessed_Filename,
                 Source_File, Insert => False);

         begin
            if Postprocessed_SFI = No_Source_File then
               declare
                  use Instrument;
                  use Subprocesses;
                  Preprocessed : Boolean;
               begin
                  if PP_Cmds.Contains (SFI) then
                     Preprocessed :=
                       Run_Command
                         (PP_Cmds.Element (SFI), "Preprocessing",
                          Preprocessed_Filename,
                          Err_To_Out   => False,
                          Ignore_Error => True);

                     if Preprocessed then
                        --  As a reminder, we compute source locations from
                        --  preprocessed sources with redundant line markers
                        --  removed. This means that the preprocessed code
                        --  locations refer to the latter version, which we
                        --  need to recompute there to have the right version
                        --  of the source.

                        Postprocess_Source
                          (Preprocessed_Filename, Postprocessed_Filename);
                        Postprocessed_SFI :=
                          Get_Index_From_Generic_Name
                            (Postprocessed_Filename,
                             Source_File,
                             Insert_After_Freeze => True);
                     else
                        PP_Cmds.Delete (SFI);
                        Outputs.Warn
                          ("unable to preprocess " & Get_Full_Name (SFI)
                           & ". This will result in degraded messages at lines"
                           & " with macro expansions.");
                     end if;
                  end if;
               end;
            end if;

            if Postprocessed_SFI /= No_Source_File then
               Sloc_Start.Source_File := Postprocessed_SFI;
               Sloc_Start.L := Info.PP_Source_Range.First_Sloc;
               Sloc_End.Source_File := Postprocessed_SFI;
               Sloc_End.L := Info.PP_Source_Range.Last_Sloc;
            end if;
         end;
      end if;

      Line := +Get_Line (Sloc_Start);

      if US.Length (Line) < Sloc_Start.L.Column then
         return "";
      end if;

      Col_Start := Sloc_Start.L.Column;

      Sloc_Bound := Sloc_Start;
      Sloc_Bound.L.Column := Sloc_Start.L.Column + Length;

      --  We will print out a shortened view (8 characters by default) of the
      --  SCO tokens.

      if Sloc_Bound <= Sloc_End then
         Col_End := Natural'Min (US.Length (Line), Sloc_Bound.L.Column);
         US.Append (Desc,
                    +US.Unbounded_Slice (Line, Col_Start, Col_End) & "...");
      else
         Col_End := Natural'Min (US.Length (Line), Sloc_End.L.Column);
         US.Append (Desc, US.Unbounded_Slice (Line, Col_Start, Col_End));
      end if;
      return +Desc;
   end SCO_Text;

   ---------------------
   -- SCO_Annotations --
   ---------------------

   function SCO_Annotations (SCO : SCO_Id) return String_Vectors.Vector is
      use Ada.Strings.Unbounded;
   begin
      if Has_PP_Info (SCO) then
         declare
            Info        : constant PP_Info := Get_PP_Info (SCO);
            Annotations : String_Vectors.Vector;
         begin
            if Info.Kind = In_Expansion then
               Annotations.Append
                 ("in definition of macro "
                  & Info.Definition_Loc.Macro_Name
                  & " at location "
                  & Slocs.Image (Info.Definition_Loc.Sloc));

               for Exp_Info of Info.Expansion_Stack loop
                  Annotations.Append
                    ("from expansion of macro "
                     & Exp_Info.Macro_Name
                     & " at location "
                     & Slocs.Image (Exp_Info.Sloc));
               end loop;

               return Annotations;
            end if;
         end;
      end if;
      return String_Vectors.Empty_Vector;
   end SCO_Annotations;

   ------------------------
   -- Output_Annotations --
   ------------------------

   procedure Output_Annotations
     (Output      : Ada.Text_IO.File_Type;
      Annotations : String_Vectors.Vector)
   is
      use Ada.Text_IO;
   begin
      for Annotation of Annotations loop
         Put_Line (Output, "  note: " & (+Annotation));
      end loop;
   end Output_Annotations;

   ---------------
   -- SCO_Image --
   ---------------

   function SCO_Image (SCO : SCO_Id; Length : Natural := 8) return String
   is
   begin
      return """" & SCO_Text (SCO, Length) & """";
   end SCO_Image;

   --------------------------
   -- To_Annotation_Format --
   --------------------------

   function To_Annotation_Format (Option : String) return Annotation_Format is
   begin
      if Option = "asm" then
         return Annotate_Asm;

      elsif Option = "xcov" then
         return Annotate_Xcov;

      elsif Option = "html" then
         return Annotate_Html;

      elsif Option = "xml" then
         return Annotate_Xml;

      elsif Option = "xcov+" then
         return Annotate_Xcov_Plus;

      --  Note that we only keep these option names (html+ and dhtml) for
      --  backward compatibility.

      elsif Option = "html+" then
         return Annotate_Html;

      elsif Option = "dhtml" then
         return Annotate_Html;

      elsif Option = "shtml" then
         return Annotate_Static_Html;

      elsif Option = "shtml+" then
         return Annotate_Static_Html_Plus;

      elsif Option = "report" then
         return Annotate_Report;

      elsif Option = "cobertura" then
         return Annotate_Cobertura;

      else
         return Annotate_Unknown;
      end if;
   end To_Annotation_Format;

   --------------------------
   -- Output_Multiline_Msg --
   --------------------------

   procedure Output_Multiline_Msg
     (Output           : Ada.Text_IO.File_Type;
      Text             : String) is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;

      Line_First : Positive := Text'First;
      --  Index of the first charcter of the current line being handled

      Line_Last : Natural;
      --  Index of the LF character terminating the current line being handled

      EOL : constant String := "" & Ada.Characters.Latin_1.LF;
   begin
      loop
         --  Find the index of the next LF character

         Line_Last := Index
           (Source => Text,
            Pattern => EOL,
            From => Line_First);

         if Line_Last = 0 then
            --  No LF found, so there is only one line remaining.
            --  Print the whole string.

            Put (Output, Text (Line_First .. Text'Last));
         else
            --  LF found at index Line_Last, print until Line_Last - 1 since
            --  we dont want to print the LF character itself.

            Put (Output, Text (Line_First .. Line_Last - 1));
            New_Line (Output);

            --  Move past LF character to get first character of the next line

            Line_First := Line_Last + 1;
         end if;
         exit when Line_Last = 0 or else Line_First > Text'Last;
      end loop;
   end Output_Multiline_Msg;

   ------------------
   -- Line_Metrics --
   ------------------

   function Line_Metrics
     (FI       : File_Info_Access;
      From, To : Natural) return Li_Stat_Array
   is
      Result : Li_Stat_Array := (others => 0);
   begin
      for L in From .. To loop
         declare
            LI : constant Line_Info_Access := Get_Line (FI, L);
            S  : constant Any_Line_State := Aggregated_State (LI.all);
         begin
            --  Update counts. Note that No_Code lines are always counted as
            --  No_Code even if they are part of an exempted region.

            if LI.State = Line_States'(others => No_Code) then
               Result (No_Code) := Result (No_Code) + 1;
            else
               Result (S) := Result (S) + 1;
            end if;
         end;
      end loop;
      return Result;
   end Line_Metrics;

   ------------------------
   -- Obligation_Metrics --
   ------------------------

   function Obligation_Metrics (From, To : SCO_Id) return Ob_Stat_Array
   is
      Result : Ob_Stat_Array;

      procedure Update_Level_Stats
        (SCO   : SCO_Id;
         State : SCO_State;
         Level : Coverage_Level);
      --  Update the obligation statistics for the given coverage level

      ------------------------
      -- Update_Level_Stats --
      ------------------------

      procedure Update_Level_Stats
        (SCO   : SCO_Id;
         State : SCO_State;
         Level : Coverage_Level)
      is
         LI : constant Line_Info_Access := Get_Line (First_Sloc (SCO));
      begin
         if State = No_Code then
            return;
         end if;

         Result (Level).Total :=
           Result (Level).Total + 1;

         if LI.Exemption /= Slocs.No_Location then
            if State = Covered or else State = Not_Coverable then
               Result (Level).Stats (Exempted_No_Violation) :=
                 Result (Level).Stats (Exempted_No_Violation) + 1;
            elsif State = Undetermined_Coverage then
               Result (Level).Stats (Exempted_With_Undetermined_Cov) :=
                 Result (Level).Stats (Exempted_With_Undetermined_Cov) + 1;
            else
               Result (Level).Stats (Exempted_With_Violation) :=
                 Result (Level).Stats (Exempted_With_Violation) + 1;
            end if;
         else
            Result (Level).Stats (State) :=
              Result (Level).Stats (State) + 1;
         end if;
      end Update_Level_Stats;

   begin
      if From = No_SCO_Id then
         return Result;
      end if;
      for SCO in From .. To loop
         case Kind (SCO) is
            when Statement =>
               Update_Level_Stats (SCO, Get_Line_State (SCO, Stmt), Stmt);
            when Decision =>
               if Coverage.Assertion_Coverage_Enabled
                 and then Is_Assertion (SCO)
               then
                  Update_Level_Stats
                    (SCO, Get_Line_State (SCO, ATC), ATC);
               else
                  Update_Level_Stats
                    (SCO, Get_Line_State (SCO, Decision), Decision);
               end if;

               declare
                  Assertion_Decision : constant Boolean :=
                    Coverage.Assertion_Condition_Coverage_Enabled
                    and then Is_Assertion (SCO);
               begin
                  if Coverage.MCDC_Coverage_Enabled or else Assertion_Decision
                  then
                     declare
                        Condition_Level : constant Coverage_Level :=
                          (if Assertion_Decision
                           then Coverage.Assertion_Condition_Level
                           else Coverage.MCDC_Level);
                     begin

                        --  Conditions in that decision

                        for J in
                          Condition_Index'First .. Last_Cond_Index (SCO)
                        loop
                           declare
                              Condition_SCO : constant SCO_Id :=
                                Condition (SCO, J);

                              Line_Condition_State : constant SCO_State :=
                                Get_Line_State (SCO,
                                                (if Assertion_Decision
                                                 then ATCC
                                                 else MCDC));
                              --  If the parent decision is partially covered,
                              --  then the SCO_State for each condition will be
                              --  No_Code, and the SCO_State for the
                              --  MCDC/Assertion condition Coverage_Level
                              --  associated to the parent decision SCO will be
                              --  Not_Covered.

                              Condition_State : SCO_State;

                           begin
                              if Line_Condition_State = Not_Covered then
                                 Condition_State := Not_Covered;
                              else
                                 Condition_State :=
                                   Get_Line_State
                                     (Condition_SCO, (Condition_Level));
                              end if;

                              Update_Level_Stats
                                (SCO, Condition_State, Condition_Level);
                           end;
                        end loop;
                     end;
                  end if;
               end;
            when others =>
               null;
         end case;
      end loop;
      return Result;
   end Obligation_Metrics;

   --------------------
   -- SCO_Kind_Image --
   --------------------

   function SCO_Kind_Image (SCO : SCO_Id) return String is
      K : constant SCO_Kind := Kind (SCO);
   begin
      return (if Kind (SCO) in Decision
              and then Decision_Type (SCO) in Pragma_Decision | Aspect
              and then Coverage.Assertion_Coverage_Enabled
              then "contract expression"
              else To_Lower (SCO_Kind'Image (K)));
   end SCO_Kind_Image;

end Annotations;
