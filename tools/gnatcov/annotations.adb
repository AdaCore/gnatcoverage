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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;

with Interfaces;

with ALI_Files;   use ALI_Files;
with Coverage;
with Coverage.Object;
with Coverage.Source;
with Coverage.Tags;
with Outputs;     use Outputs;
with Strings;     use Strings;
with Switches;    use Switches;
with Traces_Disa;

package body Annotations is

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

   function Aggregated_State (Info : Line_Info) return Any_Line_State is
      use Coverage.Source;
      Result : Line_State := No_Code;
   begin
      --  Exempted case

      if Info.Exemption /= Slocs.No_Location then
         if Get_Exemption_Count (Info.Exemption) = 0 then
            return Exempted_No_Violation;
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

      --  If there is no coverage information to display in the annotated
      --  sources (i.e. if the total number of line with a Line_State is null),
      --  then there is no useful information to add (case in particular of
      --  sources with SCOs but no associated code, e.g. generics that are
      --  not instantiated).

      if Get_Total (FI.Stats) = 0 and then not FI.Has_Source then
         return;
      end if;

      Pretty_Print_Start_File (Pp, File_Index, Skip);

      if Skip then
         return;
      end if;

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
        (Addr  : Pc_Type;
         State : Insn_State;
         Insn  : Binary_Content;
         Sym   : Symbolizer'Class);
      --  Call Pp.Pretty_Print_Insn with the corresponding parameters; this
      --  procedure is meant to be used as a callback in an iterator over
      --  assembly lines (Traces_Disa.Disp_Assembly_Lines).

      -----------------------
      -- Pretty_Print_Insn --
      -----------------------

      procedure Pretty_Print_Insn
        (Addr  : Pc_Type;
         State : Insn_State;
         Insn  : Binary_Content;
         Sym   : Symbolizer'Class) is
      begin
         Pretty_Print_Insn (Pp, Addr, State, Insn, Sym);
      end Pretty_Print_Insn;

      --  Local variables

      Instruction_Set : Addresses_Info_Acc;
      Sec_Info        : Addresses_Info_Acc;
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

            Label : constant String :=
              Get_Label (Info.Exec.all, Instruction_Set);
            Symbol : constant Addresses_Info_Acc :=
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
           (Sec_Info.Section_Content
            (Instruction_Set.First .. Instruction_Set.Last),
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
      use Coverage.Source;

      SCO_State : Line_State;
   begin
      if not Coverage.Source_Coverage_Enabled or else LI.SCOs = null then
         return;
      end if;

      for SCO of LI.SCOs.all loop
         --  Process a given SCO exactly once; to do so, only process a
         --  SCO when its first sloc is at the current line. Otherwise, it
         --  should have been processed earlier.

         if First_Sloc (SCO).L.Line = Line then
            case Kind (SCO) is
               when Statement =>
                  if Coverage.Enabled (Coverage.Stmt) then
                     SCO_State := Get_Line_State (SCO, Coverage.Stmt);
                     Pretty_Print_Statement (Pp, SCO, SCO_State);
                  end if;

               when Decision =>
                  if Coverage.Enabled (Coverage.Decision)
                    or else Coverage.MCDC_Coverage_Enabled
                  then
                     if Coverage.MCDC_Coverage_Enabled then
                        SCO_State := Get_Line_State (SCO, Coverage.MCDC_Level);

                     elsif Coverage.Enabled (Coverage.Decision) then
                        SCO_State := Get_Line_State (SCO, Coverage.Decision);
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
     (Pp           : in out Pretty_Printer'Class;
      Show_Details : Boolean)
   is
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

      procedure Compute_File_State (File_Index : Source_File_Index) is
         use Coverage;

         FI : constant File_Info_Access := Get_File (File_Index);

         procedure Compute_Line_State (L : Positive);
         --  Given a source line located in FI's source file, at line L,
         --  compute its line state and record it into the file table.

         procedure Compute_Stats (L : Positive);
         --  Update file statistics for line L. Note that this can be done
         --  only once Compute_Line_State for each line has been computed,
         --  because this depends on violation count for each exampted region.

         ------------------------
         -- Compute_Line_State --
         ------------------------

         procedure Compute_Line_State (L : Positive) is
            LI        : constant Line_Info_Access := Get_Line (FI, L);
            S         : Line_State;
            Sloc      : Source_Location;

         begin
            --  Compute state for each coverage objective

            if Object_Coverage_Enabled then
               Object.Compute_Line_State (LI);
            else
               Source.Compute_Line_State (L, LI);
            end if;

            --  Compute aggregated line state before exemption

            S := Aggregated_State (LI.all);

            --  Now determine whether this line is covered by an exemption.

            --  First check whether the beginning of the line is exempted. If
            --  not, find the first statement SCO starting on the line, and
            --  check for exemption at that point.

            --  Note that the first statement SCO for the line may be a
            --  multi-line statement starting on an earlier line).

            Sloc := (File_Index, (L, 0));
            LI.Exemption := Get_Exemption (Sloc);

            if LI.Exemption = Slocs.No_Location then
               if LI.SCOs /= null then
                  for SCO of LI.SCOs.all loop
                     if Kind (SCO) = Statement
                          and then Sloc < First_Sloc (SCO)
                     then
                        Sloc := First_Sloc (SCO);
                        exit;
                     end if;
                  end loop;
               end if;
               LI.Exemption := Get_Exemption (Sloc);
            end if;

            --  If exempted, bump exemption hit counter if generating annotated
            --  sources (or HTML). Note that for the Report case, we count
            --  exempted messages, not lines, and we manage the counter
            --  specifically in Annotation.Report.

            if LI.Exemption /= Slocs.No_Location
              and then S in Not_Covered .. Partially_Covered
              and then Annotation /= Annotate_Report
            then
               Inc_Exemption_Count (LI.Exemption);
            end if;
         end Compute_Line_State;

         -------------------
         -- Compute_Stats --
         -------------------

         procedure Compute_Stats (L : Positive) is
            LI : constant Line_Info_Access := Get_Line (FI, L);
            S  : constant Any_Line_State := Aggregated_State (LI.all);

         begin
            --  Update counts. Note that No_Code lines are always counted as
            --  No_Code even if they are part of an exempted region.

            if LI.State = Line_States'(others => No_Code) then
               FI.Stats (No_Code) := FI.Stats (No_Code) + 1;
            else
               FI.Stats (S) := FI.Stats (S) + 1;
            end if;
         end Compute_Stats;

      --  Start of processing for Compute_File_State

      begin
         FI.Stats := (others => 0);
         Iterate_On_Lines (FI, Compute_Line_State'Access);
         Iterate_On_Lines (FI, Compute_Stats'Access);

         for J in Global_Stats'Range loop
            Global_Stats (J) := Global_Stats (J) + FI.Stats (J);
         end loop;
      end Compute_File_State;

      ----------------------
      -- Process_One_File --
      ----------------------

      procedure Process_One_File (File_Index : Source_File_Index) is
         FI : constant File_Info_Access := Get_File (File_Index);
      begin
         if To_Display (FI) then
            Disp_File_Line_State (Pp, File_Index, FI);
         end if;
      end Process_One_File;

   --  Start of processing for Generate_Report

   begin
      Pp.Show_Details := Show_Details;

      --  Compute lines state, files and global statistics

      Global_Stats := (others => 0);
      Files_Table_Iterate (Compute_File_State'Access);

      --  Print

      Pretty_Print_Start (Pp);
      Files_Table_Iterate (Process_One_File'Access);
      Pretty_Print_End (Pp);
   end Generate_Report;

   -------------------
   -- Get_Exemption --
   -------------------

   function Get_Exemption (Sloc : Source_Location) return Source_Location is
      use ALI_Annotation_Maps;

      Cur : constant Cursor := ALI_Annotations.Floor (Sloc);
   begin
      if not Debug_Ignore_Exemptions
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

   -------------------------
   -- Get_Exemption_Count --
   -------------------------

   function Get_Exemption_Count
     (Sloc : Source_Location) return Natural
   is
   begin
      return ALI_Annotations.Element (Sloc).Count;
   end Get_Exemption_Count;

   ---------------------------
   -- Get_Exemption_Message --
   ---------------------------

   function Get_Exemption_Message
     (Sloc : Source_Location) return String_Access
   is
   begin
      return ALI_Annotations.Element (Sloc).Message;
   end Get_Exemption_Message;

   -------------------------
   -- Inc_Exemption_Count --
   -------------------------

   procedure Inc_Exemption_Count (Sloc : Source_Location) is

      procedure Inc_Count (K : Source_Location; E : in out ALI_Annotation);
      --  Increment E.Count

      ---------------
      -- Inc_Count --
      ---------------

      procedure Inc_Count (K : Source_Location; E : in out ALI_Annotation) is
         pragma Unreferenced (K);
      begin
         E.Count := E.Count + 1;
      end Inc_Count;

   --  Start of processing for Inc_Exemption_Count

   begin
      ALI_Annotations.Update_Element
        (ALI_Annotations.Find (Sloc), Inc_Count'Access);
   end Inc_Exemption_Count;

   ------------------------
   -- Message_Annotation --
   ------------------------

   function Message_Annotation (M : Message) return String is
      use Coverage, Coverage.Tags;
      use Ada.Strings.Unbounded;
   begin
      if M.SCO /= No_SCO_Id then
         return To_Lower (SCO_Kind'Image (Kind (M.SCO)))
           & " """ & SCO_Text (M.SCO) & '"'
           & " at "
           & Img (First_Sloc (M.SCO).L.Line) & ":"
           & Img (First_Sloc (M.SCO).L.Column)
           & (if M.Tag = No_SC_Tag
              then ""
              else " (from " & Tag_Provider.Tag_Name (M.Tag) & ")")
           & " " & To_String (M.Msg);
      else
         return Image (M.Sloc) & ": " & To_String (M.Msg);
      end if;
   end Message_Annotation;

   --------------
   -- SCO_Text --
   --------------

   function SCO_Text (SCO : SCO_Id; Length : Natural := 8) return String is
      Sloc_Start : constant Source_Location := First_Sloc (SCO);
      Sloc_End   : constant Source_Location :=
                     End_Lex_Element (Last_Sloc (SCO));
      Sloc_Bound : Source_Location;
      Line       : constant String := Get_Line (Sloc_Start);
      Col_Start  : constant Natural := Sloc_Start.L.Column;
      Col_End    : Natural;
   begin
      if Line'Last < Sloc_Start.L.Column then
         return "";
      end if;

      Sloc_Bound := Sloc_Start;
      Sloc_Bound.L.Column := Sloc_Start.L.Column + Length;

      if Sloc_Bound < Sloc_End then
         Col_End := Natural'Min (Line'Last, Sloc_Bound.L.Column);
         return Line (Col_Start .. Col_End) & "...";
      else
         Col_End := Natural'Min (Line'Last, Sloc_End.L.Column);
         return Line (Col_Start .. Col_End);
      end if;
   end SCO_Text;

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

      elsif Option = "html+" then
         return Annotate_Html_Plus;

      elsif Option = "report" then
         return Annotate_Report;

      else
         return Annotate_Unknown;
      end if;
   end To_Annotation_Format;

end Annotations;
