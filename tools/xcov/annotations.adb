------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2009-2010, AdaCore                      --
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
with Interfaces;

with ALI_Files;   use ALI_Files;
with Coverage;
with Coverage.Object;
with Coverage.Source;
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
     (Pp   : in out Pretty_Printer'Class;
      LI   : Line_Info)
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
      Info            : Files_Table.Object_Coverage_Info_Acc;
      Sec_Info        : Addresses_Info_Acc;
      Ls              : constant Any_Line_State := Aggregated_State (LI);
      In_Symbol       : Boolean;
      In_Insn_Set     : Boolean;

   --  Start of processing for Disp_Instruction_Sets

   begin
      if Coverage.Object_Coverage_Enabled then
         Info := LI.Obj_First;

         if Info /= null then
            Pretty_Print_Start_Instruction_Set (Pp, Ls);
            In_Insn_Set := True;
         else
            In_Insn_Set := False;
         end if;

         --  Iterate over each insn block for the source line

         while Info /= null loop
            Instruction_Set := Info.Instruction_Set;
            declare
               use Interfaces;

               Label : constant String :=
                 Get_Label (Info.Exec.all, Instruction_Set);
               Symbol : constant Addresses_Info_Acc :=
                 Get_Symbol (Info.Exec.all, Instruction_Set.First);
            begin
               if Label'Length > 0 and Symbol /= null then
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

            Info := Info.Next;
         end loop;

         if In_Insn_Set then
            Pretty_Print_End_Instruction_Set (Pp);
         end if;
      end if;
   end Disp_Instruction_Sets;

   -------------------
   -- Disp_Messages --
   -------------------

   procedure Disp_Messages
     (Pp : in out Pretty_Printer'Class;
      LI : Line_Info)
   is
      use Message_Vectors;

      procedure Pretty_Print_Message (Position : Cursor);
      --  Let Pp print the message in the line's message vector at Position
      --  if this message is an error or a warning

      --------------------------
      -- Pretty_Print_Message --
      --------------------------

      procedure Pretty_Print_Message (Position : Cursor) is
         M : Message renames Element (Position);
      begin
         if M.Kind /= Notice then
            Pretty_Print_Message (Pp, M);
         end if;
      end Pretty_Print_Message;

      --  Start of processing for Disp_Messages

   begin
      LI.Messages.Iterate (Pretty_Print_Message'Access);
   end Disp_Messages;

   -----------------------
   -- Disp_File_Summary --
   -----------------------

   procedure Disp_File_Summary is

      procedure Disp_One_File (File : File_Info_Access);
      --  Display summary for the given file

      procedure Process_One_File (File_Index : Source_File_Index);
      --  Process file at Index and display its summary if it has some
      --  coverage info to display.

      -------------------
      -- Disp_One_File --
      -------------------

      procedure Disp_One_File (File : File_Info_Access) is
      begin
         Put (File.Simple_Name.all);
         Put (": ");
         Put (Get_Stat_String (File.Stats));
         New_Line;
      end Disp_One_File;

      ----------------------
      -- Process_One_File --
      ----------------------

      procedure Process_One_File (File_Index : Source_File_Index) is
         FI : constant File_Info_Access := Get_File (File_Index);
      begin
         if To_Display (FI) then
            Disp_One_File (FI);
         end if;
      end Process_One_File;

   --  Start of processing for Disp_File_Summary

   begin
      Files_Table_Iterate (Process_One_File'Access);
   end Disp_File_Summary;

   ---------------
   -- Disp_SCOs --
   ---------------

   procedure Disp_SCOs
     (Pp   : in out Pretty_Printer'Class;
      Line : Natural;
      LI   : Line_Info)
   is
      use SCO_Id_Vectors;

      procedure Process_One_SCO (Position : Cursor);
      --  Let Pp display the SCO at Position (and this SCO's children)

      ---------------------
      -- Process_One_SCO --
      ---------------------

      procedure Process_One_SCO (Position : Cursor) is
         use Coverage.Source;

         SCO       : constant SCO_Id := Element (Position);
         SCO_State : Line_State;
      begin
         --  Process a given SCO exactly once; to do so, only process a
         --  SCO when its first sloc is at the current line. Otherwise, it
         --  should have been processed earlier.

         if First_Sloc (SCO).Line = Line then
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
                  --  Condition without a father decision. This should never
                  --  happen; fatal error.

                  Fatal_Error ("no decision attached to " & Image (SCO));

               when Operator =>
                  null;

            end case;
         end if;
      end Process_One_SCO;

      --  Start of processing for Disp_SCOs

   begin
      if Coverage.Source_Coverage_Enabled then
         LI.SCOs.Iterate (Process_One_SCO'Access);
      end if;
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

            Sloc := (File_Index, L, 0);
            LI.Exemption := Get_Exemption (Sloc);

            if LI.Exemption = Slocs.No_Location then
               declare
                  C : SCO_Id_Vectors.Cursor := LI.SCOs.First;
               begin
                  Line_SCOs : while SCO_Id_Vectors.Has_Element (C) loop
                     declare
                        SCO : constant SCO_Id := SCO_Id_Vectors.Element (C);
                     begin
                        if Kind (SCO) = Statement
                             and then Sloc < First_Sloc (SCO)
                        then
                           Sloc := First_Sloc (SCO);
                           exit Line_SCOs;
                        end if;
                        SCO_Id_Vectors.Next (C);
                     end;
                  end loop Line_SCOs;
               end;
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
   begin
      if M.SCO /= No_SCO_Id then
         return SCO_Kind'Image (Kind (M.SCO))
           & " """ & SCO_Text (M.SCO) & '"'
           & " at " & Img (M.Sloc.Line) & ":" & Img (M.Sloc.Column)
           & ": " & M.Msg.all;
      else
         return Image (M.Sloc) & ": " & M.Msg.all;
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
      Col_Start  : constant Natural := Sloc_Start.Column;
      Col_End    : Natural;
   begin
      if Line'Last < Sloc_Start.Column then
         return "";
      end if;

      Sloc_Bound := Sloc_Start;
      Sloc_Bound.Column := Sloc_Start.Column + Length;

      if Sloc_Bound < Sloc_End then
         Col_End := Natural'Min (Line'Last, Sloc_Bound.Column);
         return Line (Col_Start .. Col_End) & "...";
      else
         Col_End := Natural'Min (Line'Last, Sloc_End.Column);
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
