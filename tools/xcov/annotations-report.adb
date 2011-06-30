------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2011, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Time_Stamp;

with ALI_Files;
with Qemu_Traces;
with Switches;
with Traces_Files;
with Traces_Files_List;

with Coverage; use Coverage;
with Version;  use Version;
with Strings;  use Strings;

package body Annotations.Report is

   type Final_Report_Type is limited record
      --  Final report information

      Name   : String_Access := null;
      --  Final report's file name

      File   : aliased File_Type;
      --  Handle to the final report
   end record;

   Final_Report : aliased Final_Report_Type;

   procedure Open_Report_File (Final_Report_Name : String);
   --  Open Final_Report_Name and use it as the report file

   function Get_Output return File_Access;
   --  Return a handle to the current report file

   procedure Close_Report_File;
   --  Close the handle to the final report

   --  Pretty printer type for the final report

   type Violations_Array is array (Coverage_Level) of Natural;
   type Report_Pretty_Printer is new Pretty_Printer with record
      Current_File_Index : Source_File_Index;
      --  When going through the lines of a source file, this is set to the
      --  current source file index.

      Item_Count : Natural := 0;
      --  Total count of items (violations/exempted regions) in current section

      Current_Chapter : Natural := 0;
      --  Current chapter in final report

      Current_Section : Natural := 0;
      --  Current section in final report

      Exempted_Messages : Message_Vectors.Vector;
      --  Messages that have been covered by an exemption

      Exemption : Slocs.Source_Location := Slocs.No_Location;
      --  Exemption sloc applying to current line, if any

      Violations : Violations_Array := (others => 0);
      --  Tally of violations by coverage objective
   end record;

   procedure Chapter
     (Pp    : in out Report_Pretty_Printer'Class;
      Title : String);
   --  Open a new chapter in final report

   procedure Section
     (Pp    : in out Report_Pretty_Printer'Class;
      Title : String);
   --  Open a new section in final report

   procedure Put_Message
     (Pp : in out Report_Pretty_Printer'Class;
      M  : Message);
   --  Print M in the final report and update item count. The difference with
   --  Pretty_Print_Message is that Put_Message does not tries to know if the
   --  message should be exempted or not, and do not modify the
   --  Exempted_Messages buffer.

   procedure Count_Violation
     (Pp : in out Report_Pretty_Printer'Class;
      M  : Message);
   --  Count M in violations tally

   function Pluralize (Count : Natural; Item : String) return String;
   --  Return:
   --    "No <item>"       (if Count = 0)
   --    "1 <item>"        (if Count = 1)
   --    "<Count> <item>s" (if Count > 1)

   --------------------------------------------------
   -- Report_Pretty_Printer's primitive operations --
   --      (inherited from Pretty_Printer)         --
   --------------------------------------------------

   procedure Pretty_Print_Start (Pp : in out Report_Pretty_Printer);

   procedure Pretty_Print_End (Pp : in out Report_Pretty_Printer);

   procedure Pretty_Print_Start_File
     (Pp   : in out Report_Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean);

   procedure Pretty_Print_Start_Line
     (Pp       : in out Report_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String);

   procedure Pretty_Print_End_File
     (Pp : in out Report_Pretty_Printer);

   procedure Pretty_Print_Message
     (Pp : in out Report_Pretty_Printer;
      M  : Message);

   -------------
   -- Chapter --
   -------------

   procedure Chapter
     (Pp    : in out Report_Pretty_Printer'Class;
      Title : String)
   is
      Output : constant File_Access := Get_Output;
   begin
      Pp.Current_Chapter := Pp.Current_Chapter + 1;

      New_Line (Output.all);
      Put_Line (Output.all, Img (Pp.Current_Chapter) & ". " & Title);
   end Chapter;

   -----------------------
   -- Close_Report_File --
   -----------------------

   procedure Close_Report_File is
   begin
      if Final_Report.Name /= null then
         Close (Final_Report.File);
         Free (Final_Report.Name);
      end if;
   end Close_Report_File;

   ---------------------
   -- Count_Violation --
   ---------------------

   procedure Count_Violation
     (Pp : in out Report_Pretty_Printer'Class;
      M  : Message)
   is
   begin
      pragma Assert (M.SCO /= No_SCO_Id);
      case Kind (M.SCO) is
         when Statement =>
            Pp.Violations (Stmt) := Pp.Violations (Stmt) + 1;

         when Decision =>
            if Is_Expression (M.SCO) then
               Pp.Violations (MCDC_Level) := Pp.Violations (MCDC_Level) + 1;
            else
               Pp.Violations (Decision) := Pp.Violations (Decision) + 1;
            end if;

         when Condition =>
            Pp.Violations (MCDC_Level) := Pp.Violations (MCDC_Level) + 1;

         when others =>
            raise Program_Error with "unexpected SCO kind in violation";
      end case;
   end Count_Violation;

   ---------------------
   -- Generate_Report --
   ---------------------

   procedure Generate_Report (Final_Report_Name : String_Access) is
      Report_PP : Report_Pretty_Printer;
   begin
      if Final_Report_Name /= null then
         Open_Report_File (Final_Report_Name.all);
      end if;

      Annotations.Generate_Report (Report_PP, True);
      Close_Report_File;
   end Generate_Report;

   ----------------
   -- Get_Output --
   ----------------

   function Get_Output return File_Access is
   begin
      if Final_Report.Name /= null then
         return Final_Report.File'Access;
      else
         return Standard_Output;
      end if;
   end Get_Output;

   ----------------------
   -- Open_Report_File --
   ----------------------

   procedure Open_Report_File (Final_Report_Name : String) is
   begin
      Final_Report.Name := new String'(Final_Report_Name);
      Create (Final_Report.File, Out_File, Final_Report_Name);
   end Open_Report_File;

   ---------------
   -- Pluralize --
   ---------------

   function Pluralize (Count : Natural; Item : String) return String is
   begin
      if Count = 0 then
         return "No " & Item;

      elsif Count = 1 then
         return "1 " & Item;

      else
         return Img (Count) & " " & Item & "s";
      end if;
   end Pluralize;

   ----------------------
   -- Pretty_Print_End --
   ----------------------

   procedure Pretty_Print_End (Pp : in out Report_Pretty_Printer) is
      use ALI_Files;
      use ALI_Files.ALI_Annotation_Maps;

      Output : constant File_Access := Get_Output;

      procedure Process_One_Exemption (C : Cursor);
      --  Show summary information for exemption denoted by C

      ---------------------------
      -- Process_One_Exemption --
      ---------------------------

      procedure Process_One_Exemption (C : Cursor) is
         E        : constant ALI_Annotation := Element (C);
         Next_C   : constant Cursor := Next (C);
         Sloc     : constant Source_Location := Key (C);
         End_Sloc : Source_Location := Slocs.No_Location;
      begin
         if E.Kind /= Exempt_On then
            return;
         end if;

         --  Determine end sloc of exempted region

         if Next_C /= No_Element then
            declare
               Next_Sloc : constant Source_Location := Key (Next_C);
               Next_E    : constant ALI_Annotation  := Element (Next_C);
            begin
               if Next_E.Kind = Exempt_Off
                 and then Sloc.Source_File = Next_Sloc.Source_File
               then
                  End_Sloc := Next_Sloc;
               end if;
            end;
         end if;

         --  Output summary for this region: sloc range, exempted message count
         --  and justification.

         Put (Output.all,
           Image (Slocs.Source_Location_Range'
                    (First_Sloc => Sloc, Last_Sloc => End_Sloc)));
         if End_Sloc = Slocs.No_Location then
            Put (Output.all, "-<eof>");
         end if;

         Put (Output.all, ":" & E.Count'Img & " exempted violation");
         if E.Count > 1 then
            Put (Output.all, "s");
         end if;

         Put_Line (Output.all, ", justification:");
         Put_Line (Output.all, E.Message.all);
         New_Line (Output.all);

         Pp.Item_Count := Pp.Item_Count + 1;
      end Process_One_Exemption;

      Total_Violations : Natural;

   --  Start of processing for Pretty_Print_End

   begin
      if Source_Coverage_Enabled then
         Total_Violations := 0;
         for J in Pp.Violations'Range loop
            Total_Violations := Total_Violations + Pp.Violations (J);
         end loop;
      else
         Total_Violations := Pp.Item_Count;
      end if;

      if Pp.Item_Count > 0 then
         New_Line (Output.all);
      end if;

      Put_Line (Output.all, Pluralize (Total_Violations, "violation") & ".");

      if Source_Coverage_Enabled and then Switches.All_Messages then
         Put_Line
           (Output.all,
            Pluralize (Pp.Item_Count - Total_Violations, "other message")
            & ".");
      end if;

      Pp.Section ("EXEMPTED REGIONS");
      ALI_Annotations.Iterate (Process_One_Exemption'Access);

      if Pp.Item_Count > 0 then
         New_Line (Output.all);
      end if;

      Put_Line
        (Output.all, Pluralize (Pp.Item_Count, "exempted region") & ".");

      Pp.Chapter ("ANALYSIS SUMMARY");

      New_Line (Output.all);
      for J in Pp.Violations'Range loop
         if Enabled (J)
           or else (J = Decision and then MCDC_Coverage_Enabled)
         then
            Put_Line
              (Output.all,
               Pluralize (Pp.Violations (J),
                 "non-exempted " & J'Img & " violation") & '.');
         end if;
      end loop;
      New_Line (Output.all);
      Put_Line (Output.all, "END OF REPORT");
   end Pretty_Print_End;

   ---------------------------
   -- Pretty_Print_End_File --
   ---------------------------

   procedure Pretty_Print_End_File (Pp : in out Report_Pretty_Printer) is
   begin
      null;
   end Pretty_Print_End_File;

   -----------------------------
   -- Pretty_Print_Start_Line --
   -----------------------------

   procedure Pretty_Print_Start_Line
     (Pp       : in out Report_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String)
   is
      pragma Unreferenced (Line_Num, Line);
   begin
      Pp.Exemption := Info.Exemption;
   end Pretty_Print_Start_Line;

   --------------------------
   -- Pretty_Print_Message --
   --------------------------

   procedure Pretty_Print_Message
     (Pp : in out Report_Pretty_Printer;
      M  : Message) is
   begin
      --  Messages with Kind = Notice need not be included in the report

      if M.Kind > Notice then
         if Pp.Exemption /= Slocs.No_Location then
            Pp.Exempted_Messages.Append (M);
            Inc_Exemption_Count (Pp.Exemption);
         else
            Pp.Put_Message (M);
         end if;
      end if;
   end Pretty_Print_Message;

   ------------------------
   -- Pretty_Print_Start --
   ------------------------

   procedure Pretty_Print_Start (Pp : in out Report_Pretty_Printer) is
      use Ada.Command_Line;
      use Qemu_Traces;
      use Traces_Files;
      use Traces_Files_List;
      use Traces_Files_Lists;

      Output : constant File_Access := Get_Output;

      procedure Display_Trace_File_Info (Position : Cursor);
      --  Print info from trace file at Position

      -----------------------------
      -- Display_Trace_File_Info --
      -----------------------------

      procedure Display_Trace_File_Info (Position : Cursor) is
         E : constant Trace_File_Element_Acc := Element (Position);
      begin
         Put_Line (Output.all, " " & E.Filename.all);
         Put_Line (Output.all, "  program: "
                   & Get_Info (E.Trace, Exec_File_Name));
         Put_Line (Output.all, "  date: "
                   & Format_Date_Info (Get_Info (E.Trace, Date_Time)));
         Put_Line (Output.all, "  tag: " & Get_Info (E.Trace, User_Data));
      end Display_Trace_File_Info;

   --  Start of processing for Pretty_Print_Start

   begin
      Put_Line (Output.all, "COVERAGE REPORT");
      New_Line (Output.all);

      Pp.Chapter ("ASSESSMENT CONTEXT");

      New_Line (Output.all);
      Put_Line (Output.all, "Date and time of execution: "
                & GNAT.Time_Stamp.Current_Time);
      Put_Line (Output.all, "Tool version: XCOV " & Xcov_Version);
      New_Line (Output.all);

      Put_Line (Output.all, "Command line:");
      New_Line (Output.all);
      Put (Output.all, Command_Name);
      for J in 1 .. Argument_Count loop
         Put (Output.all, ' ' & Argument (J));
      end loop;
      New_Line (Output.all, 2);

      Put_Line (Output.all, "Coverage level: " & Coverage_Option_Value);
      New_Line (Output.all);

      Put_Line (Output.all, "trace files:");
      New_Line (Output.all);
      Files.Iterate (Display_Trace_File_Info'Access);

      Pp.Chapter ("VIOLATIONS AND EXEMPTION REGIONS");
      Pp.Section ("NON-EXEMPTED VIOLATIONS");
   end Pretty_Print_Start;

   -----------------------------
   -- Pretty_Print_Start_File --
   -----------------------------

   procedure Pretty_Print_Start_File
     (Pp   : in out Report_Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean)
   is
      Info : constant File_Info_Access := Get_File (File);
   begin
      if Info.Stats (Covered) /= Get_Total (Info.Stats) then

         --  Some uncovered or partially covered lines are present

         Pp.Current_File_Index := File;
         Skip := False;

      else
         --  Everything covered: nothing to report for this file

         Skip := True;
      end if;
   end Pretty_Print_Start_File;

   -----------------
   -- Put_Message --
   -----------------

   procedure Put_Message
     (Pp : in out Report_Pretty_Printer'Class;
      M  : Message)
   is
      Output : constant File_Access := Get_Output;
   begin
      if M.SCO /= No_SCO_Id then
         Put (Output.all, Image (First_Sloc (M.SCO)));
         Put (Output.all, ": ");
         Put (Output.all, To_Lower (SCO_Kind'Image (Kind (M.SCO))) & ' ');

         Pp.Count_Violation (M);
      else
         Put (Output.all, Image (M.Sloc));
         Put (Output.all, ": ");
      end if;

      Put (Output.all, M.Msg.all);
      Pp.Item_Count := Pp.Item_Count + 1;
      New_Line (Output.all);
   end Put_Message;

   -------------
   -- Section --
   -------------

   procedure Section
     (Pp    : in out Report_Pretty_Printer'Class;
      Title : String)
   is
      Output : constant File_Access := Get_Output;
   begin
      Pp.Current_Section := Pp.Current_Section + 1;
      Pp.Item_Count := 0;

      New_Line (Output.all);
      Put_Line (Output.all,
                Img (Pp.Current_Chapter) & "."
                & Img (Pp.Current_Section) & ". "
                & Title);
      New_Line (Output.all);
   end Section;

end Annotations.Report;
