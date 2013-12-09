------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Time_Stamp;

with ALI_Files;
with Coverage.Tags; use Coverage.Tags;
with Qemu_Traces;
with Switches;
with Traces_Files;
with Traces_Files_List;

with Coverage.Source;
use Coverage, Coverage.Source;

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

   type Report_Section is
     range Coverage_Level'Pos (Coverage_Level'First)
        .. Coverage_Level'Pos (Coverage_Level'Last) + 2;
   --  There is one report section for each coverage level, plus the following
   --  two special sections:
   subtype Coverage_Violations is Report_Section
     range Coverage_Level'Pos (Coverage_Level'First)
        .. Coverage_Level'Pos (Coverage_Level'Last);

   Coverage_Exclusions : constant Report_Section := Report_Section'Last - 1;
   Other_Errors        : constant Report_Section := Report_Section'Last;

   function Section_Of_SCO (SCO : SCO_Id) return Report_Section;
   function Section_Of_Message (M : Message) return Report_Section;
   --  Indicate the coverage criterion a given SCO/message pertains to (by its
   --  'Pos), or Other_Errors if SCO has no related section/M is not a
   --  violation message.

   function Underline (S : String; C : Character := '-') return String;
   --  Return, as a string with line-feeds, S underlined with a sequence
   --  of C, for example
   --
   --  input-string-in-S
   --  -----------------

   function Frame (S : String; C : Character := '=') return String;
   --  Similar to Underline, but framing S as in
   --
   --  =======================
   --  == input string in S ==
   --  =======================

   function Highlight (S : String; C : Character := '*') return String;
   --  Return S with a couple of C on both sides, as in
   --
   --  ** input string in S **

   type Messages_Array is array (Report_Section) of Message_Vectors.Vector;

   type SCO_Tally is record
      Total   : Natural := 0;
      Covered : Natural := 0;
   end record;

   type SCO_Tallies_Array is array (Coverage_Level) of SCO_Tally;

   package String_Vectors is
     new Ada.Containers.Vectors
       (Natural,
        Ada.Strings.Unbounded.Unbounded_String,
        Ada.Strings.Unbounded."=");

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

      Nonexempted_Messages : Messages_Array;
      --  All output messages, classified by section according to relevant
      --  coverage level.

      SCO_Tallies : SCO_Tallies_Array;
      --  Tally of SCOs for each report section

      Summary : String_Vectors.Vector;
      --  Lines for SUMMARY chapter
   end record;

   procedure Chapter
     (Pp    : in out Report_Pretty_Printer'Class;
      Title : String);
   --  Open a new chapter in final report

   procedure Section
     (Pp    : in out Report_Pretty_Printer'Class;
      Title : String);
   --  Open a new section in final report

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
      Put_Line (Output.all, Frame (Img (Pp.Current_Chapter) & ". " & Title));
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

   ------------
   --  Frame --
   ------------

   function Frame (S : String; C : Character := '=') return String is
      HS : constant String := Highlight (S, C);
      Line : constant String (1 .. HS'Length) := (others => C);
   begin
      return Line & ASCII.LF & HS & ASCII.LF & Line;
   end Frame;

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

   ---------------
   -- Highlight --
   ---------------

   function Highlight (S : String; C : Character := '*') return String is
      Side : constant String (1 .. 2) := (others => C);
   begin
      return Side & " " & S & " " & Side;
   end Highlight;

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
      use ALI_Files, ALI_Files.ALI_Annotation_Maps;
      use Ada.Strings.Unbounded;

      Output : constant File_Access := Get_Output;

      Total_Messages : Natural;
      --  Total count of output non-exempted messages (both violations and
      --  other messages).

      Total_Exempted_Regions : Natural;

      function Has_Exempted_Region return Boolean;
      --  True iff there's at least one exempted region

      procedure Messages_For_Section
        (MC    : Report_Section;
         Title : String;
         Item  : String);
      --  Output all buffered messages of the given class in a section with the
      --  given title (section omitted if Title is empty). Item is the noun for
      --  the summary line counting messages in the section.

      procedure Output_Message (C : Message_Vectors.Cursor);
      --  Print M in the final report and update item count. The difference
      --  with Pretty_Print_Message is that Put_Message does not tries to know
      --  if the message should be exempted or not.

      procedure Output_Exemption (C : Cursor);
      --  Show summary information for exemption denoted by C

      procedure Count_SCO (SCO : SCO_Id);
      --  Account for SCO in the coverage tally

      ---------------
      -- Count_SCO --
      ---------------

      procedure Count_SCO (SCO : SCO_Id) is
         Section : constant Report_Section := Section_Of_SCO (SCO);
         L       : Coverage_Level;
         State   : SCO_State;
      begin
         if Section /= Other_Errors then
            L := Coverage_Level'Val (Section);

            State := Get_Line_State (SCO, L);

            if State /= No_Code then
               Pp.SCO_Tallies (L).Total := Pp.SCO_Tallies (L).Total + 1;
               if State = Covered then
                  Pp.SCO_Tallies (L).Covered := Pp.SCO_Tallies (L).Covered + 1;
               end if;
            end if;
         end if;
      end Count_SCO;

      -------------------------
      -- Has_Exempted_Region --
      -------------------------

      function Has_Exempted_Region return Boolean is
         C : Cursor := ALI_Annotations.First;
      begin
         while Has_Element (C) loop
            if Element (C).Kind = Exempt_On then
               return True;
            end if;
            Next (C);
         end loop;
         return False;
      end Has_Exempted_Region;

      ----------------------
      -- Output_Exemption --
      ----------------------

      procedure Output_Exemption (C : Cursor) is
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

         New_Line (Output.all);
         Put (Output.all, Image (To_Range (Sloc, End_Sloc)));
         if End_Sloc = Slocs.No_Location then
            Put (Output.all, "-<eof>");
         end if;

         Put (Output.all, ":" & E.Count'Img & " exempted violation");
         if E.Count > 1 then
            Put (Output.all, "s");
         end if;

         Put_Line (Output.all, ", justification:");
         Put_Line (Output.all, E.Message.all);

         Total_Exempted_Regions := Total_Exempted_Regions + 1;
      end Output_Exemption;

      --------------------
      -- Output_Message --
      --------------------

      procedure Output_Message (C : Message_Vectors.Cursor) is
         M     : Message renames Message_Vectors.Element (C);
         Msg   : constant String := To_String (M.Msg);
         First : Natural         := Msg'First;

      begin
         if M.SCO /= No_SCO_Id then
            Put (Output.all, Image (First_Sloc (M.SCO)));
            Put (Output.all, ": ");
            if Msg (First) = '^' then
               First := First + 1;
            else
               Put
                 (Output.all,
                  To_Lower (SCO_Kind'Image (Kind (M.SCO))) & ' ');
            end if;

         else
            Put (Output.all, Image (M.Sloc));
            Put (Output.all, ": ");
         end if;

         Put (Output.all, Msg (First .. Msg'Last));
         if M.SCO /= No_SCO_Id and then M.Tag /= No_SC_Tag then
            Put (Output.all,
                 " (from " & Tag_Provider.Tag_Name (M.Tag) & ")");
         end if;

         Total_Messages := Total_Messages + 1;
         Pp.Item_Count := Pp.Item_Count + 1;
         New_Line (Output.all);
      end Output_Message;

      --------------------------
      -- Messages_For_Section --
      --------------------------

      procedure Messages_For_Section
        (MC    : Report_Section;
         Title : String;
         Item  : String)
      is
      begin
         if Title /= "" then
            Pp.Section (Title);
         end if;

         Pp.Nonexempted_Messages (MC).Iterate (Output_Message'Access);
         if Pp.Item_Count > 0 then
            New_Line (Output.all);
         end if;

         --  Output summary line at end of section

         Put_Line (Output.all, Pluralize (Pp.Item_Count, Item) & ".");

         --  Append summary line for general summary chapter

         Pp.Summary.Append
           (To_Unbounded_String
              (Pluralize
                 (Pp.Item_Count,
                    (case MC is
                       when Coverage_Violations =>
                        "non-exempted "
                          & Coverage_Level'Val (MC)'Img & " " & Item,
                       when Other_Errors        =>
                         "other message",
                       when Coverage_Exclusions  =>
                         "coverage exclusion")) & "."));

         --  Count of total (coverable) and covered SCOs is displayed only
         --  if --all-messages is specified.

         if Switches.All_Messages and then MC in Coverage_Violations then
            declare
               T : SCO_Tally renames Pp.SCO_Tallies (Coverage_Level'Val (MC));
            begin
               Put_Line (Output.all,
                         Pluralize (T.Covered, "coverage obligation")
                         & " covered out of" & T.Total'Img & ".");
            end;
         end if;
      end Messages_For_Section;

      Non_Exempted_Str : constant String := "non-exempted ";
      Non_Exempted     : String renames Non_Exempted_Str
                                          (Non_Exempted_Str'First ..
                                           Boolean'Pos (Has_Exempted_Region)
                                             * Non_Exempted_Str'Last);
      --  If Has_Exempted_Region is True, Non_Exempted = Non_Exempted_Str,
      --  else Non_Exempted = "". Used to omit the mention "non-exempted" when
      --  there's no exemption in sight anyway.

   --  Start of processing for Pretty_Print_End

   begin
      if Source_Coverage_Enabled then
         SC_Obligations.Iterate (Count_SCO'Access);
      end if;

      Pp.Chapter (To_Upper (Non_Exempted) & "COVERAGE VIOLATIONS");

      Total_Messages := 0;

      for L in Coverage_Level loop
         if Enabled (L)
           or else (L = Decision and then MCDC_Coverage_Enabled)
         then
            Messages_For_Section
              (Coverage_Level'Pos (L),
               Title => L'Img & " COVERAGE",
               Item  => "violation");
         else
            pragma Assert
              (Pp.Nonexempted_Messages (Coverage_Level'Pos (L)).Is_Empty);
            null;
         end if;
      end loop;

      if Source_Coverage_Enabled and then Switches.All_Messages then
         Messages_For_Section
           (Other_Errors,
            Title => "OTHER ERRORS",
            Item  => "message");
      end if;

      if Switches.Excluded_SCOs then
         Pp.Chapter ("NON COVERABLE ITEMS");
         New_Line (Output.all);

         Messages_For_Section
           (Coverage_Exclusions,
            Title => "",
            Item  => "exclusion");
      end if;

      if Has_Exempted_Region then
         Pp.Chapter ("EXEMPTED REGIONS");
         Total_Exempted_Regions := 0;
         ALI_Annotations.Iterate (Output_Exemption'Access);

         New_Line (Output.all);
         Put_Line
           (Output.all,
            Pluralize (Total_Exempted_Regions, "exempted region") & ".");
      end if;

      Pp.Chapter ("ANALYSIS SUMMARY");

      New_Line (Output.all);

      for L of Pp.Summary loop
         Put_Line (Output.all, To_String (L));
      end loop;

      if Has_Exempted_Region then
         Put_Line
           (Output.all,
            Pluralize (Total_Exempted_Regions, "exempted region") & ".");
      end if;

      New_Line (Output.all);
      Put_Line (Output.all, Highlight ("END OF REPORT"));
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
      M  : Message)
   is
      MC : constant Report_Section := Section_Of_Message (M);
   begin
      --  Messages with Kind = Notice need not be included in the report

      if M.Kind > Notice then

         --  If M is a violation, check if an exemption is currently active

         if M.Kind = Violation and then Pp.Exemption /= Slocs.No_Location then
            Pp.Exempted_Messages.Append (M);
            Inc_Exemption_Count (Pp.Exemption);
         else
            Pp.Nonexempted_Messages (MC).Append (M);
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
         Put_Line (Output.all, E.Filename.all);
         Put_Line (Output.all, "  program: "
                   & Get_Info (E.Trace, Exec_File_Name));
         Put_Line (Output.all, "  date   : "
                   & Format_Date_Info (Get_Info (E.Trace, Date_Time)));
         Put_Line (Output.all, "  tag    : " & Get_Info (E.Trace, User_Data));
      end Display_Trace_File_Info;

   --  Start of processing for Pretty_Print_Start

   begin
      Put_Line (Output.all, Highlight ("COVERAGE REPORT"));

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

      Put_Line (Output.all, "Trace files:");
      New_Line (Output.all);
      Files.Iterate (Display_Trace_File_Info'Access);
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
                Underline (Img (Pp.Current_Chapter) & "."
                           & Img (Pp.Current_Section) & ". "
                           & Title));
      New_Line (Output.all);
   end Section;

   ------------------------
   -- Section_Of_Message --
   ------------------------

   function Section_Of_Message (M : Message) return Report_Section is
   begin
      if M.SCO /= No_SCO_Id then
         pragma Assert (M.Kind in Coverage_Kind);

         if M.Kind = Exclusion then
            return Coverage_Exclusions;
         else
            pragma Assert (M.Kind = Violation);

            declare
               S : constant Report_Section := Section_Of_SCO (M.SCO);
            begin
               if S = Other_Errors then
                  --  A violation message is expected to always be relevant to
                  --  some report section.

                  raise Program_Error with "unexpected SCO kind in violation";
               end if;
               return S;
            end;
         end if;

      else
         pragma Assert (M.Kind not in Coverage_Kind);

         return Other_Errors;
      end if;
   end Section_Of_Message;

   --------------------
   -- Section_Of_SCO --
   --------------------

   function Section_Of_SCO (SCO : SCO_Id) return Report_Section is
      MCDC_Section : Report_Section;
   begin
      --  Need to initialize MCDC_Section specially because it is erroneous
      --  to evaluate MCDC_Level if MCDC coverage is not enabled.

      if MCDC_Coverage_Enabled then
         MCDC_Section := Coverage_Level'Pos (MCDC_Level);
      else
         MCDC_Section := Other_Errors;
      end if;

      case Kind (SCO) is
         when Statement =>
            return Coverage_Level'Pos (Stmt);

         when Decision =>
            if Is_Expression (SCO) then
               return MCDC_Section;

            else
               return Coverage_Level'Pos (Decision);
            end if;

         when Condition =>
            return MCDC_Section;

         when others =>
            return Other_Errors;
      end case;
   end Section_Of_SCO;

   -----------------
   --  Underline  --
   -----------------

   function Underline (S : String; C : Character := '-') return String is
      Line : constant String (1 .. S'Length) := (others => C);
   begin
      return S & ASCII.LF & Line;
   end Underline;

end Annotations.Report;
