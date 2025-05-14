------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Text_IO;             use Ada.Text_IO;

with Calendar_Utils;  use Calendar_Utils;
with Coverage;        use Coverage;
with Coverage.Source; use Coverage.Source;
with SC_Obligations;
with Switches;
with Traces_Files;    use Traces_Files;

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

   package Messages_Of_Exempted_Region is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Source_Location,
        Element_Type => Message_Vectors.Vector,
        "="          => Message_Vectors."=");

   package String_Vectors is
     new Ada.Containers.Vectors (Natural, Unbounded_String, "=");

   type Report_Pretty_Printer is new Pretty_Printer with record
      Current_File_Index : Source_File_Index;
      --  When going through the lines of a source file, this is set to the
      --  current source file index.

      Current_Chapter : Natural := 0;
      --  Current chapter in final report

      Current_Section : Natural := 0;
      --  Current section in final report

      Exempted_Messages : Messages_Of_Exempted_Region.Map :=
        Messages_Of_Exempted_Region.Empty_Map;
      --  Messages that have been covered by an exemption

      Exemption : Slocs.Source_Location := Slocs.No_Location;
      --  Exemption sloc applying to current line, if any

      Disabled_Cov : Slocs.Source_Location := Slocs.No_Location;
      --  Disabled coverage sloc applying to current line, if any

      Nonexempted_Messages : Messages_Array;
      --  All output messages, classified by section according to relevant
      --  coverage level.

      SCO_Tallies : Ob_Stat_Array;
      --  Tally of SCOs for each report section

      Summary : String_Vectors.Vector;
      --  Lines for SUMMARY chapter

      Dump_Units : Boolean;
      --  Whether to add a section for the list of names for units of interest
   end record;

   overriding function Format
     (Pp : Report_Pretty_Printer) return Annotation_Format_Family
   is (Annotate_Report);

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

   procedure Generate_Report
     (Context           : Coverage.Context_Access;
      Final_Report_Name : String_Access;
      Dump_Units        : Boolean)
   is
      Pp : Report_Pretty_Printer :=
        (Need_Sources => False,
         Context      => Context,
         Dump_Units   => Dump_Units,
         others       => <>);
   begin
      if Final_Report_Name /= null then
         Open_Report_File (Final_Report_Name.all);
      end if;

      Annotations.Generate_Report (Pp, True);
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
      use SC_Obligations.ALI_Annotation_Maps;

      ALI_Annotations : constant ALI_Annotation_Maps.Map :=
        Get_All_Annotations;

      Output : constant File_Access := Get_Output;

      Total_Exempted_Regions : Natural := 0;
      Total_Exempted_Violations : Natural := 0;

      Total_Disabled_Cov_Regions : Natural := 0;

      function Has_Exempted_Region return Boolean;
      --  True iff there's at least one exempted region

      function Has_Disabled_Cov_Region return Boolean;
      --  True iff there's at least one region with disabled coverage

      procedure Messages_For_Section
        (MC    : Report_Section;
         Title : String;
         Item  : String);
      --  Output all buffered messages of the given class in a section with the
      --  given title (section omitted if Title is empty). Item is the noun for
      --  the summary line counting messages in the section.

      procedure Output_Message (C : Message_Vectors.Cursor);
      --  Print M in the final report and update item count. The difference
      --  with Pretty_Print_Message is that Put_Message does not tries to
      --  know if the message should be exempted or not.

      procedure Output_Exemption (C : Cursor);
      --  Show summary information for exemption denoted by C

      procedure Output_Disable_Cov (C : Cursor);
      --  Show summary information for disabled coverage regions

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

            --  Some decision SCOs (complex boolean expressions not in control
            --  structures) belong to the MC/DC report section. We should still
            --  get their state according to the Decision Coverage_Level (is
            --  the decision covered or not), and not according to the MCDC
            --  coverage level.

            if Kind (SCO) = Decision then
               State := Get_Line_State (SCO, Decision);
            else
               State := Get_Line_State (SCO, L);
            end if;

            if State /= No_Code then
               Pp.SCO_Tallies (L).Total := Pp.SCO_Tallies (L).Total + 1;
               if State = Covered then
                  Pp.SCO_Tallies (L).Stats (Covered) :=
                    Pp.SCO_Tallies (L).Stats (Covered) + 1;
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

      ----------------------------
      -- Has_Disable_Cov_Region --
      ----------------------------

      function Has_Disabled_Cov_Region return Boolean is
         C : Cursor := ALI_Annotations.First;
      begin
         while Has_Element (C) loop
            if Element (C).Kind = Cov_Off then
               return True;
            end if;
            Next (C);
         end loop;
         return False;
      end Has_Disabled_Cov_Region;

      Non_Exempted_Str : constant String := "non-exempted ";
      Non_Exempted     : String renames Non_Exempted_Str
                                          (Non_Exempted_Str'First ..
                                           Boolean'Pos (Has_Exempted_Region)
                                             * Non_Exempted_Str'Last);
      --  If Has_Exempted_Region is True, Non_Exempted = Non_Exempted_Str,
      --  else Non_Exempted = "". Used to omit the mention "non-exempted" when
      --  there's no exemption in sight anyway.

      --------------------
      -- Output_Message --
      --------------------

      procedure Output_Message (C : Message_Vectors.Cursor) is
         M     : Message renames Message_Vectors.Element (C);
         Msg   : constant String := +M.Msg;
         First : Natural := Msg'First;
      begin

         --  For info messages (such as the messages displayed with
         --  --show-mcdc-vectors), do not display the SCO, as it is only
         --  used to attach the message to the right report location.

         if M.Kind /= Info and then M.SCO /= No_SCO_Id then
            Put
              (Output.all, Image (First_Sloc (M.SCO), Unique_Name => True));
            Put (Output.all, ": ");
            if Msg (First) = '^' then
               First := First + 1;
            else
               Put
                 (Output.all,
                  To_Lower (SCO_Kind'Image (Kind (M.SCO)))
                  & (if (Switches.Show_MCDC_Vectors
                    or else Switches.Show_Condition_Vectors)
                    and then Kind (M.SCO) = Condition
                    then Index (M.SCO)'Image
                    & " (" & SCO_Image (M.SCO) & ") "
                    else " "));
            end if;

         else
            Put (Output.all, Image (M.Sloc, Unique_Name => True));
            Put (Output.all, ": ");
         end if;

         Output_Multiline_Msg
           (Output => Output.all,
            Text   => Msg (First .. Msg'Last));

         New_Line (Output.all);
         Output_Annotations (Output.all, SCO_Annotations (M.SCO));
      end Output_Message;

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
         --  justification, and exempted violations.

         New_Line (Output.all);
         Put (Output.all, Image (To_Range (Sloc, End_Sloc)));
         if End_Sloc = Slocs.No_Location then
            Put (Output.all, "-<eof>");
         end if;

         Put (Output.all, ":" & E.Violation_Count'Img & " exempted violation");
         if E.Violation_Count > 1 then
            Put (Output.all, "s");
         end if;

         if E.Undetermined_Cov_Count > 0 then
            Put (Output.all, ";" & E.Undetermined_Cov_Count'Img
                 & " exempted undetermined coverage item");
            if E.Undetermined_Cov_Count > 1 then
               Put (Output.all, "s");
            end if;
         end if;

         Put_Line (Output.all, ", justification:");
         if E.Message /= null then
            Put_Line (Output.all, E.Message.all);
         else
            Put_Line (Output.all, "No justification");
         end if;

         New_Line (Output.all);

         --  Output the contents of the exempted region: the observed exempted
         --  violations.

         declare
            use Messages_Of_Exempted_Region;

            Messages_C : constant Messages_Of_Exempted_Region.Cursor :=
              Pp.Exempted_Messages.Find (Sloc);
         begin
            if Messages_C = Messages_Of_Exempted_Region.No_Element then
               Put_Line (Output.all, "No exempted violations.");
            else
               Put_Line (Output.all, "Exempted violations:");

               for C in Element (Messages_C).Iterate loop
                  Output_Message (C);

                  if Message_Vectors.Element (C).Kind /= Info then
                     Total_Exempted_Violations :=
                       Total_Exempted_Violations + 1;
                  end if;
               end loop;
            end if;
         end;

         Total_Exempted_Regions := Total_Exempted_Regions + 1;
      end Output_Exemption;

      ------------------------
      -- Output_Disable_Cov --
      ------------------------

      procedure Output_Disable_Cov (C : Cursor) is
         E        : constant ALI_Annotation := Element (C);
         Next_C   : constant Cursor := Next (C);
         Sloc     : constant Source_Location := Key (C);
         End_Sloc : Source_Location := Slocs.No_Location;
      begin
         if E.Kind /= Cov_Off then
            return;
         end if;

         --  Determine end sloc of disabled coverage region

         if Next_C /= No_Element then
            declare
               Next_Sloc : constant Source_Location := Key (Next_C);
               Next_E    : constant ALI_Annotation  := Element (Next_C);
            begin
               if Next_E.Kind = Cov_On
                 and then Sloc.Source_File = Next_Sloc.Source_File
               then
                  End_Sloc := Next_Sloc;
               end if;
            end;
         end if;

         --  Output summary for this region

         New_Line (Output.all);
         Put (Output.all, Image (To_Range (Sloc, End_Sloc)));
         if End_Sloc = Slocs.No_Location then
            Put (Output.all, "-<eof>");
         end if;
         Put_Line (Output.all, ", justification:");
         if E.Message /= null then
            Put_Line (Output.all, E.Message.all);
         else
            Put_Line (Output.all, "No justification");
         end if;
         Total_Disabled_Cov_Regions := Total_Disabled_Cov_Regions + 1;
      end Output_Disable_Cov;

      --------------------------
      -- Messages_For_Section --
      --------------------------

      procedure Messages_For_Section
        (MC    : Report_Section;
         Title : String;
         Item  : String)
      is
         procedure Output_Message (C : Message_Vectors.Cursor);
         --  Display the SCO violations messages stored at C

         Item_Count : Natural := 0;
         --  Count of the number of violation / error messages for the current
         --  section.

         Msg_Count  : Natural := 0;
         --  Count of the number of messages (including info messages) for the
         --  current section.

         --------------------
         -- Output_Message --
         --------------------

         procedure Output_Message (C : Message_Vectors.Cursor) is
            M            : Message renames Message_Vectors.Element (C);
            Msg          : constant String := +M.Msg;
            First        : Natural := Msg'First;
         begin
            if M.Kind /= Info and then M.SCO /= No_SCO_Id then
               declare
                  Show_Vectors : constant Boolean :=
                    (Switches.Show_MCDC_Vectors
                     and then not Is_Assertion_To_Cover (M.SCO))
                    or else Switches.Show_Condition_Vectors;
               begin
                  Put
                    (Output.all,
                     Image (First_Sloc (M.SCO), Unique_Name => True));
                  Put (Output.all, ": ");
                  if Msg (First) = '^' then
                     First := First + 1;
                  else
                     Put
                       (Output.all,
                        SCO_Kind_Image (M.SCO)
                        & (if Show_Vectors and then Kind (M.SCO) = Condition
                           then Index (M.SCO)'Image
                                & " (" & SCO_Image (M.SCO) & ") "
                           else " "));
                  end if;
               end;

            elsif M.SCO /= No_SCO_Id then

               --  For info messages (such as the messages displayed with
               --  --show-mcdc-vectors and --show-condition vectors), do not
               --  display the SCO, as it is only used to attach the message to
               --  the right report location.

               Put (Output.all, Image
                    ((if SC_Obligations.Kind (M.SCO) = Condition
                       then First_Sloc (Enclosing_Decision (M.SCO))
                       else First_Sloc (M.SCO)),
                       Unique_Name => True));
               Put (Output.all, ": ");
            else
               Put (Output.all, Image (M.Sloc) & ": ");
            end if;

            Output_Multiline_Msg
              (Output => Output.all,
               Text   => Msg (First .. Msg'Last));

            New_Line (Output.all);
            if M.SCO /= No_SCO_Id then
               Output_Annotations (Output.all, SCO_Annotations (M.SCO));
            end if;
         end Output_Message;

      --  Start of processing for Messages_For_Section

      begin
         if Title /= "" then
            Pp.Section (Title);
         end if;

         for C in Pp.Nonexempted_Messages (MC).Iterate loop
            Output_Message (C);

            Msg_Count := Msg_Count + 1;

            if Message_Vectors.Element (C).Kind /= Info then
               Item_Count := Item_Count + 1;
            end if;
         end loop;

         if Msg_Count > 0 then
            New_Line (Output.all);
         end if;

         --  Output summary line at end of section

         Put_Line (Output.all, Pluralize (Item_Count, Item) & ".");

         --  Append summary line for general summary chapter

         Pp.Summary.Append
           (+(Pluralize
                (Item_Count,
                   (case MC is
                      when Coverage_Violations =>
                       Non_Exempted
                         & Coverage_Level'Val (MC)'Img & " " & Item,
                      when Other_Errors        =>
                        "other message",
                      when Coverage_Exclusions  =>
                        "coverage exclusion",
                      when Undet_Coverage =>
                        "undetermined coverage item")) & "."));

         --  Count of total (coverable) and covered SCOs is displayed only
         --  if --all-messages is specified.

         if Switches.All_Messages and then MC in Coverage_Violations then
            declare
               T : SCO_Tally renames Pp.SCO_Tallies (Coverage_Level'Val (MC));
            begin
               Put_Line (Output.all,
                         Pluralize (T.Stats (Covered), "coverage obligation")
                         & " covered out of" & T.Total'Img & ".");
            end;
         end if;
      end Messages_For_Section;

   --  Start of processing for Pretty_Print_End

   begin
      if Source_Coverage_Enabled then
         SC_Obligations.Iterate (Count_SCO'Access);
      end if;

      Pp.Chapter (To_Upper (Non_Exempted) & "COVERAGE VIOLATIONS");

      for L in Coverage_Level loop
         if Enabled (L)
           or else (L = Decision and then MCDC_Coverage_Enabled)
           or else (L in ATC | ATCC and then Assertion_Coverage_Enabled)
         then
            Messages_For_Section
              (Coverage_Level'Pos (L),
               Title => L'Img & " COVERAGE",
               Item  => "violation");
         else
            pragma Assert
              (Pp.Nonexempted_Messages (Coverage_Level'Pos (L)).Is_Empty);
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

      --  No need to have the section header and so on if there are no
      --  non-instrumented constructs.

      if not Pp.Nonexempted_Messages (Undet_Coverage).Is_Empty then
         Pp.Chapter ("UNDETERMINED COVERAGE ITEMS");
         New_Line (Output.all);
         Messages_For_Section
           (Undet_Coverage,
            Title => "",
            Item  => "undetermined coverage items");
      end if;

      if Has_Exempted_Region then
         Pp.Chapter ("EXEMPTED REGIONS");
         Total_Exempted_Regions := 0;
         ALI_Annotations.Iterate (Output_Exemption'Access);

         New_Line (Output.all);
         Put_Line
           (Output.all,
            Pluralize (Total_Exempted_Regions, "exempted region")
            & ", "
            & Pluralize (Total_Exempted_Violations, "exempted violation")
            & ".");
      end if;

      if Has_Disabled_Cov_Region then
         Pp.Chapter ("COVERAGE DISABLED REGIONS");
         Total_Disabled_Cov_Regions := 0;
         ALI_Annotations.Iterate (Output_Disable_Cov'Access);

         New_Line (Output.all);
         Put_Line
           (Output.all,
            Pluralize (Total_Disabled_Cov_Regions, "region")
            & " with disabled coverage.");
      end if;

      Pp.Chapter ("ANALYSIS SUMMARY");

      New_Line (Output.all);

      for L of Pp.Summary loop
         Put_Line (Output.all, +L);
      end loop;

      if Has_Exempted_Region then
         Put_Line
           (Output.all,
            Pluralize (Total_Exempted_Regions, "exempted region")
            & ", "
            & Pluralize (Total_Exempted_Violations, "exempted violation")
            & ".");
      end if;

      if Has_Disabled_Cov_Region then
         New_Line (Output.all);
         Put_Line
           (Output.all,
            Pluralize (Total_Disabled_Cov_Regions, "region")
            & " with disabled coverage.");
      end if;

      if Pp.Dump_Units then
         Pp.Chapter ("UNITS OF INTEREST");
         New_Line (Output.all);
         Report_Units (Output.all);
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
      Pp.Disabled_Cov := Info.Disabled_Cov;
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

         if M.Kind in Violation | Undetermined_Cov
           and then Pp.Exemption /= Slocs.No_Location
         then

            declare
               use Messages_Of_Exempted_Region;

               procedure Add_Message
                 (Key     : Source_Location;
                  Element : in out Message_Vectors.Vector);
               --  Append a message to the message vector of Key

               procedure Add_Message
                 (Key     : Source_Location;
                  Element : in out Message_Vectors.Vector)
               is
                  pragma Unreferenced (Key);
               begin
                  Element.Append (M);
               end Add_Message;

               C        : Cursor := Pp.Exempted_Messages.Find (Pp.Exemption);
               Inserted : Boolean;
            begin
               if not Has_Element (C) then
                  Pp.Exempted_Messages.Insert
                    (Pp.Exemption, Message_Vectors.Empty_Vector, C, Inserted);
               end if;

               Pp.Exempted_Messages.Update_Element (C, Add_Message'Access);
            end;

            if M.Kind = Violation then
               Inc_Violation_Exemption_Count (Pp.Exemption);
            else
               Inc_Undet_Cov_Exemption_Count (Pp.Exemption);
            end if;
         else
            Pp.Nonexempted_Messages (MC).Append (M);
         end if;
      end if;
   end Pretty_Print_Message;

   ------------------------
   -- Pretty_Print_Start --
   ------------------------

   procedure Pretty_Print_Start (Pp : in out Report_Pretty_Printer) is
      Output : constant File_Access := Get_Output;

      procedure Process_One_Trace (TF : Trace_File_Element);
      --  Print info from the TF trace file

      -----------------------
      -- Process_One_Trace --
      -----------------------

      procedure Process_One_Trace (TF : Trace_File_Element) is
         Orig_Context : constant String := Original_Processing_Context (TF);
      begin
         New_Line (Output.all);
         Put_Line (Output.all, +TF.Filename);
         Put_Line (Output.all, "  kind     : " & Image (TF.Kind));
         Put_Line (Output.all, "  program  : " & (+TF.Program_Name));
         Put_Line (Output.all, "  date     : " & (+TF.Time));
         Put_Line (Output.all, "  tag      : " & (+TF.User_Data));

         --  For a trace that has been processed in an earlier run, provide
         --  information on original coverage assessment context.

         if Orig_Context /= "" then
            Put_Line (Output.all, "  processed: " & Orig_Context);
         end if;
      end Process_One_Trace;

   --  Start of processing for Pretty_Print_Start

   begin
      Put_Line (Output.all, Highlight ("COVERAGE REPORT"));

      Pp.Chapter ("ASSESSMENT CONTEXT");

      New_Line (Output.all);
      Put_Line (Output.all, "Date and time of execution: "
                & Image (Pp.Context.Timestamp));
      Put_Line (Output.all, "Tool version: XCOV "
                & (+Pp.Context.Version));
      New_Line (Output.all);

      Put_Line (Output.all, "Command line:");
      Put_Line (Output.all, +Pp.Context.Command);
      New_Line (Output.all);

      Put_Line (Output.all, "Coverage level: " & (+Pp.Context.Levels));
      New_Line (Output.all);

      Put_Line (Output.all, "Trace files:");
      Iterate_On_Traces_Files (Process_One_Trace'Access);
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
      if Info.Li_Stats (Covered) /= Get_Total (Info.Li_Stats)
        or else (Pp.Show_Details and then Info.Li_Stats (Not_Coverable) /= 0)
      then

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

      New_Line (Output.all);
      Put_Line (Output.all,
                Underline (Img (Pp.Current_Chapter) & "."
                           & Img (Pp.Current_Section) & ". "
                           & Title));
      New_Line (Output.all);
   end Section;

   -----------------
   --  Underline  --
   -----------------

   function Underline (S : String; C : Character := '-') return String is
      Line : constant String (1 .. S'Length) := (others => C);
   begin
      return S & ASCII.LF & Line;
   end Underline;

end Annotations.Report;
