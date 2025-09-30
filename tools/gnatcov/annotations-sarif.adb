------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
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

with Ada.Characters.Handling;
with Ada.Containers.Vectors;

with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Coverage; use Coverage;
with Outputs;  use Outputs;
with Version;

package body Annotations.Sarif is

   type Result_Info is record
      Is_Violation : Boolean;
      --  True if is a violation, False if undetermined/disabled coverage

      Is_Exempted : Boolean;
      --  True is the violation is in an exempted region

      Rule_Id : Unbounded_String;
      --  Id of the rule the violation violates

      Message : Unbounded_String;
      --  Message describing the cause of the violation

      Location : Unbounded_String;
      --  Full name of the file of the violation

      Region : Local_Source_Location;
      --  Location of the violation within the file
   end record;
   --  Record describing the elements of a result. A result represents a
   --  violation.

   package Result_Info_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Result_Info);
   --  Vector storing the results' information of this run of gnatcov

   type Sarif_Pretty_Printer is new Pretty_Printer with record
      --  Pretty printer type for the SARIF annotation format

      Sarif_Filename : Unbounded_String := +"coverage.sarif";
      Sarif_File     : File_Type;

      Contents : JSON_Value := Create_Object;
      --  JSON value of the whole SARIF report, to be written in the report
      --  file.

      Exemption : Slocs.Source_Location := Slocs.No_Location;
      --  Exemption sloc applying to current line, if any

      Results : Result_Info_Vectors.Vector := Result_Info_Vectors.Empty_Vector;
      --  List of violations to be reported
   end record;
   --  Pretty printer for the SARIF annotation format

   overriding
   function Format (Pp : Sarif_Pretty_Printer) return Annotation_Format_Family
   is (Annotate_Sarif);

   -------------------------------------------------
   -- Sarif_Pretty_Printer's primitive operations --
   --    (inherited from Pretty_Printer)          --
   -------------------------------------------------

   procedure Pretty_Print_End (Pp : in out Sarif_Pretty_Printer);

   procedure Pretty_Print_Start_File
     (Pp   : in out Sarif_Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean);

   procedure Pretty_Print_End_File (Pp : in out Sarif_Pretty_Printer) is null;

   procedure Pretty_Print_Start_Line
     (Pp       : in out Sarif_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String);

   procedure Pretty_Print_Message
     (Pp : in out Sarif_Pretty_Printer; M : Message);

   ----------------------------
   --  SARIF report elements --
   ----------------------------

   type Rule_Info is record
      Id : UTF8_Unbounded_String := Null_Unbounded_String;
      --  Unique "code" for the rule

      Name : UTF8_Unbounded_String := Null_Unbounded_String;
      --  Full name of the rule

      Short_Descr : UTF8_Unbounded_String := Null_Unbounded_String;
      --  Short description of the rule

      Help_Uri : UTF8_Unbounded_String := Null_Unbounded_String;
      --  Link to the documentation webpage describing the rule in detail

      Config : UTF8_Unbounded_String := Null_Unbounded_String;
      --  Default severity level of a violation of this rule
   end record;
   --  Coverage analysis rule representation

   type Rules_Range is
     range Source_Coverage_Level'Pos (Source_Coverage_Level'First)
           .. Source_Coverage_Level'Pos (Source_Coverage_Level'Last) + 1;

   Undet_Rule : constant Rules_Range := Rules_Range'Last;

   type Rule_Info_Arrays is array (Rules_Range) of Rule_Info;
   --  List of rules for currently enabled coverage levels, and one rule for
   --  undetermined coverage.

   Doc_Link : constant String :=
     "https://docs.adacore.com/gnatcoverage-docs/html/";

   Rules : constant Rule_Info_Arrays :=
     (Source_Coverage_Level'Pos (Stmt)     =>
        (Id          => +Stmt'Image,
         Name        => +"Statement Coverage",
         Short_Descr =>
           +"Control flow should reach the statement at least once.",
         Help_Uri    =>
           +(Doc_Link & "gnatcov/cov_source.html#statement-coverage-analysis"),
         Config      => +"error"),
      Source_Coverage_Level'Pos (Decision) =>
        (Id          => +Coverage_Level'Image (Decision),
         Name        => +"Decision Coverage",
         Short_Descr =>
           +("The decision has been evaluated at least once to True"
             & " and once False during program execution"),
         Help_Uri    =>
           +(Doc_Link & "gnatcov/cov_source.html#decision-coverage-analysis"),
         Config      => +"error"),
      Source_Coverage_Level'Pos (MCDC)     =>
        (Id          => +MCDC'Image,
         Name        => +"Modified Condition/Decision Coverage",
         Short_Descr =>
           +("The independant effect of the conditions on the"
             & " enclosing decision was demonstrated by the tests"),
         Help_Uri    =>
           +(Doc_Link
             & "gnatcov/cov_source.html#modified-condition-decision-coverage"
             & "-analysis"),
         Config      => +"error"),
      Source_Coverage_Level'Pos (UC_MCDC)  =>
        (Id          => +UC_MCDC'Image,
         Name        => +"Unique Cause Modified Condition/Decision Coverage",
         Short_Descr =>
           +("The independent influence of a specific condition"
             & " must be demonstrated by a pair of tests where only that"
             & " condition changes and the decision value toggles."),
         Help_Uri    => +(Doc_Link & "gnatcov/cov_source.html#mcdc-variants"),
         Config      => +"error"),
      Source_Coverage_Level'Pos (ATC)      =>
        (Id          => +ATC'Image,
         Name        => +"Assertion True Coverage",
         Short_Descr =>
           +"Control flow should reach the assertion at least once",
         Help_Uri    =>
           +(Doc_Link
             & "gnatcov/cov_source.html#assertion-true-coverage-atc-analysis"),
         Config      => +"error"),
      Source_Coverage_Level'Pos (ATCC)     =>
        (Id          => +ATCC'Image,
         Name        => +"Assertion True Condition Coverage",
         Short_Descr =>
           +("The assertion decision has been evaluated at least once"
             & " to True and once False during program execution"),
         Help_Uri    =>
           +(Doc_Link
             & "gnatcov/cov_source.html#assertion-true-condition-coverage-"
             & "analysis-atcc"),
         Config      => +"error"),
      Source_Coverage_Level'Pos (Fun_Call) =>
        (Id          => +Fun_Call'Image,
         Name        => +"Function and Call coverage",
         Short_Descr =>
           (+"The subprogram was entered at least once or the call"
            & " was executed at least once"),
         Help_Uri    =>
           +Doc_Link
           & "gnatcov/cov_source.html#function-and-call-coverage-fun-call-"
           & "analysis-experimental",
         Config      => +"error"),
      Source_Coverage_Level'Pos (GExpr)    =>
        (Id          => +GExpr'Image,
         Name        => +"Guarded Expression coverage",
         Short_Descr =>
           (+"The expression belongs to a conditional expression structure"
            & " and was executed at least once"),
         Help_Uri    => +Doc_Link & "gnatcov/cov_source.html#foobar",
         --  FIXME(dprn): valid URL
         Config      => +"error"),
      Undet_Rule                           =>
        (Id          => +"UNDET",
         Name        => +"Undetermined Coverage",
         Short_Descr =>
           +("Gnatcov was unable to determine the coverage state"
             & " of the obligation"),
         Help_Uri    => Null_Unbounded_String,
         Config      => +"warning"));

   function Create_Rules return JSON_Array;
   --  Return the SARIF rules for the enabled coverage levels

   function Create_Tool return JSON_Value;
   --  Return the SARIF tool informations

   function Create_Results
     (Results : Result_Info_Vectors.Vector) return JSON_Array;
   --  Return the JSON array of SARIF results representing each of the
   --  violations found.

   ------------------
   -- Create_Rules --
   ------------------

   function Create_Rules return JSON_Array is
      function Create_Rule (R : Rule_Info) return JSON_Value;
      --  Create single rule as a JSON object

      function Create_Rule (R : Rule_Info) return JSON_Value is
         Rule                  : constant JSON_Value := Create_Object;
         Short_Description     : constant JSON_Value := Create_Object;
         Default_Configuration : constant JSON_Value := Create_Object;

      begin
         Rule.Set_Field ("id", R.Id);
         Rule.Set_Field ("name", R.Name);

         Short_Description.Set_Field ("text", R.Short_Descr);
         Rule.Set_Field ("shortDescription", Short_Description);

         if R.Help_Uri /= Null_Unbounded_String then
            Rule.Set_Field ("helpUri", R.Help_Uri);
         end if;

         Default_Configuration.Set_Field ("level", R.Config);
         Rule.Set_Field ("defaultConfiguration", Default_Configuration);

         return Rule;
      end Create_Rule;

      Rules_Arr : JSON_Array := Empty_Array;
   begin
      for L of Coverage_Levels_Enabled loop
         Append
           (Rules_Arr, Create_Rule (Rules (Source_Coverage_Level'Pos (L))));
      end loop;

      Append (Rules_Arr, Create_Rule (Rules (Undet_Rule)));

      return Rules_Arr;
   end Create_Rules;

   -----------------
   -- Create_Tool --
   -----------------

   function Create_Tool return JSON_Value is
      function Create_Driver return JSON_Value;

      function Create_Driver return JSON_Value is
         Driver : constant JSON_Value := Create_Object;
      begin
         Driver.Set_Field ("name", "gnatcov");
         Driver.Set_Field ("version", Standard.Version.Xcov_Version);
         Driver.Set_Field ("informationUri", Doc_Link & "gnatcov.html");
         Driver.Set_Field ("rules", Create_Rules);

         return Driver;
      end Create_Driver;

      Tool : constant JSON_Value := Create_Object;
   begin
      Tool.Set_Field ("driver", Create_Driver);
      return Tool;
   end Create_Tool;

   --------------------
   -- Create_Results --
   --------------------

   function Create_Results
     (Results : Result_Info_Vectors.Vector) return JSON_Array
   is
      function Create_Result (R : Result_Info) return JSON_Value;
      --  Return a single SARIF results representing one violations

      -------------------
      -- Create_Result --
      -------------------

      function Create_Result (R : Result_Info) return JSON_Value is
         Res, Msg_Text, Region, Physical_Location, Uri, Locations :
           constant JSON_Value := Create_Object;

         Locations_Arr : JSON_Array := Empty_Array;

      begin
         --  The level of a result can be one of "none", "note", "warning",
         --  "error". We set it according to the following:
         --
         --  * error   : a non-exempted violation
         --  * warning : undetermined coverage
         --  * note    : an exempted violation

         Res.Set_Field ("ruleId", R.Rule_Id);
         Res.Set_Field ("kind", "fail");

         Res.Set_Field
           ("level",
            (if R.Is_Exempted
             then "note"
             else (if R.Is_Violation then "error" else "warning")));

         Msg_Text.Set_Field ("text", R.Message);
         Res.Set_Field ("message", Msg_Text);

         Region.Set_Field ("startLine", R.Region.Line);
         Region.Set_Field ("startColumn", R.Region.Column);

         Uri.Set_Field ("uri", R.Location);
         Physical_Location.Set_Field ("artifactLocation", Uri);
         Physical_Location.Set_Field ("region", Region);

         Locations.Set_Field ("physicalLocation", Physical_Location);
         Append (Locations_Arr, Locations);
         Res.Set_Field ("locations", Locations_Arr);

         return Res;
      end Create_Result;

      Results_Arr : JSON_Array := Empty_Array;
   begin
      for R of Results loop
         Append (Results_Arr, Create_Result (R));
      end loop;

      return Results_Arr;
   end Create_Results;

   -----------------------------
   -- Pretty_Print_Start_File --
   -----------------------------

   procedure Pretty_Print_Start_File
     (Pp   : in out Sarif_Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean)
   is
      pragma Unreferenced (Pp);

      Info : constant File_Info_Access := Get_File (File);
   begin
      if Info.Li_Stats (Covered) /= Get_Total (Info.Li_Stats) then

         --  Some uncovered or partially covered lines are present

         Skip := False;

      else
         --  Everything covered: nothing to report for this file

         Skip := True;
      end if;
   end Pretty_Print_Start_File;

   --------------------------
   -- Pretty_Print_Message --
   --------------------------

   procedure Pretty_Print_Message
     (Pp : in out Sarif_Pretty_Printer; M : Message) is
   begin
      if M.Kind > Notice then
         declare
            Rep_Section : constant Report_Section := Section_Of_Message (M);

            --  Rep_Section can directly correspond to a source coverage level,
            --  or be Coverage_Exclusions, Undet_Coverage or Other_Errors.
            --  The message is only added to the SARIF report when Rep_Section
            --  is linked to a source coverage level.

            Rep_Section_Pos   : constant Integer :=
              Report_Section'Pos (Rep_Section);
            First_Level_Pos   : constant Integer :=
              Source_Coverage_Level'Pos (Source_Coverage_Level'First);
            Last_Level_Pos    : constant Integer :=
              Source_Coverage_Level'Pos (Source_Coverage_Level'Last);
            Is_Coverage_Level : constant Boolean :=
              First_Level_Pos <= Rep_Section_Pos
              and then Rep_Section_Pos <= Last_Level_Pos;
         begin
            if (Is_Coverage_Level and then M.Kind in Violation)
              or else M.Kind = Undetermined_Cov
            then
               Pp.Results.Append
                 ((Is_Violation => M.Kind = Violation,
                   Is_Exempted  => Pp.Exemption /= Slocs.No_Location,
                   Rule_Id      =>
                     (if M.Kind = Undetermined_Cov
                      then Rules (Undet_Rule).Id
                      else
                        Rules
                          (Source_Coverage_Level'Pos
                             (Coverage_Level'Val (Rep_Section)))
                          .Id),
                   Message      =>
                     Ada.Characters.Handling.To_Lower
                       (SCO_Kind'Image (Kind (M.SCO)))
                     & (+" ")
                     & M.Msg,
                   Location     =>
                     +(Get_File (M.Sloc.Source_File).Full_Name.all),
                   Region       => First_Sloc (M.SCO).L));
            end if;
         end;
      end if;
   end Pretty_Print_Message;

   -----------------------------
   -- Pretty_Print_Start_Line --
   -----------------------------

   procedure Pretty_Print_Start_Line
     (Pp       : in out Sarif_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String)
   is
      pragma Unreferenced (Line_Num, Line);
   begin
      Pp.Exemption := Info.Exemption;
   end Pretty_Print_Start_Line;

   ----------------------
   -- Pretty_Print_End --
   ----------------------

   procedure Pretty_Print_End (Pp : in out Sarif_Pretty_Printer) is
      function Create_Run return JSON_Value;

      function Create_Run return JSON_Value is
         Run : constant JSON_Value := Create_Object;
      begin
         Run.Set_Field ("tool", Create_Tool);
         Run.Set_Field ("results", Create_Results (Pp.Results));

         return Run;
      end Create_Run;
      --  A run represents a single invocation of a single analysis tool, and
      --  the run has to describe the tool that produced it.

      Run_Arr : JSON_Array := Empty_Array;

      Schema_Link : constant String :=
        "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0.json";
   begin
      Pp.Contents.Set_Field ("$schema", Schema_Link);
      Pp.Contents.Set_Field ("version", "2.1.0");

      Append (Run_Arr, Create_Run);
      Pp.Contents.Set_Field ("runs", Run_Arr);

      Put_Line (Pp.Sarif_File, Pp.Contents.Write);
   end Pretty_Print_End;

   ---------------------
   -- Generate_Report --
   ---------------------

   procedure Generate_Report (Context : Coverage.Context_Access) is
      Pp : Sarif_Pretty_Printer :=
        (Need_Sources => True, Context => Context, others => <>);
   begin
      Create_Output_File (Pp.Sarif_File, +Pp.Sarif_Filename);

      Annotations.Generate_Report (Pp, True, Subdir => ("sarif"));

      Close (Pp.Sarif_File);
   end Generate_Report;

end Annotations.Sarif;
