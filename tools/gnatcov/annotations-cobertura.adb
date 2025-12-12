------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

with Ada.Calendar.Conversions; use Ada.Calendar.Conversions;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;

with Interfaces.C; use Interfaces.C;

with Annotations.Xml; use Annotations.Xml;
with Command_Line;    use Command_Line;
with Coverage;        use Coverage;
with Coverage.Source; use Coverage.Source;
with Outputs;         use Outputs;
with Paths;
with Support_Files;
with Switches;        use Switches;
with Version;

package body Annotations.Cobertura is

   DTD_Basename : constant String := "cobertura.dtd";
   DTD_Filename : constant String := Support_Files.In_Lib_Dir (DTD_Basename);

   type Pattern_Matcher_Acc is access all Pattern_Matcher;
   procedure Free is new
     Ada.Unchecked_Deallocation (Pattern_Matcher, Pattern_Matcher_Acc);

   type Cobertura_Pretty_Printer is new Pretty_Printer with record
      Report_Filename : Unbounded_String;
      Report_File     : File_Type;
      --  "cobertura.xml" file

      Indentation : Natural := 0;
      --  Indentation for the next XML items to write

      Source_Prefix_Pattern : Pattern_Matcher_Acc;
      --  Regexp matching a source filename prefix to remove in the report.
      --
      --  This is needed to support coverage reports in GitLab pipelines, which
      --  require filenames to be relative to the root of the tested
      --  repository. GNATcoverage users are supposed to pass the root of this
      --  repository through the --source-root option.
   end record;
   --  Pretty printer type for the Cobertura annotation format

   overriding
   function Format
     (Pp : Cobertura_Pretty_Printer) return Annotation_Format_Family
   is (Annotate_Cobertura);

   type Rate_Type is digits 3;

   function Img (R : Rate_Type) return String;
   --  Return a string representation of the rate without an exponent suffix
   --  and without a leading space.

   --------------------
   -- XML generation --
   --------------------

   Xml_Header : constant String := "<?xml version=""1.0"" ?>";

   function A (Name : String; Value : String) return String;
   --  Return a string representing an xml attribute whose name and value are
   --  given in parameter, i.e. Name = "Value".

   procedure T
     (Pp         : in out Cobertura_Pretty_Printer'Class;
      Name       : String;
      Attributes : String);
   --  Print a string representing an empty tag whose name and attributes are
   --  given in parameter, i.e. <Name Attributes/>.

   procedure ST
     (Pp         : in out Cobertura_Pretty_Printer'Class;
      Name       : String;
      Attributes : String);
   --  Print a string representing a start tag whose name and attributes are
   --  given in parameter, i.e. <Name Attributes>.

   procedure ST (Pp : in out Cobertura_Pretty_Printer'Class; Name : String);
   --  Same as ST, with no attributes

   procedure ET (Pp : in out Cobertura_Pretty_Printer'Class; Name : String);
   --  Print a string representing an end tag whose name is given in parameter,
   --  i.e. </Name>.

   -----------------------------------------------------
   -- Cobertura_Pretty_Printer's primitive operations --
   --    (inherited from Pretty_Printer)              --
   -----------------------------------------------------

   overriding
   procedure Pretty_Print_Start (Pp : in out Cobertura_Pretty_Printer);

   overriding
   procedure Pretty_Print_End (Pp : in out Cobertura_Pretty_Printer);

   overriding
   procedure Pretty_Print_Start_File
     (Pp   : in out Cobertura_Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean);

   overriding
   procedure Pretty_Print_End_File (Pp : in out Cobertura_Pretty_Printer);

   overriding
   procedure Pretty_Print_Start_Line
     (Pp       : in out Cobertura_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String);

   -----------------------------
   -- Shortcut for Put_Line's --
   -----------------------------

   procedure P (Pp : Cobertura_Pretty_Printer'Class; S : String);
   --  Put_Line S in the destination file

   -----------------------------
   -- Statistics computations --
   -----------------------------

   procedure Compute_Line_Stats
     (Li_Stat       : Li_Stat_Array;
      Lines_Valid   : out Natural;
      Lines_Covered : out Natural;
      Line_Rate     : out Rate_Type);
   --  Compute line statistics. This does not account for exempted / non-
   --  instrumented / non-coverable lines.

   procedure Compute_Branch_Stats
     (Ob_Stats         : Ob_Stat_Array;
      Branches_Valid   : out Natural;
      Branches_Covered : out Natural;
      Branch_Rate      : out Rate_Type);
   --  Compute branch statistics. This does not account exempted / non-
   --  instrumented / non-coverable obligations.
   --
   --  We choose to represent decisions in the branch terminology that
   --  cobertura uses: a decision yields two branches. If it is not covered,
   --  neither branch is covered; if it is partially covered, one of the branch
   --  is; if it is fully covered, the two branches are.

   ---------------
   -- Installed --
   ---------------

   function Installed return Boolean is
   begin
      return Ada.Directories.Exists (DTD_Filename);
   end Installed;

   ---------
   -- Img --
   ---------

   function Img (R : Rate_Type) return String is
      package Float_IO is new Ada.Text_IO.Float_IO (Rate_Type);
      Result : String (1 .. 8);
   begin
      Float_IO.Put
        (To => Result, Item => R, Aft => Rate_Type'Digits - 1, Exp => 0);
      return Ada.Strings.Fixed.Trim (Result, Side => Ada.Strings.Both);
   end Img;

   -------
   -- A --
   -------

   function A (Name : String; Value : String) return String is
   begin
      return " " & Name & "=" & '"' & To_Xml_String (Value) & '"';
   end A;

   --------
   -- ET --
   --------

   procedure ET (Pp : in out Cobertura_Pretty_Printer'Class; Name : String) is
   begin
      Pp.Indentation := Pp.Indentation - 1;
      Pp.P ("</" & Name & ">");
      New_Line (Pp.Report_File);
   end ET;

   ---------------------
   -- Generate_Report --
   ---------------------

   procedure Generate_Report (Context : Coverage.Context_Access) is
      Pp : Cobertura_Pretty_Printer :=
        (Need_Sources          => True,
         Context               => Context,
         Source_Prefix_Pattern => null,
         others                => <>);

      --  If --source-root was passed, turn it into the corresponding prefix
      --  matcher. Append a directory separator so that "/prefix/basename"
      --  always becomes "basename" instead of "/basename" regardless of
      --  whether the prefix is "/prefix" or "/prefix/".

      Source_Root_Opt : Parser.String_Option renames
        Args.String_Args (Opt_Source_Root);
      Output_Opt      : Parser.String_Option renames
        Args.String_Args (Opt_Output);
   begin
      if Source_Root_Opt.Present then
         declare
            Prefix : constant String := +Source_Root_Opt.Value & "/";
         begin
            Pp.Source_Prefix_Pattern :=
              new Pattern_Matcher'(Compile (Paths.Glob_To_Regexp (Prefix)));
         end;
      end if;

      if Output_Opt.Present then
         Pp.Report_Filename := Output_Opt.Value;
      else
         Pp.Report_Filename := +"cobertura.xml";
      end if;

      Pp.Need_Sources := False;

      Annotations.Generate_Report (Pp, True, Subdir => "cobertura");
      Free (Pp.Source_Prefix_Pattern);
   end Generate_Report;

   -------
   -- P --
   -------

   procedure P (Pp : Cobertura_Pretty_Printer'Class; S : String) is
      Spaces : constant String (1 .. Pp.Indentation) := (others => ' ');
   begin
      Put_Line (Pp.Report_File, Spaces & S);
   end P;

   ----------------------
   -- Pretty_Print_End --
   ----------------------

   overriding
   procedure Pretty_Print_End (Pp : in out Cobertura_Pretty_Printer) is
   begin
      Pp.ET ("packages");
      Pp.ET ("coverage");
      Close (Pp.Report_File);
   end Pretty_Print_End;

   ---------------------------
   -- Pretty_Print_End_File --
   ---------------------------

   overriding
   procedure Pretty_Print_End_File (Pp : in out Cobertura_Pretty_Printer) is
   begin
      Pp.ET ("lines");
      Pp.ET ("class");
      Pp.ET ("classes");
      Pp.ET ("package");
   end Pretty_Print_End_File;

   ------------------------
   -- Compute_Line_Stats --
   ------------------------

   procedure Compute_Line_Stats
     (Li_Stat       : Li_Stat_Array;
      Lines_Valid   : out Natural;
      Lines_Covered : out Natural;
      Line_Rate     : out Rate_Type) is
   begin
      Lines_Valid :=
        Li_Stat (Covered)
        + Li_Stat (Not_Covered)
        + Li_Stat (Partially_Covered);
      Lines_Covered := Li_Stat (Covered) + Li_Stat (Partially_Covered);
      Line_Rate := Rate_Type (Lines_Covered) / Rate_Type (Lines_Valid);
   end Compute_Line_Stats;

   --------------------------
   -- Compute_Branch_Stats --
   --------------------------

   procedure Compute_Branch_Stats
     (Ob_Stats         : Ob_Stat_Array;
      Branches_Valid   : out Natural;
      Branches_Covered : out Natural;
      Branch_Rate      : out Rate_Type) is
   begin
      Branches_Valid :=
        Ob_Stats (Decision).Stats (Covered)
        + Ob_Stats (Decision).Stats (Not_Covered)
        + Ob_Stats (Decision).Stats (Partially_Covered);
      Branches_Covered :=
        2
        * Ob_Stats (Decision).Stats (Covered)
        + Ob_Stats (Decision).Stats (Partially_Covered);
      if Branches_Valid = 0 then
         Branch_Rate := 1.0;
      else
         Branch_Rate :=
           Rate_Type (Branches_Covered) / Rate_Type (Branches_Valid);
      end if;
   end Compute_Branch_Stats;

   ------------------------
   -- Pretty_Print_Start --
   ------------------------

   overriding
   procedure Pretty_Print_Start (Pp : in out Cobertura_Pretty_Printer) is
      Success   : Boolean;
      Timestamp : constant String :=
        Ada.Strings.Fixed.Trim
          (long_long'Image (To_Unix_Time_64 (Pp.Context.Timestamp)),
           Ada.Strings.Left);
   begin
      --  Copy the DTD Schema to the output directory

      GNAT.OS_Lib.Copy_File
        (Name     => DTD_Filename,
         Pathname => Get_Output_Dir,
         Success  => Success,
         Mode     => GNAT.OS_Lib.Overwrite);
      if not Success then
         Fatal_Error
           ("Error while copying "
            & DTD_Filename
            & " to the output directory");
      end if;

      Create_Output_File (Pp.Report_File, +Pp.Report_Filename);

      Pp.P (Xml_Header);
      Pp.P ("<!DOCTYPE coverage SYSTEM ""cobertura.dtd"">");

      --  Compute statistics

      declare
         Lines_Valid, Lines_Covered : Natural;
         Line_Rate                  : Rate_Type;

         Branches_Valid, Branches_Covered : Natural;
         Branch_Rate                      : Rate_Type;
      begin
         Compute_Line_Stats
           (Global_Stats, Lines_Valid, Lines_Covered, Line_Rate);
         Compute_Branch_Stats
           (Global_Ob_Stats, Branches_Valid, Branches_Covered, Branch_Rate);

         Pp.ST
           ("coverage",
            A ("line-rate", Img (Line_Rate))
            & A ("branch-rate", Img (Branch_Rate))
            & A ("lines-covered", Img (Lines_Covered))
            & A ("lines-valid", Img (Lines_Valid))
            & A ("branches-covered", Img (Branches_Covered))
            & A ("branches-valid", Img (Branches_Valid))

            --  Cobertura also provides the cyclomatic complexity number.
            --  As we don't compute this, print an arbitrary invalid value.

            & A ("complexity", "-1")
            & A ("version", Version.Xcov_Version)
            & A ("timestamp", Timestamp));

         Pp.ST ("sources");
         Pp.ET ("sources");
         Pp.ST ("packages");
      end;
   end Pretty_Print_Start;

   -----------------------------
   -- Pretty_Print_Start_File --
   -----------------------------

   overriding
   procedure Pretty_Print_Start_File
     (Pp   : in out Cobertura_Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean)
   is
      Info : constant File_Info_Access := Get_File (File);

      Simple_Source_Filename : constant String := Info.Simple_Name.all;

      Lines_Valid, Lines_Covered : Natural;
      Line_Rate                  : Rate_Type;

      Branches_Valid, Branches_Covered : Natural;
      Branch_Rate                      : Rate_Type;

      Filename : Unbounded_String := +Info.Full_Name.all;
      --  Filename to mention in the coverage report. Use the full name, unless
      --  we can remove the prefix according to the --source-root option.
   begin
      Skip := False;

      --  Compute line and branch statistics

      Compute_Line_Stats
        (Info.Li_Stats, Lines_Valid, Lines_Covered, Line_Rate);
      Compute_Branch_Stats
        (Info.Ob_Stats, Branches_Valid, Branches_Covered, Branch_Rate);

      --  Remove the source root prefix (if present) from Filename so that, if
      --  Filename designates a source file that is inside the directory
      --  referenced by --source-root, Filename is set to a pathname relative
      --  to the source root.
      --
      --  Note that the pattern matcher in Pp.Source_Prefix_Pattern expects
      --  "normalized" filename with:
      --
      --  1) casing folded for case insensitive systems,
      --  2) forward slashes instead of backslashes.
      --
      --  So normalize first into Normalized_Filename, then turn Filename into
      --  a slice of it.

      if Pp.Source_Prefix_Pattern /= null then
         declare
            Matches             : GNAT.Regpat.Match_Array (0 .. 0);
            First, Last         : Natural;
            Normalized_Filename : constant String :=
              Paths.Normalize_For_Regexp (+Filename);
         begin
            GNAT.Regpat.Match
              (Pp.Source_Prefix_Pattern.all, Normalized_Filename, Matches);
            if Matches (0) /= GNAT.Regpat.No_Match then
               First := Matches (0).Last + 1;
               Last := Normalized_Filename'Last;
               Filename := +Normalized_Filename (First .. Last);
            end if;
         end;
      end if;

      Pp.ST
        ("package",
         A ("name", Simple_Source_Filename)
         & A ("line-rate", Img (Line_Rate))
         & A ("branch-rate", Img (Branch_Rate))
         & A ("complexity", "-1"));
      Pp.ST ("classes");
      Pp.ST
        ("class",
         A ("name", Simple_Source_Filename)
         & A ("filename", +Filename)
         & A ("line-rate", Img (Line_Rate))
         & A ("branch-rate", Img (Branch_Rate))
         & A ("complexity", "-1"));
      Pp.ST ("methods");
      Pp.ET ("methods");
      Pp.ST ("lines");
   end Pretty_Print_Start_File;

   -----------------------------
   -- Pretty_Print_Start_Line --
   -----------------------------

   overriding
   procedure Pretty_Print_Start_Line
     (Pp       : in out Cobertura_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String)
   is
      Coverage_State : constant Any_Line_State := Aggregated_State (Info.all);
      Exempted       : constant Boolean := Info.Exemption /= Slocs.No_Location;
   begin
      if not Exempted and then Coverage_State /= No_Code then
         declare
            subtype Coverage_Line_State is
              Any_Line_State range Not_Covered .. Covered;
            package SCOs_State_Maps is new
              Ada.Containers.Ordered_Maps
                (Key_Type     => SCO_Id,
                 Element_Type => Coverage_Line_State);

            Hit : Natural := 0;
            --  Whether the line was covered or not. Cobertura enables counting
            --  the number of coverage hits; we don't.

            Has_Decision : Boolean := False;
            --  Whether there is a decision on the current line

            State_Decisions : SCOs_State_Maps.Map;
            --  List of decisions at the current line and their coverage state

            Branches_Covered, Branches_Valid : Natural := 0;
         begin
            if Coverage_State /= Not_Covered then
               Hit := 1;
            end if;

            --  Express decision coverage in cobertura notion of condition
            --  coverage: in Cobertura terminology, a condition is represented
            --  as a two-valuation expression: either 0, 1 or 2 of its
            --  valuation are covered.

            --  Start by getting all of the decisions present on this line.
            --  Note that there can be no SCOs for the line if using binary
            --  traces with the insn coverage level. TODO??? Is this really
            --  something we want to support?

            if Info.SCOs /= null then
               for SCO of Info.SCOs.all loop
                  if Kind (SCO) /= Removed
                    and then First_Sloc (SCO).L.Line = Line_Num
                    and then SCO_Kind (Kind (SCO)) = Decision
                    and then
                      (Coverage.Enabled (Decision)
                       or else Coverage.MCDC_Coverage_Enabled)
                  then
                     declare
                        Line_State : constant Any_Line_State :=
                          Get_Line_State (SCO, Decision);
                     begin
                        if Line_State in Coverage_Line_State then
                           Has_Decision := True;
                           State_Decisions.Insert (SCO, Line_State);
                        end if;
                     end;
                  end if;
               end loop;
            end if;

            --  Compute the decision coverage rate if any

            if not State_Decisions.Is_Empty then
               for State_Decision of State_Decisions loop
                  Branches_Valid := Branches_Valid + 2;
                  if State_Decision = Covered then
                     Branches_Covered := Branches_Covered + 2;
                  elsif State_Decision = Partially_Covered then
                     Branches_Covered := Branches_Covered + 1;
                  end if;
               end loop;
            end if;

            --  Emit the xml line node. Here is an example:
            --
            --  <line number="52" hits="0" branch="true"
            --        condition-coverage="0% (0/2)">

            declare
               Line_Attributes : Unbounded_String;
            begin
               Append (Line_Attributes, A ("number", Img (Line_Num)));
               Append (Line_Attributes, A ("hits", Img (Hit)));
               if Has_Decision then
                  Append (Line_Attributes, A ("branch", "true"));
                  Append
                    (Line_Attributes,
                     A
                       ("condition-coverage",
                        Img (Ratio (Branches_Covered, Branches_Valid))
                        & "%"
                        & "("
                        & Img (Branches_Covered)
                        & "/"
                        & Img (Branches_Valid)
                        & ")"));
               else
                  Append (Line_Attributes, A ("branch", "false"));
               end if;
               Pp.ST ("line", +Line_Attributes);

               Pp.ST ("conditions");
               declare
                  I              : Integer := 0;
                  Coverage_Ratio : Integer;
               begin
                  for State_Decision of State_Decisions loop
                     Coverage_Ratio :=
                       (case State_Decision is
                          when Not_Covered       => 0,
                          when Partially_Covered => 50,
                          when Covered           => 100);
                     Pp.T
                       ("condition",
                        A ("number", Img (I))
                        & A ("type", "jump")
                        & A ("coverage", Img (Coverage_Ratio) & "%"));
                     I := I + 1;
                  end loop;
               end;
               Pp.ET ("conditions");

               Pp.ET ("line");
            end;
         end;
      end if;
   end Pretty_Print_Start_Line;

   -------
   -- T --
   -------

   procedure T
     (Pp         : in out Cobertura_Pretty_Printer'Class;
      Name       : String;
      Attributes : String) is
   begin
      Pp.P ("<" & Name & Attributes & "/>");
   end T;

   --------
   -- ST --
   --------

   procedure ST
     (Pp         : in out Cobertura_Pretty_Printer'Class;
      Name       : String;
      Attributes : String) is
   begin
      Pp.P ("<" & Name & Attributes & ">");
      Pp.Indentation := Pp.Indentation + 1;
   end ST;

   procedure ST (Pp : in out Cobertura_Pretty_Printer'Class; Name : String) is
   begin
      Pp.P ("<" & Name & ">");
      Pp.Indentation := Pp.Indentation + 1;
   end ST;

end Annotations.Cobertura;
