------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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
with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Hex_Images;  use Hex_Images;
with Traces_Disa; use Traces_Disa;
with Traces_Files_List;
with Qemu_Traces;
with Coverage;    use Coverage;
with Outputs;     use Outputs;
with Annotations.Xml; use Annotations.Xml;

package body Annotations.Html is
   type String_Cst_Acc is access constant String;
   subtype S is String;

   type Strings_Arr is array (Natural range <>) of String_Cst_Acc;

   procedure Put (F : File_Type; Strings : Strings_Arr);
   --  F being a handle to an opened file, append the content of Strings to
   --  it, printing one element per line.
   --  ??? This is only used to generate xcov.css' content. In other places,
   --  lists of Put/Put_Line are used instead. Not sure I understand what is
   --  the benefit of the Strings_Arr approach, but I feel that we are using
   --  two different ways of doing the same thing; if we have no good reason to
   --  do so, we should remove one of them. If there's a good reason to keep
   --  the Strings_Arr approach, we should move this out of this package, as it
   --  is quite independant from generating HTML annotations.

   type Html_Pretty_Printer is new Pretty_Printer with record
      --  Pretty printer type for the HTML annotation format

      Html_File : Ada.Text_IO.File_Type;
      --  When going through the source file list, handle to the html file
      --  that corresponds to the source file being processed.
      --  e.g. hello.adb.html for hello.adb.

      Index_File : Ada.Text_IO.File_Type;
      --  Handle to the HTML index

      Show_Line_Details : Boolean := False;
      --  When going through the line table of a source file, this
      --  records whether justifications of the current line state
      --  should be shown
   end record;

   ------------------------------------------------
   -- Html_Pretty_Printer's primitive operations --
   --    (inherited from Pretty_Printer)         --
   ------------------------------------------------

   procedure Pretty_Print_Start (Pp : in out Html_Pretty_Printer);

   procedure Pretty_Print_End (Pp : in out Html_Pretty_Printer);

   procedure Pretty_Print_Start_File
     (Pp   : in out Html_Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean);

   procedure Pretty_Print_Start_Line
     (Pp       : in out Html_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String);

   procedure Pretty_Print_End_Line
     (Pp : in out Html_Pretty_Printer);

   procedure Pretty_Print_Start_Symbol
     (Pp     : in out Html_Pretty_Printer;
      Name   : String;
      Offset : Pc_Type;
      State  : Line_State);

   procedure Pretty_Print_Insn
     (Pp       : in out Html_Pretty_Printer;
      Pc       : Pc_Type;
      State    : Insn_State;
      Insn     : Binary_Content;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class);

   procedure Pretty_Print_Message
     (Pp    : in out Html_Pretty_Printer;
      M     : Message);

   procedure Pretty_Print_End_File (Pp : in out Html_Pretty_Printer);

   ------------------------------------
   -- Other pretty printer utilities --
   ------------------------------------

   procedure Plh (Pp : in out Html_Pretty_Printer'Class; Str : String);
   --  Print Str in Pp's current html file; new line at the end

   procedure Wrh (Pp : in out Html_Pretty_Printer'Class; Str : String);
   --  Put Str in Pp's current html file; no new line at the end

   ---------------------------
   -- General HTML handling --
   ---------------------------

   procedure Generate_Css_File;
   --  Generate the style sheet for the HTML reports

   -------------------------------
   -- Coverage's result display --
   -------------------------------

   procedure Print_Coverage_Header
     (F             : in out File_Type;
      Key_Name      : String;
      Display_Keys  : Boolean);
   --  Print the first line of a HTML table that lists some coverage stats;
   --  that is to say, a table line with a short description of each field.
   --  Key_Name is the name of the keys of this table (e.g. file names).
   --  If Display_Keys, Index_Name will be displayed in the first column.

   procedure Print_Coverage_Stats (F : in out File_Type; Stats : Stat_Array);
   --  Put the datas of Stats as HTML table cells ("<td>") in F.

   -----------------------
   -- Generate_Css_File --
   -----------------------

   procedure Generate_Css_File is
      F   : File_Type;

      procedure Put_State_Color (S : Any_Line_State; Color : String);
      --  Set line color for state S

      ---------------------
      -- Put_State_Color --
      ---------------------

      procedure Put_State_Color (S : Any_Line_State; Color : String) is
      begin
         Put_Line (F,
           "tr." & S'Img & ", " &
           "td.SumBar" & S'Img & " { background-color: " & Color & "; }");
      end Put_State_Color;

      CSS : constant Strings_Arr :=
        (
         new S'("table.SumTable, table.TotalTable, table.TracesFiles "
                & "{ margin-left:10%; width:80%; }"),
         new S'("tr.notice { background-color: #80ff80; }"),
         new S'("tr.error { background-color: red; }"),
         new S'("tr.warning { background-color: orange; }"),
         new S'("tr.NO_CODE_odd { }"),
         new S'("tr.NO_CODE_even { background-color: #f0f0f0; }"),
         new S'("td.SumHead, td.SumFile, td.SumNoFile, td.SumBar, "
                & "td.SumPercent, td.SumLineCov, td.SumTotal, "
                & "table.TracesFiles td"
                & "{ background-color: #B0C4DE; }"),
         new S'("td.SumHead, tr.Head td { color: white; }"),
         new S'("td.SumFile { color: green; }"),
         new S'("td.SumNoFile { color: grey; }"),
         new S'("td.SumPercent, td.SumLineCov { text-align: right; }"),
         new S'("table.LegendTable "
                & "{ margin-left:10%; width:80%; text-align: center; }"),
         new S'("table.SourceFile td pre { margin: 0; }")
         );

   begin
      Create_Output_File (F, "xcov.css");
      Put (F, CSS);
      Put_State_Color (Covered,                 "#80FF80");
      Put_State_Color (Partially_Covered,       "orange");
      Put_State_Color (Not_Covered,             "red");
      Put_State_Color (Exempted_With_Violation, "#CCCCCC");
      Put_State_Color (Exempted_No_Violation,   "#5CB3FF");
      Close (F);
   exception
      when Name_Error =>
         --  Failed to create output file, an error message has been printed

         return;
   end Generate_Css_File;

   ---------------------
   -- Generate_Report --
   ---------------------

   procedure Generate_Report
     (Context      : Coverage.Context_Access;
      Show_Details : Boolean)
   is
      Pp : Html_Pretty_Printer :=
        (Context      => Context,
         Show_Details => Show_Details,
         others       => <>);
   begin
      Pp.Show_Details := Show_Details;
      Annotations.Generate_Report (Pp, Show_Details);
   end Generate_Report;

   ---------
   -- Plh --
   ---------

   procedure Plh (Pp : in out Html_Pretty_Printer'Class; Str : String) is
   begin
      Put_Line (Pp.Html_File, Str);
   end Plh;

   ----------------------
   -- Pretty_Print_End --
   ----------------------

   procedure Pretty_Print_End (Pp : in out Html_Pretty_Printer) is

      procedure Pi (S : String);
      --  Print S to Pp's index file; new line at the end

      procedure Print_Bar_Legend (S : Any_Line_State);
      --  Print legend for visual bar for state S

      --------
      -- Pi --
      --------

      procedure Pi (S : String) is
      begin
         Put_Line (Pp.Index_File, S);
      end Pi;

      ----------------------
      -- Print_Bar_Legend --
      ----------------------

      procedure Print_Bar_Legend (S : Any_Line_State) is
      begin
         Pi ("      <td class=""SumBar" & S'Img & """ width=""25%"">"
               & S'Img & "</td>");
      end Print_Bar_Legend;

   --  Start of processing for Pretty_Print_Finish

   begin
      Pi ("  </table>");

      Pi ("<div id=help>");
      Pi ("<hr/>");
      Pi ("<h4 align=""right""><a href=""#top""> top </a></h4>");

      Pi ("This report presents a global view of the coverage");
      Pi ("results for the given coverage level. It sums up:");
      Pi ("<ul>");
      Pi ("<li> the list of trace files processed by gnatcov;");
      Pi ("<li> the global coverage results;");
      Pi ("<li> the coverage results per source file.");
      Pi ("</ul>");
      Pi ("<br/>");

      Pi ("For each trace file, the following information is given:");
      Pi ("<ul>");
      Pi ("<li> the name of the trace file;");
      Pi ("<li> the name of the executable that has been used to");
      Pi ("    generate it;");
      Pi ("<li> when it has been generated;");
      Pi ("<li> the tag that has been associated with this run, if any.");
      Pi ("</ul>");
      Pi ("<br/>");

      Pi ("The results (total and per file) contain:");
      Pi ("<ul>");
      Pi ("<li> the total number of lines ""of relevance"" for the unit");
      Pi ("(definition blow);");
      Pi ("<li> the number of such lines that are");
      Pi ("considered as fully, partially, or not covered for the chosen");
      Pi ("coverage criteria;");
      Pi ("<li> the number of such lines that are part of an");
      Pi ("exemption region, with or without actually exempted violations");
      Pi ("<li> a visual summary of this coverage data.");
      Pi ("</ul>");
      Pi ("<br/>");

      Pi ("""line of relevance"" are the source lines");
      Pi ("that have associated object code and which include");
      Pi ("all or part of a source entity of interest if we are");
      Pi ("assessing a source level criterion.");
      Pi ("<br/>");
      Pi ("Source comment lines are never included in the counts, typically.");
      Pi ("In the visual summaries, the colors have the following meaning:");
      Pi ("<br/>");
      Pi ("<br/>");

      Pi ("  <table cellspacing=""1"" class=""LegendTable"">");
      Pi ("    <tr>");
      Print_Bar_Legend (Covered);
      Print_Bar_Legend (Partially_Covered);
      Print_Bar_Legend (Not_Covered);
      Print_Bar_Legend (Exempted_No_Violation);
      Print_Bar_Legend (Exempted_With_Violation);

      Pi ("     </tr>");
      Pi ("   </table>");

      Pi ("</div>");
      Pi ("</body>");
      Pi ("</html>");
      Close (Pp.Index_File);
   end Pretty_Print_End;

   ---------------------------
   -- Pretty_Print_End_File --
   ---------------------------

   procedure Pretty_Print_End_File (Pp : in out Html_Pretty_Printer) is
   begin
      Plh (Pp, "</table>");
      Plh (Pp, "</body>");
      Plh (Pp, "</html>");
      Close (Pp.Html_File);
   end Pretty_Print_End_File;

   ---------------------------
   -- Pretty_Print_End_Line --
   ---------------------------

   procedure Pretty_Print_End_Line
     (Pp : in out Html_Pretty_Printer)
   is
   begin
      if Pp.Show_Line_Details then
         Plh (Pp, "    </table></td>");
         Plh (Pp, "  </tr>");
      end if;
   end Pretty_Print_End_Line;

   -----------------------
   -- Pretty_Print_Insn --
   -----------------------

   procedure Pretty_Print_Insn
     (Pp       : in out Html_Pretty_Printer;
      Pc       : Pc_Type;
      State    : Insn_State;
      Insn     : Binary_Content;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class) is
   begin
      Wrh (Pp, "      <tr class=""");

      case State is
         when Unknown =>
            raise Program_Error;

         when Not_Covered =>
            Wrh (Pp, "not_covered");

         when Covered | Both_Taken =>
            Wrh (Pp, "covered");

         when Branch_Taken
           | Fallthrough_Taken =>
            Wrh (Pp, "partially_covered");
      end case;

      Plh (Pp, """>");
      Wrh (Pp, "        <td><pre>");
      Wrh (Pp, Hex_Image (Pc));
      Wrh (Pp, ' ' & Insn_State_Char (State) & ':');
      Wrh (Pp, "  ");
      for I in Insn.First .. Insn.Last loop
         Wrh (Pp, Hex_Image (Get (Insn, I)));
         Wrh (Pp, " ");
      end loop;
      Wrh (Pp, "  ");
      Wrh (Pp, To_Xml_String (Disassemble (Insn, Pc, Insn_Set, Sym)));
      Plh (Pp, "</pre></td>");
      Plh (Pp, "      </tr>");
   end Pretty_Print_Insn;

   --------------------------
   -- Pretty_Print_Message --
   --------------------------

   procedure Pretty_Print_Message
     (Pp : in out Html_Pretty_Printer;
      M  : Message)
   is
      use Ada.Characters.Handling;
   begin
      Plh (Pp, "      <tr class=""" & To_Lower (M.Kind'Img) & """>");
      Wrh (Pp, "        <td><pre>");
      Wrh (Pp, To_Xml_String (Message_Annotation (M)));
      Plh (Pp, "</pre></td>");
      Plh (Pp, "      </tr>");
   end Pretty_Print_Message;

   ------------------------
   -- Pretty_Print_Start --
   ------------------------

   procedure Pretty_Print_Start (Pp : in out Html_Pretty_Printer) is
      use Qemu_Traces;
      use Traces_Files_List;
      use Traces_Files_Lists;

      procedure Pi (S : String);
      --  Print S to Pp's index file; new line at the end

      --------
      -- Pi --
      --------

      procedure Pi (S : String) is
      begin
         Put_Line (Pp.Index_File, S);
      end Pi;

      --  Local variables

      Cur : Traces_Files_Lists.Cursor;
      El  : Trace_File_Element_Acc;

      --  Start of processing for Pretty_Print_Start

   begin
      begin
         Create_Output_File (Pp.Index_File, "index.html");
      exception
         when Ada.Text_IO.Name_Error =>
            Put_Line (Standard_Error,
                      "cannot open index.html");
            raise;
      end;

      Generate_Css_File;

      Pi ("<html lang=""en"">");
      Pi ("<head>");
      Pi ("  <title>Coverage results</title>");
      Pi ("  <link rel=""stylesheet"" type=""text/css"" href=""xcov.css"">");
      Pi ("</head>");
      Pi ("<body>");
      Pi ("<div id=""top"">");
      Pi ("<h4 align=""right""><a href=""#help""> help </a></h4>");
      Pi ("<h1 align=""center"">GNATcoverage report</h1>");
      Pi ("<h2 align=""center"">Coverage level: "
            & Coverage_Option_Value & "</h2>");

      Pi ("</div>");

      --  List of traces.
      Pi ("  <hr/>");
      Pi ("  <table cellspacing=""1"" class=""TracesFiles"">");
      Pi ("    <tr class=""Head"">");
      Pi ("      <td>Trace Filename</td>");
      Pi ("      <td>Program</td>");
      Pi ("      <td>Date</td>");
      Pi ("      <td>Tag</td>");
      Pi ("    </tr>");

      Cur := Files.First;
      while Has_Element (Cur) loop
         El := Element (Cur);
         Pi ("    <tr>");
         Pi ("      <td>" & El.Filename.all & "</td>");
         Pi ("      <td>" & Get_Info (El.Trace, Exec_File_Name) & "</td>");
         Pi ("      <td>" & Format_Date_Info (Get_Info (El.Trace, Date_Time))
               & "</td>");
         Pi ("      <td>" & Get_Info (El.Trace, User_Data) & "</td>");
         Pi ("    </tr>");
         Next (Cur);
      end loop;

      Pi ("  </table>");

      --  Total stats

      Pi ("  <hr/>");
      Pi ("  <table cellspacing=""1"" class=""TotalTable"">");
      Print_Coverage_Header (Pp.Index_File, "", True);
      Pi ("    <tr>");
      Pi ("      <td title=""Total"" class=""SumTotal"">Total</td>");
      Print_Coverage_Stats (Pp.Index_File, Global_Stats);
      Pi ("    </tr>");
      Pi ("  </table>");

      --  Open table for results file per file
      Pi ("  <hr/>");
      Pi ("  <table cellspacing=""1"" class=""SumTable"">");
      Print_Coverage_Header (Pp.Index_File, "Filename", True);
   end Pretty_Print_Start;

   -----------------------------
   -- Pretty_Print_Start_File --
   -----------------------------

   procedure Pretty_Print_Start_File
     (Pp   : in out Html_Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean)
   is
      use Ada.Integer_Text_IO;

      Info : constant File_Info_Access := Get_File (File);

      procedure Ni;
      --  New line to Pp's index file

      procedure Pi (S : String);
      --  Print S to Pp's index file; new line at the end

      --------
      -- Ni --
      --------

      procedure Ni is
      begin
         New_Line (Pp.Index_File);
      end Ni;

      --------
      -- Pi --
      --------

      procedure Pi (S : String) is
      begin
         Put (Pp.Index_File, S);
      end Pi;

      --  Local variables

      Simple_Source_Filename : String renames Info.Simple_Name.all;
      Output_Filename        : constant String :=
        Get_Unique_Filename (File, "html");

   --  Start of processing for Pretty_Print_File

   begin
      Skip := True;

      --  Add a line in index
      Pi ("    <tr>"); Ni;

      --  First column: file name
      if Info.Full_Name /= null then
         Pi ("      <td title=""" & Info.Full_Name.all & '"');
      else
         Pi ("      <td title=""" & Simple_Source_Filename & '"');
      end if;

      if Info.Has_Source or Flag_Show_Missing then
         Pi (" class=""SumFile""><a href=""" & Output_Filename & """ >"
               & Simple_Source_Filename & "</a>");
      else
         Pi (" class=""SumNoFile"">" & Simple_Source_Filename);
      end if;
      Pi ("</td>"); Ni;

      --  Rest of line: coverage stats
      Print_Coverage_Stats (Pp.Index_File, Info.Stats);
      Pi ("    </tr>"); Ni;

      --  Do not try to process files whose source is not available.
      if not (Info.Has_Source or Flag_Show_Missing) then
         Warn_File_Missing (Info.all);
         return;
      end if;

      begin
         Create_Output_File (Pp.Html_File, Output_Filename);
      exception
         when Ada.Text_IO.Name_Error =>
            Put_Line (Standard_Error,
                      "cannot open " & Output_Filename);
            return;
      end;

      Skip := False;

      Plh (Pp, "<html lang=""en"">");
      Plh (Pp, "<head>");
      Plh (Pp, "  <title>Coverage of "
                & To_Xml_String (Simple_Source_Filename) & "</title>");
      Plh (Pp, "  <link rel=""stylesheet"" type=""text/css"" "
             & "href=""xcov.css"">");

      if Pp.Show_Details then
         Plh (Pp, "  <script language=""JavaScript"" "
                & "type=""text/javascript"">");
         Plh (Pp, "    function flip(atr) {");
         Plh (Pp, "      var details = atr.nextSibling.nextSibling;");
         Plh (Pp, "      if (details.style.display == ""none"")");
         Plh (Pp, "        details.style.display = """";");
         Plh (Pp, "      else");
         Plh (Pp, "        details.style.display = ""none"";");
         Plh (Pp, "    }");
         Plh (Pp, "  </script>");
      end if;

      Plh (Pp, "</head>");
      Plh (Pp, "<body>");
      Plh (Pp, "<h4 align=""right""><a href=""index.html""> index </a></h4>");
      Plh (Pp, "<h1 align=""center"">" & Simple_Source_Filename & "</h1>");
      Plh (Pp, "<h2 align=""center"">Coverage level: "
             & Coverage_Option_Value & "</h2>");

      Plh (Pp, "<hr/>");

      Plh (Pp, "<table class=""SumTable""><tr>");
      Print_Coverage_Header (Pp.Html_File, "", False);
      Print_Coverage_Stats (Pp.Html_File, Info.Stats);
      Plh (Pp, "</tr></table>");

      Plh (Pp, "<hr/>");

      Plh (Pp, "<table width=""100%"" cellpadding=""0"" "
           & "class=""SourceFile"">");
   end Pretty_Print_Start_File;

   -----------------------------
   -- Pretty_Print_Start_Line --
   -----------------------------

   procedure Pretty_Print_Start_Line
     (Pp       : in out Html_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String)
   is
      use Ada.Integer_Text_IO;

      State : constant Any_Line_State := Aggregated_State (Info.all);
   begin
      Pp.Show_Line_Details := Pp.Show_Details and then State /= No_Code;
      Wrh (Pp, "  <tr class=""" & State'Img);

      if State = No_Code then
         if Line_Num mod 2 = 1 then
            Wrh (Pp, "_odd");
         else
            Wrh (Pp, "_even");
         end if;
      end if;

      Wrh (Pp, """ title=""");
      case State is
         when Not_Covered =>
            Wrh (Pp, "code not covered");
         when Partially_Covered =>
            Wrh (Pp, "code partially covered");
         when Covered =>
            Wrh (Pp, "code covered");
         when No_Code =>
            Wrh (Pp, "no code present");
         when Not_Coverable =>
            Wrh (Pp, "no code generated");
         when Exempted_With_Violation =>
            Wrh (Pp, "exempted, violation present");
         when Exempted_No_Violation =>
            Wrh (Pp, "exempted, no violation");
      end case;

      if Pp.Show_Line_Details then
         Wrh (Pp, " (click to display/mask details)");
      end if;

      Wrh (Pp, """");

      if Pp.Show_Line_Details then
         Wrh (Pp, " onclick=""flip(this)""");
      end if;

      Plh (Pp, ">");

      Wrh (Pp, "    <td><pre>");
      Put (Pp.Html_File, Line_Num, 0);
      Plh (Pp, "</pre></td>");
      Wrh (Pp, "    <td><pre>");

      Put (Pp.Html_File, State_Char (State));

      Plh (Pp, "</pre></td>");
      Wrh (Pp, "    <td><pre>");
      Wrh (Pp, To_Xml_String (Line));
      Plh (Pp, "</pre></td>");
      Plh (Pp, "  </tr>");

      if Pp.Show_Line_Details then
         Plh (Pp, "  <tr style=""display: none""><td></td><td></td>");
         Plh (Pp, "    <td><table width=""100%"">");
      end if;
   end Pretty_Print_Start_Line;

   -------------------------------
   -- Pretty_Print_Start_Symbol --
   -------------------------------

   procedure Pretty_Print_Start_Symbol
     (Pp     : in out Html_Pretty_Printer;
      Name   : String;
      Offset : Pc_Type;
      State  : Line_State)
   is
      pragma Unreferenced (State);
      Label  : constant String := "<" & Name & "+" & Hex_Image (Offset) & ">:";
   begin
      Plh (Pp, "      <tr>");
      Wrh (Pp, "        <td><pre>");
      Wrh (Pp, To_Xml_String (Label));
      Plh (Pp, "</pre></td>");
      Plh (Pp, "      </tr>");
   end Pretty_Print_Start_Symbol;

   ---------------------------
   -- Print_Coverage_Header --
   ---------------------------

   procedure Print_Coverage_Header
     (F             : in out File_Type;
      Key_Name      : String;
      Display_Keys  : Boolean) is
   begin
      Put_Line (F, "    <tr>");

      if Display_Keys then
         Put_Line (F, "      <td class=""SumHead"">"
              & Key_Name & "</td>");
      end if;

      Put_Line
        (F, "      <td class=""SumHead""> total lines of relevance</td>");
      Put_Line
        (F, "      <td class=""SumHead""> fully covered</td>");
      Put_Line
        (F, "      <td class=""SumHead""> partially covered</td>");
      Put_Line
        (F, "      <td class=""SumHead""> not covered</td>");
      Put_Line
        (F, "      <td class=""SumHead""> exempted, no violation</td>");
      Put_Line
        (F, "      <td class=""SumHead""> exempted</td>");
      Put_Line
        (F, "      <td class=""SumHead""> visual summary </td>");

      Put_Line  (F, "    </tr>");
   end Print_Coverage_Header;

   --------------------------
   -- Print_Coverage_Stats --
   --------------------------

   procedure Print_Coverage_Stats (F : in out File_Type; Stats : Stat_Array) is
      use Ada.Integer_Text_IO;

      Total : constant Natural := Get_Total (Stats);
      --  Total line count, excluding No_Code lines

      procedure Print_Ratio (Part : Natural);
      --  Total and Part being a number of lines, print the ratio of the
      --  these two quantities (Part / Total) into a cell.

      procedure Print_Bar (S : Any_Line_State);
      --  Print visual bar for the given state

      procedure Print_Bar (S : Any_Line_State) is
         Size : constant Natural := Ratio (Stats (S), Total);
      begin
         if Size /= 0 then
            Put (F, "<td class=""SumBar" & S'Img & """ width=""");
            Put (F, Size, 0);
            Put (F, """ title=""");
            Put (F, Size, 0);
            Put (F, "% " & S'Img & """></td>");
         end if;

      end Print_Bar;

      -----------------
      -- Print_Ratio --
      -----------------

      procedure Print_Ratio (Part : Natural) is
      begin
         Put (F, "      <td class=""SumPercent"" width=""10%"">");

         if Total = 0 then
            Put (F, "no code");
         else
            Put (F, Part, 0);
            Put (F, " line");

            if Part /= 1 then
               Put (F, "s");
            end if;

            Put (F, " (");
            Put (F, Ratio (Part, Total), 0);
            Put (F, " %)");
         end if;

         Put (F, "</td>");
      end Print_Ratio;

   --  Start of processing for Print_Coverage_Stats

   begin
      --  Total lines

      Put (F, "      <td class=""SumPercent"" width=""10%"">");
      Put (F, Total, 0);
      Put (F, " lines");
      Put_Line (F, "</td>");

      --  Per-state breakdown

      Print_Ratio (Stats (Covered));
      Print_Ratio (Stats (Partially_Covered));
      Print_Ratio (Stats (Not_Covered));
      Print_Ratio (Stats (Exempted_No_Violation));
      Print_Ratio (Stats (Exempted_With_Violation));

      --  Visual summary

      Put (F, "      <td class=""SumBar"" align=""center"" width=""10%"">");

      Put (F, "        <table border=""0"" cellspacing=""0"" "
             & "class=""BarGraph""><tr height=""10"">");

      if Total /= 0 then
         Print_Bar (Covered);
         Print_Bar (Partially_Covered);
         Print_Bar (Not_Covered);
         Print_Bar (Exempted_No_Violation);
         Print_Bar (Exempted_With_Violation);
      end if;

      Put_Line (F, "</tr></table></td>");
   end Print_Coverage_Stats;

   ---------
   -- Put --
   ---------

   procedure Put (F : File_Type; Strings : Strings_Arr) is
   begin
      for I in Strings'Range loop
         Put_Line (F, Strings (I).all);
      end loop;
   end Put;

   ---------
   -- Wrh --
   ---------

   procedure Wrh (Pp : in out Html_Pretty_Printer'Class; Str : String) is
   begin
      Put (Pp.Html_File, Str);
   end Wrh;

end Annotations.Html;
