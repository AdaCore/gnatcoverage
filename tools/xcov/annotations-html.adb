------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2010, AdaCore                     --
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

with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Hex_Images;  use Hex_Images;
with Traces_Disa; use Traces_Disa;
with Traces_Files;
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
     (Pp    : in out Html_Pretty_Printer;
      Pc    : Pc_Type;
      State : Insn_State;
      Insn  : Binary_Content;
      Sym   : Symbolizer'Class);

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
      CSS : constant Strings_Arr :=
        (
         new S'("table.SumTable, table.TotalTable, table.TracesFiles "
                & "{ margin-left:10%; width:80%; }"),
         new S'("tr.covered { background-color: #80ff80; }"),
         new S'("tr.not_covered { background-color: red; }"),
         new S'("tr.partially_covered { background-color: orange; }"),
         new S'("tr.exempted { background-color: #CCCCCC; }"),
         new S'("tr.exempted_no_violation { background-color: #5CB3FF; }"),
         new S'("tr.notice { background-color: #80ff80; }"),
         new S'("tr.error { background-color: red; }"),
         new S'("tr.warning { background-color: orange; }"),
         new S'("tr.no_code_odd { }"),
         new S'("tr.no_code_even { background-color: #f0f0f0; }"),
         new S'("td.SumBarCover { background-color: green; }"),
         new S'("td.SumBarPartial { background-color: orange; }"),
         new S'("td.SumBarNoCover { background-color: red; }"),
         new S'("td.SumBarExempted { background-color: #5CB3FF; }"),
         new S'("td.SumHead, td.SumFile, td.SumNoFile, td.SumBar, "
                & "td.SumPourcent, td.SumLineCov, td.SumTotal, "
                & "table.TracesFiles td"
                & "{ background-color: #B0C4DE; }"),
         new S'("td.SumHead, tr.Head td { color: white; }"),
         new S'("td.SumFile { color: green; }"),
         new S'("td.SumNoFile { color: grey; }"),
         new S'("td.SumPourcent, td.SumLineCov { text-align: right; }"),
         new S'("table.LegendTable "
                & "{ margin-left:10%; width:80%; text-align: center; }"),
         new S'("table.SourceFile td pre { margin: 0; }")
         );

   begin
      Create_Output_File (F, "xcov.css");
      Put (F, CSS);
      Close (F);
   exception
      when Name_Error =>
         Put_Line (Standard_Error, "warning: cannot create xcov.css file");
         return;
   end Generate_Css_File;

   ---------------------
   -- Generate_Report --
   ---------------------

   procedure Generate_Report (Show_Details : Boolean) is
      Html : Html_Pretty_Printer;
   begin
      Html.Show_Details := Show_Details;
      Annotations.Generate_Report (Html, Show_Details);
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

      --------
      -- Pi --
      --------

      procedure Pi (S : String) is
      begin
         Put_Line (Pp.Index_File, S);
      end Pi;

      --  Start of processing for Pretty_Print_Finish

   begin
      Pi ("  </table>");

      Pi ("<div id=help>");
      Pi ("<hr/>");
      Pi ("<h4 align=""right""><a href=""#top""> top </a></h4>");

      Pi ("This report presents a global view of the coverage");
      Pi ("results for the given coverage level. It sums up:");
      Pi ("<ul>");
      Pi ("<li> the list of trace files processed by xcov;");
      Pi ("<li> the global results;");
      Pi ("<li> the coverage result per source files.");
      Pi ("</ul>");
      Pi ("<br/>");

      Pi ("For each trace files, the following information is given:");
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
      Pi ("<li> the total number of lines for the corresponding unit;");
      Pi ("""lines"" should here be understood as ""source line of code that");
      Pi ("generates object code"" (in that respect, comments are not");
      Pi ("considered as ""lines"");");
      Pi ("<li> the number of lines (as defined previously) that are");
      Pi ("considered as covered, in accordance with the chosen coverage");
      Pi ("criteria;");
      Pi ("<li> the number of lines partially covered according to this");
      Pi ("criteria;");
      Pi ("<li> the number of lines that are not covered at all according");
      Pi ("to this criteria;");
      Pi ("<li> A visual summary of these coverage datas.");
      Pi ("</ul>");
      Pi ("<br/>");

      Pi ("In the visual summary, the colors have the following meaning:");
      Pi ("<br/>");

      Pi ("  <table cellspacing=""1"" class=""LegendTable"">");
      Pi ("    <tr>");
      Pi ("      <td class=""SumBarCover"" width=""25%"">"
            & "Fully covered</td>");
      Pi ("      <td class=""SumBarPartial"" width=""25%"">"
            & "Partially covered</td>");
      Pi ("      <td class=""SumBarNoCover"" width=""25%"">"
            & "not covered</td>");
      Pi ("      <td class=""SumBarExempted"" width=""25%"">"
            & "exempted</td>");
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
     (Pp    : in out Html_Pretty_Printer;
      Pc    : Pc_Type;
      State : Insn_State;
      Insn  : Binary_Content;
      Sym   : Symbolizer'Class) is
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
      for I in Insn'Range loop
         Wrh (Pp, Hex_Image (Insn (I)));
         Wrh (Pp, " ");
      end loop;
      Wrh (Pp, "  ");
      Wrh (Pp, To_Xml_String (Disassemble (Insn, Pc, Sym)));
      Plh (Pp, "</pre></td>");
      Plh (Pp, "      </tr>");
   end Pretty_Print_Insn;

   --------------------------
   -- Pretty_Print_Message --
   --------------------------

   procedure Pretty_Print_Message
     (Pp    : in out Html_Pretty_Printer;
      M     : Message) is
   begin
      Wrh (Pp, "      <tr class=""");

      case M.Kind is
         when Diagnostics.Error =>
            Wrh (Pp, "error");

         when Diagnostics.Warning =>
            Wrh (Pp, "warning");

         when Diagnostics.Notice =>
            Wrh (Pp, "notice");
      end case;

      Plh (Pp, """>");
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
      use Traces_Files;
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
      Pi ("<h1 align=""center"">XCOV coverage report</h1>");
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

      --  Total stats.
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

      function Create_Output_Filename return String;
      --  Create the name of the html file.

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

      function Create_Output_Filename return String is
         Img : String (1 .. 2) := "00";
      begin
         if Info.Alias_Num = 0 then
            return Info.Simple_Name.all & ".html";
         else
            pragma Assert (Info.Alias_Num < 100);
            Img (2) := Character'Val (Character'Pos ('0') +
                                        Info.Alias_Num mod 10);
            Img (1) := Character'Val (Character'Pos ('0') +
                                        Info.Alias_Num / 10);
            return Info.Simple_Name.all & '.' & Img & ".html";
         end if;
      end Create_Output_Filename;

      --  Local variables

      Simple_Source_Filename : String renames Info.Simple_Name.all;
      Output_Filename        : constant String := Create_Output_Filename;

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
      Plh (Pp, "<table class=""SumTable""><tr>");
      Print_Coverage_Header (Pp.Html_File, "", False);
      Print_Coverage_Stats (Pp.Html_File, Info.Stats);
      Plh (Pp, "</tr></table>");

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

      State : constant Line_State := Aggregated_State (Info.State);
   begin
      Pp.Show_Line_Details := Pp.Show_Details and then State /= No_Code;
      Wrh (Pp, "  <tr class=""");

      if Info.Exemption /= Slocs.No_Location then
         if Get_Exemption_Count (Info.Exemption) = 0 then
            Wrh (Pp, "exempted_no_violation");
         else
            Wrh (Pp, "exempted");
         end if;

      else
         case State is
            when Not_Covered =>
               Wrh (Pp, "not_covered");
            when Partially_Covered =>
               Wrh (Pp, "partially_covered");
            when Covered =>
               Wrh (Pp, "covered");
            when No_Code =>
               if Line_Num mod 2 = 1 then
                  Wrh (Pp, "no_code_odd");
               else
                  Wrh (Pp, "no_code_even");
               end if;
         end case;
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
            Wrh (Pp, "no code generated for this line");
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

      if Info.Exemption /= Slocs.No_Location then
         Put (Pp.Html_File,
           State_Char_Exempted (Get_Exemption_Count (Info.Exemption) > 0));
      else
         Put (Pp.Html_File, State_Char (State));
      end if;

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
         Put_Line (F, "      <td class=""SumHead"" width=""50%"">"
              & Key_Name & "</td>");
      end if;

      Put_Line (F, "      <td class=""SumHead""> total nb of lines </td");
      Put_Line (F, "      <td class=""SumHead""> fully covered </td");
      Put_Line (F, "      <td class=""SumHead""> partially covered </td");
      Put_Line (F, "      <td class=""SumHead""> not covered </td");
      Put_Line (F, "      <td class=""SumHead""> visual summary </td");
      Put_Line (F, "    </tr>");
   end Print_Coverage_Header;

   --------------------------
   -- Print_Coverage_Stats --
   --------------------------

   procedure Print_Coverage_Stats (F : in out File_Type; Stats : Stat_Array) is
      use Ada.Integer_Text_IO;

      procedure Print_Ratio (Part : Natural; Total : Natural);
      --  Total and Part being a number of lines, print the ratio of the
      --  these two quantities (Part / Total) into a cell.

      -----------------
      -- Print_Ratio --
      -----------------

      procedure Print_Ratio (Part : Natural; Total : Natural) is
      begin
         Put (F, "      <td class=""SumPourcent"" width=""10%"">");

         if Total = 0 then
            Put (F, "no code");
         else
            Put (F, Part, 0);
            Put (F, " lines (");
            Put (F, Ratio (Part, Total), 0);
            Put (F, " %)");
         end if;

         Put_Line (F, "</td>");
      end Print_Ratio;

      --  Local variables

      P       : constant Counters := Get_Counters (Stats);
      Fully   : Natural;
      Partial : Natural;
      Uncover : Natural;

      --  Start of processing for Print_Coverage_Stats

   begin
      --  First column: total nb of lines
      Put (F, "      <td class=""SumPourcent"" width=""10%"">");
      Put (F, P.Total, 0);
      Put (F, " lines");
      Put_Line (F, "</td>");

      --  Second column: fully covered
      Print_Ratio (P.Fully, P.Total);

      --  Third column: partially covered
      Print_Ratio (P.Partial, P.Total);

      --  Fourth column: not covered
      Print_Ratio (P.Not_Covered, P.Total);

      --  Fifth column: visual summary
      Put (F, "      <td class=""SumBar"" align=""center"" width=""15%"">");
      New_Line (F);
      Put (F, "        <table border=""0"" cellspacing=""0"" "
             & "class=""BarGraph""><tr height=""10"">");

      if P.Fully = P.Total then
         --  Also includes P.Total = 0.
         Put (F, "<td class=""SumBarCover"" width=""100"""
                & " title=""100% fully covered""></td>");
      else
         Fully := Ratio (P.Fully, P.Total);

         if Fully /= 0 then
            Put (F, "<td class=""SumBarCover"" width=""");
            Put (F, Fully, 0);
            Put (F, """ title=""");
            Put (F, Fully, 0);
            Put (F, "% fully covered""></td>");
         end if;

         Partial :=  Ratio (P.Partial, P.Total);

         if Partial /= 0 then
            Put (F, "<td class=""SumBarPartial"" width=""");
            Put (F, Partial, 0);
            Put (F, """ title=""");
            Put (F, Partial, 0);
            Put (F, "% partially covered""></td>");
         end if;

         Uncover := Ratio (P.Not_Covered, P.Total);

         if Uncover /= 0 then
            Put (F, "<td class=""SumBarNoCover"" width=""");
            Put (F, Uncover, 0);
            Put (F, """ title=""");
            Put (F, Uncover, 0);
            Put (F, "% not covered""></td>");
         end if;
      end if;

      Put_Line (F, "</tr></table>");
      Put_Line (F, "      </td>");
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
