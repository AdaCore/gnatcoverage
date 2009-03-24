------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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
with Ada.Directories;
with Hex_Images; use Hex_Images;
with Traces_Disa; use Traces_Disa;
with Traces_Files;
with Traces_Files_List;
with Qemu_Traces;
with Coverage; use Coverage;

package body Traces_Sources.Html is
   type String_Cst_Acc is access constant String;
   subtype S is String;

   type Strings_Arr is array (Natural range <>) of String_Cst_Acc;

   procedure Put (F : File_Type; Strings : Strings_Arr);

   procedure Put (F : File_Type; Strings : Strings_Arr) is
   begin
      for I in Strings'Range loop
         Put_Line (F, Strings (I).all);
      end loop;
   end Put;

   type Html_Pretty_Printer is new Pretty_Printer with record
      Html_File : Ada.Text_IO.File_Type;
      Index_File : Ada.Text_IO.File_Type;
      Has_Insn_Table : Boolean;
      Global_Counters : Counters;
   end record;

   procedure Pretty_Print_Start (Pp : in out Html_Pretty_Printer);
   procedure Pretty_Print_Finish (Pp : in out Html_Pretty_Printer);

   procedure Pretty_Print_File (Pp : in out Html_Pretty_Printer;
                                Source_Filename : String;
                                Stats : Stat_Array;
                                Has_Source : Boolean;
                                Skip : out Boolean);

   procedure Pretty_Print_Line (Pp : in out Html_Pretty_Printer;
                                Line_Num : Natural;
                                State : Line_State;
                                Line : String);

   procedure Pretty_Print_Label (Pp : in out Html_Pretty_Printer;
                                 Label : String);
   procedure Pretty_Print_Insn (Pp : in out Html_Pretty_Printer;
                                Pc : Pc_Type;
                                State : Trace_State;
                                Insn : Binary_Content;
                                Sym : Symbolizer'Class);

   procedure Pretty_Print_End_File (Pp : in out Html_Pretty_Printer);

   procedure Plh (Pp : in out Html_Pretty_Printer'Class; Str : String);
   procedure Wrh (Pp : in out Html_Pretty_Printer'Class; Str : String);

   --  Return the string S with '>', '<' and '&' replaced by XML entities.
   function To_Xml_String (S : String) return String;

   procedure Generate_Css_File;
   procedure Print_Coverage_Stats (F : in out File_Type; Stats : Stat_Array);
   procedure Open_Insn_Table (Pp : in out Html_Pretty_Printer'Class);
   procedure Close_Insn_Table (Pp : in out Html_Pretty_Printer'Class);

   procedure Plh (Pp : in out Html_Pretty_Printer'Class; Str : String) is
   begin
      Put_Line (Pp.Html_File, Str);
   end Plh;

   procedure Wrh (Pp : in out Html_Pretty_Printer'Class; Str : String) is
   begin
      Put (Pp.Html_File, Str);
   end Wrh;

   function To_Xml_String (S : String) return String
   is
      function Xml_Length (S : String) return Natural;

      function Xml_Length (S : String) return Natural
      is
         Add : Natural := 0;
      begin
         for I in S'Range loop
            case S (I) is
               when '>' | '<' =>
                  Add := Add + 3;
               when '&' =>
                  Add := Add + 4;
               when others =>
                  null;
            end case;
         end loop;
         return S'Length + Add;
      end Xml_Length;

      Res : String (1 .. Xml_Length (S));
      Idx : Natural;
   begin
      Idx := Res'First;
      for I in S'Range loop
         case S (I) is
            when '>' =>
               Res (Idx .. Idx + 3) := "&gt;";
               Idx := Idx + 4;
            when '<' =>
               Res (Idx .. Idx + 3) := "&lt;";
               Idx := Idx + 4;
            when '&' =>
               Res (Idx .. Idx + 4) := "&amp;";
               Idx := Idx + 5;
            when others =>
               Res (Idx) := S (I);
               Idx := Idx + 1;
         end case;
      end loop;
      pragma Assert (Idx = Res'Last + 1);
      return Res;
   end To_Xml_String;

   CSS : constant Strings_Arr :=
     (
      new S'("table.SumTable, table.TotalTable, table.TracesFiles "
               & "{ margin-left:10%; width:80%; }"),
      new S'("tr.covered { background-color: #80ff80; }"),
      new S'("tr.not_covered { background-color: red; }"),
      new S'("tr.partially_covered { background-color: orange; }"),
      new S'("tr.no_code_odd { }"),
      new S'("tr.no_code_even { background-color: #f0f0f0; }"),
      new S'("td.SumBarCover { background-color: green; }"),
      new S'("td.SumBarPartial { background-color: orange; }"),
      new S'("td.SumBarNoCover { background-color: red; }"),
      new S'("td.SumHead, td.SumFile, td.SumNoFile, td.SumBar, "
               & "td.SumPourcent, td.SumLineCov, td.SumTotal, "
               & "table.TracesFiles td"
               & "{ background-color: #B0C4DE; }"),
      new S'("td.SumHead, tr.Head td { color: white; }"),
      new S'("td.SumFile { color: green; }"),
      new S'("td.SumNoFile { color: grey; }"),
      new S'("td.SumPourcent, td.SumLineCov { text-align: right; }"),
      new S'("table.LegendTable "
               & "{ margin-left:25%; width:50%; text-align: center; }"),
      new S'("table.SourceFile td pre { margin: 0; }")
     );

   procedure Generate_Css_File
   is
      F : File_Type;
   begin
      Create (F, Out_File, "xcov.css");
      Put (F, CSS);
      Close (F);
   exception
      when Name_Error =>
         Put_Line (Standard_Error, "warning: cannot create xcov.css file");
         return;
   end Generate_Css_File;

   procedure Pretty_Print_Start (Pp : in out Html_Pretty_Printer)
   is
      procedure P (S : String);

      procedure P (S : String) is
      begin
         Put_Line (Pp.Index_File, S);
      end P;
   begin
      begin
         Create (Pp.Index_File, Out_File, "index.html");
      exception
         when Ada.Text_IO.Name_Error =>
            Put_Line (Standard_Error,
                      "cannot open index.html");
            raise;
      end;

      Pp.Global_Counters := (0, 0, 0);

      Generate_Css_File;

      P ("<html lang=""en"">");
      P ("<head>");
      P ("  <title>Coverage results</title>");
      P ("  <link rel=""stylesheet"" type=""text/css"" href=""xcov.css"">");
      P ("</head>");
      P ("<body>");
      P ("<h1 align=""center"">XCOV coverage report</h1>");
      P ("<h2 align=""center""> Coverage level: "
         & To_Coverage_Option (Get_Action) & "</h2>");
      P ("  <table cellspacing=""1"" class=""SumTable"">");
      P ("    <tr>");
      P ("      <td class=""SumHead"" width=""60%"">Filename</td>");
      P ("      <td class=""SumHead"" colspan=""3"">Coverage</td>");
      P ("    </tr>");
   end Pretty_Print_Start;

   procedure Pretty_Print_Finish (Pp : in out Html_Pretty_Printer)
   is
      use Traces_Files;
      use Traces_Files_List;
      use Traces_Files_Lists;
      Cur : Traces_Files_Lists.Cursor;
      El : Trace_File_Element_Acc;

      procedure Pi (S : String);

      procedure Pi (S : String) is
      begin
         Put_Line (Pp.Index_File, S);
      end Pi;

   begin
      Pi ("  </table>");

      --  Total stats.
      Pi ("  <hr/>");
      Pi ("  <table cellspacing=""1"" class=""TotalTable"">");
      Pi ("    <tr>");
      Pi ("      <td title=""Total"" class=""SumTotal"">Total</td>");
      Print_Coverage_Stats (Pp.Index_File, Pp.Global_Stats);
      Pi ("    </tr>");
      Pi ("  </table>");

      --  Caption
      Pi ("  <hr/>");
      Pi ("  <table cellspacing=""1"" class=""LegendTable"">");
      Pi ("    <tr>");
      Pi ("      <td class=""SumBarCover"" witdh=""33%"">"
            & "Fully covered</td>");
      Pi ("      <td class=""SumBarPartial"">"
            & "Partially covered</td>");
      Pi ("      <td class=""SumBarNoCover"" witdh=""13%"">"
            & "not covered</td>");
      Pi ("     </tr>");
      Pi ("   </table>");

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
         Pi ("      <td>");
         Pi (El.Filename.all);
         Pi ("      </td>");
         Pi ("      <td>");
         Pi (Get_Info (El.Trace, Qemu_Traces.Info_Kind_Exec_Filename));
         Pi ("      </td>");
         Pi ("      <td>");
         Pi (Format_Date_Info (Get_Info (El.Trace,
                                         Qemu_Traces.Info_Kind_Date)));
         Pi ("      </td>");
         Pi ("      <td>");
         Pi (Get_Info (El.Trace, Qemu_Traces.Info_Kind_User_Tag));
         Pi ("      </td>");
         Pi ("    </tr>");
         Next (Cur);
      end loop;

      Pi ("  </table>");

      Pi ("</body>");
      Pi ("</html>");
      Close (Pp.Index_File);
   end Pretty_Print_Finish;

   procedure Print_Coverage_Stats (F : in out File_Type; Stats : Stat_Array)
   is
      use Ada.Integer_Text_IO;
      P : constant Counters := Get_Counters (Stats);
      Fully, Partial, Uncover : Natural;
   begin
      --  Second column: bar
      Put (F, "      <td class=""SumBar"" align=""center"" width=""15%"">");
      New_Line (F);
      Put (F, "        <table border=""0"" cellspacing=""0"" "
             & "class=""BarGraph""><tr height=""10"">");
      if P.Fully = P.Total then
         --  Also includes P.Total = 0.
         Put (F, "<td class=""SumBarCover"" width=""100"""
                & " title=""100% fully covered""></td>");
      else
         Fully := P.Fully * 100 / P.Total;
         if Fully /= 0 then
            Put (F, "<td class=""SumBarCover"" width=""");
            Put (F, Fully, 0);
            Put (F, """ title=""");
            Put (F, Fully, 0);
            Put (F, "% fully covered""></td>");
         end if;
         Partial := P.Partial * 100 / P.Total;
         if Partial /= 0 then
            Put (F, "<td class=""SumBarPartial"" width=""");
            Put (F, Partial, 0);
            Put (F, """ title=""");
            Put (F, Partial, 0);
            Put (F, "% partially covered""></td>");
         end if;
         Uncover := 100 - (Fully + Partial);
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

      --  Third column: pourcentage
      Put (F, "      <td class=""SumPourcent"" width=""10%"">");
      if P.Total = 0 then
         Put (F, "no code");
      else
         Put (F, P.Fully * 100 / P.Total, 0);
         Put (F, " %");
      end if;
      Put_Line (F, "</td>");

      --  Fourth column: lines figure
      Put (F, "      <td class=""SumLineCov"" width=""15%"">");
      Put (F, P.Fully, 0);
      Put (F, " / ");
      Put (F, P.Total, 0);
      Put_Line (F, " lines</td>");
   end Print_Coverage_Stats;

   procedure Pretty_Print_File (Pp : in out Html_Pretty_Printer;
                                Source_Filename : String;
                                Stats : Stat_Array;
                                Has_Source : Boolean;
                                Skip : out Boolean)
   is
      use Ada.Integer_Text_IO;
      use Ada.Directories;

      procedure Pi (S : String);
      procedure Ni;

      Simple_Source_Filename : constant String :=
        Simple_Name (Source_Filename);

      Output_Filename : constant String := Simple_Source_Filename & ".html";

      procedure Pi (S : String) is
      begin
         Put (Pp.Index_File, S);
      end Pi;

      procedure Ni is
      begin
         New_Line (Pp.Index_File);
      end Ni;
   begin
      Skip := True;

      --  Add a line in index.

      Pi ("    <tr>"); Ni;

      --  First column: file name
      Pi ("      <td title=""" & Source_Filename & '"');
      if Has_Source or Flag_Show_Missing then
         Pi (" class=""SumFile""><a href=""" & Output_Filename & """ >"
               & Simple_Source_Filename & "</a>");
      else
         Pi (" class=""SumNoFile"">" & Simple_Source_Filename);
      end if;
      Pi ("</td>"); Ni;

      Print_Coverage_Stats (Pp.Index_File, Stats);

      Pi ("    </tr>"); Ni;

      --  Do not try to process files whose source is not available.
      if not (Has_Source or Flag_Show_Missing) then
         return;
      end if;

      begin
         Create (Pp.Html_File, Out_File, Output_Filename);
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
      if Pp.Show_Asm then
         Plh (Pp, "  <script language=""JavaScript"" "
                & "type=""text/javascript"">");
         Plh (Pp, "    function flip(atr) {");
         Plh (Pp, "      var asm = atr.nextSibling.nextSibling;");
         Plh (Pp, "      if (asm.style.display == ""none"")");
         Plh (Pp, "        asm.style.display = """";");
         Plh (Pp, "      else");
         Plh (Pp, "        asm.style.display = ""none"";");
         Plh (Pp, "    }");
         Plh (Pp, "  </script>");
      end if;
      Plh (Pp, "</head>");
      Plh (Pp, "<body>");
      Plh (Pp, "<h1 align=""center"">" & Simple_Source_Filename & "</h1>");
      Plh (Pp, "<h2 align=""center""> Coverage level: "
           & To_Coverage_Option (Get_Action) & "</h2>");
      Plh (Pp, "<table class=""SumTable""><tr>");
      Print_Coverage_Stats (Pp.Html_File, Stats);
      Plh (Pp, "</tr></table>");

      Plh (Pp, "<table width=""100%"" cellpadding=""0"" "
           & "class=""SourceFile"">");
      Pp.Has_Insn_Table := False;
   end Pretty_Print_File;

   procedure Open_Insn_Table (Pp : in out Html_Pretty_Printer'Class)
   is
   begin
      if Pp.Has_Insn_Table then
         return;
      end if;
      Plh (Pp, "  <tr style=""display: none""><td></td><td></td>");
      Plh (Pp, "    <td><table width=""100%"">");
      Pp.Has_Insn_Table := True;
   end Open_Insn_Table;

   procedure Close_Insn_Table (Pp : in out Html_Pretty_Printer'Class)
   is
   begin
      if not Pp.Has_Insn_Table then
         return;
      end if;
      Plh (Pp, "    </table></td>");
      Plh (Pp, "  </tr>");
      Pp.Has_Insn_Table := False;
   end Close_Insn_Table;

   procedure Pretty_Print_Line (Pp : in out Html_Pretty_Printer;
                                Line_Num : Natural;
                                State : Line_State;
                                Line : String)
   is
      use Ada.Integer_Text_IO;
   begin
      Close_Insn_Table (Pp);

      Wrh (Pp, "  <tr class=");
      case State is
         when Not_Covered =>
            Wrh (Pp, """not_covered""");
         when Partially_Covered
           | Branch_Taken
           | Branch_Fallthrough
           | Covered =>
            Wrh (Pp, """partially_covered""");
         when Branch_Covered
           | Covered_No_Branch =>
            Wrh (Pp, """covered""");
         when No_Code =>
            if Line_Num mod 2 = 1 then
               Wrh (Pp, """no_code_odd""");
            else
               Wrh (Pp, """no_code_even""");
            end if;
      end case;

      Wrh (Pp, " title=""");
      case State is
         when Not_Covered =>
            Wrh (Pp, "code not covered");
         when Partially_Covered =>
            Wrh (Pp, "code partially covered");
         when Branch_Taken
           | Branch_Fallthrough
           | Covered =>
            Wrh (Pp, "code partially covered at decision level");
         when Branch_Covered
           | Covered_No_Branch =>
            Wrh (Pp, "code covered");
         when No_Code =>
            Wrh (Pp, "no code generated for this line");
      end case;
      if Pp.Show_Asm and then State /= No_Code then
         Wrh (Pp, " (click to display/mask assembly code)");
      end if;
      Wrh (Pp, """");

      if Pp.Show_Asm and then State /= No_Code then
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
   end Pretty_Print_Line;

   procedure Pretty_Print_Label (Pp : in out Html_Pretty_Printer;
                                 Label : String) is
   begin
      Open_Insn_Table (Pp);
      Plh (Pp, "      <tr>");
      Wrh (Pp, "        <td><pre>");
      Wrh (Pp, To_Xml_String (Label));
      Plh (Pp, "</pre></td>");
      Plh (Pp, "      </tr>");
   end Pretty_Print_Label;

   procedure Pretty_Print_Insn (Pp : in out Html_Pretty_Printer;
                                Pc : Pc_Type;
                                State : Trace_State;
                                Insn : Binary_Content;
                                Sym : Symbolizer'Class)
   is
   begin
      Open_Insn_Table (Pp);
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
      Wrh (Pp, ' ' & Trace_State_Char (State) & ':');
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

   procedure Pretty_Print_End_File (Pp : in out Html_Pretty_Printer) is
   begin
      Close_Insn_Table (Pp);
      Plh (Pp, "</table>");
      Plh (Pp, "</body>");
      Plh (Pp, "</html>");
      Close (Pp.Html_File);
   end Pretty_Print_End_File;

   procedure Generate_Report (Show_Asm : Boolean)
   is
      Html : Html_Pretty_Printer;
   begin
      Html.Show_Asm := Show_Asm;
      Traces_Sources.Disp_Line_State (Html, Show_Asm);
   end Generate_Report;
end Traces_Sources.Html;
