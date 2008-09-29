------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
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
with Ada.Integer_Text_IO;
with Ada.Directories;
with Hex_Images; use Hex_Images;

package body Traces_Sources.Html is
   type String_Cst_Acc is access constant String;
   subtype S is String;

   type Strings_Arr is array (Natural range <>) of String_Cst_Acc;

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
      Global_Pourcentage : Pourcentage;
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
                                Insn : Binary_Content);

   procedure Pretty_Print_End_File (Pp : in out Html_Pretty_Printer);

   procedure Plh (Pp : in out Html_Pretty_Printer; Str : String) is
   begin
      Put_Line (Pp.Html_File, Str);
   end Plh;

   procedure Wrh (Pp : in out Html_Pretty_Printer; Str : String) is
   begin
      Put (Pp.Html_File, Str);
   end Wrh;

   --  Return the string S with '>', '<' and '&' replaced by XML entities.
   function To_Xml_String (S : String) return String
   is
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
      pragma Assert (Idx = S'Last + 1);
      return Res;
   end To_Xml_String;

   CSS : constant Strings_Arr :=
     (
      new S'("tr.covered { background-color: #80ff80; }"),
      new S'("tr.not_covered { background-color: red; }"),
      new S'("tr.partially_covered { background-color: orange; }"),
      new S'("tr.no_code_odd { }"),
      new S'("tr.no_code_even { background-color: #f0f0f0; }"),
      new S'("table.SumTable td { background-color: #B0C4DE; }"),
      new S'("td.SumHead { color: white; }"),
      new S'("td.SumFile { color: green; }"),
      new S'("td.SumNoFile { color: grey; }"),
      new S'("table.SumTable td.SumBarCover { background-color: green; }"),
      new S'("table.SumTable td.SumBarPartial { background-color: orange; }"),
      new S'("table.SumTable td.SumBarNoCover { background-color: red; }"),
      new S'("td.SumPourcent, td.SumLineCov { text-align: right; }"),
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

      Pp.Global_Pourcentage := (0, 0, 0);

      Generate_Css_File;

      P ("<html lang=""en"">");
      P ("<head>");
      P ("  <title>Coverage results</title>");
      P ("  <link rel=""stylesheet"" type=""text/css"" href=""xcov.css"">");
      P ("</head>");
      P ("<body>");
      P ("<h1 align=""center"">XCOV coverage report</h1>");
      P ("  <table width=""80%"" cellspacing=""1"" class=""SumTable"""
           & " align=""center"">");
      P ("    <tr>");
      P ("      <td class=""SumHead"" width=""60%"">Filename</td>");
      P ("      <td class=""SumHead"" colspan=""3"">Coverage</td>");
      P ("    </tr>");
   end Pretty_Print_Start;

   procedure Pretty_Print_Finish (Pp : in out Html_Pretty_Printer) is
   begin
      Put_Line (Pp.Index_File, "  </table>");
      Put_Line (Pp.Index_File, "</body>");
      Put_Line (Pp.Index_File, "</html>");
      Close (Pp.Index_File);
   end Pretty_Print_Finish;

   procedure Print_Coverage_Stats (F : in out File_Type; Stats : Stat_Array)
   is
      use Ada.Integer_Text_IO;
      P : constant Pourcentage := Get_Pourcentage (Stats);
      Fully, Partial, Uncover : Natural;
   begin
      -- Second column: bar
      Put (F, "      <td class=""SumBar"" align=""center"" width=""15%"">");
      New_Line (F);
      Put (F, "        <table border=""0"" cellspacing=""0"">"
             & "<tr height=""10"">");
      if P.Fully = P.Total then
         --  Also includes P.Total = 0.
         Put (F, "<td class=""SumBarCover"" width=""100""></td>");
      else
         Fully := P.Fully * 100 / P.Total;
         if Fully /= 0 then
            Put (F, "<td class=""SumBarCover"" width=""");
            Put (F, Fully, 0);
            Put (F, """></td>");
         end if;
         Partial := P.Partial * 100 / P.Total;
         if Partial /= 0 then
            Put (F, "<td class=""SumBarPartial"" width=""");
            Put (F, Partial, 0);
            Put (F, """></td>");
         end if;
         Uncover := 100 - (Fully + Partial);
         if Uncover /= 0 then
            Put (F, "<td class=""SumBarNoCover"" width=""");
            Put (F, Uncover, 0);
            Put (F, """></td>");
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
      use Ada.Integer_Text_Io;
      use Ada.Directories;

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
      if Flag_Show_Asm then
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
      Plh (Pp, "<table class=""SumTable"" align=""Center""><tr>");
      Print_Coverage_Stats (Pp.Html_File, Stats);
      Plh (Pp, "</tr></table>");

      Plh (Pp, "<table width=""100%"" cellpadding=""0"" class=""SourceFile"">");
      --Plh (Pp, "<pre>");
      Pp.Has_Insn_Table := False;
   end Pretty_Print_File;

   procedure Open_Insn_Table (Pp : in out Html_Pretty_Printer)
   is
   begin
      if Pp.Has_Insn_Table then
         return;
      end if;
      Plh (Pp, "  <tr style=""display: none""><td></td><td></td>");
      Plh (Pp, "    <td><table width=""100%"">");
      Pp.Has_Insn_Table := True;
   end Open_Insn_Table;

   procedure Close_Insn_Table (Pp : in out Html_Pretty_Printer)
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

      Put (Pp.Html_File, "  <tr class=");
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
      if Flag_Show_Asm and then State /= No_Code then
         Wrh (Pp, " onclick=""flip(this)""");
      end if;
      Plh (Pp, ">");

      Wrh (Pp, "    <td><pre>");
      Put (Pp.Html_File, Line_Num, 0);
      Plh (Pp, "</pre></td>");
      Wrh (pp, "    <td><pre>");
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
                                Insn : Binary_Content)
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
            Wrh (pp, "partially_covered");
      end case;
      Plh (Pp, """>");
      Wrh (Pp, "        <td><pre>");
      Wrh (PP, Hex_Image (Pc));
      Wrh (PP, ' ' & Trace_State_Char (State) & ':');
      Wrh (PP, "  ");
      for I in Insn'Range loop
         Wrh (Pp, Hex_Image (Insn (I)));
         Wrh (PP, " ");
      end loop;
      Wrh (PP, "  ");
      Wrh (PP, To_Xml_String (Disassemble (Insn, Pc)));
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

   procedure Generate_Report (Base : Traces_Base)
   is
      Html : Html_Pretty_Printer;
   begin
      Traces_Sources.Disp_Line_State (Html, Base);
   end Generate_Report;
end Traces_Sources.Html;
