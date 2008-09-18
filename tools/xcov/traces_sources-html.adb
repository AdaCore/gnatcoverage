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

package body Traces_Sources.Html is
   type Html_Pretty_Printer is new Pretty_Printer with record
      Html_File : Ada.Text_IO.File_Type;
      Index_File : Ada.Text_IO.File_Type;
      Global_Pourcentage : Pourcentage;
   end record;

   procedure Pretty_Print_Start (Pp : in out Html_Pretty_Printer);
   procedure Pretty_Print_Finish (Pp : in out Html_Pretty_Printer);

   procedure Pretty_Print_File (Pp : in out Html_Pretty_Printer;
                                Source_Filename : String;
                                Stats : Stat_Array;
                                Skip : out Boolean);

   procedure Pretty_Print_Line (Pp : in out Html_Pretty_Printer;
                                Line_Num : Natural;
                                State : Line_State;
                                Line : String);

   procedure Pretty_Print_End_File (Pp : in out Html_Pretty_Printer);
   
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

   procedure Generate_Css_File
   is
      F : File_Type;

      procedure P (S : String) is
      begin
         Put_Line (F, S);
      end P;
   begin
      Create (F, Out_File, "xcov.css");
      P ("span.covered { background-color: #80ff80; }");
      P ("span.not_covered { background-color: red; }");
      P ("span.partially_covered { background-color: orange; }");
      P ("span.no_code_odd { }");
      P ("span.no_code_even { background-color: #f0f0f0; }");
      P ("table.SumTable td { background-color: #B0C4DE; }");
      P ("td.SumHead { color: white; }");
      P ("td.SumFile { color: green; }");
      P ("td.SumNoFile { color: grey; }");
      P ("table.SumTable td.SumBarCover { background-color: green; }");
      P ("table.SumTable td.SumBarNoCover { background-color: red; }");
      P ("td.SumPourcent, td.SumLineCov { text-align: right; }");
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

      Pp.Global_Pourcentage := (0, 0);

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

   procedure Pretty_Print_File (Pp : in out Html_Pretty_Printer;
                                Source_Filename : String;
                                Stats : Stat_Array;
                                Skip : out Boolean)
   is
      use Ada.Integer_Text_Io;
      use Ada.Directories;

      Simple_Source_Filename : constant String :=
        Simple_Name (Source_Filename);

      Output_Filename : constant String := Simple_Source_Filename & ".html";
      Exist : constant Boolean :=
        Flag_Show_Missing or else Exists (Source_Filename);
      P : constant Pourcentage := Get_Pourcentage (Stats);
      Pc : Natural;
      
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
      if Exist then
         Pi (" class=""SumFile""><a href=""" & Output_Filename & """ >"
               & Simple_Source_Filename & "</a>");
      else
         Pi (" class=""SumNoFile"">" & Simple_Source_Filename);
      end if;
      Pi ("</td>"); Ni;

      -- Second column: bar
      Pi ("      <td class=""SumBar"" align=""center"" width=""15%"">"); Ni;
      Pi ("        <table border=""0"" cellspacing=""0"">"
            & "<tr height=""10"">");
      if P.Total = 0 or P.Nbr = 0 then
         Pi ("<td class=""SumBarNocover"" width=""100""></td>");
      elsif P.Nbr = P.Total then
         Pi ("<td class=""SumBarCover"" width=""100""></td>");
      else
         Pc := P.Nbr * 100 / P.Total;
         Pi ("<td class=""SumBarCover"" width=""");
         Put (Pp.Index_File, Pc, 0);
         Pi ("""></td>");
         Pi ("<td class=""SumBarNoCover"" width=""");
         Put (Pp.Index_File, 100 - Pc, 0);
         Pi ("""></td>");
      end if;
      Pi ("</tr></table>"); Ni;
      Pi ("      </td>"); Ni;

      --  Third column: pourcentage
      Pi ("      <td class=""SumPourcent"" width=""10%"">");
      if P.Total = 0 then
         Pi ("no code");
      else
         Put (Pp.Index_File, P.Nbr * 100 / P.Total, 0);
         Pi (" %");
      end if;
      Pi ("</td>"); Ni;

      --  Fourth column: lines figure
      Pi ("      <td class=""SumLineCov"" width=""15%"">");
      Put (Pp.Index_File, P.Nbr, 0);
      Pi (" / ");
      Put (Pp.Index_File, P.Total, 0);
      Pi (" lines</td>"); Ni;

      Pi ("    </tr>"); Ni;

      --  Do not try to process files whose source is not available.
      if not Exist then
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

      Put_Line (Pp.Html_File, "<html lang=""en"">");
      Put_Line (Pp.Html_File, "<head>");
      Put_Line (Pp.Html_File, "  <title>Coverage of "
                & To_Xml_String (Simple_Source_Filename) & "</title>");
      Put_Line (Pp.Html_File, "  <link rel=""stylesheet"" type=""text/css"" "
                  & "href=""xcov.css"">");
      Put_Line (Pp.Html_File, "</head>");
      Put_Line (Pp.Html_File, "<body>");
      Put_Line (Pp.Html_File, "<h1>" & Simple_Source_Filename & "</h1>");
      Put_Line (Pp.Html_File, Get_Stat_String (Stats));
      Put_Line (Pp.Html_File, "<pre>");
   end Pretty_Print_File;

   procedure Pretty_Print_Line (Pp : in out Html_Pretty_Printer;
                                Line_Num : Natural;
                                State : Line_State;
                                Line : String)
   is
      use Ada.Integer_Text_IO;
   begin
      Put (Pp.Html_File, "<span class=");
      case State is
         when Not_Covered =>
            Put (Pp.Html_File, """not_covered""      ");
         when Partially_Covered
           | Branch_Taken
           | Branch_Fallthrough
           | Covered =>
            Put (Pp.Html_File, """partially_covered""");
         when Branch_Covered
           | Covered_No_Branch =>
            Put (Pp.Html_File, """covered""          ");
         when No_Code =>
            if Line_Num mod 2 = 1 then
               Put (Pp.Html_File, """no_code_odd""      ");
            else
               Put (Pp.Html_File, """no_code_even""     ");
            end if;
      end case;
      Put (Pp.Html_File, ">");

      Put (Pp.Html_File, Line_Num, 4);
      Put (Pp.Html_File, ' ');
      Put (Pp.Html_File, State_Char (State));
      Put (Pp.Html_File, ": ");
      Put (Pp.Html_File, To_Xml_String (Line));
      if Line'Length < 80 then
         Put (Pp.Html_File, (1 .. 80 - Line'Length => ' '));
      end if;
      Put (Pp.Html_File, "</span>");
      New_Line (Pp.Html_File);
   end Pretty_Print_Line;

   procedure Pretty_Print_End_File (Pp : in out Html_Pretty_Printer) is
   begin
      Put_Line (Pp.Html_File, "</pre>");
      Put_Line (Pp.Html_File, "</body>");
      Put_Line (Pp.Html_File, "</html>");
      Close (Pp.Html_File);
   end Pretty_Print_End_File;

   procedure Generate_Report
   is
      Html : Html_Pretty_Printer;
   begin
      Traces_Sources.Disp_Line_State (Html);
   end Generate_Report;
end Traces_Sources.Html;
