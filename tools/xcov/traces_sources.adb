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
with Ada.Strings.Hash;
with Ada.Text_Io;
with Ada.Integer_Text_IO;
with Ada.Directories;

package body Traces_Sources is
   function Hash (El : String_Acc) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (El.all);
   end Hash;

   function Equivalent (L, R : String_Acc) return Boolean is
   begin
      return L.all = R.all;
   end Equivalent;

   function Equal (L, R : Source_Lines) return Boolean is
      pragma Unreferenced (L, R);
   begin
      return False;
   end Equal;

   --  Contains all the files.
   Filenames : Filenames_Maps.Map;

   function Find_File (Filename : String_Acc) return Filenames_Maps.Cursor
   is
      use Filenames_Maps;
      Res : Cursor;
      T : Source_Lines;
      Ok : Boolean;
   begin
      Res := Find (Filenames, Filename);
      if Res = No_Element then
         Source_Lines_Vectors.Init (T);
         Insert (Filenames, Filename, T, Res, Ok);
         if Ok = False then
            raise Program_Error;
         end if;
      end if;
      return Res;
   end Find_File;

   procedure Add_Line_State (File : Filenames_Maps.Cursor;
                             Line : Natural;
                             State : Traces.Trace_State)
   is
      procedure Process (Key : String_Acc; Element : in out Source_Lines)
      is
         pragma Unreferenced (Key);
         use Source_Lines_Vectors;
         L : constant Natural := Last (Element);
         Ls : Line_State;
      begin
         if L < Line then
            Set_Last (Element, Line);
            for I in L + 1 .. Line loop
               Element.Table (I) := (State => No_Code, others => <>);
            end loop;
         end if;
         if State = Unknown then
            raise Program_Error;
         end if;
         Ls := Element.Table (Line).State;
         Ls := Update_Table (Ls, State);
         Element.Table (Line).State := Ls;
      end Process;
   begin
      Filenames_Maps.Update_Element (Filenames, File, Process'Access);
   end Add_Line_State;

   procedure Add_Line (File : Filenames_Maps.Cursor;
                       Line : Natural;
                       Info : Addresses_Info_Acc)
   is
      procedure Process (Key : String_Acc; Element : in out Source_Lines)
      is
         pragma Unreferenced (Key);
         use Source_Lines_Vectors;
         L : constant Natural := Last (Element);
      begin
         if L < Line then
            Set_Last (Element, Line);
            for I in L + 1 .. Line loop
               Element.Table (I) := (State => No_Code, others => <>);
            end loop;
         end if;
         Append (Element.Table (Line).Lines, Info);
      end Process;
   begin
      Filenames_Maps.Update_Element (Filenames, File, Process'Access);
   end Add_Line;

--     procedure Set_Color (State : Line_State)
--     is
--        use Display;
--     begin
--        case State is
--           when No_Code =>
--              Set_Color (Black);
--           when Not_Covered =>
--              Set_Color (Red);
--           when Partially_Covered =>
--              Set_Color (Magenta);
--           when Covered | Branch_Taken | Branch_Fallthrough =>
--              Set_Color (Cyan);
--           when Covered_No_Branch | Branch_Covered =>
--              Set_Color (Green);
--        end case;
--     end Set_Color;

   type State_Map_Array is
     array (DO178B_Level_Type, Line_State) of Line_State;
   State_Map : constant State_Map_Array :=
     (Level_Raw => (No_Code => No_Code,
                    Not_Covered => Not_Covered,
                    Partially_Covered => Partially_Covered,
                    Covered => Covered,
                    Covered_No_Branch => Covered_No_Branch,
                    Branch_Taken => Branch_Taken,
                    Branch_Fallthrough => Branch_Fallthrough,
                    Branch_Covered => Branch_Covered),
      Level_A   => (No_Code => No_Code,
                    Not_Covered => Not_Covered,
                    Partially_Covered => Partially_Covered,
                    Covered => Partially_Covered,
                    Covered_No_Branch => Covered_No_Branch,
                    Branch_Taken => Partially_Covered,
                    Branch_Fallthrough => Partially_Covered,
                    Branch_Covered => Covered_No_Branch),
      Level_C   => (No_Code => No_Code,
                    Not_Covered => Not_Covered,
                    Partially_Covered => Covered_No_Branch,
                    Covered => Covered_No_Branch,
                    Covered_No_Branch => Covered_No_Branch,
                    Branch_Taken => Covered_No_Branch,
                    Branch_Fallthrough => Covered_No_Branch,
                    Branch_Covered => Covered_No_Branch));

   function Get_Stat_String (Stats : Stat_Array) return String
   is
      Total : Natural := 0;
   begin
      for J in Stats'Range loop
         Total := Total + Stats (J);
      end loop;
      Total := Total - Stats (No_Code);

      if Total = 0 then
         return "no code";
      else
         declare
            Res : constant String :=
              Natural'Image (Stats (Covered_No_Branch) * 100 / Total)
              & "% of" & Natural'Image (Total) & " lines covered";
         begin
            return Res (Res'First + 1 .. Res'Last);
         end;
      end if;
   end Get_Stat_String;

   type Pourcentage is record
      Nbr : Natural;
      Total : Natural;
   end record;

   function Get_Pourcentage (Stats : Stat_Array) return Pourcentage
   is
      Total : Natural := 0;
   begin
      for J in Stats'Range loop
         Total := Total + Stats (J);
      end loop;
      Total := Total - Stats (No_Code);

      return (Nbr => Stats (Covered_No_Branch), Total => Total);
   end Get_Pourcentage;

   type Xcov_Pretty_Printer is new Pretty_Printer with record
      Xcov_File : Ada.Text_Io.File_Type;
   end record;

   procedure Pretty_Print_File (Pp : in out Xcov_Pretty_Printer;
                                Source_Filename : String;
                                Stats : Stat_Array;
                                Skip : out Boolean);

   procedure Pretty_Print_Line (Pp : in out Xcov_Pretty_Printer;
                                Line_Num : Natural;
                                State : Line_State;
                                Line : String);

   procedure Pretty_Print_End_File (Pp : in out Xcov_Pretty_Printer);

   procedure Pretty_Print_File (Pp : in out Xcov_Pretty_Printer;
                                Source_Filename : String;
                                Stats : Stat_Array;
                                Skip : out Boolean)
   is
      use Ada.Text_IO;
      use Ada.Directories;
   begin
      Skip := True;

      --  Do not try to process files whose source is not available.
      if not Flag_Show_Missing
        and then not Exists (Source_Filename)
      then
         return;
      end if;

      declare
         Output_Filename : constant String :=
           Simple_Name (Source_Filename) & ".xcov";
      begin
         Create (Pp.Xcov_File, Out_File, Output_Filename);
      exception
         when Ada.Text_IO.Name_Error =>
            Put_Line (Standard_Error,
                      "cannot open " & Output_Filename);
            return;
      end;

      Skip := False;

      Put_Line (Pp.Xcov_File, Source_Filename & ':');
      Put_Line (Pp.Xcov_File, Get_Stat_String (Stats));
   end Pretty_Print_File;

   procedure Pretty_Print_Line (Pp : in out Xcov_Pretty_Printer;
                                Line_Num : Natural;
                                State : Line_State;
                                Line : String)
   is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
   begin
      Put (Pp.Xcov_File, Line_Num, 4);
      Put (Pp.Xcov_File, ' ');
      Put (Pp.Xcov_File, State_Char (State));
      Put (Pp.Xcov_File, ": ");
      Put (Pp.Xcov_File, Line);
      New_Line (Pp.Xcov_File);
   end Pretty_Print_Line;

   procedure Pretty_Print_End_File (Pp : in out Xcov_Pretty_Printer)
   is
      use Ada.Text_IO;
   begin
      Close (Pp.Xcov_File);
   end Pretty_Print_End_File;

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

   procedure Pretty_Print_Start (Pp : in out Html_Pretty_Printer)
   is
      use Ada.Text_IO;
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

      Put_Line (Pp.Index_File, "<html lang=""en"">");
      Put_Line (Pp.Index_File, "<head>");
      Put_Line (Pp.Index_File, "  <title>Coverage results</title>");
      Put_Line (Pp.Index_File, "</head>");
      Put_Line (Pp.Index_File, "<body>");
      Put_Line (Pp.Index_File, "  <table cellspacing=""1"">");
      Put_Line (Pp.Index_File, "    <tr>");
      Put_Line (Pp.Index_File, "      <td>Filename</td>");
      Put_Line (Pp.Index_File, "      <td>Coverage</td>");
      Put_Line (Pp.Index_File, "    </tr>");
   end Pretty_Print_Start;

   procedure Pretty_Print_Finish (Pp : in out Html_Pretty_Printer)
   is
      use Ada.Text_IO;
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
      use Ada.Text_IO;
      use Ada.Integer_Text_Io;
      use Ada.Directories;
      Simple_Source_Filename : constant String :=
        Simple_Name (Source_Filename);
      Output_Filename : constant String := Simple_Source_Filename & ".html";
      P : constant Pourcentage := Get_Pourcentage (Stats);
   begin
      Skip := True;

      Put_Line (Pp.Index_File, "    <tr>");
      Put_Line (Pp.Index_File,
                "      <td><a href=""" & Output_Filename & """>"
                & Simple_Source_Filename & "</a></td>");
      Put (Pp.Index_File, "      <td>");
      if P.Total = 0 then
         Put (Pp.Index_File, "no code");
      else
         Put (Pp.Index_File, P.Nbr * 100 / P.Total, 0);
         Put (Pp.Index_File,"%");
      end if;
      Put_Line (Pp.Index_File, "</td>");
      Put (Pp.Index_File, "      <td>");
      Put (Pp.Index_File, P.Nbr, 0);
      Put (Pp.Index_File, " / ");
      Put (Pp.Index_File, P.Total, 0);
      Put (Pp.Index_File, " lines");
      Put_Line (Pp.Index_File, "</td>");
      Put_Line (Pp.Index_File, "    </tr>");

      --  Do not try to process files whose source is not available.
      if not Flag_Show_Missing
        and then not Exists (Source_Filename)
      then
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
      Put_Line (Pp.Html_File, "  <style type=""text/css"">");
      Put_Line (Pp.Html_File,
                "    span.covered { background-color: #80ff80; }");
      Put_Line (Pp.Html_File,
                "    span.not_covered { background-color: red; }");
      Put_Line (Pp.Html_File,
                "    span.partially_covered { background-color: orange; }");
      Put_Line (Pp.Html_File,
                "    span.no_code_odd { }");
      Put_Line (Pp.Html_File,
                "    span.no_code_even { background-color: #f0f0f0; }");
      Put_Line (Pp.Html_File, "  </style>");
      Put_Line (Pp.Html_File, "</head>");
      Put_Line (Pp.Html_File, "<body>");
      Put_Line (Pp.Html_File, Get_Stat_String (Stats));
      Put_Line (Pp.Html_File, "<pre>");
   end Pretty_Print_File;

   procedure Pretty_Print_Line (Pp : in out Html_Pretty_Printer;
                                Line_Num : Natural;
                                State : Line_State;
                                Line : String)
   is
      use Ada.Text_IO;
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

   procedure Pretty_Print_End_File (Pp : in out Html_Pretty_Printer)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Pp.Html_File, "</pre>");
      Put_Line (Pp.Html_File, "</body>");
      Put_Line (Pp.Html_File, "</html>");
      Close (Pp.Html_File);
   end Pretty_Print_End_File;

   procedure Disp_File_Line_State (Pp : in out Pretty_Printer'class;
                                   Filename : String;
                                   File : Source_Lines)
   is
      use Source_Lines_Vectors;
      use Ada.Integer_Text_IO;
      use Ada.Text_IO;

      F : File_Type;
      Has_Source : Boolean;
      Line : Natural;

      Info : Addresses_Info_Acc;
      Ls : Line_State;

      Stats : Stat_Array := (others => 0);
      Skip : Boolean;
   begin
      for I in Integer range First .. Last (File) loop
         Ls := File.Table (I).State;
         Ls := State_Map (DO178B_Level, Ls);
         Stats (Ls) := Stats (Ls) + 1;
      end loop;

      Pretty_Print_File (Pp, Filename, Stats, Skip);
      if Skip then
         return;
      end if;

      begin
         Open (F, In_File, Filename);
         Has_Source := True;
      exception
         when Ada.Text_IO.Name_Error =>
            Put_Line (Filename & ": (can't open)");
            if not Flag_Show_Missing then
               return;
            end if;
            Has_Source := False;
      end;

      for I in Integer range First .. Last (File) loop
         Ls := File.Table (I).State;
         Ls := State_Map (DO178B_Level, Ls);
         if Has_Source then
            Pretty_Print_Line (Pp, I, Ls, Get_Line (F));
         else
            Pretty_Print_Line (Pp, I, Ls, "");
         end if;

         if Flag_Show_Asm then
            Info := Get_First (File.Table (I).Lines);
            while Info /= null loop
               Disp_Line (Info);
               Info := Get_Line_Next (Info);
            end loop;
         end if;
      end loop;
      if Has_Source then
         Line := Last (File) + 1;
         while not End_Of_File (F) loop
            Pretty_Print_Line (Pp, Line, No_Code, Get_Line (F));
            Line := Line + 1;
         end loop;
         Close (F);
      end if;

      Pretty_Print_End_File (Pp);
   end Disp_File_Line_State;

   procedure Disp_Line_State (Pp : in out Pretty_Printer'Class)
   is
      use Filenames_Maps;
      use Ada.Text_IO;
      use Ada.Directories;

      procedure Process (Key : String_Acc; Element : in Source_Lines) is
      begin
         Disp_File_Line_State (Pp, Key.all, Element);
      end Process;
      Cur : Cursor;
   begin
      Pretty_Print_Start (Pp);

      --  Iterates on all files.
      Cur := First (Filenames);
      while Cur /= No_Element loop
         Query_Element (Cur, Process'Access);
         Next (Cur);
      end loop;

      Pretty_Print_Finish (Pp);
   end Disp_Line_State;

   procedure Disp_Line_State (Format : Output_Format) is
   begin
      case Format is
         when Format_Html =>
            declare
               Html : Html_Pretty_Printer;
            begin
               Disp_Line_State (Html);
            end;
         when Format_Xcov =>
            declare
               Xcov : Xcov_Pretty_Printer;
            begin
               Disp_Line_State (Xcov);
            end;
      end case;
   end Disp_Line_State;

   procedure Disp_File_Summary
   is
      use Ada.Text_IO;
      use Ada.Directories;

      procedure Process (Key : String_Acc; File : in Source_Lines)
      is
         use Ada.Integer_Text_IO;
         use Source_Lines_Vectors;

         Stats : Stat_Array := (others => 0);
         State : Line_State;
      begin
         for I in Integer range First .. Last (File) loop
            State := File.Table (I).State;
            State := State_Map (DO178B_Level, State);
            Stats (State) := Stats (State) + 1;
         end loop;

         Put (Simple_Name (Key.all));
         Put (": ");
         Put (Get_Stat_String (Stats));
         New_Line;
      end Process;

      use Filenames_Maps;
      Cur : Cursor;
   begin
      Cur := First (Filenames);
      while Cur /= No_Element loop
         Query_Element (Cur, Process'Access);
         Next (Cur);
      end loop;
   end Disp_File_Summary;

end Traces_Sources;
