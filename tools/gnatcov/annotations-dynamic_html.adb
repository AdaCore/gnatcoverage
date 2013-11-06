------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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
with Ada.Command_Line;                 use Ada.Command_Line;
with Ada.Directories;                  use Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;
with GNATCOLL.JSON;                    use GNATCOLL.JSON;

with Annotations.Xml;

with Coverage;
with Hex_Images;
with Interfaces;
with Outputs;
with Qemu_Traces;
with Strings;
with Traces_Files;
with Traces_Files_List;

package body Annotations.Dynamic_Html is

   Bin_Dir : constant String :=
               (Containing_Directory
                 (GNAT.OS_Lib.Locate_Exec_On_Path (Command_Name).all));

   Prefix  : constant String := Containing_Directory (Bin_Dir);
   Lib_Dir : constant String := Prefix & "/lib/gnatcoverage/";

   DHTML_JS_Filename  : constant String := Lib_Dir & "gnatcov-dhtml.min.js";
   DHTML_CSS_Filename : constant String := Lib_Dir & "gnatcov-dhtml.min.css";

   type Dynamic_Html is new Pretty_Printer with record
      --  Pretty printer type for the Dynamic HTML annotation format

      JSON : JSON_Value := Create_Object;
      --  The JSON root. The Dynamic_Html builder will progressively populate
      --  this node with children. It will then be written into a <script>
      --  section in the final HTML document.

      Current_Mapping    : JSON_Value;
      --  A line mapping structure, containing the coverage results for the
      --  given line. This correspond to the mapping currently being generated
      --  by the Dynamic_Html builder. It is stored in Current_Mappings by the
      --  Pretty_Print_End_Line procedure.

      Current_Mappings   : JSON_Array;
      --  The list of all mappings for the file being currently processed by
      --  the builder. It is stored in the "sources" attributes of the JSON
      --  root by Pretty_Print_End_File.

      Current_Statements : JSON_Array;
      --  The statement list attached to the line currently being processed. It
      --  is stored in the Current_Mapping by Pretty_Print_End_Line if not
      --  empty.

      Current_Decision   : JSON_Value;
      --  The current decision being processed by the builder. It is stored in
      --  Current_Decisions by the Pretty_Print_End_Decision procedure.

      Current_Decisions  : JSON_Array;
      --  The decision list attached to the line currently being processed. It
      --  is stored in the Current_Mapping by Pretty_Print_End_Line if not
      --  empty.

      Current_Conditions : JSON_Array;
      --  The condition list attached to the line currently being processed. It
      --  is stored in the Current_Decision by Pretty_Print_End_Decision if not
      --  empty.

      Current_Source     : JSON_Value;
      --  The current source being processed by the builder

      Source_List        : JSON_Array;
      --  The sources array, containing all source mappings
   end record;

   -----------------------------------------
   -- Dynamic_Html's primitive operations --
   --   (inherited from Pretty_Printer)   --
   -----------------------------------------

   procedure Pretty_Print_Start (Pp : in out Dynamic_Html);

   procedure Pretty_Print_End (Pp : in out Dynamic_Html);

   procedure Pretty_Print_Start_File
     (Pp   : in out Dynamic_Html;
      File : Source_File_Index;
      Skip : out Boolean);

   procedure Pretty_Print_End_File (Pp : in out Dynamic_Html);

   procedure Pretty_Print_Start_Line
     (Pp       : in out Dynamic_Html;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String);

   procedure Pretty_Print_End_Line (Pp : in out Dynamic_Html);

   procedure Pretty_Print_Statement
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State);

   procedure Pretty_Print_Start_Decision
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State);

   procedure Pretty_Print_End_Decision (Pp : in out Dynamic_Html);

   procedure Pretty_Print_Condition
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State);

   procedure Pretty_Print_Message
     (Pp : in out Dynamic_Html;
      M  : Message);

   --------------------
   -- Set_SCO_Fields --
   --------------------

   procedure Set_SCO_Fields
     (Obj   : JSON_Value;
      SCO   : SCO_Id;
      State : Line_State);
   --  Set the following field to the given JSON object:
   --    * id
   --    * text
   --    * coverage
   --    * range

   ---------------
   -- Src_Range --
   ---------------

   function Src_Range (SCO : SCO_Id) return JSON_Array;
   --  Return a JSON array for the range Sloc_Start .. Sloc_End from SCO

   procedure Write_HTML_Report (Filename : String; Report : JSON_Value);
   --  Dump the HTML file into Filename, inlining both JS and CSS resources
   --  (see JS_Source and CSS_Source) and embedding the JSON object as well.

   ---------------
   -- Installed --
   ---------------

   function Installed return Boolean is
   begin
      return Exists (DHTML_JS_Filename) and then Exists (DHTML_CSS_Filename);
   end Installed;

   ---------------------
   -- Generate_Report --
   ---------------------

   procedure Generate_Report is
      Pp : Dynamic_Html;
   begin
      Annotations.Generate_Report (Pp, Show_Details => True);
   end Generate_Report;

   ------------------------
   -- Pretty_Print_Start --
   ------------------------

   procedure Pretty_Print_Start (Pp : in out Dynamic_Html)
   is
      Traces : JSON_Array;
      --  The array of trace records

      procedure Append_Coverage_Info;
      --  Generate generic coverage attributes

      procedure Append_Traces_List;
      --  Generate trace infomation record

      --------------------------
      -- Append_Coverage_Info --
      --------------------------

      procedure Append_Coverage_Info is
         use Coverage;
      begin
         Pp.JSON.Set_Field ("coverage_level", Coverage_Option_Value);
      end Append_Coverage_Info;

      ------------------------
      -- Append_Traces_List --
      ------------------------

      procedure Append_Traces_List is
         use Qemu_Traces;
         use Traces_Files;
         use Traces_Files_List;
         use Traces_Files_Lists;

         procedure Process_Trace (Position : Cursor);
         --  Generate the record from the trace file at Position and append it
         --  to the JSON array Traces

         -------------------
         -- Process_Trace --
         -------------------

         procedure Process_Trace (Position : Cursor) is
            TF    : constant Trace_File_Element_Acc := Element (Position);
            Trace : constant JSON_Value := Create_Object;
         begin
            Trace.Set_Field ("filename", TF.Filename.all);
            Trace.Set_Field ("program", Get_Info (TF.Trace, Exec_File_Name));
            Trace.Set_Field
              ("date", Format_Date_Info (Get_Info (TF.Trace, Date_Time)));
            Trace.Set_Field ("tag", Get_Info (TF.Trace, User_Data));

            Append (Traces, Trace);
         end Process_Trace;

      begin
         Files.Iterate (Process_Trace'Access);
         Pp.JSON.Set_Field ("traces", Traces);
      end Append_Traces_List;

   begin
      Append_Coverage_Info;
      Append_Traces_List;
   end Pretty_Print_Start;

   ----------------------
   -- Pretty_Print_End --
   ----------------------

   procedure Pretty_Print_End (Pp : in out Dynamic_Html) is
   begin
      Pp.JSON.Set_Field ("sources", Pp.Source_List);
      Write_HTML_Report ("report.html", Pp.JSON);
   end Pretty_Print_End;

   -----------------------------
   -- Pretty_Print_Start_File --
   -----------------------------

   procedure Pretty_Print_Start_File
     (Pp     : in out Dynamic_Html;
      File   : Source_File_Index;
      Skip   : out Boolean)
   is
      use Coverage;

      Info   : constant File_Info_Access := Get_File (File);
      --  No stat is emitted in the JSON output; the user is supposed
      --  to compute them by himself by post-processing the output.

      Source : constant JSON_Value := Create_Object;

   begin
      Clear (Pp.Current_Mappings);
      Clear (Pp.Current_Statements);
      Clear (Pp.Current_Decisions);
      Clear (Pp.Current_Conditions);

      if not (Info.Has_Source or Flag_Show_Missing) then
         Warn_File_Missing (Info.all);
         Skip := True;
         return;
      end if;

      Skip := False;

      Source.Set_Field ("filename", Info.Simple_Name.all);
      Source.Set_Field ("coverage_level", Coverage_Option_Value);

      Pp.Current_Source := Source;
   end Pretty_Print_Start_File;

   ---------------------------
   -- Pretty_Print_End_File --
   ---------------------------

   procedure Pretty_Print_End_File (Pp : in out Dynamic_Html) is
   begin
      if not Is_Empty (Pp.Current_Mappings) then
         Pp.Current_Source.Set_Field ("mappings", Pp.Current_Mappings);
      end if;

      Append (Pp.Source_List, Pp.Current_Source);
   end Pretty_Print_End_File;

   -----------------------------
   -- Pretty_Print_Start_Line --
   -----------------------------

   procedure Pretty_Print_Start_Line
     (Pp       : in out Dynamic_Html;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String)
   is
      use Annotations.Xml;
      use Strings;

      Coverage_State : constant String :=
                         (1 => State_Char (Aggregated_State (Info.all)));
      Exempted : constant Boolean := Info.Exemption /= Slocs.No_Location;

      Mapping  : constant JSON_Value := Create_Object;
      Line_Obj : constant JSON_Value := Create_Object;

   begin
      Clear (Pp.Current_Statements);
      Clear (Pp.Current_Decisions);
      Clear (Pp.Current_Conditions);

      Line_Obj.Set_Field ("number", Img (Line_Num));
      Line_Obj.Set_Field ("exempted", Exempted'Img);
      Line_Obj.Set_Field ("src", To_Xml_String (Line));

      Mapping.Set_Field ("coverage", Coverage_State);
      Mapping.Set_Field ("line", Line_Obj);

      Pp.Current_Mapping := Mapping;
   end Pretty_Print_Start_Line;

   ---------------------------
   -- Pretty_Print_End_Line --
   ---------------------------

   procedure Pretty_Print_End_Line (Pp : in out Dynamic_Html) is
   begin
      if not Is_Empty (Pp.Current_Statements) then
         Pp.Current_Mapping.Set_Field ("statements", Pp.Current_Statements);
      end if;

      if not Is_Empty (Pp.Current_Decisions) then
         Pp.Current_Mapping.Set_Field ("decisions", Pp.Current_Decisions);
      end if;

      Append (Pp.Current_Mappings, Pp.Current_Mapping);
   end Pretty_Print_End_Line;

   ----------------------------
   -- Pretty_Print_Statement --
   ----------------------------

   procedure Pretty_Print_Statement
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State)
   is
      Statement : constant JSON_Value := Create_Object;
   begin
      Clear (Pp.Current_Decisions);
      Clear (Pp.Current_Conditions);

      Set_SCO_Fields (Statement, SCO, State);

      Append (Pp.Current_Statements, Statement);
   end Pretty_Print_Statement;

   ---------------------------------
   -- Pretty_Print_Start_Decision --
   ---------------------------------

   procedure Pretty_Print_Start_Decision
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State)
   is
      Decision   : constant JSON_Value := Create_Object;
      Conditions : JSON_Array;
   begin
      Clear (Pp.Current_Conditions);

      Set_SCO_Fields (Decision, SCO, State);

      Pp.Current_Conditions := Conditions;
      Pp.Current_Decision := Decision;
   end Pretty_Print_Start_Decision;

   -------------------------------
   -- Pretty_Print_End_Decision --
   -------------------------------

   procedure Pretty_Print_End_Decision (Pp : in out Dynamic_Html) is
   begin
      if not Is_Empty (Pp.Current_Conditions) then
         Pp.Current_Decision.Set_Field ("conditions", Pp.Current_Conditions);
      end if;

      Append (Pp.Current_Decisions, Pp.Current_Decision);
   end Pretty_Print_End_Decision;

   ----------------------------
   -- Pretty_Print_Condition --
   ----------------------------

   procedure Pretty_Print_Condition
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State)
   is
      Condition : constant JSON_Value := Create_Object;
   begin
      Set_SCO_Fields (Condition, SCO, State);

      Append (Pp.Current_Conditions, Condition);
   end Pretty_Print_Condition;

   --------------------------
   -- Pretty_Print_Message --
   --------------------------

   procedure Pretty_Print_Message
     (Pp : in out Dynamic_Html;
      M  : Message)
   is
      use Ada.Characters.Handling;
      use Ada.Strings.Unbounded;

      use Hex_Images;
      use Interfaces;
      use Strings;

      Message : constant JSON_Value := Create_Object;

   begin
      Message.Set_Field ("kind", To_Lower (M.Kind'Img));

      if M.PC /= 0 then
         Message.Set_Field ("address", Hex_Image (M.PC));
      end if;

      if M.SCO /= No_SCO_Id then
         Message.Set_Field ("SCO", Image (M.SCO, With_Sloc => False));
      end if;

      Message.Set_Field ("message", To_String (M.Msg));
      Pp.Current_Mapping.Set_Field ("message", Message);
   end Pretty_Print_Message;

   --------------------
   -- Set_SCO_Fields --
   --------------------

   procedure Set_SCO_Fields
     (Obj   : JSON_Value;
      SCO   : SCO_Id;
      State : Line_State)
   is
      use Strings;
   begin
      Obj.Set_Field ("id", Img (Integer (SCO)));
      Obj.Set_Field ("text", SCO_Text (SCO));
      Obj.Set_Field ("coverage", String'(1 => State_Char (State)));
      Obj.Set_Field ("range", Src_Range (SCO));
   end Set_SCO_Fields;

   ---------------
   -- Src_Range --
   ---------------

   function Src_Range (SCO : SCO_Id) return JSON_Array
   is
      Sloc_Start : constant Source_Location := First_Sloc (SCO);
      Sloc_End   : constant Source_Location :=
                     End_Lex_Element (Last_Sloc (SCO));

      R : JSON_Array;

      Start_Arr : JSON_Array;
      End_Arr   : JSON_Array;

   begin
      Append (Start_Arr, Create (Sloc_Start.L.Line));
      Append (Start_Arr, Create (Sloc_Start.L.Column));

      Append (End_Arr, Create (Sloc_End.L.Line));
      Append (End_Arr, Create (Sloc_End.L.Column));

      Append (R, Create (Start_Arr));
      Append (R, Create (End_Arr));

      return R;
   end Src_Range;

   -----------------------
   -- Write_HTML_Report --
   -----------------------

   procedure Write_HTML_Report (Filename : String; Report : JSON_Value) is
      use Ada.Exceptions;
      use Ada.Text_IO;

      use Outputs;

      HTML : File_Type;

      procedure W (Item : String)
      with Pre => Is_Open (HTML);
      --  Write Item into HTML

      procedure NL
      with Pre => Is_Open (HTML);
      --  Write a New_Line into HTML

      procedure I (Filename : String; Indent : Natural := 0)
      with Pre => Is_Open (HTML);
      --  Inline the content of Filename into HTML

      -------
      -- W --
      -------

      procedure W (Item : String) is
      begin
         Put_Line (File => HTML, Item => Item);
      end W;

      --------
      -- NL --
      --------

      procedure NL is
      begin
         New_Line (File => HTML);
      end NL;

      -------
      -- I --
      -------

      procedure I (Filename : String; Indent : Natural := 0) is
         Input : File_Type;
         Space : constant String (1 .. Indent) := (others => ' ');

      begin
         Open (File => Input, Mode => In_File, Name => Filename);

         while not End_Of_File (Input) loop
            declare
               Line : constant String := Get_Line (Input);
            begin
               W (Space & Line);
            end;
         end loop;

         Close (Input);

      exception
         when Ex : others =>
            if Is_Open (Input) then
               Close (Input);
            end if;

            Fatal_Error
              ("inlining failed for " & Filename & ": " &
               Exception_Information (Ex));
      end I;

   begin
      Create_Output_File (HTML, Filename);

      W ("<!doctype html>");
      NL;
      W ("<html>");
      W (" <head>");
      W ("  <meta http-equiv=""content-type"" content=""text/html;");
      W ("        charset=UTF-8"">");
      NL;
      W ("  <title>GNATcoverage Report</title>");
      NL;
      W ("  <style>");
      I (DHTML_CSS_Filename, Indent => 3);
      W ("  </style>");
      NL;
      W ("  <script type=""text/javascript"">");
      I (DHTML_JS_Filename, Indent => 3);
      W ("  </script>");
      NL;
      W ("  <script type=""text/javascript"">");
      W ("   var JSON_REPORT = ");
      W (Write (Report, Compact => True));
      W ("  </script>");
      W (" </head>");
      NL;
      W (" <body>");
      NL;
      W ("  <noscript>");
      W ("   <div class=""gnatcov-noscript"">");
      W ("    Your web browser must have JavaScript enabled");
      W ("    in order for this report to display correctly.");
      W ("   </div>");
      W ("  </noscript>");
      NL;
      W ("  <script>gnatcov.analyse(JSON_REPORT);</script>");
      NL;
      W (" </body>");
      W ("</html>");

      Close (HTML);

   exception
      when Ex : others =>
         if Is_Open (HTML) then
            Close (HTML);
         end if;

         Fatal_Error
           ("report generation failed: " & Exception_Information (Ex));

   end Write_HTML_Report;

end Annotations.Dynamic_Html;
