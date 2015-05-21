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
with Ada.Containers.Vectors;
with Ada.Directories;                  use Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

pragma Warnings (Off, "* is an internal GNAT unit");
   with Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "* is an internal GNAT unit");

with GNAT.OS_Lib;
with GNATCOLL.JSON;                    use GNATCOLL.JSON;

with Annotations.Xml;

with Coverage;
with Hex_Images;
with Interfaces;
with Project;
with Outputs;
with Qemu_Traces;
with Strings;
with Switches;
with Traces_Disa;
with Traces_Files;
with Traces_Files_List;

--  This package generates a dynamic HTML report, i.e. an HTML document heavily
--  relying on JavaScript for presenting advanced graphical components.
--
--  It requires additional resources (CSS and JavaScript) to be installed as a
--  plugin to the GNATcoverage installation. It produces one HTML file
--  containing the JS application and multiple JS files containing the data
--  associatied with each source from the project being analyzed.
--
--  Splitting the coverage data into multiple JS files addresses a load-time
--  issue since the final set of data generated can be large enough (several
--  Mbs) for browsers to take several seconds to load the whole report. The
--  current implementation splits the coverage data of each source file in
--  separate JS file that are lazily loaded by the report application upon
--  request, i.e. when we need to display detailed coverage information for a
--  specific file.

package body Annotations.Dynamic_Html is

   Bin_Dir : constant String :=
               (Containing_Directory
                 (GNAT.OS_Lib.Locate_Exec_On_Path (Command_Name).all));

   Prefix  : constant String := Containing_Directory (Bin_Dir);
   Lib_Dir : constant String := Prefix & "/lib/gnatcoverage/";

   DHTML_JS_Filename  : constant String := Lib_Dir & "gnatcov-dhtml.min.js";
   DHTML_CSS_Filename : constant String := Lib_Dir & "gnatcov-dhtml.min.css";

   package Source_Vectors is new Ada.Containers.Vectors (Natural, JSON_Value);

   type Dynamic_Html is new Pretty_Printer with record
      --  Pretty printer type for the Dynamic HTML annotation format

      JSON : JSON_Value := Create_Object;
      --  The JSON root. The Dynamic_Html builder will progressively populate
      --  this node with children. It will then be written into a <script>
      --  section in the final HTML document.

      Current_Mapping     : JSON_Value;
      --  A line mapping structure, containing the coverage results for the
      --  given line. This correspond to the mapping currently being generated
      --  by the Dynamic_Html builder. It is stored in Current_Mappings by the
      --  Pretty_Print_End_Line procedure.

      Current_Mappings    : JSON_Array;
      --  The list of all mappings for the file being currently processed by
      --  the builder. It is stored in the "sources" attributes of the JSON
      --  root by Pretty_Print_End_File.

      Current_Statements  : JSON_Array;
      --  The statement list attached to the line currently being processed. It
      --  is stored in the Current_Mapping by Pretty_Print_End_Line if not
      --  empty.

      Current_Decision    : JSON_Value;
      --  The current decision being processed by the builder. It is stored in
      --  Current_Decisions by the Pretty_Print_End_Decision procedure.

      Current_Decisions   : JSON_Array;
      --  The decision list attached to the line currently being processed. It
      --  is stored in the Current_Mapping by Pretty_Print_End_Line if not
      --  empty.

      Current_Conditions  : JSON_Array;
      --  The condition list attached to the line currently being processed. It
      --  is stored in the Current_Decision by Pretty_Print_End_Decision if not
      --  empty.

      Current_Insn_Set    : JSON_Value;
      --  The current instruction set being processed by the builder. It is
      --  stored as "instruction_set" in the Current_Mapping by
      --  Pretty_Print_End_Instruction_Set.

      Current_Insn_Block  : JSON_Value;
      --  The current instruction block being processed by the builder. It is
      --  stored in Current_Insn_Blocks by Pretty_Print_End_Symbol.

      Current_Insn_Blocks : JSON_Array;
      --  The instruction block list attached to the line currently being
      --  processed. It is stored in Current_Insn_Set by
      --  Pretty_Print_End_Instruction_Set if not empty.

      Current_Insns       : JSON_Array;
      --  The instruction list attached to the line currently being processed.
      --  It is stored in Current_Insn_Block by Pretty_Print_End_Symbol if not
      --  empty.

      Current_Source      : JSON_Value;
      --  The current source being processed by the builder

      Source_List         : Source_Vectors.Vector;
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

   procedure Pretty_Print_Start_Instruction_Set
     (Pp    : in out Dynamic_Html;
      State : Any_Line_State);

   procedure Pretty_Print_End_Instruction_Set
     (Pp : in out Dynamic_Html);

   procedure Pretty_Print_Start_Symbol (Pp     : in out Dynamic_Html;
                                        Name   : String;
                                        Offset : Pc_Type;
                                        State  : Line_State);

   procedure Pretty_Print_End_Symbol (Pp : in out Dynamic_Html);

   procedure Pretty_Print_Insn
     (Pp       : in out Dynamic_Html;
      Pc       : Pc_Type;
      State    : Insn_State;
      Insn     : Binary_Content;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class);

   ----------------------
   -- Internal Helpers --
   ----------------------

   procedure Set_SCO_Fields
     (Obj   : JSON_Value;
      SCO   : SCO_Id;
      State : Line_State);
   --  Set the following field to the given JSON object:
   --    * id
   --    * text
   --    * coverage
   --    * range

   function Src_Range (SCO : SCO_Id) return JSON_Array;
   --  Return a JSON array for the range Sloc_Start .. Sloc_End from SCO

   function Get_Hunk_Filename (File : Source_File_Index) return String;
   --  Return the name of the file containing the coverage data for the
   --  given source file.

   procedure Write_Full_Report
     (Report   : JSON_Value;
      Hunks    : Source_Vectors.Vector);
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
      --  Generate trace information record

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

   procedure Pretty_Print_End (Pp : in out Dynamic_Html)
   is
      Sources : JSON_Array;
   begin
      for Source of Pp.Source_List loop
         declare
            Simplified : constant JSON_Value := Create_Object;
            Filename   : constant String := Source.Get ("filename");
         begin
            Simplified.Set_Field ("filename", Filename);
            Simplified.Set_Field
              ("stats", JSON_Value'(Source.Get ("stats")));
            Simplified.Set_Field
              ("coverage_level", String'(Source.Get ("coverage_level")));
            Simplified.Set_Field
              ("hunk_filename", String'(Source.Get ("hunk_filename")));

            if Source.Has_Field ("project") then
               --  Project name is optional. Add it only when relevant

               Simplified.Set_Field
                 ("project", String'(Source.Get ("project")));
            end if;

            Append (Sources, Simplified);
         end;
      end loop;

      Pp.JSON.Set_Field ("sources", Sources);
      Write_Full_Report (Pp.JSON, Pp.Source_List);
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
      use Project;

      Info   : constant File_Info_Access := Get_File (File);

      Source : constant JSON_Value := Create_Object;
      Stats  : constant JSON_Value := Create_Object;

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

      --  Compute the coverage stats and store them into a JSON dictionary

      Stats.Set_Field ("no_code", Info.Stats (No_Code));
      Stats.Set_Field ("covered", Info.Stats (Covered));
      Stats.Set_Field ("partially_covered", Info.Stats (Partially_Covered));
      Stats.Set_Field ("not_covered", Info.Stats (Not_Covered));
      Stats.Set_Field
        ("exempted_no_violation", Info.Stats (Exempted_No_Violation));
      Stats.Set_Field
        ("exempted_with_violation", Info.Stats (Exempted_With_Violation));

      --  Generate the JSON object for this source file

      Source.Set_Field ("filename", Get_Unique_Name (File));
      Source.Set_Field ("hunk_filename", Get_Hunk_Filename (File));
      Source.Set_Field ("coverage_level", Coverage_Option_Value);
      Source.Set_Field ("stats", Stats);

      if Switches.Root_Project /= null then
         Source.Set_Field ("project", Project_Name (Info.Full_Name.all));
      end if;

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

      Pp.Source_List.Append (Pp.Current_Source);
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

   ----------------------------------------
   -- Pretty_Print_Start_Instruction_Set --
   ----------------------------------------

   procedure Pretty_Print_Start_Instruction_Set
     (Pp    : in out Dynamic_Html;
      State : Any_Line_State)
   is
      Insn_Set : constant JSON_Value := Create_Object;
   begin
      Clear (Pp.Current_Insn_Blocks);

      Insn_Set.Set_Field ("coverage", State_Char (State) & "");

      Pp.Current_Insn_Set := Insn_Set;
   end Pretty_Print_Start_Instruction_Set;

   -------------------------------
   -- Pretty_Print_Start_Symbol --
   -------------------------------

   procedure Pretty_Print_Start_Symbol
     (Pp     : in out Dynamic_Html;
      Name   : String;
      Offset : Pc_Type;
      State  : Line_State)
   is
      use Hex_Images;
      Coverage_State : constant String := State_Char (State) & "";
      Insn_Block     : constant JSON_Value := Create_Object;

   begin
      Clear (Pp.Current_Insns);

      Insn_Block.Set_Field ("name", Name);
      Insn_Block.Set_Field ("offset", Hex_Image (Offset));
      Insn_Block.Set_Field ("coverage", Coverage_State);

      Pp.Current_Insn_Block := Insn_Block;
   end Pretty_Print_Start_Symbol;

   -----------------------
   -- Pretty_Print_Insn --
   -----------------------

   procedure Pretty_Print_Insn
     (Pp       : in out Dynamic_Html;
      Pc       : Pc_Type;
      State    : Insn_State;
      Insn     : Binary_Content;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class)
   is
      use Hex_Images;
      use Traces_Disa;

      Instruction : constant JSON_Value := Create_Object;

   begin
      Instruction.Set_Field ("address", Hex_Image (Pc));
      Instruction.Set_Field ("coverage", Insn_State_Char (State) & "");
      Instruction.Set_Field
        ("assembly", Disassemble (Insn, Pc, Insn_Set, Sym));

      Append (Pp.Current_Insns, Instruction);
   end Pretty_Print_Insn;

   -----------------------------
   -- Pretty_Print_End_Symbol --
   -----------------------------

   procedure Pretty_Print_End_Symbol (Pp : in out Dynamic_Html) is
   begin
      if not Is_Empty (Pp.Current_Insns) then
         Pp.Current_Insn_Block.Set_Field ("instructions", Pp.Current_Insns);
      end if;

      Append (Pp.Current_Insn_Blocks, Pp.Current_Insn_Block);
   end Pretty_Print_End_Symbol;

   --------------------------------------
   -- Pretty_Print_End_Instruction_Set --
   --------------------------------------

   procedure Pretty_Print_End_Instruction_Set
     (Pp : in out Dynamic_Html) is
   begin
      if not Is_Empty (Pp.Current_Insn_Blocks) then
         Pp.Current_Insn_Set.Set_Field
           ("instruction_blocks", Pp.Current_Insn_Blocks);
      end if;

      Pp.Current_Mapping.Set_Field ("instruction_set", Pp.Current_Insn_Set);
   end Pretty_Print_End_Instruction_Set;

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
   -- Get_Hunk_Filename --
   -----------------------

   function Get_Hunk_Filename (File : Source_File_Index) return String is
   begin
      return Get_Unique_Filename (File, "hunk.js");
   end Get_Hunk_Filename;

   -----------------------
   -- Write_Full_Report --
   -----------------------

   procedure Write_Full_Report
     (Report          : JSON_Value;
      Hunks           : Source_Vectors.Vector)
   is
      use Ada.Exceptions;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;

      use Outputs;

      HTML : File_Type;
      --  The HTML report. The procedure Write_Report is in charge of opening
      --  and closing this file. It is declared here for praticality of use of
      --  helper functions in Write_Report (see definitions below).

      procedure NL (Output : File_Type := HTML)
      with Pre => Is_Open (Output);
      --  Write a New_Line into Output

      procedure W
        (Item     : String;
         Output   : File_Type := HTML;
         New_Line : Boolean   := True)
      with Pre => Is_Open (Output);
      --  Write Item (String) into Output

      procedure W
        (Item     : Unbounded_String;
         Output   : File_Type := HTML;
         New_Line : Boolean   := True)
      with Pre => Is_Open (Output);
      --  Write Item (Unbounded String) into Output

      procedure I
        (Filename : String;
         Indent   : Natural   := 0;
         Output   : File_Type := HTML)
      with Pre => Is_Open (Output);
      --  Inline the content of Filename into Output

      procedure Write_Hunk (Hunk : JSON_Value);
      --  Dump the JSON object containing the full coverage data for a specific
      --  source file, wrapping it into a function call.

      procedure Write_Report (Filename : String);
      --  Dump the HTML report.

      -------
      -- W --
      -------

      procedure W
        (Item     : String;
         Output   : File_Type := HTML;
         New_Line : Boolean   := True) is
      begin
         Put (File => Output, Item => Item);

         if New_Line then
            NL (Output => Output);
         end if;
      end W;

      -------
      -- W --
      -------

      procedure W
        (Item     : Unbounded_String;
         Output   : File_Type := HTML;
         New_Line : Boolean   := True)
      is
         Buffer : Aux.Big_String_Access;
         Last   : Natural;
         First  : constant Natural := Aux.Big_String'First;

      begin
         Aux.Get_String (Item, Buffer, Last);
         Put (File => Output, Item => Buffer (First .. Last));

         if New_Line then
            NL (Output => Output);
         end if;
      end W;

      --------
      -- NL --
      --------

      procedure NL (Output : File_Type := HTML) is
      begin
         New_Line (File => Output);
      end NL;

      -------
      -- I --
      -------

      procedure I
        (Filename : String;
         Indent   : Natural   := 0;
         Output   : File_Type := HTML)
      is
         Input : File_Type;
         Space : constant String (1 .. Indent) := (others => ' ');

      begin
         Open (File => Input, Mode => In_File, Name => Filename);

         while not End_Of_File (Input) loop
            declare
               Line : constant String := Get_Line (Input);
            begin
               W (Space & Line, Output => Output);
            end;
         end loop;

         Close (Input);

      exception
         when Ex : others =>
            if Is_Open (Input) then
               Close (Input);
            end if;

            Fatal_Error
              ("inlining failed: " & Filename & ": " &
               Exception_Information (Ex));
      end I;

      ----------------
      -- Write_Hunk --
      ----------------

      procedure Write_Hunk (Hunk : JSON_Value) is
         Filename : constant String := Hunk.Get ("hunk_filename");
         Output   : File_Type;

      begin
         Create_Output_File (Output, Filename);
         Put (Output, "gnatcov.load_hunk(");
         W
           (Item     => Unbounded_String'(Write (Hunk, Compact => True)),
            Output   => Output,
            New_Line => False);
         Put_Line (Output, ");");
         Close (Output);

      exception
         when Ex : others =>
            if Is_Open (Output) then
               Close (Output);
            end if;

            Warn
              ("hunk generation failed: " & Filename & ": " &
               Exception_Information (Ex));

      end Write_Hunk;

      ------------------
      -- Write_Report --
      ------------------

      procedure Write_Report (Filename : String) is
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
         W ("  <script>gnatcov.load_report(", New_Line => False);
         W
           (Item     => Unbounded_String'(Write (Report, Compact => True)),
            New_Line => False);
         W (");</script>");
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

      end Write_Report;

   begin
      for Hunk of Hunks loop
         Write_Hunk (Hunk);
      end loop;

      Write_Report ("index.html");
   end Write_Full_Report;

end Annotations.Dynamic_Html;
