------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2013-2021, AdaCore                     --
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
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;

pragma Warnings (Off, "* is an internal GNAT unit");
   with Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "* is an internal GNAT unit");

with GNATCOLL.JSON; use GNATCOLL.JSON;

with GNAT.Regpat; use GNAT.Regpat;

with Annotations.Html;
with Hex_Images;
with Interfaces;
with Project;
with Outputs;
with Strings;
with Switches;
with Traces_Disa;
with Traces_Files;     use Traces_Files;
with Ada.Directories;  use Ada.Directories;
with Support_Files;    use Support_Files;

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

   GNATquilt_Dir : constant String := Support_Files.In_Lib_Dir ("gnatquilt");

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

      Source_List         : JSON_Array;
      --  The sources array in the index, containing all source mappings

      Title_Prefix        : Ada.Strings.Unbounded.Unbounded_String;
      --  Prefix to use for titles in generated HTML documents
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

   -------------------------------------------------
   -- Wrappers for efficient unbounded string I/O --
   -------------------------------------------------

   --  These wrappers around New_Line and Write use GNAT-specific unbounded
   --  string APIs to avoid copying full strings over the secondary stack.

   procedure NL (Output : Ada.Text_IO.File_Type)
   with Pre => Ada.Text_IO.Is_Open (Output);
   --  Write a New_Line into Output

   procedure W
     (Item     : String;
      Output   : Ada.Text_IO.File_Type;
      New_Line : Boolean := True)
   with Pre => Ada.Text_IO.Is_Open (Output);
   --  Write Item (String) into Output

   procedure W
     (Item     : Ada.Strings.Unbounded.Unbounded_String;
      Output   : Ada.Text_IO.File_Type;
      New_Line : Boolean := True)
   with Pre => Ada.Text_IO.Is_Open (Output);
   --  Write Item (Unbounded String) into Output

   procedure W
     (Item     : JSON_Value;
      Output   : Ada.Text_IO.File_Type;
      New_Line : Boolean := True)
   with Pre => Ada.Text_IO.Is_Open (Output);
   --  Write Item (JSON value) into Output

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

   procedure Write_Full_Report (Pp : Dynamic_Html'Class);
   --  Dump the HTML file into Filename, inlining both JS and CSS resources
   --  (see JS_Source and CSS_Source) and embedding the JSON object as well.

   ---------------
   -- Installed --
   ---------------

   function Installed return Boolean is
   begin
      return Exists (GNATquilt_Dir);
   end Installed;

   ---------------------
   -- Generate_Report --
   ---------------------

   procedure Generate_Report
     (Context      : Coverage.Context_Access;
      Report_Title : Command_Line.Parser.String_Option)
   is
      Pp : Dynamic_Html :=
        (Need_Sources => True,
         Context      => Context,
         Title_Prefix => Annotations.Html.Title_Prefix (Report_Title),
         others       => <>);
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
         Pp.JSON.Set_Field ("coverageLevel", Coverage_Option_Value);
      end Append_Coverage_Info;

      ------------------------
      -- Append_Traces_List --
      ------------------------

      procedure Append_Traces_List is
         procedure Process_Trace (TF : Trace_File_Element);
         --  Generate the record from the TF trace file and append it to the
         --  JSON array Traces.

         -------------------
         -- Process_Trace --
         -------------------

         procedure Process_Trace (TF : Trace_File_Element) is
            Orig_Context : constant String := Original_Processing_Context (TF);
            Trace        : constant JSON_Value := Create_Object;

         begin
            Trace.Set_Field ("filename", TF.Filename);
            Trace.Set_Field ("kind", Image (TF.Kind));
            Trace.Set_Field ("program", TF.Program_Name);
            Trace.Set_Field ("date", TF.Time);
            Trace.Set_Field ("tag", TF.User_Data);

            --  For a trace that has been processed in an earlier run, provide
            --  information on original coverage assessment context.

            if Orig_Context /= "" then
               Trace.Set_Field ("processed", Orig_Context);
            end if;

            Append (Traces, Trace);
         end Process_Trace;

      begin
         Iterate_On_Traces_Files (Process_Trace'Access);
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
      Write_Full_Report (Pp);
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

      Skip := False;

      Source.Set_Field ("missingSource", not Info.Has_Source);
      --  Compute the coverage stats and store them into a JSON dictionary
      Stats.Set_Field ("noCode", Info.Stats (No_Code));
      Stats.Set_Field ("covered", Info.Stats (Covered));
      Stats.Set_Field ("partiallyCovered", Info.Stats (Partially_Covered));
      Stats.Set_Field ("notCovered", Info.Stats (Not_Covered));
      Stats.Set_Field ("notCoverable", Info.Stats (Not_Coverable));
      Stats.Set_Field
        ("exemptedNoViolation", Info.Stats (Exempted_No_Violation));
      Stats.Set_Field
        ("exemptedWithViolation", Info.Stats (Exempted_With_Violation));

      --  Generate the JSON object for this source file

      Source.Set_Field ("filename", Get_Unique_Name (File));
      Source.Set_Field ("hunkFilename", Get_Hunk_Filename (File));
      Source.Set_Field ("coverageLevel", Coverage_Option_Value);
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
      use Ada.Text_IO;

      use Outputs;

      Source        : JSON_Value renames Pp.Current_Source;
      Filename      : constant String := Source.Get ("filename");
      Hunk_Filename : constant String := Source.Get ("hunkFilename");

      Output : File_Type;

      Simplified : constant JSON_Value := Create_Object;
   begin
      if not Is_Empty (Pp.Current_Mappings) then
         Source.Set_Field ("mappings", Pp.Current_Mappings);
      end if;

      --  Dump the JSON object containing the full coverage data for a specific
      --  source file, wrapping it into a function call.

      begin
         Create_Output_File (Output, Hunk_Filename);
         Put (Output, "REPORT[""" & Hunk_Filename & """] = ");
         W
           (Item     => Unbounded_String'(Write (Source, Compact => True)),
            Output   => Output,
            New_Line => False);
         Close (Output);

      exception
         when Ex : others =>
            if Is_Open (Output) then
               Close (Output);
            end if;

            Warn
              ("hunk generation failed: " & Filename & ": " &
               Exception_Information (Ex));
      end;

      --  Append a simplified "reference" entry to the index

      Simplified.Set_Field ("filename", Filename);
      Simplified.Set_Field ("stats", JSON_Value'(Source.Get ("stats")));
      Simplified.Set_Field ("coverageLevel",
                            String'(Source.Get ("coverageLevel")));
      Simplified.Set_Field ("hunkFilename", Hunk_Filename);
      Simplified.Set_Field ("missingSource",
                            Boolean'(Source.Get ("missingSource")));

      if Source.Has_Field ("project") then
         --  Project name is optional. Add it only when relevant

         Simplified.Set_Field
           ("project", String'(Source.Get ("project")));
      end if;

      Append (Pp.Source_List, Simplified);
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
      use Strings;

      Coverage_State : constant String :=
                         (1 => State_Char (Aggregated_State (Info.all)));
      Exempted       : constant Boolean := Info.Exemption /= Slocs.No_Location;

      Mapping  : constant JSON_Value := Create_Object;
      Line_Obj : constant JSON_Value := Create_Object;

   begin
      Clear (Pp.Current_Statements);
      Clear (Pp.Current_Decisions);
      Clear (Pp.Current_Conditions);

      Line_Obj.Set_Field ("lineNumber", Img (Line_Num));
      Line_Obj.Set_Field ("exempted", Exempted'Img);
      Line_Obj.Set_Field ("src", Line);

      Mapping.Set_Field ("coverage", Coverage_State);
      Mapping.Set_Field ("line", Line_Obj);

      Mapping.Set_Field ("messages", Empty_Array);

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
           ("instructionBlocks", Pp.Current_Insn_Blocks);
      end if;

      Pp.Current_Mapping.Set_Field ("instructionSet", Pp.Current_Insn_Set);
   end Pretty_Print_End_Instruction_Set;

   --------------------------
   -- Pretty_Print_Message --
   --------------------------

   procedure Pretty_Print_Message
     (Pp : in out Dynamic_Html;
      M  : Message)
   is
      use Ada.Characters.Handling;

      use Hex_Images;
      use Interfaces;

      Message : constant JSON_Value := Create_Object;

   begin
      Message.Set_Field ("kind", To_Lower (M.Kind'Img));

      if M.PC /= 0 then
         Message.Set_Field ("address", Hex_Image (M.PC));
      end if;

      if M.SCO /= No_SCO_Id then
         Message.Set_Field ("sco", Image (M.SCO, With_Sloc => False));
      end if;

      Message.Set_Field ("message", To_String (M.Msg));
      Pp.Current_Mapping.Get ("messages").Append (Message);
   end Pretty_Print_Message;

   -------
   -- W --
   -------

   procedure W
     (Item     : String;
      Output   : Ada.Text_IO.File_Type;
      New_Line : Boolean := True)
   is
      use Ada.Text_IO;
   begin
      Put (File => Output, Item => Item);

      if New_Line then
         NL (Output => Output);
      end if;
   end W;

   procedure W
     (Item     : Ada.Strings.Unbounded.Unbounded_String;
      Output   : Ada.Text_IO.File_Type;
      New_Line : Boolean := True)
   is
      use Ada.Text_IO;

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

   procedure W
     (Item     : JSON_Value;
      Output   : Ada.Text_IO.File_Type;
      New_Line : Boolean := True)
   is
      Item_US : constant Unbounded_String := Item.Write (Compact => True);
   begin
      W (Item_US, Output, New_Line);
   end W;

   --------
   -- NL --
   --------

   procedure NL (Output : Ada.Text_IO.File_Type) is
      use Ada.Text_IO;
   begin
      New_Line (File => Output);
   end NL;

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

   procedure Write_Full_Report (Pp : Dynamic_Html'Class) is
      use Ada.Text_IO;

      use Outputs;

      Report : JSON_Value renames Pp.JSON;

      HTML : File_Type;
      --  The HTML index page

      JS_Report : File_Type;
      --  The JS report. The procedure Write_Report is in charge of opening
      --  and closing this file. It is declared here for praticality of use of
      --  helper functions in Write_Report (see definitions below).

      --  Report production involves serializing the Report JSON object.
      --  On the one hand, we want to emit a compact output (no extra line
      --  breaks and indentation) to avoid output bloat, but on the other
      --  hand, producing a huge output line is known to create trouble in
      --  tools processing the HTML output.
      --
      --  We adopt a compromise here: we know that huge output lines in the
      --  compact form are due to "sources" and "traces" array attributes in
      --  the Report object, so we specifically emit one item per line for
      --  Report attributes that are arrays, and use the compact formatting
      --  for all other attributes.

      First_JSON_Item : Boolean := True;
      --  Whether Write_JSON_Item has not been called yet

      procedure Write_JSON_Item (Name : UTF8_String; Value : JSON_Value);
      --  Callback for Map_JSON_Object: called to output an attribute pair
      --  for the Report JSON object.

      procedure Copy_And_Fix_Asset (Directory_Entry : Directory_Entry_Type);
      --  Copy the given file to the output directory. For the index.html file,
      --  patch the HTML title according to the --report-title option.

      procedure Replace_Line_In_File
        (In_Filename  : String;
         Out_Filename : String;
         Pattern      : String;
         Replacement  : String);
      --  Write the content of In_Filename to Out_Filename and replace all the
      --  lines containing Pattern in In_Filename by Replacement in
      --  Out_Filename.

      ---------------------
      -- Write_JSON_Item --
      ---------------------

      procedure Write_JSON_Item (Name : UTF8_String; Value : JSON_Value) is
      begin
         if First_JSON_Item then
            First_JSON_Item := False;
            NL (JS_Report);
         else
            W (",", JS_Report);
         end if;

         --  Output the attribute name

         W ("""" & Name & """: ", JS_Report, New_Line => False);

         --  Then output the attribute value. If the attribute is an array,
         --  output one array item per line to avoid too long lines,
         --  otherwise use the regular JSON serialization function.

         if Value.Kind = JSON_Array_Type then
            declare
               Items : constant JSON_Array := Value.Get;
            begin
               W ("[", JS_Report, New_Line => False);
               for J in 1 .. Length (Items) loop
                  if J > 1 then
                     W (",", JS_Report);
                  end if;
                  W (Get (Items, J), JS_Report, New_Line => False);
               end loop;
               W ("]", JS_Report, New_Line => False);
            end;
         else
            W (Value, JS_Report, New_Line => False);
         end if;
      end Write_JSON_Item;

      ------------------------
      -- Copy_And_Fix_Asset --
      ------------------------

      procedure Copy_And_Fix_Asset (Directory_Entry : Directory_Entry_Type)
      is
      begin
         if Kind (Directory_Entry) = Ordinary_File then
            declare
               Source_Name : constant String := Full_Name (Directory_Entry);
               Target_Name : constant String :=
                 Get_Output_Dir & "/" & Simple_Name (Directory_Entry);
            begin
               --  Consider the --report-title option that can change the title
               --  of the HTML generated page.

               if Simple_Name (Directory_Entry) = "index.html" then
                  Replace_Line_In_File
                    (In_Filename  => Source_Name,
                     Out_Filename => Target_Name,
                     Pattern      => "<title>.*</title>",
                     Replacement  => "  <title>" & To_String (Pp.Title_Prefix)
                                     & "GNATcoverage Report</title>");
               else
                  Copy_File (Source_Name, Target_Name);
               end if;
            end;
         end if;
      end Copy_And_Fix_Asset;

      --------------------------
      -- Replace_Line_In_File --
      --------------------------

      procedure Replace_Line_In_File
        (In_Filename  : String;
         Out_Filename : String;
         Pattern      : String;
         Replacement  : String)
      is
         RE          : constant Pattern_Matcher := Compile (Pattern);
         Matches     : Match_Array (0 .. 0);
         Input_File  : File_Type;
         Output_File : File_Type;
      begin
         Open (File => Input_File,
               Mode => In_File,
               Name => In_Filename);
         Create (File => Output_File,
                 Mode => Out_File,
                 Name => Out_Filename);

         while not End_Of_File (Input_File) loop
            declare
               Line : constant String := Get_Line (Input_File);
            begin
               Match (RE, Line, Matches);
               if Matches (0) = No_Match then
                  Put_Line (Output_File, Line);
               else
                  Put_Line (Output_File, Replacement);
               end if;
            end;
         end loop;

         Close (Input_File);
         Close (Output_File);
      end Replace_Line_In_File;

   begin
      Create_Output_File (JS_Report, "report.js");
      W ("var REPORT = {};", JS_Report);
      W ("REPORT [""report.js""] = {", JS_Report);
      Report.Map_JSON_Object (Write_JSON_Item'Access);
      W ("}", JS_Report);
      Close (JS_Report);

      --  Populate the output directory with the needed assets

      Search (Directory => GNATquilt_Dir,
              Pattern   => "*",
              Process   => Copy_And_Fix_Asset'Access);

   exception
      when Ex : others =>
         if Is_Open (JS_Report) then
            Close (JS_Report);
         end if;

         if Is_Open (HTML) then
            Close (HTML);
         end if;

         Fatal_Error
           ("report generation failed: " & Exception_Information (Ex));

   end Write_Full_Report;

end Annotations.Dynamic_Html;
