------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2013-2024, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;         use Ada.Directories;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;

with Annotations.Html;
with Hex_Images;
with Interfaces;
with Outputs;
with Project;
with Support_Files;    use Support_Files;
with Switches;         use Switches;
with Traces_Disa;
with Traces_Files;     use Traces_Files;

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

      Current_SCOs        : JSON_Array;
      --  The SCOs list attached to the line currently being processed. It is
      --  stored in the Current_Mapping by Pretty_Print_End_Line if not empty.

      Current_Decision    : JSON_Value;
      --  The current decision being processed by the builder. It is stored in
      --  Current_SCOs by the Pretty_Print_End_Decision procedure.

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

      Title_Prefix        : Unbounded_String;
      --  Prefix to use for titles in generated HTML documents

      Scope_Metrics       : JSON_Value;
      --  The scoped metrics, e.g. stats for subprograms bodies in a package
      --  body, organized in a tree fashion.

   end record;

   overriding function Format
     (Pp : Dynamic_Html) return Annotation_Format_Family
   is (Annotate_Html);

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

   procedure Pretty_Print_Scope_Entities
     (Pp             : in out Dynamic_Html;
      File           : Source_File_Index;
      Scope_Entities : Scope_Entities_Tree);

   procedure Pretty_Print_Start_Line
     (Pp       : in out Dynamic_Html;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String);

   procedure Pretty_Print_End_Line (Pp : in out Dynamic_Html);

   procedure Pretty_Print_SCO
     (Pp    : in out Dynamic_Html'Class;
      SCO   : SCO_Id;
      State : Line_State;
      Kind  : SCO_Kind);

   procedure Pretty_Print_Statement
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State);

   procedure Pretty_Print_Fun
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State);

   procedure Pretty_Print_Call
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

   procedure Pretty_Print_Start_Symbol
     (Pp     : in out Dynamic_Html;
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
     (Item     : Unbounded_String;
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
      State : Line_State;
      Kind  : SCO_Kind);
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

   function To_JSON (Stats : Counter_Array) return JSON_Value;
   --  JSONify line statistics

   function To_JSON (Ob_Stats : Ob_Stat_Array) return JSON_Array;
   --  JSONify obligation statistics

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
      Annotations.Generate_Report
        (Pp,
         Show_Details => True,
         Subdir       => "html");
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

      Info       : constant File_Info_Access := Get_File (File);
      Source     : constant JSON_Value := Create_Object;
      Line_Stats : constant JSON_Value := Create_Object;

   begin
      Clear (Pp.Current_Mappings);
      Clear (Pp.Current_SCOs);
      Clear (Pp.Current_Conditions);
      Pp.Scope_Metrics := JSON_Null;

      Skip := False;

      Source.Set_Field ("missingSource", not Info.Has_Source);

      --  Compute the coverage stats and store them into a JSON dictionary

      Line_Stats.Set_Field ("noCode", Info.Li_Stats (No_Code));
      Line_Stats.Set_Field ("covered", Info.Li_Stats (Covered));
      Line_Stats.Set_Field
        ("partiallyCovered", Info.Li_Stats (Partially_Covered));
      Line_Stats.Set_Field ("notCovered", Info.Li_Stats (Not_Covered));
      Line_Stats.Set_Field ("notCoverable", Info.Li_Stats (Not_Coverable));
      Line_Stats.Set_Field
        ("undeterminedCoverage", Info.Li_Stats (Undetermined_Coverage));
      Line_Stats.Set_Field
        ("disabledCoverage", Info.Li_Stats (Disabled_Coverage));
      Line_Stats.Set_Field
        ("exemptedNoViolation", Info.Li_Stats (Exempted_No_Violation));
      Line_Stats.Set_Field
        ("exemptedWithViolation", Info.Li_Stats (Exempted_With_Violation));
      Line_Stats.Set_Field
        ("exemptedWithUndetCov",
         Info.Li_Stats (Exempted_With_Undetermined_Cov));

      --  Generate the JSON object for this source file

      Source.Set_Field ("filename", Get_Unique_Name (File));
      Source.Set_Field ("hunkFilename", Get_Hunk_Filename (File));
      Source.Set_Field ("coverageLevel", Coverage_Option_Value);
      Source.Set_Field ("liStats", Line_Stats);
      Source.Set_Field ("enAllStats", To_JSON (Info.Ob_Stats));

      --  Set the language for this source file

      if Is_Project_Loaded then

         --  If the project was loaded, get the language information from it

         case Language (Info.Full_Name.all) is
            when Ada_Language =>
               Source.Set_Field ("language", "ada");
            when C_Language =>
               Source.Set_Field ("language", "c");
            when CPP_Language =>
               Source.Set_Field ("language", "cpp");
            when All_Languages =>
               null;
         end case;
      else
         --  If no project was loaded, infer the language from the
         --  source extension.

         declare
            Ext : constant String := Extension (Info.Simple_Name.all);
         begin
            if Ext in "adb" | "ads" | "ada.1" | "ada.2" then
               Source.Set_Field ("language", "ada");
            elsif Ext in "c" | "h" then
               Source.Set_Field ("language", "c");
            elsif Ext in "cpp" | "cc" | "hpp" then
               Source.Set_Field ("language", "cpp");
            end if;
         end;
      end if;

      declare
         P_Name : constant String :=
           (if Is_Project_Loaded
            then Project_Name (Info.Full_Name.all)
            else "");
         --  Note that P_Name can be "" here either because we don't have a
         --  root project at all or because we were unable to find the project
         --  to which the source pertains.

      begin
         Source.Set_Field
           ("project",
            (if P_Name /= "" then P_Name else "Other Sources"));
      end;

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

      Source.Set_Field ("scopeMetrics", Pp.Scope_Metrics);

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
      Simplified.Set_Field ("liStats", JSON_Value'(Source.Get ("liStats")));
      Simplified.Set_Field
        ("enAllStats", JSON_Value'(Source.Get ("enAllStats")));
      Simplified.Set_Field
        ("coverageLevel", String'(Source.Get ("coverageLevel")));
      Simplified.Set_Field ("hunkFilename", Hunk_Filename);
      Simplified.Set_Field
        ("missingSource", Boolean'(Source.Get ("missingSource")));

      if Source.Has_Field ("project") then
         --  Project name is optional. Add it only when relevant

         Simplified.Set_Field
           ("project", String'(Source.Get ("project")));
      end if;

      Append (Pp.Source_List, Simplified);
   end Pretty_Print_End_File;

   ---------------------------------
   -- Pretty_Print_Scope_Entities --
   ---------------------------------

   procedure Pretty_Print_Scope_Entities
     (Pp             : in out Dynamic_Html;
      File           : Source_File_Index;
      Scope_Entities : Scope_Entities_Tree)
   is
      use Scope_Entities_Trees;

      function To_JSON
        (Cur     : Cursor;
         Is_Root : Boolean := False) return JSON_Value;
      --  Convert a scope entity to a JSON scoped metric: compute line and
      --  obligation statistics for the given scope and recursively for
      --  child scopes. Is_Root indicates whether the given Cur is the root
      --  scope. Store the result as a JSON object, with the name and the line
      --  of the scope.

      -------------
      -- To_JSON --
      -------------

      function To_JSON
        (Cur     : Cursor;
         Is_Root : Boolean := False) return JSON_Value
      is
         Scope_Ent : constant Scope_Entity := Element (Cur);
         Child     : Cursor := First_Child (Cur);

         Scope_Metrics_JSON          : constant JSON_Value := Create_Object;
         Children_Scope_Metrics_JSON : JSON_Array;
         --  Representation of the scope metrics for the html format

         File_Info  : constant File_Info_Access := Get_File (File);
         Line_Stats : constant Li_Stat_Array :=
           Line_Metrics
             (File_Info,
              Scope_Ent.Source_Range.L.First_Sloc.Line,
              (if Is_Root then Last_Line (File_Info)
               else Scope_Ent.Source_Range.L.Last_Sloc.Line));
         --  Adjust Scope_Ent.End_Sloc for the root node as it is
         --  No_Local_Location by default.

         Ob_Stats : constant Ob_Stat_Array :=
           Obligation_Metrics (Get_SCOs (Scope_Ent.Source_Range));
      begin
         Scope_Metrics_JSON.Set_Field ("scopeName", Scope_Ent.Name);
         Scope_Metrics_JSON.Set_Field ("scopeLine", Scope_Ent.Sloc.Line);
         Scope_Metrics_JSON.Set_Field ("stats", To_JSON (Line_Stats));
         Scope_Metrics_JSON.Set_Field ("enAllStats", To_JSON (Ob_Stats));
         while Has_Element (Child) loop
            Append (Children_Scope_Metrics_JSON, To_JSON (Child));
            Child := Next_Sibling (Child);
         end loop;
         Scope_Metrics_JSON.Set_Field
           ("children", Create (Children_Scope_Metrics_JSON));
         return Scope_Metrics_JSON;
      end To_JSON;

   begin
      for Cur in Scope_Entities.Iterate_Children (Scope_Entities.Root) loop
         Pp.Scope_Metrics := To_JSON (Cur, Is_Root => True);
      end loop;
   end Pretty_Print_Scope_Entities;

   -----------------------------
   -- Pretty_Print_Start_Line --
   -----------------------------

   procedure Pretty_Print_Start_Line
     (Pp       : in out Dynamic_Html;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String)
   is
      Coverage_State : constant String :=
                         (1 => State_Char (Aggregated_State (Info.all)));

      Mapping  : constant JSON_Value := Create_Object;
      Line_Obj : constant JSON_Value := Create_Object;

   begin
      Clear (Pp.Current_SCOs);
      Clear (Pp.Current_Conditions);

      Line_Obj.Set_Field ("lineNumber", Line_Num);
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
      if not Is_Empty (Pp.Current_SCOs) then
         Pp.Current_Mapping.Set_Field ("scos", Pp.Current_SCOs);
      end if;
      Append (Pp.Current_Mappings, Pp.Current_Mapping);
   end Pretty_Print_End_Line;

   ----------------------
   -- Pretty_Print_SCO --
   ----------------------

   procedure Pretty_Print_SCO
     (Pp    : in out Dynamic_Html'Class;
      SCO   : SCO_Id;
      State : Line_State;
      Kind  : SCO_Kind)
   is
      SCO_JSON : constant JSON_Value := Create_Object;
   begin
      Set_SCO_Fields (SCO_JSON, SCO, State, Kind);
      Append (Pp.Current_SCOs, SCO_JSON);
   end Pretty_Print_SCO;

   ----------------------------
   -- Pretty_Print_Statement --
   ----------------------------

   procedure Pretty_Print_Statement
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State) is
   begin
      Pretty_Print_SCO (Pp, SCO, State, Statement);
   end Pretty_Print_Statement;

   ----------------------
   -- Pretty_Print_Fun --
   ----------------------

   procedure Pretty_Print_Fun
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State) is
   begin
      Pretty_Print_SCO (Pp, SCO, State, Fun);
   end Pretty_Print_Fun;

   -----------------------
   -- Pretty_Print_Call --
   -----------------------

   procedure Pretty_Print_Call
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State) is
   begin
      Pretty_Print_SCO (Pp, SCO, State, Call);
   end Pretty_Print_Call;

   ---------------------------------
   -- Pretty_Print_Start_Decision --
   ---------------------------------

   procedure Pretty_Print_Start_Decision
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State)
   is
      Decision_JSON : constant JSON_Value := Create_Object;
      Conditions    : JSON_Array;
   begin
      Clear (Pp.Current_Conditions);
      Set_SCO_Fields (Decision_JSON, SCO, State, Decision);
      Pp.Current_Conditions := Conditions;
      Pp.Current_Decision := Decision_JSON;
   end Pretty_Print_Start_Decision;

   -------------------------------
   -- Pretty_Print_End_Decision --
   -------------------------------

   procedure Pretty_Print_End_Decision (Pp : in out Dynamic_Html) is
   begin
      if not Is_Empty (Pp.Current_Conditions) then
         Pp.Current_Decision.Set_Field ("conditions", Pp.Current_Conditions);
      end if;
      Append (Pp.Current_SCOs, Pp.Current_Decision);
   end Pretty_Print_End_Decision;

   ----------------------------
   -- Pretty_Print_Condition --
   ----------------------------

   procedure Pretty_Print_Condition
     (Pp    : in out Dynamic_Html;
      SCO   : SCO_Id;
      State : Line_State)
   is
      Condition_JSON : constant JSON_Value := Create_Object;
   begin
      Set_SCO_Fields (Condition_JSON, SCO, State, Condition);
      Append (Pp.Current_Conditions, Condition_JSON);
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

      Message.Set_Field ("message", +M.Msg);
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
     (Item     : Unbounded_String;
      Output   : Ada.Text_IO.File_Type;
      New_Line : Boolean := True)
   is
      use Ada.Text_IO.Unbounded_IO;
   begin
      Put (File => Output, Item => Item);

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
      State : Line_State;
      Kind  : SCO_Kind)
   is
      JSON_Annotations : JSON_Array;
   begin
      Obj.Set_Field ("id", Img (Integer (SCO)));
      Obj.Set_Field ("text", SCO_Image (SCO));
      Obj.Set_Field ("coverage", String'(1 => State_Char (State)));
      Obj.Set_Field ("range", Src_Range (SCO));
      for Annotation of SCO_Annotations (SCO) loop
         Append (JSON_Annotations, Create (Annotation));
      end loop;
      Obj.Set_Field ("annotations", JSON_Annotations);
      Obj.Set_Field ("kind", To_Lower (Kind'Image));
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

   -------------
   -- To_JSON --
   -------------

   function To_JSON (Stats : Counter_Array) return JSON_Value is

      Line_Stats : constant JSON_Value := Create_Object;

      procedure Set_If_Not_Null (Field : String; Stat : Natural);
      --  Wrapper around Line_Stats.Set_Field. Set Field to Stat if Stat is not
      --  null, do nothing otherwise.

      ---------------------
      -- Set_If_Not_Null --
      ---------------------

      procedure Set_If_Not_Null (Field : String; Stat : Natural) is
      begin
         if Stat > 0 then
            Line_Stats.Set_Field (Field, Stat);
         end if;
      end Set_If_Not_Null;

   --  Start of processing for To_JSON

   begin
      Set_If_Not_Null ("noCode", Stats (No_Code));
      Set_If_Not_Null ("covered", Stats (Covered));
      Set_If_Not_Null ("partiallyCovered", Stats (Partially_Covered));
      Set_If_Not_Null ("notCovered", Stats (Not_Covered));
      Set_If_Not_Null ("notCoverable", Stats (Not_Coverable));
      Set_If_Not_Null ("undeterminedCoverage", Stats (Undetermined_Coverage));
      Set_If_Not_Null ("disabledCoverage", Stats (Disabled_Coverage));
      Set_If_Not_Null ("exemptedNoViolation", Stats (Exempted_No_Violation));
      Set_If_Not_Null
        ("exemptedWithViolation", Stats (Exempted_With_Violation));
      Set_If_Not_Null
        ("exemptedWithUndetCov", Stats (Exempted_With_Undetermined_Cov));
      return Line_Stats;
   end To_JSON;

   -------------
   -- To_JSON --
   -------------

   function To_JSON (Ob_Stats : Ob_Stat_Array) return JSON_Array is

      use Coverage;

      Ob_Stats_JSON : JSON_Array;

      procedure Add_Ob_Stats
        (Level            : Coverage_Level;
         Obligation_Stats : SCO_Tally);
      --  Add to Obligation_Stats_Object the Obligation_Stats that correspond
      --  to the coverage level Level.

      ------------------
      -- Add_Ob_Stats --
      ------------------

      procedure Add_Ob_Stats
        (Level            : Coverage_Level;
         Obligation_Stats : SCO_Tally)
      is
         Level_Stats  : constant JSON_Value :=
           To_JSON (Obligation_Stats.Stats);
         Stats_Holder : constant JSON_Value := Create_Object;
      begin
         Stats_Holder.Set_Field ("stats", Level_Stats);
         Stats_Holder.Set_Field ("level", Image (Level));
         Append (Ob_Stats_JSON, Stats_Holder);
      end Add_Ob_Stats;
   begin
      --  Object coverage does not come with coverage obligations on the
      --  assembly instructions, so there would be no point in enabling
      --  reporting on obligations for object coverage reports.

      for Level of Source_Levels_Enabled loop
         Add_Ob_Stats (Level, Ob_Stats (Level));
      end loop;
      return Ob_Stats_JSON;
   end To_JSON;

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
                 Get_Output_Dir & GNAT.OS_Lib.Directory_Separator
                 & Simple_Name (Directory_Entry);
            begin
               --  Consider the --report-title option that can change the title
               --  of the HTML generated page.

               if Simple_Name (Directory_Entry) = "index.html" then
                  Replace_Line_In_File
                    (In_Filename  => Source_Name,
                     Out_Filename => Target_Name,
                     Pattern      => "<title>.*</title>",
                     Replacement  => "  <title>" & (+Pp.Title_Prefix)
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
