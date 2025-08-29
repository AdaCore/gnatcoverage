------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
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
with Ada.Directories;
with Ada.Text_IO;             use Ada.Text_IO;
with Interfaces;

with GNAT.OS_Lib;

with Coverage;      use Coverage;
with Hex_Images;    use Hex_Images;
with Outputs;       use Outputs;
with Support_Files;
with Traces_Disa;   use Traces_Disa;
with Traces_Files;  use Traces_Files;

package body Annotations.Xml is

   XSD_Basename : constant String := "gnatcov-xml-report.xsd";
   XSD_Filename : constant String := Support_Files.In_Lib_Dir (XSD_Basename);

   type Printing_Destination is
     --  Classes of possible output destinations for an XML pretty printer

     (Dest_Index,
      --  refers to the XML index

      Dest_Trace_Info,
      --  refers to trace info (trace.xml)

      Dest_Compilation_Unit
      --  When going through the source file list, refers to the xml file
      --  that corresponds to the source file being processed.
      --  e.g. hello.adb.xml for hello.adb.
      );

   type File_Array is array (Printing_Destination) of File_Type;
   --  Array of handle for each destination of an XML pretty printer

   type Indentation_Array is array (Printing_Destination) of Natural;
   --  Array to record the number of spaces to indent before the next
   --  lines in the corresponding destination

   type Xml_Pretty_Printer is new Pretty_Printer with record
      --  Pretty printer type for the XML annotation format

      Files        : File_Array;
      --  Handle to destination files

      Indentations : Indentation_Array := (others => 0);
      --  Number of space characters to indent for each destination file,
      --  for the forthcoming lines
   end record;

   overriding function Format
     (Pp : Xml_Pretty_Printer) return Annotation_Format_Family
   is (Annotate_Xml);

   --------------------
   -- XML generation --
   --------------------

   Xml_Header      : constant String := "<?xml version=""1.0"" ?>";
   Index_File_Name : constant String := "index.xml";
   Trace_File_Name : constant String := "trace.xml";

   function A (Name : String; Value : Character) return String;
   function A (Name : String; Value : Unbounded_String) return String;
   function A (Name : String; Value : String) return String;
   --  Return a string representing an xml attribute whose name
   --  and value are given in parameter. id est
   --   Name = "Value"

   procedure T
     (Pp         : in out Xml_Pretty_Printer'Class;
      Name       : String;
      Attributes : String;
      Dest       : Printing_Destination := Dest_Compilation_Unit);
   --  Print a string representing an empty tag whose name and
   --  attributes are given in parameter. id est
   --  <Name Attributes/>

   procedure ST
     (Pp         : in out Xml_Pretty_Printer'Class;
      Name       : String;
      Attributes : String;
      Dest       : Printing_Destination := Dest_Compilation_Unit);
   --  Print a string representing a start tag whose name and attributes
   --  are given in parameter. id est
   --  <Name Attributes>

   procedure ST
     (Pp   : in out Xml_Pretty_Printer'Class;
      Name : String;
      Dest : Printing_Destination := Dest_Compilation_Unit);
   --  Same as ST, with no attributes.

   procedure ET
     (Pp   : in out Xml_Pretty_Printer'Class;
      Name : String;
      Dest : Printing_Destination := Dest_Compilation_Unit);
   --  Print a string representing an end tag whose name is given
   --  in parameter. id est
   --  </Name>

   -----------------------------------------------
   -- Xml_Pretty_Printer's primitive operations --
   --    (inherited from Pretty_Printer)        --
   -----------------------------------------------

   procedure Pretty_Print_Start
     (Pp : in out Xml_Pretty_Printer);

   procedure Pretty_Print_End
     (Pp : in out Xml_Pretty_Printer);

   procedure Pretty_Print_Start_File
     (Pp   : in out Xml_Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean);

   procedure Pretty_Print_End_File (Pp : in out Xml_Pretty_Printer);

   procedure Pretty_Print_Scope_Entities
     (Pp             : in out Xml_Pretty_Printer;
      File           : Source_File_Index;
      Scope_Entities : Scope_Entities_Tree);

   procedure Pretty_Print_Start_Line
     (Pp       : in out Xml_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String);

   procedure Pretty_Print_End_Line (Pp : in out Xml_Pretty_Printer);

   procedure Pretty_Print_Start_Instruction_Set
     (Pp    : in out Xml_Pretty_Printer;
      State : Any_Line_State);

   procedure Pretty_Print_End_Instruction_Set
     (Pp : in out Xml_Pretty_Printer);

   procedure Pretty_Print_Start_Symbol (Pp     : in out Xml_Pretty_Printer;
                                        Name   : String;
                                        Offset : Pc_Type;
                                        State  : Line_State);

   procedure Pretty_Print_End_Symbol (Pp : in out Xml_Pretty_Printer);

   procedure Pretty_Print_Insn
     (Pp       : in out Xml_Pretty_Printer;
      Pc       : Pc_Type;
      State    : Insn_State;
      Insn     : Binary_Content;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class);

   procedure Pretty_Print_Message
     (Pp : in out Xml_Pretty_Printer;
      M  : Message);

   procedure Pretty_Print_Statement
     (Pp    : in out Xml_Pretty_Printer;
      SCO   : SCO_Id;
      State : Line_State);

   procedure Pretty_Print_Start_Decision
     (Pp    : in out Xml_Pretty_Printer;
      SCO   : SCO_Id;
      State : Line_State);

   procedure Pretty_Print_End_Decision (Pp : in out Xml_Pretty_Printer);

   procedure Pretty_Print_Condition
     (Pp    : in out Xml_Pretty_Printer;
      SCO   : SCO_Id;
      State : Line_State);

   procedure Print_Coverage_Stats
     (Pp         : in out Xml_Pretty_Printer'Class;
      Stats      : Counter_Array;
      Total_Kind : String;
      Dest       : Printing_Destination);
   --  Emit a series of <metric> tags to Pp to describe the given statistics.
   --  Total_String is the string emitted for the total kind attributes, to
   --  have <metric kind="total_lines_of_relevance"/> for line statistics, and
   --  <metric kind="total_obligations_of_relevance"/> for obligation
   --  statistics.

   procedure Print_Coverage_Li_Stats
     (Pp       : in out Xml_Pretty_Printer'Class;
      Li_Stats : Li_Stat_Array;
      Dest     : Printing_Destination);
   --  Emit a series of <obligation_statistics> tags to Pp to describe the
   --  given statistics

   procedure Print_Coverage_Ob_Stats
     (Pp       : in out Xml_Pretty_Printer'Class;
      Ob_Stats : Ob_Stat_Array;
      Dest     : Printing_Destination);
   --  Emit a series of <obligation_statistics> tags to Pp to describe the
   --  given statistics

   ---------------
   -- Installed --
   ---------------

   function Installed return Boolean is
   begin
      return Ada.Directories.Exists (XSD_Filename);
   end Installed;

   -----------------------------
   -- Shortcut for Put_Line's --
   -----------------------------

   procedure P
     (Pp   : Xml_Pretty_Printer'Class;
      S    : String;
      Dest : Printing_Destination := Dest_Compilation_Unit);
   --  Put_Line S in the destination file

   procedure Src_Block
     (Pp         : in out Xml_Pretty_Printer'Class;
      Sloc_Start : Source_Location;
      Sloc_End   : Source_Location);
   --  Emit a <src>...</src> block for the range Sloc_Start .. Sloc_End

   procedure Print_Trace_Info (Pp : in out Xml_Pretty_Printer'Class);
   --  Generate trace info file (trace.xml)

   -------
   -- A --
   -------

   function A (Name : String; Value : String) return String is
   begin
      return " " & Name & "=" & '"' & To_Xml_String (Value) & '"';
   end A;

   function A (Name : String; Value : Unbounded_String) return String is
   begin
      return A (Name, +Value);
   end A;

   function A (Name : String; Value : Character) return String is
      Value_String : constant String := Value & "";
   begin
      return A (Name, Value_String);
   end A;

   --------
   -- ET --
   --------

   procedure ET
     (Pp   : in out Xml_Pretty_Printer'Class;
      Name : String;
      Dest : Printing_Destination := Dest_Compilation_Unit) is
   begin
      Pp.Indentations (Dest) := Pp.Indentations (Dest) - 1;
      Pp.P ("</" & Name & ">", Dest);
      New_Line (Pp.Files (Dest));
   end ET;

   ---------------------
   -- Generate_Report --
   ---------------------

   procedure Generate_Report (Context : Coverage.Context_Access) is
      Pp : Xml_Pretty_Printer :=
        (Need_Sources => True,
         Use_UTF8     => True,
         Context      => Context,
         others       => <>);
   begin
      Annotations.Generate_Report
        (Pp, True, Subdir => "xml");
   end Generate_Report;

   -------
   -- P --
   -------

   procedure P
     (Pp   : Xml_Pretty_Printer'Class;
      S    : String;
      Dest : Printing_Destination := Dest_Compilation_Unit)
   is
      Spaces : constant String (1 .. Pp.Indentations (Dest)) :=
                 (others => ' ');
   begin
      Put_Line (Pp.Files (Dest), Spaces & S);
   end P;

   ----------------------------
   -- Pretty_Print_Condition --
   ----------------------------

   procedure Pretty_Print_Condition
     (Pp    : in out Xml_Pretty_Printer;
      SCO   : SCO_Id;
      State : Line_State)
   is
      Sloc_Start : constant Source_Location := First_Sloc (SCO);
      Sloc_End   : constant Source_Location :=
                     End_Lex_Element (Last_Sloc (SCO));
   begin
      Pp.ST ("condition",
             A ("id", Img (Integer (SCO)))
             & A ("text", Pp.SCO_Text (SCO))
             & A ("coverage", State_Char (State)));
      for Annotation of SCO_Annotations (SCO) loop
         Pp.T ("annotation", A ("text", Annotation));
      end loop;
      Pp.Src_Block (Sloc_Start, Sloc_End);
      Pp.ET ("condition");
   end Pretty_Print_Condition;

   ----------------------
   -- Pretty_Print_End --
   ----------------------

   procedure Pretty_Print_End
     (Pp : in out Xml_Pretty_Printer)
   is
   begin
      Pp.ET ("sources", Dest_Index);
      Pp.ET ("coverage_report", Dest_Index);
      Pp.ET ("document", Dest_Index);
      Close (Pp.Files (Dest_Index));
   end Pretty_Print_End;

   -------------------------------
   -- Pretty_Print_End_Decision --
   -------------------------------

   procedure Pretty_Print_End_Decision
     (Pp : in out Xml_Pretty_Printer) is
   begin
      Pp.ET ("decision");
   end Pretty_Print_End_Decision;

   ---------------------------
   -- Pretty_Print_End_File --
   ---------------------------

   procedure Pretty_Print_End_File (Pp : in out Xml_Pretty_Printer) is
   begin
      Pp.ET ("source");
      Close (Pp.Files (Dest_Compilation_Unit));
   end Pretty_Print_End_File;

   -----------------------
   -- Pretty_Print_Insn --
   -----------------------

   procedure Pretty_Print_Insn
     (Pp       : in out Xml_Pretty_Printer;
      Pc       : Pc_Type;
      State    : Insn_State;
      Insn     : Binary_Content;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class) is
   begin
      Pp.T ("instruction",
            A ("address", Hex_Image (Pc))
            & A ("coverage", Insn_State_Char (State) & "")
            & A ("assembly", Disassemble (Insn, Pc, Insn_Set, Sym)));
   end Pretty_Print_Insn;

   --------------------------------------
   -- Pretty_Print_End_Instruction_Set --
   --------------------------------------

   procedure Pretty_Print_End_Instruction_Set
     (Pp : in out Xml_Pretty_Printer) is
   begin
      Pp.ET ("instruction_set");
   end Pretty_Print_End_Instruction_Set;

   ---------------------------
   -- Pretty_Print_End_Line --
   ---------------------------

   procedure Pretty_Print_End_Line (Pp : in out Xml_Pretty_Printer) is
   begin
      Pp.ET ("src_mapping");
   end Pretty_Print_End_Line;

   -----------------------------
   -- Pretty_Print_End_Symbol --
   -----------------------------

   procedure Pretty_Print_End_Symbol (Pp : in out Xml_Pretty_Printer) is
   begin
      Pp.ET ("instruction_block");
   end Pretty_Print_End_Symbol;

   --------------------------
   -- Pretty_Print_Message --
   --------------------------

   procedure Pretty_Print_Message
     (Pp : in out Xml_Pretty_Printer;
      M  : Message)
   is
      use Interfaces;
      Attributes : Unbounded_String := +A ("kind", To_Lower (M.Kind'Img));
   begin
      if M.PC /= 0 then
         Append (Attributes, A ("address", Hex_Image (M.PC)));
      end if;

      if M.SCO /= No_SCO_Id then
         Append (Attributes, A ("SCO", Image (M.SCO, With_Sloc => False)));
      end if;

      Append (Attributes, A ("message", +M.Msg));
      Pp.T ("message", +Attributes);
   end Pretty_Print_Message;

   ------------------------
   -- Pretty_Print_Start --
   ------------------------

   procedure Pretty_Print_Start (Pp : in out Xml_Pretty_Printer) is
      Success : Boolean;
   begin
      --  Copy the XML Schema to the output directory

      GNAT.OS_Lib.Copy_File
        (Name     => XSD_Filename,
         Pathname => Get_Output_Dir,
         Success  => Success,
         Mode     => GNAT.OS_Lib.Overwrite);
      if not Success then
         Fatal_Error
           ("Error while copying " & XSD_Filename
            & " to the output directory");
      end if;

      Create_Output_File (Pp.Files (Dest_Index), Index_File_Name);

      Pp.P (Xml_Header, Dest_Index);
      Pp.ST ("document",
             A ("xmlns:xi", "http://www.w3.org/2001/XInclude")
             & A ("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
             & A ("xsi:noNamespaceSchemaLocation", "gnatcov-xml-report.xsd"),
             Dest_Index);
      Pp.ST ("coverage_report",
             A ("coverage_level", Coverage_Option_Value), Dest_Index);

      Pp.ST ("coverage_info", Dest_Index);
      Print_Trace_Info (Pp);
      Pp.T ("xi:include", A ("parse", "xml") & A ("href", Trace_File_Name),
            Dest_Index);
      Pp.ET ("coverage_info", Dest_Index);

      declare

         procedure Process_One_File (File_Index : Source_File_Index);

         ----------------------
         -- Process_One_File --
         ----------------------

         procedure Process_One_File (File_Index : Source_File_Index) is
            FI : constant File_Info_Access := Get_File (File_Index);
         begin
            if FI.Kind = Source_File and then To_Display (FI) then
               Pp.ST
                 ("file", A ("name", FI.Preserved_Full_Name.all), Dest_Index);
               Print_Coverage_Li_Stats (Pp, FI.Li_Stats, Dest_Index);
               Print_Coverage_Ob_Stats (Pp, FI.Ob_Stats, Dest_Index);
               Pp.ET ("file", Dest_Index);
            end if;
         end Process_One_File;

      begin
         Pp.ST ("coverage_summary", Dest_Index);
         Print_Coverage_Li_Stats (Pp, Global_Stats, Dest_Index);
         Print_Coverage_Ob_Stats (Pp, Global_Ob_Stats, Dest_Index);
         Files_Table_Iterate (Process_One_File'Access);
         Pp.ET ("coverage_summary", Dest_Index);
      end;

      Pp.ST ("sources", Dest_Index);
   end Pretty_Print_Start;

   ---------------------------------
   -- Pretty_Print_Start_Decision --
   ---------------------------------

   procedure Pretty_Print_Start_Decision
     (Pp    : in out Xml_Pretty_Printer;
      SCO   : SCO_Id;
      State : Line_State)
   is
      Sloc_Start : constant Source_Location := First_Sloc (SCO);
      Sloc_End   : constant Source_Location :=
                     End_Lex_Element (Last_Sloc (SCO));
   begin
      Pp.ST ("decision",
             A ("id", Img (Integer (SCO)))
             & A ("text", Pp.SCO_Text (SCO))
             & A ("coverage", State_Char (State)));
      for Annotation of SCO_Annotations (SCO) loop
         Pp.T ("annotation", A ("text", Annotation));
      end loop;
      Pp.Src_Block (Sloc_Start, Sloc_End);
   end Pretty_Print_Start_Decision;

   -----------------------------
   -- Pretty_Print_Start_File --
   -----------------------------

   procedure Pretty_Print_Start_File
     (Pp   : in out Xml_Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean)
   is
      Info : constant File_Info_Access := Get_File (File);
      --  No stats are emitted in the XML output; the user is supposed
      --  to compute them by himself by post-processing the output.

      Abs_Filename  : constant String := Info.Preserved_Full_Name.all;
      Xml_File_Name : constant String := Get_Unique_Filename (File, "xml");
   begin
      if not Info.Has_Source then
         Skip := True;
         return;
      end if;

      Skip := False;
      Create_Output_File (Pp.Files (Dest_Compilation_Unit), Xml_File_Name);
      Pp.P (Xml_Header);
      Pp.ST ("source",
             A ("file", Abs_Filename)
             & A ("coverage_level", Coverage_Option_Value));

      Pp.T ("xi:include", A ("parse", "xml") & A ("href", Xml_File_Name),
            Dest_Index);
   end Pretty_Print_Start_File;

   ---------------------------------
   -- Pretty_Print_Scope_Entities --
   ---------------------------------

   procedure Pretty_Print_Scope_Entities
     (Pp             : in out Xml_Pretty_Printer;
      File           : Source_File_Index;
      Scope_Entities : Scope_Entities_Tree)
   is
      use Scope_Entities_Trees;

      procedure Pp_Scope_Entity (Cur : Cursor; Is_Root : Boolean := False);

      ---------------------
      -- Pp_Scope_Entity --
      ---------------------

      procedure Pp_Scope_Entity (Cur : Cursor; Is_Root : Boolean := False)
      is
         Scope_Ent : constant Scope_Entity := Element (Cur);
         Child     : Cursor := First_Child (Cur);

         File_Info  : constant File_Info_Access := Get_File (File);
         Line_Stats : constant Li_Stat_Array :=
           Line_Metrics
             (File_Info,
              Scope_Ent.Source_Range.L.First_Sloc.Line,
              (if Is_Root then Last_Line (File_Info)
               else Scope_Ent.Source_Range.L.Last_Sloc.Line));
         --  Adjust Scope_Ent.End_Sloc for the root node as it is
         --  No_Local_Location by default.
      begin
         Pp.ST ("scope_metric",
                A ("scope_name", Scope_Ent.Name)
                & A ("scope_line", Img (Scope_Ent.Sloc.Line)));
         Print_Coverage_Li_Stats (Pp, Line_Stats, Dest_Compilation_Unit);
         Print_Coverage_Ob_Stats
           (Pp,
            Obligation_Metrics (Get_SCOs (Scope_Ent.Source_Range)),
            Dest_Compilation_Unit);
         while Has_Element (Child) loop
            Pp_Scope_Entity (Child);
            Child := Next_Sibling (Child);
         end loop;
         Pp.ET ("scope_metric");
      end Pp_Scope_Entity;

   begin
      for Cur in Scope_Entities.Iterate_Children (Scope_Entities.Root) loop
         Pp_Scope_Entity (Cur, Is_Root => True);
      end loop;
   end Pretty_Print_Scope_Entities;

   ----------------------------------------
   -- Pretty_Print_Start_Instruction_Set --
   ----------------------------------------

   procedure Pretty_Print_Start_Instruction_Set
     (Pp    : in out Xml_Pretty_Printer;
      State : Any_Line_State) is
   begin
      Pp.ST ("instruction_set", A ("coverage", State_Char (State)));
   end Pretty_Print_Start_Instruction_Set;

   -----------------------------
   -- Pretty_Print_Start_Line --
   -----------------------------

   procedure Pretty_Print_Start_Line
     (Pp       : in out Xml_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String)
   is
      Coverage_State : constant String :=
        (1 => State_Char (Aggregated_State (Info.all)));
   begin
      Pp.ST ("src_mapping", A ("coverage", Coverage_State));
      Pp.ST ("src");
      Pp.T ("line",
            A ("num", Img (Line_Num))
            & A ("src", Line));
      Pp.ET ("src");
   end Pretty_Print_Start_Line;

   -------------------------------
   -- Pretty_Print_Start_Symbol --
   -------------------------------

   procedure Pretty_Print_Start_Symbol
     (Pp     : in out Xml_Pretty_Printer;
      Name   : String;
      Offset : Pc_Type;
      State  : Line_State)
   is
      Coverage_State : constant String := State_Char (State) & "";
   begin
      Pp.ST ("instruction_block",
             A ("name", Name)
             & A ("offset", Hex_Image (Offset))
             & A ("coverage", Coverage_State));
   end Pretty_Print_Start_Symbol;

   ----------------------------
   -- Pretty_Print_Statement --
   ----------------------------

   procedure Pretty_Print_Statement
     (Pp    : in out Xml_Pretty_Printer;
      SCO   : SCO_Id;
      State : Line_State)
   is
      Sloc_Start : constant Source_Location := First_Sloc (SCO);
      Sloc_End   : constant Source_Location :=
        End_Lex_Element (Last_Sloc (SCO));
   begin
      Pp.ST ("statement",
             A ("id", Img (Integer (SCO)))
             & A ("text", Pp.SCO_Text (SCO))
             & A ("coverage", State_Char (State)));
      for Annotation of SCO_Annotations (SCO) loop
         Pp.T ("annotation", A ("text", Annotation));
      end loop;
      Pp.Src_Block (Sloc_Start, Sloc_End);
      Pp.ET ("statement");
   end Pretty_Print_Statement;

   ----------------------
   -- Print_Trace_Info --
   ----------------------

   procedure Print_Trace_Info (Pp : in out Xml_Pretty_Printer'Class) is

      procedure Process_One_Trace (TF : Trace_File_Element);
      --  Print the data attached to the TF trace file to trace.xml

      -----------------------
      -- Process_One_Trace --
      -----------------------

      procedure Process_One_Trace (TF : Trace_File_Element) is
         Attributes : Unbounded_String;
      begin
         Append (Attributes,
                 A ("filename", TF.Filename)
                 & A ("kind", Image (TF.Kind))
                 & A ("program", TF.Program_Name)
                 & A ("date", TF.Time)
                 & A ("tag", TF.User_Data));
         Pp.T ("trace", +Attributes, Dest_Trace_Info);
      end Process_One_Trace;

   --  Start of processing for Print_Trace_Info

   begin
      Create_Output_File (Pp.Files (Dest_Trace_Info), Trace_File_Name);
      Pp.P (Xml_Header, Dest_Trace_Info);
      Pp.ST ("traces", Dest_Trace_Info);
      Iterate_On_Traces_Files (Process_One_Trace'Access);
      Pp.ET ("traces", Dest_Trace_Info);
      Close (Pp.Files (Dest_Trace_Info));
   end Print_Trace_Info;

   ---------------
   -- Src_Block --
   ---------------

   procedure Src_Block
     (Pp         : in out Xml_Pretty_Printer'Class;
      Sloc_Start : Source_Location;
      Sloc_End   : Source_Location)
   is
      Current_Line_Sloc : Source_Location := Sloc_Start;
   begin
      Pp.ST ("src");
      Current_Line_Sloc.L.Column := 0;

      for Line_Num in Sloc_Start.L.Line .. Sloc_End.L.Line loop
         Current_Line_Sloc.L.Line := Line_Num;

         declare
            Attributes : Unbounded_String := +A ("num", Img (Line_Num));
            Line       : constant String :=
              Get_Line (Current_Line_Sloc, UTF8 => True);
            Src_Start  : Natural := Line'First;
            Src_End    : Natural := Line'Last;
         begin
            if Line_Num = Sloc_Start.L.Line then
               Src_Start := Natural'Min (Src_End, Sloc_Start.L.Column);
               Append
                 (Attributes, A ("column_begin", Img (Sloc_Start.L.Column)));
            end if;

            if Line_Num = Sloc_End.L.Line then
               Src_End := Natural'Min (Src_End, Sloc_End.L.Column);
               Append (Attributes, A ("column_end", Img (Sloc_End.L.Column)));
            end if;

            if Line'Length /= 0 then
               if Src_Start > 1 then
                  declare
                     Spaces : constant String (1 .. Src_Start - 1) :=
                                (others => ' ');
                  begin
                     Append
                       (Attributes,
                        A ("src", Spaces & Line (Src_Start .. Src_End)));
                  end;
               else
                  Append (Attributes, A ("src", Line (Src_Start .. Src_End)));
               end if;
            end if;

            Pp.T ("line", +Attributes);
         end;
      end loop;
      Pp.ET ("src");
   end Src_Block;

   --------
   -- ST --
   --------

   procedure ST
     (Pp         : in out Xml_Pretty_Printer'Class;
      Name       : String;
      Attributes : String;
      Dest       : Printing_Destination := Dest_Compilation_Unit) is
   begin
      Pp.P ("<" & Name & Attributes & ">", Dest);
      Pp.Indentations (Dest) := Pp.Indentations (Dest) + 1;
   end ST;

   procedure ST
     (Pp   : in out Xml_Pretty_Printer'Class;
      Name : String;
      Dest : Printing_Destination := Dest_Compilation_Unit) is
   begin
      Pp.P ("<" & Name & ">", Dest);
      Pp.Indentations (Dest) := Pp.Indentations (Dest) + 1;
   end ST;

   -------
   -- T --
   -------

   procedure T
     (Pp         : in out Xml_Pretty_Printer'Class;
      Name       : String;
      Attributes : String;
      Dest       : Printing_Destination := Dest_Compilation_Unit) is
   begin
      Pp.P ("<" & Name & Attributes & "/>", Dest);
   end T;

   -------------------
   -- To_Xml_String --
   -------------------

   function To_Xml_String (S : String) return String is

      type Cst_String_Access is access constant String;

      type Xml_Special_Character is record
         Char : Character;
         --  Special character

         Xml_Representation : Cst_String_Access;
         --  Xml Representation of Char
      end record;

      type Xml_Special_Character_Mapping is
        array (Natural range <>) of Xml_Special_Character;

      Entity_Quot : aliased constant String := "&quot;";
      Entity_Apos : aliased constant String := "&apos;";
      Entity_Gt   : aliased constant String := "&gt;";
      Entity_Lt   : aliased constant String := "&lt;";
      Entity_Amp  : aliased constant String := "&amp;";

      Char_Map : constant Xml_Special_Character_Mapping :=
        (('"', Entity_Quot'Access),
         (''', Entity_Apos'Access),
         ('>', Entity_Gt'Access),
         ('<', Entity_Lt'Access),
         ('&', Entity_Amp'Access));

      function Xml_Length (S : String) return Natural;
      --  Return the length of the string after conversion

      ----------------
      -- Xml_Length --
      ----------------

      function Xml_Length (S : String) return Natural is
         Add : Natural := 0;
      begin
         for J in S'Range loop
            for K in Char_Map'Range loop
               if S (J) = Char_Map (K).Char then
                  Add := Add + Char_Map (K).Xml_Representation.all'Length - 1;
               end if;
            end loop;
         end loop;

         return S'Length + Add;
      end Xml_Length;

      --  Local variables

      Res       : String (1 .. Xml_Length (S));
      Idx       : Natural;
      Increment : Natural;

   --  Start of processing for To_Xml_String

   begin
      Idx := Res'First;
      for J in S'Range loop
         Increment := 1;
         Res (Idx) := S (J);

         for K in Char_Map'Range loop
            if S (J) = Char_Map (K).Char then
               Increment := Char_Map (K).Xml_Representation'Length;
               Res (Idx .. Idx + Increment - 1) :=
                 Char_Map (K).Xml_Representation.all;
            end if;
         end loop;

         Idx := Idx + Increment;
      end loop;

      pragma Assert (Idx = Res'Last + 1);
      return Res;
   end To_Xml_String;

   --------------------------
   -- Print_Coverage_Stats --
   --------------------------

   procedure Print_Coverage_Stats
     (Pp         : in out Xml_Pretty_Printer'Class;
      Stats      : Counter_Array;
      Total_Kind : String;
      Dest       : Printing_Destination)
   is

      Total : constant Natural := Get_Total (Stats);

      procedure Print_Metric_Ratio (Name : String; Amount : Natural);
      --  Emit a <metric> tag to represent the given metric, including a ratio

      ------------------------
      -- Print_Metric_Ratio --
      ------------------------

      procedure Print_Metric_Ratio (Name : String; Amount : Natural) is
         Attributes : constant String :=
            A ("kind", Name)
            & A ("count", Img (Amount))
            & (if Total = 0
               then ""
               else A ("ratio", Img (Ratio (Amount, Total))));
      begin
         Pp.T ("metric", Attributes, Dest);
      end Print_Metric_Ratio;

   begin
      Pp.T ("metric", A ("kind", Total_Kind)
                      & A ("count", Img (Total)), Dest);
      Print_Metric_Ratio ("fully_covered", Stats (Covered));
      Print_Metric_Ratio ("partially_covered", Stats (Partially_Covered));
      Print_Metric_Ratio ("not_covered", Stats (Not_Covered));
      Print_Metric_Ratio ("undetermined_coverage",
                          Stats (Undetermined_Coverage));
      Print_Metric_Ratio ("exempted_no_violation",
                          Stats (Exempted_No_Violation));
      Print_Metric_Ratio ("exempted",
                          Stats (Exempted_With_Violation));
      Print_Metric_Ratio ("exempted_undetermined_coverage",
                          Stats (Exempted_With_Undetermined_Cov));
      Print_Metric_Ratio ("disabled_coverage", Stats (Disabled_Coverage));

   end Print_Coverage_Stats;

   -----------------------------
   -- Print_Coverage_Li_Stats --
   -----------------------------

   procedure Print_Coverage_Li_Stats
     (Pp       : in out Xml_Pretty_Printer'Class;
      Li_Stats : Li_Stat_Array;
      Dest     : Printing_Destination) is
   begin
      Print_Coverage_Stats (Pp, Li_Stats, "total_lines_of_relevance", Dest);
   end Print_Coverage_Li_Stats;

   -----------------------------
   -- Print_Coverage_Ob_Stats --
   -----------------------------

   procedure Print_Coverage_Ob_Stats
     (Pp       : in out Xml_Pretty_Printer'Class;
      Ob_Stats : Ob_Stat_Array;
      Dest     : Printing_Destination) is
   begin
      for Level of Source_Levels_Enabled loop
         Pp.ST ("obligation_stats", A ("kind", Image (Level)), Dest);
         Print_Coverage_Stats
           (Pp,
            Ob_Stats (Level).Stats,
            "total_obligations_of_relevance",
            Dest);
         Pp.ET ("obligation_stats", Dest);
      end loop;
   end Print_Coverage_Ob_Stats;

end Annotations.Xml;
