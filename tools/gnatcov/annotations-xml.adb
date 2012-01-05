------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2009-2012, AdaCore                      --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Interfaces;

with Coverage;    use Coverage;
with Hex_Images;  use Hex_Images;
with Outputs;     use Outputs;
with Strings;     use Strings;
with Traces_Disa; use Traces_Disa;
with Traces_Files;
with Traces_Files_List;
with Qemu_Traces;

package body Annotations.Xml is

   package ASU renames Ada.Strings.Unbounded;

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

   --------------------
   -- XML generation --
   --------------------

   Xml_Header            : constant String := "<?xml version=""1.0"" ?>";
   Doctype_Index_Header  : constant String :=
     "<!DOCTYPE document SYSTEM ""annotations-xml.dtd"">";
   Doctype_Source_Header : constant String :=
     "<!DOCTYPE source SYSTEM ""annotations-xml.dtd"">";
   Doctype_Trace_Header  : constant String :=
     "<!DOCTYPE traces SYSTEM ""annotations-xml.dtd"">";

   Index_File_Name : constant String := "index.xml";
   Trace_File_Name : constant String := "trace.xml";

   function A (Name : String; Value : Character) return String;

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

   procedure Pretty_Print_End_Symbol (Pp    : in out Xml_Pretty_Printer);

   procedure Pretty_Print_Insn
     (Pp    : in out Xml_Pretty_Printer;
      Pc    : Pc_Type;
      State : Insn_State;
      Insn  : Binary_Content;
      Sym   : Symbolizer'Class);

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

   procedure Generate_Report is
      Pp : Xml_Pretty_Printer;
   begin
      Annotations.Generate_Report (Pp, True);
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
             & A ("text", SCO_Text (SCO))
             & A ("coverage", State_Char (State)));
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
     (Pp    : in out Xml_Pretty_Printer;
      Pc    : Pc_Type;
      State : Insn_State;
      Insn  : Binary_Content;
      Sym   : Symbolizer'Class) is
   begin
      Pp.T ("instruction",
            A ("address", Hex_Image (Pc))
            & A ("coverage", Insn_State_Char (State) & "")
            & A ("assembly", Disassemble (Insn, Pc, Sym)));
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
      use Ada.Strings.Unbounded;

      Attributes : Unbounded_String :=
        ASU.To_Unbounded_String (A ("kind", To_Lower (M.Kind'Img)));
   begin
      if M.PC /= 0 then
         Attributes := Attributes & A ("address", Hex_Image (M.PC));
      end if;

      if M.SCO /= No_SCO_Id then
         Attributes := Attributes
           & A ("SCO", Image (M.SCO, With_Sloc => False));
      end if;

      Attributes := Attributes & A ("message", M.Msg.all);
      Pp.T ("message", To_String (Attributes));
   end Pretty_Print_Message;

   ------------------------
   -- Pretty_Print_Start --
   ------------------------

   procedure Pretty_Print_Start (Pp : in out Xml_Pretty_Printer) is
   begin
      Create_Output_File (Pp.Files (Dest_Index), Index_File_Name);

      Pp.P (Xml_Header, Dest_Index);
      Pp.P (Doctype_Index_Header, Dest_Index);
      Pp.ST ("document",
             A ("xmlns:xi", "http://www.w3.org/2001/XInclude"),
             Dest_Index);
      Pp.ST ("coverage_report",
             A ("coverage_level", Coverage_Option_Value), Dest_Index);

      Pp.ST ("coverage_info", Dest_Index);
      Print_Trace_Info (Pp);
      Pp.T ("xi:include", A ("parse", "xml") & A ("href", Trace_File_Name),
            Dest_Index);
      Pp.ET ("coverage_info", Dest_Index);

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
             & A ("text", SCO_Text (SCO))
             & A ("coverage", State_Char (State)));
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

      Simple_Source_Filename : constant String := Info.Simple_Name.all;
      Xml_File_Name          : constant String :=
        Simple_Source_Filename & ".xml";
   begin
      if not (Info.Has_Source or Flag_Show_Missing) then
         Warn_File_Missing (Info.all);
         Skip := True;
         return;
      end if;

      Skip := False;
      Create_Output_File (Pp.Files (Dest_Compilation_Unit), Xml_File_Name);
      Pp.P (Xml_Header);
      Pp.P (Doctype_Source_Header);
      Pp.ST ("source",
             A ("file", Simple_Source_Filename)
             & A ("coverage_level", Coverage_Option_Value));

      Pp.T ("xi:include", A ("parse", "xml") & A ("href", Xml_File_Name),
            Dest_Index);
   end Pretty_Print_Start_File;

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
      Exempted : constant Boolean := Info.Exemption /= Slocs.No_Location;
   begin
      Pp.ST ("src_mapping", A ("coverage", Coverage_State));
      Pp.ST ("src");
      Pp.T ("line",
            A ("num", Img (Line_Num))
            & A ("exempted", Exempted'Img)
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
      Sloc_Start     : constant Source_Location := First_Sloc (SCO);
      Sloc_End       : constant Source_Location :=
                         End_Lex_Element (Last_Sloc (SCO));
   begin
      Pp.ST ("statement",
             A ("id", Img (Integer (SCO)))
             & A ("text", SCO_Text (SCO))
             & A ("coverage", State_Char (State)));
      Pp.Src_Block (Sloc_Start, Sloc_End);
      Pp.ET ("statement");
   end Pretty_Print_Statement;

   ----------------------
   -- Print_Trace_Info --
   ----------------------

   procedure Print_Trace_Info (Pp : in out Xml_Pretty_Printer'Class) is
      use Qemu_Traces;
      use Traces_Files;
      use Traces_Files_List;
      use Traces_Files_Lists;

      procedure Process_One_Trace (Position : Cursor);
      --  Print the data attached to the trace file at Position to trace.xml

      -----------------------
      -- Process_One_Trace --
      -----------------------

      procedure Process_One_Trace (Position : Cursor) is
         TF : constant Trace_File_Element_Acc := Element (Position);
      begin
         Pp.T ("trace",
               A ("filename", TF.Filename.all)
               & A ("program", Get_Info (TF.Trace, Exec_File_Name))
               & A ("date", Format_Date_Info (Get_Info (TF.Trace, Date_Time)))
               & A ("tag", Get_Info (TF.Trace, User_Data)),
               Dest_Trace_Info);
      end Process_One_Trace;

   --  Start of processing for Print_Trace_Info

   begin
      Create_Output_File (Pp.Files (Dest_Trace_Info), Trace_File_Name);
      Pp.P (Xml_Header, Dest_Trace_Info);
      Pp.P (Doctype_Trace_Header, Dest_Trace_Info);
      Pp.ST ("traces", Dest_Trace_Info);
      Files.Iterate (Process_One_Trace'Access);
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
      use Ada.Strings.Unbounded;

      Current_Line_Sloc : Source_Location := Sloc_Start;
   begin
      Pp.ST ("src");
      Current_Line_Sloc.Column := 0;

      for Line_Num in Sloc_Start.Line .. Sloc_End.Line loop
         Current_Line_Sloc.Line := Line_Num;

         declare
            Attributes : Unbounded_String :=
              To_Unbounded_String (A ("num", Img (Line_Num)));
            Line       : constant String := Get_Line (Current_Line_Sloc);
            Src_Start  : Natural := Line'First;
            Src_End    : Natural := Line'Last;
         begin
            if Sloc_Start /= Sloc_End then
               if Line_Num = Sloc_Start.Line and Sloc_Start.Column > 1 then
                  Src_Start := Natural'Min (Src_End, Sloc_Start.Column);
                  Attributes := Attributes
                    & A ("column_begin", Img (Sloc_Start.Column));
               end if;

               if Line_Num = Sloc_End.Line and Sloc_End.Column /= 0 then
                  Src_End := Natural'Min (Src_End, Sloc_End.Column);
                  Attributes := Attributes
                    & A ("column_end", Img (Sloc_Start.Column));
               end if;

               if Line'Length /= 0 then
                  if Src_Start > 1 then
                     declare
                        Spaces : constant String (1 .. Src_Start - 1) :=
                                   (others => ' ');
                     begin
                        Attributes := Attributes
                          & A ("src", Spaces & Line (Src_Start .. Src_End));
                     end;
                  else
                     Attributes := Attributes
                       & A ("src", Line (Src_Start .. Src_End));
                  end if;
               end if;
            end if;

            Pp.T ("line", To_String (Attributes));
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

      type Xml_Special_Character is record
         Char : Character;
         --  Special character

         Xml_Representation : String_Access;
         --  Xml Representation of Char
      end record;

      type Xml_Special_Character_Mapping is
        array (Natural range <>) of Xml_Special_Character;

      Char_Map : constant Xml_Special_Character_Mapping :=
        (('"', new String'("&quot;")),
         (''', new String'("&apos;")),
         ('>', new String'("&gt;")),
         ('<', new String'("&lt;")),
         ('&', new String'("&amp;")));

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

end Annotations.Xml;
