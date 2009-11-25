------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

package body Annotations.Xml is

   package ASU renames Ada.Strings.Unbounded;

   type Xml_Pretty_Printer is new Pretty_Printer with record
      --  Pretty printer type for the XML annotation format

      Index_File            : File_Type;
      --  Handle to the XML index

      Compilation_Unit_File : File_Type;
      --  When going through the source file list, handle to the xml file
      --  that corresponds to the source file being processed.
      --  e.g. hello.adb.xml for hello.adb.

   end record;

   --------------------
   -- XML generation --
   --------------------

   Xml_Header : constant String := "<?xml version=""1.0"" ?>";

   function Escape_Quotes (S : String) return String;
   --  Remplace '"' by '\"' in S and return the result.

   function A (Name : String; Value : String) return String;
   --  Return a string representing an xml attribute whose name
   --  and value are given in parameter. id est
   --   Name = "Value"

   function T
     (Name       : String;
      Attributes : String)
     return String;
   --  Return a string representing an empty tag whose name and
   --  attributes are given in parameter. id est
   --  <Name Attributes/>

   function ST
     (Name       : String;
      Attributes : String)
     return String;
   --  Return a string representing a start tag whose name and attributes
   --  are given in parameter. id est
   --  <Name Attributes>

   function ST
     (Name       : String)
     return String;
   --  Same as ST, with no attributes.

   function ET
     (Name      : String)
     return String;
   --  Return a string representing an end tag whose name is given
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
     (Pp              : in out Xml_Pretty_Printer;
      Source          : File_Info_Access;
      Stats           : Stat_Array;
      Has_Source      : Boolean;
      Skip            : out Boolean);

   procedure Pretty_Print_End_File (Pp : in out Xml_Pretty_Printer);

   procedure Pretty_Print_Start_Line
     (Pp       : in out Xml_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String);

   procedure Pretty_Print_End_Line (Pp : in out Xml_Pretty_Printer);

   procedure Pretty_Print_Start_Instruction_Set
     (Pp    : in out Xml_Pretty_Printer;
      State : Line_State);

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
     (Pp       : in out Xml_Pretty_Printer;
      SCO      : SCO_Id;
      Executed : Boolean);

   procedure Pretty_Print_Start_Decision
     (Pp  : in out Xml_Pretty_Printer;
      SCO : SCO_Id);

   procedure Pretty_Print_End_Decision (Pp : in out Xml_Pretty_Printer);

   procedure Pretty_Print_Condition
     (Pp  : in out Xml_Pretty_Printer;
      SCO : SCO_Id);

   -----------------------------
   -- Shortcut for Put_Line's --
   -----------------------------

   procedure P (Pp : in out Xml_Pretty_Printer'Class; S : String);
   --  Put_Line S in Pp's current annotated source file

   procedure Pi (Pp : in out Xml_Pretty_Printer'Class; S : String);
   --  Put_Line S in Pp's index.xml

   procedure Src_Block
     (Pp         : in out Xml_Pretty_Printer'Class;
      Sloc_Start : Source_Location;
      Sloc_End   : Source_Location);
   --  Emit a <src>...</src> block for the range Sloc_Start .. Sloc_End

   -------
   -- A --
   -------

   function A (Name : String; Value : String) return String is
   begin
      return " " & Name & "=" & '"' & Escape_Quotes (Value) & '"';
   end A;

   --------
   -- ET --
   --------

   function ET
     (Name      : String)
     return String is
   begin
      return "</" & Name & ">";
   end ET;

   -------------------
   -- Escape_Quotes --
   -------------------

   function Escape_Quotes (S : String) return String is
      Count_Quotes : Natural := 0;
   begin
      for J in S'Range loop
         if S (J) = '"' then
            Count_Quotes := Count_Quotes + 1;
         end if;
      end loop;

      declare
         Result       : String (1 .. S'Length + Count_Quotes);
         Current      : Character;
         Result_Index : Natural := Result'First;
         S_Index      : Natural := S'First;
      begin
         while Result_Index <= Result'Last loop
            Current := S (S_Index);
            S_Index := S_Index + 1;

            if Current /= '"' then
               Result (Result_Index) := Current;
               Result_Index := Result_Index + 1;
            else
               Result (Result_Index) := '\';
               Result (Result_Index + 1) := Current;
               Result_Index := Result_Index + 2;
            end if;
         end loop;
         return Result;
      end;
   end Escape_Quotes;

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

   procedure P (Pp : in out Xml_Pretty_Printer'Class; S : String) is
   begin
      Put_Line (Pp.Compilation_Unit_File, S);
   end P;

   --------
   -- Pi --
   --------

   procedure Pi (Pp : in out Xml_Pretty_Printer'Class; S : String) is
   begin
      Put_Line (Pp.Index_File, S);
   end Pi;

   ----------------------------
   -- Pretty_Print_Condition --
   ----------------------------

   procedure Pretty_Print_Condition
     (Pp  : in out Xml_Pretty_Printer;
      SCO : SCO_Id)
   is
      Sloc_Start : constant Source_Location := First_Sloc (SCO);
      Sloc_End   : constant Source_Location := Last_Sloc (SCO);
   begin
      Pp.P (ST ("condition",
                A ("Id", Img (Integer (SCO)))));
      Pp.Src_Block (Sloc_Start, Sloc_End);
      Pp.P (ET ("condition"));
   end Pretty_Print_Condition;

   ----------------------
   -- Pretty_Print_End --
   ----------------------

   procedure Pretty_Print_End
     (Pp : in out Xml_Pretty_Printer)
   is
   begin
      Pp.Pi (ET ("sources"));
      Pp.Pi (ET ("coverage_report"));
      Close (Pp.Index_File);
   end Pretty_Print_End;

   -------------------------------
   -- Pretty_Print_End_Decision --
   -------------------------------

   procedure Pretty_Print_End_Decision
     (Pp : in out Xml_Pretty_Printer) is
   begin
      Pp.P (ET ("decision"));
   end Pretty_Print_End_Decision;

   ---------------------------
   -- Pretty_Print_End_File --
   ---------------------------

   procedure Pretty_Print_End_File (Pp : in out Xml_Pretty_Printer) is
   begin
      Pp.P (ET ("source"));
      Close (Pp.Compilation_Unit_File);
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
      Pp.P (T ("instruction",
               A ("address", Hex_Image (Pc))
               & A ("coverage", Insn_State_Char (State) & "")
               & A ("assembly", Disassemble (Insn, Pc, Sym))));
   end Pretty_Print_Insn;

   --------------------------------------
   -- Pretty_Print_End_Instruction_Set --
   --------------------------------------

   procedure Pretty_Print_End_Instruction_Set
     (Pp : in out Xml_Pretty_Printer) is
   begin
      Pp.P (ET ("instruction_set"));
   end Pretty_Print_End_Instruction_Set;

   ---------------------------
   -- Pretty_Print_End_Line --
   ---------------------------

   procedure Pretty_Print_End_Line (Pp : in out Xml_Pretty_Printer) is
   begin
      Pp.P (ET ("sloc"));
   end Pretty_Print_End_Line;

   -----------------------------
   -- Pretty_Print_End_Symbol --
   -----------------------------

   procedure Pretty_Print_End_Symbol (Pp : in out Xml_Pretty_Printer) is
   begin
      Pp.P (ET ("symbol"));
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
      Pp.P (T ("message", To_String (Attributes)));
   end Pretty_Print_Message;

   ------------------------
   -- Pretty_Print_Start --
   ------------------------

   procedure Pretty_Print_Start (Pp : in out Xml_Pretty_Printer) is
   begin
      Create_Output_File (Pp.Index_File, "index.xml");

      Pp.Pi (Xml_Header);
      Pp.Pi (ST ("coverage_report",
        A ("coverage_level", Coverage_Option_Value)));
      Pp.Pi (ST ("sources"));
   end Pretty_Print_Start;

   ---------------------------------
   -- Pretty_Print_Start_Decision --
   ---------------------------------

   procedure Pretty_Print_Start_Decision
     (Pp  : in out Xml_Pretty_Printer;
      SCO : SCO_Id)
   is
      Sloc_Start : constant Source_Location := First_Sloc (SCO);
      Sloc_End   : constant Source_Location := Last_Sloc (SCO);
   begin
      Pp.P (ST ("decision",
                A ("Id", Img (Integer (SCO)))));
      Pp.Src_Block (Sloc_Start, Sloc_End);
   end Pretty_Print_Start_Decision;

   -----------------------------
   -- Pretty_Print_Start_File --
   -----------------------------

   procedure Pretty_Print_Start_File
     (Pp              : in out Xml_Pretty_Printer;
      Source          : File_Info_Access;
      Stats           : Stat_Array;
      Has_Source      : Boolean;
      Skip            : out Boolean)
   is
      pragma Unreferenced (Stats);
      --  No stats are emitted in the XML output; the user is supposed
      --  to compute them by himself by post-processing the output.

      pragma Unreferenced (Has_Source);

      Simple_Source_Filename : constant String := Source.Simple_Name.all;
      Xml_File_Name          : constant String :=
        Simple_Source_Filename & ".xml";
   begin
      Skip := False;
      Create_Output_File (Pp.Compilation_Unit_File, Xml_File_Name);
      Pp.P (Xml_Header);
      Pp.P (ST ("source",
             A ("file", Simple_Source_Filename)
             & A ("Coverage_Level", Coverage_Option_Value)));

      Pp.Pi (T ("xi:include", A ("parse", "xml") & A ("href", Xml_File_Name)));
   end Pretty_Print_Start_File;

   ----------------------------------------
   -- Pretty_Print_Start_Instruction_Set --
   ----------------------------------------

   procedure Pretty_Print_Start_Instruction_Set
     (Pp    : in out Xml_Pretty_Printer;
      State : Line_State)
   is
      Coverage_State : constant String := State_Char (State) & "";
   begin
      Pp.P (ST ("instruction_set", A ("coverage", Coverage_State)));
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
                         (1 => State_Char (Aggregated_State (Info.State)));
   begin
      Pp.P (ST ("sloc", A ("coverage", Coverage_State)));
      Pp.P (ST ("src"));
      Pp.P (T ("line",
               A ("num", Img (Line_Num))
               & A ("src", Line)));
      Pp.P (ET ("src"));
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
      Pp.P (ST ("symbol",
                A ("name", Name)
                & A ("offset", Hex_Image (Offset))
                & A ("coverage", Coverage_State)));
   end Pretty_Print_Start_Symbol;

   ----------------------------
   -- Pretty_Print_Statement --
   ----------------------------

   procedure Pretty_Print_Statement
     (Pp       : in out Xml_Pretty_Printer;
      SCO      : SCO_Id;
      Executed : Boolean)
   is
      type To_State_Char_Type is array (Boolean) of Character;

      To_State_Char : constant To_State_Char_Type :=
        (False => '-', True => '+');

      Coverage_State : constant Character := To_State_Char (Executed);
      Sloc_Start     : constant Source_Location := First_Sloc (SCO);
      Sloc_End       : constant Source_Location := Last_Sloc (SCO);
   begin
      Pp.P (ST ("statement",
                A ("Id", Img (Integer (SCO)))
                & A ("coverage", Coverage_State & "")));
      Pp.Src_Block (Sloc_Start, Sloc_End);
      Pp.P (ET ("statement"));
   end Pretty_Print_Statement;

   ---------------
   -- Src_Block --
   ---------------

   procedure Src_Block
     (Pp         : in out Xml_Pretty_Printer'Class;
      Sloc_Start : Source_Location;
      Sloc_End   : Source_Location)
   is
      use Ada.Strings.Unbounded;
   begin
      Pp.P (ST ("src"));
      for Line_Num in Sloc_Start.Line .. Sloc_End.Line loop
         declare
            Attributes : Unbounded_String :=
              To_Unbounded_String (A ("num", Img (Line_Num)));
         begin
            if Sloc_Start /= Sloc_End then
               if Line_Num = Sloc_Start.Line and Sloc_Start.Column > 1 then
                  Attributes := Attributes
                    & A ("column_begin", Img (Sloc_Start.Column));
               end if;

               if Line_Num = Sloc_End.Line and Sloc_End.Column /= 0 then
                  Attributes := Attributes
                    & A ("column_end", Img (Sloc_Start.Column));
               end if;
            end if;

            Pp.P (T ("line", To_String (Attributes)));
         end;
      end loop;
      Pp.P (ET ("src"));
   end Src_Block;

   --------
   -- ST --
   --------

   function ST
     (Name       : String;
      Attributes : String)
     return String is
   begin
      return "<" & Name & Attributes & ">";
   end ST;

   function ST
     (Name       : String)
     return String is
   begin
      return "<" & Name & ">";
   end ST;

   -------
   -- T --
   -------

   function T
     (Name       : String;
      Attributes : String)
     return String is
   begin
      return "<" & Name & Attributes & "/>";
   end T;

end Annotations.Xml;
