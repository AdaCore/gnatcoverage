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

with Ada.Text_IO;     use Ada.Text_IO;
with Coverage;        use Coverage;
with Outputs;         use Outputs;
with Hex_Images;      use Hex_Images;
with Traces_Disa;     use Traces_Disa;

package body Annotations.Xml is

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

   -----------------------------
   -- Shortcut for Put_Line's --
   -----------------------------

   procedure P (Pp : in out Xml_Pretty_Printer'Class; S : String);
   --  Put_Line S in Pp's current annotated source file

   procedure Pi (Pp : in out Xml_Pretty_Printer'Class; S : String);
   --  Put_Line S in Pp's index.xml

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
               A ("offset", Hex_Image (Pc))
               --  ??? offset's value is not compliant with spec:
               --  it is an address instead of an offset from symbol.
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

   ------------------------
   -- Pretty_Print_Start --
   ------------------------

   procedure Pretty_Print_Start
     (Pp : in out Xml_Pretty_Printer)
   is
      Level : constant String :=
        To_Coverage_Option (Get_Coverage_Level);
   begin
      Create_Output_File (Pp.Index_File, "index.xml");

      Pp.Pi (Xml_Header);
      Pp.Pi (ST ("coverage_report", A ("coverage_level", Level)));
      Pp.Pi (ST ("sources"));
   end Pretty_Print_Start;

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

      Level                  : constant String :=
        To_Coverage_Option (Get_Coverage_Level);
      Simple_Source_Filename : constant String := Source.Simple_Name.all;
      Xml_File_Name          : constant String :=
        Simple_Source_Filename & ".xml";
   begin
      Skip := False;
      Create_Output_File (Pp.Compilation_Unit_File, Xml_File_Name);
      Pp.P (Xml_Header);
      Pp.P (ST ("source",
             A ("file", Simple_Source_Filename)
             & A ("Coverage_Level", Level)));

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
      Coverage_State : constant String := (1 => State_Char (Info.State));
   begin
      Pp.P (ST ("sloc", A ("coverage", Coverage_State)));
      Pp.P (ST ("src"));
      Pp.P (T ("line",
               A ("num", Line_Num'Img)
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
                & A ("address", Hex_Image (Offset))
                & A ("coverage", Coverage_State)));
   end Pretty_Print_Start_Symbol;

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

end Annotations.Xml;
