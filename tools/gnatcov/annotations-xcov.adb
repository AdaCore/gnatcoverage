------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

with Ada.Integer_Text_IO;
with Ada.Text_IO;    use Ada.Text_IO;

with Coverage;    use Coverage;
with Hex_Images;  use Hex_Images;
with Outputs;     use Outputs;
with Traces_Disa; use Traces_Disa;

package body Annotations.Xcov is

   type Xcov_Pretty_Printer is new Pretty_Printer with record
      --  Pretty printer type for the XCOV annotation format

      Xcov_File : Ada.Text_IO.File_Type;
      --  When going through the source file list, handle to the xcov file
      --  that corresponds to the source file being processed.
      --  e.g. hello.adb.xcov for hello.adb.
   end record;

   ------------------------------------------------
   -- Xcov_Pretty_Printer's primitive operations --
   --    (inherited from Pretty_Printer)         --
   ------------------------------------------------

   procedure Pretty_Print_Start_File
     (Pp   : in out Xcov_Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean);

   procedure Pretty_Print_Start_Line
     (Pp       : in out Xcov_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String);

   procedure Pretty_Print_Start_Symbol
     (Pp     : in out Xcov_Pretty_Printer;
      Name   : String;
      Offset : Pc_Type;
      State  : Line_State);

   procedure Pretty_Print_Insn
     (Pp       : in out Xcov_Pretty_Printer;
      Pc       : Pc_Type;
      State    : Insn_State;
      Insn     : Binary_Content;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class);

   procedure Pretty_Print_Message
     (Pp : in out Xcov_Pretty_Printer;
      M  : Message);

   procedure Pretty_Print_End_File (Pp : in out Xcov_Pretty_Printer);

   ---------------------
   -- Generate_Report --
   ---------------------

   procedure Generate_Report (Show_Details : Boolean)
   is
      Xcov : Xcov_Pretty_Printer;
   begin
      Annotations.Generate_Report (Xcov, Show_Details);
   end Generate_Report;

   ---------------------------
   -- Pretty_Print_End_File --
   ---------------------------

   procedure Pretty_Print_End_File (Pp : in out Xcov_Pretty_Printer) is
   begin
      Close (Pp.Xcov_File);
   end Pretty_Print_End_File;

   -----------------------
   -- Pretty_Print_Insn --
   -----------------------

   procedure Pretty_Print_Insn
     (Pp       : in out Xcov_Pretty_Printer;
      Pc       : Pc_Type;
      State    : Insn_State;
      Insn     : Binary_Content;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class) is
   begin
      Put (Pp.Xcov_File, Hex_Image (Pc));
      Put (Pp.Xcov_File, ' ' & Insn_State_Char (State) & ":  ");
      for I in Insn.First .. Insn.Last loop
         Put (Pp.Xcov_File, Hex_Image (Get (Insn, I)));
         Put (Pp.Xcov_File, " ");
      end loop;
      Put (Pp.Xcov_File, " ");
      Put_Line (Pp.Xcov_File, Disassemble (Insn, Pc, Insn_Set, Sym));
   end Pretty_Print_Insn;

   --------------------------
   -- Pretty_Print_Message --
   --------------------------

   procedure Pretty_Print_Message
     (Pp : in out Xcov_Pretty_Printer;
      M  : Message) is
   begin
      Put (Pp.Xcov_File, Message_Annotation (M));
      New_Line (Pp.Xcov_File);
   end Pretty_Print_Message;

   -----------------------------
   -- Pretty_Print_Start_Line --
   -----------------------------

   procedure Pretty_Print_Start_Line
     (Pp       : in out Xcov_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String)
   is
      use Ada.Integer_Text_IO;
   begin
      Put (Pp.Xcov_File, Line_Num, 4);
      Put (Pp.Xcov_File, ' ');
      Put (Pp.Xcov_File, State_Char (Aggregated_State (Info.all)));
      Put (Pp.Xcov_File, ": ");
      Put (Pp.Xcov_File, Line);
      New_Line (Pp.Xcov_File);
   end Pretty_Print_Start_Line;

   -----------------------------
   -- Pretty_Print_Start_File --
   -----------------------------

   procedure Pretty_Print_Start_File
     (Pp   : in out Xcov_Pretty_Printer;
      File : Source_File_Index;
      Skip : out Boolean)
   is
      Info : constant File_Info_Access := Get_File (File);
   begin
      Skip := True;

      --  Do not try to process files whose source is not available

      if not Flag_Show_Missing and then not Info.Has_Source then
         Warn_File_Missing (Info.all);
         return;
      end if;

      begin
         Create_Output_File (Pp.Xcov_File, Get_Unique_Filename (File, "xcov"));
      exception
         when Name_Error =>
            --  Failed to create output file, an error message has been printed

            return;
      end;

      Skip := False;

      Put_Line (Pp.Xcov_File, Info.Full_Name.all & ':');
      Put_Line (Pp.Xcov_File, Get_Stat_String (Info.Stats));
      Put_Line (Pp.Xcov_File, "Coverage level: " & Coverage_Option_Value);
   end Pretty_Print_Start_File;

   -------------------------------
   -- Pretty_Print_Start_Symbol --
   -------------------------------

   procedure Pretty_Print_Start_Symbol
     (Pp     : in out Xcov_Pretty_Printer;
      Name   : String;
      Offset : Pc_Type;
      State  : Line_State)
   is
      Label  : constant String := "<" & Name & "+" & Hex_Image (Offset) & ">:";
   begin
      Put (Pp.Xcov_File, Label);
      Put (Pp.Xcov_File, State_Char (State));
      New_Line (Pp.Xcov_File);
   end Pretty_Print_Start_Symbol;

end Annotations.Xcov;
