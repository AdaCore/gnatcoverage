------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with Ada.Containers; use Ada.Containers;
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
     (Pp         : in out Xcov_Pretty_Printer;
      Source     : File_Info_Access;
      Stats      : Stat_Array;
      Has_Source : Boolean;
      Skip       : out Boolean);

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
     (Pp    : in out Xcov_Pretty_Printer;
      Pc    : Pc_Type;
      State : Insn_State;
      Insn  : Binary_Content;
      Sym   : Symbolizer'Class);

   procedure Pretty_Print_End_File (Pp : in out Xcov_Pretty_Printer);

   ---------------------
   -- Generate_Report --
   ---------------------

   procedure Generate_Report (Show_Asm : Boolean)
   is
      Xcov : Xcov_Pretty_Printer;
   begin
      Annotations.Generate_Report (Xcov, Show_Asm);
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
     (Pp    : in out Xcov_Pretty_Printer;
      Pc    : Pc_Type;
      State : Insn_State;
      Insn  : Binary_Content;
      Sym   : Symbolizer'Class) is
   begin
      Put (Pp.Xcov_File, Hex_Image (Pc));
      Put (Pp.Xcov_File, ' ' & Insn_State_Char (State) & ":  ");
      for I in Insn'Range loop
         Put (Pp.Xcov_File, Hex_Image (Insn (I)));
         Put (Pp.Xcov_File, " ");
      end loop;
      Put (Pp.Xcov_File, " ");
      Put_Line (Pp.Xcov_File, Disassemble (Insn, Pc, Sym));
   end Pretty_Print_Insn;

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
      Put (Pp.Xcov_File, State_Char (Aggregated_State (Info.State)));
      if False then
         if Info.SCOs.Length > 0 then
            Put (Pp.Xcov_File, 'S');
         else
            Put (Pp.Xcov_File, ' ');
         end if;
         if Info.Obj_First /= null then
            Put (Pp.Xcov_File, 'O');
         else
            Put (Pp.Xcov_File, ' ');
         end if;
      end if;
      Put (Pp.Xcov_File, ": ");
      Put (Pp.Xcov_File, Line);
      New_Line (Pp.Xcov_File);
   end Pretty_Print_Start_Line;

   -----------------------------
   -- Pretty_Print_Start_File --
   -----------------------------

   procedure Pretty_Print_Start_File
     (Pp         : in out Xcov_Pretty_Printer;
      Source     : File_Info_Access;
      Stats      : Stat_Array;
      Has_Source : Boolean;
      Skip       : out Boolean)
   is
   begin
      Skip := True;

      --  Do not try to process files whose source is not available

      if not Flag_Show_Missing and then not Has_Source then
         Warn_File_Missing (Source.all);
         return;
      end if;

      declare
         Output_Filename : constant String := Source.Simple_Name.all & ".xcov";
      begin
         Create_Output_File (Pp.Xcov_File, Output_Filename);
      exception
         when Ada.Text_IO.Name_Error =>
            Put_Line (Standard_Error,
                      "cannot open " & Output_Filename);
            return;
      end;

      Skip := False;

      Put_Line (Pp.Xcov_File, Source.Full_Name.all & ':');
      Put_Line (Pp.Xcov_File, Get_Stat_String (Stats));
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
