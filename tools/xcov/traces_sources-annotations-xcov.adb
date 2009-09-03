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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Directories;
with Hex_Images; use Hex_Images;
with Traces_Disa; use Traces_Disa;
with Coverage; use Coverage;
with Outputs; use Outputs;

package body Traces_Sources.Annotations.Xcov is
   type Xcov_Pretty_Printer is new Pretty_Printer with record
      Xcov_File : Ada.Text_IO.File_Type;
   end record;

   procedure Pretty_Print_File (Pp : in out Xcov_Pretty_Printer;
                                Source_Filename : String;
                                Stats : Stat_Array;
                                Has_Source : Boolean;
                                Skip : out Boolean);

   procedure Pretty_Print_Line (Pp : in out Xcov_Pretty_Printer;
                                Line_Num : Natural;
                                State : Line_State;
                                Line : String);

   procedure Pretty_Print_Label (Pp : in out Xcov_Pretty_Printer;
                                 Label : String);
   procedure Pretty_Print_Insn (Pp : in out Xcov_Pretty_Printer;
                                Pc : Pc_Type;
                                State : Insn_State;
                                Insn : Binary_Content;
                                Sym : Symbolizer'Class);

   procedure Pretty_Print_End_File (Pp : in out Xcov_Pretty_Printer);

   procedure Pretty_Print_File (Pp : in out Xcov_Pretty_Printer;
                                Source_Filename : String;
                                Stats : Stat_Array;
                                Has_Source : Boolean;
                                Skip : out Boolean)
   is
      use Ada.Directories;
   begin
      Skip := True;

      --  Do not try to process files whose source is not available.
      if not Flag_Show_Missing and then not Has_Source then
         return;
      end if;

      declare
         Output_Filename : constant String :=
           Simple_Name (Source_Filename) & ".xcov";
      begin
         Create_Output_File (Pp.Xcov_File, Output_Filename);
      exception
         when Ada.Text_IO.Name_Error =>
            Put_Line (Standard_Error,
                      "cannot open " & Output_Filename);
            return;
      end;

      Skip := False;

      Put_Line (Pp.Xcov_File, Source_Filename & ':');
      Put_Line (Pp.Xcov_File, Get_Stat_String (Stats));
      Put_Line (Pp.Xcov_File, "Coverage level: "
                & To_Coverage_Option (Get_Coverage_Level));
   end Pretty_Print_File;

   procedure Pretty_Print_Line (Pp : in out Xcov_Pretty_Printer;
                                Line_Num : Natural;
                                State : Line_State;
                                Line : String)
   is
      use Ada.Integer_Text_IO;
   begin
      Put (Pp.Xcov_File, Line_Num, 4);
      Put (Pp.Xcov_File, ' ');
      Put (Pp.Xcov_File, State_Char (State));
      Put (Pp.Xcov_File, ": ");
      Put (Pp.Xcov_File, Line);
      New_Line (Pp.Xcov_File);
   end Pretty_Print_Line;

   procedure Pretty_Print_Label (Pp : in out Xcov_Pretty_Printer;
                                 Label : String) is
   begin
      Put_Line (Pp.Xcov_File, Label);
   end Pretty_Print_Label;

   procedure Pretty_Print_Insn (Pp : in out Xcov_Pretty_Printer;
                                Pc : Pc_Type;
                                State : Insn_State;
                                Insn : Binary_Content;
                                Sym : Symbolizer'Class)
   is
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

   procedure Pretty_Print_End_File (Pp : in out Xcov_Pretty_Printer) is
   begin
      Close (Pp.Xcov_File);
   end Pretty_Print_End_File;

   procedure Generate_Report (Show_Asm : Boolean)
   is
      Xcov : Xcov_Pretty_Printer;
   begin
      Traces_Sources.Annotations.Disp_Line_State (Xcov, Show_Asm);
   end Generate_Report;

end Traces_Sources.Annotations.Xcov;
