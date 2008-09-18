------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
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

package body Traces_Sources.xcov is
      type Xcov_Pretty_Printer is new Pretty_Printer with record
      Xcov_File : Ada.Text_Io.File_Type;
   end record;

   procedure Pretty_Print_File (Pp : in out Xcov_Pretty_Printer;
                                Source_Filename : String;
                                Stats : Stat_Array;
                                Skip : out Boolean);

   procedure Pretty_Print_Line (Pp : in out Xcov_Pretty_Printer;
                                Line_Num : Natural;
                                State : Line_State;
                                Line : String);

   procedure Pretty_Print_End_File (Pp : in out Xcov_Pretty_Printer);

   procedure Pretty_Print_File (Pp : in out Xcov_Pretty_Printer;
                                Source_Filename : String;
                                Stats : Stat_Array;
                                Skip : out Boolean)
   is
      use Ada.Directories;
   begin
      Skip := True;

      --  Do not try to process files whose source is not available.
      if not Flag_Show_Missing
        and then not Exists (Source_Filename)
      then
         return;
      end if;

      declare
         Output_Filename : constant String :=
           Simple_Name (Source_Filename) & ".xcov";
      begin
         Create (Pp.Xcov_File, Out_File, Output_Filename);
      exception
         when Ada.Text_IO.Name_Error =>
            Put_Line (Standard_Error,
                      "cannot open " & Output_Filename);
            return;
      end;

      Skip := False;

      Put_Line (Pp.Xcov_File, Source_Filename & ':');
      Put_Line (Pp.Xcov_File, Get_Stat_String (Stats));
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

   procedure Pretty_Print_End_File (Pp : in out Xcov_Pretty_Printer) is
   begin
      Close (Pp.Xcov_File);
   end Pretty_Print_End_File;

   procedure Generate_Report
   is
      Xcov : Xcov_Pretty_Printer;
   begin
      Traces_Sources.Disp_Line_State (Xcov);
   end Generate_Report;

end Traces_Sources.Xcov;
