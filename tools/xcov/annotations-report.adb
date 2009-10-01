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

with Ada.Integer_Text_IO;

with Hex_Images;  use Hex_Images;
with Strings;     use Strings;
with Traces_Disa; use Traces_Disa;

package body Annotations.Report is

   type Final_Report_Type is limited record
      --  Final report information

      Name   : String_Acc := null;
      --  Final report's file name

      File   : aliased File_Type;
      --  Handle to the final report
   end record;

   Final_Report : aliased Final_Report_Type;

   procedure Close_Report_File;
   --  Close the handle to the final report

   type Report_Pretty_Printer is new Pretty_Printer with record
      --  Pretty printer type for the final report

      Print_Current_Line : Boolean := False;
      --  When going through the source file list, this marks
      --  whether or not the current line's coverage information is relevant
      --  in the final report.
   end record;

   --------------------------------------------------
   -- Report_Pretty_Printer's primitive operations --
   --      (inherited from Pretty_Printer)         --
   --------------------------------------------------

   procedure Pretty_Print_Start_File
     (Pp              : in out Report_Pretty_Printer;
      Source_Filename : String;
      Stats           : Stat_Array;
      Has_Source      : Boolean;
      Skip            : out Boolean);

   procedure Pretty_Print_Start_Line
     (Pp : in out Report_Pretty_Printer;
      Line_Num : Natural;
      State : Line_State;
      Line : String);

   procedure Pretty_Print_Start_Symbol
     (Pp     : in out Report_Pretty_Printer;
      Name   : String;
      Offset : Pc_Type;
      State  : Line_State);

   procedure Pretty_Print_Insn
     (Pp    : in out Report_Pretty_Printer;
      Pc    : Pc_Type;
      State : Insn_State;
      Insn  : Binary_Content;
      Sym   : Symbolizer'Class);

   procedure Pretty_Print_End_File
     (Pp : in out Report_Pretty_Printer);

   -----------------------
   -- Close_Report_File --
   -----------------------

   procedure Close_Report_File is
   begin
      if Final_Report.Name /= null then
         Close (Final_Report.File);
         Unchecked_Deallocation (Final_Report.Name);
      end if;
   end Close_Report_File;

   ----------------
   -- Get_Output --
   ----------------

   function Get_Output return File_Access is
   begin
      if Final_Report.Name /= null then
         return Final_Report.File'Access;
      else
         return Standard_Output;
      end if;
   end Get_Output;

   ---------------------
   -- Finalize_Report --
   ---------------------

   procedure Finalize_Report
   is
      Report_PP : Report_Pretty_Printer;
      Output    : constant File_Access := Get_Output;
   begin
      Put_Line (Output.all, "ERRORS BY SOURCE LINE:");
      New_Line (Output.all);

      Annotations.Generate_Report (Report_PP, False);
      Close_Report_File;
   end Finalize_Report;

   ----------------------
   -- Open_Report_File --
   ----------------------

   procedure Open_Report_File (Final_Report_Name : String) is
   begin
      Final_Report.Name := new String'(Final_Report_Name);

      if Final_Report.Name /= null then
         Create (Final_Report.File, Out_File, Final_Report.Name.all);
      end if;
   end Open_Report_File;

   ---------------------------
   -- Pretty_Print_End_File --
   ---------------------------

   procedure Pretty_Print_End_File
     (Pp : in out Report_Pretty_Printer)
   is
   begin
      null;
   end Pretty_Print_End_File;

   -----------------------
   -- Pretty_Print_Insn --
   -----------------------

   procedure Pretty_Print_Insn
     (Pp : in out Report_Pretty_Printer;
      Pc : Pc_Type;
      State : Insn_State;
      Insn : Binary_Content;
      Sym : Symbolizer'Class)
   is
      Output : constant File_Access := Get_Output;
   begin
      if Pp.Print_Current_Line then
         Put (Output.all, Hex_Image (Pc));
         Put (Output.all, ' ' & Insn_State_Char (State) & ":  ");
         for I in Insn'Range loop
            Put (Output.all, Hex_Image (Insn (I)));
            Put (Output.all, " ");
         end loop;
         Put (Output.all, " ");
         Put_Line (Output.all, Disassemble (Insn, Pc, Sym));
      end if;
   end Pretty_Print_Insn;

   -----------------------------
   -- Pretty_Print_Start_File --
   -----------------------------

   procedure Pretty_Print_Start_File
     (Pp              : in out Report_Pretty_Printer;
      Source_Filename : String;
      Stats           : Stat_Array;
      Has_Source      : Boolean;
      Skip            : out Boolean)
   is
      pragma Unreferenced (Pp);
      P      : constant Counters := Get_Counters (Stats);
      Output : constant File_Access := Get_Output;
   begin
      Skip := True;

      if not Flag_Show_Missing and then not Has_Source then
         return;
      end if;

      if P.Fully = 0 then
         Put (Output.all, Source_Filename & ':');
         Put_Line (Output.all, " not covered");
         New_Line (Output.all);
         Skip := True;
      elsif P.Fully /= P.Total then
         Put (Output.all, Source_Filename & ':');
         Put_Line (Output.all, " not fully covered");
         Skip := False;
      end if;
   end Pretty_Print_Start_File;

   -----------------------------
   -- Pretty_Print_Start_Line --
   -----------------------------

   procedure Pretty_Print_Start_Line
     (Pp       : in out Report_Pretty_Printer;
      Line_Num : Natural;
      State    : Line_State;
      Line     : String)
   is
      use Ada.Integer_Text_IO;

      Output : constant File_Access := Get_Output;
   begin
      if State /= Covered
        and then State /= No_Code
      then
         New_Line (Output.all);
         Put (Output.all, Line_Num, 4);
         Put (Output.all, ' ');
         Put (Output.all, State_Char (State));
         Put (Output.all, ": ");
         Put (Output.all, Line);
         New_Line (Output.all);
         Pp.Print_Current_Line := State /= Not_Covered;
      else
         Pp.Print_Current_Line := False;
      end if;
   end Pretty_Print_Start_Line;

   -------------------------------
   -- Pretty_Print_Start_Symbol --
   -------------------------------

   procedure Pretty_Print_Start_Symbol
     (Pp     : in out Report_Pretty_Printer;
      Name   : String;
      Offset : Pc_Type;
      State  : Line_State)
   is
      pragma Unreferenced (State);
      Output : constant File_Access := Get_Output;
      Label  : constant String := "<" & Name & "+" & Hex_Image (Offset) & ">:";
   begin
      if Pp.Print_Current_Line then
         Put_Line (Output.all, Label);
      end if;
   end Pretty_Print_Start_Symbol;

end Annotations.Report;
