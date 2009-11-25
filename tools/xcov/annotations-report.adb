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

with Ada.Directories;

with GNAT.Strings; use GNAT.Strings;

with Coverage;     use Coverage;
with Strings;      use Strings;

package body Annotations.Report is

   type Final_Report_Type is limited record
      --  Final report information

      Name   : String_Access := null;
      --  Final report's file name

      File   : aliased File_Type;
      --  Handle to the final report
   end record;

   Final_Report : aliased Final_Report_Type;

   procedure Close_Report_File;
   --  Close the handle to the final report

   type Report_Pretty_Printer is new Pretty_Printer with record
      --  Pretty printer type for the final report

      Current_Filename : String_Access := null;
      --  When going through the lines of a source file,
      --  This is set to the current source file name.
   end record;

   --------------------------------------------------
   -- Report_Pretty_Printer's primitive operations --
   --      (inherited from Pretty_Printer)         --
   --------------------------------------------------

   procedure Pretty_Print_Start_File
     (Pp              : in out Report_Pretty_Printer;
      Source          : File_Info_Access;
      Stats           : Stat_Array;
      Has_Source      : Boolean;
      Skip            : out Boolean);

   procedure Pretty_Print_Start_Line
     (Pp : in out Report_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line : String);

   procedure Pretty_Print_End_File
     (Pp : in out Report_Pretty_Printer);

   -----------------------
   -- Close_Report_File --
   -----------------------

   procedure Close_Report_File is
   begin
      if Final_Report.Name /= null then
         Close (Final_Report.File);
         Free (Final_Report.Name);
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

   procedure Finalize_Report is
      Report_PP : Report_Pretty_Printer;
   begin
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

   procedure Pretty_Print_End_File (Pp : in out Report_Pretty_Printer) is
   begin
      null;
   end Pretty_Print_End_File;

   -----------------------------
   -- Pretty_Print_Start_File --
   -----------------------------

   procedure Pretty_Print_Start_File
     (Pp              : in out Report_Pretty_Printer;
      Source          : File_Info_Access;
      Stats           : Stat_Array;
      Has_Source      : Boolean;
      Skip            : out Boolean)
   is
      pragma Unreferenced (Has_Source);
      P : constant Counters := Get_Counters (Stats);
   begin
      if P.Fully /= P.Total then
         if Pp.Current_Filename /= null then
            Free (Pp.Current_Filename);
         end if;

         Pp.Current_Filename := new String'(Source.Full_Name.all);
         Skip := False;
      else
         Skip := True;
      end if;
   end Pretty_Print_Start_File;

   -----------------------------
   -- Pretty_Print_Start_Line --
   -----------------------------

   procedure Pretty_Print_Start_Line
     (Pp       : in out Report_Pretty_Printer;
      Line_Num : Natural;
      Info     : Line_Info_Access;
      Line     : String)
   is
      pragma Unreferenced (Line);

      use Ada.Directories;
      use Message_Vectors;

      Output : constant File_Access := Get_Output;

      procedure Put_Message (C : Cursor);
      --  Display message associated to Info

      function Default_Message
        (Level : Coverage_Level;
         State : Line_State)
        return String;
      --  Return the default error message for the given coverage level
      --  and the given line state

      function Has_Messages (MV : Vector) return Boolean;
      --  Return True iff MV contains messages that are serious enough to
      --  be included into the report

      function Should_Be_Displayed (M : Message) return Boolean;
      --  Return True is M is serious enough to be included into the report

      ---------------------
      -- Default_Message --
      ---------------------

      function Default_Message
        (Level : Coverage_Level;
         State : Line_State)
        return String is
         Prefix : constant String :=
           Simple_Name (Pp.Current_Filename.all) & ":" & Img (Line_Num) & ": ";
      begin
         if Level = Stmt then
            return Prefix & "statement not covered";
         else
            return Prefix & "line " & State'Img
              & " for " & Level'Img & " coverage";
         end if;
      end Default_Message;

      ------------------
      -- Has_Messages --
      ------------------

      function Has_Messages (MV : Vector) return Boolean is
         Position : Cursor := First (MV);
         M        : Message;
      begin
         while Position /= No_Element loop
            M := Element (Position);

            if Should_Be_Displayed (M) then
               return True;
            end if;

            Next (Position);
         end loop;

         return False;
      end Has_Messages;

      -----------------
      -- Put_Message --
      -----------------

      procedure Put_Message (C : Cursor) is
         M : Message renames Element (C);
      begin
         if Should_Be_Displayed (M) then
            Put (Output.all, Image (M.Sloc));
            Put (Output.all, ": ");

            if M.SCO /= No_SCO_Id then
               Put (Output.all, SCO_Kind'Image (Kind (M.SCO)) & ": ");
            end if;

            Put (Output.all, M.Msg.all);
            New_Line (Output.all);
         end if;
      end Put_Message;

      -------------------------
      -- Should_Be_Displayed --
      -------------------------

      function Should_Be_Displayed (M : Message) return Boolean is
      begin
         return M.Kind /= Notice;
      end Should_Be_Displayed;

   --  Start of processing for Pretty_Print_Start_Line

   begin
      --  When two coverage criteria are not met on the same line, only
      --  report errors for the "lowest" one. For example, if a decision is
      --  not covered for stmt coverage, it will certainly not be covered
      --  for decision coverage or MCDC; but report only the stmt coverage
      --  error.

      --  If error/warning messages have been attached to the line, print them
      --  in the report; otherwise, fall back to a general error message.

      for Level in Coverage_Level loop
         if Info.State (Level) /= Covered
           and then Info.State (Level) /= No_Code
         then
            if Has_Messages (Info.Messages) then
               Info.Messages.Iterate (Put_Message'Access);
            else
               Put_Line (Output.all,
                         Default_Message (Level, Info.State (Level)));
            end if;

            exit;
         end if;
      end loop;
   end Pretty_Print_Start_Line;

end Annotations.Report;
