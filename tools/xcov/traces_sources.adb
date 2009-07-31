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
with Interfaces;

with Hex_Images;
with Sources; use Sources;
with Strings; use Strings;
with Traces_Disa;
with Traces_Names; use Traces_Names;

with GNAT.Dynamic_Tables;

package body Traces_Sources is

   --  Describe a source file - one element per line.

   package Source_Line_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Line_Info,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 16,
      Table_Increment      => 100);

   subtype Source_Lines is Source_Line_Tables.Instance;

   type File_Info is record
      --  Source file information.

      Lines      : Source_Lines;
      --  Source file to display in the reports.

      Stats      : Stat_Array := (others => 0);
      --  Counters associated to the file (e.g total number of lines, number
      --  of lines that are covered).

      To_Display : Boolean := False;
      --  If True, this file should be displayed in the reports. If False,
      --  it should be skipped.
      --  The global source table in sources.adb and the table of sources
      --  that are concerned by the current coverage operation
      --  (Source_Line_Tables) share the sames indices. However, the
      --  latter is a subset of the former. A non-contiguous subset, in the
      --  general case. So, To_Display is used to discriminate files that are
      --  really concerned by the coverage from files that just happens to
      --  have an index between two "real" elements.
   end record;

   procedure Expand_Line_Table
     (File : in out File_Info;
      Line : Natural);
   --  If Line is not in File's line table, expand this table and mark the new
   --  line as No_Code.

   procedure Update_File_Info
     (File  : in out File_Info;
      Line  : Natural;
      State : Traces.Trace_State);
   --  Update the state of Line in File's line table

   package File_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => File_Info,
      Table_Index_Type     => Source_File_Index,
      Table_Low_Bound      => First_Source_File,
      Table_Initial        => 16,
      Table_Increment      => 100);

   File_Table : File_Tables.Instance;

   procedure Append
     (Info : in out Line_Info;
      Line : Addresses_Info_Acc;
      Base : Traces_Base_Acc;
      Exec : Exe_File_Acc);
   --  Comment needed???

   function Compute_Routine_State
     (Insns  : Binary_Content_Acc;
      Traces : Traces_Base_Acc) return Line_State;
   --  Compute coverage information for the routine whose code is Insns, with
   --  the given traces.

   procedure Disp_File_Line_State
     (Pp       : in out Pretty_Printer'Class;
      Filename : String;
      File     : File_Info);
   --  Comment needed???

   type Source_Search_Entry;
   type Source_Search_Entry_Acc is access Source_Search_Entry;
   type Source_Search_Entry is record
      Prefix : String_Acc;
      Next : Source_Search_Entry_Acc;
   end record;

   type Source_Rebase_Entry;
   type Source_Rebase_Entry_Acc is access Source_Rebase_Entry;
   type Source_Rebase_Entry is record
      Old_Prefix : String_Acc;
      New_Prefix : String_Acc;
      Next : Source_Rebase_Entry_Acc;
   end record;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (File : Source_File_Index;
      Line : Natural;
      Info : Addresses_Info_Acc;
      Base : Traces_Base_Acc;
      Exec : Exe_File_Acc)
   is
      Element : File_Info renames File_Table.Table (File);
   begin
      Expand_Line_Table (Element, Line);
      Append (Element.Lines.Table (Line), Info, Base, Exec);
   end Add_Line;

   --------------------
   -- Add_Line_State --
   --------------------

   procedure Add_Line_State
     (File  : Source_File_Index;
      Line  : Natural;
      State : Traces.Trace_State)
   is
      Element : File_Info renames File_Table.Table (File);
   begin
      Expand_Line_Table (Element, Line);

      if State = Unknown then
         raise Program_Error;
      end if;

      Update_File_Info (Element, Line, State);
   end Add_Line_State;

   First_Source_Search_Entry : Source_Search_Entry_Acc := null;
   Last_Source_Search_Entry  : Source_Search_Entry_Acc := null;

   -----------------------
   -- Add_Source_Search --
   -----------------------

   procedure Add_Source_Search (Prefix : String)
   is
      E : Source_Search_Entry_Acc;
   begin
      E := new Source_Search_Entry'(Prefix => new String'(Prefix),
                                    Next => null);
      if First_Source_Search_Entry = null then
         First_Source_Search_Entry := E;
      else
         Last_Source_Search_Entry.Next := E;
      end if;
      Last_Source_Search_Entry := E;
   end Add_Source_Search;

   First_Source_Rebase_Entry : Source_Rebase_Entry_Acc := null;
   Last_Source_Rebase_Entry  : Source_Rebase_Entry_Acc := null;

   -----------------------
   -- Add_Source_Rebase --
   -----------------------

   procedure Add_Source_Rebase (Old_Prefix : String; New_Prefix : String) is
      E : Source_Rebase_Entry_Acc;
   begin
      E := new Source_Rebase_Entry'(Old_Prefix => new String'(Old_Prefix),
                                    New_Prefix => new String'(New_Prefix),
                                    Next => null);
      if First_Source_Rebase_Entry = null then
         First_Source_Rebase_Entry := E;
      else
         Last_Source_Rebase_Entry.Next := E;
      end if;
      Last_Source_Rebase_Entry := E;
   end Add_Source_Rebase;

   ------------
   -- Append --
   ------------

   procedure Append
     (Info : in out Line_Info;
      Line : Addresses_Info_Acc;
      Base : Traces_Base_Acc;
      Exec : Exe_File_Acc)
   is
      El : constant Line_Chain_Acc := new Line_Chain'(Line => Line,
                                                      Base => Base,
                                                      Exec => Exec,
                                                      Link => null);
   begin
      if Info.First_Line = null then
         Info.First_Line := El;
      else
         Info.Last_Line.Link := El;
      end if;
      Info.Last_Line := El;
   end Append;

   ---------------------------
   -- Compute_Routine_State --
   ---------------------------

   function Compute_Routine_State
     (Insns  : Binary_Content_Acc;
      Traces : Traces_Base_Acc) return Line_State
   is
      use type Interfaces.Unsigned_32;
      State : Line_State := No_Code;
      Addr  : Pc_Type;
      It    : Entry_Iterator;
      T     : Trace_Entry;
   begin
      if Insns = null then
         --  The routine was not found in the executable

         return Not_Covered;

      else
         Init (Traces.all, It, 0);

         Addr := Insns'First;

         loop
            Get_Next_Trace (T, It);
            exit when T = Bad_Trace;
            if T.First > Addr then
               Update_Line_State (State, Not_Covered);
               exit;
            end if;
            Update_Line_State (State, T.State);
            Addr := T.Last + 1;
         end loop;

         if Addr < Insns'Last then
            Update_Line_State (State, Not_Covered);
         end if;

         if State = No_Code then
            return Not_Covered;
         else
            return State;
         end if;
      end if;
   end Compute_Routine_State;

   --------------------------
   -- Disp_File_Line_State --
   --------------------------

   procedure Disp_File_Line_State (Pp : in out Pretty_Printer'Class;
                                   Filename : String;
                                   File : File_Info)
   is
      use Source_Line_Tables;
      use Traces_Disa;

      procedure Disassemble_Cb
        (Addr  : Pc_Type;
         State : Trace_State;
         Insn  : Binary_Content;
         Sym   : Symbolizer'Class);
      --  Comment needed???

      procedure Try_Open
        (F       : in out File_Type;
         Name    : String;
         Success : out Boolean);
      --  Comment needed???

      --------------------
      -- Disassemble_Cb --
      --------------------

      procedure Disassemble_Cb
        (Addr  : Pc_Type;
         State : Trace_State;
         Insn  : Binary_Content;
         Sym   : Symbolizer'Class) is
      begin
         Pretty_Print_Insn (Pp, Addr, State, Insn, Sym);
      end Disassemble_Cb;

      --------------
      -- Try_Open --
      --------------

      procedure Try_Open
        (F       : in out File_Type;
         Name    : String;
         Success : out Boolean) is
      begin
         Open (F, In_File, Name);
         Success := True;
      exception
         when Name_Error =>
            Success := False;
      end Try_Open;

      F : File_Type;
      Ok : Boolean;
      Has_Source : Boolean;
      Line : Natural;

      Info : Line_Chain_Acc;
      Line_Info : Addresses_Info_Acc;
      Sec_Info : Addresses_Info_Acc;
      Ls : Line_State;

      Skip : Boolean;

   --  Start of processing for Disp_File_Line_State

   begin
      --  Try original path

      Try_Open (F, Filename, Ok);

      --  Try to rebase

      if not Ok then
         declare
            E : Source_Rebase_Entry_Acc := First_Source_Rebase_Entry;
            First : constant Positive := Filename'First;
         begin
            while E /= null loop
               if Filename'Length > E.Old_Prefix'Length
                 and then (Filename (First .. First + E.Old_Prefix'Length - 1)
                             = E.Old_Prefix.all)
               then
                  Try_Open (F,
                            E.New_Prefix.all
                              & Filename (First + E.Old_Prefix'Length
                                            .. Filename'Last),
                            Ok);
                  exit when Ok;
               end if;
               E := E.Next;
            end loop;
         end;
      end if;

      --  Try source path

      if not Ok then
         declare
            E : Source_Search_Entry_Acc := First_Source_Search_Entry;
         begin
            while E /= null loop
               Try_Open (F, E.Prefix.all & '/' & Filename, Ok);
               exit when Ok;
               E := E.Next;
            end loop;
         end;
      end if;

      if not Ok then
         Put_Line (Standard_Error, "warning: can't open " & Filename);
         Has_Source := False;
      else
         Has_Source := True;
      end if;

      Pretty_Print_File (Pp, Filename, File.Stats, Has_Source, Skip);
      if Skip then
         if Has_Source then
            Close (F);
         end if;
         return;
      end if;

      --  Iterate over each lines of the file

      for I in Integer range First .. Last (File.Lines) loop
         Ls := File.Lines.Table (I).State;
         if Has_Source then
            Pretty_Print_Line (Pp, I, Ls, Get_Line (F));
         else
            Pretty_Print_Line (Pp, I, Ls, "");
         end if;

         if Pp.Show_Asm then

            --  Iterate over each insn block for the source line

            Info := File.Lines.Table (I).First_Line;
            while Info /= null loop
               Line_Info := Info.Line;
               declare
                  Label : constant String :=
                    Get_Label (Info.Exec.all, Line_Info);
               begin
                  if Label'Length > 0 then
                     Pretty_Print_Label (Pp, Label);
                  end if;
               end;

               Sec_Info := Line_Info.Parent;

               while Sec_Info /= null
                 and then Sec_Info.Kind /= Section_Addresses
               loop
                  Sec_Info := Sec_Info.Parent;
               end loop;
               Disp_Assembly_Lines
                 (Sec_Info.Section_Content (Line_Info.First .. Line_Info.Last),
                  Info.Base.all, Disassemble_Cb'Access, Info.Exec.all);
               Info := Info.Link;
            end loop;
         end if;
      end loop;

      if Has_Source then
         Line := Last (File.Lines) + 1;
         while not End_Of_File (F) loop
            Pretty_Print_Line (Pp, Line, No_Code, Get_Line (F));
            Line := Line + 1;
         end loop;

         Close (F);
      end if;

      Pretty_Print_End_File (Pp);
   end Disp_File_Line_State;

   -----------------------
   -- Disp_File_Summary --
   -----------------------

   procedure Disp_File_Summary
   is
      use Ada.Directories;

      procedure Disp_One_File (Name : String; File : File_Info);
      --  Display summary for the given file

      -------------------
      -- Disp_One_File --
      -------------------

      procedure Disp_One_File (Name : String; File : File_Info) is
         use Source_Line_Tables;
      begin
         Put (Simple_Name (Name));
         Put (": ");
         Put (Get_Stat_String (File.Stats));
         New_Line;
      end Disp_One_File;

   --  Start of processing for Disp_File_Summary

   begin
      for J in File_Tables.First
        .. File_Tables.Last (File_Table)
      loop
         if File_Table.Table (J).To_Display then
            Disp_One_File (Sources.Get_Name (J), File_Table.Table (J));
         end if;
      end loop;
   end Disp_File_Summary;

   ---------------------
   -- Disp_Line_State --
   ---------------------

   procedure Disp_Line_State
     (Pp       : in out Pretty_Printer'Class;
      Show_Asm : Boolean)
   is
      use Ada.Directories;
   begin
      Pp.Show_Asm := Show_Asm;

      Pretty_Print_Start (Pp);

      --  Iterates on all files

      for J in File_Tables.First .. File_Tables.Last (File_Table) loop
         if File_Table.Table (J).To_Display then
            Disp_File_Line_State
              (Pp, Sources.Get_Name (J), File_Table.Table (J));
         end if;
      end loop;

      Pretty_Print_Finish (Pp);
   end Disp_Line_State;

   --------------------------
   -- Dump_Routines_Traces --
   --------------------------

   procedure Dump_Routines_Traces
   is
      use Traces_Disa;

      procedure Process_One
        (Name : String_Acc;
         Info : in out Subprogram_Info);
      --  Display traces for one routine

      procedure Process_One
        (Name : String_Acc;
         Info : in out Subprogram_Info)
      is
         use Hex_Images;
      begin
         Put (Name.all);

         if Info.Traces /= null then
            Put (' ');
            Put (State_Char (Compute_Routine_State (Info.Insns, Info.Traces)));
         end if;

         if Info.Insns /= null then
            Put (": " & Hex_Image (Info.Insns'First)
                 & '-' & Hex_Image (Info.Insns'Last));
         end if;
         New_Line;

         if Info.Traces /= null then
            if Flag_Show_Asm then
               if Info.Exec = null then
                  Disp_Assembly_Lines
                    (Info.Insns.all,
                     Info.Traces.all,
                     Textio_Disassemble_Cb'Access,
                     Disa_Symbolize.Nul_Symbolizer);

               else
                  Disp_Assembly_Lines
                    (Info.Insns.all,
                     Info.Traces.all,
                     Textio_Disassemble_Cb'Access,
                     Info.Exec.all);
               end if;
            end if;
         end if;
      end Process_One;

   --  Start of processing for Dump_Routines_Traces

   begin
      Iterate (Process_One'Access);
   end Dump_Routines_Traces;

   -----------------------------
   -- Dump_Uncovered_Routines --
   -----------------------------

   procedure Dump_Uncovered_Routines (Report : File_Access) is
      use Traces_Disa;

      procedure Process_One
        (Name : String_Acc;
         Info : in out Subprogram_Info);
      --  Report information for the given routine

      procedure Process_One
        (Name : String_Acc;
         Info : in out Subprogram_Info)
      is
         Routine_State : constant Line_State :=
                           Compute_Routine_State (Info.Insns, Info.Traces);
      begin
         if Info.Insns = null then
            Put_Line (Report.all, Name.all & " not found in executable(s)");

         elsif Routine_State /= Covered
           and then Routine_State /= No_Code
         then
            Put (Report.all, Name.all & " not fully covered : ");
            Put (Report.all, State_Char (Routine_State));
            New_Line (Report.all);
         end if;
      end Process_One;

   --  Start of processing for Dump_Uncovered_Routines

   begin
      Put_Line (Report.all, "ERRORS BY ROUTINE:");
      New_Line (Report.all);
      Iterate (Process_One'Access);
      New_Line (Report.all);
   end Dump_Uncovered_Routines;

   -----------------------
   -- Expand_Line_Table --
   -----------------------

   procedure Expand_Line_Table (File : in out File_Info; Line : Natural) is
      use Source_Line_Tables;
      Current_Last : constant Natural := Last (File.Lines);
   begin
      if Current_Last < Line then
         Set_Last (File.Lines, Line);

         for J in Current_Last + 1 .. Line loop
            File.Lines.Table (J) := (State => No_Code, others => <>);
            File.Stats (No_Code) := File.Stats (No_Code) + 1;
            Global_Stats (No_Code) := Global_Stats (No_Code) + 1;
         end loop;
      end if;
   end Expand_Line_Table;

   ---------------------
   -- New_Source_File --
   ---------------------

   procedure New_Source_File (File : Source_File_Index) is
      Last : Source_File_Index;
   begin
      Last := File_Tables.Last (File_Table);
      if File > Last then
         File_Tables.Set_Last (File_Table, File);
         for Index in Last + 1 .. File loop
            declare
               Line_Table : Source_Lines;
            begin
               Source_Line_Tables.Init (Line_Table);
               File_Table.Table (Index).Lines := Line_Table;
               File_Table.Table (Index).Stats := (others => 0);
               File_Table.Table (Index).To_Display := False;
            end;
         end loop;
      end if;
      File_Table.Table (File).To_Display := True;
   end New_Source_File;

   ----------------------
   -- Update_File_Info --
   ----------------------

   procedure Update_File_Info
     (File  : in out File_Info;
      Line  : Natural;
      State : Traces.Trace_State)
   is
      Ls : Line_State;
   begin
      Ls := File.Lines.Table (Line).State;
      File.Stats (Ls) := File.Stats (Ls) - 1;
      Global_Stats (Ls) := Global_Stats (Ls) - 1;
      Update_Line_State (Ls, State);
      File.Lines.Table (Line).State := Ls;
      File.Stats (Ls) := File.Stats (Ls) + 1;
      Global_Stats (Ls) := Global_Stats (Ls) + 1;
   end Update_File_Info;

begin
   File_Tables.Init (File_Table);
end Traces_Sources;
