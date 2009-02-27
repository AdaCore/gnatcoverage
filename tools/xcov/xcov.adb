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
with Traces; use Traces;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Traces_Elf; use Traces_Elf;
with Traces_Sources; use Traces_Sources;
with Traces_Sources.Html;
with Traces_Sources.Gcov;
with Traces_Sources.Xcov;
with Traces_Names;
with Traces_Files; use Traces_Files;
with Traces_Dbase; use Traces_Dbase;
with Traces_Disa;
with Traces_History;
with Version;
with Qemudrv;
with Qemu_Traces;
with Execs_Dbase; use Execs_Dbase;
with Strings; use Strings;

procedure Xcov is
   procedure Usage;
   procedure Error (Msg : String);
   function Parse_Hex (S : String; Flag_Name : String) return Pc_Type;

   procedure Usage
   is
      procedure P (S : String) renames Put_Line;
   begin
      P ("usage: " & Command_Name & " actions");
      New_Line;
      P ("Primary actions:");
      P (" --disp-routines {[--exclude|--include] FILE}");
      P ("    Build a list of routines from executable files");
      New_Line;
      P (" --consolidate OPTIONS {EXEFILE TRACEFILE}");
      P ("   Options are:");
      P ("   --include=RFILE      Read routines list from executable");
      P ("   --include-list=TFILE Read routine list from text file");
      P ("   -e EFILE             Use executable to display output");
      P ("   --asm                Display assembly code during output");
      New_Line;
      P (" --dump-trace-file FILES");
      P ("   Raw display of trace files");
      New_Line;
      P (" --dump-trace-asm EXE TRACE_FILES");
      P ("   Raw display of trace files with assembly code for each trace");
      New_Line;
      P (" --help  -h");
      P ("   Display this help");
      New_Line;
      P (" --version");
      P ("   Display version");
      New_Line;
      Qemudrv.Help (" ");
      New_Line;
      P (" ACTION...");
      P ("   Execute action one after the other, ACTION is a list of:");
      P ("   -r FILENAME         Read (and merge) traces from FILENAME");
      P ("   -w FILENAME         Write traces to FILENAME");
      P ("   -e FILENAME         Use FILENAME as executable");
      P ("   --dump-traces       Dump traces");
      P ("   --exe-coverage      Generate object coverage report");
      P ("   --source-coverage   Generate source coverage report");
      P ("   --function-coverage Generate function coverage report");
      P ("   --asm               Add assembly code in --source-coverage");
      P ("   --output-format=html/xcov/gcov  Select output format");
      P ("   --level=A/C         Select DO178B level for --source-coverage");
   end Usage;

   procedure Error (Msg : String) is
   begin
      Put_Line (Standard_Output, Msg);
      Set_Exit_Status (Failure);
   end Error;

   Arg_Index : Natural;
   Arg_Count : constant Natural := Argument_Count;

   function Parse_Hex (S : String; Flag_Name : String) return Pc_Type
   is
      Res : Pc_Type;
      Pos : Natural;
   begin
      if S'Length < 3
        or else S (S'First) /= '0'
        or else (S (S'First + 1) /= 'x' and then S (S'First + 1) /= 'X')
      then
         Error ("missing '0x' prefix for " & Flag_Name);
         raise Constraint_Error;
      end if;
      Pos := S'First + 2;
      Get_Pc (Res, S, Pos);
      if Pos <= S'Last then
         Error ("bad hexadecimal number for " & Flag_Name);
      end if;
      return Res;
   end Parse_Hex;

   function Option_Parameter (S : String) return String;
   --  Assuming that S is of the form "<part1>=<part2>",
   --  return "<part2>".

   function Begins_With (S : String; Beginning : String) return Boolean;
   --  If the beginning of S is equal to Beginnning, return True;
   --  otherwise, return False.

   function Begins_With (S : String; Beginning : String) return Boolean is
      Length : constant Integer := Beginning'Length;
   begin
      return S'Length > Length
        and then S (S'First .. S'First + Length - 1) = Beginning;
   end Begins_With;

   function Option_Parameter (S : String) return String is
   begin
      for J in S'Range loop
         if S (J) = '=' then
            return S (J + 1 .. S'Last);
         end if;
      end loop;
      return S;
   end Option_Parameter;

   Routine_List_Filename     : String_Acc := null;
   Routine_List_Option       : constant String := "--routine-list=";
   Routine_List_Option_Short : constant String := "-l";

   type Coverage_Action is (Insn_Coverage, Branch_Coverage, Stmt_Coverage,
                            Decision_Coverage, MCDC_Coverage,
                            Unknown_Coverage);

   Action                : Coverage_Action := Unknown_Coverage;
   Coverage_Option       : constant String := "--coverage=";
   Coverage_Option_Short : constant String := "-c";

   function To_Coverage_Action (Option : String) return Coverage_Action is
   begin
      if Option = "insn" then
         return Insn_Coverage;
      elsif Option = "branch" then
         return Branch_Coverage;
      elsif Option = "stmt" then
         return Stmt_Coverage;
      elsif Option = "decision" then
         return Decision_Coverage;
      elsif Option = "mcdc" then
         return MCDC_Coverage;
      else
         return Unknown_Coverage;
      end if;
   end To_Coverage_Action;

   type Annotation_Format is (Annotate_Asm, Annotate_Xcov, Annotate_Html,
                              Annotate_Xcov_Asm, Annotate_Html_Asm,
                              Annotate_Unknown);

   Annotations           : Annotation_Format;
   Annotate_Option       : constant String := "--annotate=";
   Annotate_Option_Short : constant String := "-a";

   function To_Annotation_Format (Option : String) return Annotation_Format is
   begin
      if Option = "asm" then
         return Annotate_Asm;
      elsif Option = "xcov" then
         return Annotate_Xcov;
      elsif Option = "html" then
         return Annotate_Html;
      elsif Option = "xcov+asm" then
         return Annotate_Xcov_Asm;
      elsif Option = "html+asm" then
         return Annotate_Html_Asm;
      else
         return Annotate_Unknown;
      end if;
   end To_Annotation_Format;

   --  Trace_File_Name : File_Name;
   --  Exec_File_Name : File_Name;

   Has_Exec : Boolean := False;
   Text_Start : Pc_Type := 0;
   type Output_Format is (Format_Xcov, Format_Gcov, Format_Html);
   Format : Output_Format := Format_Xcov;
   Trace_File : Trace_File_Type;
   Base : Traces_Base;

   Exec : Exe_File_Type;
   Sub_Exec : Exe_File_Acc;

   First_Exec : Exe_File_Acc;
   Exec_Base  : constant Exec_Base_Type := Get_Exec_Base;
begin
   --  Require at least one argument.
   if Arg_Count = 0 then
      Usage;
      return;
   end if;

   Arg_Index := 1;
   declare
      Cmd : constant String := Argument (Arg_Index);
      Mode_Exclude : Boolean := False;
   begin
      if Cmd = "--disp-routines" then
         if Arg_Index = Arg_Count then
            Error ("missing FILEs to --disp-routines");
            return;
         end if;
         for I in Arg_Index + 1 .. Arg_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if Arg = "--exclude" then
                  Mode_Exclude := True;
               elsif Arg = "--include" then
                  Mode_Exclude := False;
               else
                  Traces_Names.Read_Routines_Name (Arg, Mode_Exclude);
               end if;
            end;
         end loop;
         Traces_Names.Disp_All_Routines;
         return;
      elsif Cmd = "--consolidate" then
         if Arg_Index = Arg_Count then
            Error ("missing FILEs to --consolidate");
            return;
         end if;
         Arg_Index := Arg_Index + 1;
         while Arg_Index <= Arg_Count loop
            declare
               Arg : constant String := Argument (Arg_Index);
            begin
               Arg_Index := Arg_Index + 1;
               if Arg'Length > 10
                 and then Arg (Arg'First .. Arg'First + 9) = "--include="
               then
                  Traces_Names.Read_Routines_Name
                    (Arg (Arg'First + 10 .. Arg'Last), False);
               elsif Arg'Length > 14
                 and then Arg (Arg'First .. Arg'First + 14) = "--include-list="
               then
                  Traces_Names.Read_Routines_Name_From_Text
                    (Arg (Arg'First + 15 .. Arg'Last));
               elsif Arg = "-e" then
                  if Arg_Index > Arg_Count then
                     Error ("missing FILENAME to -e");
                     return;
                  end if;
                  begin
                     Open_File (Exec, Argument (Arg_Index), Text_Start);
                  exception
                     when others =>
                        Error ("cannot open " & Argument (Arg_Index));
                        return;
                  end;
                  Has_Exec := True;
                  Arg_Index := Arg_Index + 1;
               elsif Arg = "--asm" then
                  Traces_Disa.Flag_Show_Asm := True;
               else
                  if Arg (Arg'First) = '-' then
                     Error ("unknown option " & Arg);
                     return;
                  else
                     exit;
                  end if;
               end if;
            end;
         end loop;

         if Arg_Index > Arg_Count then
            Error ("no trace files given");
            return;
         end if;
         Arg_Index := Arg_Index - 1;
         while Arg_Index <= Arg_Count loop
            if Arg_Index + 1 > Arg_Count then
               Error ("EXEC_file TRACE_file expected");
               return;
            end if;

            --  Read Exe
            begin
               Sub_Exec := new Exe_File_Type;
               Open_File (Sub_Exec.all, Argument (Arg_Index), Text_Start);
            exception
               when others =>
                  Error ("cannot open " & Argument (Arg_Index));
                  return;
            end;
            Build_Sections (Sub_Exec.all);
            Build_Symbols (Sub_Exec.all);

            --  Read traces.
            Init_Base (Base);
            begin
               Read_Trace_File (Argument (Arg_Index + 1), Trace_File, Base);
            exception
               when others =>
                  Error ("cannot open tracefile " & Argument (Arg_Index + 1));
                  raise;
            end;
            Add_Subprograms_Traces (Sub_Exec, Base);
            Close_File (Sub_Exec.all);
            Free (Trace_File);
            Arg_Index := Arg_Index + 2;
         end loop;
         if Has_Exec then
            Traces_Names.Dump_Routines_Traces (Exec);
         else
            Traces_Names.Dump_Routines_Traces;
         end if;
         return;
      elsif Cmd = "--dump-trace" then
         if Arg_Index = Arg_Count then
            Put_Line ("missing FILENAME to --dump-trace");
            return;
         end if;
         for I in Arg_Index + 1 .. Arg_Count loop
            Dump_Trace_File (Argument (I));
         end loop;
         return;
      elsif Cmd = "--dump-trace-asm" then
         Arg_Index := Arg_Index + 1;
         if Arg_Index + 1 < Arg_Count then
            Put_Line ("missing FILENAME to --dump-trace-asm");
            return;
         end if;
         Open_File (Exec, Argument (Arg_Index), Text_Start);
         Build_Sections (Exec);
         Build_Symbols (Exec);
         for I in Arg_Index + 1 .. Arg_Count loop
            Traces_History.Dump_Traces_With_Asm (Exec, Argument (I));
         end loop;
         return;
      elsif Cmd = "--dump-symbols" then
         for I in Arg_Index + 1 .. Arg_Count loop
            Open_File (Exec, Argument (I), 0);
            Build_Sections (Exec);
            Build_Symbols (Exec);
            Disp_Symbols_Addresses (Exec);
         end loop;
         return;
      elsif Cmd = "--show-graph" then
         Arg_Index := Arg_Index + 1;
         if Arg_Index + 1 < Arg_Count then
            Put_Line ("missing EXE to --show-graph");
            return;
         end if;
         Open_File (Exec, Argument (Arg_Index), Text_Start);
         Build_Sections (Exec);
         Build_Symbols (Exec);
         Build_Debug_Lines (Exec);
         Traces_History.Generate_Graph (Exec);
         return;
      elsif Cmd = "--run" then
         Qemudrv.Driver (Arg_Index + 1);
         return;
      elsif Cmd = "-h" or else Cmd = "--help" then
         Usage;
         return;
      elsif Cmd = "--version" then
         Put_Line ("XCOV Pro " & Version.Xcov_Version);
         return;
      end if;
   end;

   while Arg_Index <= Arg_Count loop
      declare
         Arg : constant String := Argument (Arg_Index);
      begin
         Arg_Index := Arg_Index + 1;
         if Arg = "-h" or else Arg = "--help" then
            Usage;
            return;
         elsif Arg = "-r" then
            if Arg_Index > Arg_Count then
               Put_Line ("missing FILENAME to -r");
               return;
            end if;
            Read_Trace_File (Argument (Arg_Index), Trace_File, Base);
            Arg_Index := Arg_Index + 1;
         elsif Arg = "-e" then
            if Arg_Index > Arg_Count then
               Error ("missing FILENAME to -e");
               return;
            end if;
            begin
               Open_File (Exec, Argument (Arg_Index), Text_Start);
            exception
               when others =>
                  Error ("cannot open " & Argument (Arg_Index));
                  return;
            end;
            Has_Exec := True;
            Arg_Index := Arg_Index + 1;
         elsif Arg = "--dump-traces" then
            Dump_Traces (Base);
         elsif Arg = "--dump-traces-state" then
            Build_Sections (Exec);
            Set_Trace_State (Exec, Base);
            Dump_Traces (Base);
         elsif Arg = "-w" then
            if Arg_Index > Arg_Count then
               Error ("missing FILENAME to -w");
               return;
            end if;
            Write_Trace_File (Argument (Arg_Index), Trace_File, Base);
            Arg_Index := Arg_Index + 1;
         elsif Arg = "--exe-coverage" then
            if not Has_Exec then
               Error ("option --exe-coverage requires an executable");
               return;
            end if;
            Build_Sections (Exec);
            Build_Debug_Compile_Units (Exec);
            Set_Trace_State (Exec, Base);
            Build_Symbols (Exec);
            Disp_Sections_Coverage (Exec, Base);
         elsif Arg = "--dump-sections" then
            Build_Sections (Exec);
            Disp_Sections_Addresses (Exec);
         elsif Arg = "--dump-compile-units" then
            Build_Sections (Exec);
            Build_Debug_Compile_Units (Exec);
            Disp_Compile_Units_Addresses (Exec);
         elsif Arg = "--dump-subprograms" then
            Build_Sections (Exec);
            Build_Debug_Compile_Units (Exec);
            Disp_Subprograms_Addresses (Exec);
         elsif Arg = "--dump-lines" then
            Build_Sections (Exec);
            Build_Debug_Lines (Exec);
            Disp_Lines_Addresses (Exec);
         elsif Arg = "--dump-symbols" then
            Build_Sections (Exec);
            Build_Symbols (Exec);
            Disp_Symbols_Addresses (Exec);
         elsif Arg = "--asm" then
            Traces_Disa.Flag_Show_Asm := True;
         elsif Arg = "--missing-files" then
            Flag_Show_Missing := True;
         elsif Arg'Length > 8
           and then Arg (Arg'First .. Arg'First + 7) = "--level="
         then
            if Arg = "--level=A" or else Arg = "--level=a" then
               DO178B_Level := Level_A;
            elsif Arg = "--level=C" or else Arg = "--level=c" then
               DO178B_Level := Level_C;
            else
               Error ("bad parameter for --level");
               return;
            end if;
         elsif Arg'Length > 13
           and then Arg (Arg'First .. Arg'First + 12) = "--text-start="
         then
            begin
               Text_Start := Parse_Hex
                 (Arg (Arg'First + 13 .. Arg'Last), "--text-start");
            exception
               when Constraint_Error =>
                  return;
            end;
         elsif Arg'Length > 16
           and then Arg (Arg'First .. Arg'First + 15) = "--output-format="
         then
            if Arg (Arg'First + 16 .. Arg'Last) = "xcov" then
               Format := Format_Xcov;
            elsif Arg (Arg'First + 16 .. Arg'Last) = "gcov" then
               Format := Format_Gcov;
            elsif Arg (Arg'First + 16 .. Arg'Last) = "html" then
               Format := Format_Html;
            else
               Error ("bad parameter for --output-format");
               return;
            end if;
         elsif Arg'Length > 16
           and then Arg (Arg'First .. Arg'First + 15) = "--source-rebase="
         then
            declare
               Pos : Natural := 0;
            begin
               for I in Arg'First + 16 .. Arg'Last loop
                  if Arg (I) = ':' then
                     Pos := I;
                     exit;
                  end if;
               end loop;
               if Pos = 0 then
                  Error ("missing ':' in --source-rebase=");
                  return;
               end if;
               Add_Source_Rebase (Arg (Arg'First + 16 .. Pos - 1),
                                  Arg (Pos + 1 .. Arg'Last));
            end;
         elsif Arg'Length > 16
           and then Arg (Arg'First .. Arg'First + 15) = "--source-search="
         then
            Add_Source_Search (Arg (Arg'First + 16 .. Arg'Last));
         elsif Arg = "--source-coverage" then
            Build_Sections (Exec);
            Set_Trace_State (Exec, Base);
            Build_Debug_Lines (Exec);
            Build_Source_Lines (Exec, Base);
            Build_Symbols (Exec);
            case Format is
               when Format_Xcov =>
                  Traces_Sources.Xcov.Generate_Report (Base, Exec);
               when Format_Gcov =>
                  Traces_Sources.Gcov.Generate_Report (Base, Exec);
               when Format_Html =>
                  Traces_Sources.Html.Generate_Report (Base, Exec);
            end case;
         elsif Arg = "--file-coverage" then
            Build_Sections (Exec);
            Set_Trace_State (Exec, Base);
            Build_Debug_Lines (Exec);
            Build_Source_Lines (Exec, Base);
            Build_Symbols (Exec);
            Disp_File_Summary;
         elsif Arg = "--function-coverage" then
            Build_Routines_Name (Exec);
            Build_Sections (Exec);
            Build_Symbols (Exec);
            --  Add_Subprograms_Traces (Exec, Base);
            Traces_Names.Dump_Routines_Traces (Exec);

         --------------------
         -- New Option set --
         --------------------

         elsif Arg = Routine_List_Option_Short then
            if Arg_Index > Arg_Count then
               Put_Line ("Missing function list parameter to "
                         & Routine_List_Option_Short);
               return;
            end if;
            Routine_List_Filename := new String'(Argument (Arg_Index));
            Arg_Index := Arg_Index + 1;
         elsif Begins_With (Arg, Routine_List_Option) then
            Routine_List_Filename := new String'(Option_Parameter (Arg));
         elsif Arg = Annotate_Option_Short then
            if Arg_Index > Arg_Count then
               Put_Line ("Missing annotation format to"
                         & Annotate_Option_Short);
               return;
            end if;
            Annotations := To_Annotation_Format (Argument (Arg_Index));
            if Annotations = Annotate_Unknown then
               Error ("bad parameter for " & Annotate_Option_Short);
               return;
            end if;
            Arg_Index := Arg_Index + 1;
         elsif Begins_With (Arg, Annotate_Option) then
            Annotations := To_Annotation_Format (Option_Parameter (Arg));
            if Annotations = Annotate_Unknown then
               Error ("bad parameter for " & Annotate_Option);
               return;
            end if;
         elsif Arg = Coverage_Option_Short then
            if Arg_Index > Arg_Count then
               Put_Line ("Missing coverage action to "
                         & Coverage_Option_Short);
               return;
            end if;
            Action := To_Coverage_Action (Argument (Arg_Index));
            if Action = Unknown_Coverage then
               Error ("bad parameter for " & Coverage_Option_Short);
               return;
            end if;
            Arg_Index := Arg_Index + 1;
         elsif Begins_With (Arg, Coverage_Option) then
            Action := To_Coverage_Action (Option_Parameter (Arg));
            if Action = Unknown_Coverage then
               Error ("bad parameter for " & Coverage_Option);
               return;
            end if;
         elsif Arg (1) = '-' then
            Error ("unknown option: " & Arg);
            return;
         else
            --  Not an option.
            Arg_Index := Arg_Index - 1;
            exit;
         end if;
      end;
   end loop;

   if Action = Unknown_Coverage then
      --  If we end up here, that means that we are using the old interface;
      --  the rest of the code is specific to the new interface, so... return.
      --  When the old interface has been obsoleted, this will be removed.
      return;
   end if;

   if Arg_Index > Arg_Count then
      Error ("missing trace files(s)");
      return;
   end if;

   case Action is
      when Insn_Coverage =>
         Put_Line ("Insn coverage has not been implemented yet.");
         return;
      when Branch_Coverage =>
         null;
      when Stmt_Coverage =>
         Put_Line ("Stmt coverage has not been implemented yet.");
         return;
      when Decision_Coverage =>
         Put_Line ("Decision coverage has not been implemented yet.");
         return;
      when MCDC_Coverage =>
         Put_Line ("MCDC coverage has not been implemented yet.");
         return;
      when Unknown_Coverage =>
         Put_Line ("Please specify a coverage operation.");
         return;
   end case;

   if Routine_List_Filename /= null then
      Traces_Names.Read_Routines_Name_From_Text (Routine_List_Filename.all);
   else
      if Arg_Index /= Arg_Count then
         Error ("routine list required when multiple trace files");
         return;
      end if;
   end if;

   --  Read traces.
   while Arg_Index <= Arg_Count loop
      Init_Base (Base);
      Read_Trace_File (Argument (Arg_Index), Trace_File, Base);
      declare
         Exe_Name : constant String :=
           Get_Info (Trace_File, Qemu_Traces.Info_Kind_Exec_Filename);
         Exe_File : Exe_File_Acc;
      begin
         if Exe_Name = "" then
            Error ("cannot find exec filename in trace file "
                     & Argument (Arg_Index));
            return;
         end if;
         Open_Exec (Get_Exec_Base, Exe_Name, Exe_File);

         --  If there is not routine list, create it from the first executable.
         --  A test above allows this only if there is one trace file.
         if Routine_List_Filename = null then
            Build_Routines_Name (Exe_File.all);
         end if;

         Add_Subprograms_Traces (Exe_File, Base);
      end;
      Arg_Index := Arg_Index + 1;
   end loop;

   Traces_Names.Build_Routines_Trace_State;

   if Annotations /= Annotate_Asm then
      Traces_Names.Build_Source_Lines;
   end if;

   --  ??? For now, Generate_Report still suppose that only one Exec is
   --  provided to xcov. What it should do when several Execs are given
   --  is still to be defined.
   First_Exec := Deprecated_First_Exec (Exec_Base);

   case Annotations is
      when Annotate_Asm =>
         if Action in Stmt_Coverage .. MCDC_Coverage then
            Error ("Asm format not supported for source coverage.");
            return;
         end if;
         Traces_Disa.Flag_Show_Asm := True;
         Traces_Names.Dump_Routines_Traces;
      when Annotate_Xcov =>
         Traces_Sources.Xcov.Generate_Report (Base,
                                              First_Exec.all);
      when Annotate_Html =>
         Traces_Sources.Html.Generate_Report (Base,
                                              First_Exec.all);
      when Annotate_Xcov_Asm =>
         Traces_Disa.Flag_Show_Asm := True;
         Traces_Sources.Xcov.Generate_Report (Base,
                                              First_Exec.all);
      when Annotate_Html_Asm =>
         Traces_Disa.Flag_Show_Asm := True;
         Traces_Sources.Html.Generate_Report (Base,
                                              First_Exec.all);
      when Annotate_Unknown =>
         Put_Line ("Please specify an annotation format.");
         return;
   end case;
end Xcov;
