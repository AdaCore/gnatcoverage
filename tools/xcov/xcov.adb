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
with Version;

procedure Xcov is
   procedure Usage;
   procedure Error (Msg : String);
   function Parse_Hex (S : String; Flag_Name : String) return Pc_Type;

   procedure Usage
   is
      procedure P (S : String) renames Put_Line;
   begin
      P ("usage: " & Command_Name & " actions");
      P ("actions is a list of:");
      P (" -r FILENAME         Read (and merge) traces from FILENAME");
      P (" -w FILENAME         Write traces to FILENAME");
      P (" -e FILENAME         Use FILENAME as executable");
      P (" --dump-traces       Dump traces");
      P (" --exe-coverage      Generate object coverage report");
      P (" --source-coverage   Generate source coverage report");
      P (" --function-coverage Generate function coverage report");
      P (" --asm               Add assembly code in --source-coverage");
      P (" --output-format=html/xcov/gcov  Select output format");
      P (" --level=A/C         Select DO178B level for --source-coverage");
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

   Has_Exec : Boolean := False;
   Text_Start : Pc_Type := 0;
   type Output_Format is (Format_Xcov, Format_Gcov, Format_Html);
   Format : Output_Format := Format_Xcov;
   Base : Traces_Base;

   Exec : Exe_File_Type;
   Sub_Exec : Exe_File_Type;
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
            begin
               Open_File (Sub_Exec, Argument (Arg_Index), Text_Start);
            exception
               when others =>
                  Error ("cannot open " & Argument (Arg_Index));
                  return;
            end;
            Build_Sections (Sub_Exec);
            Build_Symbols (Sub_Exec);
            begin
               Read_Trace_File (Base, Argument (Arg_Index + 1));
            exception
               when others =>
                  Error ("cannot open tracefile " & Argument (Arg_Index + 1));
                  raise;
            end;
            Add_Subprograms_Traces (Sub_Exec, Base);
            Init_Base (Base);
            Arg_Index := Arg_Index + 2;
         end loop;
         Traces_Names.Dump_Routines_Traces (Exec);
         return;
      elsif Cmd = "--dump-trace-file" then
         if Arg_Index = Arg_Count then
            Put_Line ("missing FILENAME to --dump-trace-file");
            return;
         end if;
         for I in Arg_Index + 1 .. Arg_Count loop
            Dump_Trace_File (Argument (I));
         end loop;
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
            Read_Trace_File (Base, Argument (Arg_Index));
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
            Write_Trace_File (Base, Argument (Arg_Index));
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
            Build_Routine_Names (Exec);
            Build_Sections (Exec);
            Build_Symbols (Exec);
            Add_Subprograms_Traces (Exec, Base);
            Traces_Names.Dump_Routines_Traces (Exec);
         else
            Error ("unknown option: " & Arg);
            return;
         end if;
      end;
   end loop;
end Xcov;
