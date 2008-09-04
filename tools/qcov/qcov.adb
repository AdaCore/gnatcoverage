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
with Display;

procedure Qcov is
   procedure Usage is
   begin
      Put_Line ("usage: " & Command_Name & " actions");
      Put_Line ("actions is a list of:");
      Put_Line (" -r FILENAME         Read (and merge) traces from FILENAME");
      Put_Line (" -w FILENAME         Write traces to FILENAME");
      Put_Line (" --dump-traces       Dump traces");
      Put_Line (" --objdump-coverage  Annotate objdump -d output");
      Put_Line (" -e FILENAME         Use FILENAME as executale");
      Put_Line (" --color             Use vt100 colors in outputs");
   end Usage;

   Arg_Index : Natural;
   Arg_Count : constant Natural := Argument_Count;

   Has_Exec : Boolean := False;
begin
   --  Require at least one argument.
   if Arg_Count = 0 then
      Usage;
      return;
   end if;

   Arg_Index := 1;
   while Arg_Index <= Arg_Count loop
      declare
         Arg : constant String := Argument (Arg_Index);
      begin
         Arg_Index := Arg_Index + 1;
         if Arg = "-r" then
            if Arg_Index > Arg_Count then
               Put_Line ("missing FILENAME to -r");
               return;
            end if;
            Read_Trace_File (Argument (Arg_Index));
            Arg_Index := Arg_Index + 1;
         elsif Arg = "--dump-traces" then
            Dump_Traces;
         elsif Arg = "--dump-traces-state" then
            Build_Sections;
            Set_Trace_State;
            Dump_Traces;
         elsif Arg = "-w" then
            if Arg_Index > Arg_Count then
               Put_Line ("missing FILENAME to -w");
               return;
            end if;
            Write_Trace_File (Argument (Arg_Index));
            Arg_Index := Arg_Index + 1;
         elsif Arg = "-e" then
            if Arg_Index > Arg_Count then
               Put_Line ("missing FILENAME to -e");
               return;
            end if;
            begin
               Open_File (Argument (Arg_Index));
            exception
               when others =>
                  Put_Line ("cannot open " & Argument (Arg_Index));
                  return;
            end;
            Has_Exec := True;
            Arg_Index := Arg_Index + 1;
         elsif Arg = "--objdump-coverage" then
            Annotate_Objdump;
         elsif Arg = "--color" then
            Display.Flag_Color := True;
         elsif Arg = "--exe-coverage" then
            Build_Sections;
            Build_Debug_Compile_Units;
            Set_Trace_State;
            Build_Symbols;
            Disp_Sections_Coverage;
         elsif Arg = "--dump-sections" then
            Build_Sections;
            Disp_Sections_Addresses;
         elsif Arg = "--dump-compile-units" then
            Build_Debug_Compile_Units;
            Disp_Compile_Units_Addresses;
         elsif Arg = "--dump-subprograms" then
            Build_Debug_Compile_Units;
            Disp_Subprograms_Addresses;
         elsif Arg = "--dump-lines" then
            Build_Debug_Lines;
            Disp_Lines_Addresses;
         elsif Arg = "--dump-symbols" then
            Build_Symbols;
            Disp_Symbols_Addresses;
         elsif Arg = "--asm" then
            Flag_Show_Asm := True;
         elsif Arg = "--missing-files" then
            Flag_Show_Missing := True;
         elsif Arg = "--source-coverage" then
            Build_Sections;
            Set_Trace_State;
            Build_Debug_Lines;
            Build_Source_Lines;
            Build_Symbols;
            Disp_Line_State;
         elsif Arg = "--file-coverage" then
            Build_Sections;
            Set_Trace_State;
            Build_Debug_Lines;
            Build_Source_Lines;
            Build_Symbols;
            Disp_File_Summary;
         else
            Put_Line ("unknown option: " & Arg);
            return;
         end if;
      end;
   end loop;
end Qcov;
