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

with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Containers;          use Ada.Containers;

with Coverage;
--  No use clause for this package, to avoid a conflict with Commands.Coverage.
--  ??? We cannot change the command identifier (Commands.Coverage) without
--  changing the command name (and, therefore, xcov's interface). We may want
--  to rename the Coverage package to resolve this conflict.

with Outputs;           use Outputs;
with Inputs;            use Inputs;
with Commands;          use Commands;
with Coverage.Source;   use Coverage.Source;
with Decision_Map;      use Decision_Map;
with Execs_Dbase;       use Execs_Dbase;
with SC_Obligations;    use SC_Obligations;
with Elf_Files;
with Switches;          use Switches;
with Traces;            use Traces;
with Traces_Elf;        use Traces_Elf;
with Slocs;             use Slocs;
with Files_Table;       use Files_Table;
with Traces_Sources.Annotations; use Traces_Sources.Annotations;
with Traces_Sources.Annotations.Html;
with Traces_Sources.Annotations.Xcov;
with Traces_Sources.Annotations.Xml;
with Traces_Sources.Annotations.Report;
with Traces_Names;
with Traces_Dump;
with Traces_Files;      use Traces_Files;
with Traces_Dbase;      use Traces_Dbase;
with Traces_Disa;
with Version;
with Qemudrv;
with Qemu_Traces;
with Strings;           use Strings;
with Traces_Files_List; use Traces_Files_List;

procedure Xcov is

   procedure Usage;
   --  Display usage information for documented commands

   procedure Usage_Dump;
   --  Display usage information for internal debugging commands

   procedure Check_Argument_Available
     (Args    : Inputs.Inputs_Type;
      What    : String;
      Command : Command_Type := No_Command);
   --  Report a fatal error if Args is empty

   ------------------------------
   -- Check_Argument_Available --
   ------------------------------

   procedure Check_Argument_Available
     (Args    : Inputs.Inputs_Type;
      What    : String;
      Command : Command_Type := No_Command) is
   begin
      if Inputs.Length (Args) = 0 then
         Fatal_Error ("missing " & What & " argument "
                      & For_Command_Switch (Command));
      end if;
   end Check_Argument_Available;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      procedure P (S : String) renames Put_Line;
   begin
      P ("Usage: " & Command_Name & " ACTION");
      P ("Action is one of:");
      P (" --help  -h");
      P ("   Display this help");
      New_Line;
      P (" --help-dump");
      P ("   Display extra help for dump commands (useful for maintainance)");
      New_Line;
      P (" --version");
      P ("   Display version");
      New_Line;
      Qemudrv.Help (" ");
      New_Line;
      P (" disp-routines {[--exclude|--include] FILES}");
      P ("    Build a list of routines from object files");
      New_Line;
      P (" coverage OPTIONS TRACE_FILES");
      P ("   Generate coverage report");
      P ("   -c LEVEL --level LEVEL        Specify coverage level");
      P ("      LEVEL is one of " & Coverage.All_Known_Coverage_Levels);
      P ("   -l FILE  --routine-list=FILE  Get routine names from LIST");
      P ("   -a FORM  --annotate=FORM      Generate a FORM report");
      P ("      FORM is one of asm,xcov,html,xcov+asm,html+asm,report");
      P ("   --output-dir=DIR              Generate reports in DIR");
      P ("   -T FILE --trace FILE          Add a trace file to the list");
      New_Line;
   end Usage;

   ----------------
   -- Usage_Dump --
   ----------------

   procedure Usage_Dump is
      procedure P (S : String) renames Put_Line;
   begin
      P ("Debugging commands:");
      New_Line;
      P (" dump-trace FILES");
      P ("   Raw display of trace files");
      New_Line;
      P (" dump-trace-base FILES");
      P ("   Raw display of merged trace files");
      New_Line;
      P (" dump-trace-asm EXE TRACE_FILES");
      P ("   Raw display of trace files with assembly code for each trace");
      New_Line;
      P (" dump-sections EXEs");
      P (" dump-symbols EXEs");
      P (" dump-compile-units EXEs");
      P (" dump-subprograms EXEs");
      P (" dump-lines EXEs");
      P ("   Dump info from executable files");
      New_Line;
      P (" disassemble EXEs");
      P (" disassemble-raw EXEs");
      P ("   Disassemble executables");
      New_Line;
   end Usage_Dump;

   Coverage_Option           : constant String := "--level=";
   Coverage_Option_Short     : constant String := "-c";
   Annotate_Option           : constant String := "--annotate=";
   Annotate_Option_Short     : constant String := "-a";
   Routine_List_Option       : constant String := "--routine-list=";
   Routine_List_Option_Short : constant String := "-l";
   ALI_List_Option           : constant String := "--ali-list=";
   Final_Report_Option       : constant String := "--report=";
   Final_Report_Option_Short : constant String := "-o";
   Output_Dir_Option         : constant String := "--output-dir=";
   Trace_Option_Short        : constant String := "-T";
   Trace_Option              : constant String := "--trace=";

   Command                   : Command_Type := No_Command;
   Annotations               : Annotation_Format := Annotate_Unknown;
   Level                     : Coverage.Coverage_Level;
   Trace_Inputs              : Inputs.Inputs_Type;
   Exe_Inputs                : Inputs.Inputs_Type;
   Excluded_Obj_Inputs       : Inputs.Inputs_Type;
   Included_Obj_Inputs       : Inputs.Inputs_Type;
   Text_Start                : Pc_Type := 0;

   procedure Parse_Command_Line;
   --  Parse the command line and set the above local variables

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      Arg_Index : Natural;
      Arg_Count : constant Natural := Argument_Count;

      function Parse_Hex (S : String; Flag_Name : String) return Pc_Type;
      --  Parse S to get an hexadecimal number (form : 0x[0-9a-f]+) and
      --  return the value. If the parsing fails, fatal error.

      function Option_Parameter (S : String) return String;
      --  Assuming that S is of the form "<part1>=<part2>",
      --  return "<part2>".

      function Begins_With (S : String; Beginning : String) return Boolean;
      --  If the beginning of S is equal to Beginnning, return True;
      --  otherwise, return False.

      function Next_Arg (What : String) return String;
      --  Increment Arg_Index then return Argument (Arg_Index). If
      --  end of command line is reached, display an error message and
      --  raise Constraint_Error.

      procedure Check_Argument_Available
        (What    : String;
         Command : Command_Type := No_Command);
      --  Check that Arg_Index is not greater than Arg_Count. If not, display
      --  an error message and raise Fatal_Error.

      -----------------
      -- Begins_With --
      -----------------

      function Begins_With (S : String; Beginning : String) return Boolean is
         Length : constant Integer := Beginning'Length;
      begin
         return S'Length > Length
           and then S (S'First .. S'First + Length - 1) = Beginning;
      end Begins_With;

      ------------------------------
      -- Check_Argument_Available --
      ------------------------------

      procedure Check_Argument_Available
        (What    : String;
         Command : Command_Type := No_Command) is
      begin
         if Arg_Index > Arg_Count then
            Fatal_Error ("missing " & What & " argument "
                         & For_Command_Switch (Command));
         end if;
      end Check_Argument_Available;

      --------------
      -- Next_Arg --
      --------------

      function Next_Arg (What : String) return String is
      begin
         Arg_Index := Arg_Index + 1;
         Check_Argument_Available (What);
         return Argument (Arg_Index);
      end Next_Arg;

      ----------------------
      -- Option_Parameter --
      ----------------------

      function Option_Parameter (S : String) return String is
      begin
         for J in S'Range loop
            if S (J) = '=' then
               return S (J + 1 .. S'Last);
            end if;
         end loop;
         return S;
      end Option_Parameter;

      ---------------
      -- Parse_Hex --
      ---------------

      function Parse_Hex (S : String; Flag_Name : String) return Pc_Type
      is
         Res : Pc_Type;
         Pos : Natural;
      begin
         if S'Length < 3
           or else S (S'First) /= '0'
           or else (S (S'First + 1) /= 'x' and then S (S'First + 1) /= 'X')
         then
            Fatal_Error ("missing '0x' prefix for " & Flag_Name);
         end if;
         Pos := S'First + 2;
         Get_Pc (Res, S, Pos);
         if Pos <= S'Last then
            Fatal_Error ("bad hexadecimal number for " & Flag_Name);
         end if;
         return Res;
      end Parse_Hex;

      --  Local variables

      Mode_Exclude : Boolean := False;

      --  Start of processing for Command_Line_Handling

   begin
      --  Require at least one argument

      if Arg_Count = 0 then
         Usage;
         Normal_Exit;
      end if;

      --  Decode command

      Arg_Index := 1;
      Command := To_Command (Argument (Arg_Index));
      if Command /= No_Command then
         Arg_Index := Arg_Index + 1;
      end if;

      --  Special case for command "run". The command line is decoded in
      --  qemudrv.adb.
      --  ??? This special case shall be removed at some point. The command
      --  line parsing should be done in xcov.adb, and Qemudrv.Driver should
      --  not depend on Ada.Command_Line.
      if Command = Run then
         Qemudrv.Driver (Arg_Index);
         Normal_Exit;
      end if;

      --  Decode options

      while Arg_Index <= Arg_Count loop
         declare
            Arg : String renames Argument (Arg_Index);
         begin
            if Arg = "-h" or Arg = "--help" then
               Check_Option (Arg, Command, (1 => No_Command));
               Usage;
               Normal_Exit;

            elsif Arg = "--help-dump" then
               Check_Option (Arg, Command, (1 => No_Command));
               Usage_Dump;
               Normal_Exit;

            elsif Arg = "--version" then
               Check_Option (Arg, Command, (1 => No_Command));
               Put_Line ("XCOV Pro " & Standard.Version.Xcov_Version);
               Normal_Exit;

            elsif Arg = "-v" then
               Verbose := True;

            elsif Arg = Coverage_Option_Short then
               Check_Option (Arg, Command, (1 => Commands.Coverage,
                                            2 => Run));
               Level :=
                 Coverage.To_Coverage_Level (Next_Arg ("coverage level"));
               if Coverage."=" (Level, Coverage.Unknown) then
                  Fatal_Error ("bad parameter for " & Coverage_Option_Short);
               end if;
               Coverage.Set_Coverage_Level (Level);

            elsif Begins_With (Arg, Coverage_Option) then
               Check_Option (Arg, Command, (1 => Commands.Coverage,
                                            2 => Run));
               Level := Coverage.To_Coverage_Level (Option_Parameter (Arg));
               if Coverage."=" (Level, Coverage.Unknown) then
                  Fatal_Error ("bad parameter for " & Coverage_Option);
               end if;
               Coverage.Set_Coverage_Level (Level);

            elsif Begins_With (Arg, ALI_List_Option) then
               Check_Option (Arg, Command, (1 => Map_Routines,
                                            2 => Commands.Coverage,
                                            3 => Run));
               ALI_List_Filename :=
                 new String'(Option_Parameter (Arg));

            elsif Arg = Routine_List_Option_Short then
               Check_Option (Arg, Command, (1 => Commands.Coverage,
                                            2 => Run));
               Routine_List_Filename :=
                 new String'(Next_Arg ("function list"));

            elsif Begins_With (Arg, Routine_List_Option) then
               Check_Option (Arg, Command, (1 => Commands.Coverage,
                                            2 => Run));
               Routine_List_Filename := new String'(Option_Parameter (Arg));

            elsif Arg = "--missing-files" then
               Check_Option (Arg, Command, (1 => Commands.Coverage));
               Flag_Show_Missing := True;

            elsif Begins_With (Arg, "--text-start=") then
               --  FIXME: not yet supported???
               --  Should be a global option (used when building decision map
               --  for --run)???

               begin
                  Text_Start := Parse_Hex
                    (Arg (Arg'First + 13 .. Arg'Last), "--text-start");
               exception
                  when Constraint_Error =>
                     Fatal_Error ("Failure to parse --text-start");
               end;

            elsif Begins_With (Arg, "--source-rebase=") then
               Check_Option (Arg, Command, (1 => Commands.Coverage));
               declare
                  Pos : Natural := 0;
               begin
                  --  Parse source-rebase's argument. This option's form should
                  --  be "--source-rebase=<OLD_PREFIX>=<NEW_PREFIX>".
                  for I in Arg'First + 16 .. Arg'Last loop
                     if Arg (I) = '=' then
                        Pos := I;
                        exit;
                     end if;
                  end loop;
                  if Pos = 0 then
                     Fatal_Error ("missing '=' in --source-rebase=");
                  end if;
                  Add_Source_Rebase (Arg (Arg'First + 16 .. Pos - 1),
                                     Arg (Pos + 1 .. Arg'Last));
               end;

            elsif Begins_With (Arg, "--source-search=") then
               Check_Option (Arg, Command, (1 => Commands.Coverage));
               Add_Source_Search (Arg (Arg'First + 16 .. Arg'Last));

            elsif Arg = Annotate_Option_Short then
               Check_Option (Arg, Command, (1 => Commands.Coverage));
               Annotations :=
                 To_Annotation_Format (Next_Arg ("annotation format"));
               if Annotations = Annotate_Unknown then
                  Fatal_Error ("bad parameter for " & Annotate_Option_Short);
               end if;

            elsif Begins_With (Arg, Annotate_Option) then
               Check_Option (Arg, Command, (1 => Commands.Coverage));
               Annotations := To_Annotation_Format (Option_Parameter (Arg));
               if Annotations = Annotate_Unknown then
                  Fatal_Error ("bad parameter for " & Annotate_Option);
               end if;

            elsif Arg = Final_Report_Option_Short then
               Check_Option (Arg, Command, (1 => Commands.Coverage,
                                            2 => Run));
               Traces_Sources.Annotations.Report.Open_Report_File
                 (Next_Arg ("final report name"));

            elsif Begins_With (Arg, Final_Report_Option) then
               Check_Option (Arg, Command, (1 => Commands.Coverage));
               Traces_Sources.Annotations.Report.Open_Report_File
                 (Option_Parameter (Arg));

            elsif Begins_With (Arg, Output_Dir_Option) then
               Check_Option (Arg, Command, (1 => Commands.Coverage));
               Outputs.Set_Output_Dir (Option_Parameter (Arg));

            elsif Arg = Trace_Option_Short then
               Check_Option (Arg, Command, (1 => Commands.Coverage,
                                            2 => Dump_Trace,
                                            3 => Dump_Trace_Base,
                                            4 => Dump_Trace_Asm));
               Inputs.Add_Input (Trace_Inputs, Next_Arg ("trace file"));

            elsif Begins_With (Arg, Trace_Option) then
               Check_Option (Arg, Command, (1 => Commands.Coverage,
                                            2 => Dump_Trace,
                                            3 => Dump_Trace_Base,
                                            4 => Dump_Trace_Asm));
               Inputs.Add_Input (Trace_Inputs, Option_Parameter (Arg));

            elsif Arg = "--exclude" then
               Check_Option (Arg, Command, (1 => Disp_Routines));
               Mode_Exclude := True;

            elsif Arg = "--include" then
               Check_Option (Arg, Command, (1 => Disp_Routines));
               Mode_Exclude := False;

            elsif Arg (1) = '-' then
               Fatal_Error ("unknown option: " & Arg);

            else
               --  Handling of parameters that are not options (e.g. file list)

               case Command is
                  when No_Command =>
                     Fatal_Error ("No command specified");

                  when Commands.Coverage
                    | Dump_Trace
                    | Dump_Trace_Base =>
                     Inputs.Add_Input (Trace_Inputs, Arg);

                  when Disp_Routines =>
                     if Mode_Exclude then
                        Inputs.Add_Input (Excluded_Obj_Inputs, Arg);
                     else
                        Inputs.Add_Input (Included_Obj_Inputs, Arg);
                     end if;

                  when Dump_Sections
                    | Dump_Symbols
                    | Dump_Compile_Units
                    | Dump_Subprograms
                    | Dump_Lines
                    | Disassemble_Raw
                    | Disassemble =>
                     Inputs.Add_Input (Exe_Inputs, Arg);

                  when Map_Routines =>
                     Inputs.Add_Input (Exe_Inputs, Arg);

                  when Run =>
                     if Inputs.Length (Exe_Inputs) > 1 then
                        Fatal_Error ("Only one EXEC parameter is allowed with "
                                     & To_Switch (Command));
                     end if;
                     Inputs.Add_Input (Exe_Inputs, Arg);

                  when Dump_Trace_Asm =>
                     if Inputs.Length (Exe_Inputs) < 1 then
                        Inputs.Add_Input (Exe_Inputs, Arg);
                     else
                        Inputs.Add_Input (Trace_Inputs, Arg);
                     end if;
               end case;
            end if;
         end;

         Arg_Index := Arg_Index + 1;
      end loop;
   end Parse_Command_Line;

   Base : aliased Traces_Base;
   Exec : aliased Exe_File_Type;

   --  Start of processing for Xcov

begin
   Parse_Command_Line;

   --  Now execute the specified command

   case Command is
      when No_Command =>
         Usage;
         return;

      when Disp_Routines =>
         declare

            procedure Include_Routine (Obj_File_Name : String);
            --  Include routines of Obj_File_Name in the routine
            --  database.

            procedure Exclude_Routine (Obj_File_Name : String);
            --  Exclude routines of Obj_File_Name from the routine
            --  database.

            ---------------------
            -- Exclude_Routine --
            ---------------------

            procedure Exclude_Routine (Obj_File_Name : String) is
            begin
               Traces_Elf.Read_Routines_Name
                 (Obj_File_Name,
                  Exclude   => True,
                  Keep_Open => False);
            exception
               when Elf_Files.Error =>
                  Fatal_Error ("can't open: " & Obj_File_Name);
            end Exclude_Routine;

            ---------------------
            -- Include_Routine --
            ---------------------

            procedure Include_Routine (Obj_File_Name : String) is
            begin
               Traces_Elf.Read_Routines_Name
                 (Obj_File_Name,
                  Exclude   => False,
                  Keep_Open => False);
            exception
               when Elf_Files.Error =>
                  Fatal_Error ("can't open: " & Obj_File_Name);
            end Include_Routine;

         begin
            Check_Argument_Available (Included_Obj_Inputs, "EXEC", Command);
            Inputs.Iterate (Included_Obj_Inputs, Include_Routine'Access);
            Inputs.Iterate (Excluded_Obj_Inputs, Exclude_Routine'Access);
            Traces_Names.Disp_All_Routines;
            return;
         end;

      when Map_Routines =>
         if ALI_List_Filename = null then
            Fatal_Error ("Please give a ALI list using " & ALI_List_Option);
         end if;
         Load_SCOs (ALI_List_Filename.all);
         Inputs.Iterate (Exe_Inputs, Build_Decision_Map'Access);
         return;

      when Dump_Trace =>
         Check_Argument_Available (Trace_Inputs, "TRACEFILEs", Command);
         Inputs.Iterate (Trace_Inputs, Dump_Trace_File'Access);
         return;

      when Dump_Trace_Base =>
         declare
            procedure Dump_Trace_Base (Trace_File_Name : String);
            --  Raw display of merged trace files

            ---------------------
            -- Dump_Trace_Base --
            ---------------------

            procedure Dump_Trace_Base (Trace_File_Name : String) is
               Trace_File : constant Trace_File_Element_Acc :=
                 new Trace_File_Element;
            begin
               Read_Trace_File (Trace_File_Name, Trace_File.Trace, Base);
               Dump_Traces (Base);
            end Dump_Trace_Base;

         begin
            Check_Argument_Available (Trace_Inputs, "TRACEFILEs", Command);
            Inputs.Iterate (Trace_Inputs, Dump_Trace_Base'Access);
         end;
         return;

      when Dump_Trace_Asm =>
         declare

            procedure Open_Exec (Exec_File_Name : String);
            --  Open Exec_File_Name and build its sections and symbol
            --  information.

            procedure Dump_Trace (Trace_File_Name : String);
            --  Raw display of Trace_File_Name with assembly code

            ----------------
            -- Dump_Trace --
            ----------------

            procedure Dump_Trace (Trace_File_Name : String) is
            begin
               Traces_Disa.Dump_Traces_With_Asm (Exec, Trace_File_Name);
            end Dump_Trace;

            ---------------
            -- Open_Exec --
            ---------------

            procedure Open_Exec (Exec_File_Name : String) is
            begin
               Open_File (Exec, Exec_File_Name, Text_Start);
               Build_Sections (Exec);
               Build_Symbols (Exec'Unchecked_Access);
            end Open_Exec;

         begin
            Check_Argument_Available (Exe_Inputs, "EXEC", Command);
            Check_Argument_Available (Trace_Inputs, "TRACEFILEs", Command);
            Inputs.Iterate (Exe_Inputs, Open_Exec'Access);
            Inputs.Iterate (Trace_Inputs, Dump_Trace'Access);
         end;
         return;

      when Dump_Sections
        | Dump_Symbols
        | Dump_Subprograms
        | Dump_Lines =>
         declare

            procedure Dump_Exec (Exec_File_Name : String);
            --  Dump Exec_File_Name's sections|symbols|subprograms|lines,
            --  depending on the current command.

            ---------------
            -- Dump_Exec --
            ---------------

            procedure Dump_Exec (Exec_File_Name : String) is
               To_Display : Addresses_Kind;
            begin
               Open_File (Exec, Exec_File_Name, 0);
               Build_Sections (Exec);

               case Command is
                  when Dump_Sections =>
                     To_Display := Section_Addresses;

                  when Dump_Symbols =>
                     Build_Symbols (Exec'Unchecked_Access);
                     To_Display := Symbol_Addresses;

                  when Dump_Subprograms =>
                     Build_Debug_Compile_Units (Exec);
                     To_Display := Subprogram_Addresses;

                  when Dump_Lines =>
                     Build_Debug_Lines (Exec);
                     To_Display := Line_Addresses;

                  when others =>
                     --  Never happens

                     pragma Assert (False);
                     return;
               end case;

               Disp_Addresses (Exec, To_Display);
               Close_File (Exec);
            end Dump_Exec;

         begin
            Check_Argument_Available (Exe_Inputs, "EXECs", Command);
            Inputs.Iterate (Exe_Inputs, Dump_Exec'Access);
         end;
         return;

      when Dump_Compile_Units =>
         declare
            procedure Dump_Compilation_Units (Exec_File_Name : String);
            --  Dump Exec_File_Name's compilation units

            ----------------------------
            -- Dump_Compilation_Units --
            ----------------------------

            procedure Dump_Compilation_Units (Exec_File_Name : String) is
            begin
               Open_File (Exec, Exec_File_Name, 0);
               Build_Sections (Exec);
               Build_Debug_Compile_Units (Exec);
               Disp_Compilation_Units (Exec);
               Close_File (Exec);
            end Dump_Compilation_Units;

         begin
            Check_Argument_Available (Exe_Inputs, "EXECs", Command);
            Inputs.Iterate (Exe_Inputs, Dump_Compilation_Units'Access);
         end;
         return;

      when Disassemble_Raw =>
         declare

            procedure Disassemble (Exec_File_Name : String);
            --  Disassemble Exec_File_Name and display the raw result

            -----------------
            -- Disassemble --
            -----------------

            procedure Disassemble (Exec_File_Name : String) is
            begin
               Open_File (Exec, Exec_File_Name, 0);
               Disassemble_File_Raw (Exec);
               Close_File (Exec);
            end Disassemble;

         begin
            Check_Argument_Available (Exe_Inputs, "EXECs", Command);
            Inputs.Iterate (Exe_Inputs, Disassemble'Access);
         end;
         return;

      when Disassemble =>
         declare

            procedure Disassemble (Exec_File_Name : String);
            --  Disassemble Exec_File_Name and display the raw result

            -----------------
            -- Disassemble --
            -----------------

            procedure Disassemble (Exec_File_Name : String) is
            begin
               Open_File (Exec, Exec_File_Name, 0);
               Build_Sections (Exec);
               Build_Symbols (Exec'Unchecked_Access);
               Disassemble_File (Exec);
               Close_File (Exec);
            end Disassemble;

         begin
            Check_Argument_Available (Exe_Inputs, "EXECs", Command);
            Inputs.Iterate (Exe_Inputs, Disassemble'Access);
         end;
         return;

      when Commands.Coverage =>
         declare
            procedure Process_Trace (Trace_File_Name : String);
            --  Open Trace_File_Name and merge it into the trace database

            -------------------
            -- Process_Trace --
            -------------------

            procedure Process_Trace (Trace_File_Name : String) is
               use Coverage;

               Trace_File : constant Trace_File_Element_Acc :=
                              new Trace_File_Element;
            begin
               Init_Base (Base, Full_History => Get_Coverage_Level = MCDC);
               Trace_File.Filename := new String'(Trace_File_Name);
               Read_Trace_File (Trace_File.Filename.all,
                                Trace_File.Trace, Base);
               Traces_Files_List.Files.Append (Trace_File);
               declare
                  Exe_Name : constant String :=
                    Get_Info (Trace_File.Trace, Qemu_Traces.Exec_File_Name);
                  Exe_File : Exe_File_Acc;
               begin
                  if Exe_Name = "" then
                     Fatal_Error ("cannot find exec filename in trace file "
                                  & Trace_File_Name);
                  end if;
                  begin
                     Open_Exec (Get_Exec_Base, Exe_Name, Exe_File);
                  exception
                     when Elf_Files.Error =>
                        Fatal_Error ("cannot open ELF file "
                                     & Exe_Name
                                     & " for trace file "
                                     & Trace_File_Name);
                  end;

                  --  If there is not routine list, create it from the first
                  --  executable. A test above allows this only if there is one
                  --  trace file.

                  if Routine_List_Filename = null then
                     Read_Routines_Name (Exe_File, Exclude => False);
                  end if;

                  Load_Code_And_Traces (Exe_File, Base'Access);
               end;
            end Process_Trace;

         begin
            Check_Argument_Available (Trace_Inputs, "TRACEFILEs", Command);

            if Coverage."=" (Coverage.Get_Coverage_Level,
                             Coverage.Unknown) then
               Fatal_Error ("Please specify a coverage level");
            end if;

            if Routine_List_Filename /= null then
               Traces_Names.Read_Routines_Name_From_Text
                 (Routine_List_Filename.all);
            else
               if Inputs.Length (Trace_Inputs) > 1 then
                  Fatal_Error ("routine list required"
                               & " when reading multiple trace files");
               end if;
            end if;

            --  Read traces

            Inputs.Iterate (Trace_Inputs, Process_Trace'Access);

            --  Now determine coverage according to the requested metric

            case Coverage.Get_Coverage_Level is
               when Coverage.Object_Coverage_Level =>
                  Traces_Elf.Build_Routines_Insn_State;

                  if Annotations /= Annotate_Asm then
                     Traces_Elf.Build_Source_Lines;
                  end if;

               when Coverage.Source_Coverage_Level =>
                  Load_SCOs (ALI_List_Filename.all);
                  Coverage.Source.Process_Traces (Base);

                  Error ("Source coverage is not implemented yet");

               when Coverage.Unknown =>
                  --  A fatal error should have been diagnosed earlier

                  pragma Assert (False);
                  return;
            end case;

            case Annotations is
               when Annotate_Asm =>
                  if Coverage.Get_Coverage_Level
                    in Coverage.Source_Coverage_Level then
                     Error ("Asm format not supported for source coverage.");
                     return;
                  end if;
                  Traces_Disa.Flag_Show_Asm := True;
                  Coverage.Dump_Coverage_Option (Standard_Output);
                  Traces_Dump.Dump_Routines_Traces;

               when Annotate_Xcov =>
                  Traces_Sources.Annotations.Xcov.Generate_Report (False);

               when Annotate_Html =>
                  Traces_Sources.Annotations.Html.Generate_Report (False);

               when Annotate_Xml =>
                  Traces_Sources.Annotations.Xml.Generate_Report;

               when Annotate_Xcov_Asm =>
                  --  Case of source coverage???
                  Traces_Sources.Annotations.Xcov.Generate_Report (True);

               when Annotate_Html_Asm =>
                  --  Case of source coverage???
                  Traces_Sources.Annotations.Html.Generate_Report (True);

               when Annotate_Report =>
                  Coverage.Dump_Coverage_Option
                    (Traces_Sources.Annotations.Report.Get_Output);
                  Traces_Dump.Dump_Uncovered_Routines
                    (Traces_Sources.Annotations.Report.Get_Output);
                  Traces_Sources.Annotations.Report.Finalize_Report;

               when Annotate_Unknown =>
                  Fatal_Error ("Please specify an annotation format.");
            end case;
         end;

      when Run =>
         --  We should never end up here. Run has been dealt with
         --  earlier.
         pragma Assert (False);
         return;

   end case;

exception
   when Xcov_Exit_Exc =>
      --  An error message has already been displayed

      null;
end Xcov;
