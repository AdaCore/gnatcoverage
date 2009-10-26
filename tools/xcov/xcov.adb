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

with Coverage;          use Coverage;
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
with Annotations;       use Annotations;
with Annotations.Html;
with Annotations.Xcov;
with Annotations.Xml;
with Annotations.Report;
with Traces_Names;
with Traces_Dump;
with Traces_Files;      use Traces_Files;
with Traces_Dbase;      use Traces_Dbase;
with Traces_Disa;
with Version;
with Qemudrv;
with Qemu_Traces;
with Traces_Files_List; use Traces_Files_List;
with GNAT.Strings;      use GNAT.Strings;

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
      P ("   Build a list of routines from object files");
      New_Line;
      P (" coverage OPTIONS TRACE_FILES");
      P ("   Generate coverage report");
      P ("   -c LEVEL --level=LEVEL        Specify coverage level");
      P ("      LEVEL is one of " & All_Known_Coverage_Levels);
      P ("   -a FORM  --annotate=FORM      Generate a FORM report");
      P ("      FORM is one of asm,xcov,html,xcov+asm,html+asm,report");
      P ("   --routines=<FILE|@LISTFILE> Add ROUTINE, or all routine listed");
      P ("                               in LISTFILE to the list of routines");
      P ("   --scos=<FILE|@LISTFILE>     Add FILE being an ALI file,");
      P ("                               consider all its scos for this");
      P ("                               operation; or, do that for each ALI");
      P ("                               file listed in LISTFILE");
      P ("   --output-dir=DIR              Generate reports in DIR");
      P ("   -T FILE --trace=FILE          Add a trace file to the list");
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

   --  General options

   Coverage_Option           : constant String := "--level=";
   Coverage_Option_Short     : constant String := "-c";
   Annotate_Option           : constant String := "--annotate=";
   Annotate_Option_Short     : constant String := "-a";
   Routines_Option           : constant String := "--routines=";
   SCOs_Option               : constant String := "--scos=";
   Final_Report_Option       : constant String := "--report=";
   Output_Dir_Option         : constant String := "--output-dir=";
   Trace_Option              : constant String := "--trace=";
   Trace_Option_Short        : constant String := "-T";
   Target_Option             : constant String := "--target=";
   Target_Option_Short       : constant String := "-t";
   Output_Option             : constant String := "--output=";
   Output_Option_Short       : constant String := "-o";
   Tag_Option                : constant String := "--tag=";
   Verbose_Option            : constant String := "--verbose";
   Verbose_Option_Short      : constant String := "-v";
   Eargs_Option              : constant String := "-eargs";

   --  Undocumented (maintenance only) options

   Exec_Option               : constant String := "--exec=";
   --  --exec=E tells xcov to use E as the base executable for all the traces
   --  passed for analysis on the xcov command line.

   Deprecated_Routine_List_Option       : constant String := "--routine-list=";
   Deprecated_Routine_List_Option_Short : constant String := "-l";
   --  "--routine-list=@LISTFILE" adds all routines listed in LISTFILE to the
   --  list of routines. Same thing for  "-l ".
   --  This option is now deprecated; use --routines=@LISTFILE instead.

   --  Results of the command line parsing

   Command             : Command_Type := No_Command;
   Annotation          : Annotation_Format := Annotate_Unknown;
   Level               : Coverage_Level;
   Trace_Inputs        : Inputs.Inputs_Type;
   Exe_Inputs          : Inputs.Inputs_Type;
   Obj_Inputs          : Inputs.Inputs_Type;
   SCOs_Inputs         : Inputs.Inputs_Type;
   Routines_Inputs     : Inputs.Inputs_Type;
   Text_Start          : Pc_Type := 0;
   Target              : String_Access := null;
   Output              : String_Access := null;
   Tag                 : String_Access := null;
   Eargs               : String_List_Access := null;

   Opt_Exe_Name : String_Access := null;
   --  Path to executable from the command line; it overrides the default one
   --  from trace files.

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

      function Rest_Of_Command_Line return String_List_Access;
      --  Return the rest of the command line in a string list

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

      --------------------------
      -- Rest_Of_Command_Line --
      --------------------------

      function Rest_Of_Command_Line return String_List_Access is
         Result : constant String_List_Access :=
           new String_List (1 .. Arg_Count - Arg_Index);
         I : Natural := 1;
      begin
         while Arg_Index < Arg_Count loop
            Result (I) := new String'(Next_Arg ("eargs"));
            I := I + 1;
         end loop;
         return Result;
      end Rest_Of_Command_Line;

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

            elsif Arg = Verbose_Option
              or else Arg = Verbose_Option_Short
            then
               Verbose := True;

            elsif Arg = Eargs_Option then
               Check_Option (Arg, Command, (1 => Cmd_Run));
               Eargs := Rest_Of_Command_Line;
               return;

            elsif Arg = Target_Option_Short then
               Check_Option (Arg, Command, (1 => Cmd_Run));
               Target := new String'(Next_Arg ("target"));

            elsif Begins_With (Arg, Target_Option) then
               Check_Option (Arg, Command, (1 => Cmd_Run));
               Target := new String'(Option_Parameter (Arg));

            elsif Arg = Output_Option_Short then
               Check_Option (Arg, Command, (1 => Cmd_Run,
                                            2 => Cmd_Coverage));
               Output := new String'(Next_Arg ("output"));

            elsif Begins_With (Arg, Output_Option) then
               Check_Option (Arg, Command, (1 => Cmd_Run,
                                            2 => Cmd_Coverage));
               Output := new String'(Option_Parameter (Arg));

            elsif Begins_With (Arg, Tag_Option) then
               Check_Option (Arg, Command, (1 => Cmd_Run));
               Tag := new String'(Option_Parameter (Arg));

            elsif Arg = Coverage_Option_Short then
               Check_Option (Arg, Command, (1 => Cmd_Coverage,
                                            2 => Cmd_Run));
               Level :=
                 To_Coverage_Level (Next_Arg ("coverage level"));
               if Level = Unknown then
                  Fatal_Error ("bad parameter for " & Coverage_Option_Short);
               end if;
               Set_Coverage_Level (Level);

            elsif Begins_With (Arg, Coverage_Option) then
               Check_Option (Arg, Command, (1 => Cmd_Coverage,
                                            2 => Cmd_Run));
               Level := To_Coverage_Level (Option_Parameter (Arg));
               if Level = Unknown then
                  Fatal_Error ("bad parameter for " & Coverage_Option);
               end if;
               Set_Coverage_Level (Level);

            elsif Begins_With (Arg, SCOs_Option) then
               Check_Option (Arg, Command, (1 => Cmd_Map_Routines,
                                            2 => Cmd_Coverage,
                                            3 => Cmd_Run));
               Inputs.Add_Input (SCOs_Inputs, Option_Parameter (Arg));

            elsif Begins_With (Arg, Routines_Option) then
               Check_Option (Arg, Command, (1 => Cmd_Map_Routines,
                                            2 => Cmd_Coverage,
                                            3 => Cmd_Run));
               Inputs.Add_Input (Routines_Inputs, Option_Parameter (Arg));

            elsif Arg = Deprecated_Routine_List_Option_Short then
               Check_Option (Arg, Command, (1 => Cmd_Coverage,
                                            2 => Cmd_Run));
               Inputs.Add_Input (Routines_Inputs,
                                 "@" & Next_Arg ("function list"));

            elsif Begins_With (Arg, Deprecated_Routine_List_Option) then
               Check_Option (Arg, Command, (1 => Cmd_Coverage,
                                            2 => Cmd_Run));
               Inputs.Add_Input (Routines_Inputs,
                                 "@" & Option_Parameter (Arg));

            elsif Begins_With (Arg, Exec_Option) then
               Check_Option (Arg, Command, (1 => Cmd_Coverage));
               Opt_Exe_Name := new String'(Option_Parameter (Arg));

            elsif Arg = "--missing-files" then
               Check_Option (Arg, Command, (1 => Cmd_Coverage));
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
               Check_Option (Arg, Command, (1 => Cmd_Coverage));
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
               Check_Option (Arg, Command, (1 => Cmd_Coverage));
               Add_Source_Search (Arg (Arg'First + 16 .. Arg'Last));

            elsif Arg = Annotate_Option_Short then
               Check_Option (Arg, Command, (1 => Cmd_Coverage));
               Annotation :=
                 To_Annotation_Format (Next_Arg ("annotation format"));
               if Annotation = Annotate_Unknown then
                  Fatal_Error ("bad parameter for " & Annotate_Option_Short);
               end if;

            elsif Begins_With (Arg, Annotate_Option) then
               Check_Option (Arg, Command, (1 => Cmd_Coverage));
               Annotation := To_Annotation_Format (Option_Parameter (Arg));
               if Annotation = Annotate_Unknown then
                  Fatal_Error ("bad parameter for " & Annotate_Option);
               end if;

            elsif Begins_With (Arg, Final_Report_Option) then
               Check_Option (Arg, Command, (1 => Cmd_Coverage));
               Annotations.Report.Open_Report_File
                 (Option_Parameter (Arg));

            elsif Begins_With (Arg, Output_Dir_Option) then
               Check_Option (Arg, Command, (1 => Cmd_Coverage));
               Outputs.Set_Output_Dir (Option_Parameter (Arg));

            elsif Arg = Trace_Option_Short then
               Check_Option (Arg, Command, (1 => Cmd_Coverage,
                                            2 => Cmd_Dump_Trace,
                                            3 => Cmd_Dump_Trace_Base,
                                            4 => Cmd_Dump_Trace_Asm,
                                            5 => Cmd_Run));

               --  Tag_Option_Short conflicts with Trace_Option_Short...
               if Command = Cmd_Run then
                  Tag := new String'(Next_Arg (Arg));
               else
                  Inputs.Add_Input (Trace_Inputs, Next_Arg ("trace file"));
               end if;

            elsif Begins_With (Arg, Trace_Option) then
               Check_Option (Arg, Command, (1 => Cmd_Coverage,
                                            2 => Cmd_Dump_Trace,
                                            3 => Cmd_Dump_Trace_Base,
                                            4 => Cmd_Dump_Trace_Asm));
               Inputs.Add_Input (Trace_Inputs, Option_Parameter (Arg));

            elsif Arg = "--exclude" then
               Inputs.Add_Input (Obj_Inputs, Arg);

            elsif Arg = "--include" then
               Inputs.Add_Input (Obj_Inputs, Arg);

            elsif Arg (1) = '-' then
               Fatal_Error ("unknown option: " & Arg);

            else
               --  Handling of parameters that are not options (e.g. file list)

               case Command is
                  when No_Command =>
                     Fatal_Error ("No command specified");

                  when Cmd_Coverage
                    | Cmd_Dump_Trace
                    | Cmd_Dump_Trace_Base =>
                     Inputs.Add_Input (Trace_Inputs, Arg);

                  when Cmd_Disp_Routines =>
                     Inputs.Add_Input (Obj_Inputs, Arg);

                  when Cmd_Dump_Sections
                    | Cmd_Dump_Symbols
                    | Cmd_Dump_Compile_Units
                    | Cmd_Dump_Subprograms
                    | Cmd_Dump_Lines
                    | Cmd_Disassemble_Raw
                    | Cmd_Disassemble =>
                     Inputs.Add_Input (Exe_Inputs, Arg);

                  when Cmd_Map_Routines =>
                     Inputs.Add_Input (Exe_Inputs, Arg);

                  when Cmd_Run =>
                     if Inputs.Length (Exe_Inputs) > 1 then
                        Fatal_Error ("Only one EXEC parameter is allowed with "
                                     & To_Switch (Command));
                     end if;
                     Inputs.Add_Input (Exe_Inputs, Arg);

                  when Cmd_Dump_Trace_Asm =>
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

      when Cmd_Disp_Routines =>
         declare
            Mode_Exclude : Boolean := False;

            procedure Read_Routine_Name (Disp_Routine_Arg : String);
            --  Process Disp_Routine_Arg:
            --  * if it equals "--exclude", switch mode to exclude;
            --  * if it equals "--include", switch mode to include;
            --  * otherwise, consider Disp_Routine_Arg as a object file name
            --    and exclude or include it according to the current mode.

            -----------------------
            -- Read_Routine_Name --
            -----------------------

            procedure Read_Routine_Name (Disp_Routine_Arg : String) is
            begin
               if Disp_Routine_Arg = "--exclude" then
                  Mode_Exclude := True;
               elsif Disp_Routine_Arg = "--include" then
                  Mode_Exclude := False;
               else
                  Traces_Elf.Read_Routines_Name
                    (Disp_Routine_Arg,
                     Exclude   => Mode_Exclude,
                     Keep_Open => False);
               end if;
            end Read_Routine_Name;

         begin
            Check_Argument_Available (Obj_Inputs, "EXEC", Command);
            Inputs.Iterate (Obj_Inputs, Read_Routine_Name'Access);
            Traces_Names.Disp_All_Routines;
            return;
         end;

      when Cmd_Map_Routines =>
         Check_Argument_Available (SCOs_Inputs, "SCOs FILEs", Command);
         Inputs.Iterate (SCOs_Inputs, Load_SCOs'Access);
         Inputs.Iterate (Exe_Inputs, Build_Decision_Map'Access);
         return;

      when Cmd_Dump_Trace =>
         Check_Argument_Available (Trace_Inputs, "TRACEFILEs", Command);
         Inputs.Iterate (Trace_Inputs, Dump_Trace_File'Access);
         return;

      when Cmd_Dump_Trace_Base =>
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

      when Cmd_Dump_Trace_Asm =>
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

      when Cmd_Dump_Sections
        | Cmd_Dump_Symbols
        | Cmd_Dump_Subprograms
        | Cmd_Dump_Lines =>
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
                  when Cmd_Dump_Sections =>
                     To_Display := Section_Addresses;

                  when Cmd_Dump_Symbols =>
                     Build_Symbols (Exec'Unchecked_Access);
                     To_Display := Symbol_Addresses;

                  when Cmd_Dump_Subprograms =>
                     Build_Debug_Compile_Units (Exec);
                     To_Display := Subprogram_Addresses;

                  when Cmd_Dump_Lines =>
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

      when Cmd_Dump_Compile_Units =>
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

      when Cmd_Disassemble_Raw =>
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

      when Cmd_Disassemble =>
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

      when Cmd_Coverage =>

         case Get_Coverage_Level is
            when Source_Coverage_Level =>
               Check_Argument_Available
                 (SCOs_Inputs, "SCOs FILEs", Command);
               Inputs.Iterate (SCOs_Inputs, Load_SCOs'Access);
            when others =>
               null;
         end case;

         declare
            procedure Process_Trace (Trace_File_Name : String);
            --  Open Trace_File_Name and merge it into the trace database

            -------------------
            -- Process_Trace --
            -------------------

            procedure Process_Trace (Trace_File_Name : String) is
               Trace_File : constant Trace_File_Element_Acc :=
                              new Trace_File_Element;
            begin
               Init_Base (Base, Full_History => Get_Coverage_Level = MCDC);
               Trace_File.Filename := new String'(Trace_File_Name);
               Read_Trace_File (Trace_File.Filename.all,
                                Trace_File.Trace, Base);
               Traces_Files_List.Files.Append (Trace_File);

               declare
                  function Override_Exec_Name (S : String) return String;
                  --  If an exec file name as been passed from the command
                  --  line, it overrides the one in the trace file.

                  ------------------------
                  -- Override_Exec_Name --
                  ------------------------

                  function Override_Exec_Name (S : String) return String is
                  begin
                     if Opt_Exe_Name /= null then
                        return Opt_Exe_Name.all;
                     else
                        return S;
                     end if;
                  end Override_Exec_Name;

                  Exe_Name : constant String :=
                    Override_Exec_Name (Get_Info (Trace_File.Trace,
                                                  Qemu_Traces.Exec_File_Name));
                  Exe_File : Exe_File_Acc;

               --  Start of processing for Process_Trace

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

                  case Get_Coverage_Level is
                     when Object_Coverage_Level =>
                        --  If there is no routine in list, get
                        --  routine names from the first executable. A
                        --  test earlier allows this only if there is
                        --  one trace file.

                        if Inputs.Length (Routines_Inputs) = 0 then
                           Read_Routines_Name (Exe_File, Exclude => False);
                        end if;

                     when Source_Coverage_Level =>
                        --  If there is no routine in list, get
                        --  routine names from SCOs.

                        --  ??? Ultimately, routine names should
                        --  always been extracted from SCOs.  But, for
                        --  now, this method gives less precise
                        --  results than --routines. More precisely,
                        --  it may include more source files than
                        --  would actually be needed in xcov's
                        --  reports: with inlining, most source files
                        --  may be included in the reports as their
                        --  elaboration code may be inlined in the
                        --  "_ada_" procedure; this procedure glues
                        --  everything together. We need to filter out
                        --  source files that do not appear in the
                        --  SCOs to fix this issue.

                        if Inputs.Length (Routines_Inputs) = 0 then
                           Routine_Names_From_Lines (Exe_File, Has_SCO'Access);
                        end if;

                     when Unknown =>
                        --  A fatal error should have been diagnosed earlier

                        pragma Assert (False);
                        return;
                  end case;

                  Load_Code_And_Traces (Exe_File, Base'Access);

                  --  If performing source coverage, load sloc information

                  if Get_Coverage_Level in Source_Coverage_Level then
                     Build_Debug_Lines (Exe_File.all);
                  end if;
               end;
            end Process_Trace;

         begin
            Check_Argument_Available (Trace_Inputs, "TRACEFILEs", Command);

            if Get_Coverage_Level = Unknown then
               Fatal_Error ("Please specify a coverage level");
            end if;

            if Inputs.Length (Routines_Inputs) /= 0 then
               Inputs.Iterate (Routines_Inputs,
                               Traces_Names.Add_Routine_Name'Access);
            else
               case Get_Coverage_Level is
                  when Object_Coverage_Level =>
                     if Inputs.Length (Trace_Inputs) > 1 then
                        Fatal_Error ("routine list required"
                                     & " when reading multiple trace files");
                     end if;
                  when others =>
                     null;
               end case;
            end if;

            --  Read traces

            Inputs.Iterate (Trace_Inputs, Process_Trace'Access);

            --  Now determine coverage according to the requested metric

            case Get_Coverage_Level is
               when Object_Coverage_Level =>
                  if Inputs.Length (SCOs_Inputs) /= 0 then
                     Error ("List of SCOs not allowed for object coverage");
                  end if;

                  Traces_Elf.Build_Routines_Insn_State;

                  if Annotation /= Annotate_Asm then
                     Traces_Elf.Build_Source_Lines;
                  end if;

               when Source_Coverage_Level =>
                  if Get_Coverage_Level > Stmt then
                     --  Build decision map

                     Inputs.Iterate (Exe_Inputs, Build_Decision_Map'Access);
                  end if;

                  --  Process traces for each routine

                  Traces_Names.Iterate
                    (Source.Compute_Source_Coverage'Access);

                  --  Build source information

                  Traces_Elf.Build_Routines_Insn_State;
                  --  Is this still necessary???

                  Traces_Elf.Build_Source_Lines;

               when Unknown =>
                  --  A fatal error should have been diagnosed earlier

                  pragma Assert (False);
                  return;
            end case;

            case Annotation is
               when Annotate_Asm =>
                  if Get_Coverage_Level in Source_Coverage_Level then
                     Error ("Asm format not supported for source coverage.");
                     return;
                  end if;
                  Traces_Disa.Flag_Show_Asm := True;
                  Dump_Coverage_Option (Standard_Output);
                  Traces_Dump.Dump_Routines_Traces;

               when Annotate_Xcov =>
                  Annotations.Xcov.Generate_Report (False);

               when Annotate_Html =>
                  Annotations.Html.Generate_Report (False);

               when Annotate_Xml =>
                  Annotations.Xml.Generate_Report;

               when Annotate_Xcov_Asm =>
                  --  Case of source coverage???
                  Annotations.Xcov.Generate_Report (True);

               when Annotate_Html_Asm =>
                  --  Case of source coverage???
                  Annotations.Html.Generate_Report (True);

               when Annotate_Report =>
                  Dump_Coverage_Option (Annotations.Report.Get_Output);
                  Annotations.Report.Finalize_Report;

               when Annotate_Unknown =>
                  Fatal_Error ("Please specify an annotation format.");
            end case;
         end;

      when Cmd_Run =>
         declare
            procedure Run (Exe_File : String);
            --  Run Exe_File in QEMU

            ---------
            -- Run --
            ---------

            procedure Run (Exe_File : String) is
            begin
               if Get_Coverage_Level = MCDC then
                  if Inputs.Length (Routines_Inputs) > 0 then
                     Inputs.Iterate (Routines_Inputs,
                                     Traces_Names.Add_Routine_Name'Access);
                  else
                     Read_Routines_Name (Exe_File, Exclude => False,
                                         Keep_Open => False);
                  end if;

                  Inputs.Iterate (SCOs_Inputs, Load_SCOs'Access);
                  Build_Decision_Map (Exe_File);
               end if;

               Qemudrv.Driver (Exe_File, Target, Tag, Output, Eargs);
            end Run;
         begin
            Inputs.Iterate (Exe_Inputs, Run'Access);
         end;
   end case;

exception
   when Xcov_Exit_Exc =>
      --  An error message has already been displayed

      null;
end Xcov;
