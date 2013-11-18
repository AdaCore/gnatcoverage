------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Interfaces;

with Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.Strings;      use GNAT.Strings;

with ALI_Files;         use ALI_Files;
with Annotations;       use Annotations;
with Annotations.Dynamic_Html;
with Annotations.Html;
with Annotations.Xcov;
with Annotations.Xml;
with Annotations.Report;
with Check_SCOs;
with Commands;          use Commands;
with Convert;
with Coverage;          use Coverage;
with Coverage.Source;   use Coverage.Source;
with Coverage.Tags;     use Coverage.Tags;
with Decision_Map;      use Decision_Map;
with Elf_Files;
with Execs_Dbase;       use Execs_Dbase;
with Files_Table;       use Files_Table;
with Inputs;            use Inputs;
with Outputs;           use Outputs;
with Perf_Counters;
with Project;           use Project;
with Qemu_Traces;
with Rundrv;
with SC_Obligations;    use SC_Obligations;
with Slocs;             use Slocs;
with Strings;           use Strings;
with Switches;          use Switches;
with Traces;            use Traces;
with Traces_Elf;        use Traces_Elf;
with Traces_Files_List; use Traces_Files_List;
with Traces_Names;      use Traces_Names;
with Traces_Dump;
with Traces_Files;      use Traces_Files;
with Traces_Dbase;      use Traces_Dbase;
with Traces_Disa;
with Version;

procedure GNATcov is

   procedure Usage;
   --  Display usage information for documented commands

   procedure Usage_Dump;
   --  Display usage information for internal debugging commands

   procedure Show_Version;
   --  Show gnatcov version

   procedure Show_CWD;
   --  Show the current working directory

   procedure Check_Argument_Available
     (Args            : Inputs.Inputs_Type;
      What            : String;
      Command         : Command_Type := No_Command;
      Additional_Info : String := "");
   --  Report a fatal error if Args is empty

   procedure Load_All_SCOs (Check_SCOs : Boolean);
   --  Load all listed SCO files and initialize source coverage data structure.
   --  If Check_SCOs is True, report an error if no SCOs are provided.

   ------------------------------
   -- Check_Argument_Available --
   ------------------------------

   procedure Check_Argument_Available
     (Args            : Inputs.Inputs_Type;
      What            : String;
      Command         : Command_Type := No_Command;
      Additional_Info : String := "")
   is
   begin
      if Inputs.Length (Args) = 0 then
         Fatal_Error
           ("missing " & What & For_Command_Switch (Command)
           & Additional_Info);
      end if;
   end Check_Argument_Available;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      procedure P (S : String) renames Put_Line;
   begin
      P ("Usage: " & Ada.Command_Line.Command_Name & " ACTION [OPTIONS...]");
      P ("Action is one of:");
      P (" --help");
      P ("   Display this help");
      New_Line;
      P (" --help-dump");
      P ("   Display extra help for dump commands (useful for maintainance)");
      New_Line;
      P (" --version");
      P ("   Display version");
      New_Line;
      Rundrv.Help (" ");
      New_Line;
      P (" disp-routines {[--exclude|--include] FILES}");
      P ("   Build a list of routines from object files");
      P (" scan-objects {FILES}");
      P ("   Scan object FILES for empty symbols or orphan regions");
      New_Line;
      P (" convert --trace-source=SOURCE_ID --exec=EXECUTABLE");
      P ("  --input=INPUT_TRACE [OPTIONS]");
      P ("     SOURCE_ID specifies source of trace data (e.g. iSystem-5634)");
      P ("     EXECUTABLE is name of executable which generated data");
      P ("     INPUT_TRACE is file containing trace data to be converted");
      P ("  --output=OUTPUT_TRACE file to place converted trace into");
      P ("  --hw-trigger-traces=START_ID,START_ADDR,STOP_ID");
      P ("     identity of start and stop triggers, and address for start");
      P ("  --tag=TAG put TAG into trace file");
      P ("  --level=.... same as for 'run' command");
      P ("  --scos=...   same as for 'run' command");
      New_Line;
      P (" coverage OPTIONS TRACE_FILES");
      P ("   Generate coverage report");
      P ("   -c LEVEL --level=LEVEL     Specify coverage levels");
      P ("      LEVEL is one of " & Valid_Coverage_Options);
      P ("   -a FORM  --annotate=FORM    Generate a FORM report");
      P ("      FORM is one of asm,xcov,html,xcov+,html+,dhtml,report");
      P ("   --routines=<ROUTINE|@FILE>  Add ROUTINE, or all routine listed");
      P ("                               in FILE to the list of routines");
      P ("   --alis=<FILE|@LISTFILE>");
      P ("   --scos=<FILE|@LISTFILE>     Load SCOs and exemption info from");
      P ("                               FILE for this operation; or do that");
      P ("                               for each file listed in LISTFILE");
      P ("   -P=<GPR>                    Use GPR as root project to locate");
      P ("                               SCOs, select units to analyze and");
      P ("                               find default options.");
      P ("   --projects=<GPR|@LISTFILE>  Focus on specific projects within");
      P ("                               the transitive closure reachable");
      P ("                               from the root designated by -P.");
      P ("   --recursive                 In addition to those designated by");
      P ("                               -P/--projects, consider units from");
      P ("                               any transitively imported project.");
      P ("   --units=<UNIT|@LISTFILE>    State the set of units of interest");
      P ("                               by name, overriding the gpr based");
      P ("                               selection by -P etc.");
      P ("   -t TARGET --target=TARGET   When using projects files, state");
      P ("                               the target toolchain prefix used to");
      P ("                               build the analyzed pograms. This is");
      P ("                               required for correct project files");
      P ("                               processing with cross targets.");
      P ("   --subdirs=<SUBDIR>          When using project files, look for");
      P ("                               ALI files in the provided SUBDIR of");
      P ("                               the projects' build directory.");
      P ("   -o FILE --output=FILE       Put the report|asm output into FILE");
      P ("   -T|--trace <FILE|@LISTFILE> Add FILE or all the files listed in");
      P ("                               LISTFILE to the list of traces");
      P ("   -S <routine|instance>       Perform separate source coverage");
      P ("                               (EXPERIMENTAL)");
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
      P (" check-scos");
      P ("   Parse and load SCOs files to check them");
      New_Line;
      P (" dump-trace FILES");
      P ("   Display of trace files, slided if necessary");
      New_Line;
      P (" dump-trace-raw FILES");
      P ("   Raw display of trace files");
      New_Line;
      P (" dump-trace-base FILES");
      P ("   Display of merged trace files");
      New_Line;
      P (" dump-trace-asm EXE TRACE_FILES");
      P ("   Display of trace files with assembly code for each trace");
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
   ALIs_Option               : constant String := "--alis=";
   Units_Option              : constant String := "--units=";
   Final_Report_Option       : constant String := "--report=";
   Kernel_Option             : constant String := "--kernel=";
   Output_Dir_Option         : constant String := "--output-dir=";
   Root_Project_Option       : constant String := "-P";
   Projects_Option           : constant String := "--projects=";
   Subdirs_Option            : constant String := "--subdirs=";
   Scenario_Var_Option       : constant String := "-X";
   Recursive_Option          : constant String := "--recursive";
   Trace_Option              : constant String := "--trace=";
   Trace_Option_Short        : constant String := "-T";
   Trace_Source_Option       : constant String := "--trace-source=";
   Target_Option             : constant String := "--target=";
   Target_Option_Short       : constant String := "-t";
   HW_Trigger_Traces_Option  : constant String := "--hw-trigger-traces=";
   Input_Option              : constant String := "--input=";
   Output_Option             : constant String := "--output=";
   Output_Option_Short       : constant String := "-o";
   Separate_Option_Short     : constant String := "-S";
   Tag_Option                : constant String := "--tag=";
   Verbose_Option            : constant String := "--verbose";
   Verbose_Option_Short      : constant String := "-v";
   Eargs_Option              : constant String := "-eargs";
   Stats_Option              : constant String := "--stats";

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
   Annotation          : Annotation_Format renames Annotations.Annotation;
   Trace_Inputs        : Inputs.Inputs_Type;
   Exe_Inputs          : Inputs.Inputs_Type;
   Obj_Inputs          : Inputs.Inputs_Type;
   ALIs_Inputs         : Inputs.Inputs_Type;
   Routines_Inputs     : Inputs.Inputs_Type;
   Units_Inputs        : Inputs.Inputs_Type;
   Projects_Inputs     : Inputs.Inputs_Type;
   Text_Start          : Pc_Type := 0;
   Target              : String_Access := null;
   Output              : String_Access := null;
   Tag                 : String_Access := null;
   Kernel              : String_Access := null;
   Eargs               : String_List_Access := null;
   Root_Project        : String_Access := null;

   Opt_Exe_Name : String_Access := null;
   --  Path to executable from the command line; it overrides the default one
   --  from trace files.

   procedure Parse_Command_Line;
   --  Parse the command line and set the above local variables

   procedure Set_Subjects_From_Project;
   --  Load the project file and set defaults for options identifying the
   --  entities of interest coverage analysis if they have not been identified
   --  on the command line.

   -------------------
   -- Load_All_SCOs --
   -------------------

   procedure Load_All_SCOs (Check_SCOs : Boolean) is
   begin
      if Check_SCOs then
         Check_Argument_Available
           (ALIs_Inputs,
            "SCOs",
            Command,
            " (specify Units in project or use --units/--scos)");
      end if;
      Inputs.Iterate (ALIs_Inputs, Load_SCOs'Access);
      Coverage.Source.Initialize_SCI;
   end Load_All_SCOs;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      type Pass_Type is (Command_Line_1, Root_Prj, Command_Line_2);

      procedure Process_Switches
        (S     : Switches_Source'Class;
         First : Natural;
         Pass  : Pass_Type);
      --  Process switches from S starting at index First. Pass Command_Line_1
      --  is the initial scan for the root project, pass Root_Prj is for
      --  default switches from the root project, and pass Command_Line_2 is
      --  for the normal processing of the remainder of the command line.

      ----------------------
      -- Process_Switches --
      ----------------------

      procedure Process_Switches
        (S     : Switches_Source'Class;
         First : Natural;
         Pass  : Pass_Type)
      is
         Arg_Count : constant Natural := S.Argument_Count;
         Arg_Index : Natural := First;

         procedure Check_Annotation_Format (Annotation : Annotation_Format);
         --  Warn if Annotation is unknown or deprecated

         procedure Check_Argument_Available
           (What    : String;
            Command : Command_Type := No_Command);
         --  Check that Arg_Index is not greater than Arg_Count. If not,
         --  display an error message and raise Fatal_Error.

         function Next_Arg (What : String) return String;
         --  Increment Arg_Index and return Argument (Arg_Index). If end of
         --  command line is reached, display an error message, and raise
         --  Constraint_Error.

         function Option_Parameter (S : String) return String;
         --  Assuming that S is of the form "<part1>=<part2>",
         --  return "<part2>".

         function Parse_Hex (S : String; Flag_Name : String) return Pc_Type;
         --  Parse S to get an hexadecimal number (form : 0x[0-9a-f]+) and
         --  return the value. If the parsing fails, fatal error.

         function Rest_Of_Command_Line return String_List_Access;
         --  Return the rest of the command line in a string list

         procedure Set_Root_Project (Prj_Name : String);
         --  Use the named project file as root project. No-op during
         --  Command_Line_2, fatal error if encountered during Root_Prj.

         -----------------------------
         -- Check_Annotation_Format --
         -----------------------------

         procedure Check_Annotation_Format (Annotation : Annotation_Format) is
         begin
            if Annotation = Annotate_Unknown then
               Fatal_Error ("bad parameter for " & Annotate_Option_Short);
            end if;
         end Check_Annotation_Format;

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
            return S.Argument (Arg_Index);
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

         ----------------------
         -- Set_Root_Project --
         ----------------------

         procedure Set_Root_Project (Prj_Name : String) is
         begin
            if Root_Project /= null then
               Fatal_Error ("only one root project may be specified");
            end if;
            Root_Project := new String'(Prj_Name);
            Load_Root_Project (Prj_Name, Target);
         end Set_Root_Project;

      --  Start of processing for Process_Switches

      begin
         while Arg_Index <= Arg_Count loop
            Process_Switch : declare
               Arg : String renames S.Argument (Arg_Index);

               function Common_Switch return Boolean;
               --  Processing for switch valid in both command line and
               --  project, and that need to be processed in all passes.
               --  Return True if switch processed.

               -------------------
               -- Common_Switch --
               -------------------

               function Common_Switch return Boolean is
               begin
                  --  Debugging options

                  if Has_Prefix (Arg, "-d") then
                     declare
                        Pos : Positive := Arg'First + 2;
                     begin
                        if Pos > Arg'Last then
                           Fatal_Error ("parameter required for -d");
                        end if;

                        while Pos <= Arg'Last loop
                           case Arg (Pos) is
                           when 'b' =>
                              Switches.Debug_Break_Long_Instructions := True;
                           when 'h' =>
                              Switches.Debug_Full_History       := True;
                           when 'i' =>
                              Switches.Debug_Ignore_Exemptions  := True;
                           when others =>
                              Fatal_Error ("bad parameter -d" & Arg (Pos));
                           end case;
                           Pos := Pos + 1;
                        end loop;
                     end;

                  elsif Arg = Verbose_Option
                          or else
                        Arg = Verbose_Option_Short
                  then
                     Verbose := True;

                  else
                     return False;
                  end if;

                  return True;
               end Common_Switch;

            --  Start of processing for Process_Switch

            begin
               case Pass is
                  when Command_Line_1 =>
                     --  A subset of switches are processed before projects
                     --  are loaded.

                     if Arg = Root_Project_Option then
                        Set_Root_Project (Next_Arg ("root project"));

                     elsif Has_Prefix (Arg, Root_Project_Option) then
                        Set_Root_Project
                          (Arg (Arg'First + Root_Project_Option'Length ..
                                Arg'Last));

                     elsif Has_Prefix (Arg, Scenario_Var_Option) then

                        --  Get name and value from "-X<name>=<value>"

                        declare
                           Name_First, Name_Last, Value_First : Positive;
                        begin
                           Name_First := Arg'First + 2;
                           Name_Last := Name_First - 1;
                           while Name_Last < Arg'Last
                             and then Arg (Name_Last + 1) /= '='
                           loop
                              Name_Last := Name_Last + 1;
                           end loop;

                           Value_First := Name_Last + 2;

                           Add_Scenario_Var
                             (Key   => Arg (Name_First .. Name_Last),
                              Value => Arg (Value_First .. Arg'Last));
                        end;

                     elsif Has_Prefix (Arg, Subdirs_Option) then
                        Set_Subdirs (Option_Parameter (Arg));

                     elsif Arg = Target_Option_Short then
                        Target := new String'(Next_Arg ("target"));

                     elsif Has_Prefix (Arg, Target_Option) then
                        Target := new String'(Option_Parameter (Arg));

                     elsif Common_Switch then
                        null;
                     end if;

                  when Command_Line_2 | Root_Prj =>
                     if Arg = Root_Project_Option
                       or else Arg = Target_Option_Short
                       or else Has_Prefix (Arg, Root_Project_Option)
                       or else Has_Prefix (Arg, Scenario_Var_Option)
                       or else Has_Prefix (Arg, Subdirs_Option)
                       or else Has_Prefix (Arg, Target_Option)
                     then
                        --  Ignore in command line pass 2, reject in project

                        if Pass = Root_Prj then
                           Fatal_Error
                             (Arg & " may not be specified in a project");
                        end if;

                        --  Ignore next argument as well if appropriate

                        if Arg = Root_Project_Option
                          or else Arg = Target_Option_Short
                        then
                           Arg_Index := Arg_Index + 1;
                        end if;

                     elsif Arg = Eargs_Option then
                        Check_Option (Arg, Command, (1 => Cmd_Run));

                        --  If we don't yet have an Executable specified,
                        --  pick the first earg.

                        if Inputs.Length (Exe_Inputs) = 0 then
                           Inputs.Add_Input (Exe_Inputs, Next_Arg ("eargs"));
                        end if;

                        Eargs := Rest_Of_Command_Line;
                        return;

                     elsif Arg = Output_Option_Short then
                        Check_Option (Arg, Command, (1 => Cmd_Run,
                                                     2 => Cmd_Coverage));
                        Output := new String'(Next_Arg ("output"));

                     elsif Has_Prefix (Arg, Output_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Run,
                                                     2 => Cmd_Coverage,
                                                     3 => Cmd_Convert));
                        Output := new String'(Option_Parameter (Arg));

                     elsif Has_Prefix (Arg, Projects_Option) then
                        Inputs.Add_Input
                          (Projects_Inputs, Option_Parameter (Arg));

                     elsif Arg = Recursive_Option then
                        Switches.Recursive_Projects := True;

                     elsif Has_Prefix (Arg, Tag_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Run,
                                                     2 => Cmd_Convert));
                        Tag := new String'(Option_Parameter (Arg));

                     elsif Has_Prefix (Arg, Kernel_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Run));
                        Kernel := new String'(Option_Parameter (Arg));

                     elsif Arg = Coverage_Option_Short then
                        Check_Option (Arg, Command, (1 => Cmd_Coverage,
                                                     2 => Cmd_Run));
                        declare
                           Coverage_Level : constant String := Next_Arg
                             ("coverage level");
                        begin
                           Set_Coverage_Levels (Coverage_Level);
                        exception
                           when Constraint_Error =>
                              Fatal_Error
                                ("bad parameter "
                                 & Coverage_Option_Short & " "
                                 & Coverage_Level);
                        end;

                     elsif Has_Prefix (Arg, Coverage_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Coverage,
                                                     2 => Cmd_Run,
                                                     3 => Cmd_Convert));
                        begin
                           Set_Coverage_Levels (Option_Parameter (Arg));
                        exception
                           when Constraint_Error =>
                              Fatal_Error
                                ("bad parameter " & Arg);
                        end;

                     elsif Has_Prefix (Arg, SCOs_Option)
                       or else Has_Prefix (Arg, ALIs_Option)
                     then
                        Check_Option (Arg, Command, (1 => Cmd_Map_Routines,
                                                     2 => Cmd_Coverage,
                                                     3 => Cmd_Run,
                                                     4 => Cmd_Convert));
                        Inputs.Add_Input
                          (ALIs_Inputs, Option_Parameter (Arg));

                     elsif Has_Prefix (Arg, Units_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Map_Routines,
                                                     2 => Cmd_Coverage,
                                                     3 => Cmd_Run));
                        Inputs.Add_Input
                          (Units_Inputs, Option_Parameter (Arg));

                     elsif Has_Prefix (Arg, Routines_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Map_Routines,
                                                     2 => Cmd_Coverage,
                                                     3 => Cmd_Run));
                        Inputs.Add_Input
                          (Routines_Inputs, Option_Parameter (Arg));

                     elsif Arg = Deprecated_Routine_List_Option_Short then
                        Check_Option (Arg, Command, (1 => Cmd_Coverage,
                                                     2 => Cmd_Run));
                        Inputs.Add_Input (Routines_Inputs,
                                          "@" & Next_Arg ("function list"));

                     elsif Has_Prefix
                       (Arg, Deprecated_Routine_List_Option)
                     then
                        Check_Option (Arg, Command, (1 => Cmd_Coverage,
                                                     2 => Cmd_Run));
                        Inputs.Add_Input (Routines_Inputs,
                                          "@" & Option_Parameter (Arg));

                     elsif Has_Prefix (Arg, Exec_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Coverage,
                                                     2 => Cmd_Convert));
                        Opt_Exe_Name := new String'(Option_Parameter (Arg));

                     elsif Arg = "--all-decisions" then
                        Switches.All_Decisions := True;

                     elsif Arg = "--all-messages" then
                        Switches.All_Messages := True;

                     elsif Arg = "--missing-files" then
                        Check_Option (Arg, Command, (1 => Cmd_Coverage));
                        Flag_Show_Missing := True;

                     elsif Has_Prefix (Arg, "--text-start=") then
                        --  FIXME: not yet supported???
                        --  Should be a global option (used when building
                        --  decision map for --run)???

                        begin
                           Text_Start :=
                             Parse_Hex
                               (Arg (Arg'First + 13 .. Arg'Last),
                                "--text-start");
                        exception
                           when Constraint_Error =>
                              Fatal_Error ("Failure to parse --text-start");
                        end;

                     elsif Has_Prefix (Arg, "--source-rebase=") then
                        Check_Option (Arg, Command, (1 => Cmd_Coverage));
                        declare
                           Pos : Natural := 0;
                        begin
                           --  Parse source-rebase's argument. This option's
                           --  form should be
                           --  "--source-rebase=<OLD_PREFIX>=<NEW_PREFIX>".

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

                     elsif Has_Prefix (Arg, "--source-search=") then
                        Check_Option (Arg, Command, (1 => Cmd_Coverage));
                        Add_Source_Search (Arg (Arg'First + 16 .. Arg'Last));

                     elsif Arg = Annotate_Option_Short then
                        Check_Option (Arg, Command, (1 => Cmd_Coverage));
                        Annotation :=
                          To_Annotation_Format
                            (Next_Arg ("annotation format"));
                        Check_Annotation_Format (Annotation);

                     elsif Has_Prefix (Arg, Annotate_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Coverage));
                        Annotation :=
                          To_Annotation_Format (Option_Parameter (Arg));
                        Check_Annotation_Format (Annotation);

                     elsif Has_Prefix (Arg, Final_Report_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Coverage));
                        Output := new String'(Option_Parameter (Arg));

                     elsif Has_Prefix (Arg, Output_Dir_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Coverage));
                        Outputs.Set_Output_Dir (Option_Parameter (Arg));

                     elsif Arg = Trace_Option_Short then
                        Check_Option (Arg, Command, (Cmd_Coverage,
                          Cmd_Dump_Trace,
                          Cmd_Dump_Trace_Raw,
                          Cmd_Dump_Trace_Base,
                          Cmd_Dump_Trace_Asm,
                          Cmd_Run));

                        --  Tag_Option_Short conflicts with Trace_Option_Short

                        if Command = Cmd_Run then
                           Tag := new String'(Next_Arg (Arg));
                        else
                           Inputs.Add_Input
                             (Trace_Inputs, Next_Arg ("trace file"));
                        end if;

                     elsif Has_Prefix (Arg, Trace_Option) then
                        Check_Option (Arg, Command, (Cmd_Coverage,
                          Cmd_Dump_Trace,
                          Cmd_Dump_Trace_Raw,
                          Cmd_Dump_Trace_Base,
                          Cmd_Dump_Trace_Asm));
                        Inputs.Add_Input
                          (Trace_Inputs, Option_Parameter (Arg));

                     elsif Has_Prefix (Arg, Trace_Source_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Convert));
                        Convert.Set_Trace_Source (Option_Parameter (Arg));

                     elsif Has_Prefix (Arg, HW_Trigger_Traces_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Convert));
                        Convert.HW_Trigger_Arg :=
                          new String'(Option_Parameter (Arg));

                     elsif Has_Prefix (Arg, Input_Option) then
                        Check_Option (Arg, Command, (1 => Cmd_Convert));
                        Convert.Input_Arg :=
                          new String'(Option_Parameter (Arg));

                     elsif Arg = "--exclude" then
                        Inputs.Add_Input (Obj_Inputs, Arg);

                     elsif Arg = "--include" then
                        Inputs.Add_Input (Obj_Inputs, Arg);

                     elsif Arg = Separate_Option_Short then
                        begin
                           Check_Option (Arg, Command, (1 => Cmd_Coverage));
                           Tag_Provider :=
                             Tag_Providers.Create
                               (Next_Arg ("separate coverage scope"));
                        exception
                           when Constraint_Error =>
                              Fatal_Error
                                ("bad parameter for " & Separate_Option_Short);
                        end;

                     elsif Arg = Stats_Option then
                        Check_Option (Arg, Command, (1 => Cmd_Map_Routines));
                        Branch_Stats := True;

                     elsif Common_Switch then
                        null;

                     elsif Arg (1) = '-' then
                        Fatal_Error ("unknown option: " & Arg);

                     else
                        --  Handling of parameters that are not options (i.e.
                        --  file list).

                        case Command is
                        when No_Command =>
                           Fatal_Error ("No command specified");

                        when Cmd_Help
                           | Cmd_Help_Dump =>
                           Fatal_Error ("no parameter allowed");

                        when Cmd_Version =>
                           null;

                        when Cmd_Coverage
                           | Cmd_Dump_Trace
                           | Cmd_Dump_Trace_Raw
                           | Cmd_Dump_Trace_Base =>
                           Inputs.Add_Input (Trace_Inputs, Arg);

                        when Cmd_Disp_Routines
                           | Cmd_Scan_Objects =>
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
                           --  Set MC/DC coverage level in order to generate
                           --  a complete decision map.

                           Set_Coverage_Levels ("stmt+mcdc");
                           Inputs.Add_Input (Exe_Inputs, Arg);

                        when Cmd_Run =>
                           if Inputs.Length (Exe_Inputs) > 1 then
                              Fatal_Error
                                ("Only one EXEC parameter is allowed with "
                                 & To_Switch (Command));
                           end if;
                           Inputs.Add_Input (Exe_Inputs, Arg);

                        when Cmd_Check_SCOs =>
                           Inputs.Add_Input (ALIs_Inputs, Arg);

                        when Cmd_Dump_Trace_Asm =>
                           if Inputs.Length (Exe_Inputs) < 1 then
                              Inputs.Add_Input (Exe_Inputs, Arg);
                           else
                              Inputs.Add_Input (Trace_Inputs, Arg);
                           end if;

                        when Cmd_Convert =>
                           null;

                        end case;
                     end if;
               end case;
            end Process_Switch;

            Arg_Index := Arg_Index + 1;
         end loop;
      end Process_Switches;

      Command_Line : Command_Line_Switches_Source;

   --  Start of processing for Parse_Command_Line

   begin
      --  Require at least one argument

      if Command_Line.Argument_Count = 0 then
         Usage;
         Normal_Exit;
      end if;

      --  Decode command

      Command := To_Command (Command_Line.Argument (1));
      if Command = No_Command then
         Fatal_Error ("bad command " & Command_Line.Argument (1)
                      & ".  Try option --help");
      end if;

      --  First command line scan: set root project and scenario variables

      Process_Switches (Command_Line, 2, Command_Line_1);

      --  Scan default switches from root project

      if Root_Project /= null then
         Compute_Project_View;

         Switches_From_Project : declare
            procedure Get_Switches_From_Project (Index : String);
            --  Get switches from project with indicated index

            -------------------------------
            -- Get_Switches_From_Project --
            -------------------------------

            procedure Get_Switches_From_Project (Index : String) is
               Project_Switches : String_List_Access :=
                                    Project.Switches (Index);
            begin
               if Project_Switches /= null then
                  declare
                     Project_Src : String_List_Switches_Source
                       (Project_Switches.all'Access);
                  begin
                     Process_Switches (Project_Src, 1, Root_Prj);
                     Free (Project_Switches);
                  end;
               end if;
            end Get_Switches_From_Project;

         --  Start of processing for Switches_From_Project

         begin
            --  First get common switches...

            Get_Switches_From_Project ("*");

            --  ... then get command-specific switches

            Get_Switches_From_Project (To_Switch (Command));
         end Switches_From_Project;

         --  Set default output dir from project if not defined from project
         --  switches (can be overridden later on in pass Command_Line_2).

         if not Outputs.Output_Dir_Defined then
            Outputs.Set_Output_Dir (Project.Output_Dir);
         end if;
      end if;

      --  Second command line scan: process remainder of options

      Process_Switches (Command_Line, 2, Command_Line_2);
   end Parse_Command_Line;

   -------------------------------
   -- Set_Subjects_From_Project --
   -------------------------------

   procedure Set_Subjects_From_Project is

      generic
         Input_List : in out Inputs_Type;
      procedure Add_Item (S : String);
      --  Add S to Input_List

      --------------
      -- Add_Item --
      --------------

      procedure Add_Item (S : String) is
      begin
         Inputs.Add_Input (Input_List, S);
      end Add_Item;

      procedure Add_LI is new Add_Item (ALIs_Inputs);

   --  Start of processing for Set_Subjects_From_Project

   begin
      if Object_Coverage_Enabled then

         --  Set routines from project, not supported yet???

         null;

      elsif Inputs.Length (ALIs_Inputs) = 0 then
         Enumerate_LIs (Add_LI'Access, Override_Units => Units_Inputs);
      end if;
   end Set_Subjects_From_Project;

   ------------------
   -- Show_Version --
   ------------------

   procedure Show_Version is
   begin
      Put_Line ("GNATcoverage " & Standard.Version.Xcov_Version);
   end Show_Version;

   --------------
   -- Show_CWD --
   --------------

   procedure Show_CWD is
   begin
      Put_Line ("CWD = " & GNAT.OS_Lib.Normalize_Pathname ("."));
   end Show_CWD;

   Base : aliased Traces_Base;
   Exec : aliased Exe_File_Type;

   use type Tag_Provider_Access;

--  Start of processing for Xcov

begin
   --  Process command line

   Parse_Command_Line;

   if Verbose then
      Show_Version;
      Show_CWD;
   end if;

   if Root_Project /= null then
      --  If a root project has been specified but no project is being
      --  considered for coverage analysis, consider the root by default.

      if Length (Projects_Inputs) = 0 then
         Inputs.Add_Input (Projects_Inputs, Root_Project.all);
      end if;

      Inputs.Iterate (Projects_Inputs, Project.Add_Project'Access);
      Set_Subjects_From_Project;

   elsif Length (Projects_Inputs) /= 0 then
      Fatal_Error (Projects_Option & " requires " & Root_Project_Option);
   end if;

   --  Set defaults for options not specified so far

   if Tag_Provider = null then
      Tag_Provider := Tag_Providers.Create (Default_Tag_Provider_Name);
   end if;

   --  Now execute the specified command

   case Command is
      when No_Command | Cmd_Help =>
         Usage;

      when Cmd_Help_Dump =>
         Usage_Dump;

      when Cmd_Version =>
         if not Verbose then
            Show_Version;
         end if;

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
                  Traces_Elf.Read_Routine_Names
                    (Disp_Routine_Arg,
                     Exclude => Mode_Exclude,
                     Strict  => False);
               end if;
            end Read_Routine_Name;

         begin
            Check_Argument_Available (Obj_Inputs, "EXEC", Command);
            Inputs.Iterate (Obj_Inputs, Read_Routine_Name'Access);
            Traces_Names.Disp_All_Routines_Of_Interest;
         end;

      when Cmd_Scan_Objects =>
         declare

            procedure Scan_One_Elf (Elf_Name : String);
            --  Process to Scan_Symbols_From ELF_NAME in strict mode, warning
            --  about text section points of note wrt object coverage (empty
            --  symbols, orphan regions, ...)

            ------------------
            -- Scan_One_Elf --
            ------------------

            procedure Scan_One_Elf (Elf_Name : String) is
            begin
               Traces_Elf.Scan_Symbols_From
                 (Elf_Name,
                  Sym_Cb => null,
                  Strict => True);
            end Scan_One_Elf;

         begin
            Check_Argument_Available (Obj_Inputs, "EXEC", Command);
            Inputs.Iterate (Obj_Inputs, Scan_One_Elf'Access);
         end;

      when Cmd_Map_Routines =>
         declare
            procedure Build_Decision_Map (Exec_Name : String);
            --  Prepare decision map build for Exec_Name

            ------------------------
            -- Build_Decision_Map --
            ------------------------

            procedure Build_Decision_Map (Exec_Name : String) is
            begin
               --  Just set the filename

               Build_Decision_Map (Exec_Name, Text_Start, Exec_Name & ".dmap");
            end Build_Decision_Map;

         begin
            Load_All_SCOs (Check_SCOs => True);
            Inputs.Iterate (Exe_Inputs, Build_Decision_Map'Access);
            if Verbose then
               SC_Obligations.Report_SCOs_Without_Code;
            end if;
         end;

      when Cmd_Check_SCOs =>
         Inputs.Iterate (ALIs_Inputs, Check_SCOs.Check_SCO_Syntax'Access);
         Load_All_SCOs (Check_SCOs => True);

      when Cmd_Dump_Trace =>
         Check_Argument_Available (Trace_Inputs, "TRACEFILEs", Command);
         Inputs.Iterate (Trace_Inputs, Dump_Trace_File'Access);

      when Cmd_Dump_Trace_Raw =>
         Check_Argument_Available (Trace_Inputs, "TRACEFILEs", Command);
         Inputs.Iterate (Trace_Inputs, Dump_Raw_Trace_File'Access);

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
               To_Display : Address_Info_Kind;
            begin
               Open_File (Exec, Exec_File_Name, Text_Start);
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

                     raise Program_Error;
               end case;

               Disp_Addresses (Exec, To_Display);
               Close_File (Exec);
            end Dump_Exec;

         begin
            Check_Argument_Available (Exe_Inputs, "EXECs", Command);
            Inputs.Iterate (Exe_Inputs, Dump_Exec'Access);
         end;

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

      when Cmd_Coverage =>

         --  Validate combination of output format and coverage level

         if Annotation = Annotate_Report and then Object_Coverage_Enabled then
            Fatal_Error
              ("Report output is supported for source coverage only");
         end if;

         --  Validate availability of the output format

         if Annotation = Annotate_Dynamic_Html and then
            not Annotations.Dynamic_Html.Installed
         then
            Fatal_Error
              ("Dynamic HTML report format support is not installed");
         end if;

         --  Load ALI files

         if Source_Coverage_Enabled then
            Load_All_SCOs (Check_SCOs => True);

         elsif Object_Coverage_Enabled then
            Inputs.Iterate (ALIs_Inputs, Load_ALI'Access);

         else
            Fatal_Error ("Please specify a coverage level");
         end if;

         --  Load routines from command line

         if Object_Coverage_Enabled then

            if Inputs.Length (Routines_Inputs) /= 0 then
               Inputs.Iterate (Routines_Inputs,
                               Traces_Names.Add_Routine_Of_Interest'Access);
            elsif Inputs.Length (Trace_Inputs) > 1 then
               Fatal_Error ("routine list required"
                            & " when reading multiple trace files");
            end if;

            --  If no routines were given on the command line, we'll add them
            --  when processing the list of symbols from the only executable
            --  file (using Read_Routines_Names, see below).

         else
            if Inputs.Length (Routines_Inputs) /= 0 then
               Fatal_Error ("routine list not allowed"
                            & " for source coverage");
            end if;
         end if;

         --  Read and process traces

         declare
            procedure Process_Trace (Trace_File_Name : String);
            --  Common dispatching point for object and source coverage

            procedure Process_Trace_For_Obj_Coverage
              (Trace_File : Trace_File_Element_Acc);
            --  Open Trace_File and merge it into the trace database

            procedure Process_Trace_For_Src_Coverage
              (Trace_File : Trace_File_Element_Acc);
            --  Process Trace_File for source coverage. No trace database is
            --  used.

            function Open_Exec
              (Trace_File_Name : String;
               Trace_File      : Trace_File_Type) return Exe_File_Acc;
            --  Open the executable for TF, taking into account a possible
            --  command line override of the executable file name. The opened
            --  exec file is entered in the global execs list.

            ---------------
            -- Open_Exec --
            ---------------

            function Open_Exec
              (Trace_File_Name : String;
               Trace_File      : Trace_File_Type) return Exe_File_Acc
            is
               use Qemu_Traces;
               Exe_Name : String_Access;
            begin
               if Opt_Exe_Name /= null then
                  Exe_Name := Opt_Exe_Name;
               else
                  Exe_Name :=
                    new String'(Get_Info (Trace_File, Exec_File_Name));
                  if Exe_Name.all = "" then
                     Fatal_Error ("cannot find exec filename in trace file "
                                  & Trace_File_Name);
                  end if;
               end if;
               return Exe_File : Exe_File_Acc do
                  Open_Exec (Exe_Name.all, Text_Start, Exe_File);
                  declare
                     Mismatch_Reason : constant String :=
                        Match_Trace_Executable (Exe_File.all, Trace_File);

                  begin
                     if Mismatch_Reason /= "" then
                        Warn
                          ("ELF file " & Exe_Name.all
                           & " does not seem to match trace file "
                           & Trace_File_Name & ": " & Mismatch_Reason);
                     end if;
                  end;
               end return;
            exception
               when E : Elf_Files.Error =>
                  Fatal_Error ("cannot open ELF file " & Exe_Name.all
                               & " for trace file " & Trace_File_Name & ": "
                               & Ada.Exceptions.Exception_Message (E));
                  raise;
            end Open_Exec;

            -------------------
            -- Process_Trace --
            -------------------

            procedure Process_Trace (Trace_File_Name : String) is
               Trace_File : constant Trace_File_Element_Acc :=
                              new Trace_File_Element'
                                (Filename => new String'(Trace_File_Name),
                                 others   => <>);
            begin
               Traces_Files_List.Files.Append (Trace_File);
               if Object_Coverage_Enabled then
                  Process_Trace_For_Obj_Coverage (Trace_File);
               else
                  Process_Trace_For_Src_Coverage (Trace_File);
               end if;
            end Process_Trace;

            ------------------------------------
            -- Process_Trace_For_Obj_Coverage --
            ------------------------------------

            procedure Process_Trace_For_Obj_Coverage
              (Trace_File : Trace_File_Element_Acc)
            is
               Exe_File : Exe_File_Acc;
            begin
               Init_Base (Base);
               Read_Trace_File
                 (Trace_File.Filename.all, Trace_File.Trace, Base);

               Exe_File :=
                 Open_Exec (Trace_File.Filename.all, Trace_File.Trace);

               --  If there is no routine in list, get routine names from the
               --  first executable. A test earlier allows this only if there
               --  is one trace file.

               if Inputs.Length (Routines_Inputs) = 0 then
                  Read_Routine_Names (Exe_File, Exclude => False);
               end if;

               Build_Debug_Compile_Units (Exe_File.all);

               if Verbose then
                  Put_Line
                    ("processing traces from " & Trace_File.Filename.all);
               end if;

               Load_Code_And_Traces (Exe_File, Base'Access);
            end Process_Trace_For_Obj_Coverage;

            ------------------------------------
            -- Process_Trace_For_Src_Coverage --
            ------------------------------------

            procedure Process_Trace_For_Src_Coverage
              (Trace_File : Trace_File_Element_Acc)
            is
               use Interfaces;

               Exe_File                : Exe_File_Acc;
               Current_Sym             : Address_Info_Acc;
               Current_Subp_Key        : Subprogram_Key;
               Current_Subp_Info       : aliased Subprogram_Info;
               Current_Subp_Info_Valid : Boolean;
               E                       : Trace_Entry;
               Desc                    : Trace_File_Descriptor;
               Eof                     : Boolean;
               Offset                  : Pc_Type := 0;

            --  Start of processing for Process_Trace_For_Src_Coverage

            begin
               Open_Trace_File (Trace_File.Filename.all,
                                Desc, Trace_File.Trace);

               Exe_File := Open_Exec (Trace_File.Filename.all,
                                      Trace_File.Trace);

               --  Load symbols from executable (sets the rebase offset for
               --  each symbol) and perform static analysis.

               Decision_Map.Analyze (Exe_File);

               --  Read the load address
               Read_Loadaddr_Trace_Entry (Desc, Trace_File.Trace, Offset);

               --  Iterate on trace entries

               if Verbose then
                  Put_Line
                    ("processing traces from " & Trace_File.Filename.all);
               end if;

               loop
                  Read_Trace_Entry (Desc, Eof, E);
                  exit when Eof;

                  if E.Op = Qemu_Traces.Trace_Op_Special then
                     Fatal_Error
                       ("Unexpected 'loadaddr' special trace entry");
                  end if;

                  --  Skip everything until the first trace entry after
                  --  "Offset", and remove "Offset" from the bounds of the
                  --  remainder.

                  if Offset /= 0 then
                     if E.First < Offset then
                        goto Skip;
                     else
                        E.First := E.First - Offset;
                        E.Last := E.Last - Offset;
                     end if;
                  end if;

                  --  Get the symbol the trace entry is in

                  if Current_Sym = null
                    or else
                    E.First not in Current_Sym.First .. Current_Sym.Last
                  then
                     Current_Sym :=
                       Get_Address_Info
                         (Exe_File.all, Symbol_Addresses, E.First);

                     if Current_Sym = null then
                        Current_Subp_Info_Valid := False;
                     else
                        Key_From_Symbol
                          (Exe_File, Current_Sym, Current_Subp_Key);
                        Current_Subp_Info_Valid :=
                           Is_In (Current_Subp_Key);
                     end if;

                     if Current_Subp_Info_Valid then
                        Current_Subp_Info :=
                          Get_Subp_Info (Current_Subp_Key);
                     end if;
                  end if;

                  if Current_Subp_Info_Valid then
                     Compute_Source_Coverage
                       (Current_Subp_Key, Current_Subp_Info, E);
                  end if;

                  << Skip >> null;
               end loop;

               Close_Trace_File (Desc);
            end Process_Trace_For_Src_Coverage;

         begin
            Check_Argument_Available (Trace_Inputs, "TRACEFILEs", Command);
            Inputs.Iterate (Trace_Inputs, Process_Trace'Access);
         end;

         --  Now determine coverage according to the requested metric (for
         --  source coverage, complete coverage information has been determined
         --  when loading traces above).

         if Object_Coverage_Enabled then
            Traces_Elf.Build_Routines_Insn_State;

            if Annotation /= Annotate_Asm then
               Traces_Elf.Build_Source_Lines;
            end if;
         end if;

         --  Generate annotated reports

         case Annotation is
            when Annotate_Asm =>
               if Source_Coverage_Enabled then
                  Fatal_Error
                    ("Asm output supported for object coverage only");
               end if;
               Traces_Disa.Flag_Show_Asm := True;
               Traces_Dump.Dump_Routines_Traces (Output);

            when Annotate_Xml =>
               Annotations.Xml.Generate_Report;

            when Annotate_Xcov      |
                 Annotate_Xcov_Plus =>
               Annotations.Xcov.Generate_Report
                 (Show_Details => Annotation = Annotate_Xcov_Plus);

            when Annotate_Html      |
                 Annotate_Html_Plus =>
               Annotations.Html.Generate_Report
                 (Show_Details => Annotation = Annotate_Html_Plus);

            when Annotate_Dynamic_Html =>
               Annotations.Dynamic_Html.Generate_Report;

            when Annotate_Report =>
               Annotations.Report.Generate_Report (Output);

            when Annotate_Unknown =>
               Fatal_Error ("Please specify an annotation format.");
         end case;

      when Cmd_Run =>
         Check_Argument_Available (Exe_Inputs, "EXE", Command);

         declare
            procedure Run (Exe_File : String);
            --  Run Exe_File in QEMU

            ---------
            -- Run --
            ---------

            procedure Run (Exe_File : String) is
               Histmap : String_Access := null;
            begin
               if MCDC_Coverage_Enabled then
                  if Length (ALIs_Inputs) = 0 then
                     Warn ("No SCOs specified for MC/DC level");

                  else
                     Histmap := new String'(Exe_File & ".dmap");
                     Load_All_SCOs (Check_SCOs => False);
                     Build_Decision_Map (Exe_File, Text_Start, Histmap.all);
                  end if;
               end if;

               Rundrv.Driver (Exe_File, Target, Tag, Output, Histmap,
                              Kernel, Eargs);
            end Run;
         begin
            Inputs.Iterate (Exe_Inputs, Run'Access);
         end;

      when Cmd_Convert =>
         declare
            Histmap : String_Access := null;
         begin
            if MCDC_Coverage_Enabled then
               if Length (ALIs_Inputs) = 0 then
                  Warn ("No SCOs specified for MC/DC level");

               else
                  Histmap := new String'(Opt_Exe_Name.all & ".dmap");
                  Load_All_SCOs (Check_SCOs => False);
                  Build_Decision_Map (Opt_Exe_Name.all, Text_Start,
                                      Histmap.all);
               end if;
            end if;

            Convert.Run_Convert (Opt_Exe_Name, Output, Histmap, Tag);
         end;
   end case;

   if Verbose then
      Perf_Counters.Display;
   end if;

exception
   when Xcov_Exit_Exc =>
      --  An error message has already been displayed

      null;
end GNATcov;
