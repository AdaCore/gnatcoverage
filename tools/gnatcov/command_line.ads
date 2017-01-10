------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2015, AdaCore                       --
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

with Argparse;
with Coverage;
with Coverage.Tags;
with Rundrv.Config;
with Switches;

package Command_Line is

   type Command_Type is
     (None,

      Cmd_Help,
      Cmd_Help_Internal,
      Cmd_Version,

      Cmd_Run,
      Cmd_Convert,
      Cmd_Coverage,
      Cmd_Scan_Decisions,

      Cmd_Disp_Routines,
      Cmd_Map_Routines,
      Cmd_Check_SCOs,

      Cmd_Dump_Trace,
      Cmd_Dump_Trace_Raw,
      Cmd_Dump_Trace_Base,
      Cmd_Dump_Trace_Asm,
      Cmd_Dump_Sections,
      Cmd_Dump_Symbols,
      Cmd_Dump_Compile_Units,
      Cmd_Dump_Subprograms,
      Cmd_Dump_Lines,
      Cmd_Dump_CFG,

      Cmd_Disassemble_Insn_Properties,
      Cmd_Disassemble_Raw,
      Cmd_Disassemble,
      Cmd_Scan_Objects);
   --  Set of commands we support. More complete descriptions below.

   type Bool_Options is
     (Opt_Verbose,
      Opt_Recursive,
      Opt_All_Decisions,
      Opt_All_Messages,
      Opt_Include,
      Opt_Exclude,
      Opt_Branch_Stats,
      Opt_Excluded_SCOs,
      Opt_Keep_Edges,
      Opt_Pretty_Print);
   --  Set of boolean options we support. More complete descriptions below.

   type String_Options is
     (Opt_Project,
      Opt_Subdirs,
      Opt_Target,
      Opt_Runtime,
      Opt_Config,
      Opt_Output,
      Opt_Output_Directory,
      Opt_Tag,
      Opt_Kernel,
      Opt_Coverage_Level,
      Opt_Text_Start,
      Opt_Exec_Prefix,
      Opt_Annotation_Format,
      Opt_Final_Report,
      Opt_HW_Trigger_Traces,
      Opt_Input,
      Opt_Separate,
      Opt_Output_Format,
      Opt_Trace_Source,
      Opt_Save_Checkpoint);
   --  Set of string options we support. More complete descriptions below.

   type String_List_Options is
     (Opt_Debug,
      Opt_Projects,
      Opt_Scenario_Var,
      Opt_Eargs,
      Opt_Scos,
      Opt_Units,
      Opt_Routines,
      Opt_Routines_List,
      Opt_Exec,
      Opt_Source_Rebase,
      Opt_Source_Search,
      Opt_Trace,
      Opt_Checkpoint,
      Opt_Ignore_Source_Files);
   --  Set of string list options we support. More complete descriptions below.

   package Parser is new Argparse
     (Command_Type, Bool_Options, String_Options, String_List_Options);
   use Parser;

   Command_Infos : constant Command_Info_Array :=
     (Cmd_Help => Create
        (Name        => "--help",
         Description => ("Display help for command line commands and"
                         & " arguments."),
         Internal    => False),
      Cmd_Help_Internal => Create
        (Name        => "--help-internal",
         Description => ("Display help for internal command line commands and"
                         & "arguments."),
         Internal    => True),
      Cmd_Version => Create
        (Name        => "--version",
         Description => "Display the version.",
         Internal    => False),

      Cmd_Run => Create
        (Name        => "run",
         Pattern     => "[OPTIONS] [EXE] [-eargs [EXE] EARGS...]",
         Description => "Run a program and generate a trace file.",
         Internal    => False),
      Cmd_Convert => Create
        (Name        => "convert",
         Pattern     => ("[OPTIONS] --trace-source=SOURCE_ID --exec=EXECUTABLE"
                         & " --input=INPUT_TRACE"),
         Description => ("Convert vendor-specific trace files into"
                         & " GNATcoverage-compatible ones."
                         & ASCII.LF
                         & "  SOURCE_ID specifies source of trace data (e.g."
                         & " iSystem-5634)." & ASCII.LF
                         & "  EXECUTABLE is name of executable which generated"
                         & " data." & ASCII.LF
                         & "  INPUT_TRACE is file containing trace data to be"
                         & " converted."),
         Internal    => False),
      Cmd_Coverage => Create
        (Name        => "coverage",
         Pattern     => ("[OPTIONS] TRACE_FILEs"),
         Description => "Generate a coverage report with TRACE_FILEs.",
         Internal    => False),
      Cmd_Scan_Decisions => Create
        (Name        => "scan-decisions",
         Pattern     => ("--scos=[SCOs] [OPTIONS]"),
         Description => ("Scan source coverage obligations for multiple paths"
                         & " decisions."),
         Internal    => False),

      Cmd_Disp_Routines => Create
        (Name        => "disp-routines",
         Pattern     => "{[--exclude|--include] FILEs}",
         Description => "Build a list of routines from object FILEs.",
         Internal    => False),
      Cmd_Map_Routines => Create
        (Name        => "map-routines",
         Pattern     => "--scos=[SCOs] [EXE]",
         Description => "Only perform decision static analysis.",
         Internal    => True),
      Cmd_Check_SCOs => Create
        (Name        => "check-scos",
         Description => "Parse and load SCOs files to check them.",
         Internal    => True),

      Cmd_Dump_Trace => Create
        (Name        => "dump-trace",
         Pattern     => "[TRACE_FILEs]",
         Description => "Display of trace files, slided if necessary.",
         Internal    => True),
      Cmd_Dump_Trace_Raw => Create
        (Name        => "dump-trace-raw",
         Pattern     => "[TRACE_FILEs]",
         Description => "Raw display of trace files.",
         Internal    => True),
      Cmd_Dump_Trace_Base => Create
        (Name        => "dump-trace-base",
         Pattern     => "[TRACE_FILEs]",
         Description => "",
         Internal    => True),
      Cmd_Dump_Trace_Asm => Create
        (Name        => "dump-trace-asm",
         Pattern     => "[EXE] [TRACE_FILEs]",
         Description => ("Display of trace files with assembly code for each"
                         & " trace."),
         Internal    => True),
      Cmd_Dump_Sections => Create
        (Name        => "dump-sections",
         Pattern     => "[EXEs]",
         Description => "Dump sections from executable files.",
         Internal    => True),
      Cmd_Dump_Symbols => Create
        (Name        => "dump-symbols",
         Pattern     => "[EXEs]",
         Description => "Dump symbols from executable files.",
         Internal    => True),
      Cmd_Dump_Compile_Units => Create
        (Name        => "dump-compile-units",
         Pattern     => "[EXEs]",
         Description => ("Dump the name of DW_TAG_compile_unit from the"
                         & " debugging information in executable files."),
         Internal    => True),
      Cmd_Dump_Subprograms => Create
        (Name        => "dump-subprograms",
         Description => "Dump subprograms from executable files.",
         Internal    => True),
      Cmd_Dump_Lines => Create
        (Name        => "dump-lines",
         Pattern     => "[EXEs]",
         Description => "Dump source line information from executable files.",
         Internal    => True),
      Cmd_Dump_CFG => Create
        (Name        => "dump-cfg",
         Pattern     => "[OPTIONS] [EXE] [SELECTORs]",
         Description => ("Display object code from EXE as a graph. SELECTORs"
                         & " must be a non-empty list of patterns, which is"
                         & " used to match code to draw. The patterns can be:"
                         & ASCII.LF & ASCII.LF
                         & "file:line1:col1-line2-col2" & ASCII.LF
                         & "  Match code included in a source location range"
                         & ASCII.LF & ASCII.LF
                         & "@0xADDRESS" & ASCII.LF
                         & "  Match all code under the symbol an address"
                         & "  belongs to"
                         & ASCII.LF & ASCII.LF
                         & "0xADDRESS..0xADDRESS" & ASCII.LF
                         & "  Match code included in an address range"
                         & ASCII.LF & ASCII.LF
                         & "SYMBOL_NAME" & ASCII.LF
                         & "  Match all code under a symbol"
                         & ASCII.LF & ASCII.LF
                         & "The object code included if the graph must belong"
                         & " to at least one provided pattern."),
                         Internal    => True),

      Cmd_Disassemble_Insn_Properties => Create
        (Name        => "disassemble-insn-properties",
         Pattern     => "[EXE] [SELECTORs]",
         Description => ("Disassemble instructions in the executable matching"
                         & " SELECTORs. The output is a JSON document that"
                         & " describes sections, instruction disassembly"
                         & " tokens and instruction properties. See SELECTORs"
                         & " in the dump-cfg command for details."),
         Internal    => True),
      Cmd_Disassemble_Raw => Create
        (Name        => "disassemble-raw",
         Pattern     => "[EXEs]",
         Description => "Disassemble executables.",
         Internal    => True),
      Cmd_Disassemble => Create
        (Name        => "disassemble",
         Pattern     => "[EXEs]",
         Description => "Disassemble executables.",
         Internal    => True),
      Cmd_Scan_Objects => Create
        (Name        => "scan-objects",
         Pattern     => "[FILEs]",
         Description => ("Scan object FILEs for empty symbols or orphan"
                         & " regions."),
         Internal    => True));

   Bool_Infos : constant Bool_Option_Info_Array :=
     (Opt_Verbose => Create
        (Long_Name => "--verbose", Short_Name => "-v",
         Help      => "Print lots of debugging information.",
         Internal  => True),
      Opt_Recursive => Create
        (Long_Name  => "--recursive",
         Help       => ("In addition to those designated by -P/--projects,"
                        & " consider units from any transitively imported"
                        & " project."),
         Commands   => (Cmd_Run | Cmd_Coverage | Cmd_Dump_CFG => True,
                        others => False),
         Internal   => False),
      Opt_All_Decisions => Create
        (Long_Name => "--all-decisions",
         Help      => ("Perform decision coverage in stmt+decision mode even"
                       & " for decisions outside of control structures."),
         --  TODO??? Is this really supposed to be internal?
         Internal  => True),
      Opt_All_Messages => Create
        (Long_Name => "--all-messages",
         Help      => ("When performing source coverage analysis, also include"
                       & " in the report messages other than violations of a"
                       & " source coverage obligation."),
         Internal  => True),
      Opt_Include => Create
        (Long_Name      => "--include",
         Help           => ("Include the symbols from the next object file on"
                            & " the command line to the list of routines."),
         Commands       => (Cmd_Disp_Routines => True,
                            others            => False),
         Internal       => False),
      Opt_Exclude => Create
        (Long_Name      => "--exclude",
         Help           => ("Exclude the symbols from the next object file on"
                            & " the command line to the list of routines"),
         Commands       => (Cmd_Disp_Routines => True,
                            others            => False),
         Internal       => False),
      Opt_Branch_Stats => Create
        (Long_Name => "--stats",
         Help      => ("Dump statistics about branch instructions after the"
                       & " static analysis pass."),
         Commands   => (Cmd_Map_Routines => True,
                        others => False),
         Internal  => True),
      Opt_Excluded_SCOs => Create
        (Long_Name => "--non-coverable",
         Help      => ("Report SCOs whose coverage cannot be established due"
                       & " to absence of executable code."),
         Commands  => (Cmd_Coverage => True,
                       others => False),
         Internal  => False),
      Opt_Keep_Edges => Create
        (Short_Name => "-k",
         Help      => ("Do not strip edges that are supposed to be uncoverable"
                       & " due to exceptions."),
         Commands  => (Cmd_Dump_CFG => True,
                       others => False),
         Internal  => True),
      Opt_Pretty_Print => Create
        (Long_Name => "--pretty-print",
         Help      => "Output a pretty-printed JSON to ease debugging.",
         Commands  => (Cmd_Disassemble_Insn_Properties => True,
                       others => False),
         Internal  => True));

   String_Infos : constant String_Option_Info_Array :=
     (Opt_Project => Create
        (Short_Name   => "-P",
         Pattern      => "[GPR]",
         Help         => ("Use GPR as root project to locate SCOs, select"
                          & " units to analyze and find default options."),
         At_Most_Once => True,
         Internal     => False),
      Opt_Subdirs => Create
        (Long_Name    => "--subdirs",
         Pattern      => "[SUBDIR]",
         Help         => ("When using project files, look for ALI files in the"
                          & " provided SUBDIR of the projects' build"
                          & " directory."),
         At_Most_Once => False,
         Internal     => False),
      Opt_Target => Create
        (Long_Name    => "--target",
         Short_Name   => "-t",
         Pattern      => "[TARGET]",
         Help         => ("When using projects files, state the target"
                          & " toolchain prefix used to build the analyzed"
                          & " programs. If project files don't already set the"
                          & " target, this is required for correct project"
                          & " files processing and correct execution target"
                          & " selection with cross targets."
                          & ASCII.LF & ASCII.LF
                          & "Target must either match one of:" & ASCII.LF
                          & "  " & Rundrv.Config.Available_Targets
                          & ASCII.LF
                          & "or there must be a TARGET-gnatemu program"
                          & " available."),
         At_Most_Once => True,
         Internal     => False),
      Opt_Runtime => Create
        (Long_Name    => "--RTS",
         Pattern      => "[RUNTIME]",
         Help         => ("When using projects files, state the runtime"
                          & " used to build the analyzed programs. If project"
                          & " files don't already set the runtime, this is"
                          & " required for correct project files processing."),
         At_Most_Once => True,
         Internal     => False),
      Opt_Config => Create
        (Long_Name    => "--config",
         Pattern      => "[CONFIG-FILE]",
         Help         => ("Specify a configuration project file name. If"
                          & " passed, this file must exist and neither"
                          & " --target nor --RTS must be present."),
         At_Most_Once => True,
         Internal     => False),
      Opt_Output => Create
        (Long_Name    => "--output",
         Short_Name   => "-o",
         Pattern      => "[FILE]",
         Help         => "Put the report/asm output/trace file into FILE.",
         Commands     => (Cmd_Run | Cmd_Coverage | Cmd_Convert
                              | Cmd_Dump_CFG => True,
                          others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Output_Directory => Create
        (Long_Name    => "--output-dir",
         Pattern      => "[SUBDIR]",
         Help         => ("Subdirectory where XCOV or HTML report files"
                          & " should be produced"),
         Commands     => (Cmd_Coverage => True,
                          others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Tag => Create
        (Long_Name    => "--tag",
         Short_Name   => "-T",
         Pattern      => "[TAG]",
         Help         => ("Tag to put into trace file."),
         Commands     => (Cmd_Run | Cmd_Convert => True,
                          others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Kernel => Create
        (Long_Name    => "--kernel",
         Pattern      => "[KERNEL]",
         Help         => ("Specify which kernel to use."),
         Commands     => (Cmd_Run => True,
                          others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Coverage_Level => Create
        (Long_Name    => "--level",
         Short_Name   => "-c",
         Pattern      => "[LEVEL]",
         Help         => ("Specify coverage levels. LEVEL is one of:"
                          & ASCII.LF
                          & "  " & Coverage.Valid_Coverage_Options),
         Commands     => (Cmd_Run | Cmd_Coverage | Cmd_Convert => True,
                          others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Text_Start => Create
        (Long_Name    => "--text-start",
         Pattern      => "[HEX_ADDR]",
         Help         => ("??? This option was likely introduced for a"
                          & " feature that is not completely implemented."),
         At_Most_Once => False,
         Internal     => True),
      Opt_Exec_Prefix => Create
        (Long_Name    => "--exec-prefix",
         Pattern      => "[PREFIX]",
         Help         => ("In cases where we cannot find executable files,"
                          & " look for them in the PREFIX directory."),
         At_Most_Once => False,
         Internal     => True),
      Opt_Annotation_Format => Create
        (Long_Name    => "--annotate",
         Short_Name   => "-a",
         Pattern      => "[FORM]",
         Help         => ("Generate a FORM report. FORM is one of:" & ASCII.LF
                          & "  asm, xcov, html, xcov+, html+, dhtml, report"),
         Commands     => (Cmd_Coverage => True,
                          others => False),
         At_Most_Once => False,
         Internal     => True),
      Opt_Final_Report => Create
        (Long_Name    => "--report",
         Pattern      => "[FILE]",
         Help         => ("??? This option is redundant with --option and is"
                          & " not documented in the User Manual."),
         Commands     => (Cmd_Coverage => True,
                          others => False),
         At_Most_Once => False,
         Internal     => True),
      Opt_HW_Trigger_Traces => Create
        (Long_Name    => "--hw-trigger-traces",
         Pattern      => "[START_ID],[START_ADDR],[STOP_ID]",
         Help         => ("Identity of start and stop triggers, and address"
                          & " for start."),
         Commands     => (Cmd_Convert => True,
                          others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Input => Create
        (Long_Name    => "--input",
         Pattern      => "[INPUT_TRACE]",
         Help         => ("File containing trace data to be converted."),
         Commands     => (Cmd_Convert => True,
                          others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Separate => Create
        (Short_Name   => "-S",
         Pattern      => Coverage.Tags.Tag_Providers.Registered_Names ("|"),
         Help         => ("Perform separate source coverage (EXPERIMENTAL)."),
         Commands     => (Cmd_Coverage => True,
                          others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Output_Format => Create
        (Short_Name   => "-f",
         Pattern      => "[FORMAT]",
         Help         => ("If given, call dot(1) to produce the actual output"
                          & " (SVG, PDF, DOT, ...)."),
         Commands     => (Cmd_Dump_CFG => True,
                          others => False),
         At_Most_Once => False,
         Internal     => True),
      Opt_Trace_Source => Create
        (Long_Name    => "--trace-source",
         Pattern      => "[SOURCE]",
         Help         => "Specify a trace source.",
         Commands     => (Cmd_Convert => True,
                        others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Save_Checkpoint => Create
        (Long_Name    => "--save-checkpoint",
         Pattern      => "CHECKPOINT",
         Help         => "Save source coverage checkpoint to named file.",
         Commands     => (Cmd_Coverage => True,
                          others       => False),
         At_Most_Once => True,
         Internal     => False));

   String_List_Infos : constant String_List_Option_Info_Array :=
     (Opt_Debug => Create
        (Short_Name => "-d",
         Pattern    => Switches.Debug_Command_Line_Pattern,
         Help       => Switches.Debug_Command_Line_Help,
         Internal   => True),
      Opt_Projects => Create
        (Long_Name  => "--projects",
         Pattern    => "[GPR|@LISTFILE]",
         Help       => ("Focus on specific projects within the transitive"
                          & " closure reachable from the root designated by"
                          & " -P."),
         Internal   => False),
      Opt_Scenario_Var => Create
        (Short_Name => "-X",
         Pattern    => "[NAME]=[VALUE]",
         Help       => "Define a scenario variable for project files.",
         Internal   => False),
      Opt_Eargs => Create
        (Long_Name  => "-eargs",
         Pattern    => "[EARGS ...]",
         Help       => ("For emulated cross targets, pass EARGS to the"
                        & " low-level emulator. For native configurations,"
                        & " pass EARGS as command line arguments to the"
                        & " executed program. The first EARG is picked as the"
                        & " executable program to run if it was not provided"
                        & " explicitely otherwise."),
         Commands   => (Cmd_Run => True,
                        others => False),
         Internal   => False,
         Greedy     => True),
      Opt_Scos => Create
        (Long_Name   => "--scos|--alis",
         Pattern     => "[FILE|@LISTFILE]",
         Help        => ("Load SCOs and exemption info from FILE for this"
                         & " operation; or do that for each file listed in"
                         & " LISTFILE."),
         Commands   => (Cmd_Run | Cmd_Convert | Cmd_Coverage
                            | Cmd_Scan_Decisions | Cmd_Map_Routines
                              | Cmd_Dump_CFG => True,
                        others => False),
         Internal    => False),
      Opt_Units => Create
        (Long_Name   => "--units",
         Pattern     => "[UNIT|@LISTFILE]",
         Help        => ("State the set of units of interest by name,"
                         & " overriding the GPR-based selection by -P, etc."),
         Commands    => (Cmd_Run | Cmd_Coverage | Cmd_Scan_Decisions
                             | Cmd_Map_Routines => True,
                         others => False),
         Internal    => False),
      Opt_Routines => Create
        (Long_Name   => "--routines",
         Pattern     => "[ROUTINE|@LISTFILE]",
         Help        => ("Add ROUTINE, or all routines listed in LISTFILE to"
                         & " the list of routines."),
         Commands    => (Cmd_Run | Cmd_Coverage | Cmd_Scan_Decisions
                             | Cmd_Map_Routines => True,
                         others => False),
         Internal    => False),
      Opt_Routines_List => Create
        (Long_Name   => "--routine-list",
         Short_Name  => "-l",
         Pattern     => "[LISTFILE] (DEPRECATED)",
         Help        => ("Add all routines listed in LISTFILE to the list of"
                         & " routines."),
         Commands    => (Cmd_Run | Cmd_Coverage => True,
                         others => False),
         Internal    => False),
      Opt_Exec => Create
        (Long_Name    => "--exec",
         Pattern      => "[EXE]",
         Help         => ("For ""coverage"", use E as the base executable for"
                          & " the traces that follow on the command line. For"
                          & " ""convert"", use E as the base executable for"
                          & " trace conversion."),
         Commands     => (Cmd_Convert | Cmd_Coverage => True,
                          others => False),
         Internal     => True),
      Opt_Source_Rebase => Create
        (Long_Name   => "--source-rebase",
         Pattern     => "[OLD_PREFIX]=[NEW_PREFIX]",
         Help        => "Specify alternative path to find source files.",
         Commands    => (Cmd_Coverage => True,
                         others => False),
         Internal    => True),
      Opt_Source_Search => Create
        (Long_Name   => "--source-search",
         Pattern     => "[PREFIX]",
         Help        => "Specify an alternative prefix to find source files.",
         Commands    => (Cmd_Coverage => True,
                         others => False),
         Internal    => True),

      --  Opt_Trace will receive both trace files arguments, but also
      --  executable names to force the association between trace files and
      --  programs. In order to distinguish both, executable names are prefixed
      --  with ASCII.NUL.

      Opt_Trace => Create
        (Long_Name   => "--trace",
         Short_Name  => "-T",
         Pattern     => "[TRACE|@LISTFILE]",
         Help        => "Specify trace files to read.",
         Commands    => (Cmd_Coverage | Cmd_Dump_Trace | Cmd_Dump_Trace_Raw
                             | Cmd_Dump_Trace_Base | Cmd_Dump_Trace_Asm
                               | Cmd_Dump_CFG => True,
                         others => False),
         Internal    => False),

      Opt_Checkpoint => Create
        (Long_Name   => "--checkpoint",
         Short_Name  => "-C",
         Pattern     => "[CHECKPOINT|@LISTFILE]",
         Help        => "Specify checkpointed coverage information to load.",
         Commands    => (Cmd_Coverage => True,
                         others       => False),
         Internal    => False),
      Opt_Ignore_Source_Files => Create
        (Long_Name => "--ignore-source-files",
         Pattern   => "[FILE|@LISTFILE]",
         Help      => "Specify a list of source files to ignore for coverage",
         Commands  => (Cmd_Coverage | Cmd_Map_Routines => True,
                       others => False),
         Internal  => False));

   procedure Bool_Callback
     (Result : in out Parsed_Arguments;
      Option : Bool_Options);
   --  Handle the --include and --exclude options to add them to remaining
   --  options.

   procedure String_List_Callback
     (Result : in out Parsed_Arguments;
      Option : String_List_Options;
      Value  : String);
   --  Add --exec options to the Opt_Trace vector with the ASCII.NUL prefix.

   procedure Arg_Callback
     (Result : in out Parsed_Arguments;
      Value  : String);
   --  For the commands that interpret remaining argument as trace files,
   --  forward remaining arguments to Opt_Trace.

   function Create return Parser_Type
   is
     (Create (Command_Infos        => Command_Infos,
              Bool_Infos           => Bool_Infos,
              String_Infos         => String_Infos,
              String_List_Infos    => String_List_Infos,
              Bool_Callback        => Bool_Callback'Access,
              String_Callback      => null,
              String_List_Callback => String_List_Callback'Access,
              Arg_Callback         => Arg_Callback'Access));

end Command_Line;
