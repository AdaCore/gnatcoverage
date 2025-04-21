------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2015-2024, AdaCore                     --
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

with GPR2.Options;

with Argparse;
with Coverage_Options; use Coverage_Options;

package Command_Line is

   type Command_Type is
     (None,

      Cmd_Help,
      Cmd_Help_Internal,
      Cmd_Version,

      Cmd_List_Logs,
      Cmd_Print_GPR_Registry,

      Cmd_Run,
      Cmd_Convert,
      Cmd_Coverage,
      Cmd_Extract_Base64_Trace,
      Cmd_Scan_Decisions,

      Cmd_Disp_Routines,
      Cmd_Map_Routines,
      Cmd_Check_SCOs,

      Cmd_Dump_Trace,
      Cmd_Dump_Trace_Raw,
      Cmd_Dump_Trace_Base,
      Cmd_Dump_Trace_Asm,
      Cmd_Dump_Src_Trace,
      Cmd_Dump_Sections,
      Cmd_Dump_Symbols,
      Cmd_Dump_Compile_Units,
      Cmd_Dump_Subprograms,
      Cmd_Dump_Inlined_Subprograms,
      Cmd_Dump_Lines,
      Cmd_Dump_CFG,
      Cmd_Dump_Pragmas,

      Cmd_Disassemble_Insn_Properties,
      Cmd_Disassemble_Raw,
      Cmd_Disassemble,
      Cmd_Scan_Objects,

      Cmd_Setup,
      Cmd_Instrument_Project,
      Cmd_Instrument_Source,
      Cmd_Instrument_Main,
      Cmd_Setup_Integration,
      Cmd_Gcc_Wrapper,

      Cmd_Add_Annotation,
      Cmd_Delete_Annotation,
      Cmd_Show_Annotations);
   --  Set of commands we support. More complete descriptions below.

   type Bool_Options is
     (Opt_Verbose,
      Opt_Quiet,
      Opt_Recursive,
      Opt_No_Subprojects,
      Opt_All_Decisions,
      Opt_All_Messages,
      Opt_Include,
      Opt_Exclude,
      Opt_Excluded_SCOs,
      Opt_Keep_Edges,
      Opt_Pretty_Print,
      Opt_Keep_Reading_Traces,
      Opt_Externally_Built_Projects,
      Opt_GNAT_Pragmas,
      Opt_Show_MCDC_Vectors,
      Opt_Show_Condition_Vectors,
      Opt_Dump_Filename_Simple,
      Opt_Allow_Mix_Trace_Kind,
      Opt_Boolean_Short_Circuit_And_Or,
      Opt_Cancel_Annotate,
      Opt_All_Warnings,
      Opt_Save_Temps,
      Opt_SPARK_Compat,
      Opt_Full_Slugs,
      Opt_Relocate_Build_Tree,
      Opt_Warnings_As_Errors,
      Opt_Instrument_Block,
      Opt_Force,
      Opt_Annotate_After,
      Opt_No_Stdlib);
   --  Set of boolean options we support. More complete descriptions below.

   type String_Options is
     (Opt_Project,
      Opt_Root_Dir,
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
      Opt_HW_Trigger_Traces,
      Opt_Input,
      Opt_Separate,
      Opt_Output_Format,
      Opt_Trace_Source,
      Opt_Save_Checkpoint,
      Opt_Report_Title,
      Opt_Dump_Trigger,
      Opt_Dump_Channel,
      Opt_Dump_Filename_Env_Var,
      Opt_Dump_Filename_Prefix,
      Opt_Dump_Filename_Tag,
      Opt_Ada,
      Opt_Dump_Units_To,
      Opt_Timezone,
      Opt_Prefix,
      Opt_RTS_Profile,
      Opt_Path_Count_Limit,
      Opt_Install_Name,
      Opt_Runtime_Project,
      Opt_Lang,
      Opt_Parallelism_Level,
      Opt_Compiler_Driver,
      Opt_Spec_Suffix,
      Opt_Body_Suffix,
      Opt_Dot_Replacement,
      Opt_Casing,
      Opt_Gnatem,
      Opt_Config_Pragmas_Mapping,
      Opt_Ada_Preprocessor_Data,
      Opt_Project_Name,
      Opt_Source_Root,
      Opt_Db,
      Opt_GPR_Registry_Format,
      Opt_Annotation_Kind,
      Opt_Annotation_Id,
      Opt_Location,
      Opt_Start_Location,
      Opt_End_Location,
      Opt_Justification,
      Opt_SS_Backend,
      Opt_Source_Encoding);
   --  Set of string options we support. More complete descriptions below.

   type String_List_Options is
     (Opt_Log,
      Opt_Projects,
      Opt_Scenario_Var,
      Opt_Cargs,
      Opt_Eargs,
      Opt_Gargs,
      Opt_Scos,
      Opt_Units,
      Opt_SID,
      Opt_Subp_Of_Interest,
      Opt_Routines,
      Opt_Routines_List,
      Opt_Exec,
      Opt_Source_Rebase,
      Opt_Source_Search,
      Opt_Trace,
      Opt_Checkpoint,
      Opt_Ignore_Source_Files,
      Opt_Shared_Object,
      Opt_Restricted_To_Languages,
      Opt_Annotation_Format,
      Opt_C_Opts,
      Opt_CPP_Opts,
      Opt_Files,
      Opt_Runtime_Dir,
      Opt_Compiler_Wrappers,
      Opt_Ext_Annotations);
   --  Set of string list options we support. More complete descriptions below.

   subtype Cmd_Instrument is Command_Type
     range Cmd_Instrument_Project .. Cmd_Instrument_Main;

   subtype Cmd_Instrument_With_Setup is Command_Type
     range Cmd_Instrument_Project .. Cmd_Setup_Integration;

   subtype Cmd_All_Setups is Command_Type
   with Static_Predicate =>
     Cmd_All_Setups in Cmd_Setup | Cmd_Setup_Integration;

   subtype Cmd_All_Annotate is Command_Type
     range Cmd_Add_Annotation .. Cmd_Show_Annotations;

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

      Cmd_List_Logs          => Create
        (Name          => "list-logs",
         Pattern       => "",
         Description   =>
           "Dump the list of GNATcov available logs (GNATCOLL traces).",
         Internal      => True),
      Cmd_Print_GPR_Registry => Create
        (Name          => GPR2.Options.Print_GPR_Registry_Option,
         Pattern       => "",
         Description   => "Print the GPR registry.",
         Internal      => True),

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
      Cmd_Extract_Base64_Trace => Create
        (Name        => "extract-base64-trace",
         Pattern     => ("[OPTIONS] INPUT_BASE64_FILE OUTPUT_TRACE_FILE"),
         Description => ("Extract a trace file from a log file that contains"
                         & " Base64-encoded traces."),
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
      Cmd_Dump_Src_Trace => Create
        (Name        => "dump-src-trace",
         Pattern     => "[TRACE_FILEs]",
         Description => "Dump the content of a source trace file in a human"
                        & "-readable text form.",
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
      Cmd_Dump_Inlined_Subprograms => Create
        (Name        => "dump-inlined-subprograms",
         Description => "Dump inlined subprograms from executable files.",
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
                         & "file:line1:col1-line2:col2" & ASCII.LF
                         & "  Match code included in a source location range."
                         & ASCII.LF & ASCII.LF
                         & "@0xADDRESS" & ASCII.LF
                         & "  Match all code under the symbol an address"
                         & "  belongs to."
                         & ASCII.LF & ASCII.LF
                         & "0xADDRESS..0xADDRESS" & ASCII.LF
                         & "  Match code included in an address range."
                         & ASCII.LF & ASCII.LF
                         & "SYMBOL_NAME" & ASCII.LF
                         & "  Match all code under a symbol."
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
      Cmd_Dump_Pragmas => Create
        (Name          => "dump-pragmas",
         Pattern       => "",
         Description   => "Dump the list of names for Ada pragmas that gnatcov"
                          & " knows, or that GNAT knows (through gnat_util) if"
                          & " --gnat-pragmas is passed.",
         Internal      => True),
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
         Internal    => True),
      Cmd_Setup => Create
        (Name        => "setup",
         Description => "Build and install the runtime for source"
                        & " instrumentation and set up default arguments"
                        & " for future ""gnatcov instrument"" invocations.",
         Internal    => False),
      Cmd_Instrument_Project => Create
        (Name        => "instrument",
         Description => ("Instrument the given project and produce the"
                         & " associated Source Instrumentation Data files."),
         Internal    => False),
      Cmd_Instrument_Source => Create
        (Name        => "instrument-source",
         Description => "Instrument the given files, all belonging to the"
                        & " same unit.",
         Pattern     => "[FILES]",
         Internal    => True),
      Cmd_Instrument_Main => Create
        (Name        => "instrument-main",
         Description => "Insert dump trigger code in the the given main file,"
                        & " dumping coverage buffers of all of the specified"
                        & " files (through the --files switch).",
         Pattern     => "--files=<files_of_interest> [MAIN]",
         Internal    => True),
      Cmd_Setup_Integration => Create
        (Name        => "setup-integration",
         Description =>
           "Generate the adequate configuration to use gnatcov in integrated"
           & " instrumentation mode. The files of interest must be passed"
           & " through the --files switch, the compiler driver in used through"
           & " the --compilers switch, and the configuration and compiler"
           & " driver wrappers are generated in the subdirectory pointed by"
           & " the --output-dir switch.",
         Pattern     =>
           "--files=<files_of_interest> --compilers=<compiler>"
         & " [--output-dir=<dir>]",
         Internal    => False),
      Cmd_Gcc_Wrapper => Create
        (Name        => "gcc-wrapper",
         Description =>
           "Implementation of the GCC wrapper for integrated instrumentation.",
         Pattern     => "[JSON-CONFIG-FILE]",
         Internal    => True),
      Cmd_Add_Annotation => Create
        (Name        => "add-annotation",
         Description =>
           "Add an annotation tied to the specified source locations, to"
           & " influence the instrumentation of source files or the coverage"
           & " analysis. All annotations passed through --external-annotations"
           & " are consolidated and written to OUTPUT_FILENAME."
           & ASCII.LF & ASCII.LF
           & "The relevant switches depend on the annotation KIND:"
           & Annot_Kind_Relevant_Switches,
         Pattern     =>
           "--kind=KIND [--external-annotations=FILENAME] --output="
           & "OUTPUT_FILENAME [OPTIONS] FILENAME",
         Internal    => False),
      Cmd_Delete_Annotation => Create
        (Name        => "delete-annotation",
         Description =>
           "Delete the annotation with identifier IDENTIFIER from those stored"
           & " in EXT_FILENAMEs, and write back the remaining annotations to"
           & " OUTPUT_FILENAME.",
         Pattern     =>
           "--annotation-id=IDENTIFIER --external-annotations=EXT_FILENAME"
           & "--output=OUTPUT_FILENAME",
         Internal    => False),
      Cmd_Show_Annotations => Create
        (Name        => "show-annotations",
         Description =>
           "Show the annotations stored in EXT_FILENAME that apply to FILES,"
           & " if present or to the project sources. Optionally filter the"
           & " kind of annotations to show with --kind.",
         Pattern     =>
           "--external-annotations=EXT_FILENAME [--kind=KIND] [OPTIONS]"
           & " [FILENAME]",
         Internal    => False));

   Bool_Infos : constant Bool_Option_Info_Array :=
     (Opt_Verbose => Create
        (Long_Name => "--verbose", Short_Name => "-v",
         Help      => "Print lots of debugging information.",
         Internal  => True),
      Opt_Quiet => Create
        (Long_Name => "--quiet", Short_Name => "-q",
         Help      => "Print nothing but errors/warnings.",
         Internal  => False),
      Opt_Recursive => Create
        (Long_Name  => "--recursive",
         Help       => "DEPRECATED: In addition to those designated by"
                       & " -P/--projects, consider units from any"
                       & " transitively imported project.",
         Commands   => (Cmd_Run
                        | Cmd_Coverage
                        | Cmd_Instrument_Project
                        | Cmd_Dump_CFG => True,
                        others         => False),
         Internal   => False),
      Opt_No_Subprojects => Create
        (Long_Name  => "--no-subprojects",
         Help       => "Consider only units designated by the project"
                       & " designated by -P/--projects. Units from any"
                       & " transitively imported projects are not considered.",
         Commands   => (Cmd_Run
                        | Cmd_Coverage
                        | Cmd_Instrument_Project
                        | Cmd_Dump_CFG => True,
                        others         => False),
         Internal   => False),
      Opt_All_Decisions => Create
        (Long_Name => "--all-decisions",
         Help      => "Perform decision coverage in stmt+decision mode even"
                      & " for decisions outside of control structures.",
         --  TODO??? Is this really supposed to be internal?
         Internal  => True),
      Opt_All_Messages => Create
        (Long_Name => "--all-messages",
         Help      => "When performing source coverage analysis, also include"
                      & " in the report messages other than violations of a"
                      & " source coverage obligation.",
         Internal  => True),
      Opt_Include => Create
        (Long_Name      => "--include",
         Help           => "Include the symbols from the next object file on"
                           & " the command line to the list of routines.",
         Commands       => (Cmd_Disp_Routines => True, others => False),
         Internal       => False),
      Opt_Exclude => Create
        (Long_Name      => "--exclude",
         Help           => "Exclude the symbols from the next object file on"
                           & " the command line to the list of routines",
         Commands       => (Cmd_Disp_Routines => True, others => False),
         Internal       => False),
      Opt_Excluded_SCOs => Create
        (Long_Name => "--non-coverable",
         Help      => "Report SCOs whose coverage cannot be established due to"
                      & " absence of executable code.",
         Commands  => (Cmd_Coverage => True, others => False),
         Internal  => False),
      Opt_Keep_Edges => Create
        (Short_Name => "-k",
         Help      => "Do not strip edges that are supposed to be uncoverable"
                      & " due to exceptions.",
         Commands  => (Cmd_Dump_CFG => True, others => False),
         Internal  => True),
      Opt_Pretty_Print => Create
        (Long_Name => "--pretty-print",
         Help      => "For the disassemble-insn-properties command, output a"
                      & " pretty-printed JSON to ease debugging. For the"
                      & " instrument command, run gnatpp on the generated"
                      & " sources.",
         Commands  => (Cmd_Disassemble_Insn_Properties
                       | Cmd_Instrument => True,
                       others           => False),
         Internal  => True),

      Opt_Keep_Reading_Traces => Create
        (Long_Name => "--keep-reading-traces",
         Help      => "When an error occurs while reading a trace file,"
                      & " skip it and keep reading other trace files until a"
                      & " coverage report can be produced. Note that this"
                      & " raises a warning, but does not make gnatcov exit"
                      & " with an error status.",
         Commands  => (Cmd_Coverage => True, others => False),
         Internal  => False),

      Opt_Externally_Built_Projects => Create
        (Long_Name => "--externally-built-projects",
         Help      => "Look into projects marked as externally built when"
                      & " computing the list of units of interest (they are"
                      & " ignored by default). For the ""instrument"" command,"
                      & " this only influences the instrumentation of mains.",
         Commands  => (Cmd_Run
                       | Cmd_Instrument
                       | Cmd_Coverage
                       | Cmd_Dump_CFG => True,
                       others         => False),
         Internal  => False),

      Opt_GNAT_Pragmas => Create
        (Long_Name => "--gnat-pragmas",
         Help      => "Dump the list of GNAT pragmas, from gnat_util.",
         Commands  => (Cmd_Dump_Pragmas => True, others => False),
         Internal  => True),

      Opt_Show_MCDC_Vectors => Create
        (Long_Name => "--show-mcdc-vectors",
         Help      => "DEPRECATED: If set, show MCDC evaluation vectors"
                      & " encountered for decisions where there is at least a"
                      & " MCDC violation on one of the conditions. Prefer"
                      & " using --show-condition-vectors.",
         Commands  => (Cmd_Coverage => True, others => False),
         Internal  => False),

      Opt_Show_Condition_Vectors => Create
        (Long_Name => "--show-condition-vectors",
         Help      => "If set, show MCDC and ATCC evaluation vectors"
                      & " encountered for decisions where there is at least a"
                      & " MCDC or ATCC violation on one of the conditions.",
         Commands  => (Cmd_Coverage => True, others => False),
         Internal  => False),

      Opt_Dump_Filename_Simple => Create
        (Long_Name => "--dump-filename-simple",
         Help      => "Valid only when --dump-channel=bin-file. Create simple"
                      & " filenames for source trace files (no PID/timestamp"
                      & " variations).",
         Commands  => (Cmd_Setup
                       | Cmd_Instrument_Project
                       | Cmd_Setup_Integration
                       | Cmd_Instrument_Main => True,
                       others                => False),
         Internal  => False),

      Opt_Allow_Mix_Trace_Kind => Create
        (Long_Name => "--mix-trace-kind",
         Help      => "Allow mixing binary and source traces. A warning will"
                      & " still be emitted.",
         Commands  => (Cmd_Coverage => True, others => False),
         Internal  => True),

      Opt_Boolean_Short_Circuit_And_Or => Create
        (Long_Name => "--short-circuit-and-or",
         Help      => "For Ada sources, consider that boolean operators"
                      & " ""and"" and ""or"" have short-circuit semantics and"
                      & " instrument them accordingly. This does not modify"
                      & " the actual semantics of the operators, which needs"
                      & " to be done with the Short_Circuit_And_Or pragma.",
         Commands   => (Cmd_Instrument => True, others => False),
         Internal   => True),

      Opt_Cancel_Annotate => Create
        (Long_Name => "--cancel-annotate",
         Help      => "Prevent gnatcov from generating a coverage report if"
                      & " one was requested by --annotate. This option"
                      & " requires --save-checkpoint to also be specified on"
                      & " the command line.",
         Commands  => (Cmd_Coverage => True, others => False),
         Internal  => True),

      Opt_All_Warnings => Create
        (Long_Name => "--all-warnings",
         Help      => "Print low warnings in addition to warnings and errors.",
         Internal  => True),

      Opt_Save_Temps => Create
        (Long_Name => "--save-temps",
         Help      => "Do not remove temporary files and directories.",
         Internal  => True),

      Opt_SPARK_Compat => Create
        (Long_Name => "--spark-compat",
         Help      => "Enable the SPARK compatibility mode. This ensures"
                      & " instrumented code will be ghost compliant.",
         Commands  => (Cmd_Instrument => True, others => False),
         Internal  => False),

      Opt_Full_Slugs => Create
        (Long_Name => "--full-slugs",
         Help      => "Use full unit slugs instead of hashes for buffer units",
         Commands  => (Cmd_Instrument => True, others => False),
         Internal  => True),

      Opt_Relocate_Build_Tree => Create
        (Long_Name => "--relocate-build-tree",
         Help      => "Relocate object, library and exec directories in the"
                      & " current directory.",
         Commands  => (Cmd_All_Setups | Cmd_Print_GPR_Registry => False,
                       others                                  => True),
         Internal  => False),

      Opt_Warnings_As_Errors => Create
        (Long_Name  => "--warnings-as-errors",
         Short_Name => "-W",
         Help       => "Treat warnings as errors, i.e. exit with a non-zero"
                       & " status code if a warning is emitted.",
         Commands   => (others => True),
         Internal   => True),

      Opt_Instrument_Block => Create
        (Long_Name => "--instrument-block",
         Help      => "Instrument the statements as blocks, i.e. insert a"
                      & " single instrumentation counter incrementation for a"
                      & " statement block.",
         Commands  => (Cmd_Instrument => True, others => False),
         Internal  => False),
      Opt_Force => Create
        (Long_Name  => "--force",
         Short_Name => "-f",
         Help       =>
           "Overwrite preexisting annotations with the same kind matching the"
           & " same location, or any preexisting annotation with the same"
           & " identifier as the one specified on the command line.",
         Commands   => (Cmd_Add_Annotation | Cmd_Delete_Annotation => True,
                        others => False),
         Internal   => False),
      Opt_Annotate_After => Create
        (Long_Name => "--annotate-after",
         Help      =>
           "Add the annotation after the statement designated by the"
           & " --location switch, as opposed to the default behavior which"
           & " inserts the annotation before the designated statement.",
         Commands  => (Cmd_Add_Annotation => True, others => False),
         Internal  => False),
      Opt_No_Stdlib => Create
        (Long_Name => "--no-stdlib",
         Help      =>
           "Setup the coverage runtime in a way that is compatible with the"
           & " -nostdlib builder switch. This requires that the output"
           & " function for the coverage runtime must be provided by the user,"
           & " see section ""Coverage runtime setup for configurations with no"
           & " Ada runtime"" of the User's Guide for more details.",
         Commands  => (Cmd_Setup => True, others => False),
         Internal  => False));

   String_Infos : constant String_Option_Info_Array :=
     (Opt_Project => Create
        (Short_Name   => "-P",
         Pattern      => "[GPR]",
         Help         => "Use GPR as root project to locate SCOs, select"
                         & " units to analyze and find default options.",
         Commands     => (Cmd_Print_GPR_Registry
                          | Cmd_Setup
                          | Cmd_Setup_Integration
                          | Cmd_Instrument_Source
                          | Cmd_Instrument_Main => False,
                          others => True),
         At_Most_Once => True,
         Internal     => False),
      Opt_Root_Dir => Create
        (Long_Name    => "--root-dir",
         Pattern      => "[DIR]",
         Help         => "When --relocate-build-tree is active, this"
                         & " designates the topmost directory of the tree of"
                         & " projects. By default the root project's directory"
                         & " is used.",
         Commands     => (Cmd_Print_GPR_Registry
                          | Cmd_All_Setups => False,
                          others => True),
         At_Most_Once => True,
         Internal     => False),
      Opt_Subdirs => Create
        (Long_Name    => "--subdirs",
         Pattern      => "[SUBDIR]",
         Help         => "When using project files, look for ALI/SID files in"
                         & " the provided SUBDIR of the projects' build"
                         & " directory.",
         Commands     => (Cmd_Print_GPR_Registry
                          | Cmd_All_Setups => False,
                          others => True),
         At_Most_Once => False,
         Internal     => False),
      Opt_Target => Create
        (Long_Name    => "--target",
         Short_Name   => "-t",
         Pattern      => "[TARGET]",
         Help         => "State the target toolchain configuration used to"
                         & " build the analyzed programs, as provided to"
                         & " gprbuild. For cross or 32bit native"
                         & " configurations, this switch, together with its"
                         & " possible --RTS companion, is required for all"
                         & " commands using project files unless the root"
                         & " project provides the information with"
                         & " ""Target""/""Runtime"" attributes. It is also"
                         & " needed for ""run"" commands without a project"
                         & " file.",
         Commands     => (Cmd_Print_GPR_Registry
                          | Cmd_Setup_Integration => False,
                          others => True),
         At_Most_Once => True,
         Internal     => False),
      Opt_Runtime => Create
        (Long_Name    => "--RTS",
         Pattern      => "[RUNTIME]",
         Help         => "When using projects files, state the runtime used"
                         & " to build the analyzed programs. If project files"
                         & " don't already set the runtime, this is required"
                         & " for correct project files processing.",
         Commands     => (Cmd_Print_GPR_Registry
                          | Cmd_Setup_Integration => False,
                          others => True),
         At_Most_Once => True,
         Internal     => False),
      Opt_Config => Create
        (Long_Name    => "--config",
         Pattern      => "[CONFIG-FILE]",
         Help         => "Specify a configuration project file name. If"
                         & " passed, this file must exist and neither --target"
                         & " nor --RTS must be present.",
         Commands     => (Cmd_Print_GPR_Registry => False, others => True),
         At_Most_Once => True,
         Internal     => False),
      Opt_Output => Create
        (Long_Name    => "--output",
         Short_Name   => "-o",
         Pattern      => "[FILE]",
         Help         =>
           "Put the report/asm output/trace file/external annotations into"
           & " FILE.",
         Commands     => (Cmd_Run
                          | Cmd_Coverage
                          | Cmd_Convert
                          | Cmd_Dump_CFG
                          | Cmd_Add_Annotation
                          | Cmd_Delete_Annotation => True,
                          others                  => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Output_Directory => Create
        (Long_Name    => "--output-dir",
         Pattern      => "[SUBDIR]",
         Help         => "Place the output files into SUBDIR.",
         Commands     => (Cmd_Coverage
                          | Cmd_Instrument_Source
                          | Cmd_Instrument_Main
                          | Cmd_Setup_Integration => True, others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Tag => Create
        (Long_Name    => "--tag",
         Short_Name   => "-T",
         Pattern      => "[TAG]",
         Help         => "Tag to put into trace file.",
         Commands     => (Cmd_Run | Cmd_Convert => True, others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Kernel => Create
        (Long_Name    => "--kernel",
         Pattern      => "[KERNEL]",
         Help         => "Specify which kernel to use.",
         Commands     => (Cmd_Run => True, others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Coverage_Level => Create
        (Long_Name    => "--level",
         Short_Name   => "-c",
         Pattern      => "[LEVEL]",
         Help         =>
           ("Specify coverage levels to assess."
            & ASCII.LF
            & "For all commands except ""instrument"", LEVEL is one of:"
            & ASCII.LF
            & "  " & Object_Level_Options (Separator => ", ")
            & " (object coverage)"
            & ASCII.LF
            & "  " & Source_Level_Options
            & " (source coverage)"
            & ASCII.LF
            & "For the ""instrument"" command, "
            & "a source coverage level is required."
            & ASCII.LF
            & "ATC and ATCC are experimental features."
           ),
         Commands     => (Cmd_Run
                          | Cmd_Coverage
                          | Cmd_Convert
                          | Cmd_Instrument_With_Setup => True,
                          others                      => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Text_Start => Create
        (Long_Name    => "--text-start",
         Pattern      => "[HEX_ADDR]",
         Help         => "??? This option was likely introduced for a feature"
                         & " that is not completely implemented.",
         At_Most_Once => False,
         Internal     => True),
      Opt_Exec_Prefix => Create
        (Long_Name    => "--exec-prefix",
         Pattern      => "[PREFIX]",
         Help         => "In cases where we cannot find executable files, look"
                         & " for them in the PREFIX directory.",
         At_Most_Once => False,
         Internal     => True),
      Opt_HW_Trigger_Traces => Create
        (Long_Name    => "--hw-trigger-traces",
         Pattern      => "[START_ID],[START_ADDR],[STOP_ID]",
         Help         => "Identity of start and stop triggers, and address for"
                         & " start.",
         Commands     => (Cmd_Convert => True, others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Input => Create
        (Long_Name    => "--input",
         Pattern      => "[INPUT_TRACE]",
         Help         => "File containing trace data to be converted.",
         Commands     => (Cmd_Convert => True, others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Separate => Create
        (Short_Name   => "-S",
         Pattern      => "[TAG]",
         Help         => "Perform separate source coverage analysis.",
         Commands     => (Cmd_Coverage | Cmd_Instrument => True,
                          others                        => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Output_Format => Create
        (Short_Name   => "-f",
         Pattern      => "[FORMAT]",
         Help         => "If given, call dot(1) to produce the actual output"
                         & " (SVG, PDF, DOT, ...).",
         Commands     => (Cmd_Dump_CFG => True, others => False),
         At_Most_Once => False,
         Internal     => True),
      Opt_Trace_Source => Create
        (Long_Name    => "--trace-source",
         Pattern      => "[SOURCE]",
         Help         => "Specify a trace source.",
         Commands     => (Cmd_Convert => True, others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Save_Checkpoint => Create
        (Long_Name    => "--save-checkpoint",
         Pattern      => "CHECKPOINT",
         Help         => "Save source coverage checkpoint to named file.",
         Commands     => (Cmd_Coverage => True, others => False),
         At_Most_Once => True,
         Internal     => False),
      Opt_Report_Title => Create
        (Long_Name    => "--report-title",
         Pattern      => "TITLE LABEL",
         Help         => "Override the default title for HTML coverage"
                         & " reports.",
         Commands     => (Cmd_Coverage => True, others => False),
         At_Most_Once => False,
         Internal     => False),

      --  --dump-trigger is a special case: It used to only accept a single
      --  value, but it now should also accept a comma separated list of
      --  filenames for the manual case. In order not to break compatibility
      --  with the previous behavior (last --dump-trigger switch present was
      --  the one used), we'll need to split the option string manually.

      Opt_Dump_Trigger => Create
        (Long_Name    => "--dump-trigger",
         Help         => "Select a trigger to dump coverage buffers in the"
                         & " instrumented program."
                         & ASCII.LF & ASCII.LF
                         & """manual"" searches for user-written dump"
                         & " indication in all the project sources (the search"
                         & " is restricted to FILES if specified) and replaces"
                         & " such indications with a call to the subprogram"
                         & " responsible for dumping the coverage buffers. A"
                         & " warning is emitted if no dump indications were"
                         & " found. See the user manual for more information."
                         & ASCII.LF & ASCII.LF
                         & """atexit"" uses the libc's atexit() routine to"
                         & " schedule the dump."
                         & ASCII.LF & ASCII.LF
                         & """main-end"" instructs to append a call to the"
                         & " dump routine at the end of the main subprogram."
                         & ASCII.LF & ASCII.LF
                         & """ravenscar-task-termination"" uses the Ravenscar"
                         & "-specific Ada.Task_Termination to schedule the"
                         & " dump on task termination."
                         & ASCII.LF & ASCII.LF
                         & "Except for ""manual"", these methods inject code"
                         & " in all mains in the project closure to dump"
                         & " coverage buffers for all units of interest in the"
                         & " main closure. The --dump-channel option"
                         & " determines the dump procedure."
                         & ASCII.LF & ASCII.LF & "Only the last occurrence of"
                         & " the switch is taken into account.",
         Commands     => (Cmd_Setup | Cmd_Instrument => True, others => False),
         At_Most_Once => False,
         Pattern      => "manual[,FILES]|atexit|main-end",
         Internal     => False),

      Opt_Dump_Channel => Create
        (Long_Name    => "--dump-channel",
         Help         => "Select a channel to dump coverage buffers in the"
                         & " instrumented program. Note that this option"
                         & " matters only when --dump-trigger is not"
                         & " ""manual""."
                         & ASCII.LF & ASCII.LF
                         & """bin-file"" (the default) uses the"
                         & " GNATcov_RTS.Traces.Output.Write_Trace_File"
                         & " subprogram, from the gnatcov_rts_full.gpr project"
                         & " in order to create a binary trace file. This is"
                         & " the preferred channel for native programs."
                         & ASCII.LF & ASCII.LF
                         & """base64-stdout"" uses the GNATcov_RTS.Traces"
                         & ".Generic_Output.Write_Trace_File_Base64 procedure"
                         & " to dump a base64 trace on the standard output"
                         & " using Ada.Text_IO. This is the preferred channel"
                         & " for non-native programs.",
         Commands     => (Cmd_Setup
                          | Cmd_Instrument_Project
                          | Cmd_Setup_Integration
                          | Cmd_Instrument_Main => True,
                          others                => False),
         At_Most_Once => False,
         Internal     => False,
         Pattern      => "bin-file|base64-stdout"),
      Opt_Dump_Filename_Env_Var => Create
        (Long_Name    => "--dump-filename-env-var",
         Help         => "Valid only when --dump-channel=bin-file. Name of the"
                         & " environment variable which, if set, contains the"
                         & " filename for created source traces.",
         Commands     => (Cmd_Setup
                          | Cmd_Instrument_Project
                          | Cmd_Setup_Integration
                          | Cmd_Instrument_Main => True,
                          others                => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Dump_Filename_Prefix => Create
        (Long_Name    => "--dump-filename-prefix",
         Help         => "Valid only when --dump-channel=bin-file. Select a"
                         & " filename prefix for created source traces.",
         Commands     => (Cmd_Setup
                          | Cmd_Instrument_Project
                          | Cmd_Setup_Integration
                          | Cmd_Instrument_Main
                          | Cmd_Add_Annotation  => True,
                          others                => False),
         At_Most_Once => False,
         Internal     => False),

      Opt_Dump_Filename_Tag => Create
        (Long_Name    => "--dump-filename-tag",
         Help         => "Valid only when --dump-channel=bin-file. Select a"
                         & " filename tag for created source traces.",
         Commands     => (Cmd_Setup
                          | Cmd_Instrument_Project
                          | Cmd_Setup_Integration
                          | Cmd_Instrument_Main => True,
                          others                => False),
         At_Most_Once => False,
         Internal     => False),

      Opt_Ada => Create
        (Long_Name    => "--ada",
         Pattern      => "83|95|05|12|22|1983|1995|2005|2012|2022",
         Help         => "Select a target language version for source"
                         & " instrumentation. This restricts the set of"
                         & " language constructs that can be introduced in "
                         & " instrumented sources. Emit a warning when such a"
                         & " construct is needed. Use Ada 2012 by default as"
                         & " Ada 2022 support is in beta phase.",
         Commands     => (Cmd_Instrument => True, others => False),
         At_Most_Once => False,
         Internal     => False),

      Opt_Dump_Units_To => Create
        (Long_Name    => "--dump-units-to",
         Pattern      => "-|FILE",
         Help         => "Ouput the list of names for all units of interest"
                         & " and, for each unit of interest, the list of"
                         & " source files individually ignored with the"
                         & " ""Ignored_Source_Files"" project attribute or"
                         & " corresponding command-line option." & ASCII.LF
                         & "For ""-"", the option dumps the list to standard"
                         & " output or in an additional dedicated section of a"
                         & " ""report"" result when that annotation format is"
                         & " requested. The list is written to the given FILE"
                         & " otherwise.",
         Commands     => (Cmd_Coverage => True, others => False),
         At_Most_Once => True,
         Internal     => False),

      Opt_Timezone => Create
        (Long_Name    => "--timezone",
         Pattern      => "local|utc",
         Help         => "State the reference timezone to use when displaying"
                         & " dates in reports, either ""local"" or ""utc"". If"
                         & " not specified, default to ""local"".",
         Commands     => (Cmd_Coverage => True, others => False),
         At_Most_Once => True,
         Internal     => False),

      Opt_Prefix => Create
        (Long_Name    => "--prefix",
         Pattern      => "DIR",
         Help         =>
           "Installation prefix for the instrumentation runtime. If not"
           & " provided, install it in the toolchain prefix.",
         Commands     => (Cmd_Setup => True, others => False),
         At_Most_Once => False,
         Internal     => False),

      Opt_RTS_Profile => Create
        (Long_Name    => "--rts-profile",
         Pattern      => "auto|full|embedded",
         Help         =>
           "Profile for the language runtime, which determines what features"
           & " the instrumentation runtime can use."
           & ASCII.LF & ASCII.LF
           & """full"" allows the instrumentation runtime to use all features"
           & " from a standard C runtime: read environment variables, open"
           & " files, get the current time, ..."
           & ASCII.LF & ASCII.LF
           & """embedded"" only allows the instrumentation runtime to print"
           & " bytes to some output."
           & ASCII.LF & ASCII.LF
           & """auto"" lets gnatcov automatically choose between ""full"" and"
           & " ""embedded"" depending on the selected target and RTS.",
         Commands     => (Cmd_Setup => True, others => False),
         At_Most_Once => False,
         Internal     => True),

      Opt_Path_Count_Limit => Create
        (Long_Name    => "--path-count-limit",
         Pattern      => "POSITIVE",
         Help         =>
           "Limit beyond which enumeration of BDD paths stops. If a decision"
           & " has more paths in its BDD than the limit, MC/DC coverage will"
           & " not be assessed for that decision, resulting in violations in"
           & " the report.",
         Commands     => (Cmd_Instrument => True, others => False),
         At_Most_Once => True,
         Internal     => False),

      Opt_Install_Name => Create
        (Long_Name    => "--install-name",
         Pattern      => "NAME",
         Help         =>
           "Name for the installed project. By default, keep the original:"
           & " ""gnatcov_rts"", or the name on the project passed to ""gnatcov"
           & " setup"". Using non-default names allows one to install"
           & " different projects in the same installation prefix.",
         Commands     => (Cmd_Setup | Cmd_Instrument => True, others => False),
         At_Most_Once => False,
         Internal     => False),

      Opt_Runtime_Project => Create
        (Long_Name    => "--runtime-project",
         Pattern      => "NAME",
         Help         =>
           "Name of the installed instrumentation runtime project (see"
           & " ""gnatcov setup""'s ""--install-name"" option). By default, use"
           & " ""gnatcov_rts"".",
         Commands     => (Cmd_Instrument_With_Setup => True, others => False),
         At_Most_Once => False,
         Internal     => False),

      Opt_Lang => Create
        (Long_Name => "--lang",
         Pattern   => "[C|C++|Ada]",
         Help      => "Language for the given compilation unit",
         Commands  => (Cmd_Instrument_Main | Cmd_Instrument_Source => True,
                       others                                      => False),
         At_Most_Once => False,
         Internal  => True),

      Opt_Parallelism_Level => Create
        (Short_Name   => "-j",
         Long_Name    => "--jobs",
         Pattern      => "NATURAL",
         Help         =>
           "Maximal number of concurrently running tasks. Number of processors"
           & " of the machine if set to 0. Defaults to 1.",
         Commands     => (Cmd_Instrument_Project => True, others => False),
         At_Most_Once => False,
         Internal     => False),

      Opt_Compiler_Driver => Create
        (Long_Name    => "--compiler-driver",
         Pattern      => "NAME",
         Help         =>
           "Compiler driver used to compile the source (e.g. gcc). This is"
         & " used when instrumenting a C/C++ source, to retrieve builtin"
         & " macros that may modify the file preprocessing.",
         Commands     =>
           (Cmd_Instrument_Source | Cmd_Instrument_Main => True,
            others                                      => False),
         At_Most_Once => False,
         Internal     => True),

      Opt_Body_Suffix => Create
        (Long_Name    => "--body-suffix",
         Pattern      => "NAME",
         Help         =>
           "Body suffix for source files created by the instrumenter for the"
         & " instrumentation of a source",
         Commands     =>
           (Cmd_Instrument_Source | Cmd_Instrument_Main => True,
            others                                      => False),
         At_Most_Once => False,
         Internal     => True),

      Opt_Spec_Suffix => Create
        (Long_Name    => "--spec-suffix",
         Pattern      => "NAME",
         Help         =>
           "Spec suffix for source files created by the instrumenter for the"
         & " instrumentation of a source",
         Commands     =>
           (Cmd_Instrument_Source | Cmd_Instrument_Main => True,
            others                                      => False),
         At_Most_Once => False,
         Internal     => True),

      Opt_Dot_Replacement => Create
        (Long_Name    => "--dot-replacement",
         Pattern      => "NAME",
         Help         =>
           "Dot replacement for source files created by the instrumenter for"
         & " the instrumentation of a source",
         Commands     =>
           (Cmd_Instrument_Source | Cmd_Instrument_Main => True,
            others                                      => False),
         At_Most_Once => False,
         Internal     => True),

      Opt_Casing => Create
        (Long_Name    => "--casing",
         Pattern      => "CASING",
         Help         =>
           "Casing for source files created by the instrumenter for the"
           & " instrumentation of a source.",
         Commands     =>
           (Cmd_Instrument_Source | Cmd_Instrument_Main => True,
            others                                      => False),
         At_Most_Once => False,
         Internal     => True),

      Opt_Gnatem => Create
        (Long_Name    => "--gnatem",
         Pattern      => "NAME",
         Help         =>
           "Name of the file containing unit dependencies in the GNAT format.",
         Commands     =>
           (Cmd_Instrument_Source | Cmd_Instrument_Main => True,
            others                                      => False),
         At_Most_Once => False,
         Internal     => True),

      Opt_Config_Pragmas_Mapping => Create
        (Long_Name    => "--config-pragmas-mapping",
         Pattern      => "NAME",
         Help         =>
           "Name of the file containing the configuration pragmas mapping.",
         Commands     =>
           (Cmd_Instrument_Source | Cmd_Instrument_Main => True,
            others                                      => False),
         At_Most_Once => False,
         Internal     => True),

      Opt_Ada_Preprocessor_Data => Create
        (Long_Name    => "--ada-preprocessor-data",
         Pattern      => "NAME",
         Help         =>
           "Name of the file containing preprocessor configuration data for"
           & " Ada.",
         Commands     =>
           (Cmd_Instrument_Source | Cmd_Instrument_Main => True,
            others                                      => False),
         At_Most_Once => False,
         Internal     => True),

      Opt_Project_Name => Create
        (Long_Name    => "--project-name",
         Pattern      => "NAME",
         Help         =>
           "Name of the root project, without its gpr extension.",
         Commands     =>
           (Cmd_Instrument_Source | Cmd_Instrument_Main => True,
            others                                      => False),
         At_Most_Once => False,
         Internal     => True),

      Opt_Source_Root => Create
        (Long_Name    => "--source-root",
         Pattern      => "PATH",
         Help         =>
           "Remove the specified prefix from the filenames in the Cobertura"
           & " report (coverage command), or from the generated annotation"
           & " files (add-annotation command).",
         Commands     =>
           (Cmd_Coverage | Cmd_Add_Annotation => True, others => False),
         At_Most_Once => True,
         Internal     => False),

      Opt_Db => Create
        (Long_Name    => "--db",
         Pattern      => "DIR",
         Help         =>
           "Parse DIR as an additional GPR knowledge base directory.",
         Commands     => (Cmd_Print_GPR_Registry
                          | Cmd_Setup_Integration
                          | Cmd_Instrument_Source
                          | Cmd_Instrument_Main => False, others => True),
         At_Most_Once => True,
         Internal     => False),

      Opt_GPR_Registry_Format => Create
        (Long_Name    => "--gpr-registry-format",
         Pattern      => "FORMAT",
         Help         => "Format for the printed GPR registry.",
         Commands     => (Cmd_Print_GPR_Registry => True, others => False),
         At_Most_Once => False,
         Internal     => True),

      Opt_Annotation_Kind => Create
        (Long_Name    => "--kind",
         Pattern      => "KIND",
         Help         =>
           "Kind of annotation, must be one of: " & Annotation_Kind_Options,
         Commands     => (Cmd_Add_Annotation
                          | Cmd_Delete_Annotation
                          | Cmd_Show_Annotations  => True,
                          others                  => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Annotation_Id => Create
        (Long_Name    => "--annotation-id",
         Pattern      => "IDENTIFIER",
         Help         =>
           "Unique identifier for the annotation. Automatically generated if"
           & " not specified",
         Commands     => (Cmd_Add_Annotation | Cmd_Delete_Annotation => True,
                          others                                     => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Location => Create
        (Short_Name   => "-l",
         Long_Name    => "--location",
         Pattern      => "LINE:COL",
         Help         =>
           "Location at which the annotation applies. LINE and COL should be"
           & " positive integers.",
         Commands     => (Cmd_Add_Annotation => True, others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Start_Location => Create
        (Long_Name    => "--start-location",
         Pattern      => "LINE:COL",
         Help         =>
           "Location of the beginning of the range to which the annotation"
           & " applies. LINE and COL should be positive integers.",
         Commands     => (Cmd_Add_Annotation => True, others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_End_Location => Create
        (Long_Name    => "--end-location",
         Pattern      => "LINE:COL",
         Help         =>
           "Location of the end of the range to which the annotation applies."
           & " LINE and COL should be positive integers.",
         Commands     => (Cmd_Add_Annotation => True, others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_Justification => Create
        (Long_Name    => "--justification",
         Pattern      => "TEXT",
         Help         =>
           "Justification for the exemption annotations. Required for the"
           & " ""exempt.on"" and ""exempt.region"" annotation kinds.",
         Commands     => (Cmd_Add_Annotation => True, others => False),
         At_Most_Once => False,
         Internal     => False),
      Opt_SS_Backend => Create
        (Long_Name    => "--ss_backend",
         Pattern      => "NAME",
         Help         =>
           "Specify the Stable_Sloc backend to be used when creating the"
           & " annotation entry",
         Commands     => (Cmd_Add_Annotation => True, others => False),
         At_Most_Once => False,
         Internal     => True),
      Opt_Source_Encoding => Create
        (Long_Name    => "--source-encoding",
         Pattern      => "ENCODING",
         Help         =>
           "Specify the encoding to use to read source files in order to"
           & " produce XML coverage reports.",
         Commands     => (Cmd_Coverage => True, others => False),
         At_Most_Once => False,
         Internal     => False));

   String_List_Infos : constant String_List_Option_Info_Array :=
     (Opt_Log => Create
        (Long_Name               => "--log",
         Pattern                 => "[GNATCOLL_TRACE_NAME]",
         Help                    =>
           "Enable logging for the given GNATCOLL trace name.",
         Internal                => True,
         Accepts_Comma_Separator => True),
      Opt_Projects => Create
        (Long_Name  => "--projects",
         Pattern    => "[GPR|@LISTFILE]",
         Help       => "Focus on specific projects within the transitive"
                       & " closure reachable from the root designated by -P.",
         Commands   => (Cmd_Print_GPR_Registry
                        | Cmd_Setup
                        | Cmd_Setup_Integration
                        | Cmd_Instrument_Source
                        | Cmd_Instrument_Main => False,
                        others                => True),
         Internal   => False),
      Opt_Subp_Of_Interest => Create
        (Long_Name => "--subprograms",
         Pattern   => "FILE:LINE",
         Help      =>
           "Experimental feature: restrict coverage analysis to specific"
           & " subprograms designated by the specification source line for"
           & " Ada, or to specific functions designated by the definition"
           & " source line for C/C++.",
         Commands  => (Cmd_Coverage => True, others => False),
         Internal  => False),
      Opt_Scenario_Var => Create
        (Short_Name => "-X",
         Pattern    => "[NAME]=[VALUE]",
         Help       => "Specify an external reference for Project Files.",
         Commands   => (Cmd_Print_GPR_Registry
                        | Cmd_All_Setups => False,
                        others => True),
         Internal   => False),
      Opt_Cargs => Create
        (Long_Name  => "--cargs",
         Pattern    => "[CARGS ...]",
         Help       => "Pass CARGS arguments to the wrapped compiler.",
         Commands   => (Cmd_Gcc_Wrapper => True, others => False),
         Internal   => True,
         Greedy     => True),
      Opt_Eargs => Create
        (Long_Name  => "-eargs",
         Pattern    => "[EARGS ...]",
         Help       => "For emulated cross targets, pass EARGS to the"
                       & " low-level emulator. For native configurations, pass"
                       & " EARGS as command line arguments to the executed"
                       & " program. The first EARG is picked as the executable"
                       & " program to run if it was not provided explicitly"
                       & " otherwise.",
         Commands   => (Cmd_Run => True, others => False),
         Internal   => False,
         Greedy     => True),
      Opt_Gargs => Create
        (Long_Name  => "-gargs",
         Pattern    => "[GARGS ...]",
         Help       => "Pass GARGS arguments to gprbuild.",
         Commands   => (Cmd_Setup => True, others => False),
         Internal   => True,
         Greedy     => True),
      Opt_Scos => Create
        (Long_Name   => "--scos|--alis",
         Pattern     => "[FILE|@LISTFILE]",
         Help        => "Load SCOs and exemption info from FILE for this"
                        & " operation; or do that for each file listed in"
                        & " LISTFILE.",
         Commands   => (Cmd_Run
                        | Cmd_Convert
                        | Cmd_Coverage
                        | Cmd_Scan_Decisions
                        | Cmd_Map_Routines
                        | Cmd_Dump_CFG => True,
                        others         => False),
         Internal    => False),
      Opt_Units => Create
        (Long_Name   => "--units",
         Pattern     => "[UNIT|@LISTFILE]",
         Help        => "State the set of units of interest by name,"
                        & " overriding the GPR-based selection by -P, etc."
                        & " Supports globbing patterns.",
         Commands    => (Cmd_Run
                         | Cmd_Coverage
                         | Cmd_Scan_Decisions
                         | Cmd_Map_Routines
                         | Cmd_Instrument_Project => True,
                         others                   => False),
         Internal    => False),
      Opt_SID => Create
        (Long_Name   => "--sid",
         Pattern     => "[FILE|@LISTFILE]",
         Help        => "Load Source Instrumentation Data from FILE for this"
                        & " operation; or do that for each file listed in"
                        & " LISTFILE.",
         Commands    => (Cmd_Coverage | Cmd_Instrument_Source => True,
                         others                               => False),
         Internal    => False),
      Opt_Routines => Create
        (Long_Name   => "--routines",
         Pattern     => "[ROUTINE|@LISTFILE]",
         Help        => "Add ROUTINE, or all routines listed in LISTFILE to"
                        & " the list of routines.",
         Commands    => (Cmd_Run
                         | Cmd_Coverage
                         | Cmd_Scan_Decisions
                         | Cmd_Map_Routines => True,
                         others             => False),
         Internal    => False),
      Opt_Routines_List => Create
        (Long_Name   => "--routine-list",
         Short_Name  => "-l",
         Pattern     => "[LISTFILE] (DEPRECATED)",
         Help        => "Add all routines listed in LISTFILE to the list of"
                        & " routines.",
         Commands    => (Cmd_Run | Cmd_Coverage => True, others => False),
         Internal    => False),
      Opt_Exec => Create
        (Long_Name    => "--exec",
         Pattern      => "[EXE]",
         Help         => "For ""coverage"", use E as the base executable for"
                         & " the traces that follow on the command line. For"
                         & " ""convert"", use E as the base executable for"
                         & " trace conversion.",
         Commands     => (Cmd_Convert | Cmd_Coverage => True, others => False),
         Internal     => True),
      Opt_Source_Rebase => Create
        (Long_Name   => "--source-rebase",
         Pattern     => "[OLD_PREFIX]=[NEW_PREFIX]|@[LISTFILE]",
         Help        => "Specify alternative path to find source files."
                        & " [OLD_PREFIX] accepts globbing patterns. Each line"
                        & " in LISTFILE shall be of the form [OLD_PREFIX]="
                        & "[NEW_PREFIX]. Globbing patterns can also be used in"
                        & " [OLD_PREFIX] in a response file.",
         Commands    => (Cmd_Coverage => True, others => False),
         Internal    => False),
      Opt_Source_Search => Create
        (Long_Name   => "--source-search",
         Pattern     => "[PREFIX]|@[LISTFILE]",
         Help        => "Specify an alternative prefix to find source files,"
                        & " or add each line of LISTFILE as an alternative"
                        & " prefix.",
         Commands    => (Cmd_Coverage => True, others => False),
         Internal    => False),

      --  Opt_Trace will receive both trace files arguments, but also
      --  executable names to force the association between trace files and
      --  programs. In order to distinguish both, executable names are prefixed
      --  with ASCII.NUL.

      Opt_Trace => Create
        (Long_Name   => "--trace",
         Short_Name  => "-T",
         Pattern     => "[TRACE|@LISTFILE]",
         Help        =>
           "Specify trace files to read. If the path ends with a directory "
           & " separator, consider it is a directory path containing the list"
           & " of traces to be read.",
         Commands    => (Cmd_Coverage
                         | Cmd_Dump_Trace
                         | Cmd_Dump_Trace_Raw
                         | Cmd_Dump_Trace_Base
                         | Cmd_Dump_Trace_Asm
                         | Cmd_Dump_CFG => True,
                         others         => False),
         Internal    => False),

      Opt_Checkpoint => Create
        (Long_Name   => "--checkpoint",
         Short_Name  => "-C",
         Pattern     => "[CHECKPOINT|@LISTFILE]",
         Help        => "Specify checkpointed coverage information to load.",
         Commands    => (Cmd_Coverage => True, others => False),
         Internal    => False),
      Opt_Ignore_Source_Files => Create
        (Long_Name => "--ignore-source-files",
         Pattern   => "[FILE|@LISTFILE]",
         Help      => "Specify a list of source files to ignore for coverage."
                      & " Supports globbing patterns.",
         Commands  => (Cmd_Coverage
                       | Cmd_Map_Routines
                       | Cmd_Instrument_Project => True,
                       others                   => False),
         Internal  => False),

      Opt_Files => Create
        (Long_Name => "--files",
         Pattern   => "[FILE|@LISTFILE]",
         Help      => "Specify a list of source files of interest by their"
                      & " full name.",
         Commands => (Cmd_Instrument_Source
                      | Cmd_Instrument_Main
                      | Cmd_Setup_Integration => True,
                      others                 => False),
         Internal => False),

      Opt_Shared_Object => Create
        (Long_Name   => "--shared-object",
         Short_Name  => "-L",
         Pattern     => "[none|all|SHARED_OBJECT|@LISTFILE]",
         Help        => "Specify the set of shared object files that will"
                        & " contribute to the code coverage assessment. The"
                        & " following special values are supported:"
                        & ASCII.LF & ASCII.LF
                        & "  * ""none"" (the default), which prevents all"
                        & " shared objects from contributing to coverage;"
                        & ASCII.LF & ASCII.LF
                        & "  * ""all"", which makes all shared objects"
                        & " contribute to coverage."
                        & ASCII.LF & ASCII.LF
                        & "Note that if one such special value is passed,"
                        & " it must be the only value passed to this"
                        & " option.",
         Commands    => (Cmd_Run => True, others => False),
         Internal    => False),

      Opt_Restricted_To_Languages => Create
        (Long_Name               => "--restricted-to-languages",
         Pattern                 => "[LANGUAGE|LIST|@LISTFILE]",
         Help                    =>
           "Restrict the set of languages for units of interest. Supports Ada,"
           & " C and C++. Note that main units of other languages may still be"
           & " instrumented to dump the coverage state to trace files.",
         Commands                => (Cmd_Setup
                                     | Cmd_Instrument_Project
                                     | Cmd_Coverage => True,
                                     others                   => False),
         Internal                => False,
         Accepts_Comma_Separator => True),
      Opt_Annotation_Format => Create
        (Long_Name               => "--annotate",
         Short_Name              => "-a",
         Pattern                 => "[FORM|LIST]",
         Help                    =>
           "Generate a FORM report. FORM is one of:"
           & ASCII.LF
           & "  asm, cobertura, html, report, sarif, xcov, xcov+, xml."
           & ASCII.LF & "Multiple reports can be produced in a single"
           & " execution by passing a comma separated list of FORMs to this"
           & " option, or by specifying this option multiple times on the"
           & " command line.",
         Commands                => (Cmd_Coverage => True, others => False),
         Internal                => False,
         Accepts_Comma_Separator => True),

        Opt_C_Opts => Create
          (Long_Name => "--c-opts",
           Pattern   => "[COMMA-SEPARATED-OPTIONS]",
           Help      =>
             "List of additional compiler switches to analayze C source"
             & " files.",
           Commands  => (Cmd_Instrument => True, others => False),
           Internal  => False),

        Opt_CPP_Opts => Create
          (Long_Name    => "--c++-opts",
           Pattern      => "[COMMA-SEPARATED-OPTIONS]",
           Help         =>
             "List of additional compiler switches to analayze C++ source"
             & " files.",
           Commands     => (Cmd_Instrument => True, others => False),
           Internal     => False),

      Opt_Runtime_Dir => Create
        (Long_Name    => "--runtime-dir",
         Pattern      => "NAME",
         Help         =>
           "Directories containing the compiler runtime sources.",
         Commands     =>
           (Cmd_Instrument_Source | Cmd_Instrument_Main => True,
            others                                      => False),
         Internal     => True),

      Opt_Compiler_Wrappers => Create
        (Long_Name => "--compilers",
         Pattern   => "NAME",
         Help      =>
           "List of compiler drivers for which we should generate wrappers."
           & " Supported compilers are: gcc, g++.",
         Commands  => (Cmd_Print_GPR_Registry => False, others => True),
         Internal  => True),

      Opt_Ext_Annotations => Create
        (Long_Name  => "--external-annotations",
         Help       =>
           "Specify external annotation files. For annotation commands that"
           & " modify the set of external annotations (add-annotation,"
           & " delete-annotation), all the annotations from all files"
           & " (after addition / deletion) are written back to the file passed"
           & " to --output, effectively combining the input annotation files.",
         Commands   =>
           (Cmd_Instrument
            | Cmd_Coverage
            | Cmd_All_Annotate => True,
            others             => False),
         Pattern    => "FILENAME|@LISTFILE",
         Internal   => False));

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
