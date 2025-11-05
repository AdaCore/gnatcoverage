------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Directories;
with Ada.Exceptions;

with GNAT.Strings; use GNAT.Strings;
with GPR2;

with Calendar_Utils; use Calendar_Utils;
with Command_Line;   use Command_Line;
with Files_Handling; use Files_Handling;
with Logging;
with SC_Obligations;
with Strings;        use Strings;

package Switches is

   Misc_Trace : constant Logging.GNATCOLL_Trace :=
     Logging.Create_Trace ("MISC");
   --  Trace to log all messages for which creating a dedicated trace was not
   --  worth it.

   function Exception_Info
     (Exc          : Ada.Exceptions.Exception_Occurrence;
      Discard_Name : Boolean := False) return String;
   --  Helper to display the right amount of exception information depending on
   --  enabled traces.
   --
   --  If Discard_Name is True, Exc has an exception message and traces are not
   --  enabled, do not include the exception name in the error message. Passing
   --  True for this is appropriate when formatting a user-level error message
   --  (exception names make the error message look like a crash).

   procedure Parse_Arguments (From_Driver : Boolean);
   --  Load arguments from command-line and from the project file (if any) into
   --  Args (see below). Print usage and exit if there is no argument.
   --
   --  If From_Driver is True, do not compute the lists of projects/units of
   --  interest from project files. This is meant to be used only in the
   --  gnatcov driver, where we just need to determine the target.

   Arg_Parser : constant Command_Line.Parser.Parser_Type :=
     Command_Line.Create;
   --  Parser for command-line arguments

   Args : Command_Line.Parser.Parsed_Arguments;
   --  Results of the arguments parsing (first only command-line, then also
   --  arguments from project file).

   --  All the variables below are used to store results of the command line
   --  parsing and analysis.

   procedure Copy_Arg (Option : String_Options; Variable : out String_Access);
   --  Copy the string for Option into Variable. If the option is present, this
   --  allocates a new string in Variable.

   function Expand_Argument (Argument : String) return String_Vectors.Vector;
   --  If Argument starts with a '@' character, consider that it is a response
   --  file: read it and return its contents as a vector, assuming the
   --  following format:
   --
   --  * lines starting with '#' are ignored,
   --  * lines that start with '@@' are included with the leading '@' removed,
   --  * the other lines that start with '@' are treated as filenames and
   --    processed recursively,
   --  * one item per line,
   --  * no blanks allowed.
   --
   --  Otherwise, just return a 1-element vector to contain Argument.

   procedure Append_Expanded_Argument
     (Argument : String; List : in out String_Vectors.Vector);
   --  Expand Argument and append the resulting arguments to List

   procedure Copy_Arg_List
     (Option : String_List_Options; List : in out String_Vectors.Vector);
   procedure Copy_Arg_List
     (Args : String_Vectors.Vector; List : in out String_Vectors.Vector);
   --  Copy the list of strings referenced in Option to the List input list,
   --  expanding arguments that start with '@' according to rules described in
   --  Inputs.Expand_Argument.

   procedure Process_File_Or_Dir_Switch
     (Args              : String_Vectors.Vector;
      Orig_Switch       : String;
      Process_Dir_Entry :
        access procedure (Dir : Ada.Directories.Directory_Entry_Type);
      Process_Arg       : access procedure (Exp_Arg : String);
      Pattern           : String := "");
   --  This procedure is dedicated to switches accepting a list of files, e.g.
   --  --trace, which are either to be passed as an explicit list of files /
   --  response file, or as a directory (thus ending with a directory
   --  separator) containing a list of files.
   --
   --  Args is the list of arguments, Orig_Switch is the name of the switch
   --  for proper error messaging. Process_Dir_Entry processes a directory
   --  entry belonging to one of the directory arguments. Process_Arg processes
   --  a file explicitly passed on the command line / response file (expanded).
   --  If not empty, Pattern is the file pattern to look for in directory
   --  arguments.

   ----------------------------
   -- Miscellaneous switches --
   ----------------------------

   Warnings_As_Errors : Boolean := False;
   --  Whether to treat the warnings that gnatcov emits as errors, i.e. whether
   --  gnatcov must exit with a non-zero status code if it emits warning.

   Quiet : Boolean := False;
   --  Whenther the "--quiet/-q" command line switch is active.
   --
   --  When it is, do not display information about progress on the standard
   --  output.  Intended output, such as the "report" coverage output when no
   --  "--output/-o" argument is passed is still emitted in this case.

   All_Decisions : Boolean := False;
   --  If True, perform decision coverage in stmt+decision mode even for
   --  decisions outside of control structures.

   All_Messages : Boolean := False;
   --  If True, then when performing source coverage analysis, also include in
   --  the report messages other than violations of a source coverage
   --  obligation.

   Recursive_Projects : Boolean := False;
   --  When a project file is specified using -P, also consider all imported
   --  projects for coverage.

   Excluded_SCOs : Boolean := False;
   --  If True, report SCOs whose coverage cannot be established due to
   --  absence of executable code.

   Dump_Units : Boolean := False;
   --  If True, try to dump the list of names for units of interest. This
   --  corresponds to the --dump-units-to command-line option.

   Show_MCDC_Vectors : Boolean := False;
   --  If True, show the evaluation vectors for each decision where there is
   --  an MCDC violation. The "--show-mcdc-vectors" switch is now deprecated.
   --  Displaying the evaluation vectors is possible for assertion coverage,
   --  in which case keeping "mcdc" in the name can be missleading. This switch
   --  is replaced by "--show-condition-vectors" that behaves the same way for
   --  both MCDC and ATCC.

   Show_Condition_Vectors : Boolean := False;
   --  If True, show the evaluation vectors for each decision where there is
   --  an MCDC or ATCC violation.

   Timezone : Any_Timezone := Local_Time;
   --  Control the date display format (either in local time, or UTC time)

   Pretty_Print : Boolean := False;
   --  If true, run "gnatformat" on the generate sources

   Allow_Mixing_Trace_Kinds : Boolean := False;
   --  If true, mixing trace kinds (binary and source) will not result in an
   --  error but only output a warning.

   Short_Circuit_And_Or : Boolean := False;
   --  If True, consider that standard boolean operators "and" and "or" have
   --  short-circuit semantics and instrument the operands of these operators
   --  as conditions.

   Save_Temps : Boolean := False;
   --  When True, do not remove temporary files and directories

   SPARK_Compat : Boolean := False;
   --  When True, tune the instrumenter for maximum SPARK compatibility

   Files_Of_Interest : File_Sets.Set;
   --  Lower abstraction for files of interest, when the --files switch is
   --  used.

   Use_Full_Slugs : Boolean := False;
   --  When True, use full unit/filename slugs for generated buffer units
   --  instead of hashes.

   Subps_Of_Interest : SC_Obligations.Scope_Id_Set;
   --  List of subprograms of interest requested by users on the command line

   Instrument_Block : Boolean := False;
   --  When True, instrument statement blocks instead of statements

   Force : Boolean := False;
   --  When True, overwrite annotations of the same kind and same location, or
   --  of the same identifier if specified on the command line. Only relevant
   --  for the annotate command.

   Split_Extracted_Traces : Boolean := False;
   --  When True, each trace decoded by extract-base64-traces will be written
   --  in separate files instead of overwritting them in the same file.

   type Separated_Source_Coverage_Type is (None, Routines, Instances);
   Separated_Source_Coverage : Separated_Source_Coverage_Type := None;

   Units_Inputs : String_Vectors.Vector;
   --  List of names for requested units of interest

   C_Opts   : String_Vectors.Vector;
   CPP_Opts : String_Vectors.Vector;

   type Any_Language is
     (All_Languages, Ada_Language, C_Language, CPP_Language);
   --  A language that is supported for source coverage All_Languages is a
   --  special value to designate all languages at once.

   subtype Some_Language is Any_Language range Ada_Language .. CPP_Language;
   subtype C_Family_Language is Any_Language range C_Language .. CPP_Language;

   function To_Language (Name : String) return Some_Language;
   --  Convert a human-readable language name to the corresponding enumeration
   --  value. Abort with a fatal error if Name is invalid.

   function To_Language (Id : GPR2.Language_Id) return Some_Language;
   --  Convert a GPR2 lanugage ID to our enumeration value. Abort with a fatal
   --  error if Name is invalid.

   function To_Language_Or_All (Name : String) return Any_Language;
   --  Like To_Language, but return All_Languages if Name is invalid

   function To_Language_Or_All (Id : GPR2.Language_Id) return Any_Language;
   --  Like To_Language, but return All_Languages if Name is invalid

   function Image (Language : Some_Language) return String;
   --  Return a human-readable name for the given language

   function To_Language_Id (Language : Some_Language) return GPR2.Language_Id;
   --   Convert our enumeration value for languages to the corresponding GPR2
   --   language ID.

   subtype Bin_Supported_Language is
     Some_Language range Ada_Language .. C_Language;
   subtype Src_Supported_Language is
     Some_Language range Ada_Language .. CPP_Language;
   --  A language that is supported for source coverage in binary (respectively
   --  source) trace mode.

   Src_Enabled_Languages : array (Src_Supported_Language) of Boolean :=
     (others => False);
   --  List of languages for which coverage analysis is enabled. Initialized
   --  during command line arguments parsing.

   Builtin_Support : array (Src_Supported_Language) of Boolean :=
     (others => True);
   --  Whether gnatcov supports the given language. Used when building gnatcov
   --  without C instrumentation support.

   ------------------------
   -- Target information --
   ------------------------

   Target_Family : String_Access := null;
   --  Name of the target platform for analyzed programs

   Target_Board : String_Access := null;
   --  Name of the board on which analyzed programs run

   -------------------------------------------------------------
   -- Project related switches that may need to be propagated --
   -------------------------------------------------------------

   package Key_Element_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => String);

   S_Variables : Key_Element_Maps.Map;
   --  All defined scenario variables, as provided through -X options on
   --  the command line.

   ---------------------------------------
   --  Instrumentation-related switches --
   ---------------------------------------

   type Any_Language_Version is
     (Ada_1983, Ada_1995, Ada_2005, Ada_2012, Ada_2022);

   function Set_Language_Version
     (V : in out Any_Language_Version; From : String) return Boolean;
   --  Try to find a language version substring in From. This will search for
   --  the presence of any of the years defined in Any_Language_Version. If one
   --  is found, update V accordingly and return True; return False otherwise.

   type Any_Dump_Trigger is
     (Manual, At_Exit, Ravenscar_Task_Termination, Main_End, None);
   --  Trigger to dump coverage buffers in instrumented programs. See the user
   --  documentation for the --dump-trigger command-line option.

   subtype Auto_Dump_Trigger is Any_Dump_Trigger range At_Exit .. None;
   subtype Valid_Auto_Dump_Trigger is
     Auto_Dump_Trigger range At_Exit .. Main_End;
   subtype Valid_Dump_Trigger is Any_Dump_Trigger range Manual .. Main_End;

   type Any_Dump_Channel is (Binary_File, Base64_Standard_Output);
   --  Channel where to dump coverage buffers. See the user documentation for
   --  the --dump-channel command-line option.

   --  Serialization/deserialization functions for the enumeration types. The
   --  deserialization ones raise Constraint_Error exceptions for invalid input
   --  strings.

   function Image (Dump_Trigger : Valid_Dump_Trigger) return String;
   function Image (Dump_Channel : Any_Dump_Channel) return String;
   function Value (Dump_Trigger : String) return Valid_Dump_Trigger;
   function Value (Dump_Channel : String) return Any_Dump_Channel;

   type Any_Dump_Config
     (Channel : Any_Dump_Channel := Any_Dump_Channel'First)
   is record

      Auto_Trigger   : Auto_Dump_Trigger := None;
      Manual_Trigger : Boolean := False;

      Manual_Indication_Files : File_Sets.Set;
      --  Files containing manual indications as specified per the user
      --  (optional).

      case Channel is
         when Binary_File =>
            Filename_Simple : Boolean := False;
            --  Whether to generate source traces with simple filenames.
            --
            --  Controlled by --dump-filename-simple.

            Filename_Env_Var : Unbounded_String;
            --  Name of the environment variable which, if set, contains the
            --  default filename for created source traces. If empty, use the
            --  default one (see Default_Trace_Filename_Env_Var in
            --  GNATcov_RTS.Traces.Output.Files).
            --
            --  Controlled by --dump-filename-env-var.

            Filename_Prefix : Unbounded_String;
            --  Prefix for the source trace filename. If empty, use the
            --  program's basename (see Default_Trace_Filename_Prefix in
            --  GNATcov_RTS.Traces.Output.Files).
            --
            --  Controlled by --dump-filename-prefix.

         when others =>
            null;
      end case;
   end record;

   function Load_Dump_Config
     (Default_Dump_Config : Any_Dump_Config) return Any_Dump_Config;
   --  Create the Any_Dump_Config value corresponding to Default_Dump_Config
   --  and the given --dump-* arguments for source trace dumping.

   function Unparse_Config
     (Dump_Config : Any_Dump_Config) return String_Vectors.Vector;
   --  Return a suitable set of gnatcov command line switches that represent
   --  the given dump config.

   function Common_Switches
     (Cmd : Command_Line.Command_Type) return String_Vectors.Vector;
   --  Return the unparsed command line arguments supported by the given
   --  Cmd. This is used to propagate a set of switches to a gnatcov
   --  subprocess.

   Global_Language_Version : Any_Language_Version;
   --  Language version to be used in case no language version pragma is
   --  present in the source being instrumented.

end Switches;
