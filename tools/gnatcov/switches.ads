------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2022, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Maps;

with GNAT.Strings; use GNAT.Strings;

with Calendar_Utils;       use Calendar_Utils;
with Command_Line;         use Command_Line;
with Command_Line_Support; use Command_Line_Support;
with Inputs;
with Instrument;           use Instrument;

package Switches is

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

   procedure Copy_Arg
     (Option   : String_Options;
      Variable : out String_Access);
   --  Copy the string for Option into Variable. If the option is present, this
   --  allocates a new string in Variable.

   procedure Copy_Arg_List
     (Option : String_List_Options;
      List   : in out Inputs.Inputs_Type);
   --  Copy the list of strings referenced in Option to the List input list

   function Load_Dump_Config
     (Default_Dump_Config : Any_Dump_Config) return Any_Dump_Config;
   --  Create the Any_Dump_Config value corresponding to Default_Dump_Config
   --  and the given --dump-* arguments for source trace dumping.

   ----------------------------
   -- Miscellaneous switches --
   ----------------------------

   Verbose : Boolean := False;
   --  Verbose informational output

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

   Branch_Stats : Boolean := False;
   --  If True, dump statistics about branch instructions after the static
   --  analysis pass.

   Excluded_SCOs : Boolean := False;
   --  If True, report SCOs whose coverage cannot be established due to
   --  absence of executable code.

   Dump_Units : Boolean := False;
   --  If True, try to dump the list of names for units of interest. This
   --  corresponds to the --dump-units-to command-line option.

   Show_MCDC_Vectors : Boolean := False;
   --  If True, show the evaluation vectors for each decision where there is
   --  an MCDC violation.

   Timezone : Any_Timezone := Local_Time;
   --  Control the date display format (either in local time, or UTC time)

   Pretty_Print : Boolean := False;
   --  If true, run "gnatpp" on the generate sources

   Allow_Mixing_Trace_Kinds : Boolean := False;
   --  If true, mixing trace kinds (binary and source) will not result in an
   --  error but only output a warning.

   Analyze_Entry_Barriers : Boolean := False;
   --  If True, instrument and do not discard SCOs from entry barriers

   Short_Circuit_And_Or : Boolean := False;
   --  If True, consider that standard boolean operators "and" and "or" have
   --  short-circuit semantics and instrument the operands of these operators
   --  as conditions.

   Save_Temps : Boolean := False;
   --  When True, do not remove temporary files and directories

   SPARK_Compat : Boolean := False;
   --  When True, tune the instrumenter for maximum SPARK compatibility

   type Separated_Source_Coverage_Type is (None, Routines, Instances);
   Separated_Source_Coverage : Separated_Source_Coverage_Type := None;

   Units_Inputs : Inputs.Inputs_Type;
   --  List of names for requested units of interest

   type Any_Language is
     (All_Languages, Ada_Language, C_Language, CPP_Language);
   --  A language that is supported for source coverage All_Languages is a
   --  special value to designate all languages at once.

   subtype Some_Language is Any_Language range Ada_Language .. CPP_Language;
   subtype C_Family_Language is Any_Language range C_Language .. CPP_Language;

   function To_Language (Name : String) return Some_Language;
   --  Convert a human-readable language name to the corresponding enumeration
   --  value. Abort with a fatal error if Name is invalid.

   function Image (Language : Some_Language) return String;
   --  Return a human-readable name for the given language

   subtype Bin_Supported_Language is
     Some_Language range Ada_Language .. C_Language;
   subtype Src_Supported_Language is
     Some_Language range Ada_Language .. CPP_Language;
   --  A language that is supported for source coverage in binary (respectively
   --  source) trace mode.

   Src_Enabled_Languages : array (Src_Supported_Language) of Boolean :=
     (others => False);
   --  List of languages for which source files should be instrumented.
   --  Initialized during command line arguments parsing.

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

   Root_Project : String_Access := null;
   --  Project name as specified to the -P option of the command line

   Runtime : String_Access := null;
   --  Name of the runtime (--RTS or defined in the project file)

   CGPR_File : String_Access := null;
   --  Name of the configuration project file

   package Key_Element_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => String);

   S_Variables : Key_Element_Maps.Map;
   --  All defined scenario variables, as provided through -X options on
   --  the command line.

   ------------------------------
   -- Debugging switches (-d?) --
   ------------------------------

   Debug_Switches : array (Valid_Debug_Type) of Boolean := (others => False);
   --  For each debug switches, tell whether it's enabled

   --  Convenience shortcuts:

   Debug_Break_Long_Instructions : Boolean renames
      Debug_Switches (Break_Long_Instructions);

   Debug_Full_History : Boolean renames
      Debug_Switches (Full_History);

   Debug_Ignore_Exemptions : Boolean renames
      Debug_Switches (Ignore_Exemptions);

   Debug_File_Table : Boolean renames
      Debug_Switches (File_Table);

end Switches;
