------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2021, AdaCore                     --
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

with Ada.Containers;        use Ada.Containers;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.Regexp;
with GNAT.Strings; use GNAT.Strings;

with ALI_Files;
with Annotations;           use Annotations;
with Annotations.Dynamic_Html;
with Annotations.Html;
with Annotations.Xcov;
with Annotations.Xml;
with Annotations.Report;
with Calendar_Utils;
with CFG_Dump;
with Check_SCOs;
with Checkpoints;
with Command_Line;          use Command_Line;
use Command_Line.Parser;
with Command_Line_Support;
with Convert;
with Coverage;              use Coverage;
with Coverage.Source;       use Coverage.Source;
with Coverage.Tags;         use Coverage.Tags;
with Decision_Map;          use Decision_Map;
with Disassemble_Insn_Properties;
with Binary_Files;
with Execs_Dbase;           use Execs_Dbase;
with Files_Table;           use Files_Table;
with Inputs;                use Inputs;
with Instrument;
with Instrument.Input_Traces;
with Object_Locations;
with Outputs;               use Outputs;
with Perf_Counters;
with Project;               use Project;
with Qemu_Traces;
with Rundrv;                use Rundrv;
with SC_Obligations;        use SC_Obligations;
with Snames;
with Strings;               use Strings;
with Switches;              use Switches;
with Traces;                use Traces;
with Traces_Elf;            use Traces_Elf;
with Traces_Files_Registry; use Traces_Files_Registry;
with Traces_Names;          use Traces_Names;
with Traces_Dump;
with Traces_Files;          use Traces_Files;
with Traces_Dbase;          use Traces_Dbase;
with Traces_Disa;
with Version;

with GNATcov_RTS.Traces;

procedure GNATcov_Bits_Specific is

   --  This is the main of the bits-specific gnatcov program. Only the gnatcov
   --  driver (see gnatcov.adb) is supposed to run this main.

   --  Results of the command line processing. It is filled by
   --  Process_Arguments once Switches.Args reached its final state.

   Annotation           : Annotation_Format renames Annotations.Annotation;
   Trace_Inputs         : Inputs.Inputs_Type;
   Exe_Inputs           : Inputs.Inputs_Type;
   Obj_Inputs           : Inputs.Inputs_Type;
   ALIs_Inputs          : Inputs.Inputs_Type;
   Routines_Inputs      : Inputs.Inputs_Type;
   Checkpoints_Inputs   : Inputs.Inputs_Type;
   SID_Inputs           : Inputs.Inputs_Type;
   Ignored_Source_Files : Inputs.Inputs_Type;
   Source_Rebase_Inputs : Inputs.Inputs_Type;
   Source_Search_Inputs : Inputs.Inputs_Type;
   Text_Start           : Pc_Type := 0;
   Output               : String_Access := null;
   Tag                  : String_Access := null;
   Kernel               : String_Access := null;
   Save_Checkpoint      : String_Access := null;
   Eargs                : String_Vectors.Vector;
   Executable_Path      : String_Access := null;
   Locations_Inputs     : Object_Locations.User_Locations;
   CFG_Output_Format    : CFG_Dump.Output_Format := CFG_Dump.None;
   Keep_Edges           : Boolean := False;
   SO_Inputs            : SO_Set_Type;
   Keep_Reading_Traces  : Boolean := False;
   Emit_Report          : Boolean := True;

   Dump_Units_Filename  : String_Access := null;
   --  If null, dump the list of units of interest as a section in the report
   --  annotation format (if "-a report") or on the standard output
   --  (otherwise). When non-null, dump the list of units to the designated
   --  file.

   SCOs_Loaded : Boolean := False;
   --  Whether we loaded SCOs from ALI files

   SIDs_Loaded : Boolean := False;
   --  Whether we loaded SID files for units of interest

   procedure Process_Arguments;
   --  Process all the arguments present in Args and forward them to local
   --  varibles.

   procedure Show_Version;
   --  Show gnatcov version

   procedure Show_CWD;
   --  Show the current working directory

   procedure Report_Missing_Argument
     (What            : String;
      Additional_Info : String := "");
   --  Report a fatal error telling the user that an argument is missing.

   procedure Check_Argument_Available
     (Input_Args      : Inputs.Inputs_Type;
      What            : String;
      Additional_Info : String := "");
   --  Invoke Report_Missing_Argument if Input_Args is empty

   procedure Check_User_Provided_SCOs;
   --  If source coverage is enabled, report an error if no SCOs are provided.
   --  Do nothing in object coverage mode.

   procedure Load_All_ALIs (Check_SCOs : Boolean);
   --  Load all listed ALI files and initialize source coverage data structure
   --  (if appropriate). If Check_SCOs is True, call Check_User_Provided_SCOs.
   --
   --  If no ALI Files are specified, try to enumerate them from a given
   --  project file.

   procedure Load_All_SIDs;
   --  Load all SID files for units of interest (if no --sid option is passed)
   --  or all explicitly passed SID files.

   procedure Report_Bad_Trace (Trace_Filename : String; Result : Read_Result)
      with Pre => not Result.Success;
   --  Emit the error corresponding to Result with Outputs. If
   --  Keep_Reading_Tracess is false, this is a fatal error.

   -----------------------------
   -- Report_Missing_Argument --
   -----------------------------

   procedure Report_Missing_Argument
     (What            : String;
      Additional_Info : String := "")
   is
   begin
      Fatal_Error_With_Usage
        ("Please specify " & What & " on the command line" & Additional_Info
         & ".");
   end Report_Missing_Argument;

   ------------------------------
   -- Check_Argument_Available --
   ------------------------------

   procedure Check_Argument_Available
     (Input_Args      : Inputs.Inputs_Type;
      What            : String;
      Additional_Info : String := "")
   is
   begin
      if Inputs.Length (Input_Args) = 0 then
         Report_Missing_Argument (What, Additional_Info);
      end if;
   end Check_Argument_Available;

   ------------------------------
   -- Check_User_Provided_SCOs --
   ------------------------------

   procedure Check_User_Provided_SCOs is
   begin
      --  As long as the user requested source coverage, they need to provide
      --  a set of units for which we will compute code coverage. Passing a
      --  project file, checkpoints, SID or ALI files are all ways to convey
      --  that. If the user provided no such set of units, we cannot compute
      --  anything useful, so we need to ask for that and stop.

      if Source_Coverage_Enabled
         and then Inputs.Length (Checkpoints_Inputs) = 0
         and then Inputs.Length (SID_Inputs) = 0
         and then Inputs.Length (ALIs_Inputs) = 0
         and then not Args.String_Args (Opt_Project).Present
      then
         Report_Missing_Argument
           ("SCOs",
            ", specifying Units in project or using --units/--scos/--sid");
      end if;
   end Check_User_Provided_SCOs;

   -------------------
   -- Load_All_ALIs --
   -------------------

   procedure Load_All_ALIs (Check_SCOs : Boolean) is
      Matcher     : aliased GNAT.Regexp.Regexp;
      Has_Matcher : Boolean;

      procedure Add_LI (S : String);
      --  Callback to add items to ALIs_Inputs

      procedure Load_SCOs_Wrapper (ALI_Filename : String);
      --  Wrapper for SC_Obligations.Load_SCOs that uses Ignored to ignore
      --  source files.

      ------------
      -- Add_LI --
      ------------

      procedure Add_LI (S : String) is
      begin
         Inputs.Add_Input (ALIs_Inputs, S);
      end Add_LI;

      -----------------------
      -- Load_SCOs_Wrapper --
      -----------------------

      procedure Load_SCOs_Wrapper (ALI_Filename : String) is
      begin
         Load_SCOs (ALI_Filename, (if Has_Matcher
                                   then Matcher'Access
                                   else null));
      end Load_SCOs_Wrapper;

   --  Start of processing for Load_All_ALIs

   begin
      if Check_SCOs then
         Check_User_Provided_SCOs;
      end if;

      --  Do not load SCOs more than once

      if SCOs_Loaded then
         return;
      end if;
      SCOs_Loaded := True;

      --  Set defaults for options identifying the entities of interest
      --  coverage analysis if they have not been identified on the command
      --  line.
      --
      --  Load_All_ALIs is called after filling the ALI_Inputs container from
      --  command line options, so if it is empty at that point, it means SCOs
      --  have to be enumerated from a project file.

      if Inputs.Length (ALIs_Inputs) = 0 and then Is_Project_Loaded then
         Enumerate_LIs (Add_LI'Access);
      end if;

      --  When appropriate, warn about units of interest with no LI

      if Is_Project_Loaded and then LIs_Enumerated then
         Report_Units_Without_LI;
      end if;

      if Source_Coverage_Enabled then

         --  Load SCOs from ALI files and initialize source coverage data
         --  structures.

         Create_Matcher (Ignored_Source_Files, Matcher, Has_Matcher);
         Inputs.Iterate (ALIs_Inputs, Load_SCOs_Wrapper'Access);
         Coverage.Source.Initialize_SCI;

      elsif Object_Coverage_Enabled then

         --  For object coverage, just load ALIs (not SCOs inside them) just to
         --  get exemptions as they apply to instruction/branch coverage.

         Inputs.Iterate (ALIs_Inputs, ALI_Files.Load_ALI'Access);
      end if;
   end Load_All_ALIs;

   -------------------
   -- Load_All_SIDs --
   -------------------

   procedure Load_All_SIDs is

      Has_Matcher : Boolean;
      Matcher     : aliased GNAT.Regexp.Regexp;

      procedure Add_SID_File (SID_Name : String);
      --  Callback for Enumerate_SIDs. Add SID_Name to SID_Inputs

      procedure SID_Load_Wrapper (Filename : String);
      --  Wrapper for SID_Load to include the ignored source file regexp

      ------------------
      -- Add_SID_File --
      ------------------

      procedure Add_SID_File (SID_Name : String) is
      begin
         Inputs.Add_Input (SID_Inputs, SID_Name);
      end Add_SID_File;

      ----------------------
      -- SID_Load_Wrapper --
      ----------------------

      procedure SID_Load_Wrapper (Filename : String) is
      begin
         Checkpoints.SID_Load
           (Filename, (if Has_Matcher then Matcher'Access else null));
      end SID_Load_Wrapper;

   --  Start of processing for Load_All_SIDs

   begin
      if SIDs_Loaded then
         return;
      end if;
      SIDs_Loaded := True;

      --  If no --sid option is present, enumerate all available SID for units
      --  of interest. This requires a project file.

      if Is_Project_Loaded and then Inputs.Length (SID_Inputs) = 0 then
         Enumerate_SIDs (Add_SID_File'Access);
      end if;

      --  Now load the SID files, applying the Ignore_Source_Files filter,
      --  if present.

      Create_Matcher (Ignored_Source_Files, Matcher, Has_Matcher);
      Inputs.Iterate (SID_Inputs, SID_Load_Wrapper'Access);
   end Load_All_SIDs;

   -----------------------
   -- Process_Arguments --
   -----------------------

   procedure Process_Arguments is

      function Parse_Hex (S : String; Flag_Name : String) return Pc_Type;
      --  Parse S to get an hexadecimal number (form : 0x[0-9a-f]+) and
      --  return the value. If the parsing fails, fatal error.

      Current_Exec : GNAT.Strings.String_Access := null;
      --  Some arguments specify what executable the next traces will have to
      --  refer to: this holds the current executable for the next traces.

      procedure Handle_Trace_List_Element (Element : String);
      --  If Element starts with ASCII.NUL, consider it comes from --exec and
      --  assign it to Current_Exec. Otherwise, add it to the Trace_Inputs
      --  input-list.

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
            Fatal_Error ("Missing '0x' prefix for " & Flag_Name);
         end if;
         Pos := S'First + 2;
         Get_Pc (Res, S, Pos);
         if Pos <= S'Last then
            Fatal_Error ("Bad hexadecimal number for " & Flag_Name);
         end if;
         return Res;
      end Parse_Hex;

      -------------------------------
      -- Handle_Trace_List_Element --
      -------------------------------

      procedure Handle_Trace_List_Element (Element : String) is
      begin
         if Element'Length > 0
           and then Element (Element'First) = ASCII.NUL
         then
            Current_Exec :=
              new String'(Element (Element'First + 1 .. Element'Last));
         else
            Inputs.Add_Input
              (Trace_Inputs, Element, Qualifier => Current_Exec);
         end if;
      end Handle_Trace_List_Element;

   --  Start of processing for Process_Arguments

   begin
      --  First, handle all options...

      Verbose                  := Args.Bool_Args (Opt_Verbose);
      Switches.All_Decisions   := Args.Bool_Args (Opt_All_Decisions);
      Switches.All_Messages    := Args.Bool_Args (Opt_All_Messages);
      Branch_Stats             := Args.Bool_Args (Opt_Branch_Stats);
      Excluded_SCOs            := Args.Bool_Args (Opt_Excluded_SCOs);
      Keep_Edges               := Args.Bool_Args (Opt_Keep_Edges);
      Pretty_Print             := Args.Bool_Args (Opt_Pretty_Print);
      Keep_Reading_Traces      := Args.Bool_Args (Opt_Keep_Reading_Traces);
      Dump_Units               := Args.String_Args (Opt_Dump_Units_To).Present;
      Show_MCDC_Vectors        := (Args.Bool_Args (Opt_Show_MCDC_Vectors)
                                 or else All_Messages
                                 or else Verbose);
      Allow_Mixing_Trace_Kinds := Args.Bool_Args (Opt_Allow_Mix_Trace_Kind);
      Analyze_Entry_Barriers   := Args.Bool_Args (Opt_Analyze_Entry_Barriers);
      Short_Circuit_And_Or     := Args.Bool_Args
                                    (Opt_Boolean_Short_Circuit_And_Or);
      Emit_Report              := not Args.Bool_Args (Opt_Cancel_Annotate);

      if Args.Bool_Args (Opt_Recursive) then
         Warn ("--recursive is deprecated. Recursive is now the default"
               & " behavior.");
      end if;

      Copy_Arg (Opt_Output, Output);
      Copy_Arg (Opt_Final_Report, Output);
      Copy_Arg (Opt_Tag, Tag);
      Copy_Arg (Opt_Kernel, Kernel);
      Copy_Arg (Opt_HW_Trigger_Traces, Convert.HW_Trigger_Arg);
      Copy_Arg (Opt_Input, Convert.Input_Arg);
      Copy_Arg (Opt_Save_Checkpoint, Save_Checkpoint);

      Copy_Arg_List (Opt_Scos, ALIs_Inputs);
      if Inputs.Length (ALIs_Inputs) /= 0 then
         Invalidate_Unit_List ("--scos is present");
      end if;

      Copy_Arg_List (Opt_SID, SID_Inputs);
      if Inputs.Length (SID_Inputs) /= 0 then
         Invalidate_Unit_List ("--sid is present");
      end if;

      Copy_Arg_List (Opt_Routines, Routines_Inputs);
      Copy_Arg_List (Opt_Exec, Exe_Inputs);
      Copy_Arg_List (Opt_Checkpoint, Checkpoints_Inputs);
      Copy_Arg_List (Opt_Ignore_Source_Files, Ignored_Source_Files);

      if Args.String_Args (Opt_Coverage_Level).Present then
         declare
            Arg : constant String :=
              +Args.String_Args (Opt_Coverage_Level).Value;
         begin
            Set_Coverage_Levels (Arg);
         exception
            when Constraint_Error =>
               Fatal_Error ("Bad coverage level: " & Arg);
         end;
      end if;

      if Args.String_Args (Opt_Annotation_Format).Present then
         declare
            Arg : constant String :=
              +Args.String_Args (Opt_Annotation_Format).Value;
         begin
            Annotation := To_Annotation_Format (Arg);
            if Annotation = Annotate_Unknown then
               Fatal_Error ("Bad annotation format: " & Arg);
            end if;
         end;
      end if;

      for Arg of Args.String_List_Args (Opt_Routines_List) loop
         Inputs.Add_Input (Routines_Inputs, '@' & (+Arg));
      end loop;

      if Args.String_Args (Opt_Text_Start).Present then

         --  FIXME: not yet supported???
         --
         --  Should be a global option (used when building decision map for
         --  --run)???

         begin
            Text_Start := Parse_Hex
              (+Args.String_Args (Opt_Text_Start).Value,
               "--text-start");
         exception
            when Constraint_Error =>
               Fatal_Error ("Failure to parse --text-start");
         end;
      end if;

      if Args.String_Args (Opt_Timezone).Present then
         Timezone :=
           Calendar_Utils.To_Timezone (+Args.String_Args (Opt_Timezone).Value);
      end if;

      --  Parse --source-rebase options

      declare
         procedure Process_One_Entry (Arg : String);
         --  Parse source-rebase's argument. This option's form should be:
         --
         --  "<OLD_PREFIX>=<NEW_PREFIX>"

         -----------------------
         -- Process_One_Entry --
         -----------------------

         procedure Process_One_Entry (Arg : String) is
            Pos : Natural := 0;
         begin
            for I in Arg'First .. Arg'Last loop
               if Arg (I) = '=' then
                  Pos := I;
                  exit;
               end if;
            end loop;
            if Pos = 0 then
               Fatal_Error ("Missing '=' in --source-rebase");
            end if;
            Add_Source_Rebase (Arg (Arg'First .. Pos - 1),
                               Arg (Pos + 1 .. Arg'Last));
         end Process_One_Entry;

      begin
         Copy_Arg_List (Opt_Source_Rebase, Source_Rebase_Inputs);
         Iterate (Source_Rebase_Inputs, Process_One_Entry'Access);
      end;

      --  Parse --source-search options

      Copy_Arg_List (Opt_Source_Search, Source_Search_Inputs);
      Iterate (Source_Search_Inputs, Add_Source_Search'Access);

      if Args.String_Args (Opt_Exec_Prefix).Present then
         Set_Exec_Prefix (+Args.String_Args (Opt_Exec_Prefix).Value);
      end if;

      if Args.String_Args (Opt_Output_Directory).Present then
         Outputs.Set_Output_Dir
           (+Args.String_Args (Opt_Output_Directory).Value);
      end if;

      for Arg of Args.String_List_Args (Opt_Trace) loop
         Handle_Trace_List_Element (+Arg);
      end loop;

      if Args.String_Args (Opt_Trace_Source).Present then
         Convert.Set_Trace_Source (+Args.String_Args (Opt_Trace_Source).Value);
      end if;

      if Args.String_Args (Opt_Separate).Present then
         declare
            Name : constant String := +Args.String_Args (Opt_Separate).Value;
         begin
            Tag_Provider := Tag_Providers.Create (Name);
         exception
            when Constraint_Error =>
               Fatal_Error ("Invalid separated coverage analysis mode: "
                            & Name & " (available: "
                            & Tag_Providers.Registered_Names (", ") & ")");
         end;
      end if;

      if Args.String_Args (Opt_Output_Format).Present then
         declare
            Arg : constant String :=
              +Args.String_Args (Opt_Output_Format).Value;
         begin
            CFG_Output_Format := CFG_Dump.Output_Format'Value (Arg);
         exception
            when Constraint_Error =>
               Fatal_Error ("Invalid output format: " & Arg);
         end;
      end if;

      for Arg of Args.String_List_Args (Opt_Debug) loop
         for Char of Ada.Strings.Unbounded.To_String (Arg) loop
            declare
               use Command_Line_Support;

               Switch : constant Debug_Type := Debug_Switches_Map (Char);
            begin
               if Switch = None then
                  Fatal_Error ("Invalid debug switch: -d" & Char);
               else
                  Debug_Switches (Switch) := True;
               end if;
            end;
         end loop;
      end loop;

      for Arg of Args.String_List_Args (Opt_Shared_Object) loop
         declare

            procedure SO_Error (Msg : String);
            --  Raise a fatal error for a bad usage of the Opt_Shared_Object
            --  option.

            procedure None_And_All_Error;
            --  Raise a fatal error for using both "none" and "all" options

            procedure Set_Mode (SO_Set : SO_Set_Type)
               with Pre => SO_Set.Kind /= Some_SO;
            --  Switch to the given SO_Set mode. Raise a fatal error if there
            --  is already a non-default mode or if there are already
            --  explicitly listed shared objects.

            procedure Invalid_SO_With (Mode : SO_Set_Kind)
               with Pre => Mode /= Some_SO;
            --  Raise a fatal error for trying to switch to the non-default
            --  "mode" whereas there are already explicitly listed shared
            --  objects.

            --------------
            -- SO_Error --
            --------------

            procedure SO_Error (Msg : String) is
               Opt_Name : constant String := Option_Name
                 (Arg_Parser, (String_List_Opt, Opt_Shared_Object));
            begin
               Fatal_Error (Opt_Name & ": " & Msg);
            end SO_Error;

            ------------------------
            -- None_And_All_Error --
            ------------------------

            procedure None_And_All_Error is
            begin
               SO_Error
                 ("""none"" and ""all"" cannot be used at the same time");
            end None_And_All_Error;

            --------------
            -- Set_Mode --
            --------------

            procedure Set_Mode (SO_Set : SO_Set_Type) is
            begin
               if Length (SO_Inputs.Set) /= 0 then
                  Invalid_SO_With (SO_Set.Kind);
               end if;
               SO_Inputs := SO_Set;
            end Set_Mode;

            ---------------------
            -- Invalid_SO_With --
            ---------------------

            procedure Invalid_SO_With (Mode : SO_Set_Kind) is
               Name : constant String :=
                 (case Mode is
                  when None => "none",
                  when Some_SO => raise Program_Error,
                  when All_SO => "all");
            begin
               SO_Error
                 ("cannot provide shared objects with """ & Name & """");
            end Invalid_SO_With;

         begin
            --  If they are provided, "none" or "all" must be the first value
            --  we get. Besides, if we have one of them, we must not have
            --  anything else.

            if +Arg = "none" then
               case SO_Inputs.Kind is
                  when None    => null;
                  when Some_SO => Set_Mode ((Kind => None));
                  when All_SO  => None_And_All_Error;
               end case;

            elsif +Arg = "all" then
               case SO_Inputs.Kind is
                  when None    => None_And_All_Error;
                  when Some_SO => Set_Mode ((Kind => All_SO));
                  when All_SO  => null;
               end case;

            else
               case SO_Inputs.Kind is
                  when None | All_SO => Invalid_SO_With (SO_Inputs.Kind);
                  when Some_SO       => Add_Input (SO_Inputs.Set, +Arg);
               end case;
            end if;
         end;
      end loop;

      if Args.String_Args (Opt_Dump_Units_To).Present then
         if Object_Coverage_Enabled then
            Fatal_Error ("--dump-units-to works in source coverage only");
         end if;

         declare
            Arg : constant String :=
               +Args.String_Args (Opt_Dump_Units_To).Value;
         begin
            if Arg /= "-" then
               Dump_Units_Filename := new String'(Arg);
            end if;
         end;
      end if;

      --  ... then, handle remaning arguments, which have subcommand-specific
      --  meanings.

      case Args.Command is
         when Cmd_Coverage
            | Cmd_Dump_Trace
            | Cmd_Dump_Trace_Raw
            | Cmd_Dump_Trace_Base
            | Cmd_Dump_Trace_Asm
            | Cmd_Dump_Src_Trace =>

            --  For "coverage", require an annotation format unless we must
            --  save a checkpoint. In this case, we'll just skip report
            --  production.

            if Args.Command = Cmd_Coverage
              and then
                (not Args.String_Args (Opt_Annotation_Format).Present
                 and Save_Checkpoint = null)
            then
               Report_Missing_Argument ("an annotation format");
            end if;

            --  If --no-cov-report is on the command line, check that
            --  --save-checkpoint is also present.

            if Args.Command = Cmd_Coverage
              and then not Emit_Report
              and then Save_Checkpoint = null
            then
               Report_Missing_Argument
                 ("--save-checkpoint",
                  " when --cancel-annotate is also specified");
            end if;

            --  Remaining arguments are supposed to be copied to Opt_Trace,
            --  which is already handled, so there's nothing left to do with
            --  them.

         when Cmd_Disp_Routines
            | Cmd_Scan_Objects =>
            for Arg of Args.Remaining_Args loop
               Inputs.Add_Input (Obj_Inputs, +Arg);
            end loop;

         when Cmd_Dump_Sections
            | Cmd_Dump_Symbols
            | Cmd_Dump_Compile_Units
            | Cmd_Dump_Subprograms
            | Cmd_Dump_Inlined_Subprograms
            | Cmd_Dump_Lines
            | Cmd_Disassemble_Raw
            | Cmd_Disassemble =>
            for Arg of Args.Remaining_Args loop
               Inputs.Add_Input (Exe_Inputs, +Arg);
            end loop;

         when Cmd_Disassemble_Insn_Properties | Cmd_Dump_CFG =>
            --  The first argument is the executable. The other ones are
            --  locations.

            declare
               use String_Vectors;
               Arg_Vector : Vector renames Args.Remaining_Args;
               Cur        : Cursor;
            begin
               if Arg_Vector.Length < 2 then
                  Fatal_Error ("Missing arguments");
               else
                  Cur := Arg_Vector.First;
                  Executable_Path := new String'(+Element (Cur));
                  loop
                     Next (Cur);
                     exit when Cur = No_Element;
                     Locations_Inputs.Append
                       (Object_Locations.Parse_User_Location
                          (+Element (Cur)));
                  end loop;
               end if;
            end;

         when Cmd_Dump_Pragmas =>
            if Args.Bool_Args (Opt_GNAT_Pragmas) then
               for P in Snames.Pragma_Id loop
                  Put_Line (P'Image);
               end loop;
            else
               for P in Pragma_Id loop
                  Put_Line (P'Image);
               end loop;
            end if;

         when Cmd_Map_Routines =>
            --  Set MC/DC coverage level in order to generate a complete
            --  decision map.

            Set_Coverage_Levels ("stmt+mcdc");
            for Arg of Args.Remaining_Args loop
               Inputs.Add_Input (Exe_Inputs, +Arg);
            end loop;

         when Cmd_Run =>

            --  Sort out what to use as the executable name and what EARGS to
            --  forward to our Eargs local, depending on whether we have an
            --  executable argument on the command line, in the eargs or in a
            --  project file.

            case Args.Remaining_Args.Length is

               when 0 =>

                  --  We don't have an executable specified on the base
                  --  command line (before eargs).
                  --
                  --  If the first EARG is an executable file, use it and
                  --  forward the rest.
                  --
                  --  Otherwise, if we can get an executable from a project
                  --  file, use that and forward all the EARGS.
                  --
                  --  Otherwise, complain about missing an executable to run.

                  declare
                     Eargs_Arg : String_Vectors.Vector
                     renames Args.String_List_Args (Opt_Eargs);

                     Exe_From_Project : constant String :=
                       (if Is_Project_Loaded
                          then Get_Single_Main_Executable
                          else "");

                     C : String_Vectors.Cursor :=
                       String_Vectors.First (Eargs_Arg);

                     use String_Vectors;

                     Earg0 : constant String :=
                       (if Has_Element (C) then +Element (C) else "");

                     Earg0_Executable : constant Boolean :=
                       GNAT.OS_Lib.Is_Executable_File (Earg0);
                  begin

                     if Earg0_Executable then
                        Inputs.Add_Input (Exe_Inputs, Earg0);
                        loop
                           Next (C);
                           exit when not Has_Element (C);
                           Eargs.Append (Element (C));
                        end loop;

                     elsif Exe_From_Project /= "" then
                        Inputs.Add_Input (Exe_Inputs, Exe_From_Project);
                        Eargs := Eargs_Arg;
                     else
                        Report_Missing_Argument ("an executable to run (EXE)");
                     end if;

                  end;

               when 1 =>

                  --  We have single executable argument on the base command
                  --  line (before eargs). Use it and forward all the EARGS
                  --  options we have to the Eargs local.

                  Inputs.Add_Input
                    (Exe_Inputs, +Args.Remaining_Args.First_Element);
                  Eargs := Args.String_List_Args (Opt_Eargs);

               when others =>

                  --  We have more than one non-earg trailing argument on the
                  --  base command line, complain.

                  Fatal_Error ("Only one EXEC parameter is allowed");
            end case;

         when Cmd_Check_SCOs =>
            for Arg of Args.Remaining_Args loop
               Inputs.Add_Input (ALIs_Inputs, +Arg);
            end loop;

         when Cmd_Convert =>
            if Args.String_List_Args (Opt_Exec).Is_Empty then
               Fatal_Error
                 (Option_Name (Arg_Parser, (String_List_Opt, Opt_Exec))
                  & " is missing (required for ""convert"")");
            end if;

         when Cmd_Instrument =>

            --  Ensure we have a source coverage level. If not, we might have
            --  either an object level specified, or possibly no --level at
            --  all.

            if not Source_Coverage_Enabled then
               Fatal_Error
                 ("Instrumentation requires a source coverage level"
                  & ASCII.LF
                  & "  (--level=" & Source_Level_Options ("|") & ")");
            end if;

            if Args.Remaining_Args.Length > 0 then
               Fatal_Error
                 ("Invalid extra argument: "
                  & (+Args.Remaining_Args.First_Element));
            end if;

         when others =>
            null;
      end case;

      if not Is_Project_Loaded
         and then not Args.String_List_Args (Opt_Projects).Is_Empty
      then
            Fatal_Error ("--projects requires -P");
      end if;

      if Inputs.Length (Ignored_Source_Files) = 0 and then Is_Project_Loaded
      then
         declare
            procedure Add_Source_File (S : String);
            --  Add S to the list of ignored source files

            ---------------------
            -- Add_Source_File --
            ---------------------

            procedure Add_Source_File (S : String) is
            begin
               Inputs.Add_Input (Ignored_Source_Files, S);
            end Add_Source_File;
         begin
            Enumerate_Ignored_Source_Files (Add_Source_File'Access);
         end;
      end if;

      --  Set defaults for options not specified so far

      declare
         use type Tag_Provider_Access;
      begin
         if Tag_Provider = null then
            Tag_Provider := Tag_Providers.Create (Default_Tag_Provider_Name);
         end if;
      end;
   end Process_Arguments;

   ----------------------
   -- Report_Bad_Trace --
   ----------------------

   procedure Report_Bad_Trace (Trace_Filename : String; Result : Read_Result)
   is
      Message : constant String :=
         Trace_Filename & ": "
         & Ada.Strings.Unbounded.To_String (Result.Error);
   begin
      if Keep_Reading_Traces then
         Outputs.Error (Message);
      else
         Outputs.Fatal_Error (Message);
      end if;
   end Report_Bad_Trace;

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

   Base        : aliased Traces_Base;
   Exec        : Exe_File_Acc;

--  Start of processing for GNATcov

begin

   --  Load arguments from command-line and from the project file (if any),
   --  then update our local state according to them. Create an artificial
   --  internal error in the middle, if requested.

   Parse_Arguments (From_Driver => False);
   Raise_Stub_Internal_Error_For (Arguments_Loading);
   Process_Arguments;

   if Verbose then
      Show_Version;
      Show_CWD;
      Perf_Counters.Enable;
   end if;

   --  Now execute the specified command

   case Args.Command is
      when None =>
         --  This can happen only if there is an error during arguments
         --  parsing. In this case, we are supposed to stop earlier, so
         --  we're not supposed to end up here.

         raise Program_Error;

      when Cmd_Help =>
         Print_Usage (Arg_Parser, False, False);

      when Cmd_Help_Internal =>
         Print_Usage (Arg_Parser, True, False);

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
            Check_Argument_Available (Obj_Inputs, "FILEs");
            Inputs.Iterate (Obj_Inputs, Read_Routine_Name'Access);
            Traces_Names.Disp_All_Routines_Of_Interest;
         end;

      when Cmd_Instrument =>
         if not Is_Project_Loaded then
            Fatal_Error ("instrumentation requires a project file;"
                         & " please use the -P option");
         end if;

         declare
            use Instrument;

            Matcher     : aliased GNAT.Regexp.Regexp;
            Has_Matcher : Boolean;

            Dump_Trigger_Opt : String_Option renames
               Args.String_Args (Opt_Dump_Trigger);
            Dump_Channel_Opt : String_Option renames
               Args.String_Args (Opt_Dump_Channel);

            Dump_Filename_Simple      : Boolean renames
               Args.Bool_Args (Opt_Dump_Filename_Simple);
            Dump_Filename_Env_Var_Opt : String_Option renames
               Args.String_Args (Opt_Dump_Filename_Env_Var);
            Dump_Filename_Prefix_Opt  : String_Option renames
               Args.String_Args (Opt_Dump_Filename_Prefix);

            Language_Version_Opt : String_Option renames
               Args.String_Args (Opt_Ada);

            Dump_Config      : Any_Dump_Config := (others => <>);
            Language_Version : Any_Language_Version := Ada_2012;
         begin
            Create_Matcher (Ignored_Source_Files, Matcher, Has_Matcher);

            --  Create the appropritae dump configuration depending on
            --  command-line options. Unless --dump-channel is passed, the dump
            --  channel is "bin-file".

            declare
               Value : constant String :=
                 (if Dump_Channel_Opt.Present
                  then +Dump_Channel_Opt.Value
                  else "bin-file");
            begin
               if Value = "bin-file" then
                  Dump_Config :=
                    (Channel          => Binary_File,
                     Trigger          => <>,
                     Filename_Simple  => Dump_Filename_Simple,
                     Filename_Env_Var =>
                        Value_Or_Null (Dump_Filename_Env_Var_Opt),
                     Filename_Prefix  =>
                        Value_Or_Null (Dump_Filename_Prefix_Opt));
               elsif Value = "base64-stdout" then
                  Dump_Config :=
                    (Channel => Base64_Standard_Output,
                     Trigger => <>);
               else
                  Fatal_Error ("Bad buffers dump channel: " & Value);
               end if;
            end;

            if Dump_Trigger_Opt.Present then
               declare
                  Value : constant String := +Dump_Trigger_Opt.Value;
               begin
                  if Value = "manual" then
                     Dump_Config.Trigger := Manual;
                  elsif Value = "atexit" then
                     Dump_Config.Trigger := At_Exit;
                  elsif Value = "ravenscar-task-termination" then
                     Dump_Config.Trigger := Ravenscar_Task_Termination;
                  elsif Value = "main-end" then
                     Dump_Config.Trigger := Main_End;
                  else
                     Fatal_Error ("Bad buffers dump trigger: " & Value);
                  end if;
               end;
            end if;

            if Language_Version_Opt.Present then
               declare
                  Value : constant String := +Language_Version_Opt.Value;
               begin
                  if Value in "83" | "1983" then
                     Language_Version := Ada_83;
                  elsif Value in "95" | "1995" then
                     Language_Version := Ada_95;
                  elsif Value in "05" | "2005" then
                     Language_Version := Ada_2005;
                  elsif Value in "12" | "2012" then
                     Language_Version := Ada_2012;
                  end if;
               end;
            end if;

            if Dump_Config.Channel /= Binary_File then
               if Dump_Filename_Simple then
                  Fatal_Error
                    ("--dump-filename-simple requires"
                     & " --dump-channel=bin-file");
               elsif Dump_Filename_Env_Var_Opt.Present then
                  Fatal_Error
                    ("--dump-filename-env-var requires"
                     & " --dump-channel=bin-file");
               elsif Dump_Filename_Prefix_Opt.Present then
                  Fatal_Error
                    ("--dump-filename-prefix requires"
                     & " --dump-channel=bin-file");
               end if;
            end if;

            --  Even though instrumentation does not create any traces, the
            --  structure of a SID file is basically a checkpoint, so it has a
            --  Trace_Kind field in its header. Instead of leaving it to
            --  Unknown (default value) mark it as Source_Trace_File so that
            --  when the .sid file is loaded, it will set gnatcov in
            --  "source trace mode" and it will be rejected if binary traces
            --  have already been loaded.

            Update_Current_Trace_Kind (Source_Trace_File);

            if Analyze_Entry_Barriers then
               Warn ("With source traces, entry barrier analysis (enabled"
                     & " with --analyze-entry-barriers) is only supported when"
                     & " when using a full runtime profile. In particular,"
                     & " enabeling this option with a ravenscar profile will"
                     & " result in invalid code being emitted.");
            end if;

            Instrument.Instrument_Units_Of_Interest
              (Dump_Config          => Dump_Config,
               Language_Version     => Language_Version,
               Ignored_Source_Files =>
                 (if Has_Matcher then Matcher'Access else null));
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
            Check_Argument_Available (Obj_Inputs, "FILEs");
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
            Load_All_ALIs (Check_SCOs => True);
            Inputs.Iterate (Exe_Inputs, Build_Decision_Map'Access);
            if Verbose then
               SC_Obligations.Report_SCOs_Without_Code;
            end if;
            SC_Obligations.Report_Units_Without_Code;
         end;

      when Cmd_Scan_Decisions =>
         Set_Coverage_Levels ("stmt");
         Load_All_ALIs (Check_SCOs => True);
         SC_Obligations.Report_Multipath_Decisions;

      when Cmd_Check_SCOs =>
         Set_Coverage_Levels ("stmt");
         Inputs.Iterate (ALIs_Inputs, Check_SCOs.Check_SCO_Syntax'Access);
         Load_All_ALIs (Check_SCOs => True);

      when Cmd_Dump_Trace =>
         Check_Argument_Available (Trace_Inputs, "TRACE_FILEs");
         Inputs.Iterate (Trace_Inputs, Dump_Trace_File'Access);

      when Cmd_Dump_Trace_Raw =>
         Check_Argument_Available (Trace_Inputs, "TRACE_FILEs");
         Inputs.Iterate (Trace_Inputs, Dump_Raw_Trace_File'Access);

      when Cmd_Dump_Trace_Base =>
         declare
            procedure Dump_Trace_Base (Trace_File_Name : String);
            --  Raw display of merged trace files

            ---------------------
            -- Dump_Trace_Base --
            ---------------------

            procedure Dump_Trace_Base (Trace_File_Name : String) is
               Trace_File : Trace_File_Type;
               Result     : Read_Result;
            begin
               Read_Trace_File (Trace_File_Name, Trace_File, Result, Base);
               Success_Or_Fatal_Error (Trace_File_Name, Result);
               Dump_Traces (Base);
            end Dump_Trace_Base;

         begin
            Check_Argument_Available (Trace_Inputs, "TRACE_FILEs");
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
               Traces_Disa.Dump_Traces_With_Asm (Exec.all, Trace_File_Name);
            end Dump_Trace;

            ---------------
            -- Open_Exec --
            ---------------

            procedure Open_Exec (Exec_File_Name : String) is
            begin
               Exec := Open_File (Exec_File_Name, Text_Start);
               Build_Sections (Exec.all);
               Build_Symbols (Exec.all);
            end Open_Exec;

         begin
            Check_Argument_Available (Exe_Inputs, "EXE");
            Check_Argument_Available (Trace_Inputs, "TRACE_FILEs");
            Inputs.Iterate (Exe_Inputs, Open_Exec'Access);
            Inputs.Iterate (Trace_Inputs, Dump_Trace'Access);
         end;

      when Cmd_Dump_Src_Trace =>
         Check_Argument_Available (Trace_Inputs, "TRACE_FILE");
         Inputs.Iterate
           (Trace_Inputs,
            Instrument.Input_Traces.Dump_Source_Trace_File'Access);

      when Cmd_Dump_Sections
        | Cmd_Dump_Symbols
        | Cmd_Dump_Subprograms
        | Cmd_Dump_Inlined_Subprograms
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
               Exec := Open_File (Exec_File_Name, Text_Start);
               Build_Sections (Exec.all);

               case Args.Command is
                  when Cmd_Dump_Sections =>
                     To_Display := Section_Addresses;

                  when Cmd_Dump_Symbols =>
                     Build_Symbols (Exec.all);
                     To_Display := Symbol_Addresses;

                  when Cmd_Dump_Subprograms =>
                     Build_Debug_Compile_Units (Exec.all);
                     To_Display := Subprogram_Addresses;

                  when Cmd_Dump_Inlined_Subprograms =>
                     Build_Debug_Lines (Exec.all);
                     To_Display := Inlined_Subprogram_Addresses;

                  when Cmd_Dump_Lines =>
                     Build_Debug_Lines (Exec.all);
                     To_Display := Line_Addresses;

                  when others =>
                     --  Never happens

                     raise Program_Error;
               end case;

               Disp_Addresses (Exec.all, To_Display);
               Close_File (Exec);
            end Dump_Exec;

         begin
            Check_Argument_Available (Exe_Inputs, "EXEs");
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
               Exec := Open_File (Exec_File_Name, 0);
               Build_Sections (Exec.all);
               Build_Debug_Compile_Units (Exec.all);
               Disp_Compilation_Units (Exec.all);
               Close_File (Exec);
            end Dump_Compilation_Units;

         begin
            Check_Argument_Available (Exe_Inputs, "EXEs");
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
               Exec := Open_File (Exec_File_Name, 0);
               Disassemble_File_Raw (Exec.all);
               Close_File (Exec);
            end Disassemble;

         begin
            Check_Argument_Available (Exe_Inputs, "EXEs");
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
               Exec := Open_File (Exec_File_Name, 0);
               Build_Sections (Exec.all);
               Build_Symbols (Exec.all);
               Disassemble_File (Exec.all);
               Close_File (Exec);
            end Disassemble;

         begin
            Check_Argument_Available (Exe_Inputs, "EXEs");
            Inputs.Iterate (Exe_Inputs, Disassemble'Access);
         end;

      when Cmd_Disassemble_Insn_Properties | Cmd_Dump_CFG =>
         if Executable_Path = null then
            Report_Missing_Argument ("an executable (EXE)");
         elsif Locations_Inputs.Is_Empty then
            Report_Missing_Argument ("at least one location (SELECTORs)");
         end if;

         if Args.Command = Cmd_Disassemble_Insn_Properties then
            Disassemble_Insn_Properties.Disassemble
              (Executable_Path.all,
               Locations_Inputs,
               not Pretty_Print);

         else
            CFG_Dump.Dump
              (Executable_Path.all,
               Locations_Inputs,
               Output,
               CFG_Output_Format,
               ALIs_Inputs,
               Trace_Inputs,
               Keep_Edges);
         end if;
         Free (Executable_Path);

      when Cmd_Dump_Pragmas =>
         null;

      when Cmd_Coverage =>

         --  Make sure we have a coverage level

         if not (Source_Coverage_Enabled or else Object_Coverage_Enabled)
         then
            Report_Missing_Argument ("a coverage level");
         end if;

         --  Reject the use of several features that are not supported with
         --  object coverage.

         if Object_Coverage_Enabled then
            if Annotation = Annotate_Report then
               Fatal_Error
                 ("""report"" output format (from --annotate) is"
                    & " only for source coverage criteria"
                    & ASCII.LF
                    & "  (--level=" & Source_Level_Options ("|")
                    & ", not --level="
                    & Coverage_Option_Value (Current_Levels) & ")");

            elsif Inputs.Length (Checkpoints_Inputs) > 0
              or else Save_Checkpoint /= null
            then
               Fatal_Error ("Incremental coverage is supported for source"
                            & " coverage only");
            end if;
         end if;

         --  Check the availability of the output format

         case Annotation is
            when Annotate_Xml =>
               if not Annotations.Xml.Installed then
                  Fatal_Error
                    ("XML report format support is not installed.");
               end if;

            when Annotate_Dynamic_Html =>
               if not Annotations.Dynamic_Html.Installed then
                  Fatal_Error
                    ("Dynamic HTML report format support is not installed.");
               end if;

            when others =>
               null;
         end case;

         --  Check that the user specified units of interest. We'll load SCOs
         --  from ALIs/SIDs only when necessary, i.e. only the first time we
         --  process a binary trace file. This will avoid conflicts between
         --  incompatible source obligations (instrumentation-based SCOs from
         --  an SID, ALIs from a rebuilt project, ...).

         Check_User_Provided_SCOs;

         --  Build the list of units of interest from project files option

         declare
            procedure Add_Unit (Name : String; Is_Subunit : Boolean);
            --  Add Name to the list of names for units of interest

            --------------
            -- Add_Unit --
            --------------

            procedure Add_Unit (Name : String; Is_Subunit : Boolean) is
               pragma Unreferenced (Is_Subunit);
            begin
               Add_Unit_Name (Name);
            end Add_Unit;
         begin
            Enumerate_Units_Of_Interest (Add_Unit'Access);
         end;

         --  Load routines from command line

         if Object_Coverage_Enabled then

            if Inputs.Length (Routines_Inputs) /= 0 then
               Inputs.Iterate (Routines_Inputs,
                               Traces_Names.Add_Routine_Of_Interest'Access);
               Routines_Of_Interest_Origin := From_Command_Line;

            elsif Inputs.Length (Trace_Inputs) > 1 then
               Report_Missing_Argument
                 ("a list of routines",
                  "required when reading multiple trace files");

            else
               --  If no routines were given on the command line, we'll add
               --  them when processing the list of symbols from the only
               --  executable file (using Read_Routines_Names, see below).

               Routines_Of_Interest_Origin := From_Elf_Symbols;
            end if;

         else
            if Inputs.Length (Routines_Inputs) /= 0 then
               Fatal_Error ("Routine list not allowed for source coverage.");
            end if;
         end if;

         --  Read and process traces

         declare
            use Ada.Strings.Unbounded;

            Bin_Traces_Present : Boolean := False;
            Src_Traces_Present : Boolean := False;

            procedure Process_Exec (Exec_Name : String);
            --  Load a consolidated executable

            procedure Process_Trace
              (Trace_File_Name    : String;
               Exec_Name_Override : String);
            --  Try to read Trace_File_Name. Depending on the probed trace file
            --  kind, dispatch to Process_Binary_Trace or Process_Source_Trace.
            --  If Trace_File_Name is an empty string, just dispatch to
            --  Process_Binary_Trace (case of forcing the load of a program).

            procedure Process_Source_Trace (Trace_File_Name : String);
            --  Process the given source trace file, discharging SCIs
            --  referenced by its coverage buffers.

            procedure Process_Binary_Trace
              (Trace_File_Name    : String;
               Exec_Name_Override : String);
            --  Common dispatching point for object and source coverage:
            --  process one trace file (with optional override of exec file
            --  name), or load one consolidated executable (if Trace_File_Name
            --  is an empty string, in which case Exec_Name_Override is not
            --  allowed to be null).

            procedure Process_Trace_For_Obj_Coverage
              (Trace_File         : Trace_File_Element_Acc;
               Exec_Name_Override : String);
            --  Open Trace_File and merge it into the trace database

            procedure Process_Trace_For_Src_Coverage
              (Trace_File         : Trace_File_Element_Acc;
               Exec_Name_Override : String);
            --  Process Trace_File for source coverage. No trace database is
            --  used.

            function Open_Exec_For_Trace
              (Trace_File_Name    : String;
               Trace_File         : Trace_File_Type;
               Exec_Name_Override : String) return Exe_File_Acc;
            --  Open the executable for TF, taking into account a possible
            --  command line override of the executable file name. The opened
            --  exec file is entered in the global execs list.

            -------------------------
            -- Open_Exec_For_Trace --
            -------------------------

            function Open_Exec_For_Trace
              (Trace_File_Name    : String;
               Trace_File         : Trace_File_Type;
               Exec_Name_Override : String) return Exe_File_Acc
            is
               use Qemu_Traces;

               function Get_Exe_Name return String;
               --  Executable name as determined from trace file or overridden

               ------------------
               -- Get_Exe_Name --
               ------------------

               function Get_Exe_Name return String is
               begin
                  if Exec_Name_Override /= "" then
                     return Exec_Name_Override;
                  end if;

                  declare
                     Exec_Name_From_Trace : constant String :=
                       Get_Info (Trace_File, Exec_File_Name);
                  begin
                     if Exec_Name_From_Trace = "" then
                        Fatal_Error
                          ("Cannot find executable filename in trace file "
                           & Trace_File_Name);
                     end if;

                     return Exec_Name_From_Trace;
                  end;
               end Get_Exe_Name;

               Exe_Name : constant String := Get_Exe_Name;

            --  Start of processing for Open_Exec_For_Trace

            begin
               return Exe_File : Exe_File_Acc do
                  Execs_Dbase.Open_Exec_For_Trace
                    (Exe_Name, Text_Start,
                     Trace_File_Name, Get_Signature (Trace_File),
                     Exe_File);
               end return;
            exception
               when E : Binary_Files.Error =>
                  Fatal_Error ("Cannot open ELF file " & Exe_Name
                               & " for trace file " & Trace_File_Name & ": "
                               & Ada.Exceptions.Exception_Message (E));
                  raise;
            end Open_Exec_For_Trace;

            ------------------
            -- Process_Exec --
            ------------------

            procedure Process_Exec (Exec_Name : String) is
            begin
               Load_All_ALIs (Check_SCOs => False);
               Process_Trace
                 (Trace_File_Name => "", Exec_Name_Override => Exec_Name);
            end Process_Exec;

            -------------------
            -- Process_Trace --
            -------------------

            procedure Process_Trace
              (Trace_File_Name    : String;
               Exec_Name_Override : String)
            is
               Kind   : Trace_File_Kind;
               Result : Read_Result;
            begin
               if Trace_File_Name = "" then
                  Process_Binary_Trace (Trace_File_Name, Exec_Name_Override);
                  return;
               end if;

               Probe_Trace_File (Trace_File_Name, Kind, Result);
               if not Result.Success then
                  Report_Bad_Trace (Trace_File_Name, Result);
               end if;

               case Kind is
                  when Binary_Trace_File =>
                     Process_Binary_Trace
                       (Trace_File_Name, Exec_Name_Override);
                  when Source_Trace_File =>
                     Process_Source_Trace (Trace_File_Name);
               end case;
            end Process_Trace;

            --------------------------
            -- Process_Source_Trace --
            --------------------------

            procedure Process_Source_Trace (Trace_File_Name : String) is
               procedure On_Trace_Info
                 (Kind : GNATcov_RTS.Traces.Supported_Info_Kind;
                  Data : String);
               --  Callback for Read_Source_Trace_File

               procedure Read_Source_Trace_File is new
                  Instrument.Input_Traces.Generic_Read_Source_Trace_File
                    (On_Trace_Info  => On_Trace_Info,
                     On_Trace_Entry => Compute_Source_Coverage);

               Trace_File : Trace_File_Element_Acc;
               Result     : Read_Result;

               procedure On_Trace_Info
                 (Kind : GNATcov_RTS.Traces.Supported_Info_Kind;
                  Data : String)
               is
               begin
                  Update_From_Source_Trace (Trace_File.all, Kind, Data);
               end On_Trace_Info;

            --  Start of processing for Process_Source_Trace

            begin
               --  Reccord we are loading a source trace

               Update_Current_Trace_Kind (Source_Trace_File);
               Src_Traces_Present := True;

               --  Make sure SID files (to decode source trace files) are
               --  loaded.

               Load_All_SIDs;

               --  Register the trace file, so it is included in coverage
               --  reports.

               Trace_File := Create_Trace_File_Element
                 (Trace_File_Name, Source_Trace_File);

               --  We can now read it and import its data

               Read_Source_Trace_File (Trace_File_Name, Result);
               if not Result.Success then
                  Report_Bad_Trace (Trace_File_Name, Result);
               end if;

               Add_Traces_File (Trace_File);
            end Process_Source_Trace;

            --------------------------
            -- Process_Binary_Trace --
            --------------------------

            procedure Process_Binary_Trace
              (Trace_File_Name    : String;
               Exec_Name_Override : String)
            is
               Trace_File : Trace_File_Element_Acc;
            begin
               --  Record we are loading a binary trace

               Update_Current_Trace_Kind (Binary_Trace_File);
               Bin_Traces_Present := True;

               Load_All_ALIs (Check_SCOs => False);

               if Trace_File_Name /= "" then
                  Trace_File := Create_Trace_File_Element
                    (Trace_File_Name, Binary_Trace_File);
               else
                  pragma Assert (Exec_Name_Override /= "");
               end if;

               if Object_Coverage_Enabled then
                  Process_Trace_For_Obj_Coverage
                    (Trace_File, Exec_Name_Override);
               else
                  Process_Trace_For_Src_Coverage
                    (Trace_File, Exec_Name_Override);
               end if;

               if Trace_File /= null then
                  Add_Traces_File (Trace_File);
               end if;
            end Process_Binary_Trace;

            ------------------------------------
            -- Process_Trace_For_Obj_Coverage --
            ------------------------------------

            procedure Process_Trace_For_Obj_Coverage
              (Trace_File         : Trace_File_Element_Acc;
               Exec_Name_Override : String)
            is
               --  TODO??? Handle shared objects

               Trace_Filename : constant String :=
                 (if Trace_File = null
                  then ""
                  else To_String (Trace_File.Filename));
               Exe_File : Exe_File_Acc;
            begin
               Init_Base (Base);

               if Trace_File = null then
                  Open_Exec (Exec_Name_Override, Text_Start, Exe_File);
               else
                  declare
                     Result   : Read_Result;
                     TF       : Trace_File_Type;
                  begin
                     Read_Trace_File (Trace_Filename, TF, Result, Base);
                     if not Result.Success then
                        Report_Bad_Trace (Trace_Filename, Result);
                        return;
                     end if;
                     Update_From_Binary_Trace (Trace_File.all, TF);
                     Exe_File := Open_Exec_For_Trace
                       (Trace_Filename, TF, Exec_Name_Override);
                     Free (TF);
                  end;
               end if;

               --  If there is no routine in list, get routine names from the
               --  first executable. A test earlier allows this only if there
               --  is one trace file.

               if Inputs.Length (Routines_Inputs) = 0 then
                  Read_Routine_Names (Exe_File.all, Exclude => False);
               end if;

               Build_Debug_Compile_Units (Exe_File.all);

               if Verbose and then Trace_File /= null then
                  Put_Line ("Processing traces from " & Trace_Filename);
               end if;

               Load_Code_And_Traces (Exe_File, Base'Access);
            end Process_Trace_For_Obj_Coverage;

            ------------------------------------
            -- Process_Trace_For_Src_Coverage --
            ------------------------------------

            procedure Process_Trace_For_Src_Coverage
              (Trace_File         : Trace_File_Element_Acc;
               Exec_Name_Override : String)
            is
               Exe_File : Exe_File_Acc;
               --  Executable this trace file refers to

               Trace_Filename : constant String :=
                 (if Trace_File = null
                  then ""
                  else To_String (Trace_File.Filename));

               Current_Exec            : Exe_File_Acc;
               Current_Sym             : Address_Info_Acc;
               Current_Subp_Key        : Subprogram_Key;
               Current_Subp_Info       : aliased Subprogram_Info;
               Current_Subp_Info_Valid : Boolean;

               procedure Process_Info_Entries
                 (TF     : Trace_File_Type;
                  Result : out Read_Result);

               function Load_Shared_Object
                  (TF          : Trace_File_Type;
                   Filename    : String;
                   Signature   : Binary_Files.Binary_File_Signature;
                   First, Last : Traces.Pc_Type) return Exe_File_Acc;

               procedure Process_Trace_Entry
                 (TF : Trace_File_Type; SO : Exe_File_Acc; E : Trace_Entry);

               procedure Read_Trace_File is new Read_Trace_File_Gen
                 (Shared_Object_Type   => Exe_File_Acc,
                  No_Shared_Object     => null,
                  Process_Info_Entries => Process_Info_Entries,
                  Load_Shared_Object   => Load_Shared_Object,
                  Process_Trace_Entry  => Process_Trace_Entry);

               --------------------------
               -- Process_Info_Entries --
               --------------------------

               procedure Process_Info_Entries
                 (TF     : Trace_File_Type;
                  Result : out Read_Result) is
               begin
                  Check_Trace_File_From_Exec (TF, Result);
                  if not Result.Success then
                     return;
                  end if;
                  Exe_File := Open_Exec_For_Trace
                    (Trace_Filename, TF, Exec_Name_Override);
                  Decision_Map.Analyze (Exe_File);
               end Process_Info_Entries;

               ------------------------
               -- Load_Shared_Object --
               ------------------------

               function Load_Shared_Object
                  (TF          : Trace_File_Type;
                   Filename    : String;
                   Signature   : Binary_Files.Binary_File_Signature;
                   First, Last : Traces.Pc_Type) return Exe_File_Acc
               is
                  pragma Unreferenced (TF);
                  pragma Unreferenced (First);
                  pragma Unreferenced (Last);
                  Result : Exe_File_Acc;
               begin
                  Open_Exec_For_Trace
                    (Filename, 0, Trace_Filename, Signature, Result);
                  Decision_Map.Analyze (Result);
                  return Result;
               end Load_Shared_Object;

               -------------------------
               -- Process_Trace_Entry --
               -------------------------

               procedure Process_Trace_Entry
                 (TF : Trace_File_Type; SO : Exe_File_Acc; E : Trace_Entry)
               is
                  pragma Unreferenced (TF);

                  Exe : constant Exe_File_Acc :=
                    (if SO = null then Exe_File else SO);
               begin
                  --  Get the symbol the trace entry is in

                  if Current_Sym = null or else Current_Exec /= Exe
                    or else
                     E.First not in Current_Sym.First .. Current_Sym.Last
                  then
                     Current_Exec := Exe;
                     Current_Sym :=
                       Get_Address_Info
                         (Exe.all, Symbol_Addresses, E.First);

                     if Current_Sym = null then
                        Current_Subp_Info_Valid := False;
                     else
                        Key_From_Symbol
                          (Exe, Current_Sym, Current_Subp_Key);
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
               end Process_Trace_Entry;

            --  Start of processing for Process_Trace_For_Src_Coverage

            begin
               --  Whether we have a trace file or just an executable, load
               --  symbols from executable (sets the rebase offset for each
               --  symbol) and perform static analysis.
               --
               --  If this is a trace file, also make it contribute to coverage
               --  state.

               if Trace_File = null then
                  Open_Exec (Exec_Name_Override, Text_Start, Exe_File);
                  Decision_Map.Analyze (Exe_File);
               else
                  declare
                     TF     : Trace_File_Type;
                     Result : Read_Result;
                  begin
                     Read_Trace_File (Trace_Filename, TF, Result);
                     if not Result.Success then
                        Report_Bad_Trace (Trace_Filename, Result);
                        return;
                     end if;
                     Update_From_Binary_Trace (Trace_File.all, TF);
                     Free (TF);
                  end;
               end if;
            end Process_Trace_For_Src_Coverage;

         begin
            if Inputs.Length (Checkpoints_Inputs) = 0 then
               Check_Argument_Available (Trace_Inputs, "TRACE_FILEs");
            end if;
            Inputs.Iterate (Exe_Inputs,  Process_Exec'Access);
            Inputs.Iterate (Trace_Inputs, Process_Trace'Access);

            --  Warn when using --scos with source traces or --sid with bin
            --  traces.

            if Inputs.Length (SID_Inputs) > 0 and then Bin_Traces_Present then
               Warn ("Using option --sid with binary trace files has no"
                     & " effect." & ASCII.LF & "Please consider using option"
                     & " --scos or -P<project file> in conjunction with"
                     & " --units to specify units of interest.");
            end if;
            if Inputs.Length (ALIs_Inputs) > 0 and then Src_Traces_Present then
               Warn ("Using option --scos with source trace files has no"
                     & " effect." & ASCII.LF & "Please consider using option"
                     & " --sid or -P<project file> in conjunction with --units"
                     & " to specify units of interest.");
            end if;
         end;

         --  Reconstruct unit names for ignored source files.
         --  This is done before loading checkpoints, because this will already
         --  have been done for the files in the checkpoints when creating
         --  them.

         if Project.Is_Project_Loaded
           and then Coverage.Source.Unit_List_Is_Valid
         then
            Compute_Unit_Name_For_Ignored_Sources;
         end if;

         --  Read checkpointed coverage data from previous executions

         Inputs.Iterate
           (Checkpoints_Inputs, Checkpoints.Checkpoint_Load'Access);

         --  Now determine coverage according to the requested metric (for
         --  source coverage, complete coverage information has been determined
         --  when loading traces above).

         if Object_Coverage_Enabled then
            Traces_Elf.Build_Routines_Insn_State;

            if Annotation /= Annotate_Asm then
               Traces_Elf.Build_Source_Lines;
            end if;
         end if;

         if Source_Coverage_Enabled and then Verbose then
            SC_Obligations.Report_Units_Without_Code;
         end if;

         --  Now that the list of files is final, we can create the map from
         --  units to ignored source files, if needed.

         if Dump_Units and then Coverage.Source.Unit_List_Is_Valid then
            Fill_Ignored_SF_Map;
         end if;

         declare
            Context : aliased Coverage.Context := Get_Context;

            Dump_Units_In_Report : constant Boolean :=
               Dump_Units
               and then Dump_Units_Filename = null
               and then Annotation = Annotate_Report;
            --  Whether the list of units of interest should be dumped in the
            --  coverage report itself.
         begin
            --  If the dump of the list of units of interest is requested, make
            --  sure we can do it.

            if Dump_Units and not Unit_List_Is_Valid then
               Fatal_Error ("Cannot dump the list of names for units of"
                            & " interest: see above.");
            end if;

            --  If we must dump the list of units of interest in a dedicated
            --  file or on the standard output, do it now.

            if Dump_Units and then not Dump_Units_In_Report then
               declare
                  File : aliased File_Type;
                  --  Output file for the list of names for units of interest

                  Output : constant access File_Type :=
                    (if Dump_Units_Filename = null
                     then Standard_Output
                     else File'Access);

                  procedure Print_Unit_Name (Name : String);
                  --  Print the name of the file and if it was always or
                  --  sometimes ignored on the report, if it was ignored at
                  --  some point during the coverage analysis.

                  procedure Print_Ignored_File
                    (FI : Files_Table.File_Info);

                  procedure Print_Ignored_File
                    (FI : Files_Table.File_Info) is
                  begin
                     if FI.Ignore_Status = Files_Table.Sometimes then
                        Put_Line (Output.all, "   " & FI.Unique_Name.all
                                  & " sometimes ignored");
                     elsif FI.Ignore_Status = Files_Table.Always then
                        Put_Line (Output.all, "   " & FI.Unique_Name.all
                                  & " always ignored");

                     end if;
                  end Print_Ignored_File;

                  procedure Print_Unit_Name (Name : String) is
                  begin
                     Put_Line (Output.all, Name);
                  end Print_Unit_Name;

               begin
                  if Dump_Units_Filename /= null then
                     Create (File, Name => Dump_Units_Filename.all);
                  end if;
                  Iterate_On_Unit_List
                    (Print_Unit_Name'Access, Print_Ignored_File'Access);
               end;
            end if;

            --  Generate annotated reports

            if Emit_Report then
               case Annotation is
               when Annotate_Asm =>
                  if Source_Coverage_Enabled then
                     Fatal_Error
                       ("""asm"" output format (from --annotate) is"
                          & " only for object coverage criteria"
                          & ASCII.LF
                          & "  (--level=" & Object_Level_Options ("|")
                          & ", not --level="
                          & Coverage_Option_Value (Current_Levels) & ")");
                  end if;
                  Traces_Dump.Dump_Routines_Traces (Output);

               when Annotate_Xml =>
                  Annotations.Xml.Generate_Report
                    (Context'Unchecked_Access);

               when Annotate_Xcov      |
                    Annotate_Xcov_Plus =>
                  Annotations.Xcov.Generate_Report
                    (Context'Unchecked_Access,
                     Show_Details => Annotation = Annotate_Xcov_Plus);

               when Annotate_Html      |
                    Annotate_Html_Plus =>
                  Annotations.Html.Generate_Report
                    (Context'Unchecked_Access,
                     Show_Details => Annotation = Annotate_Html_Plus,
                     Report_Title => Args.String_Args (Opt_Report_Title));

               when Annotate_Dynamic_Html =>
                  Annotations.Dynamic_Html.Generate_Report
                    (Context'Unchecked_Access,
                     Report_Title => Args.String_Args (Opt_Report_Title));

               when Annotate_Report =>
                  Annotations.Report.Generate_Report
                    (Context'Unchecked_Access, Output, Dump_Units_In_Report);

               when Annotate_Unknown =>
                  pragma Assert (Save_Checkpoint /= null);
               end case;
            else
               pragma Assert (Save_Checkpoint /= null);
            end if;

            --  Generate checkpoint, if requested. Use the last version as
            --  soon as instrumentation is involved, as the default version
            --  does not allow us to encode all the information we need to
            --  encode.

            if Save_Checkpoint /= null then
               Checkpoints.Checkpoint_Save
                 (Save_Checkpoint.all,
                  Context'Access,
                  Purpose => Checkpoints.Consolidation);
            end if;
         end;

      when Cmd_Run =>
         Check_Argument_Available (Exe_Inputs, "EXE");

         declare
            procedure Run (Exe_File : String);
            --  Run Exe_File in QEMU

            ---------
            -- Run --
            ---------

            procedure Run (Exe_File : String) is
               package OS renames GNAT.OS_Lib;
               use type OS.File_Descriptor;

               Dmap_FD       : OS.File_Descriptor := OS.Null_FD;
               Dmap_Filename : OS.Temp_File_Name;
               Success       : Boolean;
               Histmap       : String_Access;
            begin
               if MCDC_Coverage_Enabled then
                  Load_All_ALIs (Check_SCOs => False);
                  if Length (ALIs_Inputs) = 0 then
                     Warn ("No SCOs specified for MC/DC level.");

                  else
                     --  Create a temporary file to hold the decision map

                     OS.Create_Temp_File (Dmap_FD, Dmap_Filename);
                     if Dmap_FD = OS.Invalid_FD then
                        Fatal_Error ("Cannot open a temporary file for the"
                                     & "decision map");
                     end if;
                     OS.Close (Dmap_FD, Success);
                     if not Success then
                        Fatal_Error ("Cannot close the decision map temporary"
                                     & " file: " & Dmap_Filename);
                     end if;

                     --  For some reason, Create_Temp_File leaves a NUL byte at
                     --  the end of Dmap_Filename. We need to get rid of it.

                     pragma Assert
                       (Dmap_Filename (OS.Temp_File_Name'Last) = ASCII.NUL);
                     Histmap := new String'
                       (Dmap_Filename (1 ..  OS.Temp_File_Len - 1));

                     Build_Decision_Map (Exe_File, Text_Start, Histmap.all);
                  end if;
               end if;

               Rundrv.Driver (Exe_File, Target_Family, Target_Board, Tag,
                              Output, Histmap, Kernel, Vector_To_List (Eargs),
                              SO_Inputs);

               --  Now that we are done with the decision map file, make sure
               --  we remove it to avoid polluting users' filesystems.

               if Histmap /= null then
                  OS.Delete_File (Histmap.all, Success);
                  if not Success then
                     Warn
                       ("Could not delete the temporary decision map file: "
                        & Histmap.all);
                  end if;
                  Free (Histmap);
               end if;
            end Run;
         begin
            Inputs.Iterate (Exe_Inputs, Run'Access);
         end;

      when Cmd_Convert =>
         declare
            Exec    : constant String :=
              +Args.String_List_Args (Opt_Exec).Last_Element;
            Histmap : constant String :=
              Exec & ".dmap";
         begin
            if MCDC_Coverage_Enabled then
               Load_All_ALIs (Check_SCOs => False);
               if Length (ALIs_Inputs) = 0 then
                  Warn ("No SCOs specified for MC/DC level.");

               else
                  Build_Decision_Map (Exec, Text_Start, Histmap);
               end if;
            end if;

            --  TODO??? Run_Convert is not supposed to write to Exec. Actually,
            --  it uses the String_Access as a way to know if the user passed
            --  an executable argument, which is not needed now we have a clean
            --  way to process arguments. Likwise for Histmap.

            Convert.Run_Convert (Exec'Unrestricted_Access, Output,
                                 Histmap'Unrestricted_Access, Tag);
         end;

      when Cmd_Extract_Base64_Trace =>
         if Args.Remaining_Args.Length /= 2 then
            Fatal_Error ("Exactly two arguments expected");
         end if;
         Instrument.Input_Traces.Extract_Base64_Trace
           (Input_File  => +Args.Remaining_Args (0),
            Output_File => +Args.Remaining_Args (1));

   end case;

   if Verbose then
      Perf_Counters.Display;
   end if;

   Project.Finalize;

exception
   --  The following handler is for files that gnatcov could not locate or
   --  open. Display the corresponding error message.

   when Error : Binary_Files.Error
      | Ada.IO_Exceptions.Name_Error =>
      Outputs.Error (Ada.Exceptions.Exception_Message (Error));
      Project.Finalize;

   --  Each chunk of code with "raise" statements for Xcov_Exit_Exc exceptions
   --  is supposed to print an error message and set a failure exit status
   --  before raising the exception.

   when Xcov_Exit_Exc =>
      Project.Finalize;
end GNATcov_Bits_Specific;
