------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Ada.Containers;  use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;     use Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.Regexp;
with GNAT.Strings; use GNAT.Strings;

with System.Multiprocessors;

with GPR2.Project.Registry.Exchange;

with Snames;

with Annotations.Cobertura;
with Annotations.Sarif;
with Annotations.Dynamic_Html;
with Annotations.Html;
with Annotations.Report;
with Annotations.Xcov;
with Annotations.Xml;
with Annotations.Index;
with Annotations;           use Annotations;
with Binary_Files;
with CFG_Dump;
with Calendar_Utils;
with Check_SCOs;
with Checkpoints;
with Command_Line;          use Command_Line;
use Command_Line.Parser;
with Convert;
with Coverage.Source;       use Coverage.Source;
with Coverage.Tags;         use Coverage.Tags;
with Coverage;              use Coverage;
with Coverage_Options;      use Coverage_Options;
with Decision_Map;          use Decision_Map;
with Disassemble_Insn_Properties;
with Execs_Dbase;           use Execs_Dbase;
with Files_Handling;        use Files_Handling;
with Files_Table;           use Files_Table;
with Inputs;                use Inputs;
with Instrument;
with Instrument.Common;     use Instrument.Common;
with Instrument.Config;
with Instrument.Gcc_Wrapper;
with Instrument.Input_Traces;
with Instrument.Main;
with Instrument.Projects;
with Instrument.Setup_Config;
with Instrument.Source;
with Logging;
with Object_Locations;
with Outputs;               use Outputs;
with Perf_Counters;
with Project;               use Project;
with Qemu_Traces;
with Rundrv;                use Rundrv;
with SC_Obligations;        use SC_Obligations;
with Setup_RTS;             use Setup_RTS;
with SS_Annotations;        use SS_Annotations;
with Strings;               use Strings;
with Switches;              use Switches;
with Temp_Dirs;             use Temp_Dirs;
with Traces;                use Traces;
with Traces_Dbase;          use Traces_Dbase;
with Traces_Disa;
with Traces_Dump;
with Traces_Elf;            use Traces_Elf;
with Traces_Files;          use Traces_Files;
with Traces_Files_Registry; use Traces_Files_Registry;
with Traces_Names;          use Traces_Names;
with Traces_Source;
with Types;                 use Types;
with Version;

procedure GNATcov_Bits_Specific is

   --  This is the main of the bits-specific gnatcov program. Only the gnatcov
   --  driver (see gnatcov.adb) is supposed to run this main.

   use all type Unbounded_String;

   --  Results of the command line processing. It is filled by
   --  Process_Arguments once Switches.Args reached its final state.

   Annotation           : Annotation_Formats_Arr renames
     Annotations.Annotation;
   Trace_Inputs         : Requested_Trace_Vectors.Vector;
   Exe_Inputs           : String_Vectors.Vector;
   Obj_Inputs           : String_Vectors.Vector;
   ALIs_Inputs          : String_Vectors.Vector;
   Routines_Inputs      : String_Vectors.Vector;
   Checkpoints_Inputs   : String_Vectors.Vector;
   SID_Inputs           : String_Vectors.Vector;
   Ignored_Source_Files : String_Vectors.Vector;
   Files_Of_Interest    : String_Vectors.Vector;
   Compiler_Drivers     : String_Vectors.Vector;
   Source_Rebase_Inputs : String_Vectors.Vector;
   Source_Search_Inputs : String_Vectors.Vector;
   Subprograms_Inputs   : String_Vectors.Vector;
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

   Runtime_Project : String_Access;
   --  For "gnatcov setup", project file name for the instrumentation runtime
   --  to build and install.

   procedure Process_Arguments;
   --  Process all the arguments present in Args and forward them to local
   --  varibles.

   procedure Show_Version;
   --  Show gnatcov version

   procedure Show_CWD;
   --  Show the current working directory

   procedure Report_Missing_Argument
     (What            : String;
      Additional_Info : String := "")
   with No_Return;
   --  Report a fatal error telling the user that an argument is missing.

   procedure Check_Argument_Available
     (Input_Args      : String_Vectors.Vector;
      What            : String;
      Additional_Info : String := "");
   --  Invoke Report_Missing_Argument if Input_Args is empty

   procedure Check_Traces_Available;
   --  Invoke Report_Missing_Argument if Trace_Inputs is empty

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
      with Pre => not Is_Success (Result);
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
     (Input_Args      : String_Vectors.Vector;
      What            : String;
      Additional_Info : String := "")
   is
   begin
      if Input_Args.Is_Empty then
         Report_Missing_Argument (What, Additional_Info);
      end if;
   end Check_Argument_Available;

   ----------------------------
   -- Check_Traces_Available --
   ----------------------------

   procedure Check_Traces_Available is
   begin
      if Trace_Inputs.Is_Empty then
         Report_Missing_Argument ("TRACE_FILEs");
      end if;
   end Check_Traces_Available;

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
         and then Checkpoints_Inputs.Is_Empty
         and then SID_Inputs.Is_Empty
         and then ALIs_Inputs.Is_Empty
         and then not Args.String_Args (Opt_Project).Present
      then
         Report_Missing_Argument
           ("SCOs",
            ", specifying Units in project or using "
            & "[--units and -P]|--scos|--sid");
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

      ------------
      -- Add_LI --
      ------------

      procedure Add_LI (S : String) is
      begin
         Append_Expanded_Argument (S, ALIs_Inputs);
      end Add_LI;

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

      if ALIs_Inputs.Is_Empty and then Is_Project_Loaded then
         Enumerate_SCOs_Files (Add_LI'Access, Binary_Trace_File);
      end if;

      if Source_Coverage_Enabled then

         --  Load SCOs from ALI files and initialize source coverage data
         --  structures.

         Create_Matcher (Ignored_Source_Files, Matcher, Has_Matcher);
         for Filename of ALIs_Inputs loop
            Load_SCOs
              (+Filename, (if Has_Matcher then Matcher'Access else null));
         end loop;
         Coverage.Source.Initialize_SCI;

      elsif Object_Coverage_Enabled then

         --  For object coverage, just load ALIs (not SCOs inside them) just to
         --  get exemptions as they apply to instruction/branch coverage.

         for Filename of ALIs_Inputs loop
            Load_ALI (+Filename);
         end loop;
      end if;

      --  If subprograms of interest were passed warn the user that they will
      --  be ignored

      if not Args.String_List_Args (Opt_Subp_Of_Interest).Is_Empty then
         Warn
           ("Ignoring --subprograms switches as this is not supported with"
            & " binary traces.");
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

      ------------------
      -- Add_SID_File --
      ------------------

      procedure Add_SID_File (SID_Name : String) is
      begin
         Append_Expanded_Argument (SID_Name, SID_Inputs);
      end Add_SID_File;

   --  Start of processing for Load_All_SIDs

   begin
      if SIDs_Loaded then
         return;
      end if;
      SIDs_Loaded := True;

      --  If no --sid option is present, enumerate all available SID for units
      --  of interest. This requires a project file.

      if Is_Project_Loaded and then SID_Inputs.Is_Empty then
         Enumerate_SCOs_Files (Add_SID_File'Access, Source_Trace_File);
      end if;

      --  Now load the SID files, applying the Ignore_Source_Files filter,
      --  if present.

      Create_Matcher (Ignored_Source_Files, Matcher, Has_Matcher);
      for Filename of SID_Inputs loop
         Checkpoints.SID_Load
           (+Filename, (if Has_Matcher then Matcher'Access else null));
      end loop;

      --  Now that all the scope entities that can be referenced by
      --  --subprograms are known, dump them if requested.

      if Scope_Entities_Trace.Is_Active then
         for CU in 1 .. Last_CU loop
            Scope_Entities_Trace.Trace ("Scopes for " & Image (CU) & ":");
            Dump (Get_Scope_Entities (CU), Line_Prefix => "| ");
         end loop;
      end if;

      --  Parse the listed subprograms of interest

      Copy_Arg_List (Opt_Subp_Of_Interest, Subprograms_Inputs);
      for Name of Subprograms_Inputs loop
         declare
            N            : constant String := +Name;
            Column_Index : constant Natural :=
              Ada.Strings.Fixed.Index (N, Ada.Strings.Maps.To_Set (':'));
            Filename     : String renames N (N'First .. Column_Index - 1);
            Column       : String renames N (Column_Index + 1 .. N'Last);

            Identifier : Scope_Entity_Identifier;
         begin
            if Column_Index = 0 then
               raise Constraint_Error;
            end if;
            Identifier.Decl_SFI :=
              Get_Index_From_Full_Name
                (Full_Name => Full_Name (Filename),
                 Kind      => Source_File,
                 Insert    => False);
            if Identifier.Decl_SFI = No_Source_File then
               Outputs.Fatal_Error
                 ("Error when parsing --subprograms argument "
                  & N & ": unknown source file");
            end if;
            Identifier.Decl_Line := Natural'Value (Column);

            if not Available_Subps_Of_Interest.Contains (Identifier) then
               Outputs.Fatal_Error
                 ("Error when parsing --subprograms argument "
                  & N & ": unknown subprogram");
            end if;
            Subps_Of_Interest.Include (Identifier);
         exception
            --  Deal gracefully with parsing errors

            when Constraint_Error =>
               Outputs.Fatal_Error
                 ("Wrong argument passed to --subprograms: "
                  & "expecting <file>:<line> but got " & N);
         end;
      end loop;
   end Load_All_SIDs;

   -----------------------
   -- Process_Arguments --
   -----------------------

   procedure Process_Arguments is

      function Parse_Hex (S : String; Flag_Name : String) return Pc_Type;
      --  Parse S to get an hexadecimal number (form : 0x[0-9a-f]+) and
      --  return the value. If the parsing fails, fatal error.

      Current_Exec : Unbounded_String;
      --  Some arguments specify what executable the next traces will have to
      --  refer to: this holds the current executable for the next traces.

      procedure Handle_Trace_List_Element (Element : String);
      --  If Element starts with ASCII.NUL, consider it comes from --exec and
      --  assign it to Current_Exec. Otherwise, add it to Trace_Inputs.

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
         if Has_Prefix (Element, (1 => ASCII.NUL)) then
            Current_Exec := +Element (Element'First + 1 .. Element'Last);
         else
            Trace_Inputs.Append ((+Element, Current_Exec));
         end if;
      end Handle_Trace_List_Element;

   --  Start of processing for Process_Arguments

   begin
      --  First, handle all options...

      Switches.All_Decisions   := Args.Bool_Args (Opt_All_Decisions);
      Switches.All_Messages    := Args.Bool_Args (Opt_All_Messages);
      Excluded_SCOs            := Args.Bool_Args (Opt_Excluded_SCOs);
      Keep_Edges               := Args.Bool_Args (Opt_Keep_Edges);
      Pretty_Print             := Args.Bool_Args (Opt_Pretty_Print);
      Keep_Reading_Traces      := Args.Bool_Args (Opt_Keep_Reading_Traces);
      Dump_Units               := Args.String_Args (Opt_Dump_Units_To).Present;
      Show_MCDC_Vectors        := (Args.Bool_Args (Opt_Show_MCDC_Vectors)
                                   or else All_Messages);
      Show_Condition_Vectors   := (Args.Bool_Args (Opt_Show_Condition_Vectors)
                                   or else All_Messages);
      Allow_Mixing_Trace_Kinds := Args.Bool_Args (Opt_Allow_Mix_Trace_Kind);
      Short_Circuit_And_Or     := Args.Bool_Args
                                    (Opt_Boolean_Short_Circuit_And_Or);
      Emit_Report              := not Args.Bool_Args (Opt_Cancel_Annotate);
      Save_Temps               := Args.Bool_Args (Opt_Save_Temps);
      SPARK_Compat             := Args.Bool_Args (Opt_SPARK_Compat);
      Use_Full_Slugs           := Args.Bool_Args (Opt_Full_Slugs);
      Force                    := Args.Bool_Args (Opt_Force);

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
      if not ALIs_Inputs.Is_Empty then
         Invalidate_Unit_List ("--scos is present");
      end if;

      Copy_Arg_List (Opt_SID, SID_Inputs);
      if not SID_Inputs.Is_Empty then
         Invalidate_Unit_List ("--sid is present");
      end if;

      Copy_Arg_List (Opt_Routines, Routines_Inputs);
      Copy_Arg_List (Opt_Exec, Exe_Inputs);

      if not Args.String_List_Args (Opt_Checkpoint).Is_Empty
        and then not Args.String_List_Args (Opt_Units).Is_Empty
      then
         Warn ("Specifying units of interest through --units has no effect on "
               & "checkpoints");
      end if;

      Copy_Arg_List (Opt_Checkpoint, Checkpoints_Inputs);

      Copy_Arg_List (Opt_Ignore_Source_Files, Ignored_Source_Files);
      Copy_Arg_List (Opt_Files, Files_Of_Interest);
      Copy_Arg_List (Opt_Compiler_Wrappers, Compiler_Drivers);

      for File of Files_Of_Interest loop
         Switches.Files_Of_Interest.Include (Create_Normalized (+File));
      end loop;

      --  Compute the languages for which we want coverage analysis, or enable
      --  just the default ones.

      if Args.String_List_Args (Opt_Restricted_To_Languages).Is_Empty then
         Src_Enabled_Languages :=
           (Ada_Language => True,
            C_Language   => True,
            CPP_Language => True);
      else
         for Arg of Args.String_List_Args (Opt_Restricted_To_Languages) loop
            Src_Enabled_Languages (To_Language (+Arg)) := True;
         end loop;
      end if;

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

      if not Args.String_List_Args (Opt_Annotation_Format).Is_Empty then

         --  Add each requested report kind to the enabled annotations, while
         --  checking that all the options passed on the command line are
         --  valid.

         Annotation (Annotate_Unknown) := False;

         for Arg of Args.String_List_Args (Opt_Annotation_Format) loop
            Annotation (To_Annotation_Format (+Arg)) := True;
            if Annotation (Annotate_Unknown) then
               Fatal_Error ("Bad annotation format: " & (+Arg));
            end if;
         end loop;

         --  Check if there is more than one annotation format which requires
         --  an output dir.

         declare
            Format_Count : Natural := 0;
         begin
            for Format in Annotate_Xcov .. Annotate_Xml loop
               if Annotation (Format) then
                  Format_Count := Format_Count + 1;
               end if;
            end loop;
            Annotations.Multiple_Reports := Format_Count > 1;
         end;

      end if;

      for Arg of Args.String_List_Args (Opt_Routines_List) loop
         Routines_Inputs.Append (+"@" & Arg);
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

      --  Parse --source-rebase options. This option's form should be:
      --
      --    "<OLD_PREFIX>=<NEW_PREFIX>".

      Copy_Arg_List (Opt_Source_Rebase, Source_Rebase_Inputs);
      for Arg of Source_Rebase_Inputs loop
         declare
            Pos : Natural := 0;
         begin
            for I in 1 .. Length (Arg) loop
               if Element (Arg, I) = '=' then
                  Pos := I;
                  exit;
               end if;
            end loop;
            if Pos = 0 then
               Fatal_Error ("Missing '=' in --source-rebase");
            end if;
            Add_Source_Rebase
              (Old_Prefix => Slice (Arg, 1, Pos - 1),
               New_Prefix => Slice (Arg, Pos + 1, Length (Arg)));
         end;
      end loop;

      --  Parse --source-search options

      Copy_Arg_List (Opt_Source_Search, Source_Search_Inputs);
      for Dirname of Source_Search_Inputs loop
         Add_Source_Search (+Dirname);
      end loop;

      if Args.String_Args (Opt_Exec_Prefix).Present then
         Set_Exec_Prefix (+Args.String_Args (Opt_Exec_Prefix).Value);
      end if;

      if Args.String_Args (Opt_Output_Directory).Present then
         Outputs.Set_Output_Dir
           (+Args.String_Args (Opt_Output_Directory).Value);
      end if;

      for Arg of Args.String_List_Args (Opt_Trace) loop
         for Exp_Arg of Expand_Argument (+Arg) loop
            Handle_Trace_List_Element (+Exp_Arg);
         end loop;
      end loop;

      if Args.String_Args (Opt_Trace_Source).Present then
         Convert.Set_Trace_Source (+Args.String_Args (Opt_Trace_Source).Value);
      end if;

      if Args.String_Args (Opt_Separate).Present then
         Warn ("-S is deprecated. This option will be removed in release 26.");
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
               if not SO_Inputs.Set.Is_Empty then
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
                  when Some_SO       => SO_Inputs.Set.Append (Arg);
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

      --  Import all external annotation files (this does not yet match the
      --  entries on actual source files) and validate that the annotations
      --  relevant to gnatcov are well formed.

      for Arg of Args.String_List_Args (Opt_Ext_Annotations) loop
         Load_Ext_Annotations (Arg);
      end loop;
      Validate_Annotations;

      --  ... then, handle remaning arguments, which have subcommand-specific
      --  meanings.

      case Args.Command is
         when Cmd_Setup =>

            --  Accept one optional argument: the project file name for the
            --  instrumentation runtime to build and install.

            if Args.Remaining_Args.Is_Empty then
               Runtime_Project := new String'(Setup_RTS.Default_Project_File);
            elsif Args.Remaining_Args.Length = 1 then
               declare
                  Prj : constant String := +Args.Remaining_Args.Element (0);
               begin
                  Runtime_Project := new String'
                    (if GNAT.OS_Lib.Is_Absolute_Path (Prj)
                     then Prj
                     else (Current_Directory & "/" & Prj));
               end;
            else
               Fatal_Error ("at most one positional argument allowed");
            end if;

         when Cmd_Coverage
            | Cmd_Dump_Trace
            | Cmd_Dump_Trace_Raw
            | Cmd_Dump_Trace_Base
            | Cmd_Dump_Trace_Asm
            | Cmd_Dump_Src_Trace =>

            --  For "coverage", if the annotation format is not specified on
            --  the command line, default to report, unless we must save a
            --  checkpoint.

            if Args.Command = Cmd_Coverage
              and then Args.String_List_Args (Opt_Annotation_Format).Is_Empty
              and then Save_Checkpoint = null
            then
               Annotation (Annotate_Report) := True;
               Annotation (Annotate_Unknown) := False;
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

            --  If a source encoding is specified, use it to decode source
            --  files.

            if Args.String_Args (Opt_Source_Encoding).Present then
               Set_Encoding (+Args.String_Args (Opt_Source_Encoding).Value);
            end if;

            --  Remaining arguments are supposed to be copied to Opt_Trace,
            --  which is already handled, so there's nothing left to do with
            --  them.

         when Cmd_Disp_Routines
            | Cmd_Scan_Objects =>
            Copy_Arg_List (Args.Remaining_Args, Obj_Inputs);

         when Cmd_Dump_Sections
            | Cmd_Dump_Symbols
            | Cmd_Dump_Compile_Units
            | Cmd_Dump_Subprograms
            | Cmd_Dump_Inlined_Subprograms
            | Cmd_Dump_Lines
            | Cmd_Disassemble_Raw
            | Cmd_Disassemble =>
            Copy_Arg_List (Args.Remaining_Args, Exe_Inputs);

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
            Copy_Arg_List (Args.Remaining_Args, Exe_Inputs);

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
                        Append_Expanded_Argument (Earg0, Exe_Inputs);
                        loop
                           Next (C);
                           exit when not Has_Element (C);
                           Eargs.Append (Element (C));
                        end loop;

                     elsif Exe_From_Project /= "" then
                        Append_Expanded_Argument
                          (Exe_From_Project, Exe_Inputs);
                        Eargs := Eargs_Arg;
                     else
                        Report_Missing_Argument ("an executable to run (EXE)");
                     end if;

                  end;

               when 1 =>

                  --  We have single executable argument on the base command
                  --  line (before eargs). Use it and forward all the EARGS
                  --  options we have to the Eargs local.

                  Append_Expanded_Argument
                    (+Args.Remaining_Args.First_Element, Exe_Inputs);
                  Eargs := Args.String_List_Args (Opt_Eargs);

               when others =>

                  --  We have more than one non-earg trailing argument on the
                  --  base command line, complain.

                  Fatal_Error ("Only one EXEC parameter is allowed");
            end case;

         when Cmd_Check_SCOs =>
            Copy_Arg_List (Args.Remaining_Args, ALIs_Inputs);

         when Cmd_Convert =>
            if Args.String_List_Args (Opt_Exec).Is_Empty then
               Fatal_Error
                 (Option_Name (Arg_Parser, (String_List_Opt, Opt_Exec))
                  & " is missing (required for ""convert"")");
            end if;

         when Cmd_Instrument_With_Setup =>

            --  Ensure we have a source coverage level. If not, we might have
            --  either an object level specified, or possibly no --level at
            --  all.

            if Object_Coverage_Enabled then
               Fatal_Error
                 ("Instrumentation requires a source coverage level"
                  & ASCII.LF
                  & "  (--level=" & Source_Level_Options & ")");
            elsif not Source_Coverage_Enabled then
               Warn
                 ("Coverage level not specified on the command"
                  & " line or in the project file (--level="
                  & Source_Level_Options & "), defaulting to ""stmt"".");
               Set_Coverage_Levels ("stmt");
            end if;

            if Args.String_Args (Opt_Path_Count_Limit).Present then
               declare
                  Limit : Positive;
               begin
                  begin
                     Limit :=
                       Positive'Value
                         (+Args.String_Args (Opt_Path_Count_Limit).Value);
                  exception
                     when Constraint_Error =>
                        Fatal_Error ("--path-count-limit expects a positive"
                                     & " integer value");
                  end;
                  SC_Obligations.Set_Path_Count_Limit (Limit);
               end;
            end if;

            Copy_Arg_List (Opt_C_Opts, C_Opts);
            Copy_Arg_List (Opt_CPP_Opts, CPP_Opts);

            if Args.String_Args (Opt_Parallelism_Level).Present then
               declare
                  Parallelism_Level : Natural;
               begin
                  begin
                     Parallelism_Level :=
                       Natural'Value
                         (+Args.String_Args (Opt_Parallelism_Level).Value);
                  exception
                     when Constraint_Error =>
                        Fatal_Error ("Parallelism level (-j or --jobs)"
                                     & " must be a natural integer value");
                  end;

                  --  Limit the number of spawned subprocesses to the number
                  --  of cores.

                  if Parallelism_Level = 0 then
                     Instrument.Parallelism_Level :=
                       Positive (System.Multiprocessors.Number_Of_CPUs);
                  else
                     Instrument.Parallelism_Level := Parallelism_Level;
                  end if;
               end;
            end if;

         when others =>
            null;
      end case;

      --  Compute the set of units of interest from the command line and
      --  project file switches. If no project option was specified (and thus,
      --  no root project was loaded) but a project filtering option was, exit
      --  with an error.

      if Args.String_Args (Opt_Project).Present then

         for Arg of Args.String_List_Args (Opt_Projects) loop
            Project.Add_Project (+Arg);
         end loop;

         Switches.Recursive_Projects :=
           not Args.Bool_Args (Opt_No_Subprojects);
         Copy_Arg_List (Opt_Units, Units_Inputs);
         Project.Compute_Units_Of_Interest (Units_Inputs);

      else
         if not Args.String_List_Args (Opt_Units).Is_Empty then
            Fatal_Error ("--units requires -P");
         end if;
         if not Args.String_List_Args (Opt_Projects).Is_Empty then
            Fatal_Error ("--projects requires -P");
         end if;
         if Args.Bool_Args (Opt_No_Subprojects) then
            Fatal_Error ("--no-subprojects requires -P");
         end if;
      end if;

      if Ignored_Source_Files.Is_Empty and then Is_Project_Loaded then
         declare
            procedure Add_Source_File (S : String);
            --  Add S to the list of ignored source files

            ---------------------
            -- Add_Source_File --
            ---------------------

            procedure Add_Source_File (S : String) is
            begin
               Append_Expanded_Argument (S, Ignored_Source_Files);
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
      Message : constant String := Trace_Filename & ": " & (+Result.Error);
   begin
      if Result.Kind in Recoverable_Read_Result_Kind then
         Outputs.Warn (Message);
      elsif Keep_Reading_Traces then
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

   if Misc_Trace.Is_Active then
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
         if not Misc_Trace.Is_Active then
            Show_Version;
         end if;

      when Cmd_List_Logs =>
         Logging.Print_List;

      when Cmd_Print_GPR_Registry =>
         declare
            use GPR2.Project.Registry.Exchange;
            Format_Arg : constant String :=
              Value (Args, Opt_GPR_Registry_Format, "json");
            Format     : Export_Format;
         begin
            if Format_Arg = "text" then
               Format := K_TEXT;
            elsif Format_Arg = "json" then
               Format := K_JSON;
            elsif Format_Arg = "json-compact" then
               Format := K_JSON_COMPACT;
            else
               Fatal_Error ("Bad GPR registry format: " & Format_Arg);
            end if;
            Export (Format => Format, Output => Put'Access);
         end;

      when Cmd_Disp_Routines =>
         declare
            Mode_Exclude : Boolean := False;
         begin
            Check_Argument_Available (Obj_Inputs, "FILEs");
            for Arg of Obj_Inputs loop
               if Arg = "--exclude" then
                  Mode_Exclude := True;
               elsif Arg = "--include" then
                  Mode_Exclude := False;
               else
                  Traces_Elf.Read_Routine_Names
                    (+Arg,
                     Exclude => Mode_Exclude,
                     Strict  => False);
               end if;
            end loop;
            Traces_Names.Disp_All_Routines_Of_Interest;
         end;

      when Cmd_Setup =>
         declare
            Project_File : String renames Runtime_Project.all;

            --  If --target was passed, get the target family (the option may
            --  also contain board info). Otherwise use the default target.
            --
            --  Note that when --target is not passed, Target_Family is set to
            --  the native 64-bit platform. This is the correct semantics for
            --  all gnatcov commands except "gnatcov setup", which is meant to
            --  behave just like gprbuild with equivalent arguments (i.e. as if
            --  users ran gprbuild themselves with the same
            --  -P/--target/--config arguments). So use Target_Family only when
            --  --target is passed.

            Target_Arg : constant String := Value (Args, Opt_Target);
            Target     : constant String :=
              (if Target_Arg = ""
               then ""
               else Target_Family.all);

            --  Unless a specific installation name is requested, use the name
            --  of the project to build and install.

            Install_Name : constant String :=
              Value
                 (Args,
                  Opt_Install_Name,
                  Setup_RTS.Project_Name (Project_File));

            RTS_Profile_Str : constant String :=
              Value (Args, Opt_RTS_Profile, "auto");
            RTS_Profile     : Any_RTS_Profile;
         begin
            --  Decode the --rts-profile option

            if RTS_Profile_Str = "auto" then
               RTS_Profile := Auto;
            elsif RTS_Profile_Str = "full" then
               RTS_Profile := Full;
            elsif RTS_Profile_Str = "embedded" then
               RTS_Profile := Embedded;
            else
               Fatal_Error ("Invalid RTS profile: " & RTS_Profile_Str);
            end if;

            Setup
              (Project_File => Project_File,
               Target       => Target,
               RTS          => Value (Args, Opt_Runtime),
               Config_File  => Value (Args, Opt_Config),
               Db_Dir       => Value (Args, Opt_Db),
               Prefix       => Value (Args, Opt_Prefix),
               RTS_Profile  => RTS_Profile,
               Install_Name => Install_Name,
               Gargs        => Args.String_List_Args (Opt_Gargs));
         end;

      when Cmd_Instrument_Project =>
         if not Is_Project_Loaded then
            Fatal_Error ("instrumentation requires a project file;"
                         & " please use the -P option");
         end if;

         declare
            --  Try to load the setup config from metadata installed with
            --  instrumentation runtime, and from there, decode the --dump-*
            --  options.

            Runtime_Project : constant String :=
              Value (Args, Opt_Runtime_Project, "gnatcov_rts");

            Setup_Cfg   : constant Setup_Config := Load
              (Project.Target,
               Project.Runtime,
               Value (Args, Opt_Config),
               Runtime_Project);
            Dump_Config : constant Any_Dump_Config :=
              Load_Dump_Config (Setup_Cfg.Default_Dump_Config);

            Matcher     : aliased GNAT.Regexp.Regexp;
            Has_Matcher : Boolean;
            --  Matcher for the source files to ignore

         begin
            Create_Matcher (Ignored_Source_Files, Matcher, Has_Matcher);

            declare
               V : constant String := Value (Args, Opt_Ada, "2012");
            begin
               if not Set_Language_Version (Global_Language_Version, From => V)
               then
                  Fatal_Error ("Bad Ada language version: " & V);
               end if;
            end;

            --  Emit warnings if we detect an incompatibility between the
            --  selected RTS and the selected dump configuration.

            if Setup_Cfg.RTS_Profile_Present
               and then Check_RTS_Profile (Setup_Cfg.RTS_Profile, Dump_Config)
            then
               Warn
                 ("(selected runtime from " & (+Setup_Cfg.Project_File) & ")");
            end if;

            Instrument.Projects
              (Dump_Config          => Dump_Config,
               Ignored_Source_Files =>
                 (if Has_Matcher then Matcher'Access else null),
               Mains                => Args.Remaining_Args);
         end;

      when Cmd_Setup_Integration =>

         --  The integrated instrumentation scheme does not support Ada, so
         --  disable it.

         Src_Enabled_Languages (Ada_Language) := False;

         declare
            --  Try to load the setup config from metadata installed with
            --  instrumentation runtime, and from there, decode the --dump-*
            --  options.

            Runtime_Project : constant String :=
              Value (Args, Opt_Runtime_Project, "gnatcov_rts");

            Setup_Cfg : constant Setup_Config := Load
              (Target          => "",
               RTS             => "",
               Config_File     => Value (Args, Opt_Config),
               Runtime_Project => Runtime_Project);
            --  TODO??? We should not leave the target and runtime empty, but
            --  we have no project to load here.

            Dump_Config : constant Any_Dump_Config :=
              Load_Dump_Config (Setup_Cfg.Default_Dump_Config);

            Compiler_Drivers_Set : constant String_Sets.Set :=
              To_String_Set (Compiler_Drivers);

         begin
            Instrument.Setup_Config.Generate_Config
              (Files_Of_Interest => Switches.Files_Of_Interest,
               Coverage_Level    => Coverage_Option_Value,
               Dump_Config       => Dump_Config,
               Compiler_Drivers  => Compiler_Drivers_Set,
               Output_Dir        => Get_Output_Dir,
               Runtime_Project   => Runtime_Project);
         end;

      when Cmd_Gcc_Wrapper =>
         if Args.Remaining_Args.Length /= 2 then
            Fatal_Error ("exactly two positional arguments expected");
         end if;
         Instrument.Gcc_Wrapper
           (Config_File   => +Args.Remaining_Args.Element (0),
            Compiler_Exec => +Args.Remaining_Args.Element (1),
            Cargs         => Args.String_List_Args (Opt_Cargs));

      when Cmd_Instrument_Source =>

         --  For the instrument-source command, and for the instrument-main,
         --  we do not check the command-line semantics as these commands are
         --  internal and spawned by a gnatcov main process. They are thus by
         --  default well-formed, and if they are not, it is a gnatcov bug.
         --
         --  The unit to instrument is the trailing argument

         declare
            Instrumenter : Language_Instrumenter'Class := Instrument.Config;
         begin
            Instrument.Source
              (Instrumenter      => Instrumenter,
               Files_Of_Interest => Switches.Files_Of_Interest,
               Prj               => Instrument.Load_From_Command_Line,
               Unit_Name         => +Args.Remaining_Args.First_Element,
               SID_Name          =>
                 +Args.String_List_Args (Opt_SID).First_Element);
         end;

      when Cmd_Instrument_Main =>
         declare
            --  The dump config is loaded from the command line. The
            --  implementation of the main instrumentation process assumes that
            --  it is fully explicited, i.e. that nothing is left as default.

            Dump_Config : constant Any_Dump_Config :=
              Load_Dump_Config (Any_Dump_Config'(others => <>));

            --  Trailing argument is the main to instrument

            Main_Filename : constant String :=
              +Args.Remaining_Args.First_Element;

            Instrumenter : Language_Instrumenter'Class := Instrument.Config;
         begin
            Instrument.Main
              (Instrumenter,
               Dump_Config,
               Main_Filename,
               Instrument.Load_From_Command_Line);
         end;

      when Cmd_Scan_Objects =>

         --  Scan symbols from all given ELF files in strict mode, warning
         --  about text section points of note wrt object coverage (empty
         --  symbols, orphan regions, ...)

         Check_Argument_Available (Obj_Inputs, "FILEs");
         for Filename of Obj_Inputs loop
            Traces_Elf.Scan_Symbols_From
              (+Filename,
               Sym_Cb => null,
               Strict => True);
         end loop;

      when Cmd_Map_Routines =>
         Load_All_ALIs (Check_SCOs => True);
         for Filename of Exe_Inputs loop
            declare
               F : constant String := +Filename;
            begin
               Build_Decision_Map (F, Text_Start, F & ".dmap");
            end;
         end loop;
         if SCOs_Trace.Is_Active then
            SC_Obligations.Report_SCOs_Without_Code;
         end if;
         SC_Obligations.Report_Units_Without_Code;

      when Cmd_Scan_Decisions =>
         Set_Coverage_Levels ("stmt");
         Load_All_ALIs (Check_SCOs => True);
         SC_Obligations.Report_Multipath_Decisions;

      when Cmd_Check_SCOs =>
         Set_Coverage_Levels ("stmt");
         for Filename of ALIs_Inputs loop
            Check_SCOs.Check_SCO_Syntax (+Filename);
         end loop;
         Load_All_ALIs (Check_SCOs => True);

      when Cmd_Dump_Trace =>
         Check_Traces_Available;
         for RT of Trace_Inputs loop
            Dump_Trace_File (+RT.Filename);
         end loop;

      when Cmd_Dump_Trace_Raw =>
         Check_Traces_Available;
         for RT of Trace_Inputs loop
            Dump_Raw_Trace_File (+RT.Filename);
         end loop;

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
            Check_Traces_Available;
            for RT of Trace_Inputs loop
               Dump_Trace_Base (+RT.Filename);
            end loop;
         end;

      when Cmd_Dump_Trace_Asm =>
         Check_Argument_Available (Exe_Inputs, "EXE");
         Check_Traces_Available;
         for Filename of Exe_Inputs loop
            Exec := Open_File (+Filename, Text_Start);
            Build_Sections (Exec.all);
            Build_Symbols (Exec.all);
         end loop;
         for TF of Trace_Inputs loop
            Traces_Disa.Dump_Traces_With_Asm (Exec.all, +TF.Filename);
         end loop;

      when Cmd_Dump_Src_Trace =>
         Check_Traces_Available;
         for TF of Trace_Inputs loop
            Instrument.Input_Traces.Dump_Source_Trace_File (+TF.Filename);
         end loop;

      when Cmd_Dump_Sections
        | Cmd_Dump_Symbols
        | Cmd_Dump_Subprograms
        | Cmd_Dump_Inlined_Subprograms
        | Cmd_Dump_Lines =>
         declare
            --  Dump Exec_File_Name's sections|symbols|subprograms|lines,
            --  depending on the current command.

            To_Display : Address_Info_Kind;
         begin
            Check_Argument_Available (Exe_Inputs, "EXEs");
            for Filename of Exe_Inputs loop
               Exec := Open_File (+Filename, Text_Start);
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
            end loop;
         end;

      when Cmd_Dump_Compile_Units =>
         Check_Argument_Available (Exe_Inputs, "EXEs");
         for Filename of Exe_Inputs loop
            Exec := Open_File (+Filename, 0);
            Build_Sections (Exec.all);
            Build_Debug_Compile_Units (Exec.all);
            Disp_Compilation_Units (Exec.all);
            Close_File (Exec);
         end loop;

      when Cmd_Disassemble_Raw =>
         Check_Argument_Available (Exe_Inputs, "EXEs");
         for Filename of Exe_Inputs loop
            Exec := Open_File (+Filename, 0);
            Disassemble_File_Raw (Exec.all);
            Close_File (Exec);
         end loop;

      when Cmd_Disassemble =>
         Check_Argument_Available (Exe_Inputs, "EXEs");
         for Filename of Exe_Inputs loop
            Exec := Open_File (+Filename, 0);
            Build_Sections (Exec.all);
            Build_Symbols (Exec.all);
            Disassemble_File (Exec.all);
            Close_File (Exec);
         end loop;

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

         --  Warn when the user hasn't explicitly set a coverage level and
         --  default to stmt.

         if not (Source_Coverage_Enabled or else Object_Coverage_Enabled) then
            Warn ("Coverage level not specified on the command line or in the"
                  & " project file (--level=" & Source_Level_Options
                  & "|" & Object_Level_Options ("|") & "), defaulting to"
                  & " ""stmt"".");
            Set_Coverage_Levels ("stmt");
         end if;

         --  Reject the use of several features that are not supported with
         --  object coverage.

         if Object_Coverage_Enabled then
            if Annotation (Annotate_Report)
              or else Annotation (Annotate_Sarif)
            then
               Fatal_Error
                 (""""
                  & (if Annotation (Annotate_Report)
                    then "report"
                    else "SARIF")
                  & """ output format (from --annotate) is"
                  & " only for source coverage criteria"
                  & ASCII.LF
                  & "  (--level=" & Source_Level_Options
                  & ", not --level="
                  & Coverage_Option_Value (Current_Levels) & ")");

            elsif not Checkpoints_Inputs.Is_Empty
              or else Save_Checkpoint /= null
            then
               Fatal_Error ("Incremental coverage is supported for source"
                            & " coverage only");
            end if;
         end if;

         --  Check the availability of the output format

         if Annotation (Annotate_Xml) and then not Annotations.Xml.Installed
         then
            Fatal_Error ("XML report format support is not installed.");
         end if;

         if Annotation (Annotate_Html)
           and then not Annotations.Dynamic_Html.Installed
         then
            Fatal_Error
              ("Dynamic HTML report format support is not installed.");
         end if;

         if Annotation (Annotate_Cobertura)
            and then not Annotations.Cobertura.Installed
         then
            Fatal_Error ("Cobertura report format support is not installed.");
         end if;

         --  Check that the user specified units of interest. We'll load SCOs
         --  from ALIs/SIDs only when necessary, i.e. only the first time we
         --  process a binary trace file. This will avoid conflicts between
         --  incompatible source obligations (instrumentation-based SCOs from
         --  an SID, ALIs from a rebuilt project, ...).

         Check_User_Provided_SCOs;

         --  Build the list of units of interest from project files

         Enumerate_Units_Of_Interest (Add_Unit'Access);

         --  Load routines from command line

         if Object_Coverage_Enabled then

            if not Routines_Inputs.Is_Empty then
               for Name of Routines_Inputs loop
                  Traces_Names.Add_Routine_Of_Interest (+Name);
               end loop;
               Routines_Of_Interest_Origin := From_Command_Line;

            elsif Trace_Inputs.Length > 1 then
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
            if not Routines_Inputs.Is_Empty then
               Fatal_Error ("Routine list not allowed for source coverage.");
            end if;
         end if;

         --  Read and process traces

         declare
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
                               & Switches.Exception_Info (E));
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
               if not Is_Success (Result) then
                  Report_Bad_Trace (Trace_File_Name, Result);

                  --  If the file does not exist, do not try to Process it
                  --  further.

                  if Result.Kind = Open_Error then
                     return;
                  end if;
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
                 (Kind : Traces_Source.Supported_Info_Kind; Data : String);
               --  Callback for Read_Source_Trace_File

               procedure Read_Source_Trace_File is new
                  Instrument.Input_Traces.Generic_Read_Source_Trace_File
                    (On_Trace_Info  => On_Trace_Info,
                     On_Trace_Entry => Compute_Source_Coverage);

               Trace_File : Trace_File_Element_Acc;
               Result     : Read_Result;

               -------------------
               -- On_Trace_Info --
               -------------------

               procedure On_Trace_Info
                 (Kind : Traces_Source.Supported_Info_Kind; Data : String) is
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
               if not Is_Success (Result) then
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
                  else +Trace_File.Filename);
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
                     if not Is_Success (Result) then
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

               if Routines_Inputs.Is_Empty then
                  Read_Routine_Names (Exe_File.all, Exclude => False);
               end if;

               Build_Debug_Compile_Units (Exe_File.all);

               if Trace_File /= null then
                  Misc_Trace.Trace
                    ("Processing traces from " & Trace_Filename);
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
                  else +Trace_File.Filename);

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
                  if not Is_Success (Result) then
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
                     if not Is_Success (Result) then
                        Report_Bad_Trace (Trace_Filename, Result);
                        return;
                     end if;
                     Update_From_Binary_Trace (Trace_File.all, TF);
                     Free (TF);
                  end;
               end if;
            end Process_Trace_For_Src_Coverage;

         begin
            if Checkpoints_Inputs.Is_Empty then
               Check_Traces_Available;
            end if;
            for Filename of Exe_Inputs loop
               Process_Exec (+Filename);
            end loop;
            for RT of Trace_Inputs loop
               Process_Trace (+RT.Filename, +RT.Executable);
            end loop;

            --  Use dominant information to refine decision coverage
            --  information wrt. outcomes taken. This is a binary information
            --  specificity as we always know which outcomes were taken in the
            --  context of source traces.

            if Currently_Accepted_Trace_Kind = Binary_Trace_File then
               Refine_Source_Coverage;
            end if;

            --  Warn when using --scos with source traces or --sid with bin
            --  traces.

            if not SID_Inputs.Is_Empty and then Bin_Traces_Present then
               Warn ("Using option --sid with binary trace files has no"
                     & " effect." & ASCII.LF & "Please consider using option"
                     & " --scos or -P<project file> in conjunction with"
                     & " --units to specify units of interest.");
            end if;
            if not ALIs_Inputs.Is_Empty and then Src_Traces_Present then
               Warn ("Using option --scos with source trace files has no"
                     & " effect." & ASCII.LF & "Please consider using option"
                     & " --sid or -P<project file> in conjunction with --units"
                     & " to specify units of interest.");
            end if;
         end;

         --  Reconstruct unit names for ignored source files. This is done
         --  before loading checkpoints, because this will already have been
         --  done for the files in the checkpoints when creating them.

         if Project.Is_Project_Loaded
           and then Coverage.Source.Unit_List_Is_Valid
         then
            Compute_Unit_Name_For_Ignored_Sources;
         end if;

         --  Read checkpointed coverage data from previous executions

         for Filename of Checkpoints_Inputs loop
            Checkpoints.Checkpoint_Load (+Filename);
         end loop;

         --  Now determine coverage according to the requested metric (for
         --  source coverage, complete coverage information has been determined
         --  when loading traces above).

         if Object_Coverage_Enabled then
            Traces_Elf.Build_Routines_Insn_State;

            if not Annotation (Annotate_Asm) then
               Traces_Elf.Build_Source_Lines;
            end if;
         end if;

         if Source_Coverage_Enabled and then SCOs_Trace.Is_Active then
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
               and then Annotation (Annotate_Report);
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
               if Dump_Units_Filename = null then
                  Report_Units (Standard_Output);
               else
                  declare
                     File : File_Type;
                  begin
                     Create (File, Name => Dump_Units_Filename.all);
                     Report_Units (File);
                     Close (File);
                  end;
               end if;
            end if;

            --  Generate annotated reports

            if Emit_Report then
               if Annotation (Annotate_Asm) then
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
               end if;

               --  Create the temporary directory that will hold the
               --  preprocessed view of sources.

               Create_Temporary_Directory (PP_Temp_Dir, "gnatcov_rts_pp",
                                           Auto_Delete => not Save_Temps);

               if Annotation (Annotate_Xml) then
                  Annotations.Xml.Generate_Report (Context'Unchecked_Access);
               end if;

               if Annotation (Annotate_Xcov)
                 or else Annotation (Annotate_Xcov_Plus)
               then
                  Annotations.Xcov.Generate_Report
                    (Context'Unchecked_Access,
                     Show_Details => Annotation (Annotate_Xcov_Plus));
               end if;

               if Annotation (Annotate_Static_Html)
                 or else Annotation (Annotate_Static_Html_Plus)
               then
                  Annotations.Html.Generate_Report
                    (Context'Unchecked_Access,
                     Show_Details => Annotation (Annotate_Static_Html_Plus),
                     Report_Title => Args.String_Args (Opt_Report_Title));
               end if;

               if Annotation (Annotate_Html) then
                  Annotations.Dynamic_Html.Generate_Report
                    (Context'Unchecked_Access,
                     Report_Title => Args.String_Args (Opt_Report_Title));
               end if;

               if Annotation (Annotate_Cobertura) then
                  Annotations.Cobertura.Generate_Report
                    (Context'Unchecked_Access);
               end if;

               if Annotation (Annotate_Sarif) then
                  Annotations.Sarif.Generate_Report
                    (Context'Unchecked_Access);
               end if;

               if Annotation (Annotate_Report) then
                  Annotations.Report.Generate_Report
                    (Context'Unchecked_Access, Output, Dump_Units_In_Report);
               end if;

               --  For report and xcov reports, also generate coverage stats
               --  indices.

               if Annotation (Annotate_Report)
                 or else Annotation (Annotate_Xcov)
                 or else Annotation (Annotate_Xcov_Plus)
               then
                  Annotations.Index.Generate_Indices;
               end if;

               if Annotation (Annotate_Unknown) then
                  pragma Assert (Save_Checkpoint /= null);
               end if;

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
         for Filename of Exe_Inputs loop
            declare
               package OS renames GNAT.OS_Lib;
               use type OS.File_Descriptor;

               Dmap_FD       : OS.File_Descriptor := OS.Null_FD;
               Dmap_Filename : OS.Temp_File_Name;
               Success       : Boolean;
               Histmap       : String_Access;
            begin
               if MCDC_Coverage_Enabled then
                  Load_All_ALIs (Check_SCOs => False);
                  if ALIs_Inputs.Is_Empty then
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

                     Build_Decision_Map (+Filename, Text_Start, Histmap.all);
                  end if;
               end if;

               Rundrv.Driver (+Filename, Target_Family, Target_Board, Tag,
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
            end;
         end loop;

      when Cmd_Convert =>
         declare
            Exec    : constant String :=
              +Args.String_List_Args (Opt_Exec).Last_Element;
            Histmap : constant String :=
              Exec & ".dmap";
         begin
            if MCDC_Coverage_Enabled then
               Load_All_ALIs (Check_SCOs => False);
               if ALIs_Inputs.Is_Empty then
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

      when Cmd_Add_Annotation =>
         SS_Annotations.Add_Annotation (Args);

      when Cmd_Delete_Annotation =>
         SS_Annotations.Delete_Annotation (Args);

      when Cmd_Show_Annotations =>
         SS_Annotations.Show_Annotations (Args);
   end case;

   if Misc_Trace.Is_Active then
      Perf_Counters.Display;
   end if;

   Project.Finalize;

exception
   --  The following handler is for files that gnatcov could not locate or
   --  open. Display the corresponding error message.

   when Error : Binary_Files.Error
      | Ada.IO_Exceptions.Name_Error =>
      Outputs.Error (Switches.Exception_Info (Error));
      Project.Finalize;

   --  Each chunk of code with "raise" statements for Xcov_Exit_Exc exceptions
   --  is supposed to print an error message and set a failure exit status
   --  before raising the exception.

   when Xcov_Exit_Exc =>
      Project.Finalize;
end GNATcov_Bits_Specific;
