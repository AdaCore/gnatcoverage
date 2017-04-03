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

with Ada.Command_Line;
with Ada.Containers;        use Ada.Containers;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Interfaces;

with GNAT.OS_Lib;
with GNAT.Strings; use GNAT.Strings;

with ALI_Files;         use ALI_Files;
with Annotations;       use Annotations;
with Annotations.Dynamic_Html;
with Annotations.Html;
with Annotations.Xcov;
with Annotations.Xml;
with Annotations.Report;
with CFG_Dump;
with Check_SCOs;
with Checkpoints;
with Command_Line;      use Command_Line;
use Command_Line.Parser;
with Convert;
with Coverage;          use Coverage;
with Coverage.Source;   use Coverage.Source;
with Coverage.Tags;     use Coverage.Tags;
with Decision_Map;      use Decision_Map;
with Disassemble_Insn_Properties;
with Binary_Files;
with Execs_Dbase;       use Execs_Dbase;
with Files_Table;       use Files_Table;
with Inputs;            use Inputs;
with Object_Locations;  use Object_Locations;
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

   Arg_Parser   : Parser_Type := Command_Line.Create;
   --  Parser for command-line arguments

   Args         : Parsed_Arguments;
   --  Results of the arguments parsing (first only command-line, then also
   --  arguments from project file).

   --  Results of the command line parsing. It is filled by Process_Arguments
   --  once Args reached its final value.

   Annotation           : Annotation_Format renames Annotations.Annotation;
   Trace_Inputs         : Inputs.Inputs_Type;
   Exe_Inputs           : Inputs.Inputs_Type;
   Obj_Inputs           : Inputs.Inputs_Type;
   ALIs_Inputs          : Inputs.Inputs_Type;
   Routines_Inputs      : Inputs.Inputs_Type;
   Units_Inputs         : Inputs.Inputs_Type;
   Projects_Inputs      : Inputs.Inputs_Type;
   Checkpoints_Inputs   : Inputs.Inputs_Type;
   Ignored_Source_Files : Inputs.Inputs_Type;
   Text_Start           : Pc_Type := 0;
   Target               : String_Access := null;
   Output               : String_Access := null;
   Tag                  : String_Access := null;
   Kernel               : String_Access := null;
   Save_Checkpoint      : String_Access := null;
   Eargs                : String_Vectors.Vector;
   Executable_Path      : String_Access := null;
   Locations_Inputs     : Object_Locations.User_Locations;
   CFG_Output_Format    : CFG_Dump.Output_Format := CFG_Dump.None;
   Keep_Edges           : Boolean := False;
   Pretty_Print         : Boolean := False;

   function Command_Name return String
   is
     (Command_Line.Parser.Command_Name (Arg_Parser, Args.Command));

   procedure Fatal_Error_With_Usage (Msg : String);
   --  Shortcut for Outputs.Fatal_Error_With_Usage

   function Command_Line_Args return String_List_Access;
   --  Return a dynamically alocated list of arguments to hold arguments from
   --  Ada.Command_Line.

   function Parse
     (Argv         : GNAT.Strings.String_List_Access;
      With_Command : Command_Type := No_Command;
      Callback     : access procedure (Result : in out Parsed_Arguments;
                                       Ref    : Option_Reference) := null)
      return Parsed_Arguments;
   --  Parse Args using Arg_Parser. Deallocate Args before returning. If there
   --  is an error, call Fatal_Error with the error message.

   procedure Copy_Arg
     (Option   : String_Options;
      Variable : out String_Access);
   --  Copy Arg into Var. If Arg is not null, this allocates a new string in
   --  Var.

   procedure Copy_Arg_List
     (Option : String_List_Options;
      List   : in out Inputs.Inputs_Type);
   --  Copy the list of strings referenced in Option to the List input list

   procedure Load_Project_Arguments;
   --  Load the project, if any, specified in Args, get the command-line
   --  arguments it may specify in its Coverage package corresponding to
   --  Args.Command. Then decode them and merge them with Args into Args
   --  itself.

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

   procedure Load_All_SCOs (Check_SCOs : Boolean);
   --  Load all listed SCO files and initialize source coverage data structure.
   --  If Check_SCOs is True, report an error if no SCOs are provided.

   function Ignored_Source_Files_Set return String_Sets.Set;
   --  Create a string set out of Ignored_Source_File and return it

   ----------------------------
   -- Fatal_Error_With_Usage --
   ----------------------------

   procedure Fatal_Error_With_Usage (Msg : String) is
   begin
      Error (Msg);
      Print_Usage (Arg_Parser, False, True, Args.Command);
      raise Xcov_Exit_Exc;
   end Fatal_Error_With_Usage;

   -----------------------
   -- Command_Line_Args --
   -----------------------

   function Command_Line_Args return String_List_Access is
      Result : constant String_List_Access :=
        new String_List (1 .. Ada.Command_Line.Argument_Count);
   begin
      for I in Result'Range loop
         Result (I) := new String'(Ada.Command_Line.Argument (I));
      end loop;
      return Result;
   end Command_Line_Args;

   -----------
   -- Parse --
   -----------

   function Parse
     (Argv         : GNAT.Strings.String_List_Access;
      With_Command : Command_Type := No_Command;
      Callback     : access procedure (Result : in out Parsed_Arguments;
                                       Ref    : Option_Reference) := null)
      return Parsed_Arguments
   is
      Args_Var : GNAT.Strings.String_List_Access := Argv;
      Result   : constant Parsed_Arguments :=
        Parse (Arg_Parser, Argv, With_Command, Callback);
      Error    : constant String := +Result.Error;
   begin
      Free (Args_Var);
      if Error'Length /= 0 then
         Args.Command := Result.Command;
         Fatal_Error_With_Usage (Error);
      end if;
      return Result;
   end Parse;

   --------------
   -- Copy_Arg --
   --------------

   procedure Copy_Arg
     (Option   : String_Options;
      Variable : out String_Access)
   is
      Opt : String_Option renames Args.String_Args (Option);
   begin
      if Opt.Present then
         Variable := new String'(+Opt.Value);
      end if;
   end Copy_Arg;

   -------------------
   -- Copy_Arg_List --
   -------------------

   procedure Copy_Arg_List
     (Option : String_List_Options;
      List   : in out Inputs.Inputs_Type) is
   begin
      for Arg of Args.String_List_Args (Option) loop
         Inputs.Add_Input (List, +Arg);
      end loop;
   end Copy_Arg_List;

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

   -------------------
   -- Load_All_SCOs --
   -------------------

   procedure Load_All_SCOs (Check_SCOs : Boolean) is
      Ignored : constant String_Sets.Set :=
        Ignored_Source_Files_Set;

      procedure Load_SCOs_Wrapper (ALI_Filename : String);
      --  Wrapper for SC_Obligations.Load_SCOs that uses Ignored to ignore
      --  source files.

      -----------------------
      -- Load_SCOs_Wrapper --
      -----------------------

      procedure Load_SCOs_Wrapper (ALI_Filename : String) is
      begin
         Load_SCOs (ALI_Filename, Ignored);
      end Load_SCOs_Wrapper;

   --  Start of processing for Load_All_SCOs

   begin
      if Check_SCOs and then Inputs.Length (Checkpoints_Inputs) = 0 then
         Check_Argument_Available
           (ALIs_Inputs,
            "SCOs",
            ", specifying Units in project or using --units/--scos");
      end if;
      Inputs.Iterate (ALIs_Inputs, Load_SCOs_Wrapper'Access);
      Coverage.Source.Initialize_SCI;
   end Load_All_SCOs;

   ----------------------------
   -- Load_Project_Arguments --
   ----------------------------

   procedure Load_Project_Arguments is

      procedure Check_Allowed_Option (Result : in out Parsed_Arguments;
                                      Ref    : Option_Reference);
      --  Put an error message in Result if Ref is an option that is forbidden
      --  in project files.

      --------------------------
      -- Check_Allowed_Option --
      --------------------------

      procedure Check_Allowed_Option (Result : in out Parsed_Arguments;
                                      Ref    : Option_Reference)
      is
         Complain : Boolean := False;
      begin
         case Ref.Kind is
            when String_Opt =>
               Complain := Ref.String_Option in
                 Opt_Project | Opt_Target | Opt_Subdirs;
            when others =>
               null;
         end case;
         if Complain then
            Result.Error := +(Option_Name (Arg_Parser, Ref)
                              & " may not be specified in a project.");
         end if;
      end Check_Allowed_Option;

      Project_Args : Parsed_Arguments;

   --  Start of processing for Load_Project_Arguments

   begin
      if not Args.String_Args (Opt_Project).Present then
         return;
      end if;

      --  In order to load the project file we need to set:
      --    * scenario variables;
      --    * the object subdir;
      --    * the target architecture.

      Root_Project := new String'(+Args.String_Args (Opt_Project).Value);

      for S_Var of Args.String_List_Args (Opt_Scenario_Var) loop
         --  Get name and value from "-X<name>=<value>"

         declare
            Str                    : constant String := +S_Var;
            Name_Last, Value_First : Natural;
         begin
            Name_Last := Str'First - 1;
            while Name_Last < Str'Last
              and then Str (Name_Last + 1) /= '='
            loop
               Name_Last := Name_Last + 1;
            end loop;

            Value_First := Name_Last + 2;

            S_Variables.Include
              (Str (Str'First .. Name_Last),
               Str (Value_First .. Str'Last));
         end;
      end loop;

      if Args.String_Args (Opt_Subdirs).Present then
         Set_Subdirs (+Args.String_Args (Opt_Subdirs).Value);
      end if;

      --  If the project file does not define a target, loading it needs the
      --  target information: load it here.

      Copy_Arg (Opt_Target, Target);

      --  All -X command line switches have now been processed: initialize the
      --  project subsystem and load the root project.

      Load_Root_Project (Root_Project.all, Target);
      Compute_Project_View;

      --  Get common and command-specific switches, decode them (if any) and
      --  store the result in Project_Args, then merge it into Args.

      declare
         Common_Switches  : constant String_List_Access :=
           Project.Switches ("*");
         Command_Switches : constant String_List_Access :=
           Project.Switches (Command_Name);
      begin
         if Common_Switches /= null then
            Project_Args := Parse
              (Common_Switches,
               With_Command => Args.Command,
               Callback     => Check_Allowed_Option'Access);
         end if;

         if Command_Switches /= null then
            Merge
              (Project_Args,
               Parse
                 (Command_Switches,
                  With_Command => Args.Command,
                  Callback     => Check_Allowed_Option'Access));
         end if;

         --  Project_Args have precedence over Args, so merge in Project_Args
         --  first.

         Merge (Project_Args, Args);
         Args := Project_Args;
      end;

      --  Set default output directory and target from the project

      if not Args.String_Args (Opt_Output_Directory).Present then
         Args.String_Args (Opt_Output_Directory) :=
           (Present => True, Value => +Project.Output_Dir);
      end if;

      if not Args.String_Args (Opt_Target).Present
        and then Project.Target /= ""
      then
         Args.String_Args (Opt_Target) :=
           (Present => True, Value => +Project.Target);
      end if;
   end Load_Project_Arguments;

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

      Switches.Recursive_Projects := Args.Bool_Args (Opt_Recursive);
      Verbose                     := Args.Bool_Args (Opt_Verbose);
      Switches.All_Decisions      := Args.Bool_Args (Opt_All_Decisions);
      Switches.All_Messages       := Args.Bool_Args (Opt_All_Messages);
      Branch_Stats                := Args.Bool_Args (Opt_Branch_Stats);
      Excluded_SCOs               := Args.Bool_Args (Opt_Excluded_SCOs);
      Keep_Edges                  := Args.Bool_Args (Opt_Keep_Edges);
      Pretty_Print                := Args.Bool_Args (Opt_Pretty_Print);

      Copy_Arg (Opt_Target, Target);
      Copy_Arg (Opt_Output, Output);
      Copy_Arg (Opt_Final_Report, Output);
      Copy_Arg (Opt_Tag, Tag);
      Copy_Arg (Opt_Kernel, Kernel);
      Copy_Arg (Opt_HW_Trigger_Traces, Convert.HW_Trigger_Arg);
      Copy_Arg (Opt_Input, Convert.Input_Arg);
      Copy_Arg (Opt_Save_Checkpoint, Save_Checkpoint);

      Copy_Arg_List (Opt_Projects, Projects_Inputs);
      Copy_Arg_List (Opt_Scos, ALIs_Inputs);
      Copy_Arg_List (Opt_Units, Units_Inputs);
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

      for Arg_Acc of Args.String_List_Args (Opt_Source_Rebase) loop
         declare
            --  Parse source-rebase's argument. This option's form should be:
            --
            --  "<OLD_PREFIX>=<NEW_PREFIX>"

            Arg : constant String := +Arg_Acc;
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
         end;
      end loop;

      for Arg of Args.String_List_Args (Opt_Source_Search) loop
         Add_Source_Search (+Arg);
      end loop;

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
               Switch : constant Switches.Debug_Type :=
                  Debug_Switches_Map (Char);
            begin
               if Switch = Switches.None then
                  Fatal_Error ("Invalid debug switch: -d" & Char);
               else
                  Switches.Debug_Switches (Switch) := True;
               end if;
            end;
         end loop;
      end loop;

      --  ... then, handle remaning arguments, which have subcommand-specific
      --  meanings.

      case Args.Command is
         when Cmd_Coverage
            | Cmd_Dump_Trace
            | Cmd_Dump_Trace_Raw
            | Cmd_Dump_Trace_Base
            | Cmd_Dump_Trace_Asm =>

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

         when Cmd_Map_Routines =>
            --  Set MC/DC coverage level in order to generate a complete
            --  decision map.

            Set_Coverage_Levels ("stmt+mcdc");
            for Arg of Args.Remaining_Args loop
               Inputs.Add_Input (Exe_Inputs, +Arg);
            end loop;

         when Cmd_Run =>
            --  If we don't yet have an executable specified, pick the first
            --  EARG. Forward the remaining EARGS from Args to the Eargs local.

            case Args.Remaining_Args.Length is
               when 0 =>
                  declare
                     Eargs_Arg : String_Vectors.Vector
                     renames Args.String_List_Args (Opt_Eargs);
                  begin
                     if Eargs_Arg.Length = 0 then
                        Report_Missing_Argument
                          ("an executable to run (EXE)");
                     end if;
                     for Arg of Eargs_Arg loop
                        if Inputs.Length (Exe_Inputs) = 0 then
                           Inputs.Add_Input
                             (Exe_Inputs, +Eargs_Arg.First_Element);
                        else
                           Eargs.Append (Arg);
                        end if;
                     end loop;
                  end;

               when 1 =>
                  Inputs.Add_Input
                    (Exe_Inputs, +Args.Remaining_Args.First_Element);
                  Eargs := Args.String_List_Args (Opt_Eargs);

               when others =>
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

         when others =>
            null;
      end case;

      if Root_Project /= null then
         --  If a root project has been specified but no project is being
         --  considered for coverage analysis, then:
         --
         --  * If it has an Origin_Project attribute, consider the project it
         --    references. GNATtest uses this attribute in the generated
         --    harness project to reference the user project that is tested, so
         --    this behavior is helpful.
         --
         --  * Otherwise just consider the root project.

         if Length (Projects_Inputs) = 0 then
            if Origin_Project'Length /= 0 then
               Inputs.Add_Input (Projects_Inputs, Origin_Project);
            else
               Inputs.Add_Input (Projects_Inputs, Root_Project.all);
            end if;
         end if;
         Inputs.Iterate (Projects_Inputs, Project.Add_Project'Access);

         --  Set defaults for options identifying the entities of interest
         --  coverage analysis if they have not been identified on the command
         --  line.

         if Object_Coverage_Enabled then
            --  Set routines from project, not supported yet???
            null;

         elsif Inputs.Length (ALIs_Inputs) = 0 then
            declare
               procedure Add_LI (S : String);
               --  Callback to add items to ALIs_Inputs

               ------------
               -- Add_LI --
               ------------

               procedure Add_LI (S : String) is
               begin
                  Inputs.Add_Input (ALIs_Inputs, S);
               end Add_LI;
            begin
               Enumerate_LIs (Add_LI'Access, Override_Units => Units_Inputs);
            end;
         end if;

      elsif Length (Projects_Inputs) /= 0 then
         Fatal_Error ("--projects requires -P");
      end if;

      if Inputs.Length (Ignored_Source_Files) = 0 then
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

   ------------------------------
   -- Ignored_Source_Files_Set --
   ------------------------------

   function Ignored_Source_Files_Set return String_Sets.Set is
      Result : String_Sets.Set;

      procedure Process (File : String);
      --  Include File in Result

      -------------
      -- Process --
      -------------

      procedure Process (File : String) is
      begin
         Result.Include (Ada.Strings.Unbounded.To_Unbounded_String (File));
      end Process;

   --  Start of processing for Ignored_Source_Files_Set

   begin
      Inputs.Iterate (Ignored_Source_Files, Process'Access);
      return Result;
   end Ignored_Source_Files_Set;

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
   --  Require at least one argument

   if Ada.Command_Line.Argument_Count = 0 then
      Print_Usage (Arg_Parser, False, False);
      Normal_Exit;
   end if;

   --  Load arguments from command-line and from the project file (if any)

   Args := Parse (Command_Line_Args);
   Load_Project_Arguments;
   Process_Arguments;

   if Verbose then
      Show_Version;
      Show_CWD;
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
            Load_All_SCOs (Check_SCOs => True);
            Inputs.Iterate (Exe_Inputs, Build_Decision_Map'Access);
            if Verbose then
               SC_Obligations.Report_SCOs_Without_Code;
            end if;
            SC_Obligations.Report_Units_Without_Code;
         end;

      when Cmd_Scan_Decisions =>
         Load_All_SCOs (Check_SCOs => True);
         SC_Obligations.Report_Multipath_Decisions;

      when Cmd_Check_SCOs =>
         Inputs.Iterate (ALIs_Inputs, Check_SCOs.Check_SCO_Syntax'Access);
         Load_All_SCOs (Check_SCOs => True);

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
               Trace_File : constant Trace_File_Element_Acc :=
                 new Trace_File_Element;
            begin
               Read_Trace_File (Trace_File_Name, Trace_File.Trace, Base);
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

      when Cmd_Coverage =>

         --  Validate combination of output format and coverage level

         if Annotation = Annotate_Report and then Object_Coverage_Enabled then
            Fatal_Error
              ("Report output is supported for source coverage only.");
         end if;

         --  Validate availability of the output format

         if Annotation = Annotate_Dynamic_Html and then
            not Annotations.Dynamic_Html.Installed
         then
            Fatal_Error
              ("Dynamic HTML report format support is not installed.");
         end if;

         --  Validate checkpoint related arguments and coverage level

         if (Inputs.Length (Checkpoints_Inputs) > 0
             or else Save_Checkpoint /= null)
           and then not Source_Coverage_Enabled
         then
            Fatal_Error ("Incremental object coverage not supported");
         end if;

         --  Load ALI files

         if Source_Coverage_Enabled then
            Load_All_SCOs (Check_SCOs => True);

         elsif Object_Coverage_Enabled then
            Inputs.Iterate (ALIs_Inputs, Load_ALI'Access);

         else
            Report_Missing_Argument ("a coverage level");
         end if;

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

         --  Read checkpointed coverage data from previous executions

         Inputs.Iterate
           (Checkpoints_Inputs, Checkpoints.Checkpoint_Load'Access);

         --  Read and process traces

         declare
            procedure Process_Exec (Exec_Name : String);
            --  Load a consolidated executable

            procedure Process_Trace
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
                  Open_Exec (Exe_Name, Text_Start, Exe_File);
                  declare
                     Mismatch_Reason : constant String :=
                        Match_Trace_Executable (Exe_File.all, Trace_File);

                  begin
                     if Mismatch_Reason /= "" then
                        Warn
                          ("ELF file " & Exe_Name
                           & " does not seem to match trace file "
                           & Trace_File_Name & ": " & Mismatch_Reason);
                     end if;
                  end;
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
               Trace_File : Trace_File_Element_Acc;
            begin
               if Trace_File_Name /= "" then
                  Trace_File := new Trace_File_Element'
                    (From_Checkpoint => False,
                     Filename        => new String'(Trace_File_Name),
                     others          => <>);

                  Traces_Files_List.Files.Append (Trace_File);
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
            end Process_Trace;

            ------------------------------------
            -- Process_Trace_For_Obj_Coverage --
            ------------------------------------

            procedure Process_Trace_For_Obj_Coverage
              (Trace_File         : Trace_File_Element_Acc;
               Exec_Name_Override : String)
            is
               Exe_File : Exe_File_Acc;
            begin
               Init_Base (Base);

               if Trace_File = null then
                  Open_Exec (Exec_Name_Override, Text_Start, Exe_File);
               else
                  Read_Trace_File
                    (Trace_File.Filename.all, Trace_File.Trace, Base);

                  Exe_File :=
                    Open_Exec_For_Trace (Trace_File.Filename.all,
                               Trace_File.Trace,
                               Exec_Name_Override);
               end if;

               --  If there is no routine in list, get routine names from the
               --  first executable. A test earlier allows this only if there
               --  is one trace file.

               if Inputs.Length (Routines_Inputs) = 0 then
                  Read_Routine_Names (Exe_File.all, Exclude => False);
               end if;

               Build_Debug_Compile_Units (Exe_File.all);

               if Verbose and then Trace_File /= null then
                  Put_Line
                    ("Processing traces from " & Trace_File.Filename.all);
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
               if Trace_File = null then
                  Open_Exec (Exec_Name_Override, Text_Start, Exe_File);
               else
                  Open_Trace_File
                    (Trace_File.Filename.all, Desc, Trace_File.Trace);

                  Exe_File := Open_Exec_For_Trace
                                (Trace_File.Filename.all,
                                 Trace_File.Trace,
                                 Exec_Name_Override);
               end if;

               --  Load symbols from executable (sets the rebase offset for
               --  each symbol) and perform static analysis.

               Decision_Map.Analyze (Exe_File);

               if Trace_File = null then
                  return;
               end if;

               --  Read the load address

               Read_Loadaddr_Trace_Entry (Desc, Trace_File.Trace, Offset);

               --  Iterate on trace entries

               if Verbose then
                  Put_Line
                    ("Processing traces from " & Trace_File.Filename.all);
               end if;

               loop
                  Read_Trace_Entry (Desc, Eof, E);
                  exit when Eof;

                  if E.Op = Qemu_Traces.Trace_Op_Special then
                     Fatal_Error
                       ("Unexpected 'loadaddr' special trace entry.");
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
            if Inputs.Length (Checkpoints_Inputs) = 0 then
               Check_Argument_Available (Trace_Inputs, "TRACE_FILEs");
            end if;
            Inputs.Iterate (Exe_Inputs,  Process_Exec'Access);
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

         if Source_Coverage_Enabled and then Verbose then
            SC_Obligations.Report_Units_Without_Code;
         end if;

         declare
            Context : aliased Coverage.Context := Get_Context;
         begin
            --  Generate annotated reports

            case Annotation is
            when Annotate_Asm =>
               if Source_Coverage_Enabled then
                  Fatal_Error
                    ("Asm output supported for object coverage only.");
               end if;
               Traces_Disa.Flag_Show_Asm := True;
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
                  Show_Details => Annotation = Annotate_Html_Plus);

            when Annotate_Dynamic_Html =>
               Annotations.Dynamic_Html.Generate_Report
                 (Context'Unchecked_Access);

            when Annotate_Report =>
               Annotations.Report.Generate_Report
                 (Context'Unchecked_Access, Output);

            when Annotate_Unknown =>
               pragma Assert (Save_Checkpoint /= null);
            end case;

            --  Generate checkpoint, if requested

            if Save_Checkpoint /= null then
               Checkpoints.Checkpoint_Save
                 (Save_Checkpoint.all, Context'Access);
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
               Histmap : String_Access := null;
            begin
               if MCDC_Coverage_Enabled then
                  if Length (ALIs_Inputs) = 0 then
                     Warn ("No SCOs specified for MC/DC level.");

                  else
                     Histmap := new String'(Exe_File & ".dmap");
                     Load_All_SCOs (Check_SCOs => False);
                     Build_Decision_Map (Exe_File, Text_Start, Histmap.all);
                  end if;
               end if;

               Rundrv.Driver (Exe_File, Target, Tag, Output, Histmap,
                              Kernel, Vector_To_List (Eargs));
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
               if Length (ALIs_Inputs) = 0 then
                  Warn ("No SCOs specified for MC/DC level.");

               else
                  Load_All_SCOs (Check_SCOs => False);
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
   end case;

   Destroy (Arg_Parser);

   if Verbose then
      Perf_Counters.Display;
   end if;

exception
   when Error : Binary_Files.Error
      | Ada.IO_Exceptions.Name_Error =>
      Outputs.Error (Ada.Exceptions.Exception_Message (Error));

   when Xcov_Exit_Exc =>
      --  An error message has already been displayed

      null;
end GNATcov;
