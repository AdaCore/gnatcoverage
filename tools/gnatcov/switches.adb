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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers;          use Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;

with Inputs;
with Outputs; use Outputs;
with Project; use Project;

package body Switches is

   use type Unbounded_String;
   use Command_Line.Parser;

   function Command_Line_Args return String_Vectors.Vector;
   --  Return a string vectors to hold arguments from Ada.Command_Line

   function Parse
     (Argv         : String_Vectors.Vector;
      With_Command : Command_Type := No_Command;
      Callback     :
        access procedure
          (Result : in out Parsed_Arguments; Ref : Option_Reference) := null)
      return Parsed_Arguments;
   --  Parse Args using Arg_Parser. If there is an error, call Fatal_Error with
   --  the error message.

   procedure Load_Target_Option (Default_Target : Boolean);
   --  Split the --target option into its family name (Target_Family) and the
   --  board name (Target_Board), if any.
   --
   --  If Default_Target is True and the target option is not present or empty,
   --  use the native target. The target has the following format:
   --  FAMILY[,BOARD]. In this case, the returned Target_Family is never null.
   --  Otherwise, leave it unmodified.
   --
   --  In any case, the returned Target_Board may be null.

   procedure Load_Project_Arguments (From_Driver : Boolean);
   --  Load the project, if any, specified in Args, get the command-line
   --  arguments it may specify in its Coverage package corresponding to
   --  Args.Command. Then decode them and merge them with Args into Args
   --  itself.
   --
   --  Note that this also writes Args.String_Args (Opt_Target) (same for
   --  Opt_Runtime) if they are not present to mirror the target/RTS used to
   --  load the project file.
   --
   --  From_Driver is passed as Project.Load_Root_Project's From_Driver
   --  argument.

   --------------
   -- Copy_Arg --
   --------------

   procedure Copy_Arg (Option : String_Options; Variable : out String_Access)
   is
      Opt : String_Option renames Args.String_Args (Option);
   begin
      if Opt.Present then
         Variable := new String'(+Opt.Value);
      end if;
   end Copy_Arg;

   ---------------------
   -- Expand_Argument --
   ---------------------

   function Expand_Argument (Argument : String) return String_Vectors.Vector is
      Result : String_Vectors.Vector;

      procedure Process (Argument : String);

      -------------
      -- Process --
      -------------

      procedure Process (Argument : String) is
      begin
         if Argument'Length = 0 then
            return;
         end if;

         if Argument (Argument'First) /= '@' then
            Result.Append (+Argument);
         elsif Argument'Length > 1 then
            if Argument (Argument'First + 1) /= '@' then
               declare
                  File_Name : constant String :=
                    Argument (Argument'First + 1 .. Argument'Last);
               begin
                  Inputs.Read_List_From_File (File_Name, Process'Access);
               exception
                  when Name_Error | Status_Error =>
                     Fatal_Error ("cannot open input list: " & File_Name);
               end;
            else
               Result.Append (+Argument (Argument'First + 1 .. Argument'Last));
            end if;
         end if;
      end Process;

      --  Start of processing for Expand_Argument

   begin
      Process (Argument);
      return Result;
   end Expand_Argument;

   ------------------------------
   -- Append_Expanded_Argument --
   ------------------------------

   procedure Append_Expanded_Argument
     (Argument : String; List : in out String_Vectors.Vector) is
   begin
      List.Append_Vector (Expand_Argument (Argument));
   end Append_Expanded_Argument;

   -------------------
   -- Copy_Arg_List --
   -------------------

   procedure Copy_Arg_List
     (Option : String_List_Options; List : in out String_Vectors.Vector) is
   begin
      Copy_Arg_List (Args.String_List_Args (Option), List);
   end Copy_Arg_List;

   procedure Copy_Arg_List
     (Args : String_Vectors.Vector; List : in out String_Vectors.Vector) is
   begin
      for Arg of Args loop
         Append_Expanded_Argument (+Arg, List);
      end loop;
   end Copy_Arg_List;

   --------------------------------
   -- Process_File_Or_Dir_Switch --
   --------------------------------

   procedure Process_File_Or_Dir_Switch
     (Args              : String_Vectors.Vector;
      Orig_Switch       : String;
      Process_Dir_Entry :
        access procedure (Dir : Ada.Directories.Directory_Entry_Type);
      Process_Arg       : access procedure (Exp_Arg : String);
      Pattern           : String := "")
   is
      use Ada.Directories;
      use US;
   begin
      for Arg of Args loop

         --  The argument is either a directory or a file / response file when
         --  prefixed with a '@'.

         --  First, deal with the case when it is a directory.

         if US.Element (Arg, Length (Arg)) in '/' | '\' then
            declare
               Path      : constant String := +Arg;
               S         : Search_Type;
               Dir_Entry : Directory_Entry_Type;
            begin
               if Kind (Path) = Directory then
                  Start_Search
                    (Search    => S,
                     Directory => Path,
                     Pattern   => Pattern,
                     Filter    => (Ordinary_File => True, others => False));

                  while More_Entries (S) loop
                     Get_Next_Entry (S, Dir_Entry);
                     Process_Dir_Entry (Dir_Entry);
                  end loop;
                  End_Search (S);
               else
                  Outputs.Warn
                    ("Skipping processing of "
                     & Orig_Switch
                     & " argument "
                     & Path
                     & ". Expecting a directory but got a "
                     & Ada.Directories.File_Kind'Image (Kind (Path))
                     & ".");
               end if;
            end;
         else
            for Exp_Arg of Expand_Argument (+Arg) loop
               Process_Arg (+Exp_Arg);
            end loop;
         end if;
      end loop;
   end Process_File_Or_Dir_Switch;

   ----------------------
   -- Load_Dump_Config --
   ----------------------

   function Load_Dump_Config
     (Default_Dump_Config : Any_Dump_Config) return Any_Dump_Config
   is
      use String_Vectors;
      Dump_Channel_Opt : String_Option renames
        Args.String_Args (Opt_Dump_Channel);
      Dump_Trigger_Opt : Vector;

      Manual_Indication_Files : File_Sets.Set;
      --  Files containing manual indications (optional) when using the manual
      --  dump trigger.

      Dump_Filename_Env_Var_Opt : String_Option renames
        Args.String_Args (Opt_Dump_Filename_Env_Var);
      Dump_Filename_Prefix_Opt  : String_Option renames
        Args.String_Args (Opt_Dump_Filename_Prefix);

      Dump_Channel          : Any_Dump_Channel;
      Dump_Trigger          : Any_Dump_Trigger;
      Dump_Filename_Simple  : Boolean := False;
      Dump_Filename_Env_Var : Unbounded_String;
      Dump_Filename_Prefix  : Unbounded_String;
   begin
      --  First, load the config from Default_Dump_Config, then override it
      --  using command-line arguments.

      Dump_Channel := Default_Dump_Config.Channel;
      if Dump_Channel_Opt.Present then
         begin
            Dump_Channel := Value (+Dump_Channel_Opt.Value);
         exception
            when Exc : Constraint_Error =>
               Fatal_Error (Exception_Info (Exc));
         end;
      end if;

      --  Two cases for the dump-trigger:

      --    * It is a single string, in which case we expect the dump-trigger
      --      to be atexit|main-end|manual.
      --
      --    * It is a string list, in which case we expect the first argument
      --      to be manual, and the rest a list of files to process for the
      --      manual indications replacement.

      Dump_Trigger := Default_Dump_Config.Trigger;
      if Args.String_Args (Opt_Dump_Trigger).Present then
         begin
            --  Post-process the switch value manually, see comment for the
            --  Opt_Dump_Trigger option in command_lines.ads.

            declare
               Non_Expanded_Args : Vector;
            begin
               --  Split the argument in a string list

               Append_From_String
                 (Non_Expanded_Args,
                  Args.String_Args (Opt_Dump_Trigger).Value);

               --  Expand possible response files

               Copy_Arg_List (Non_Expanded_Args, Dump_Trigger_Opt);
            end;

            --  Process the dump trigger value

            Dump_Trigger := Value (+Dump_Trigger_Opt.First_Element);

            --  Now check whether there are additional arguments

            if Dump_Trigger_Opt.Length > 1 then
               if Dump_Trigger /= Manual then
                  Fatal_Error
                    ("--dump-trigger=atexit|main-end accepts a single"
                     & " argument");
               end if;

               declare
                  Cur : Cursor := Next (Dump_Trigger_Opt.First);
               begin
                  while Has_Element (Cur) loop
                     declare
                        Filename : constant String := +Element (Cur);
                     begin
                        if not Ada.Directories.Exists (Filename) then
                           Fatal_Error
                             ("File " & Filename & " does not exist");
                        end if;
                        Manual_Indication_Files.Include
                          (Create_Normalized (Filename));
                        Cur := Next (Cur);
                     end;
                  end loop;
               end;
            end if;
         exception
            when Exc : Constraint_Error =>
               Fatal_Error (Exception_Info (Exc));
         end;
      end if;

      case Default_Dump_Config.Channel is
         when Binary_File =>
            Dump_Filename_Simple := Default_Dump_Config.Filename_Simple;
            Dump_Filename_Env_Var := Default_Dump_Config.Filename_Env_Var;
            Dump_Filename_Prefix := Default_Dump_Config.Filename_Prefix;

         when others      =>
            null;
      end case;

      if Args.Bool_Args (Opt_Dump_Filename_Simple) then
         Dump_Filename_Simple := True;
      end if;
      if Dump_Filename_Env_Var_Opt.Present then
         Dump_Filename_Env_Var := Dump_Filename_Env_Var_Opt.Value;
      end if;
      if Dump_Filename_Prefix_Opt.Present then
         Dump_Filename_Prefix := Dump_Filename_Prefix_Opt.Value;
      end if;

      --  Now, re-create an Any_Dump_Config record from the overriden config
      --  data.
      --
      --  Note that some arguments may end up being ignored depending on other
      --  arguments. For instance, --dump-filename-simple is ignored if
      --  --dump-channel=base64-stdout is passed. This is fine, as this allows
      --  one to blindly pass --dump-filename-simple to get deterministic file
      --  names without worrying about the dump channel that will be selected
      --  in the end (whether that happens in gnatcov setup, in another script,
      --  etc.).

      return Dump_Config : Any_Dump_Config do
         case Dump_Channel is
            when Binary_File            =>
               Dump_Config :=
                 (Channel                 => Binary_File,
                  Trigger                 => Dump_Trigger,
                  Manual_Indication_Files => Manual_Indication_Files,
                  Filename_Simple         => Dump_Filename_Simple,
                  Filename_Env_Var        => Dump_Filename_Env_Var,
                  Filename_Prefix         => Dump_Filename_Prefix);

            when Base64_Standard_Output =>
               Dump_Config :=
                 (Channel                 => Base64_Standard_Output,
                  Trigger                 => Dump_Trigger,
                  Manual_Indication_Files => Manual_Indication_Files);
         end case;
      end return;
   end Load_Dump_Config;

   --------------------
   -- Unparse_Config --
   --------------------

   function Unparse_Config
     (Dump_Config : Any_Dump_Config) return String_Vectors.Vector
   is
      Result : String_Vectors.Vector;
   begin
      Result.Append (+"--dump-trigger");
      case Dump_Config.Trigger is
         when Manual                     =>
            Result.Append (+"manual");

         when At_Exit                    =>
            Result.Append (+"atexit");

         when Ravenscar_Task_Termination =>
            Result.Append (+"ravenscar-task-termination");

         when Main_End                   =>
            Result.Append (+"main-end");
      end case;
      case Dump_Config.Channel is
         when Binary_File            =>
            Result.Append (+"--dump-channel=bin-file");
            if Dump_Config.Filename_Simple then
               Result.Append (+"--dump-filename-simple");
            end if;
            if Dump_Config.Filename_Env_Var /= "" then
               Result.Append
                 ("--dump-filename-env-var=" & Dump_Config.Filename_Env_Var);
            end if;
            if Dump_Config.Filename_Prefix /= "" then
               Result.Append
                 (+"--dump-filename-prefix=" & Dump_Config.Filename_Prefix);
            end if;

         when Base64_Standard_Output =>
            Result.Append (+"--dump-channel=base64-stdout");
      end case;
      return Result;
   end Unparse_Config;

   -----------------
   -- To_Language --
   -----------------

   function To_Language (Name : String) return Some_Language is
      Result : constant Any_Language := To_Language_Or_All (Name);
   begin
      if Result = All_Languages then
         Fatal_Error ("Unsupported language: " & Name);
      else
         return Result;
      end if;
   end To_Language;

   function To_Language (Id : GPR2.Language_Id) return Some_Language is
      Result : constant Any_Language := To_Language_Or_All (Id);
   begin
      if Result = All_Languages then
         Fatal_Error ("Unsupported language: " & String (GPR2.Name (Id)));
      else
         return Result;
      end if;
   end To_Language;

   ------------------------
   -- To_Language_Or_All --
   ------------------------

   function To_Language_Or_All (Name : String) return Any_Language is
      Lower_Name : constant String := To_Lower (Name);
   begin
      if Lower_Name = "ada" then
         return Ada_Language;
      elsif Lower_Name = "c" then
         return C_Language;
      elsif Lower_Name = "c++" then
         return CPP_Language;
      else
         return All_Languages;
      end if;
   end To_Language_Or_All;

   function To_Language_Or_All (Id : GPR2.Language_Id) return Any_Language is
      use type GPR2.Language_Id;
   begin
      if Id = GPR2.Ada_Language then
         return Ada_Language;
      elsif Id = GPR2.C_Language then
         return C_Language;
      elsif Id = GPR2.CPP_Language then
         return CPP_Language;
      else
         return All_Languages;
      end if;
   end To_Language_Or_All;

   -----------
   -- Image --
   -----------

   function Image (Language : Some_Language) return String is
   begin
      return
        (case Language is
           when Ada_Language => "Ada",
           when C_Language   => "C",
           when CPP_Language => "C++");
   end Image;

   --------------------
   -- To_Language_Id --
   --------------------

   function To_Language_Id (Language : Some_Language) return GPR2.Language_Id
   is
   begin
      return
        (case Language is
           when Ada_Language => GPR2.Ada_Language,
           when C_Language   => GPR2.C_Language,
           when CPP_Language => GPR2.CPP_Language);
   end To_Language_Id;

   -----------------------
   -- Command_Line_Args --
   -----------------------

   function Command_Line_Args return String_Vectors.Vector is
   begin
      return Result : String_Vectors.Vector do
         for I in 1 .. Ada.Command_Line.Argument_Count loop
            Result.Append (+Ada.Command_Line.Argument (I));
         end loop;
      end return;
   end Command_Line_Args;

   ------------------------
   -- Load_Target_Option --
   ------------------------

   procedure Load_Target_Option (Default_Target : Boolean) is
      Target_Arg : String_Option renames Args.String_Args (Opt_Target);
   begin
      if not Default_Target and then not Target_Arg.Present then

         --  We have no target information and we are asked not to use a
         --  default one: do nothing.

         return;
      end if;

      declare
         Real_Target : constant String :=
           (if Target_Arg.Present
            then +Target_Arg.Value
            else Standard'Target_Name);
      begin
         --  If we find a comma, then we have both a target family and a board
         --  name.

         for I in Real_Target'Range loop
            if Real_Target (I) = ',' then
               Target_Family :=
                 new String'(Real_Target (Real_Target'First .. I - 1));
               Target_Board :=
                 new String'(Real_Target (I + 1 .. Real_Target'Last));
               return;
            end if;
         end loop;

         --  Otherwise, it's just a family

         Target_Family := new String'(Real_Target);
         Target_Board := null;
      end;
   end Load_Target_Option;

   ----------------------------
   -- Load_Project_Arguments --
   ----------------------------

   procedure Load_Project_Arguments (From_Driver : Boolean) is

      procedure Check_Allowed_Option
        (Result : in out Parsed_Arguments; Ref : Option_Reference);
      --  Put an error message in Result if Ref is an option that is forbidden
      --  in project files.

      --------------------------
      -- Check_Allowed_Option --
      --------------------------

      procedure Check_Allowed_Option
        (Result : in out Parsed_Arguments; Ref : Option_Reference)
      is
         Complain : Boolean := False;
      begin
         case Ref.Kind is
            when String_Opt =>
               Complain :=
                 Ref.String_Option
                 in Opt_Project
                  | Opt_Target
                  | Opt_Runtime
                  | Opt_Subdirs
                  | Opt_Root_Dir;

            when others     =>
               null;
         end case;
         if Complain then
            Result.Error :=
              +(Option_Name (Arg_Parser, Ref)
                & " may not be specified in a project.");
         end if;
      end Check_Allowed_Option;

      Project_Args : Parsed_Arguments;
      Root_Project : String_Access;
      Runtime      : String_Access;
      CGPR_File    : String_Access;

      --  Start of processing for Load_Project_Arguments

   begin
      if not Args.String_Args (Opt_Project).Present then
         return;
      end if;

      --  In order to load the project file we need to set:
      --    * scenario variables;
      --    * the object subdir;
      --    * the target architecture;
      --    * the runtime system (RTS);
      --    * the requested list of projects of interest (if any);
      --    * the requested list of units of interest (if any);
      --    * whether to process recursively the project tree.

      Root_Project := new String'(+Args.String_Args (Opt_Project).Value);

      for S_Var of Args.String_List_Args (Opt_Scenario_Var) loop
         --  Get name and value from "-X<name>=<value>"

         declare
            Str                    : constant String := +S_Var;
            Name_Last, Value_First : Natural;
         begin
            Name_Last := Str'First - 1;
            while Name_Last < Str'Last and then Str (Name_Last + 1) /= '=' loop
               Name_Last := Name_Last + 1;
            end loop;

            Value_First := Name_Last + 2;

            S_Variables.Include
              (Str (Str'First .. Name_Last), Str (Value_First .. Str'Last));
         end;
      end loop;

      if Args.String_Args (Opt_Subdirs).Present then
         Set_Subdirs (+Args.String_Args (Opt_Subdirs).Value);
      end if;

      if Args.Bool_Args (Opt_Externally_Built_Projects) then
         Enable_Externally_Built_Projects_Processing;
      end if;

      if Args.Bool_Args (Opt_Relocate_Build_Tree) then
         Set_Build_Tree_Dir_To_Current;

         if Args.String_Args (Opt_Root_Dir).Present then
            Set_Root_Dir (+Args.String_Args (Opt_Root_Dir).Value);
         end if;
      end if;

      --  If the project file does not define a target, loading it needs the
      --  target information: load it here. Likewise for the runtime system.

      Load_Target_Option (Default_Target => False);
      Copy_Arg (Opt_Runtime, Runtime);
      Copy_Arg (Opt_Config, CGPR_File);

      --  All -X command line switches have now been processed: initialize the
      --  project subsystem and load the root project.

      Load_Root_Project
        (Root_Project.all,
         Target_Family,
         Runtime,
         CGPR_File,
         Value_Or_Null (Args.String_Args (Opt_Db)),
         From_Driver);

      --  Get common and command-specific switches, decode them (if any) and
      --  store the result in Project_Args, then merge it into Args.

      declare
         Command_Name     : constant String :=
           Parser.Command_Name (Arg_Parser, Args.Command);
         Common_Switches  : constant String_Vectors.Vector :=
           Project.Switches ("*");
         Command_Switches : constant String_Vectors.Vector :=
           Project.Switches (Command_Name);
      begin
         Project_Args :=
           Parse
             (Common_Switches,
              With_Command => Args.Command,
              Callback     => Check_Allowed_Option'Access);
         Merge
           (Project_Args,
            Parse
              (Command_Switches,
               With_Command => Args.Command,
               Callback     => Check_Allowed_Option'Access));

         --  Project_Args have precedence over Args, so merge in Project_Args
         --  first.

         Merge (Project_Args, Args);
         Args := Project_Args;
      end;

      --  Set default output directory, target and runtime from the project

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

      if not Args.String_Args (Opt_Runtime).Present
        and then Project.Runtime /= ""
      then
         Args.String_Args (Opt_Runtime) :=
           (Present => True, Value => +Project.Runtime);
      end if;
   end Load_Project_Arguments;

   -----------
   -- Parse --
   -----------

   function Parse
     (Argv         : String_Vectors.Vector;
      With_Command : Command_Type := No_Command;
      Callback     :
        access procedure
          (Result : in out Parsed_Arguments; Ref : Option_Reference) := null)
      return Parsed_Arguments
   is
      Result : constant Parsed_Arguments :=
        Parse (Arg_Parser, Argv, With_Command, Callback);
      Error  : constant String := +Result.Error;
   begin
      if Error'Length /= 0 then
         Args.Command := Result.Command;
         Fatal_Error_With_Usage (Error);
      end if;
      return Result;
   end Parse;

   --------------------
   -- Exception_Info --
   --------------------

   function Exception_Info
     (Exc          : Ada.Exceptions.Exception_Occurrence;
      Discard_Name : Boolean := False) return String is
   begin
      if Misc_Trace.Is_Active then
         return Ada.Exceptions.Exception_Information (Exc);
      end if;

      declare
         Name : constant String := Ada.Exceptions.Exception_Name (Exc);
         Msg  : constant String := Ada.Exceptions.Exception_Message (Exc);
      begin
         if Msg = "" then
            return Name;
         elsif Discard_Name then
            return Msg;
         else
            return Name & ": " & Msg;
         end if;
      end;
   end Exception_Info;

   ---------------------
   -- Parse_Arguments --
   ---------------------

   procedure Parse_Arguments (From_Driver : Boolean) is
   begin
      --  Require at least one argument

      if Ada.Command_Line.Argument_Count = 0 then
         Print_Usage (Arg_Parser, False, False);
         Normal_Exit;
      end if;

      --  Parse actual command-line arguments, then load the project, which may
      --  contain additional arguments.

      Args := Parse (Command_Line_Args);
      Load_Project_Arguments (From_Driver);

      --  Enable logs according to the logs/verbosity options

      Logging.Initialize
        (Verbose   => Args.Bool_Args (Opt_Verbose),
         To_Enable => Args.String_List_Args (Opt_Log));
      Quiet := Args.Bool_Args (Opt_Quiet);

      --  Loading the project may have set a new target/RTS: update our
      --  internal state accordingly.

      Load_Target_Option (Default_Target => True);

      --  At this point we know what the final value for Warnings_As_Errors
      --  should be: if we already emitted a warning, make sure the exit code
      --  is updated accordingly.

      Warnings_As_Errors := Args.Bool_Args (Opt_Warnings_As_Errors);
      if Warnings_Registered then
         Register_Warning;
      end if;
      Instrument_Block := Args.Bool_Args (Opt_Instrument_Block);
   end Parse_Arguments;

   -----------
   -- Image --
   -----------

   function Image (Dump_Trigger : Any_Dump_Trigger) return String is
   begin
      return
        (case Dump_Trigger is
           when Manual                     => "manual",
           when At_Exit                    => "atexit",
           when Ravenscar_Task_Termination => "ravenscar-task-termination",
           when Main_End                   => "main-end");
   end Image;

   function Image (Dump_Channel : Any_Dump_Channel) return String is
   begin
      return
        (case Dump_Channel is
           when Binary_File            => "bin-file",
           when Base64_Standard_Output => "base64-stdout");
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Dump_Trigger : String) return Any_Dump_Trigger is
   begin
      if Dump_Trigger = "manual" then
         return Manual;
      elsif Dump_Trigger = "atexit" then
         return At_Exit;
      elsif Dump_Trigger = "ravenscar-task-termination" then
         return Ravenscar_Task_Termination;
      elsif Dump_Trigger = "main-end" then
         return Main_End;
      else
         return
           (raise Constraint_Error
              with "invalid dump trigger: " & Dump_Trigger);
      end if;
   end Value;

   function Value (Dump_Channel : String) return Any_Dump_Channel is
   begin
      if Dump_Channel = "bin-file" then
         return Binary_File;
      elsif Dump_Channel = "base64-stdout" then
         return Base64_Standard_Output;
      else
         return
           (raise Constraint_Error
              with "invalid dump channel: " & Dump_Channel);
      end if;
   end Value;

   ---------------------
   -- Common_Switches --
   ---------------------

   function Common_Switches
     (Cmd : Command_Line.Command_Type) return String_Vectors.Vector
   is
      Has_Config : constant Boolean :=
        Is_Present (Args, Option_Reference'(String_Opt, Opt_Config));
      --  Whether the --config flag is on the command line. If this is the
      --  case, do not pass the --target and --RTS flags (they will be parsed
      --  from the config).

      Result : String_Vectors.Vector;

      procedure Process (Option : Option_Reference);
      --  Add the command line value of Option to Result if Cmd supports it

      -------------
      -- Process --
      -------------

      procedure Process (Option : Option_Reference) is
      begin
         if Is_Present (Args, Option)
           and then Supports (Arg_Parser, Cmd, Option)
         then
            Result.Append_Vector (Unparse (Arg_Parser, Args, Option));
         end if;
      end Process;

   begin
      --  Unfortunately, we can't avoid the code duplication. Deal with all
      --  kind of options: boolean, string and strings list. Do not pass
      --  the --target and --RTS flags if there is a --config flag.

      for Opt in Bool_Options loop
         Process (Option_Reference'(Bool_Opt, Opt));
      end loop;

      for Opt in String_Options loop
         if not Has_Config or else Opt not in Opt_Target | Opt_Runtime then
            Process (Option_Reference'(String_Opt, Opt));
         end if;
      end loop;

      for Opt in String_List_Options loop
         Process (Option_Reference'(String_List_Opt, Opt));
      end loop;
      return Result;
   end Common_Switches;

   --------------------------
   -- Set_Language_Version --
   --------------------------

   function Set_Language_Version
     (V : in out Any_Language_Version; From : String) return Boolean
   is
      use Ada.Strings.Fixed;
   begin
      for Ada_Version in Any_Language_Version loop
         declare
            Year : constant String := Ada_Version'Image (5 .. 8);
         begin
            if Index (From, Year) /= 0 or else Index (From, Year (7 .. 8)) /= 0
            then
               V := Ada_Version;
               return True;
            end if;
         end;
      end loop;
      return False;
   end Set_Language_Version;

end Switches;
