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
with Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;
with System.Multiprocessors;

with Inputs;
with Outputs; use Outputs;

package body Switches is

   use type Unbounded_String;
   use Command_Line.Parser;

   Current_Parallelism_Level : Positive := 1;
   --  Number of jobs that can be run in parallel

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

   -----------------------
   -- Parallelism_Level --
   -----------------------

   function Parallelism_Level return Positive is
   begin
      return Current_Parallelism_Level;
   end Parallelism_Level;

   ---------------------------
   -- Set_Parallelism_Level --
   ---------------------------

   procedure Set_Parallelism_Level (Level : Natural) is
   begin
      Current_Parallelism_Level :=
        (if Level = 0
         then Positive (System.Multiprocessors.Number_Of_CPUs)
         else Level);
   end Set_Parallelism_Level;

   ----------------------
   -- Load_Dump_Config --
   ----------------------

   function Load_Dump_Config
     (Default_Dump_Config : Any_Dump_Config) return Any_Dump_Config
   is
      use String_Vectors;
      Dump_Channel_Opt      : String_Option renames
        Args.String_Args (Opt_Dump_Channel);
      Manual_Dump_Files_Opt : Vector;

      Auto_Trigger            : Auto_Dump_Trigger := None;
      Manual_Trigger          : Boolean := False;
      Manual_Indication_Files : File_Sets.Set;
      --  Files containing manual indications (optional) when using the manual
      --  dump trigger.

      Dump_Filename_Env_Var_Opt : String_Option renames
        Args.String_Args (Opt_Dump_Filename_Env_Var);
      Dump_Filename_Prefix_Opt  : String_Option renames
        Args.String_Args (Opt_Dump_Filename_Prefix);

      Dump_Channel          : Any_Dump_Channel;
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

      for Arg of Args.String_List_Args (Opt_Dump_Trigger) loop
         declare
            Parsed_Dump_Trigger : Valid_Dump_Trigger;
         begin

            --  Process the dump trigger value

            Parsed_Dump_Trigger := Value (+Arg);
            case Parsed_Dump_Trigger is
               when Valid_Auto_Dump_Trigger =>
                  if Auto_Trigger /= None
                    and then Parsed_Dump_Trigger /= Auto_Trigger
                  then
                     Fatal_Error
                       ("Encountered --dump-trigger="
                        & Image (Parsed_Dump_Trigger)
                        & " after --dump-trigger="
                        & Image (Auto_Trigger)
                        & ". Multiple auto dump triggers are not supported.");
                  end if;
                  Auto_Trigger := Parsed_Dump_Trigger;

               when Manual                  =>
                  Manual_Trigger := True;
            end case;
         exception
            when Exc : Constraint_Error =>
               Fatal_Error (Exception_Info (Exc));
         end;
      end loop;

      --  Fallback to the default dump triggers if --dump-trigger wasn't seen.

      if not Manual_Trigger and then Auto_Trigger = None then
         Manual_Trigger := Default_Dump_Config.Manual_Trigger;
         Auto_Trigger := Default_Dump_Config.Auto_Trigger;
      end if;

      --  Expand manual dump files

      for Arg of Args.String_List_Args (Opt_Manual_Dump_Files) loop
         declare
            Non_Expanded_Args : Vector;
         begin
            --  Split the argument in a string list
            --  (Turn "foo.adb,bar.adb" in ["foo.adb", "bar.adb"])

            Append_From_String (Non_Expanded_Args, Arg);

            --  Expand possible response files

            Copy_Arg_List (Non_Expanded_Args, Manual_Dump_Files_Opt);
         end;
      end loop;

      --  Check the existence of manual dump indication files

      declare
         Cur : Cursor := Manual_Dump_Files_Opt.First;
      begin
         if Has_Element (Cur) and then not Manual_Trigger then
            Fatal_Error ("--manual-dump-files requires --dump-trigger=manual");
         end if;
         while Has_Element (Cur) loop
            declare
               Filename : constant String := +Element (Cur);
            begin
               if not Ada.Directories.Exists (Filename) then
                  Fatal_Error ("File " & Filename & " does not exist");
               end if;
               Manual_Indication_Files.Include (Create_Normalized (Filename));
               Cur := Next (Cur);
            end;
         end loop;
      end;

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
                  Auto_Trigger            => Auto_Trigger,
                  Manual_Trigger          => Manual_Trigger,
                  Manual_Indication_Files => Manual_Indication_Files,
                  Filename_Simple         => Dump_Filename_Simple,
                  Filename_Env_Var        => Dump_Filename_Env_Var,
                  Filename_Prefix         => Dump_Filename_Prefix);

            when Base64_Standard_Output =>
               Dump_Config :=
                 (Channel                 => Base64_Standard_Output,
                  Auto_Trigger            => Auto_Trigger,
                  Manual_Trigger          => Manual_Trigger,
                  Manual_Indication_Files => Manual_Indication_Files);
         end case;
      end return;
   end Load_Dump_Config;

   --------------------
   -- Unparse_Config --
   --------------------

   function Unparse_Config
     (Dump_Config : Any_Dump_Config) return Command_Line_Args
   is
      Result : Command_Line_Args;
   begin
      if Dump_Config.Auto_Trigger /= None then
         Result.Append
           (Create ("--dump-trigger=" & Image (Dump_Config.Auto_Trigger)));
      end if;

      if Dump_Config.Manual_Trigger then
         declare
            Manual_Trigger_Str : Unbounded_String := +"--dump-trigger=manual";
         begin
            for Dump_File of Dump_Config.Manual_Indication_Files loop
               Manual_Trigger_Str :=
                 Manual_Trigger_Str & "," & Dump_File.Display_Full_Name;
            end loop;

            Result.Append (Create (+Manual_Trigger_Str));
         end;
      end if;

      case Dump_Config.Channel is
         when Binary_File            =>
            Result.Append (Create ("--dump-channel=bin-file"));
            if Dump_Config.Filename_Simple then
               Result.Append (Create ("--dump-filename-simple"));
            end if;
            if Dump_Config.Filename_Env_Var /= "" then
               Result.Append
                 (Create
                    ("--dump-filename-env-var="
                     & (+Dump_Config.Filename_Env_Var)));
            end if;
            if Dump_Config.Filename_Prefix /= "" then
               Result.Append
                 (Create
                    ("--dump-filename-prefix="
                     & (+Dump_Config.Filename_Prefix)));
            end if;

         when Base64_Standard_Output =>
            Result.Append (Create ("--dump-channel=base64-stdout"));
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

   -----------
   -- Image --
   -----------

   function Image (Dump_Trigger : Valid_Dump_Trigger) return String is
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

   function Value (Dump_Trigger : String) return Valid_Dump_Trigger is
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

   Common_Switches_Cache : Command_Line_Arg_Vectors_Maps.Map;

   ---------------------
   -- Common_Switches --
   ---------------------

   function Common_Switches
     (Cmd : Command_Line.Command_Type) return Command_Line_Args
   is
      Cmd_Img    : constant String := Command_Type'Image (Cmd);
      Has_Config : constant Boolean :=
        Is_Present (Args, Option_Reference'(String_Opt, Opt_Config));
      --  Whether the --config flag is on the command line. If this is the
      --  case, do not pass the --target and --RTS flags (they will be parsed
      --  from the config).

      Result : Command_Line_Args;

      procedure Process
        (Opt         : Option_Reference;
         Opt_Name    : Unbounded_String;
         Opt_Args    : String_Vectors.Vector;
         Incremental : Boolean);
      --  Add the command line value of Option to Result if Cmd supports it

      -------------
      -- Process --
      -------------

      procedure Process
        (Opt         : Option_Reference;
         Opt_Name    : Unbounded_String;
         Opt_Args    : String_Vectors.Vector;
         Incremental : Boolean)
      is
         Mode : constant GPR2.Build.Command_Line.Signature_Mode :=
           (if Incremental
            then GPR2.Build.Command_Line.In_Signature
            else GPR2.Build.Command_Line.Ignore);
      begin
         if Supports (Arg_Parser, Cmd, Opt) then
            if Opt.Kind = Bool_Opt then
               Result.Append (Create (+Opt_Name, Mode));
            else
               for Opt_Arg of Opt_Args loop
                  Result.Append (Create (+Opt_Name, Mode));
                  Result.Append (Create (+Opt_Arg, Mode));
               end loop;
            end if;
         end if;
      end Process;

   begin
      if Common_Switches_Cache.Contains (Cmd_Img) then
         return Common_Switches_Cache.Element (Cmd_Img);
      end if;

      --  Unfortunately, we can't avoid the code duplication. Deal with all
      --  kinds of options: boolean, string and strings list. Do not pass the
      --  --target and --RTS flags if there is a --config flag.

      for Opt in Bool_Options loop
         Process_Option
           (Arg_Parser,
            Args,
            Option_Reference'(Bool_Opt, Opt),
            Process'Access);
      end loop;

      for Opt in String_Options loop
         if not Has_Config or else Opt not in Opt_Target | Opt_Runtime then
            Process_Option
              (Arg_Parser,
               Args,
               Option_Reference'(String_Opt, Opt),
               Process'Access);
         end if;
      end loop;

      for Opt in String_List_Options loop
         Process_Option
           (Arg_Parser,
            Args,
            Option_Reference'(String_List_Opt, Opt),
            Process'Access);
      end loop;

      --  Save in the cache then return

      Common_Switches_Cache.Insert (Cmd_Img, Result);
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
