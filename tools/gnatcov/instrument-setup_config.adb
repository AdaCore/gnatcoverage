------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

with Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.Regexp; use GNAT.Regexp;
with GNAT.Strings;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Coverage;
with Files_Table;   use Files_Table;
with JSON;          use JSON;
with Logging;
with Outputs;       use Outputs;
with Paths;         use Paths;
with Support_Files; use Support_Files;

package body Instrument.Setup_Config is

   GCC_Regexp : constant Regexp := Compile (".*gcc.*");
   GXX_Regexp : constant Regexp := Compile (".*g\+\+.*");

   function Find_Compiler_Prog
     (Compiler_Driver    : String;
      Program_Names      : String_Vectors.Vector;
      Output_Dir         : String;
      Fallback_To_System : Boolean := True) return String;
   --  Locate the program whose name is one of Program_Names elements in the
   --  Compiler_Driver installation directory. Output_Dir is the directory
   --  where temporary output files are generated. If the program is not
   --  found under the compiler driver installation, assume there is a
   --  system installation if Fallback_To_System is True (and so return the
   --  first name of Program_Names), otherwise error out.

   ------------------------
   -- Find_Compiler_Prog --
   ------------------------

   function Find_Compiler_Prog
     (Compiler_Driver    : String;
      Program_Names      : String_Vectors.Vector;
      Output_Dir         : String;
      Fallback_To_System : Boolean := True) return String
   is
      Driver_Arguments : String_Vectors.Vector;
      Output_Filename  : constant String := Output_Dir / "which-compiler-prog";
      Output_File      : File_Type;
   begin
      for Program_Name of Program_Names loop
         Driver_Arguments.Clear;
         Driver_Arguments.Append (+"--print-prog-name");
         Driver_Arguments.Append (Program_Name);

         Run_Command
           (Compiler_Driver,
            Arguments           => Driver_Arguments,
            Origin_Command_Name => "gnatcov instrument-setup",
            Output_File         => Output_Filename);

         --  Check if the output is a full path to an existing file. If this is
         --  the case, then store the program path.

         Open (Output_File, In_File, Output_Filename);
         declare
            Output : constant String := Get_Line (Output_File);
         begin
            Close (Output_File);
            if Ada.Directories.Exists (Output) then
               Ada.Directories.Delete_File (Output_Filename);
               return Output;
            end if;
         end;
      end loop;
      Ada.Directories.Delete_File (Output_Filename);
      if Fallback_To_System then
         return +Program_Names.First_Element;
      else
         Outputs.Fatal_Error
           ("Could not locate program "
            & (+Program_Names.First_Element)
            & " in compiler driver installation.");
      end if;
   end Find_Compiler_Prog;

   ---------------------
   -- Generate_Config --
   ---------------------

   procedure Generate_Config
     (Files_Of_Interest : File_Sets.Set;
      Coverage_Level    : String;
      Dump_Config       : Any_Dump_Config;
      Compiler_Drivers  : String_Sets.Set;
      Output_Dir        : String;
      Runtime_Project   : String)
   is
      Config    : constant JSON_Value := Create_Object;
      Compilers : constant JSON_Value := Create_Object;
      Linkers   : constant JSON_Value := Create_Object;
      --  For each compiler driver, location of the ld executable

      Nms : constant JSON_Value := Create_Object;
      --  For each compiler driver, location of the nm executable

      Exec_Suffix : constant String := Executable_Suffix;
   begin
      --  Find the installed coverage runtime, using gprls. TODO??? Deal with
      --  cross cases.
      --
      --  We are looking for the following lines:
      --  Source Search Path:
      --     some/path/to/include/
      --  [...]
      --  Object Search Path:
      --     some/path/to/lib/gnatcov_rts.static

      declare
         use Ada.Strings.Fixed;

         Args            : String_Vectors.Vector;
         Output_Filename : constant String := Output_Dir / "gprls_output";
         Output_File     : File_Type;
      begin
         Args.Append (+"-P");
         Args.Append (+Runtime_Project);
         Args.Append (+"-vP1");
         Run_Command
           (Command             => "gprls",
            Arguments           => Args,
            Origin_Command_Name => "gnatcov setup-integration",
            Output_File         => Output_Filename);
         Open (Output_File, In_File, Output_Filename);
         while not End_Of_File (Output_File) loop
            declare
               Line : constant String := Get_Line (Output_File);
            begin
               if Line = "Source Search Path:" then
                  Config.Set_Field
                    ("gnatcov_rts_include_dir",
                     Trim (Get_Line (Output_File), Left));
               elsif Line = "Object Search Path:" then
                  Config.Set_Field
                    ("gnatcov_rts_object_dir",
                     Trim (Get_Line (Output_File), Left));
               end if;
            end;
         end loop;
         Close (Output_File);
      end;

      --  Generate the compiler driver wrappers, checking that they are
      --  supported.

      for Compiler_Driver of Compiler_Drivers loop

         --  Locate the compiler driver, and if it is not found, terminate
         --  with an exception.

         declare
            use type GNAT.Strings.String_Access;
            Compiler_Fullname : constant GNAT.OS_Lib.String_Access :=
              GNAT.OS_Lib.Locate_Exec_On_Path (+Compiler_Driver);
         begin
            --  Check if the compiler executable is on the PATH

            if Compiler_Fullname = null then
               Outputs.Fatal_Error
                 ("Could not locate compiler "
                  & (+Compiler_Driver)
                  & " on the PATH.");
            end if;

            --  Check if this is a supported compiler and instantiate the
            --  compiler wrapper accordingly.

            declare
               Actual_Compiler_Driver : constant String :=
                 GNAT.OS_Lib.Normalize_Pathname (Compiler_Fullname.all);
               Actual_CD_Basename     : constant String :=
                 Ada.Directories.Simple_Name (Actual_Compiler_Driver);
               Success                : Boolean;
               Matched                : Boolean := True;
               Compiler_Identifier    : Unbounded_String;
            begin
               if GNAT.Regexp.Match (Actual_Compiler_Driver, GCC_Regexp)
                 or else GNAT.Regexp.Match (Actual_Compiler_Driver, GXX_Regexp)
               then
                  Compiler_Identifier := +"gcc";
               else
                  Matched := False;
               end if;

               if Matched then
                  Compilers.Set_Field
                    (+Compiler_Driver, Compiler_Fullname.all);

                  --  Then, find the linker associated to this compiler. Look
                  --  for ld / ld.bfd. TODO??? This may need reworking when we
                  --  want to support toolchain other than ours, not
                  --  necessarily packaging ld.

                  declare
                     Linker_Programs : String_Vectors.Vector;
                  begin
                     Linker_Programs.Append (+"ld");
                     Linker_Programs.Append (+"ld.bfd");

                     Linkers.Set_Field
                       (+Compiler_Driver,
                        Find_Compiler_Prog
                          (Compiler_Driver => Actual_Compiler_Driver,
                           Program_Names   => Linker_Programs,
                           Output_Dir      => Output_Dir));
                  end;

                  --  Do the same with nm, as we need to be able to inspect the
                  --  object / library file contents. TODO??? This may need
                  --  reworking when we want to support toolchain other than
                  --  ours, not necessarily packaging nm.

                  declare
                     Nm_Program : String_Vectors.Vector;
                  begin
                     Nm_Program.Append (+"nm");

                     Nms.Set_Field
                       (+Compiler_Driver,
                        Find_Compiler_Prog
                          (Compiler_Driver => Actual_Compiler_Driver,
                           Program_Names   => Nm_Program,
                           Output_Dir      => Output_Dir));
                  end;

                  GNAT.OS_Lib.Copy_File
                    (Name     =>
                       Support_Files.Libexec_Dir
                       / ("compiler_wrappers-"
                          & (+Compiler_Identifier)
                          & Exec_Suffix),
                     Pathname => Output_Dir / (+Compiler_Driver),
                     Success  => Success,
                     Mode     => GNAT.OS_Lib.Overwrite,
                     Preserve => GNAT.OS_Lib.Full);
                  if not Success then
                     Outputs.Fatal_Error
                       ("Could not generate a compiler wrapper in the given"
                        & " directory "
                        & Output_Dir);
                  end if;
               else
                  Outputs.Fatal_Error
                    ("Unsupported compiler driver: " & Actual_CD_Basename);
               end if;
            end;

         end;
      end loop;
      Config.Set_Field ("compilers", Compilers);
      Config.Set_Field ("linkers", Linkers);
      Config.Set_Field ("nms", Nms);

      --  Then, register the files of interest

      declare
         SFIs                   : SFI_Sets.Set;
         Files_Of_Interest_JSON : JSON_Array;

         File_Info : JSON_Value;
         --  JSON object containing the name of the file and its SID file

      begin
         --  Start by adding into the files table all of the files of interest

         for F of Files_Of_Interest loop
            SFIs.Insert (Get_Index_From_Full_Name (+F.Full_Name, Source_File));
         end loop;

         --  Then, compute file information (SID name) for every file of
         --  interest and add entries into the generated configuration.

         for SFI of SFIs loop
            File_Info := Create_Object;
            File_Info.Set_Field ("source-file", Get_Full_Name (SFI));
            File_Info.Set_Field ("sid-file", Get_Unique_Filename (SFI, "sid"));
            Append (Files_Of_Interest_JSON, File_Info);
         end loop;

         Config.Set_Field ("files-of-interest", Files_Of_Interest_JSON);
      end;

      --  Then, register the dump config that we will pass explicitly on the
      --  command line.

      declare
         Dump_Config_JSON       : constant JSON_Value := Create_Object;
         Manual_Dump_Files_JSON : JSON_Array;
      begin

         Dump_Config_JSON.Set_Field
           ("manual-dump-trigger", Create (Dump_Config.Manual_Trigger));
         if Dump_Config.Manual_Trigger then
            if Dump_Config.Manual_Indication_Files.Is_Empty then
               Outputs.Fatal_Error
                 ("Manual dump trigger FILES indication is missing,"
                  & " mandatory in integrated instrumentation mode:"
                  & " --dump-trigger=manual,FILES");
            else
               for Manual_Dump_File of Dump_Config.Manual_Indication_Files loop
                  Append
                    (Manual_Dump_Files_JSON,
                     Create (Display_Full_Name (Manual_Dump_File)));
               end loop;
               Dump_Config_JSON.Set_Field
                 ("manual-dump-files", Manual_Dump_Files_JSON);
            end if;
         end if;

         case Dump_Config.Auto_Trigger is
            when At_Exit                    =>
               Dump_Config_JSON.Set_Field ("auto-dump-trigger", "atexit");

            when Ravenscar_Task_Termination =>
               Dump_Config_JSON.Set_Field
                 ("auto-dump-trigger", "ravenscar-task-termination");

            when Main_End                   =>
               Dump_Config_JSON.Set_Field ("auto-dump-trigger", "main-end");

            when None                       =>
               null;
            --  Do not create the field when the there is no auto trigger
         end case;

         case Dump_Config.Channel is
            when Binary_File            =>
               Dump_Config_JSON.Set_Field ("dump-channel", "bin-file");
               if Dump_Config.Filename_Simple then
                  Dump_Config_JSON.Set_Field ("dump-filename-simple", True);
               end if;
               if Dump_Config.Filename_Env_Var /= "" then
                  Dump_Config_JSON.Set_Field
                    ("dump-filename-env-var", Dump_Config.Filename_Env_Var);
               end if;
               if Dump_Config.Filename_Prefix /= "" then
                  Dump_Config_JSON.Set_Field
                    ("dump-filename-prefix", Dump_Config.Filename_Prefix);
               end if;

            when Base64_Standard_Output =>
               Dump_Config_JSON.Set_Field ("dump-channel", "base64-stdout");
         end case;
         Config.Set_Field ("dump-config", Dump_Config_JSON);
      end;

      Config.Set_Field ("coverage_level", Coverage_Level);
      Config.Set_Field ("tag", Instrumentation_Tag);
      Config.Set_Field ("save_temps", Switches.Save_Temps);

      Config.Set_Field ("quiet", Switches.Quiet);
      declare
         Verbose   : Boolean;
         To_Enable : String_Vectors.Vector;
         Names     : JSON_Array;
      begin
         Logging.Get_Configuration (Verbose, To_Enable);
         for N of To_Enable loop
            Append (Names, Create (+N));
         end loop;
         Config.Set_Field ("verbose", Verbose);
         Config.Set_Field ("logs", Names);
      end;

      --  Dump the instrumentation configuration in a JSON file. Do not write
      --  the compact representation of the JSON as we may reach the character
      --  limit.

      Write
        (Filename => Output_Dir / Instrumentation_Config_Filename,
         Value    => Config,
         Compact  => False);
   end Generate_Config;

   -----------------
   -- Load_Config --
   -----------------

   function Load_Config (Config_File : String) return Instrumentation_Config is
      Result      : Instrumentation_Config;
      Channel     : Any_Dump_Channel;
      Config_JSON : JSON_Value;
      Parsed_JSON : constant Read_Result := JSON.Read (Config_File);
   begin
      if not Parsed_JSON.Success then
         Outputs.Fatal_Error
           ("Parsing error while reading the instrumentation configuration"
            & " file.");
      end if;
      Config_JSON := Parsed_JSON.Value;

      Switches.Save_Temps := Config_JSON.Get ("save_temps");
      Coverage.Set_Coverage_Levels (Config_JSON.Get ("coverage_level"));

      declare
         Verbose   : Boolean;
         To_Enable : String_Vectors.Vector;
      begin
         Switches.Quiet := Config_JSON.Get ("quiet");
         Verbose := Config_JSON.Get ("verbose");
         for N of JSON_Array'(Config_JSON.Get ("logs")) loop
            To_Enable.Append (+N.Get);
         end loop;
         Logging.Initialize (Verbose, To_Enable);
      end;

      declare
         FOI_JSON : constant JSON_Array :=
           Config_JSON.Get ("files-of-interest");
      begin
         if Is_Empty (FOI_JSON) then
            Every_File_Of_Interest := True;
         end if;
         for FOI of FOI_JSON loop
            Result.File_To_SID.Insert
              (Create_Normalized (FOI.Get ("source-file")),
               FOI.Get ("sid-file"));
         end loop;
      end;

      --  Read the dump config

      declare
         Dump_Config_JSON : constant JSON_Value :=
           Config_JSON.Get ("dump-config");
         Dump_Channel_Str : constant String :=
           Dump_Config_JSON.Get ("dump-channel");
      begin
         if Dump_Channel_Str = "bin-file" then
            Channel := Binary_File;
         elsif Dump_Channel_Str = "base64-stdout" then
            Channel := Base64_Standard_Output;
         else
            Outputs.Fatal_Error ("unsupported dump channel");
         end if;

         declare
            Dump_Config : Any_Dump_Config (Channel);
         begin

            --  Handle manual dump trigger and its indication files

            if Dump_Config_JSON.Get ("manual-dump-trigger") then
               Dump_Config.Manual_Trigger := True;

               if Dump_Config_JSON.Has_Field ("manual-dump-files") then
                  declare
                     Manual_Dump_Files_JSON : constant JSON_Array :=
                       Dump_Config_JSON.Get ("manual-dump-files");
                  begin
                     for Manual_Dump_File_JSON of Manual_Dump_Files_JSON loop
                        Dump_Config.Manual_Indication_Files.Include
                          (Create_From_UTF8
                             (String'(Get (Manual_Dump_File_JSON))));
                     end loop;
                  end;
               end if;
            end if;

            --  Handle auto dump trigger
            if Dump_Config_JSON.Has_Field ("auto-dump-trigger") then
               declare
                  Auto_Trigger_Str : constant String :=
                    Dump_Config_JSON.Get ("auto-dump-trigger");
               begin
                  if Auto_Trigger_Str = "atexit" then
                     Dump_Config.Auto_Trigger := At_Exit;
                  elsif Auto_Trigger_Str = "main-end" then
                     Dump_Config.Auto_Trigger := Main_End;
                  elsif Auto_Trigger_Str = "ravenscar-task-termination" then
                     Dump_Config.Auto_Trigger := Ravenscar_Task_Termination;
                  else
                     Outputs.Fatal_Error
                       ("unsupported dump trigger: " & Auto_Trigger_Str);
                  end if;
               end;
            end if;

            Result.Dump_Config := Dump_Config;
         end;
      end;

      declare

         procedure Fill_String_Map
           (Result : out String_Maps.Map; JSON_Object : JSON_Value);
         --  Fill the Result string map with the contents of the given JSON
         --  object

         procedure Fill_String_Map
           (Result : out String_Maps.Map; JSON_Object : JSON_Value)
         is
            procedure Insert_Element (Key : UTF8_String; Value : JSON_Value);

            procedure Insert_Element (Key : UTF8_String; Value : JSON_Value) is
            begin
               Result.Insert (+Key, Get (Value));
            end Insert_Element;
         begin
            Map_JSON_Object (JSON_Object, Insert_Element'Access);
         end Fill_String_Map;

      begin
         Fill_String_Map
           (Result.Compiler_Drivers, Config_JSON.Get ("compilers"));
         Fill_String_Map (Result.Linkers, Config_JSON.Get ("linkers"));
         Fill_String_Map (Result.Nms, Config_JSON.Get ("nms"));
      end;

      Result.Tag := Config_JSON.Get ("tag");
      Result.GNATcov_RTS_Include_Dir :=
        Config_JSON.Get ("gnatcov_rts_include_dir");
      Result.GNATcov_RTS_Object_Dir :=
        Config_JSON.Get ("gnatcov_rts_object_dir");
      return Result;
   end Load_Config;

end Instrument.Setup_Config;
