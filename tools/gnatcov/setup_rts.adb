------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Directories;           use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Text_IO;               use Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.Regexp;

with GNATCOLL.JSON;     use GNATCOLL.JSON;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GPR2.Context;
with GPR2.Containers;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Configuration;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;

with JSON;          use JSON;
with Outputs;       use Outputs;
with Paths;         use Paths;
with Subprocesses;  use Subprocesses;
with Support_Files;
with Temp_Dirs;     use Temp_Dirs;
with Text_Files;

package body Setup_RTS is

   use type GPR2.Filename_Optional;

   type Library_Support is (None, Static_Only, Full);
   --  Support for libraries in the given target/runtime configuration, as
   --  defined in GPR configuration files (see the Library_Support GPR
   --  configuration attribute).

   LK_Static      : aliased constant String := "static";
   LK_Static_PIC  : aliased constant String := "static-pic";
   LK_Relocatable : aliased constant String := "relocatable";
   Library_Kinds  : constant array (Positive range <>)
                             of access constant String :=
     (LK_Static'Access, LK_Static_PIC'Access, LK_Relocatable'Access);

   Ravenscar_RTS_Regexp : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile (".*(ravenscar|light-tasking|embedded).*");

   procedure Load_Project
     (Project      : in out GPR2.Project.Tree.Object;
      Project_File : String;
      Target       : String;
      RTS          : String;
      Config_File  : String;
      Context      : GPR2.Context.Object := GPR2.Context.Empty);
   --  Load Project_File for the given Target/RTS/Config_File/Context.
   --
   --  This does not raise an exception in case of project loading failure:
   --  callers are supposed to check Project.Log_Messages to handle possible
   --  errors.

   function Setup_Project
     (Temp_Dir     : Temporary_Directory;
      Project_File : String;
      Install_Name : String) return String;
   --  Copy the directory that contains Project_File (i.e. the sources for the
   --  instrumentation runtime project to build) to Temp_Dir.
   --
   --  If the project name for the given Project_File does not match
   --  Install_Name, also create an extending project in Temp_Dir that does.
   --
   --  Return the absolute file name for the project file to build in the end
   --  (the copied project file, or the extending one).
   --
   --  Note that ideally, we should avoid copying the project file and its
   --  sources: the assumption that the project layout is self-contained in
   --  that directory could be wrong in edge cases (e.g. if the Source_Dirs
   --  attribute contains a ".." component). Besides, that copy could be
   --  costly, for instance if users left big binaries unrelated to the work of
   --  "gnatcov setup" in that directory.
   --
   --  An out-of-tree build feature in GPRbuild could allow us to avoid the
   --  copy. Unfortunately the existing one (--relocate-build-tree/--root-dir)
   --  works relocating the whole tree starting from a parent directory that
   --  contains the object directories of all the projects to build. In our
   --  case, the "gnatcov_rts" project may be in one place in the filesystem
   --  while the temporary directory containing the extended project may be in
   --  a completely different place, forcing GPRbuild to use the filesystem
   --  root directory as its relocation root, which would be highly inefficient
   --  and could even trigger path size limits on Windows systems.

   procedure Load_Project_Parameters
     (Project_File     : String;
      Target           : String;
      RTS              : String;
      Config_File      : String;
      Actual_RTS       : out Unbounded_String;
      Auto_RTS_Profile : out Any_RTS_Profile;
      Lib_Support      : out Library_Support);
   --  Load the project file at Project_File using the Target/RTS/Config_File
   --  parameters, then try to guess the profile of the actual runtime in
   --  effect (Auto_RTS_Profile) and determine the support for libraries for
   --  this configuration (Lib_Support).

   procedure Uninstall (Project_Name, Prefix : String);
   --  Try to uninstall a previous installation of the project called
   --  Project_Name in the given Prefix installation directory.

   procedure Build_And_Install
     (Project_File : String;
      Temp_Dir     : Temporary_Directory;
      Common_Args  : String_Vectors.Vector;
      Build_Args   : String_Vectors.Vector;
      Install_Args : String_Vectors.Vector;
      Library_Kind : String := "");
   --  Build and install the Project_File instrumentation project in the given
   --  temporary directory.
   --
   --  Common_Args are arguments to be passed both to gprbuild and gprinstall,
   --  Build_Args to gprbuild, and Install_Args are passed only to gprinstall.
   --
   --  If Library_Kind is not an empty string, build for that library kind and
   --  install it as a variant.

   Load_Setup_Config_Error : exception;
   --  Exception raised when loading the setup config file failed (because the
   --  runtime project could not be loaded / the config file could not be
   --  decoded etc.).

   function Load
     (Project_File      : String;
      Setup_Config_File : Virtual_File) return Setup_Config;
   --  Helper for the public Load function: load the setup config file at
   --  Setup_Config_File. On success, use Project_File for the result's
   --  Project_File component.

   function Setup_Config_File_Basename (Project_Name : String) return String
   is ("setup-config-" & To_Lower (Base_Name (Project_Name)) & ".json");
   --  Return the base filename for the setup config file corresponding to the
   --  given Project_Name.

   function Load (J : JSON_Value) return Setup_Config
   with Pre => J.Kind = JSON_Object_Type;
   --  Helper for the public Load function: load a setup config from the given
   --  JSON document.

   procedure Save_Setup_Config
     (Project_Dir  : String;
      Project_Name : String;
      Config       : Setup_Config);
   --  Write Config as a JSON file in Project_Dir for the Project_Name runtime
   --  project to install. Use a filename that will match the Install'Artifacts
   --  attribute in the runtime project file.

   function Has_Shared_Lib
     (RTS_Dir : String; Shared_Lib_Ext : String) return Boolean;
   --  Return whether there is any file with extension Shared_Lib_Ext in the
   --  directory RTS_Dir/adalib.

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Project      : in out GPR2.Project.Tree.Object;
      Project_File : String;
      Target       : String;
      RTS          : String;
      Config_File  : String;
      Context      : GPR2.Context.Object := GPR2.Context.Empty)
   is
      PF : constant GPR2.Path_Name.Object :=
        GPR2.Path_Name.Create_File
           (GPR2.Filename_Type (Project_File), GPR2.Path_Name.No_Resolution);
   begin
      --  According to the documentation, an error message is recorded in
      --  Project when Load and Load_Autoconf raise GPR2.Project_Error
      --  exceptions, so we can just ignore this exception and let callers deal
      --  with errors recorded in Project.Log_Messages.

      --  If a configuration project file is provided, just load it to create
      --  the configuration.

      if Config_File /= "" then
         declare
            F      : constant GPR2.Path_Name.Object :=
              GPR2.Path_Name.Create_File (GPR2.Filename_Type (Config_File));
            Config : constant GPR2.Project.Configuration.Object :=
              GPR2.Project.Configuration.Load (F);
         begin
            Project.Load
              (Filename         => PF,
               Context          => Context,
               Absent_Dir_Error => GPR2.Project.Tree.No_Error,
               Config           => Config);
         exception
            when GPR2.Project_Error =>
               null;
         end;

      --  Otherwise, use the "auto configuration" mode that will use the
      --  optional target and RTS information. Always require C (needed for the
      --  core coverage runtime) and require the Ada language only if enabled.

      else
         declare
            RTS_Map       : GPR2.Containers.Lang_Value_Map;
            Actual_Target : constant GPR2.Optional_Name_Type :=
              GPR2.Optional_Name_Type (Target);
         begin
            RTS_Map.Include (GPR2.C_Language, RTS);
            if Src_Enabled_Languages (Ada_Language) then
               RTS_Map.Include (GPR2.Ada_Language, RTS);
            end if;

            begin
               Project.Load_Autoconf
                 (Filename          => PF,
                  Context           => Context,
                  Absent_Dir_Error  => GPR2.Project.Tree.No_Error,
                  Target            => Actual_Target,
                  Language_Runtimes => RTS_Map);
            exception
               when GPR2.Project_Error =>
                  null;
            end;
         end;
      end if;
   end Load_Project;

   -----------
   -- Image --
   -----------

   function Image (Profile : Any_RTS_Profile) return String is
   begin
      return (case Profile is
              when Auto     => "auto",
              when Full     => "full",
              when Embedded => "embedded");
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Profile : String) return Any_RTS_Profile is
   begin
      if Profile = "auto" then
         return Auto;
      elsif Profile = "full" then
         return Full;
      elsif Profile = "embedded" then
         return Embedded;
      else
         return
           (raise Constraint_Error
            with "invalid RTS profile: " & Profile);
      end if;
   end Value;

   -------------------------
   -- Default_Dump_Config --
   -------------------------

   function Default_Dump_Config
     (RTS_Profile : Resolved_RTS_Profile; RTS : String) return Any_Dump_Config
   is
   begin
      case RTS_Profile is
         when Full =>

            --  The runtime is full so, use the handiest dump configuration:
            --  directly create source trace files, and to it automatically at
            --  process exit.

            return (Channel => Binary_File, Trigger => At_Exit, others => <>);

         when Embedded =>

            --  The runtime may be restricted for embedded targets: use base64
            --  encoded traces on the standard output, and trigger it when the
            --  main ends (by default) or at task termination (Ranverscar
            --  runtimes).

            return
              (Channel => Base64_Standard_Output,
               Trigger => (if GNAT.Regexp.Match (RTS, Ravenscar_RTS_Regexp)
                           then Ravenscar_Task_Termination
                           else Main_End));

      end case;
   end Default_Dump_Config;

   --------------------------
   -- Default_Project_File --
   --------------------------

   function Default_Project_File return String is
   begin
      return Result : constant String :=
        Support_Files.In_Share_Dir ("gnatcov_rts") / "gnatcov_rts.gpr"
      do
         if not Exists (Result) then
            Fatal_Error ("No instrumentation runtime project file at "
                         & Result);
         end if;
      end return;
   end Default_Project_File;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name (Project_File : String) return String is
   begin
      --  Remove the directory and the extension from Project_File

      return Base_Name (Project_File);
   end Project_Name;

   -------------------
   -- Setup_Project --
   -------------------

   function Setup_Project
     (Temp_Dir     : Temporary_Directory;
      Project_File : String;
      Install_Name : String) return String
   is
      Tree_Dir : constant String := Containing_Directory (Project_File);
      --  Directory that contains the project to copy/build/install

      Tree_Dir_Copy : constant String := Temp_Dir.Directory_Name / "src";
      --  Name of the copy for that directory

      Project_File_Copy : constant String :=
        Tree_Dir_Copy / Simple_Name (Project_File);
      --  Name for the copy of Project_File

      Name : constant String := Project_Name (Project_File);
      File : Text_Files.File_Type;
   begin
      --  Copy the sources of the project to build

      declare
         Success  : Boolean;
      begin
         Create (+Tree_Dir).Copy (+Tree_Dir_Copy, Success);
         if not Success then
            Fatal_Error ("Could not copy the instrumentation runtime sources");
         end if;
      end;

      --  If the installation name matches the given project file, we can
      --  return the project file directly. If not, create the extending
      --  project file and return it.

      if Name = Install_Name then
         return Project_File_Copy;
      else
         return Result : constant String :=
           Temp_Dir.Directory_Name / (Install_Name & ".gpr")
         do
            File.Create (Result);
            File.Put_Line ("project " & Install_Name);
            File.Put_Line ("  extends """ & Project_File_Copy & """");
            File.Put_Line ("is");

            --  Library and object directories are not inherited, so we have to
            --  redefine them.

            File.Put_Line ("  for Object_Dir use " & Name & "'Object_Dir;");
            File.Put_Line ("  for Library_Dir use " & Name & "'Library_Dir;");

            File.Put_Line ("end " & Install_Name & ";");
            File.Close;
         end return;
      end if;
   end Setup_Project;

   -----------------------------
   -- Load_Project_Parameters --
   -----------------------------

   procedure Load_Project_Parameters
     (Project_File     : String;
      Target           : String;
      RTS              : String;
      Config_File      : String;
      Actual_RTS       : out Unbounded_String;
      Auto_RTS_Profile : out Any_RTS_Profile;
      Lib_Support      : out Library_Support)
   is
      Has_Ada       : constant Boolean := Src_Enabled_Languages (Ada_Language);
      Main_Language : constant GPR2.Language_Id :=
        (if Has_Ada
         then GPR2.Ada_Language
         else GPR2.C_Language);

      Ctx : GPR2.Context.Object;
      Prj : GPR2.Project.Tree.Object;
   begin
      --  Compute scenario variables that determine the set of languages, as we
      --  may not be able to load the full Ada+C project if there is no Ada
      --  toolchain available.

      if Has_Ada then
         Ctx.Include ("GNATCOV_RTS_WITH_ADA", "true");
      end if;

      --  Now load the project

      Load_Project (Prj, Project_File, Target, RTS, Config_File, Ctx);

      --  Print all messages in verbose mode, and all but the Information ones
      --  otherwise. Abort on error, or if we failed to get a runtime project
      --  (when we cannot find a toolchain, GPR2 only emits warnings).
      --
      --  C-only projects do not require a runtime, so do not complain if there
      --  is no runtime project in this case.

      declare
         Logs      : constant access GPR2.Log.Object := Prj.Log_Messages;
         Has_Error : constant Boolean :=
           Logs.Has_Error
           or else (Has_Ada and then not Prj.Has_Runtime_Project);
      begin
         Logs.Output_Messages (Information => Verbose);
         if Has_Error then
            Fatal_Error ("Could not load the coverage runtime project file");
         end if;
      end;

      Actual_RTS :=
        (if Prj.Has_Runtime_Project
         then To_Unbounded_String (String (Prj.Runtime (Main_Language)))
         else Null_Unbounded_String);

      --  In verbose mode, show the actual names for the target and the runtime
      --  that GPR2 uses. They can be slightly different from the names users
      --  passed.

      if Verbose then
         Put_Line
           ("Actual target: " & String (Prj.Target (Canonical => True)));
         Put_Line ("Actual RTS: " & To_String (Actual_RTS));
      end if;

      --  The best heuristic we have to determine if the actual runtime is
      --  "full" is to look for an Ada source file that is typically found in
      --  full runtime: "a-comlin.ads" for the Ada.Command_Line unit. If we do
      --  not have it, consider that the runtime is embedded.
      --
      --  When only C is enabled and we do not have a runtime at hand to guess
      --  what is available, fall back to the full profile for native
      --  applications and the embedded runtime otherwise.

      Auto_RTS_Profile := Embedded;
      if Has_Ada then
         for F of Prj.Runtime_Project.Sources loop
            if F.Path_Name.Simple_Name = "a-comlin.ads" then
               Auto_RTS_Profile := Full;
               exit;
            end if;
         end loop;
      else
         declare
            use GPR2;
            Tool_Name : constant Name_Type := "gcc";
         begin
            --  Add_Tool_Prefix's documentation states that the tool name is
            --  returned unchanged for native compilation, so use this as a
            --  proxy to determine if we are setting up the gnatcov RTS for
            --  a cross target or not.

            Auto_RTS_Profile :=
              (if Tool_Name = Prj.Add_Tool_Prefix (Tool_Name)
               then Full
               else Embedded);
         end;
      end if;

      --  First, determine what kind of library is supported by the current
      --  target / RTS configuration. If we have full library support, still
      --  try to determine if we have access to a shared Ada runtime as it is
      --  not always available.
      --
      --  To do this, we search in the <Runtime_Dir>/adalib directory any file
      --  with a <Shared_Library_Suffix> extension. If we can't find anything
      --  then assume only static libraries are supported.
      --
      --  If GPRconfig fails to find a toolchain, it only emits warnings (no
      --  Invalid_Project exception raise in the call to Load above), so when
      --  reaching this point, assume empty strings for all the attributes,
      --  (and thus no library support).

      declare
         function Config_Attribute
           (Id    : GPR2.Q_Attribute_Id;
            Index : GPR2.Project.Attribute_Index.Object :=
              GPR2.Project.Attribute_Index.Undefined) return String;
         --  Return the string value for the attribute denoted by Id, at Index.
         --  If the attribute value is not defined, return the empty string.

         ----------------------
         -- Config_Attribute --
         ----------------------

         function Config_Attribute
           (Id    : GPR2.Q_Attribute_Id;
            Index : GPR2.Project.Attribute_Index.Object :=
              GPR2.Project.Attribute_Index.Undefined) return String
         is
            Attr : constant GPR2.Project.Attribute.Object :=
              Prj.Configuration.Corresponding_View.Attribute (Id, Index);
         begin
            return (if Attr.Is_Defined then String (Attr.Value.Text) else "");
         end Config_Attribute;

         Lib_Support_Str : constant String :=
           Config_Attribute (GPR2.Project.Registry.Attribute.Library_Support);

         RTS_Dir : constant String :=
           Config_Attribute
             (GPR2.Project.Registry.Attribute.Runtime_Dir,
              Index =>
                GPR2.Project.Attribute_Index.Create (GPR2.Ada_Language));

         Shared_Lib_Ext : constant String :=
           Config_Attribute
             (GPR2.Project.Registry.Attribute.Shared_Library_Suffix);
      begin
         Lib_Support := Library_Support'Value (Lib_Support_Str);

         if Lib_Support = Full
           and then not Has_Shared_Lib (RTS_Dir, Shared_Lib_Ext)
         then
            Lib_Support := Static_Only;
         end if;

      exception
         when Constraint_Error =>
            Fatal_Error ("Cannot get library support for this configuration");
      end;

      if Verbose then
         Put_Line ("Library support: " & Lib_Support'Image);
      end if;
   end Load_Project_Parameters;

   ---------------
   -- Uninstall --
   ---------------

   procedure Uninstall (Project_Name, Prefix : String) is
      Dummy : Boolean;
      Args  : String_Vectors.Vector;
   begin
      if Verbose then
         Put_Line ("Trying to uninstall " & Project_Name & " from " & Prefix);
      end if;

      Args.Append (+"--uninstall");

      if Prefix /= "" then
         Args.Append (+("--prefix=" & Prefix));
      end if;

      --  Convert the project name to lower case, as it is case-sensitive for
      --  gprinstall, and must correspond to the case of the project file that
      --  is installed (without the ".gpr" extension).

      Args.Append (+To_Lower (Project_Name));
      Args.Append (+"--sources-subdir=" & ("include" / "gnatcov_rts"));

      --  The project may not have been installed there yet, so ignore errors

      Dummy := Run_Command
        (Command             => "gprinstall",
         Arguments           => Args,
         Origin_Command_Name => "gprinstall",
         Out_To_Null         => not Verbose,
         Ignore_Error        => True);
   end Uninstall;

   -----------------------
   -- Build_And_Install --
   -----------------------

   procedure Build_And_Install
     (Project_File : String;
      Temp_Dir     : Temporary_Directory;
      Common_Args  : String_Vectors.Vector;
      Build_Args   : String_Vectors.Vector;
      Install_Args : String_Vectors.Vector;
      Library_Kind : String := "")
   is
      use String_Vectors;

      Env : String_Maps.Map;

      Actual_Common_Args  : String_Vectors.Vector := Common_Args;
      Actual_Install_Args : String_Vectors.Vector := Install_Args;
   begin
      --  Complete argument lists: pass the project, automatically create
      --  missing output directories (-p is by default nowadays, but we want to
      --  support older GPR tools).

      Actual_Common_Args.Append (+("-P" & Project_File));
      Actual_Common_Args.Append (+"-p");

      --  If we manage several library kinds, build and install the requested
      --  variant.

      if Library_Kind /= "" then
         Actual_Common_Args.Append (+("-XLIBRARY_TYPE=" & Library_Kind));
         Actual_Install_Args.Append (+"--build-var=LIBRARY_TYPE");
         Actual_Install_Args.Append (+("--build-name=" & Library_Kind));
      end if;

      --  Make the default "gnatcov_rts" project file available to GPR tools as
      --  we may be processing a project that extends it.

      declare
         use Ada.Environment_Variables;

         Var_Name : constant String := "GPR_PROJECT_PATH";
         Dir      : constant String :=
           Support_Files.In_Share_Dir ("gnatcov_rts");
         Path     : constant String := Value (Var_Name, "");
         New_Path : constant String :=
           (if Path = ""
            then Dir
            else Dir & GNAT.OS_Lib.Path_Separator & Path);
      begin
         Env.Insert
           (To_Unbounded_String (Var_Name), To_Unbounded_String (New_Path));
      end;

      --  Now run gprbuild and gprinstall

      Run_Command
        (Command             => "gprbuild",
         Arguments           => Actual_Common_Args & Build_Args,
         Environment         => Env,
         Origin_Command_Name => "gprbuild");
      Run_Command
        (Command             => "gprinstall",
         Arguments           => Actual_Common_Args & Actual_Install_Args,
         Environment         => Env,
         Origin_Command_Name => "gprinstall");
   end Build_And_Install;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Project_File : String;
      Target       : String;
      RTS          : String;
      Config_File  : String;
      Prefix       : String;
      RTS_Profile  : Any_RTS_Profile;
      Install_Name : String;
      Gargs        : String_Vectors.Vector)
   is
      Temp_Dir : Temporary_Directory;

      Actual_RTS         : Unbounded_String;
      Auto_RTS_Profile   : Any_RTS_Profile;
      Actual_RTS_Profile : Resolved_RTS_Profile;
      Lib_Support        : Library_Support;
   begin
      --  The core runtime is implemented in C, so C must be in the set of
      --  enabled languages.

      if not Src_Enabled_Languages (C_Language) then
         Fatal_Error ("The C language must be enabled");
      end if;

      --  Load the instrumentation runtime project to know about the actual
      --  runtime and the library support for this configuration.

      Load_Project_Parameters
        (Project_File,
         Target,
         RTS,
         Config_File,
         Actual_RTS,
         Auto_RTS_Profile,
         Lib_Support);

      --  If a specific RTS profile was requested, use it

      Actual_RTS_Profile :=
        (if RTS_Profile = Auto
         then Auto_RTS_Profile
         else RTS_Profile);

      if Verbose then
         Put_Line ("Actual RTS profile: " & Actual_RTS_Profile'Image);
      end if;

      --  Create the temporary directory to host the build of the
      --  instrumentation runtime and other temporary files.

      Create_Temporary_Directory
        (Temp_Dir, "gnatcov_rts", Auto_Delete => not Save_Temps);

      --  If an alternative installation name for the instrumentation runtime
      --  project is requested, create an extending project with that name and
      --  use it later on.

      declare
         Actual_Project_File : constant String :=
           Setup_Project (Temp_Dir, Project_File, Install_Name);

         --  Now compute the default dump configuration to be recorded with the
         --  installed instrumentation runtime project. First comute the
         --  default dump config for the selected language runtime, then refine
         --  it with the command-line arguments.

         Dump_Config         : constant Any_Dump_Config :=
           Load_Dump_Config
             (Default_Dump_Config
                (Actual_RTS_Profile, To_String (Actual_RTS)));
      begin
         --  Try to uninstall a previous installation of the instrumentation
         --  runtime in the requested prefix. This is to avoid installation
         --  update issues: for instance, the "Language" project attribute in
         --  the already installed project could be set to ("Ada", "C") whereas
         --  we are here installing a runtime only for "C". Uninstalling the
         --  project first allows gprinstall to start from a sanitized
         --  environment, and thus the installed project to accurately describe
         --  what is installed.

         Uninstall (Install_Name, Prefix);

         --  Save the setup profile and default values in the instrumented
         --  project.

         Save_Setup_Config
           (Containing_Directory (Actual_Project_File),
            Install_Name,
            (Project_File        => <>,
             RTS_Profile         => Actual_RTS_Profile,
             RTS_Profile_Present => True,
             Default_Dump_Config => Dump_Config));

         --  Check that the RTS profile is compatible with the selected
         --  defaults for the dump config.

         declare
            Dummy : constant Boolean :=
              Check_RTS_Profile (Actual_RTS_Profile, Dump_Config);
         begin
            null;
         end;

         --  Now build and install the instrumentation runtime

         declare
            Common_Args, Build_Args, Install_Args : String_Vectors.Vector;
         begin
            if Target /= "" then
               Common_Args.Append (+("--target=" & Target));
            end if;
            if RTS /= "" then
               Common_Args.Append (+("--RTS=" & RTS));
            end if;
            if Config_File /= "" then
               Common_Args.Append (+("--config=" & Config_File));
            end if;
            if Prefix /= "" then
               Install_Args.Append (+("--prefix=" & Prefix));
            end if;

            --  Avoid installing several time the same set of sources

            Install_Args.Append
              (+"--sources-subdir=" & ("include" / Install_Name));

            --  Let the project know about its installation name

            Common_Args.Append
              (+("-XGNATCOV_RTS_PROJECT_NAME=" & Install_Name));

            Build_Args.Append_Vector (Gargs);

            --  Tune external variables according to enabled languages and the
            --  RTS profile.

            declare
               With_Ada : constant Boolean :=
                 Src_Enabled_Languages (Ada_Language);
            begin
               Common_Args.Append
                 (+("-XGNATCOV_RTS_WITH_ADA=" & To_Lower (With_Ada'Image)));
               Common_Args.Append
                 (+("-XGNATCOV_RTS_RTS_PROFILE="
                    & Image (Actual_RTS_Profile)));
            end;

            case Lib_Support is
               when None =>
                  Fatal_Error
                    ("This target/runtime configuration does not support"
                     & " library projects: cannot build the instrumentation"
                     & " runtime");

               when Static_Only =>
                  Build_And_Install
                    (Actual_Project_File,
                     Temp_Dir,
                     Common_Args,
                     Build_Args,
                     Install_Args);

               when Full =>
                  for LK of Library_Kinds loop
                     Build_And_Install
                       (Actual_Project_File,
                        Temp_Dir,
                        Common_Args,
                        Build_Args,
                        Install_Args,
                        LK.all);
                  end loop;
            end case;
         end;
      end;
   end Setup;

   ----------
   -- Load --
   ----------

   function Load
     (Target          : String;
      RTS             : String;
      Config_File     : String;
      Runtime_Project : String) return Setup_Config
   is
      Result            : Setup_Config := Default_Setup_Config;
      Setup_Config_File : Virtual_File;

      Prj_Ext      : constant String := ".gpr";
      Prj_Filename : constant String :=
        (if not Has_Suffix (Runtime_Project, Prj_Ext)
            and then Exists (Runtime_Project & Prj_Ext)
         then Runtime_Project & Prj_Ext
         else Runtime_Project);
      --  Name of the runtime project file to load it with GPR2.
      --
      --  TODO??? (V921-006) While the call to Prj.Load below is responsible
      --  for resolving GPR file base names to full names, it is not able to
      --  append the ".gpr" extension when missing, whereas command-line tools
      --  are supposed to accept this case. Add ".gpr" ourselves when needed
      --  for now, and remove this special case when GPR2 is able to do it

      Prj : GPR2.Project.Tree.Object;

      Project_File : Virtual_File;
      --  Name of the runtime project file after it has been loaded
      --  (i.e. resolved file name).
   begin
      --  Load the runtime project file, only to locate the prefix where it has
      --  been installed: do not print any error message, and if the project
      --  fails to load, just return the default setup config.

      Load_Project (Prj, Prj_Filename, Target, RTS, Config_File);
      if Prj.Log_Messages.Has_Error then
         if Verbose then
            Put_Line
              ("Could not load the coverage runtime project to get dump"
               & " options defaults: " & Runtime_Project);
            Prj.Log_Messages.Output_Messages;
         end if;
         raise Load_Setup_Config_Error;
      end if;

      --  The project file is in $PREFIX/share/gpr, so get $PREFIX first and
      --  then look for the config file under it.

      Project_File := Create (+Prj.Root_Project.Path_Name.Value);
      if Verbose then
         Put_Line
           ("Loaded the coverage runtime project at: "
            & (+Project_File.Full_Name));
      end if;

      declare
         Prefix               : constant Virtual_File :=
           Project_File.Get_Parent.Get_Parent.Get_Parent;
         Config_File_Basename : constant String :=
           Setup_Config_File_Basename (Runtime_Project);
      begin
         Setup_Config_File :=
           Prefix / (+"share") / (+"gnatcov_rts") / (+Config_File_Basename);
      end;

      if Setup_Config_File.Is_Regular_File then
         Result := Load (+Project_File.Full_Name, Setup_Config_File);
      elsif Verbose then
         Put_Line ("Could not find the setup config file: "
                   & (+Setup_Config_File.Full_Name));
         raise Load_Setup_Config_Error;
      end if;

      --  At that point, setup config file was loaded successfully. Otherwise,
      --  we will return through the below exception handler.

      if Verbose then
         Put_Line ("Successfully loaded the setup configuration file "
                   & (+Setup_Config_File.Full_Name) & ".");
      end if;
      return Result;

   exception
      when Load_Setup_Config_Error =>
         return Result;
   end Load;

   function Load
     (Project_File      : String;
      Setup_Config_File : Virtual_File) return Setup_Config
   is
      --  Load and parse the setup config file

      Parsed_JSON : constant Read_Result := Read (Setup_Config_File);
   begin
      --  If parsing was successful, load the various parameters from the JSON
      --  document. Otherwise, unless the verbose mode is active, silently
      --  ignore the setup config file.

      if Parsed_JSON.Success then
         if Parsed_JSON.Value.Kind = JSON_Object_Type then
            return Result : Setup_Config := Load (Parsed_JSON.Value) do
               Result.Project_File := To_Unbounded_String (Project_File);
            end return;
         end if;

      elsif Verbose then
         Put_Line ("Parsing error while reading the setup config file:");
         Put_Line (Format_Parsing_Error (Parsed_JSON.Error));
      end if;

      raise Load_Setup_Config_Error;
   end Load;

   function Load (J : JSON_Value) return Setup_Config is

      Format_Error : exception;
      --  Exception to raise in this function when the format of the setup
      --  config file is unexpected and loading it cannot continue.

      procedure Check_Field (Name : String; Kind : JSON_Value_Type);
      --  Raise a Format_Error exception if J does not have a Name field or if
      --  that field does not have the given kind.

      function Get (Name : String) return Unbounded_String;
      --  Return the Name field in J as a string. If there is no such field or
      --  if it is not a string, raise a Format_Error exception.

      function Get (Name : String) return Boolean;
      --  Likewise, but for a boolean field

      -----------------
      -- Check_Field --
      -----------------

      procedure Check_Field (Name : String; Kind : JSON_Value_Type) is
      begin
         if not J.Has_Field (Name) then
            raise Format_Error with "missing " & Name & " field";
         end if;

         if J.Get (Name).Kind /= Kind then
            raise Format_Error with "invalid " & Name & " field";
         end if;
      end Check_Field;

      ---------
      -- Get --
      ---------

      function Get (Name : String) return Unbounded_String is
      begin
         Check_Field (Name, JSON_String_Type);
         return J.Get (Name);
      end Get;

      function Get (Name : String) return Boolean is
      begin
         Check_Field (Name, JSON_Boolean_Type);
         return J.Get (Name);
      end Get;

      Result : Setup_Config := Default_Setup_Config;

      Channel : Any_Dump_Channel;
      Trigger : Any_Dump_Trigger;

   begin
      declare
         RTS_Profile : constant String := To_String (Get ("rts-profile"));
      begin
         Result.RTS_Profile := Value (RTS_Profile);
         Result.RTS_Profile_Present := True;
      exception
         when Constraint_Error =>
            raise Format_Error with "invalid rts-profile field";
      end;

      begin
         Channel := Value (To_String (Get ("dump-channel")));
      exception
         when Constraint_Error =>
            raise Format_Error with "invalid dump-channel field";
      end;

      begin
         Trigger := Value (To_String (Get ("dump-trigger")));
      exception
         when Constraint_Error =>
            raise Format_Error with "invalid dump-trigger field";
      end;

      case Channel is
         when Binary_File =>
            Result.Default_Dump_Config :=
              (Channel          => Binary_File,
               Trigger          => Trigger,
               Filename_Simple  => Get ("dump-filename-simple"),
               Filename_Env_Var => Get ("dump-filename-env-var"),
               Filename_Prefix  => Get ("dump-filename-prefix"));

         when Base64_Standard_Output =>
            Result.Default_Dump_Config :=
              (Channel => Base64_Standard_Output,
               Trigger => Trigger);
      end case;

      return Result;

   exception
      when Exc : Format_Error =>
         if Verbose then
            Put_Line
              ("Setup config file decoding error: "
               & Exception_Information (Exc));
         end if;
         raise Load_Setup_Config_Error;
   end Load;

   -----------------------
   -- Save_Setup_Config --
   -----------------------

   procedure Save_Setup_Config
     (Project_Dir  : String;
      Project_Name : String;
      Config       : Setup_Config)
   is
      Config_Filename : constant String :=
        Project_Dir / Setup_Config_File_Basename (Project_Name);

      --  Create the JSON to write in the setup config file

      J : constant JSON_Value := Create_Object;
   begin
      J.Set_Field ("rts-profile", Image (Config.RTS_Profile));
      declare
         Dump_Cfg : Any_Dump_Config renames Config.Default_Dump_Config;
      begin
         J.Set_Field ("dump-channel", Image (Dump_Cfg.Channel));
         J.Set_Field ("dump-trigger", Image (Dump_Cfg.Trigger));
         case Dump_Cfg.Channel is
            when Binary_File =>
               J.Set_Field
                 ("dump-filename-simple", Dump_Cfg.Filename_Simple);
               J.Set_Field
                 ("dump-filename-env-var", Dump_Cfg.Filename_Env_Var);
               J.Set_Field
                 ("dump-filename-prefix", Dump_Cfg.Filename_Prefix);
            when Base64_Standard_Output =>
               null;
         end case;
      end;

      --  Write the setup config file

      Write (Config_Filename, J, Compact => False);
   end Save_Setup_Config;

   -----------------------
   -- Check_RTS_Profile --
   -----------------------

   function Check_RTS_Profile
     (Profile     : Resolved_RTS_Profile;
      Dump_Config : Any_Dump_Config) return Boolean
   is
      Had_Warnings : Boolean := False;

      procedure Warn (Cond : Boolean; What : String);
      --  If Cond is True, emit a warning saying that "What" may not work with
      --  the selected runtime and set Had_Warnings to True.

      ----------
      -- Warn --
      ----------

      procedure Warn (Cond : Boolean; What : String) is
      begin
         if Cond then
            Had_Warnings := True;
            Outputs.Warn
              (What & " may not be compatible with the selected runtime");
         end if;
      end Warn;

   begin
      --  If the dump is manual, there is nothing we can check here, as the
      --  other parameters may not correspond to the way the manual dump is
      --  done in user code.

      if Dump_Config.Trigger = Manual then
         return False;
      end if;

      case Profile is
         when Full =>
            Warn (Dump_Config.Trigger = Ravenscar_Task_Termination,
                  "--dump-trigger=ravenscar-task-termination");

         when Embedded =>
            Warn (Dump_Config.Trigger = At_Exit, "--dump-trigger=atexit");
            Warn (Dump_Config.Channel = Binary_File,
                  "--dump-channel=bin-file");
      end case;

      return Had_Warnings;
   end Check_RTS_Profile;

   --------------------
   -- Has_Shared_Lib --
   --------------------

   function Has_Shared_Lib
     (RTS_Dir : String; Shared_Lib_Ext : String) return Boolean
   is
      Lib_Dir_Search : Search_Type;
   begin
      if not Exists (RTS_Dir) then
         return False;
      end if;

      Start_Search
        (Lib_Dir_Search,
         Directory =>
         RTS_Dir & GNAT.OS_Lib.Directory_Separator & "adalib",
         Pattern   => "*" & Shared_Lib_Ext,
         Filter    =>
           (Ordinary_File => True, others => False));

      return Res : constant Boolean := More_Entries (Lib_Dir_Search) do
         End_Search (Lib_Dir_Search);
      end return;

   exception
      when Ada.Directories.Name_Error =>
         End_Search (Lib_Dir_Search);
         return False;
   end Has_Shared_Lib;

end Setup_RTS;
