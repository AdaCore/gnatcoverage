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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;         use Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Strings;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Outputs;      use Outputs;
with Subprocesses; use Subprocesses;
with Switches;     use Switches;
with Temp_Dirs;    use Temp_Dirs;
with Text_Files;

package body Setup_RTS is

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

   function Rename_Project
     (Temp_Dir     : Temporary_Directory;
      Project_Name : String) return String;
   --  Create a renamed copy of the "gnatcov_rts.gpr" project file in Temp_Dir.
   --  Project_Name is the new name for this project. Return the absolute path
   --  to the renamed project file.

   procedure Load_Project_Parameters
     (Project_File : String;
      Target       : String;
      RTS          : String;
      Config_File  : String;
      Actual_RTS   : out Unbounded_String;
      Lib_Support  : out Library_Support);
   --  Load the project file at Project_File using the Target/RTS/Config_File
   --  parameters, then determine the actual runtime in effect (Actual_RTS) and
   --  the support for libraries for this configuration (Lib_Support).

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

   --------------------
   -- Rename_Project --
   --------------------

   function Rename_Project
     (Temp_Dir     : Temporary_Directory;
      Project_Name : String) return String
   is
      use GNAT.Strings;

      Dest_File : constant String :=
        Temp_Dir.Directory_Name & "/" & To_Lower (Project_Name) & ".gpr";
      --  Name of the project file to create

      Dest : Text_Files.File_Type;
      --  Descriptor for the project file to write

      Src_Name : constant String := "GNATcov_RTS";
      --  Original project name

      Src_File    : constant String :=
        Temp_Dir.Directory_Name & "/gnatcov_rts.gpr";
      Src_Content : GNAT.Strings.String_Access;
      Src_Last    : Natural;
      --  Original project file name and content

      Index      : Natural;
      Next_Index : Natural;
   begin
      --  Read the original file, open the file to write, and start processing
      --  its first byte.

      Src_Content := Create (+Src_File).Read_File;
      Src_Last := Src_Content.all'Last;
      Index := Src_Content.all'First;
      Dest.Create (Dest_File);

      --  Replace all occurences of Src_Name in Src_Content with Project_Name,
      --  storing the result in Dest_Content. Index tracks the index of the
      --  first character in Src_Content to process next.

      while Index <= Src_Last loop

         --  Look for the next occurence to replace

         Next_Index := Ada.Strings.Fixed.Index
           (Source  => Src_Content.all (Index .. Src_Last),
            Pattern => Src_Name);

         if Next_Index = 0 then

            --  None was found: just forward the rest of Src_Content to
            --  Dest_Content and stop.

            Dest.Put (Src_Content.all (Index .. Src_Last));
            exit;

         else
            --  Forward the part of Src_Content that goes from the current
            --  position to the occurence of Src_Name we found, then append
            --  Project_Name and continue just past the occurence of Src_Name.

            Dest.Put (Src_Content.all (Index .. Next_Index - 1));
            Dest.Put (Project_Name);
            Index := Next_Index + Src_Name'Length;
         end if;
      end loop;

      Free (Src_Content);
      Dest.Close;

      return Dest_File;
   end Rename_Project;

   -----------------------------
   -- Load_Project_Parameters --
   -----------------------------

   procedure Load_Project_Parameters
     (Project_File : String;
      Target       : String;
      RTS          : String;
      Config_File  : String;
      Actual_RTS   : out Unbounded_String;
      Lib_Support  : out Library_Support)
   is
      Env : Project_Environment_Access;
      Prj : Project_Tree;
   begin
      --  Initialize the environment: config file, target and runtime
      --  information as well as the scenario variables that determine the set
      --  of languages, as we may not be able to load the full Ada+C project if
      --  there is no Ada toolchain available.

      Initialize (Env);

      if Config_File = "" then
         Env.Set_Automatic_Config_File;
      else
         Env.Set_Config_File (Create (+Config_File));
      end if;

      if Target /= "" or else RTS /= "" then
         Env.Set_Target_And_Runtime (Target, RTS);
      end if;

      if Enable_Languages.Contains (To_Unbounded_String ("Ada")) then
         Env.Change_Environment ("GNATCOV_RTS_WITH_ADA", "true");
      end if;

      --  Now load the project, and get its actual RTS

      begin
         Prj.Load
           (Root_Project_Path => Create (+Project_File),
            Env               => Env);
      exception
         when Invalid_Project =>
            Fatal_Error ("Could not load the runtime project file");
      end;

      Actual_RTS := To_Unbounded_String (Prj.Root_Project.Get_Runtime);

      if Verbose then
         Put_Line ("Actual target: " & Prj.Root_Project.Get_Target);
         Put_Line ("Actual RTS: " & To_String (Actual_RTS));
      end if;

      --  Query the support for libraries in this configuration. When GPRconfig
      --  fails to find the toolchain for the requested target/RTS, it only
      --  emits warnings (no Invalid_Project exception raised in the call to
      --  Load above). In this case, we reach this point and have an empty
      --  string for the Library_Support attribute.

      declare
         Attr : constant Attribute_Pkg_String := Build ("", "Library_Support");
         LS   : constant String := Prj.Root_Project.Attribute_Value (Attr);
      begin
         Lib_Support := Library_Support'Value (LS);
      exception
         when Constraint_Error =>
            Fatal_Error ("Cannot get library support for this configuration");
      end;

      if Verbose then
         Put_Line ("Library support: " & Lib_Support'Image);
      end if;

      --  We are done: clean up resources

      Prj.Unload;
      Free (Env);
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

      --  Now run gprbuild and gprinstall

      Run_Command
        (Command             => "gprbuild",
         Arguments           => Actual_Common_Args & Build_Args,
         Origin_Command_Name => "gprbuild");
      Run_Command
        (Command             => "gprinstall",
         Arguments           => Actual_Common_Args & Actual_Install_Args,
         Origin_Command_Name => "gprinstall");
   end Build_And_Install;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Target             : String;
      RTS                : String;
      Config_File        : String;
      Prefix             : String;
      RTS_Profile        : Any_RTS_Profile;
      Runtime_Project    : String;
      Runtime_Source_Dir : String;
      Gargs              : String_Vectors.Vector)
   is
      Temp_Dir : Temporary_Directory;
      Success  : Boolean;

      Orig_Project_File : constant String :=
        Runtime_Source_Dir & "/gnatcov_rts.gpr";

      Actual_RTS         : Unbounded_String;
      Actual_RTS_Profile : Resolved_RTS_Profile;
      Lib_Support        : Library_Support;
   begin
      if not Exists (Orig_Project_File) then
         Fatal_Error ("No instrumentation runtime project file at "
                      & Orig_Project_File);
      end if;

      --  Create the temporary directory to host the build of the
      --  instrumentation runtime and copy sources for the instrumentation
      --  runtime project there.

      Create_Temporary_Directory
        (Temp_Dir, "gnatcov_rts", Auto_Delete => not Save_Temps);
      Create (+Runtime_Source_Dir).Copy (+Temp_Dir.Directory_Name, Success);
      if not Success then
         Fatal_Error ("Could not copy the instrumentation runtime sources");
      end if;

      declare
         Project_File : constant String :=
           Rename_Project (Temp_Dir, Runtime_Project);
      begin
         --  Load the instrumentation runtime project to know about the actual
         --  runtime and the library support for this configuration.

         Load_Project_Parameters
           (Project_File, Target, RTS, Config_File, Actual_RTS, Lib_Support);

         --  If a specific RTS profile was requested, use it. Otherwise, infer
         --  it from the actual RTS. For now there are only two cases:
         --
         --  * The name of the runtime is empty, we are likely using the only
         --    runtime of a native toolchain:  we have a full runtime.
         --
         --  * The name of the runtime is not empty: we are likely using a
         --    runtime for an embedded target: we have an embedded runtime.

         if RTS_Profile = Auto then
            Actual_RTS_Profile :=
              (if Actual_RTS = Null_Unbounded_String
               then Full
               else Embedded);
         else
            Actual_RTS_Profile := RTS_Profile;
         end if;

         if Verbose then
            Put_Line ("Actual RTS profile: " & Actual_RTS_Profile'Image);
         end if;

         --  Try to uninstall a previous installation of the instrumentation
         --  runtime in the requested prefix. This is to avoid installation
         --  update issues: for instance, the "Language" project attribute in
         --  the already installed project could be set to ("Ada", "C") whereas
         --  we are here installing a runtime only for "C". Uninstalling the
         --  project first allows gprinstall to start from a sanitized
         --  environment, and thus the installed project to accurately describe
         --  what is installed.

         Uninstall (Runtime_Project, Prefix);

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

            Build_Args.Append_Vector (Gargs);

            --  Tune external variables according to enabled languages and the
            --  RTS profile.

            declare
               With_Ada : constant Boolean :=
                 Enable_Languages.Contains (To_Unbounded_String ("Ada"));
            begin
               Common_Args.Append
                 (+("-XGNATCOV_RTS_WITH_ADA=" & To_Lower (With_Ada'Image)));
               Common_Args.Append
                 (+("-XGNATCOV_RTS_RTS_PROFILE="
                    & To_Lower (Actual_RTS_Profile'Image)));
            end;

            case Lib_Support is
               when None =>
                  Fatal_Error
                    ("This target/runtime configuration does not support"
                     & " library projects: cannot build the instrumentation"
                     & " runtime");

               when Static_Only =>
                  Build_And_Install
                    (Project_File,
                     Temp_Dir,
                     Common_Args,
                     Build_Args,
                     Install_Args);

               when Full =>
                  for LK of Library_Kinds loop
                     Build_And_Install
                       (Project_File,
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

end Setup_RTS;
