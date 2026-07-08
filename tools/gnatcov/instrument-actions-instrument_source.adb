------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2026, AdaCore                       --
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

with GNATCOLL.Traces;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with GPR2.Build.Artifacts;
with GPR2.Build.Artifacts.Key_Value;
with GPR2.Build.Makefile_Parser;
with GPR2.Build.Source;
with GPR2.Project;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;

with Command_Line; use Command_Line;
with Files_Table;  use Files_Table;
with Project;      use Project;
with Support_Files;
with Switches_GPR; use Switches_GPR;
with Text_Files;
with Version;

package body Instrument.Actions.Instrument_Source is

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   function Dep_File (Self : Object) return GPR2.Path_Name.Object
   is (GPR2.Path_Name.Create
         (Instrument.Common.Dependency_File
            (Self.Prj_Info.Desc,
             Self.LU_Info.Main_Part_Src.Path_Name.Virtual_File)));
   --  Return the path for the dependency file for this unit instrumentation
   --  action.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self         : in out Object;
      LU_Info      : Library_Unit_Info;
      IC           : Inst_Context_Acc;
      Instrumenter : Instrument.Common.Language_Instrumenter_Acc;
      Prj_Info     : Project_Info_Access;
      Dump_Config  : Any_Dump_Config) is
   begin
      Self.LU_Info := LU_Info;
      Self.IC := IC;
      Self.Instrumenter := Instrumenter;
      Self.Prj_Info := Prj_Info;
      Self.Dump_Config := Dump_Config;
      Self.Artifacts := Artifact_Sets.Empty_Set;

      --  Make the dump config explicit and hardcode the filename prefix here
      --  to pass it to the spawned subcommand.

      if LU_Info.Is_Main and then Dump_Config.Channel = Binary_File then

         --  If no dump filename prefix was specified, compute it
         --  here: we use the executable name, that is retrieved
         --  from the project.

         if Dump_Config.Filename_Prefix = "" then
            Self.Dump_Config.Filename_Prefix :=
              Strings."+"
                (String
                   (Self.LU_Info.Instr_Project.Executable
                      (Source =>
                         Self.LU_Info.Main_Part_Src.Path_Name.Simple_Name,
                       At_Pos => 0)
                      .Simple_Name));
         end if;
      end if;

      --  Then add the output artifacts

      if Self.LU_Info.Is_UOI then

         --  Add the SID file

         Self.Artifacts.Include
           (GPR2.Build.Artifacts.Files.Create (Self.SID_Path));
      end if;

      --  Add other instrumentation artifacts

      for VF of
        Instrumentation_Artifacts
          (Self.LU_Info.Main_Part_Src, Self.Prj_Info.Desc)
      loop
         Self.Artifacts.Include
           (GPR2.Build.Artifacts.Files.Create (GPR2.Path_Name.Create (VF)));
      end loop;
   end Initialize;

   ------------------------------
   -- Common_Compute_Signature --
   ------------------------------

   procedure Common_Compute_Signature
     (Self            : in out Object;
      Signature       : in out GPR2.Build.Signature.Object;
      Check_Checksums : Boolean)
   is
      Deps : constant GPR2.Containers.Filename_Set :=
        Object'Class (Self).Dependencies;
   begin
      for Dep of Deps loop
         declare
            Path : constant GPR2.Path_Name.Object :=
              GPR2.Path_Name.Create_File (Dep);
         begin
            if not Path.Exists then
               Instrument.Sources_Trace.Trace
                 ("Compute_Signature: cannot find dependency " & String (Dep));

               if Check_Checksums then
                  Signature.Clear;
                  return;
               end if;

            elsif not Signature.Add_Input
                        (GPR2.Build.Artifacts.Files.Create (Path),
                         Check_Checksums)
            then
               Signature.Clear;
               return;
            end if;
         end;
      end loop;

      --  If we reach this, then we have access to dependencies, so write
      --  the instrumented files.

      Object'Class (Self).Write_Instrumented_Files_List;

      for Art of Self.Artifacts loop
         if not Signature.Add_Output (Art, Check_Checksums) then
            Signature.Clear;
            return;
         end if;
      end loop;

      --  Compute signatures for external annotation files

      for Arg of Switches.Args.String_List_Args (Opt_Ext_Annotations) loop
         if not Signature.Add_Output
                  (GPR2.Build.Artifacts.Files.Create (Filename_Type (+Arg)),
                   Check_Checksums)
         then
            Signature.Clear;
            return;
         end if;
      end loop;

      --  Add the gnatcov version as a signature input artifact

      if not Signature.Add_Input
               (GPR2.Build.Artifacts.Key_Value.Create
                  (Key   => Value_Type'("gnatcov_version"),
                   Value => Value_Type'(Version.Xcov_Version)),
                Check_Checksums)
      then
         Signature.Clear;
         return;
      end if;
   end Common_Compute_Signature;

   -----------------------
   -- Compute_Signature --
   -----------------------

   procedure Compute_Signature
     (Self            : in out Object;
      Signature       : in out GPR2.Build.Signature.Object;
      Check_Checksums : Boolean)
   is
      use GPR2.Build.Signature;
      Deps : constant GPR2.Containers.Filename_Set :=
        Object'Class (Self).Dependencies;
   begin
      if Deps.Is_Empty then

         --  Dependency file parsing went wrong, at least put the direct
         --  source as an input.

         if not Signature.Add_Input
                  (GPR2.Build.Artifacts.Files.Create
                     (Self.LU_Info.Main_Part_Src.Path_Name))
         then
            Signature.Clear;
            return;
         end if;

         --  This actions should be done no matter what since the
         --  dependencies states are unknown.

         Signature.Clear;
         return;
      end if;

      Self.Common_Compute_Signature (Signature, Check_Checksums);
   end Compute_Signature;

   ---------------------
   -- Compute_Command --
   ---------------------

   function Compute_Command
     (Self : in out Object) return GPR2.Build.Command_Line.Object
   is
      Cmd_Line : GPR2.Build.Command_Line.Object;
   begin
      Cmd_Line.Set_Driver (Support_Files.Gnatcov64);
      Cmd_Line.Add_Argument ("instrument-source");

      for Arg of Common_Switches (Cmd_Instrument_Source) loop
         Cmd_Line.Add_Argument (+Arg.Arg, Arg.Mode);
      end loop;

      --  Add the arguments that are specific to the compilation unit

      for Arg of
        Compilation_Unit_Options
          (IC   => Self.IC.all,
           Prj  =>
             Self.IC.Project_Info_Map.Element
               (+Self.LU_Info.Instr_Project.Name)
               .Desc,
           Lang => Self.LU_Info.Language,
           Src  => Self.LU_Info.Main_Part_Src)
      loop
         Cmd_Line.Add_Argument (+Arg.Arg, Arg.Mode);
      end loop;

      Cmd_Line.Add_Argument ("--sid");
      Cmd_Line.Add_Argument (String (SID_Path (Self).Name));

      if Self.LU_Info.Is_UOI then
         Cmd_Line.Add_Argument ("--uoi");
      end if;

      if Self.LU_Info.Is_Main then
         Cmd_Line.Add_Argument ("--main");
      end if;

      --  Add the arguments for dump trigger instrumentation purposes

      if Self.LU_Info.Is_Main or else Self.Dump_Config.Manual_Trigger then
         for Arg of Unparse_Config (Self.Dump_Config) loop
            Cmd_Line.Add_Argument (+Arg.Arg, Arg.Mode);
         end loop;
      end if;

      Cmd_Line.Add_Argument (Self.Unit_Name);
      return Cmd_Line;
   end Compute_Command;

   -----------------------------------
   -- Write_Instrumented_Files_List --
   -----------------------------------

   procedure Write_Instrumented_Files_List (Self : in out Object) is
      use Text_Files;
      F : File_Type;
   begin
      F.Create
        (Name =>
           Instrumented_Files_File
             (Self.Prj_Info.Desc,
              Self.LU_Info.Main_Part_Src.Path_Name.Virtual_File)
             .Display_Full_Name);
      for Dep of Self.Dependencies loop
         if Self.IC.Files_Of_Interest.Contains
              (GNATCOLL.VFS.Create (+String (Dep)))
         then
            F.Put_Line (String (Dep));
         end if;
      end loop;
      Close (F);
   end Write_Instrumented_Files_List;

   --------------------
   -- Post_Execution --
   --------------------

   function Post_Execution
     (Self   : in out Object;
      Status : GPR2.Build.Actions.Execution_Status;
      Stdout : US.Unbounded_String := US.Null_Unbounded_String;
      Stderr : US.Unbounded_String := US.Null_Unbounded_String) return Boolean
   is
   begin
      --  Add the instrumentation artifacts

      for VF of
        Instrumentation_Artifacts
          (Self.LU_Info.Main_Part_Src, Self.Prj_Info.Desc)
      loop
         Self.Artifacts.Include
           (GPR2.Build.Artifacts.Files.Create (GPR2.Path_Name.Create (VF)));
      end loop;

      --  Write the list of files instrumented in the context of this source
      --  instrumentation.

      Object'Class (Self).Write_Instrumented_Files_List;
      return True;
   end Post_Execution;

   ---------
   -- UID --
   ---------

   function UID (Self : Object) return GPR2.Build.Actions.Action_Id'Class is
      Result : constant Instrument.Actions.Instrument_Source.Instrument_Id :=
        (Name_Len => Self.LU_Info.Main_Part_Src.Path_Name.Simple_Name'Length,
         Lang     => To_Language_Id (Self.LU_Info.Language),
         Ctxt     => Self.LU_Info.Instr_Project,
         Src_Name => Self.LU_Info.Main_Part_Src.Path_Name.Simple_Name);
   begin
      return Result;
   end UID;

   ------------------
   -- Dependencies --
   ------------------

   function Dependencies (Self : in out Object) return Containers.Filename_Set
   is
      BN     : constant Simple_Name :=
        Self.LU_Info.Main_Part_Src.Path_Name.Base_Filename;
      O_Suff : constant Simple_Name :=
        Simple_Name
          (Self.LU_Info.Instr_Project.Attribute
             (PRA.Compiler.Object_File_Suffix,
              PAI.Create (Self.LU_Info.Main_Part_Src.Language))
             .Value
             .Text);
      --  Object file path
   begin
      if not Self.Dep_File.Is_Defined then
         return Containers.Empty_Filename_Set;
      end if;

      if not Self.Deps_Cache.Is_Empty then
         return Self.Deps_Cache;
      end if;

      if not GPR2.Build.Makefile_Parser.Dependencies
               (Self.Dep_File,
                Self.LU_Info.Instr_Project.Object_Directory.Compose
                  (BN & O_Suff),
                Self.Deps_Cache)
      then
         Instrument.Sources_Trace.Trace
           ("Failed to parse dependencies from the dependency "
            & "file "
            & Self.Dep_File.String_Value);

         return Containers.Empty_Filename_Set;
      end if;

      return Self.Deps_Cache;
   end Dependencies;

   ---------------
   -- Unit_Name --
   ---------------

   function Unit_Name (Self : Object) return String is
      Src : constant GPR2.Build.Source.Object := Self.LU_Info.Main_Part_Src;
   begin
      if Src.Has_Units then
         return To_Lower (String (First_Unit (Src).Name));
      else
         return Src.Path_Name.Virtual_File.Display_Full_Name;
      end if;
   end Unit_Name;

end Instrument.Actions.Instrument_Source;
