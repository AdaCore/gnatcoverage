------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with GNAT.Exception_Actions;

with GNAT.OS_Lib;
with GNAT.Regexp;
with GNAT.Strings;

with GNATCOLL.JSON;         use GNATCOLL.JSON;
with GNATCOLL.VFS;
with GNATCOLL.Projects.Aux; use GNATCOLL.Projects.Aux;

with Binary_Files;
with Command_Line;        use Command_Line;
with Files_Table;
with Instrument.Ada_Unit; use Instrument.Ada_Unit;
with Instrument.C;        use Instrument.C;
with Instrument.Clean_Objdirs;
with Instrument.Common;   use Instrument.Common;
with Instrument.Main;
with Instrument.Source;
with JSON;                use JSON;
with Outputs;
with Paths;               use Paths;
with Project;             use Project;
with Strings;             use Strings;
with Support_Files;
with Text_Files;          use Text_Files;

--  Generate instrumented sources for the source files of all units of
--  interest. Also save mappings between coverage buffers and SCOs for each
--  library units to SID files (one per library unit).
--
--  Depending on Dump_Config, instrument mains to schedule a call to the
--  dump procedure for the list of coverage buffers in all mains in the
--  project.
--
--  Language_Version restricts what source constructs the instrumenter is
--  allowed to use. For instance, if Ada_2005 (or a lower version) is
--  passed, it will not be allowed to introduce expression functions, and
--  thus will emit a warning when it needed to do so.
--
--  If Ignored_Source_File is non-null, ignore files whose names match the
--  accessed pattern.
--
--  Mains is the list of source files that were listed on the command line:
--  if non-empty, they replace the mains specified in project files.

procedure Instrument.Projects
  (Dump_Config          : Any_Dump_Config;
   Ignored_Source_Files : access GNAT.Regexp.Regexp;
   Mains                : String_Vectors.Vector)
is
   type Project_Info is record
      Project : Project_Type;
      --  Project that this record describes

      Externally_Built : Boolean;
      --  Whether this project is externally built. In that case, we assume its
      --  units of interest have already been instrumented.

      Output_Dir : Ada.Strings.Unbounded.Unbounded_String;
      --  Subdirectory in the project file's object directory. All we generate
      --  for this project must land in it.

      Desc : Prj_Desc;

   end record;

   type Project_Info_Access is access all Project_Info;

   package Project_Info_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Project_Info_Access,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      Hash            => Ada.Strings.Unbounded.Hash);
   --  Mapping from project name (as returned by GNATCOLL.Projects.Name) to
   --  Project_Info records. Project_Info records are owned by this map, and
   --  thus must be deallocated when maps are deallocated.

   type Main_To_Instrument is record
      CU_Name : Compilation_Unit_Part;
      --  Compilation unit of the main to instrument

      File : GNATCOLL.VFS.Virtual_File;
      --  Source file to instrument

      Prj_Info : Project_Info_Access;
      --  Reference to the Project_Info record corresponding to the project
      --  that owns the main to instrument.
   end record;

   package Main_To_Instrument_Vectors is new Ada.Containers.Vectors
     (Positive, Main_To_Instrument);

   use type GNATCOLL.VFS.Filesystem_String;
   function Less (L, R : File_Info) return Boolean is
     (L.File.Full_Name < R.File.Full_Name);
   function Equal (L, R : File_Info) return Boolean is
     (L.File.Full_Name = R.File.Full_Name);

   package File_Info_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => File_Info, "<" => Less, "=" => Equal);

   package Prj_Has_Manual_Helper_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Element_Type => Unbounded_String);

   type Library_Unit_Info is record
      Unit_Name : Unbounded_String;
      --  Name of this unit: unit name for unit-based languages, simple name
      --  for file-based languages.

      Instr_Project : GPR.Project_Type;
      --  Project in which instrumentation artifacts for this unit are
      --  generated.

      Language_Kind : Any_Language_Kind;
      --  Higher level representation of a language (unit-based or file-based)

      Language : Any_Language;
      --  Actual language representation

      All_Externally_Built : Boolean;
      --  Whether all of the parts of this unit belongs to an externally-built
      --  project. If it is the case, the unit won't be instrumented;
      --  otherwise, every unit part will.

   end record;

   package Unit_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Library_Unit_Info);
   --  Map to unit names to unit info of files implementing this unit. For
   --  file- based languages, the unit name is the full name (to simplify
   --  dealing with homonym in different projects).

   type Inst_Context is limited record
      Mapping_File, Config_Pragmas_File : Unbounded_String;

      Predefined_Source_Dirs : String_Vectors.Vector;

      Sources_Of_Interest_Response_File : Unbounded_String;
      --  File containing the list of units of interest, identified by their
      --  fullname. This is passed on to gnatcov instrument-source invokations
      --  (for Ada), to know which part of a unit (spec / body / separate) must
      --  be instrumented.

      Ignored_Source_Files_Present : Boolean;
      Ignored_Source_Files         : GNAT.Regexp.Regexp;
      --  If present, instrumentation will ignore files whose names match the
      --  accessed pattern.

      Project_Info_Map : Project_Info_Maps.Map;
      --  For each project that contains units of interest, this tracks a
      --  Project_Info record.

      Tag : Unbounded_String;
      --  Tag relative to the current instrumentation run

   end record;

   function Create_Context
     (Ignored_Source_Files : access GNAT.Regexp.Regexp) return Inst_Context;
   --  Create an instrumentation context for the currently loaded project

   procedure Destroy_Context (Context : in out Inst_Context);
   --  Free dynamically allocated resources in Context

   function Is_Ignored_Source_File
     (Context : Inst_Context; Filename : String) return Boolean;
   --  Return whether the instrumentation process must ignore the Filename
   --  source file.

   function Get_Or_Create_Project_Info
     (Context : in out Inst_Context;
      Project : Project_Type) return Project_Info_Access;
   --  Return the Project_Info record corresponding to Project. Create it if it
   --  does not exist.

   procedure Register_Main_To_Instrument
     (Context : in out Inst_Context;
      Mains   : in out Main_To_Instrument_Vectors.Vector;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type);
   --  Register in Mains a main to be instrumented so that it dumps coverage
   --  buffers. File is the source file for this main, and Project is the
   --  project that owns this main.

   procedure Prepare_Output_Dirs (IC : Inst_Context);
   --  Make sure we have the expected tree of directories for the
   --  instrumentation output.

   function SID_Filename
     (LU_Info : Library_Unit_Info; In_Library_Dir : Boolean) return String;
   --  Return the filename of the SID file to create for the given library
   --  unit. If In_Library_Dir is true, then return a filename located in the
   --  project library directory. Otherwise, the filename is located in the
   --  object directory.

   function Load_From_Project (Prj : Project_Type) return Prj_Desc;
   --  Load the project description from the given project

   function Compilation_Unit_Options
     (IC      : Inst_Context;
      Prj     : Prj_Desc;
      LU_Info : Library_Unit_Info) return String_Vectors.Vector;
   --  Return the list of options to pass to a gnatcov instrument-source /
   --  instrument-main (depending on Purpose) for the given compilation unit
   --  LU_Info, belonging to the project Prj.

   procedure Add_Project_Source
     (Project : GPR.Project_Type; Source_File : GPR.File_Info);
   --  Add Source_File to the C or Ada list of all project sources depending on
   --  its language.

   procedure Insert_Manual_Dump_Trigger
     (Language             : Src_Supported_Language;
      Project_Sources      : in out File_Info_Sets.Set;
      Instrumenter         : in out Language_Instrumenter'Class;
      Manual_Dump_Inserted : in out Boolean);
   --  For all sources in Project_Sources of language Language, call
   --  Replace_Manual_Dump_Indication. If a dump procedure call was inserted
   --  and id there is not one already, also emit a dump helper unit for the
   --  project the source belongs to. Set Manual_Dump_Inserted to True if at
   --  least one manual dump indication was found.

   procedure Add_Instrumented_Unit
     (Project : GPR.Project_Type; Source_File : GPR.File_Info);
   --  Add this source file to the list of units to instrument

   procedure Clean_And_Print (Exc : Ada.Exceptions.Exception_Occurrence);
   --  Clean the instrumentation directories and print any relevant information
   --  regarding the instrumentation context.

   -----------------------
   -- Load_From_Project --
   -----------------------

   function Load_From_Project (Prj : Project_Type) return Prj_Desc
   is
      Dot_Replacement : constant String :=
        Prj.Attribute_Value
          (Attribute => Build ("Naming", "Dot_Replacement"));
      Result          : Prj_Desc;
   begin
      for Lang in Some_Language loop
         if Builtin_Support (Lang) then
            declare
               Body_Suffix  : constant String :=
                 Project.Source_Suffix (Lang, GPR.Unit_Body, Prj);
               Spec_Suffix  : constant String :=
                 Project.Source_Suffix (Lang, GPR.Unit_Spec, Prj);
            begin
               Result.Body_Suffix (Lang) := +Body_Suffix;
               Result.Spec_Suffix (Lang) := +Spec_Suffix;
            end;
         end if;
      end loop;

      Result.Prj_Name := +Prj.Name;
      Result.Dot_Replacement := +Dot_Replacement;

      --  Register the source directories of the project tree

      declare
         procedure Register_Source_Dirs
           (P : GNATCOLL.Projects.Project_Type);
         --  Add the source directories of P's project file to the search
         --  paths to be passed as -I arguments later. The order in which
         --  the paths are added to the search paths vector is the same
         --  order in which GNATCOLL retrieves the files in the project
         --  tree. gprbuild also depends on GNATCOLL which ensures we
         --  have the same behavior here.

         --------------------------
         -- Register_Source_Dirs --
         --------------------------

         procedure Register_Source_Dirs
           (P : GNATCOLL.Projects.Project_Type) is
         begin
            for Dir of P.Source_Dirs loop
               Result.Search_Paths.Append
                 ("-I" & (+GNATCOLL.VFS."+" (GNATCOLL.VFS.Dir_Name (Dir))));
            end loop;
         end Register_Source_Dirs;
      begin
         --  Pass the source directories of the project file as -I options.
         --  Note that this will duplicate with the project tree traversal
         --  below, but we need this project source directories to be
         --  picked first. We thus make sure to add them first to the
         --  PP_Search_Path list.

         Register_Source_Dirs (Prj);

         --  Pass the source directories of included projects as -I options

         Project.Iterate_Projects
           (Prj,
            Register_Source_Dirs'Access,
            Recursive                => True,
            Include_Extended         => True,
            Include_Externally_Built => True);
      end;

      --  Load the set of compiler switches for languages requiring it

      for Lang in C_Family_Language loop
         declare
            use type GNAT.Strings.String_List_Access;
            Options         : Analysis_Options;
            Compiler_Opts   : String_Vectors.Vector;
            Switches        : GNAT.Strings.String_List_Access;
            Compiler_Driver : constant String :=
              GNATCOLL.Projects.Attribute_Value
                (Prj,
                 GPR.Compiler_Driver_Attribute,
                 Image (Lang));

         begin
            --  Get the compiler switches from the project file. When
            --  registering a compilation unit for instrumentation, we also
            --  fill the compilation unit specific switches that will override
            --  the project defaults, if there are any (see
            --  Add_Instrumented_Unit).

            Switches :=
              Prj.Attribute_Value
                (Attribute => GPR.Compiler_Default_Switches_Attribute,
                 Index     => Image (Lang));

            --  If we manage to find appropriate switches, convert them to a
            --  string vector import the switches.

            if Switches /= null then
               declare
                  Args : String_Vectors.Vector;
               begin
                  for S of Switches.all loop
                     Args.Append (To_Unbounded_String (S.all));
                  end loop;
                  GNAT.Strings.Free (Switches);
                  Import_From_Args (Options, Args);
               end;
            end if;

            Add_Options (Compiler_Opts, Options, Pass_Builtins => False);

            Result.Compiler_Options (Lang) := Compiler_Opts;
            Result.Compiler_Driver (Lang) := +Compiler_Driver;
         end;
      end loop;

      return Result;
   end Load_From_Project;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Ignored_Source_Files : access GNAT.Regexp.Regexp) return Inst_Context is
   begin
      return IC : Inst_Context do
         IC.Ignored_Source_Files_Present := Ignored_Source_Files /= null;
         if Ignored_Source_Files /= null then
            IC.Ignored_Source_Files := Ignored_Source_Files.all;
         end if;
      end return;
   end Create_Context;

   ---------------------
   -- Destroy_Context --
   ---------------------

   procedure Destroy_Context (Context : in out Inst_Context) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Project_Info, Project_Info_Access);
   begin
      --  Deallocate all Project_Info in Context, and then clear the hashed
      --  map, both to avoid dangling pointers and to make Destroy_Context
      --  callable more than once, like conventional deallocation procedures
      --  in Ada.

      for Cur in Context.Project_Info_Map.Iterate loop
         declare
            PI : Project_Info_Access := Project_Info_Maps.Element (Cur);
         begin
            Free (PI);
         end;
      end loop;
      Context.Project_Info_Map := Project_Info_Maps.Empty_Map;
   end Destroy_Context;

   ----------------------------
   -- Is_Ignored_Source_File --
   ----------------------------

   function Is_Ignored_Source_File
     (Context : Inst_Context; Filename : String) return Boolean
   is
   begin
      return
         Context.Ignored_Source_Files_Present
         and then GNAT.Regexp.Match
                    (S => Fold_Filename_Casing (Filename),
                     R => Context.Ignored_Source_Files);
   end Is_Ignored_Source_File;

   --------------------------------
   -- Get_Or_Create_Project_Info --
   --------------------------------

   function Get_Or_Create_Project_Info
     (Context : in out Inst_Context;
      Project : Project_Type) return Project_Info_Access
   is
      use Project_Info_Maps;

      --  Look for an existing Project_Info record corresponding to Project

      Project_Name : constant Unbounded_String := +Project.Name;
      Position     : constant Cursor := Context.Project_Info_Map.Find
        (Project_Name);
   begin
      if Has_Element (Position) then
         return Element (Position);

      else
         --  The requested Project_Info record does not exist yet. Create it,
         --  register it and return it.

         declare
            Storage_Project : constant Project_Type :=
              Project.Extending_Project (Recurse => True);
            --  Actual project that will host instrumented sources: even when
            --  we instrument an extended project, the resulting instrumented
            --  sources must go to the ultimate extending project's object
            --  directory. This is similar to the object directory that hosts
            --  object files when GPRbuild processes a project that is
            --  extended.

            Result : constant Project_Info_Access := new Project_Info'
              (Project          => Project,
               Externally_Built => Project.Externally_Built,
               Output_Dir       => +Project_Output_Dir (Storage_Project),
               Desc             => Load_From_Project (Project));
         begin
            Result.Desc.Output_Dir := Result.Output_Dir;
            Context.Project_Info_Map.Insert (Project_Name, Result);
            return Result;
         end;
      end if;
   end Get_Or_Create_Project_Info;

   -------------------------
   -- Prepare_Output_Dirs --
   -------------------------

   procedure Prepare_Output_Dirs (IC : Inst_Context) is
      use Project_Info_Maps;
   begin
      for Cur in IC.Project_Info_Map.Iterate loop
         declare
            Prj_Info   : Project_Info renames Element (Cur).all;
            Output_Dir : constant String := +(Element (Cur).Output_Dir);
         begin
            --  Do not create output directories for externally built projects:
            --  we don't instrument them and we may not have filesystem
            --  permissions to create directories there.

            if not Prj_Info.Externally_Built
               and then not Ada.Directories.Exists (Output_Dir)
            then
               Ada.Directories.Create_Path (Output_Dir);
            end if;
         end;
      end loop;
   end Prepare_Output_Dirs;

   ---------------------------------
   -- Register_Main_To_Instrument --
   ---------------------------------

   procedure Register_Main_To_Instrument
     (Context : in out Inst_Context;
      Mains   : in out Main_To_Instrument_Vectors.Vector;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type)
   is
      File_Info : constant GNATCOLL.Projects.File_Info :=
         Standard.Project.Project.Info (File);
      CU_Name   : constant Compilation_Unit_Part :=
         To_Compilation_Unit_Name (File_Info);
      Prj_Info  : constant Project_Info_Access :=
        Get_Or_Create_Project_Info (Context, Project);
   begin
      Mains.Append
        (Main_To_Instrument'
           (CU_Name  => CU_Name,
            File     => File,
            Prj_Info => Prj_Info));
   end Register_Main_To_Instrument;

   ------------------
   -- SID_Filename --
   ------------------

   function SID_Filename
     (LU_Info        : Library_Unit_Info;
      In_Library_Dir : Boolean) return String
   is
      use GNATCOLL.VFS;

      --  Determine in which project we will put this SID file, and the
      --  basename for the SID file to create. Mimic how GNAT creates ALI
      --  files: use the project of the main source of the library unit, start
      --  from the basename of that source file, replace the last extension
      --  with ".sid". Also make sure to use the most extending project in the
      --  hierarchy, which is where GPRbuild puts ALI/object files.

      SID_Basename : US.Unbounded_String;

      Project  : constant GPR.Project_Type :=
        GPR.Extending_Project
          (Project => LU_Info.Instr_Project,
           Recurse => True);
      pragma Assert (Project /= GPR.No_Project);

      Output_Directory : constant Virtual_File :=
        (if In_Library_Dir
         then Project.Library_Ali_Directory
         else Project.Object_Dir);
   begin
      case Language_Kind (LU_Info.Language) is
         when Unit_Based_Language =>
            declare
               Src_Body_Basename : constant String := +Project.File_From_Unit
                 (Unit_Name       => +LU_Info.Unit_Name,
                  Part            => Unit_Body,
                  Language        =>  Image (LU_Info.Language),
                  File_Must_Exist => True);
               Src_Spec_Basename : constant String := +Project.File_From_Unit
                 (Unit_Name       => +LU_Info.Unit_Name,
                  Part            => Unit_Spec,
                  Language        =>  Image (LU_Info.Language),
                  File_Must_Exist => True);
               Src_Basename      : constant String :=
                 (if Src_Body_Basename = ""
                  then Src_Spec_Basename
                  else Src_Body_Basename);
               Src_Ext_Index     : constant Positive :=
                 Ada.Strings.Fixed.Index
                   (Src_Basename, ".", Ada.Strings.Backward);
            begin
               SID_Basename :=
                 +(Src_Basename (Src_Basename'First ..  Src_Ext_Index)
                   & "sid");
            end;
         when File_Based_Language =>
            SID_Basename :=
              +Ada.Directories.Simple_Name (+LU_Info.Unit_Name) & ".sid";
      end case;

      return String'(+Output_Directory.Full_Name) / (+SID_Basename);
   end SID_Filename;

   ------------------------------
   -- Compilation_Unit_Options --
   ------------------------------

   function Compilation_Unit_Options
     (IC      : Inst_Context;
      Prj     : Prj_Desc;
      LU_Info : Library_Unit_Info) return String_Vectors.Vector
   is
      Result : String_Vectors.Vector;

      procedure Compilation_Unit_Options_Ada;

      ----------------------------------
      -- Compilation_Unit_Options_Ada --
      ----------------------------------

      procedure Compilation_Unit_Options_Ada is
      begin
         Result.Append ("--gnatem=" & IC.Mapping_File);
         Result.Append ("--gnatec=" & IC.Config_Pragmas_File);

         --  Load the predefined source directories

         for Dir of IC.Predefined_Source_Dirs loop
            Result.Append ("--runtime-dir=" & Dir);
         end loop;
      end Compilation_Unit_Options_Ada;

   begin
      --  Depending on the language, pass the right set of options

      case LU_Info.Language is
         when Ada_Language =>
            Compilation_Unit_Options_Ada;
         when others =>
            null;
      end case;

      --  Pass the list of sources of interest, to e.g. skip the
      --  instrumentation of the spec / body / subunit for an Ada unit if
      --  it was requested through a --ignored-source-files switch.

      Result.Append ("--files=@" & IC.Sources_Of_Interest_Response_File);

      --  Pass the right language

      Result.Append (+"--lang=" & Image (LU_Info.Language));

      --  Pass the project description options

      Result.Append
        (String_Vectors.Vector'
           (Unparse (Prj, LU_Info.Unit_Name, LU_Info.Language)));

      return Result;
   end Compilation_Unit_Options;

   --  Create the instrumenter context

   IC : Inst_Context := Create_Context (Ignored_Source_Files);

   Root_Project_Info : constant Project_Info_Access :=
     Get_Or_Create_Project_Info (IC, Project.Project.Root_Project);

   --  Create a map from library units to lists of compilation units to
   --  instrument for them.

   Instrumented_Sources : Unit_Maps.Map;
   --  List of units that should be instrumented

   Files_Of_Interest_Str_Set : String_Sets.Set;
   Files_Of_Interest         : File_Info_Sets.Set;
   --  List of files of interest.
   --
   --  This is passed on to instrument-source invocations when instrumenting
   --  an Ada file (to know which part of a compilation unit must be
   --  instrumented, i.e. spec / body / separates).

   Project_Sources : File_Info_Sets.Set;
   --  Sets of all Ada and C family source files of all projects. If the dump
   --  trigger is "manual", traversing all these project sources will be
   --  necessary to find the indication marking the location where the user
   --  wishes the Dump_Buffers call to be inserted.

   Prj_Has_Manual_Helper : Prj_Has_Manual_Helper_Sets.Set;
   --  In the case of a manual dump config, set of names of projects for which
   --  a manual dump helper unit has already been emitted.

   ------------------------
   -- Add_Project_Source --
   ------------------------

   procedure Add_Project_Source
     (Project : GPR.Project_Type; Source_File : GPR.File_Info)
   is
      pragma Unreferenced (Project);
   begin
      --  Check if gnatcov was built with support for this language. If not,
      --  exit early.

      if not Builtin_Support (To_Language (Source_File.Language)) then
         return;
      end if;

      Project_Sources.Insert (Source_File);
   end Add_Project_Source;

   ---------------------------
   -- Add_Instrumented_Unit --
   ---------------------------

   procedure Add_Instrumented_Unit
     (Project : GPR.Project_Type; Source_File : GPR.File_Info)
   is
      Language  : constant Src_Supported_Language :=
        To_Language (Source_File.Language);
      Lang_Kind : constant Any_Language_Kind := Language_Kind (Language);

      use Unit_Maps;
      Unit_Name : constant String :=
        (case Lang_Kind is
            when Unit_Based_Language => Owning_Unit_Name (Source_File),
            when File_Based_Language =>
              GNATCOLL.VFS."+" (Source_File.File.Full_Name));

      Prj_Info : constant Project_Info_Access :=
        Get_Or_Create_Project_Info (IC, Source_File.Project);

      LU_Info : constant Library_Unit_Info :=
        (Unit_Name            => +Unit_Name,
         Instr_Project        => Project,
         Language_Kind        => Lang_Kind,
         Language             => Language,
         All_Externally_Built => Prj_Info.Externally_Built);

   begin
      --  Check if this is an ignored source file

      if Is_Ignored_Source_File (IC, +Source_File.File.Base_Name) then
         return;
      end if;

      --  Check if gnatcov was built with support for this language. If not,
      --  exit early.

      if not Builtin_Support (Language) then
         return;
      end if;

      --  Otherwise, this is a source of interest

      Files_Of_Interest.Insert (Source_File);
      Files_Of_Interest_Str_Set.Insert (+(+Source_File.File.Full_Name));

      --  Headers are not instrumented by themselves, so exit early as soon
      --  as they have been added to the sources of interest.

      if Language in C_Family_Language
         and then Source_File.Unit_Part = Unit_Spec
      then
         return;
      end if;

      if not Instrumented_Sources.Contains (Unit_Name) then
         Instrumented_Sources.Insert (Unit_Name, LU_Info);
      end if;
      declare
         Cur_Ref : constant Unit_Maps.Reference_Type :=
           Instrumented_Sources.Reference (Unit_Name);
      begin
         if not Prj_Info.Externally_Built then
            Cur_Ref.Instr_Project := Project;
            Cur_Ref.All_Externally_Built := False;
         end if;
      end;

      --  If the unit belongs to an externally built project, exit after it
      --  was added it to the instrumented sources. We won't instrument it
      --  in the current instrumentation run, so there is no need to grab
      --  information useful for instrumentation purposes.

      if Source_File.Project.Externally_Built
      then
         return;
      end if;

      --  Also get compiler switches that are file-specific and register them
      --  in the project description.

      if Language in C_Family_Language then
         declare
            use type GNAT.Strings.String_List_Access;
            Options       : Analysis_Options;
            Compiler_Opts : String_Vectors.Vector;
            Switches      : GNAT.Strings.String_List_Access;
            Basename      : constant String := +Source_File.File.Base_Name;
         begin
            Switches :=
              Project.Attribute_Value
                (Attribute => GPR.Build ("compiler", "switches"),
                 Index     => Basename);
            if Switches /= null then
               declare
                  Args : String_Vectors.Vector;
               begin
                  for S of Switches.all loop
                     Args.Append (To_Unbounded_String (S.all));
                  end loop;
                  GNAT.Strings.Free (Switches);
                  Import_From_Args (Options, Args);
               end;
               Add_Options (Compiler_Opts, Options, Pass_Builtins => False);
               Prj_Info.Desc.Compiler_Options_Unit.Insert
                 (LU_Info.Unit_Name, Compiler_Opts);
            end if;
         end;
      end if;
   end Add_Instrumented_Unit;

   --------------------------------
   -- Insert_Manual_Dump_Trigger --
   --------------------------------

   procedure Insert_Manual_Dump_Trigger
     (Language             : Src_Supported_Language;
      Project_Sources      : in out File_Info_Sets.Set;
      Instrumenter         : in out Language_Instrumenter'Class;
      Manual_Dump_Inserted : in out Boolean)
   is
      function Dump_Helper_Output_Dir_Exists
        (Source : File_Info; Prj : Prj_Desc)
         return Boolean
      is (Ada.Directories.Exists
          (GNATCOLL.VFS."+" (Source.Project.Object_Dir.Base_Dir_Name))
            and then Ada.Directories.Exists (+Prj.Output_Dir));
      --  True if the project's object directory and the instrumented sources
      --  directory exist, False otherwise.

      package Non_Root_Src_Calls_Sets is new
        Ada.Containers.Indefinite_Ordered_Sets (Element_Type => String);

      Non_Root_Src_Calls : Non_Root_Src_Calls_Sets.Set;
      --  Set of names of source files containing a dump buffers indication
      --  that belong to a non-root project.

   begin
      for Source_C in Project_Sources.Iterate loop
         declare
            Source : constant File_Info := File_Info_Sets.Element (Source_C);
         begin
            if To_Language (Source.Language) = Language then
               declare
                  use Prj_Has_Manual_Helper_Sets;

                  Prj_Info             : constant Project_Info_Access :=
                    Get_Or_Create_Project_Info (IC, Source.Project);
                  Prj                  : Prj_Desc renames Prj_Info.Desc;
                  Is_Root_Prj          : constant Boolean :=
                    Prj.Prj_Name = Root_Project_Info.Project.Name;
                  Source_Name          : constant String :=
                    GNATCOLL.VFS."+" (Source.File.Full_Name);
                  Helper_Unit          : Unbounded_String;
                  Contained_Indication : Boolean := False;

               begin
                  Instrumenter.Replace_Manual_Dump_Indication
                    (Prj_Info.Desc,
                     Source,
                     Contained_Indication);

                  if Contained_Indication and then not Is_Root_Prj
                  then
                     --  A call to the dump buffers procedure is only able to
                     --  dump the buffers of the project it is in and its
                     --  subprojects, meaning coverage data for all projects
                     --  higher in the project tree will be missing. Record
                     --  what file this call was in to warn the user later.

                     Non_Root_Src_Calls.Include (Source_Name);
                  end if;

                  --  Only generate one manual dump helper unit per project.
                  --  At this point, if the project's object directory and the
                  --  instrumented sources directory do not exist there is no
                  --  need to emit the dump helper unit. There are no units of
                  --  interest or call to a manual dump procedure for this
                  --  project.

                  if Prj_Has_Manual_Helper.Find (Prj.Prj_Name) = No_Element
                    and then Dump_Helper_Output_Dir_Exists (Source, Prj)
                  then
                     Instrumenter.Emit_Dump_Helper_Unit_Manual
                       (Helper_Unit, Dump_Config, Prj);

                     declare
                        use Files_Table;
                        Instr_Units  : Unit_Sets.Set;
                        Source_Files : GNATCOLL.VFS.File_Array_Access
                          := Source.Project.Source_Files (Recursive => True);
                     begin
                        for S of Source_Files.all loop
                           declare
                              use Unit_Maps;
                              Unit_C : constant Unit_Maps.Cursor :=
                                Instrumented_Sources.Find
                                  (+To_Compilation_Unit
                                     (Project.Project.Info (S)).Unit_Name);
                           begin
                              if Unit_C /= Unit_Maps.No_Element then
                                 declare
                                    Unit       : constant Library_Unit_Info :=
                                      Element (Unit_C);
                                    Instr_Unit : constant Compilation_Unit :=
                                      Compilation_Unit'
                                        (Unit.Language_Kind,
                                         Unit.Unit_Name);
                                 begin
                                    if not Instr_Units.Contains (Instr_Unit)
                                    then
                                       Instr_Units.Insert (Instr_Unit);
                                    end if;
                                 end;
                              end if;
                           end;
                        end loop;

                        --  The creation of the root project's buffers list
                        --  unit is already taken care of by the regular
                        --  instrumentation process, so skip it.

                        if not Is_Root_Prj then
                           Instrumenter.Emit_Buffers_List_Unit
                             (Instr_Units, Prj);
                        end if;

                        GNATCOLL.VFS.Unchecked_Free (Source_Files);
                     end;

                     Prj_Has_Manual_Helper.Insert (Prj.Prj_Name);
                  end if;

                  Manual_Dump_Inserted :=
                    Manual_Dump_Inserted or else Contained_Indication;
               end;
            end if;
         end;
      end loop;

      if not Non_Root_Src_Calls.Is_Empty then
         --  For each manual dump call inserted in a file belonging to a
         --  non-root project, warn the user the coverage data it will produce
         --  will not cover the whole project tree.

         declare
            All_File_Names : Unbounded_String := +"";
         begin
            for File_Name of Non_Root_Src_Calls loop
               All_File_Names :=
                 All_File_Names & (+File_Name) & Ada.Characters.Latin_1.LF;
            end loop;

            Outputs.Warn ("Manual dump trigger indications were found in"
                          & " subprojects in the following files:"
                          & Ada.Characters.Latin_1.LF
                          & (+All_File_Names)
                          & "The coverage report built from the source traces"
                          & " they will produce will show all code from"
                          & " projects higher in the project tree as not"
                          & " covered. To get a full coverage analysis,"
                          & " consider placing the manual dump buffers"
                          & " indication in the root project.");
         end;
      end if;
   end Insert_Manual_Dump_Trigger;

   ---------------------
   -- Clean_And_Print --
   ---------------------

   procedure Clean_And_Print (Exc : Ada.Exceptions.Exception_Occurrence) is
   begin
      Clean_Objdirs;
      Outputs.Print_Internal_Error (Exc);
   end Clean_And_Print;

   Mains_To_Instrument : array (Src_Supported_Language)
     of Main_To_Instrument_Vectors.Vector;
   --  For each supported language, list of mains to instrument. Note that
   --  this is filled even when dump-trigger is manual: in that case the
   --  instrumentation of the main will do nothing.

   Manual_Dump_Inserted : Boolean := False;
   --  Whether or not a dump procedure was inserted in any source file

   Exec_Filename : constant String := Ada.Directories.Compose
     (Support_Files.Libexec_Dir,
      "gnatcov64" & GNAT.OS_Lib.Get_Executable_Suffix.all);
   --  Launch gnatcov64 for gnatcov subprocesses (when instrumenting sources
   --  and mains), to bypass the wrapper and save some execution time.

   Ada_Instrumenter : aliased Instrument.Ada_Unit.Ada_Instrumenter_Type;
   C_Instrumenter   : aliased Instrument.C.C_Instrumenter_Type;
   CPP_Instrumenter : aliased Instrument.C.CPP_Instrumenter_Type;
   Instrumenters    : constant array (Src_Supported_Language)
     of access Language_Instrumenter'Class :=
       (Ada_Language => Ada_Instrumenter'Access,
        C_Language   => C_Instrumenter'Access,
        CPP_Language => CPP_Instrumenter'Access);

   --  Start of processing for Instrument_Units_Of_Interest

begin
   --  Set the instrumentation tag

   IC.Tag := +Instrumentation_Tag;

   --  Delete output directories from previous instrumentations

   Clean_Objdirs;

   --  First get the list of all units of interest

   for Lang in Src_Supported_Language loop
      if Src_Enabled_Languages (Lang) then
         Project.Enumerate_Sources
           (Add_Instrumented_Unit'Access,
            Lang,
            Include_Stubs => True,
            Only_UOIs     => True);

         if Dump_Config.Trigger = Manual then
            --  The expected manual dump indication can be located in any
            --  source file, not only in sources of interest.

            Project.Enumerate_Sources
              (Add_Project_Source'Access, Lang, Include_Stubs => True);
         end if;
      end if;
   end loop;

   --  Remove all of the separate whose parent unit was not instrumented, as
   --  this is not supported. TODO???: we should probably issue a warning
   --  there.

   for Source of Files_Of_Interest.Copy loop
      if Source.Unit_Part = Unit_Separate then
         declare
            Parent_Unit : constant GNATCOLL.Projects.File_Info :=
              Project.Project.Info
                (Project.Project.Other_File (Source.File));
         begin
            if not Files_Of_Interest.Contains (Parent_Unit) then
               Files_Of_Interest.Delete (Source);
            end if;
         end;
      end if;
   end loop;

   --  If we need to instrument all the mains, also go through them now, so
   --  that we can prepare output directories for their projects later on.
   --  Note that for user convenience, we want to do this for all the
   --  languages that gnatcov supports, even those that are not considered
   --  for coverage analysis.
   --
   --  If no source file was specified on the command line to be a main,
   --  use the list of mains specified in project files.

   if Mains.Is_Empty then
      for Lang in Src_Supported_Language loop
         for Main of Project.Enumerate_Mains (Lang) loop
            Register_Main_To_Instrument
              (IC, Mains_To_Instrument (Lang), Main.File, Main.Project);
         end loop;
      end loop;

   --  Otherwise, make sure we can find the source file of each main in
   --  the project tree and that we can instrument them (supported
   --  language).

   else
      for Filename of Mains loop
         declare
            use GNATCOLL.VFS;

            F       : constant String := +Filename;
            Info    : constant File_Info :=
              Project.Project.Root_Project.Create_From_Project (+F);
            File    : constant Virtual_File := Info.File;
            Project : constant Project_Type := Info.Project;
            Lang    : Any_Language;
         begin
            if File = No_File or else Project = No_Project then
               Outputs.Fatal_Error ("No such source file: " & F);
            end if;

            Lang := To_Language_Or_All (Info.Language);
            if Lang not in Src_Supported_Language
              or else not Builtin_Support (Lang)
            then
               Outputs.Fatal_Error
                 ("Cannot instrument main source file (unsupported"
                  & " language): " & F);
            end if;

            Register_Main_To_Instrument
              (IC, Mains_To_Instrument (Lang), File, Project);
         end;
      end loop;
   end if;

   --  Check early if there are no sources of interest

   if Files_Of_Interest.Length = 0 then
      Outputs.Fatal_Error ("No unit to instrument.");
   end if;

   --  Now that we know all the sources we need to instrument, prepare
   --  output directories.

   Prepare_Output_Dirs (IC);

   --  Clean the instrumentation directories if the process terminated with
   --  an exception. Note that we can't do this with a handler, as the
   --  Clean_And_Print hook must be executed before local context objects at
   --  the raise point are finalized. Using the
   --  Register_Global_Unhandled_Action, the hook will be triggered before
   --  local controlled objects are finalized.

   GNAT.Exception_Actions.Register_Global_Unhandled_Action
     (Clean_And_Print'Unrestricted_Access);

   --  Record in a file all of the files and headers of interest. We separate
   --  both as we do not expect to have coverage buffers for header files (the
   --  coverage data is in including bodies' coverage buffers).

   IC.Mapping_File :=
     +Create_Ada_Mapping_File (Project.Project.Root_Project);
   IC.Config_Pragmas_File :=
     +Create_Config_Pragmas_File (Project.Project.Root_Project);
   IC.Sources_Of_Interest_Response_File :=
     +To_String (Root_Project_Info.all.Output_Dir) / ".sources_of_interest";

   --  Set the runtime directories

   declare
      use GNATCOLL.VFS;
   begin
      for Dir of
        Project.Project.Root_Project.Get_Environment.Predefined_Source_Path
      loop
         IC.Predefined_Source_Dirs.Append (+(+Full_Name (Dir)));
      end loop;
   end;

   --  Initialize the instrumenters: we will use them when parallelization is
   --  disabled, but also to generate the unit holding the list of buffers,
   --  in one of the supported languages.

   Ada_Instrumenter :=
     Create_Ada_Instrumenter
       (IC.Tag,
        +IC.Config_Pragmas_File,
        +IC.Mapping_File,
        IC.Predefined_Source_Dirs);
   C_Instrumenter := Create_C_Instrumenter (IC.Tag);
   CPP_Instrumenter := Create_CPP_Instrumenter (IC.Tag);

   if Dump_Config.Trigger = Manual then
      --  Replace manual dump indications for C-like languages

      for Lang in C_Family_Language loop
         Insert_Manual_Dump_Trigger
           (Lang,
            Project_Sources,
            Instrumenters (Lang).all,
            Manual_Dump_Inserted);
      end loop;
   end if;

   --  Write the files of interest to temporary files in the instrumentation
   --  directory.

   declare
      Sources_Of_Interest_File : Text_Files.File_Type;
      --  File containing the list of sources of interest

   begin
      Sources_Of_Interest_File.Create
        (+IC.Sources_Of_Interest_Response_File);
      for Source of Files_Of_Interest loop
         Sources_Of_Interest_File.Put_Line (+Source.File.Full_Name);
      end loop;
      Sources_Of_Interest_File.Close;
   end;

   --  Instrument every unit of interest asynchronously

   declare
      Instrument_Source_Pool : Process_Pool (Parallelism_Level);

      Instrument_Source_Args : String_Vectors.Vector;
      --  Common arguments for all of the gnatcov instrument-source
      --  invocations.

   begin
      Instrument_Source_Args.Append (+"instrument-source");
      Instrument_Source_Args.Append
        (Common_Switches (Cmd_Instrument_Source));
      for Cur in Instrumented_Sources.Iterate loop

         declare
            Unit_Args : String_Vectors.Vector :=
              Instrument_Source_Args.Copy;
            --  Args specific to a gnatcov instrument-source invocation
            --  (e.g. the common arguments + the sources that must be
            --  instrumented for a specific unit).

            Unit_Name : constant String := Unit_Maps.Key (Cur);
            LU_Info   : constant Library_Unit_Info := Unit_Maps.Element (Cur);
            Obj_SID   : constant String :=
              SID_Filename (LU_Info, In_Library_Dir => False);

            Prj       : constant Project_Type := LU_Info.Instr_Project;
            Desc      : constant Prj_Desc :=
              IC.Project_Info_Map.Element (+Prj.Name).Desc;
         begin
            --  Skip instrumentation of the unit if it was already
            --  instrumented.

            if LU_Info.All_Externally_Built then
               goto Continue;
            end if;

            --  Add the arguments that are specific to the compilation unit

            Unit_Args.Append
              (Compilation_Unit_Options
                 (IC,
                  Desc,
                  LU_Info));

            Unit_Args.Append (+"--sid=" & Obj_SID);

            --  We instrument the body, spec and separates as a whole

            Unit_Args.Append (+Unit_Name);

            --  According to the set parallelism level, instrument in
            --  the same process (thus reusing the libadalang context, which
            --  is a big gain of time), or spawn another instrumentation
            --  process.

            if Parallelism_Level = 1 then
               Instrument.Source
                 (Unit_Name         => Unit_Name,
                  SID_Name          => Obj_SID,
                  Instrumenter      => Instrumenters (LU_Info.Language).all,
                  Files_Of_Interest => Files_Of_Interest_Str_Set,
                  Prj               => Desc);
            else
               --  Asynchronously instrument

               Instrument_Source_Pool.Run_Command
                 (Command             => Exec_Filename,
                  Arguments           => Unit_Args,
                  Origin_Command_Name => "gnatcov instrument",
                  Ignore_Error        => False);
            end if;
         end;
         <<Continue>>
      end loop;
   end;

   --  Copy SID files into the library directory

   for LU_Info of Instrumented_Sources loop
      declare
         Obj_SID : constant String :=
           SID_Filename (LU_Info, In_Library_Dir => False);
         Lib_SID : constant String :=
           SID_Filename (LU_Info, In_Library_Dir => True);
         Success : Boolean;
      begin
         if not LU_Info.All_Externally_Built and then Obj_SID /= Lib_SID then

            --  Unlike the object directory, which GNATCOLL.Project
            --  creates automatically, the library directory may not
            --  exist: create it if needed.

            declare
               use GNATCOLL.VFS;
            begin
               Create (Create (+Lib_SID).Dir_Name).Make_Dir;
            exception
               when Exc : VFS_Directory_Error =>
                  Outputs.Fatal_Error
                    (Ada.Exceptions.Exception_Message (Exc));
            end;

            GNAT.OS_Lib.Copy_File
              (Name     => Obj_SID,
               Pathname => Lib_SID,
               Success  => Success,
               Mode     => GNAT.OS_Lib.Overwrite);
            if not Success then
               Outputs.Fatal_Error
                 ("Error while copying " & Obj_SID
                  & " to the library directory: " & Lib_SID);
            end if;
         end if;
      end;
   end loop;

   --  Then, instrument asynchronously every main

   declare
      Instrument_Main_Pool : Process_Pool (Parallelism_Level);
      Instrument_Main_Args : String_Vectors.Vector;

      Main_Filename : Unbounded_String;
      --  Fullname for the main. It can either be an instrumented version of
      --  the main (if it also was instrumented as a source), or the original
      --  version.

      Explicit_Dump_Config : Any_Dump_Config := Dump_Config;
      --  Dump config with explicited defaults. The only interesting thing
      --  is the dump-filename-prefix that is computed after the name of
      --  the main in the project file, if not specified explicitly on the
      --  command line.

   begin
      Instrument_Main_Args.Append (+"instrument-main");

      --  Add the root project name, as the symbol holding the list of
      --  coverage buffers is defined accordingly.

      Instrument_Main_Args.Append
        (+"--project-name=" & Root_Project_Info.Project.Name);

      Instrument_Main_Args.Append (Common_Switches (Cmd_Instrument_Main));

      for Language in Src_Supported_Language loop
         for Main of Mains_To_Instrument (Language) loop
            declare
               Unit_Name : constant Unbounded_String :=
                 +(case Main.CU_Name.Language_Kind is
                      when Unit_Based_Language =>
                        To_Ada (Main.CU_Name.Unit),
                      when File_Based_Language => (+Main.File.Full_Name));
               Unit_Args : String_Vectors.Vector :=
                 Instrument_Main_Args.Copy;
            begin
               Unit_Args.Append
                 (Compilation_Unit_Options
                    (IC,
                     Main.Prj_Info.Desc,
                     Library_Unit_Info'
                       (Unit_Name            => Unit_Name,
                        Instr_Project        => Main.Prj_Info.Project,
                        Language_Kind        => Language_Kind (Language),
                        Language             => Language,
                        All_Externally_Built => False)));

               --  Pass main-specific dump-config options

               if Dump_Config.Channel = Binary_File then

                  --  If no dump filename prefix was specified, compute it
                  --  here: we use the executable name, that is retrieved from
                  --  the project.

                     if Length (Dump_Config.Filename_Prefix) = 0 then
                        Explicit_Dump_Config.Filename_Prefix :=
                          +(+Root_Project_Info.Project.Executable_Name
                              (Main.File.Full_Name, Include_Suffix => True));
                     end if;
               end if;
            end;

            if Dump_Config.Trigger /= Manual then
               declare
                  Unit_Name : constant Unbounded_String :=
                    +(case Main.CU_Name.Language_Kind is
                         when Unit_Based_Language =>
                           To_Ada (Main.CU_Name.Unit),
                         when File_Based_Language => (+Main.File.Full_Name));
                  Unit_Args : String_Vectors.Vector :=
                    Instrument_Main_Args.Copy;
               begin
                  Unit_Args.Append
                    (Compilation_Unit_Options
                       (IC,
                        Main.Prj_Info.Desc,
                        Library_Unit_Info'
                          (Unit_Name            => Unit_Name,
                           Instr_Project        => Main.Prj_Info.Project,
                           Language_Kind        => Language_Kind (Language),
                           Language             => Language,
                           All_Externally_Built => False)));

                  Unit_Args.Append (Unparse_Config (Explicit_Dump_Config));

                  --  Then append the main filename. If the main was
                  --  instrumented as a unit of interest before, then pass the
                  --  instrumented version.

                  if Instrumented_Sources.Contains (+Unit_Name) then
                     Main_Filename :=
                       +(+Root_Project_Info.Output_Dir)
                       / (+Main.File.Base_Name);
                  else
                     Main_Filename := +(+Main.File.Full_Name);
                  end if;

                  Unit_Args.Append (Main_Filename);

                  if Parallelism_Level = 1 then
                     declare
                        Instr_Units : String_Sets.Set;
                     begin
                        for Source of Files_Of_Interest loop
                           Instr_Units.Insert (+(+Source.File.Full_Name));
                        end loop;
                        Instrument.Main
                          (Instrumenter  => Instrumenters (Language).all,
                           Dump_Config   => Explicit_Dump_Config,
                           Main_Filename => +Main_Filename,
                           Prj           => Main.Prj_Info.Desc);
                     end;
                  else
                     Instrument_Main_Pool.Run_Command
                       (Command             => Exec_Filename,
                        Arguments           => Unit_Args,
                        Origin_Command_Name => "gnatcov instrument",
                        Ignore_Error        => False);
                  end if;
               end;
            end if;
         end loop;
      end loop;
   end;

   if not Args.Bool_Args (Opt_Save_Temps) then
      Ada.Directories.Delete_File (+IC.Sources_Of_Interest_Response_File);
   end if;

   --  Emit the unit to contain the list of coverage buffers, exported to a
   --  C symbol, in one of the language supported by the project.
   --
   --  Note that this has an implicit hack to it: if Ada is a language of
   --  the project, it will pick it over the others (as it is the first
   --  enumeration member of the Src_Supported_Language type). This matters
   --  as we make the assumption in the Emit_Dump_Helper_Unit implementation
   --  in instrument-ada_unit.adb (when instrumenting for an Ada main) that
   --  the Ada package for buffers list units always exists: we need to
   --  include it in the main closure, as it puts buffer units in scope
   --  by importing them (otherwise they aren't as they are used through
   --  C symbol importations).

   declare
      use Files_Table;
      Instr_Units : Unit_Sets.Set;
   begin
      for LU_Info of Instrumented_Sources loop
         Instr_Units.Insert
           (Compilation_Unit'(LU_Info.Language_Kind, LU_Info.Unit_Name));
      end loop;
      for Lang in Src_Supported_Language loop
         if Builtin_Support (Lang)
           and then Root_Project_Info.Project.Has_Language (Image (Lang))
         then
            Instrumenters (Lang).Emit_Buffers_List_Unit
              (Instr_Units, Root_Project_Info.Desc);
            exit;
         end if;
      end loop;
   end;

   if Dump_Config.Trigger = Manual then
      Insert_Manual_Dump_Trigger
        (Ada_Language,
         Project_Sources,
         Ada_Instrumenter,
         Manual_Dump_Inserted);

      --  At this point, all source files for all languages have been looked
      --  through to insert a call to the manual dump procedure. If no call
      --  has been inserted (i.e. no manual dump location indication was
      --  found), warn the user.
      if not Manual_Dump_Inserted then
         Outputs.Warn
           ("no indication for dump location was found, this might be"
            & " caused by a misspelling in the expected pragma"
            & " statement or comment.");
      end if;
   end if;

   Destroy_Context (IC);

   --  Save the dump trigger+channel information in the root project's
   --  object directory. This allows user scripts to automatically know
   --  where to expect source trace files (dump channel) without inspecting
   --  all inputs (command-line arguments, project file, instrumentation
   --  runtime, etc.) and whether that info is reliable (it is not if the
   --  dump trigger is manual).
   --
   --  TODO: this should go at some point in Instrument_Main (in which case
   --  one would be generated per main).

   declare
      J        : constant GNATCOLL.JSON.JSON_Value := Create_Object;
      Filename : constant String :=
        Project.Output_Dir & "/gnatcov-instr.json";
   begin
      J.Set_Field ("dump-trigger", Image (Dump_Config.Trigger));
      J.Set_Field ("dump-channel", Image (Dump_Config.Channel));
      Write (Filename, J, Compact => False);
   end;

exception
   --  We need to clear the object directories in case an exception (caught
   --  by a global handler and not dealt with by the registered hook) was
   --  raised.

   when Binary_Files.Error
      | Ada.IO_Exceptions.Name_Error
      | Outputs.Xcov_Exit_Exc =>
      Clean_Objdirs;
      raise;
end Instrument.Projects;
