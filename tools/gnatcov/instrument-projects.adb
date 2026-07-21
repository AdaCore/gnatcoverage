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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

with GNAT.Exception_Actions;

with GNAT.Regexp;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.VFS;  use GNATCOLL.VFS;

with GPR2;
with GPR2.Build.Compilation_Unit;
with GPR2.Build.Actions_Scheduler;
with GPR2.Build.Source;
with GPR2.Build.Source.Sets;
with GPR2.Build.Tree_Db;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Libadalang.Project_Provider; use Libadalang.Project_Provider;

with Binary_Files;
with Command_Line;        use Command_Line;
with Files_Handling;      use Files_Handling;
with Files_Table;         use Files_Table;
with Instrument.Actions.Instrument_Source.Process;
with Instrument.Actions.Instrument_Source.Thread;
with Instrument.Ada_Preprocessing;
with Instrument.Ada_Unit; use Instrument.Ada_Unit;
with Instrument.Ada_Unit_Provider;
with Instrument.C;        use Instrument.C;
with Instrument.Clean_Objdirs;
with Instrument.Common;   use Instrument.Common;
with Instrument.Debug_Dump;
with JSON;                use JSON;
with Outputs;
with Paths;               use Paths;
with Project;             use Project;
with Switches_GPR;        use Switches_GPR;
with Text_Files;          use Text_Files;

--  Generate instrumented sources for the source files of all units of
--  interest. Also save mappings between coverage buffers and SCOs for each
--  library units to SID files (one per library unit).
--
--  Depending on Dump_Config, instrument mains to schedule a call to the
--  dump procedure for the list of coverage buffers in all mains in the
--  project.
--
--  RTS_Source_Dirs specifies the list of source directories in the coverage
--  runtime. Used to give access to its C headers from the C/C++ instrumenter.
--
--  Language_Version restricts what source constructs the instrumenter is
--  allowed to use. For instance, if Ada_2005 (or a lower version) is
--  passed, it will not be allowed to introduce expression functions, and
--  thus will emit a warning when it needed to do so.
--
--  If Excluded_Source_File is non-null, ignore files whose names match the
--  accessed pattern.
--
--  Mains is the list of source files that were listed on the command line:
--  if non-empty, they replace the mains specified in project files.

procedure Instrument.Projects
  (Dump_Config           : Any_Dump_Config;
   RTS_Source_Dirs       : File_Vectors.Vector;
   Excluded_Source_Files : access GNAT.Regexp.Regexp;
   Mains                 : String_Vectors.Vector)
is
   use type GPR2.Language_Id;
   use type GPR2.Unit_Kind;

   function Create_Context
     (Excluded_Source_Files : access GNAT.Regexp.Regexp) return Inst_Context;
   --  Create an instrumentation context for the currently loaded project

   procedure Destroy_Context (Context : in out Inst_Context);
   --  Free dynamically allocated resources in Context, and cleanup temporary
   --  files.

   function Is_Excluded_Source_File
     (Context : Inst_Context; Filename : String) return Boolean;
   --  Return whether the instrumentation process must ignore the Filename
   --  source file.

   procedure Prepare_Output_Dirs (IC : Inst_Context);
   --  Make sure we have the expected tree of directories for the
   --  instrumentation output.

   function Unique_Unit_Name (Source : GPR2.Build.Source.Object) return String;
   --  Return the unique unit name for the given source. For Ada, this is the
   --  unit name, and for C/C++, this is the source full name.

   function Main_Part_Src
     (Source : GPR2.Build.Source.Object) return GPR2.Build.Source.Object;
   --  Return the main part of the compilation unit Source is part of. It may
   --  be Source itself, or the body part if Source is e.g. a specification and
   --  the unit also has a body.

   function Skip_Source (Src : GPR2.Build.Source.Object) return Boolean;
   --  Return whether the given source should be skipped (e.g. if it is a C/C++
   --  header).

   function Is_Multi_Unit (Src : GPR2.Build.Source.Object) return Boolean
   is (Src.Has_Units and then Src.Units.Length > 1);

   procedure Add_Project_Source
     (Source  : GPR2.Build.Source.Object;
      Is_UOI  : Boolean := False;
      Is_Main : Boolean := False);
   --  Add the given source to the list of units to instrument, for source,
   --  instrumentation, main instrumentation or annotations replacement.
   --
   --  If Is_UOI is True, consider Source to be a unit of interest.
   --
   --  If Is_Main is True, consider Source to be a main.

   procedure Add_Instrumented_Unit
     (Project : GPR2.Project.View.Object; Source : GPR2.Build.Source.Object);
   --  Add this source file to the list of units (of interest) to instrument

   function Units_Of_Interest
     (IC : Inst_Context; Project : GPR2.Project.View.Object)
      return Unit_Sets.Set;
   --  Return the list of units of interest in the project closure

   procedure Clean_And_Print (Exc : Ada.Exceptions.Exception_Occurrence);
   --  Clean the instrumentation directories and print any relevant information
   --  regarding the instrumentation context.

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Excluded_Source_Files : access GNAT.Regexp.Regexp) return Inst_Context is
   begin
      return IC : Inst_Context do
         IC.Excluded_Source_Files_Present := Excluded_Source_Files /= null;
         if Excluded_Source_Files /= null then
            IC.Excluded_Source_Files := Excluded_Source_Files.all;
         end if;
      end return;
   end Create_Context;

   ---------------------
   -- Destroy_Context --
   ---------------------

   procedure Destroy_Context (Context : in out Inst_Context) is
      procedure Free is new
        Ada.Unchecked_Deallocation (Project_Info, Project_Info_Access);
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
   -- Is_Excluded_Source_File --
   ----------------------------

   function Is_Excluded_Source_File
     (Context : Inst_Context; Filename : String) return Boolean is
   begin
      return
        Context.Excluded_Source_Files_Present
        and then
          GNAT.Regexp.Match
            (S => Fold_Filename_Casing (Filename),
             R => Context.Excluded_Source_Files);
   end Is_Excluded_Source_File;

   -------------------------
   -- Prepare_Output_Dirs --
   -------------------------

   procedure Prepare_Output_Dirs (IC : Inst_Context) is
      use Project_Info_Maps;
   begin
      for Cur in IC.Project_Info_Map.Iterate loop
         declare
            Prj_Info   : Project_Info renames Element (Cur).all;
            Output_Dir : constant String :=
              +(Element (Cur).Desc.Output_Dir.Full_Name);
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

   --  Create the instrumenter context

   IC : Inst_Context := Create_Context (Excluded_Source_Files);

   Root_Project_Info : constant Project_Info_Access :=
     Get_Or_Create_Project_Info (IC, Project.Root_Project);

   --  Create a map from library units to lists of compilation units to
   --  instrument for them.

   Instrumented_Sources : Unit_Maps.Map;
   --  List of units that should be instrumented, for source instrumentation
   --  purpose.

   Files_Of_Interest_Info : Source_Sets.Set;
   --  List of GPR2.Build.Source.Object, to be able to retrieve, e.g. the
   --  separates of a compilation unit.

   Mains_To_Instrument : Source_Sets.Set;
   --  List of mains to instrument. Note that this is filled even when using
   --  the manual dump trigger, to insert a with clause to the dump helper
   --  unit, thus making sure the coverage buffers end up in the main closure.

   ----------------------
   -- Unique_Unit_Name --
   ----------------------

   function Unique_Unit_Name (Source : GPR2.Build.Source.Object) return String
   is
      Language  : constant Src_Supported_Language :=
        To_Language (Source.Language);
      Lang_Kind : constant Supported_Language_Kind := Language_Kind (Language);
   begin
      if Lang_Kind = Unit_Based_Language then
         return Get_Unit_Name (Source);
      else
         return String (Source.Path_Name.Value);
      end if;
   end Unique_Unit_Name;

   -------------------
   -- Main_Part_Src --
   -------------------

   function Main_Part_Src
     (Source : GPR2.Build.Source.Object) return GPR2.Build.Source.Object
   is
      Root_Prj  : constant GPR2.Project.View.Object := Project.Root_Project;
      Lang_Kind : constant Supported_Language_Kind :=
        Language_Kind (To_Language (Source.Language));
   begin
      if Lang_Kind = Unit_Based_Language then
         return
           Root_Prj.Visible_Source
             (Root_Prj.Unit (Source.Unit.Name).Main_Part.Source);
      else
         return Source;
      end if;
   end Main_Part_Src;

   -----------------
   -- Skip_Source --
   -----------------

   function Skip_Source (Src : GPR2.Build.Source.Object) return Boolean is
      Language : constant Src_Supported_Language := To_Language (Src.Language);
   begin
      --  Headers are not instrumented by themselves, so exit early as soon
      --  as they have been added to the sources of interest.

      return
        (Language in C_Family_Language and then Src.Kind = GPR2.S_Spec)

        --  Ada bodies that just contain the No_Body pragmas cannot be
        --  instrumented (not worthwhile anyway): just skip them.

        or else
          (Language = Ada_Language
           and then First_Unit (Src).Kind = GPR2.S_No_Body);
   end Skip_Source;

   ------------------------
   -- Add_Project_Source --
   ------------------------

   procedure Add_Project_Source
     (Source  : GPR2.Build.Source.Object;
      Is_UOI  : Boolean := False;
      Is_Main : Boolean := False)
   is
      Project  : constant GPR2.Project.View.Object := Source.Owning_View;
      Prj_Info : constant Project_Info_Access :=
        Get_Or_Create_Project_Info (IC, Project);
   begin
      --  Start by checking if this is a supported language

      if To_Language_Or_All (Source.Language) = All_Languages then
         return;
      end if;

      declare
         Language  : constant Src_Supported_Language :=
           To_Language (Source.Language);
         Lang_Kind : constant Supported_Language_Kind :=
           Language_Kind (Language);
         Unit_Name : constant String := Unique_Unit_Name (Source);
      begin
         if not Builtin_Support (Language)
           or else Project.Is_Externally_Built
           or else Project.Is_Runtime
           or else Skip_Source (Source)
         then
            return;
         end if;

         --  Check that this is not a multi unit source as gnatcov does not
         --  support these.

         if Is_Multi_Unit (Source) then
            Outputs.Error
              ("instrumentation failed for " & String (Source.Path_Name.Name));
            Outputs.Error
              ("source files containing multiple compilation units"
               & " are not supported");
            raise Outputs.Xcov_Exit_Exc;
         end if;

         --  If not already done, note that the unit that owns this source file
         --  must be instrumented.

         declare
            Cur      : Unit_Maps.Cursor :=
              Instrumented_Sources.Find (Unit_Name);
            Inserted : Boolean;
         begin
            if not Unit_Maps.Has_Element (Cur) then
               Instrumented_Sources.Insert
                 (Unit_Name,
                  (Main_Part_Src => Main_Part_Src (Source),

                   --  Set both fields afterwards

                   Is_UOI        => False,
                   Is_Main       => False,
                   Language_Kind => Lang_Kind,
                   Language      => Language,
                   others        => <>),
                  Cur,
                  Inserted);
               pragma Assert (Inserted);

               --  Also get compiler switches that are file-specific and
               --  register them in the project description.

               if Language in C_Family_Language then
                  declare
                     package R renames GPR2.Project.Registry.Attribute;

                     Options       : Analysis_Options;
                     Compiler_Opts : String_Vectors.Vector;
                     Switches      : GPR2.Project.Attribute.Object;
                     Basename      : constant String :=
                       String (Source.Path_Name.Simple_Name);
                  begin
                     Switches :=
                       Project.Attribute
                         (Name  => R.Compiler.Switches,
                          Index =>
                            GPR2.Project.Attribute_Index.Create (Basename));
                     if Switches.Is_Defined then
                        declare
                           Args : String_Vectors.Vector;
                        begin
                           for S of Switches.Values loop
                              Args.Append (+S.Text);
                           end loop;
                           Import_From_Args (Options, Args);
                        end;
                        Add_Options
                          (Compiler_Opts, Options, Pass_Builtins => False);
                        Prj_Info.Desc.Compiler_Options_Unit.Insert
                          (Create_Normalized (Unit_Name), Compiler_Opts);
                     end if;
                  end;
               end if;
            end if;

            --  Keep track of this source file inside that unit. Also memorize
            --  the project that owns it if it is a non-Ada source file, an Ada
            --  spec or an Ada body.

            declare
               LU_Info : Library_Unit_Info renames
                 Instrumented_Sources.Reference (Cur);
               Kind    : constant GPR2.Unit_Kind :=
                 (if Language = Ada_Language
                  then First_Unit (Source).Kind
                  else GPR2.S_Body);
            begin
               case Kind is
                  when GPR2.S_Spec =>
                     LU_Info.Spec_Project := Project;

                  when GPR2.S_Body =>
                     LU_Info.Body_Project := Project;

                  when others      =>
                     null;
               end case;
               LU_Info.Sources.Include (Source);

               LU_Info.Is_UOI := LU_Info.Is_UOI or else Is_UOI;
               LU_Info.Is_Main := LU_Info.Is_Main or else Is_Main;
            end;
         end;
      end;
   end Add_Project_Source;

   ---------------------------
   -- Add_Instrumented_Unit --
   ---------------------------

   procedure Add_Instrumented_Unit
     (Project : GPR2.Project.View.Object; Source : GPR2.Build.Source.Object)
   is
      pragma Unreferenced (Project);
      Language : constant Src_Supported_Language :=
        To_Language (Source.Language);
   begin
      --  Check if this is a file of interest

      if not Is_Excluded_Source_File
               (IC, String (Source.Path_Name.Simple_Name))
        and then Builtin_Support (Language)
      then
         Add_Project_Source (Source, Is_UOI => True);
         if not Is_Multi_Unit (Source) then
            Files_Of_Interest_Info.Insert (Source);

            --  Normalize the path: lookups in Files_Of_Interest use
            --  Create_Normalized probes, and Virtual_File comparisons do not
            --  resolve path differences on Windows.

            IC.Files_Of_Interest.Insert
              (Create_Normalized (String (Source.Path_Name.Value)));
         end if;
      end if;

      --  If dump debug info was requested, add entries for buffer symbols

      if Args.String_Args (Opt_Dump_Debug).Present
        and then not Skip_Source (Source)
      then
         declare
            CU : constant Files_Table.Compilation_Unit :=
              To_Compilation_Unit (Source);
         begin
            Instrument.Debug_Dump.Register_Buffer_Symbols_For_Unit (CU);
         end;
      end if;
   end Add_Instrumented_Unit;

   -----------------------
   -- Units_Of_Interest --
   -----------------------

   function Units_Of_Interest
     (IC : Inst_Context; Project : GPR2.Project.View.Object)
      return Unit_Sets.Set
   is
      Result : Unit_Sets.Set;
   begin
      --  Include all units of interest, including those from externally
      --  built projects.

      for S of
        Source_Closure
          (View                  => Project,
           With_Externally_Built =>
             Externally_Built_Projects_Processing_Enabled,
           With_Runtime          => False)
      loop
         if IC.Files_Of_Interest.Contains (S.Path_Name.Virtual_File)
           and then not Skip_Source (S)
         then
            Result.Include (To_Compilation_Unit (S));
         end if;
      end loop;
      return Result;
   end Units_Of_Interest;

   ---------------------
   -- Clean_And_Print --
   ---------------------

   procedure Clean_And_Print (Exc : Ada.Exceptions.Exception_Occurrence) is
   begin
      if not Save_Temps then
         Clean_Objdirs
           (IC,
            Instrumented_Sources,

            --  Do not preserve instrumentation artifacts on an exception
            --  termination.

            Preserve_Artifacts => False);
         Outputs.Print_Internal_Error (Exc);
      end if;
   end Clean_And_Print;

   use all type GPR2.Build.Actions_Scheduler.Report_Status;

   Ada_Instrumenter : aliased Instrument.Ada_Unit.Ada_Instrumenter_Type;
   C_Instrumenter   : aliased Instrument.C.C_Instrumenter_Type;
   CPP_Instrumenter : aliased Instrument.C.CPP_Instrumenter_Type;
   Instrumenters    :
     constant array (Src_Supported_Language) of Language_Instrumenter_Acc :=
       (Ada_Language => Ada_Instrumenter'Unrestricted_Access,
        C_Language   => C_Instrumenter'Unrestricted_Access,
        CPP_Language => CPP_Instrumenter'Unrestricted_Access);

   Tree_Db : GPR2.Build.Tree_Db.Object_Access renames
     Project.Project.Artifacts_Database;

   Scheduler : GPR2.Build.Actions_Scheduler.Object;
   --  The scheduler takes care of scheduling source instrument actions

   Serial_Execution : constant Boolean :=
     not Force_Parallelism and then Parallelism_Level = 1;

   --  Start of processing for Instrument_Units_Of_Interest

begin
   --  Set the instrumentation tag

   IC.Tag := +Instrumentation_Tag;

   --  First, go through the mains.
   --
   --  Note that for user convenience, we want to do this for all the
   --  languages that gnatcov supports, even those that are not considered
   --  for coverage analysis.
   --
   --  If no source file was specified on the command line to be a main,
   --  use the list of mains specified in project files.

   if Mains.Is_Empty then
      for Lang in Src_Supported_Language loop
         for Main of Project.Enumerate_Mains (Lang) loop
            Mains_To_Instrument.Insert
              (Main.View.Visible_Source (Main.Source));
         end loop;
      end loop;

   --  Otherwise, make sure we can find the source file of each main in the
   --  project tree and that we can instrument them (supported language).

   else
      for Filename of Mains loop
         declare
            F      : constant String := +Filename;
            Source : constant GPR2.Build.Source.Object :=
              Project.Root_Project.Source (GPR2.Simple_Name (F));
            Lang   : Any_Language;
         begin
            if not Source.Is_Defined then
               Outputs.Fatal_Error ("No such source file: " & F);
            end if;

            Lang := To_Language_Or_All (Source.Language);
            if Lang not in Src_Supported_Language
              or else not Builtin_Support (Lang)
            then
               Outputs.Fatal_Error
                 ("Cannot instrument main source file (unsupported"
                  & " language): "
                  & F);
            end if;

            Mains_To_Instrument.Insert (Source);
         end;
      end loop;
   end if;

   --  Then, get the list of all units of interest

   for Lang in Src_Supported_Language loop
      if Src_Enabled_Languages (Lang) then
         Project.Enumerate_Sources
           (Add_Instrumented_Unit'Access, Lang, Mode => Only_UOI_Closures);
      end if;
   end loop;

   --  Remove all of the separate whose parent unit was not instrumented, as
   --  this is not supported. TODO??? We should probably issue a warning there.

   for Source of Files_Of_Interest_Info.Copy loop
      if Source.Has_Units and then First_Unit (Source).Kind = GPR2.S_Separate
      then
         declare
            Parent_Unit   :
              constant GPR2.Build.Compilation_Unit.Unit_Location :=
                Source.Owning_View.Namespace_Roots.First_Element.Unit
                  (Source.Unit.Name)
                  .Main_Body;
            Parent_File   : constant GPR2.Path_Name.Object :=
              Parent_Unit.Source;
            Parent_Source : constant GPR2.Build.Source.Object :=
              Parent_Unit.View.Source (Parent_File.Simple_Name);
         begin
            if not Files_Of_Interest_Info.Contains (Parent_Source) then
               Files_Of_Interest_Info.Delete (Source);
               Files_Of_Interest.Delete (Source.Path_Name.Virtual_File);
            end if;
         end;
      end if;
   end loop;

   --  Add the other project sources that needs to be instrumented, for main
   --  instrumentation or manual dump annotations replacement.

   for Main of Mains_To_Instrument loop
      Add_Project_Source (Main, Is_Main => True);
   end loop;

   if Dump_Config.Manual_Trigger then

      --  Iterate over all the project sources if the user did not specify
      --  manual indication files.

      if Dump_Config.Manual_Indication_Files.Is_Empty then
         for Source of Root_Project_Info.Project.Visible_Sources loop
            Add_Project_Source (Source, Is_Main => False);
         end loop;
      else
         for Source of Dump_Config.Manual_Indication_Files loop
            Add_Project_Source
              (Root_Project_Info.Project.Visible_Source
                 (GPR2.Path_Name.Create (Source)),
               Is_Main => False);
         end loop;
      end if;
   end if;

   --  For all units to instrument, determine the project that owns the source
   --  file that will be compiled. In the process, remove units for which we
   --  have found no spec and no body. TODO???  We should probably issue a
   --  warning there, too.

   declare
      To_Remove : String_Sets.Set;
   begin
      for Cur in Instrumented_Sources.Iterate loop
         declare
            Unit_Name : constant String := Unit_Maps.Key (Cur);
            LU_Info   : Library_Unit_Info renames
              Instrumented_Sources.Reference (Cur);
         begin
            if LU_Info.Body_Project.Is_Defined then
               LU_Info.Instr_Project := LU_Info.Body_Project;
            elsif LU_Info.Spec_Project.Is_Defined then
               LU_Info.Instr_Project := LU_Info.Spec_Project;
            else
               To_Remove.Insert (+Unit_Name);
            end if;

            declare
               Unit_Prj_Info   : Project_Info renames
                 Get_Or_Create_Project_Info (IC, LU_Info.Instr_Project).all;
               Source_Prj_Info : Project_Info_Access;
            begin
               for Source of LU_Info.Sources loop
                  if GPR2.Project.View."/="
                       (Source.Owning_View, LU_Info.Instr_Project)
                  then
                     Source_Prj_Info :=
                       Get_Or_Create_Project_Info (IC, Source.Owning_View);
                     Unit_Prj_Info.Desc.Special_Output_Dirs.Insert
                       (+String (Source.Path_Name.Simple_Name),
                        +Source_Prj_Info.Desc.Output_Dir.Display_Full_Name);
                  end if;
               end loop;
            end;
         end;
      end loop;

      for Unit_Name of To_Remove loop
         Instrumented_Sources.Delete (+Unit_Name);
      end loop;
   end;

   --  Check early if there are no sources of interest

   if Files_Of_Interest_Info.Length = 0 then
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

   IC.Ada_Default_Charset := +Default_Charset_From_Project (Project.Project);
   IC.Mapping_File :=
     +(+Root_Project_Info.Desc.Output_Dir.Full_Name) / ".ada-src-mapping";
   IC.Config_Pragmas_Mapping :=
     +(+Root_Project_Info.Desc.Output_Dir.Full_Name) / "config-pragmas.json";
   IC.Sources_Of_Interest_Response_File :=
     +(+Root_Project_Info.Desc.Output_Dir.Full_Name) / ".sources_of_interest";
   IC.Ada_Preprocessor_Data_File :=
     +(+Root_Project_Info.Desc.Output_Dir.Full_Name) / "prep-data.json";
   IC.Ada_Preprocessor_Data_File :=
     +(+Root_Project_Info.Desc.Output_Dir.Full_Name) / "instr-mapping.json";

   Instrument.Ada_Unit_Provider.Create_Mapping_File (+IC.Mapping_File);

   Instrument.Ada_Unit.Save_Config_Pragmas_Mapping
     (+IC.Config_Pragmas_Mapping);

   Instrument.Ada_Preprocessing.Create_Preprocessor_Data_File
     (+IC.Ada_Preprocessor_Data_File);

   --  Initialize the instrumenters: we will use them when parallelization is
   --  disabled, but also to generate the unit holding the list of buffers,
   --  in one of the supported languages.

   Ada_Instrumenter :=
     Create_Ada_Instrumenter
       (Default_Charset            => IC.Ada_Default_Charset,
        Tag                        => IC.Tag,
        Config_Pragmas_Mapping     => +IC.Config_Pragmas_Mapping,
        Mapping_Filename           => +IC.Mapping_File,
        Preprocessor_Data_Filename => +IC.Ada_Preprocessor_Data_File);
   C_Instrumenter :=
     Create_C_Instrumenter (IC.Tag, Project_Instrumentation, RTS_Source_Dirs);
   CPP_Instrumenter :=
     Create_CPP_Instrumenter
       (IC.Tag, Project_Instrumentation, RTS_Source_Dirs);

   --  Write the files of interest to a temporary file in the instrumentation
   --  directory.

   declare
      Sources_Of_Interest_File : Text_Files.File_Type;
      --  File containing the list of sources of interest

   begin
      Sources_Of_Interest_File.Create (+IC.Sources_Of_Interest_Response_File);
      for Source of Files_Of_Interest_Info loop
         Sources_Of_Interest_File.Put_Line (String (Source.Path_Name.Value));
      end loop;
      Sources_Of_Interest_File.Close;
   end;

   --  Instrument every unit of interest

   for Cur in Instrumented_Sources.Iterate loop
      declare
         LU_Info : constant Library_Unit_Info := Unit_Maps.Element (Cur);
         Prj     : constant GPR2.Project.View.Object := LU_Info.Instr_Project;

      begin
         if Serial_Execution then
            declare
               Inst_Action :
                 Instrument.Actions.Instrument_Source.Thread.Object;
            begin
               Inst_Action.Initialize
                 (LU_Info      => LU_Info,
                  IC           => IC'Unrestricted_Access,
                  Instrumenter => Instrumenters (LU_Info.Language),
                  Prj_Info     =>
                    IC.Project_Info_Map.Element (+String (Prj.Name)),
                  Dump_Config  => Dump_Config);
               if not Tree_Db.Add_Action (Inst_Action) then
                  raise Program_Error;
               end if;
            end;
         else
            declare
               Inst_Action :
                 Instrument.Actions.Instrument_Source.Process.Object;
            begin
               Inst_Action.Initialize
                 (LU_Info      => LU_Info,
                  IC           => IC'Unrestricted_Access,
                  Instrumenter => Instrumenters (LU_Info.Language),
                  Prj_Info     =>
                    IC.Project_Info_Map.Element (+String (Prj.Name)),
                  Dump_Config  => Dump_Config);
               if not Tree_Db.Add_Action (Inst_Action) then
                  raise Program_Error;
               end if;
            end;
         end if;
      end;
   end loop;

   --  Execute all of the actions

   if Serial_Execution then
      loop
         declare
            use GPR2.Build.Actions_Scheduler;
            Action_Rep : constant Action_Report :=
              Tree_Db.Execute_Next_Action
                (Catch_Exceptions => False, Force_Execution => Force);
         begin
            exit when Action_Rep.Status = No_Action_To_Execute;
         end;
      end loop;
   else
      declare
         Opt : constant GPR2.Build.Actions_Scheduler.Options :=
           (Force => Force, Keep_Temp_Files => Save_Temps, others => <>);
      begin
         case Tree_Db.Execute (Scheduler, Opt) is
            when GPR2.Build.Actions_Scheduler.Success =>
               null;

            when others                               =>
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
               raise Outputs.Xcov_Exit_Exc;
         end case;
      end;
   end if;

   --  If using the manual dump trigger: look for unit with dump annotations
   --  and emit a dump helper unit for them.

   if Dump_Config.Manual_Trigger then
      declare
         package Non_Root_Src_Calls_Sets is new
           Ada.Containers.Indefinite_Ordered_Sets (Element_Type => String);

         Non_Root_Src_Calls : Non_Root_Src_Calls_Sets.Set;
         --  Set of names of source files containing a dump buffers indication
         --  that belong to a non-root project when using the manual dump
         --  trigger.

         Emitted_Manual_Helpers : String_Sets.Set;
         --  Set of names of manual dump helper units that have been emitted
         --  thus far.

         Manual_Dump_Inserted : Boolean := False;
         --  Whether or not a dump procedure was inserted in any source file

      begin
         for Cur in Instrumented_Sources.Iterate loop
            declare
               Unit_Name    : constant String := Unit_Maps.Key (Cur);
               LU_Info      : constant Library_Unit_Info :=
                 Unit_Maps.Element (Cur);
               Instrumenter : constant Language_Instrumenter_Acc :=
                 Instrumenters (LU_Info.Language);
               Prj          : Prj_Desc renames
                 Get_Or_Create_Project_Info (IC, LU_Info.Instr_Project).Desc;
               Is_Root_Prj  : constant Boolean :=
                 LU_Info.Instr_Project.Is_Namespace_Root;

               Files_Instr_Info_JSON : constant JSON_Value :=
                 JSON.Read_File
                   (Files_Instrumentation_Info_File (Prj, Unit_Name)
                      .Display_Full_Name);
            begin
               --  Process dump indications

               for Filename_JSON of
                 JSON_Array'(Files_Instr_Info_JSON.Get ("dump_indications"))
               loop
                  Manual_Dump_Inserted := True;
                  if not Is_Root_Prj then

                     --  A call to the dump/reset buffers procedure is only
                     --  able to dump/reset the buffers of the project it is
                     --  in and its subprojects, meaning coverage data for
                     --  all projects higher in the project tree will be
                     --  missing or not reset. Record what file this call
                     --  was in to warn the user later.

                     Non_Root_Src_Calls.Include (Get (Filename_JSON));
                  end if;
               end loop;

               --  Same for reset indications

               for Filename_JSON of
                 JSON_Array'(Files_Instr_Info_JSON.Get ("reset_indications"))
               loop
                  if not Is_Root_Prj then
                     Non_Root_Src_Calls.Include (Get (Filename_JSON));
                  end if;
               end loop;

               --  Then, emit the dump helper unit and the buffers list unit
               --  for the project if needed.

               declare
                  Helper_Unit_Name : constant Unbounded_String :=
                    Instrumenter.Dump_Manual_Helper_Unit (Prj).Unit_Name;
               begin
                  if not Emitted_Manual_Helpers.Contains (Helper_Unit_Name)
                  then
                     Instrumenter.Emit_Dump_Helper_Unit_Manual
                       (Dump_Config, Prj);

                     --  The creation of the root project's buffers list
                     --  unit is already taken care of by the regular
                     --  instrumentation process, so skip it.

                     if not Is_Root_Prj then
                        Instrumenter.Emit_Buffers_List_Unit
                          (Instr_Units =>
                             Units_Of_Interest (IC, LU_Info.Instr_Project),
                           Prj         => Prj);
                     end if;
                  end if;
               end;
            end;
         end loop;

         if not Non_Root_Src_Calls.Is_Empty then

            --  For each manual dump/reset call inserted in a file belonging
            --  to a non-root project, warn the user the coverage data it
            --  will produce will not cover the whole project tree or may
            --  be inconsistent.

            declare
               All_File_Names : Unbounded_String;
            begin
               for File_Name of Non_Root_Src_Calls loop
                  US.Append (All_File_Names, File_Name);
                  US.Append (All_File_Names, ASCII.LF);
               end loop;

               Outputs.Warn
                 ("Manual buffer dump/reset indications were found in"
                  & " subprojects in the following files:"
                  & ASCII.LF
                  & (+All_File_Names)
                  & "The coverage report built from the source traces they"
                  & " will produce will show all code from projects higher in"
                  & " the project tree as not covered. To get a full coverage"
                  & " analysis, consider placing the manual dump buffers"
                  & " indication in the root project."
                  & ASCII.LF
                  & ASCII.LF
                  & "Additionally, resetting the buffers in a subproject may"
                  & " result in incoherent coverage reports from traces dumped"
                  & " from a source in a parent project.");
            end;
         end if;

         if not Manual_Dump_Inserted then
            Outputs.Warn
              ("no indication for dump location was found, this might be"
               & " caused by a misspelling in the expected pragma"
               & " statement or comment.");
         end if;
      end;
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
      Langs : constant GPR2.Containers.Language_Set :=
        Root_Project_Info.Project.Language_Ids;
   begin
      for Lang in Src_Supported_Language loop
         if Builtin_Support (Lang)
           and then Langs.Contains (To_Language_Id (Lang))
         then
            Instrumenters (Lang).Emit_Buffers_List_Unit
              (Units_Of_Interest (IC, Root_Project_Info.Project),
               Root_Project_Info.Desc);
            Instrumenters (Lang).Emit_Observability_Unit
              (Root_Project_Info.Desc);
            exit;
         end if;
      end loop;
   end;

   --  Delete instrumentation artifacts from previous instrumentations

   Clean_Objdirs (IC, Instrumented_Sources, Preserve_Artifacts => True);

   Destroy_Context (IC);

   --  Save the dump trigger+channel information in the root project's
   --  object directory. This allows user scripts to automatically know
   --  where to expect source trace files (dump channel) without inspecting
   --  all inputs (command-line arguments, project file, instrumentation
   --  runtime, etc.) and whether that info is reliable (it is not if the
   --  dump trigger is manual).
   --
   --  TODO: this should go at some point in Instrument.Source (in which case
   --  one would be generated per main).

   declare
      J        : constant GNATCOLL.JSON.JSON_Value := Create_Object;
      Filename : constant String := Project.Output_Dir & "/gnatcov-instr.json";
   begin
      J.Set_Field ("manual-dump-trigger", Dump_Config.Manual_Trigger);
      if Dump_Config.Auto_Trigger /= None then
         J.Set_Field ("auto-dump-trigger", Image (Dump_Config.Auto_Trigger));
      end if;
      J.Set_Field ("dump-channel", Image (Dump_Config.Channel));
      Write (Filename, J, Compact => False);
   end;

exception
   --  We need to clear the object directories in case an exception (caught
   --  by a global handler and not dealt with by the registered hook) was
   --  raised.

   when
     Binary_Files.Error | Ada.IO_Exceptions.Name_Error | Outputs.Xcov_Exit_Exc
   =>
      if not Save_Temps then
         Clean_Objdirs
           (IC,
            Instrumented_Sources,

            --  Do not preserve instrumentation artifacts on an exception
            --  termination.

            Preserve_Artifacts => False,
            Keep_Going         => True);
         raise;
      end if;
end Instrument.Projects;
