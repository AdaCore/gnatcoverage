------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2012-2024, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories; use Ada.Directories;

with GNAT.OS_Lib;
with GNAT.Regexp;

with GNATCOLL.Traces;
with GNATCOLL.VFS; use GNATCOLL.VFS;

--  ??? Remove pragma Warnings once eng/toolchain/gnat#1283 is fixed

pragma Warnings (Off, "not referenced");
with GPR2.Build.Source.Sets;
pragma Warnings (On, "not referenced");
with GPR2.Message;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack.Description;
with GPR2.Project.Registry.Pack;
with GPR2.Reporter.Console;

with Inputs;     use Inputs;
with Instrument; use Instrument;
with Outputs;    use Outputs;
with Paths;      use Paths;
with Traces_Source;

package body Project is

   use type Ada.Containers.Count_Type;
   use type Unbounded_String;

   use type GPR2.Filename_Type;
   use type GPR2.Language_Id;
   use type GPR2.Path_Name.Object;
   use type GPR2.Project.View.Object;
   use type GPR2.Project_Kind;
   use type GPR2.Unit_Kind;

   subtype Compilation_Unit is Files_Table.Compilation_Unit;
   use type Compilation_Unit;

   use type Traces_Source.Supported_Language_Kind;

   Project_File_Extension : constant String := ".gpr";

   Coverage_Package      : constant String := "coverage";
   GPR2_Coverage_Package : constant GPR2.Package_Id :=
     GPR2."+" (GPR2.Name_Type (Coverage_Package));

   type Attribute is
     (Units,
      Excluded_Units,
      Routines,
      Excluded_Routines,
      Ignored_Source_Files,
      Switches,

      Units_List,
      Excluded_Units_List,
      Routines_List,
      Excluded_Routines_List,
      Ignored_Source_Files_List);

   subtype List_Attribute is
     Attribute range Units .. Switches;
   subtype String_Attribute is
     Attribute range Units_List .. Ignored_Source_Files_List;

   function "+" (A : Attribute) return String;
   function "+" (A : Attribute) return GPR2.Q_Attribute_Id;
   --  Build identifiers for attributes in package Coverage

   procedure Iterate_Source_Files
     (Root_Project : GPR2.Project.View.Object;
      Process      : access procedure
        (Source : GPR2.Build.Source.Object; Unit_Name : String);
      Recursive    : Boolean);
   --  Call Process on all source files in Root_Project (recursively
   --  considering source files of sub-projects if Recursive is true).
   --
   --  This passes the name of the unit as Unit_Name for languages featuring
   --  this notion (Ada) and the base file name otherwise (i.e. for C sources).

   function SCO_Filename
     (Source : GPR2.Build.Source.Object;
      Kind   : Trace_File_Kind)
      return GPR2.Path_Name.Object;
   --  Return the SCO absolute filename corresponding to the given source file
   --  and the given trace kind (.ali/.gli for binary traces, .sid for source
   --  traces). Return ``Undefined`` if none can be found.

   Prj_Tree : GPR2.Project.Tree.Object;
   --  Loaded project tree

   Obj_Subdir : Unbounded_String;
   --  Hold the object subdirectory to use (if any) for all loaded projects.
   --  Should be processed each time we load a project tree.

   Build_Tree_Dir : Virtual_File;
   --  The root of the build tree for the loaded projects

   Root_Dir : Virtual_File;
   --  The root dir of the project tree, to consider when Build_Root_Dir is
   --  set.

   type Project_Info is record
      Project : GPR2.Project.View.Object;
      --  The project this info relates to

      Has_Units_Of_Interest : Boolean;
      --  Whether this project contributed to the selection of units of
      --  interest. They do contribute either:
      --
      --    * having unit selection attributes or containing units of interest
      --      (if no --units argument is passed)
      --
      --    * having at least one unit of interest (as soon as one --units
      --      argument is passed).
   end record;

   package Project_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_type => String);
   package Project_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Project_Info);

   Requested_Projects : Project_Sets.Set;
   --  Set of all projects passed to the Add_Project procedure

   Prj_Map : Project_Maps.Map;
   --  Map of all projects for which coverage analysis is desired. Populated in
   --  Build_Prj_Map, called from Load_Root_Project.

   type Unit_Info is record
      Original_Name : Unbounded_String;
      --  Units are referenced in unit maps under their lowercased name.
      --  Here we record the name with original casing (from the project or
      --  the command line).

      Present_In_Projects : Boolean;
      --  Whether we found at least one source file in the projects of interest
      --  that matches this unit.

      Language : Some_Language;
      --  Language for this unit

      LI_Seen : Boolean;
      --  Set true if the LI file for this unit has been seen

      Warned_About_Missing_Info : Boolean;
      --  Whether Warn_Missing_Info was called for this unit
   end record;

   package Unit_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => Compilation_Unit,
      Element_Type => Unit_Info);

   procedure Add_Unit
     (Units          : in out Unit_Maps.Map;
      Cur            : out Unit_Maps.Cursor;
      Original_Name  : String;
      Source         : GPR2.Build.Source.Object;
      Language       : Some_Language);
   --  Add a Unit_Info entry to Units. The key for this new entry (which is a
   --  Compilation_Unit) is computed using the Original_Name, and Source's
   --  project name if the unit is of a file-based language.

   procedure Warn_Missing_Info (What_Info : String; Unit : in out Unit_Info);
   --  If we haven't already, warn that we miss information (ALI or SID) about
   --  Unit.

   Unit_Map : Unit_Maps.Map;
   --  Map lower-case unit names to Unit_Info records for all units of
   --  interest. This map contains header files (C/C++) but does not contain
   --  separate (Ada) units.

   function Lookup_Project (Prj_Name : String) return GPR2.Project.View.Object;
   --  Look for the project in Prj_Tree whose name matches Prj_Name and return
   --  it. Emit a fatal error if there is no such project.

   procedure Build_Prj_Map with Pre => Is_Project_Loaded;
   --  Add entries in Prj_Map for all relevant projects

   procedure Build_Unit_Map (Override_Units : String_Vectors.Vector);
   --  Add entries in Unit_Map for all units of interest

   procedure List_From_Project
     (Prj            : GPR2.Project.View.Object;
      List_Attr      : List_Attribute;
      List_File_Attr : String_Attribute;
      Process_Item   : access procedure (Attr, Item : String);
      Defined        : out Boolean);
   --  If List_Attr, an attribute that can contain a list of strings, is
   --  defined in Prj, call Process_Item on each of the strings and set
   --  Defined. If List_File_Attr, an attribute that can contain a file name,
   --  is defined in Prj, read each line from the file and call Process_Item on
   --  each line, then set Defined. Just reset Defined if no attribute is
   --  defined.

   procedure Units_From_Project
     (Prj            : GPR2.Project.View.Object;
      List_Attr      : List_Attribute;
      List_File_Attr : String_Attribute;
      Units          : out Unit_Maps.Map;
      Defined        : out Boolean);
   --  Build a map of units from project attributes.
   --
   --  This considers that List_Attr is an attribute that can contain a list of
   --  globbing patterns for unit names and that List_File_Attr is an attribute
   --  that can contain a file name for a text file, and that this file
   --  contains one globbing pattern for unit names per line.
   --
   --  If Prj contains at least one of these attributes, set Defined to True
   --  and fill Units with one entry per unit. Set Defined to False otherwise.

   function Enumerate_Mains
     (Root_Project : GPR2.Project.View.Object;
      Language     : Any_Language)
      return GPR2.Build.Compilation_Unit.Unit_Location_Vector;
   --  Return the list of all main source files recursively found in the
   --  Root_Project for the given Language, or for all languages if Language is
   --  All_Languages.
   --
   --  Note that this also returns source files for mains that are not units of
   --  interest.

   type Reporter is new GPR2.Reporter.Object with record
      Inner : GPR2.Reporter.Console.Object;
   end record;

   overriding function Verbosity
     (Self : Reporter) return GPR2.Reporter.Verbosity_Level;

   overriding procedure Internal_Report
     (Self : in out Reporter; Message : GPR2.Message.Object);

   -------------------
   -- Get_Unit_Name --
   -------------------

   function Get_Unit_Name (Source : GPR2.Build.Source.Object) return String is
   begin
      return (if Source.Has_Units
              then To_Lower (String (First_Unit (Source).Name))
              else String (Source.Path_Name.Simple_Name));
   end Get_Unit_Name;

   -------------------------
   -- To_Compilation_Unit --
   -------------------------

   function To_Compilation_Unit
     (Source : GPR2.Build.Source.Object) return Files_Table.Compilation_Unit
   is
      Language : constant Some_Language := To_Language (Source.Language);
      U        : Compilation_Unit;
   begin
      U.Language := Language_Kind (Language);
      case U.Language is
         when Traces_Source.File_Based_Language =>
            U.Unit_Name := +String (Source.Path_Name.Value);
         when Traces_Source.Unit_Based_Language =>
            U.Unit_Name := +To_Lower (String (First_Unit (Source).Name));
      end case;
      return U;
   end To_Compilation_Unit;

   ---------
   -- "+" --
   ---------

   function "+" (A : Attribute) return String is
   begin
      return Coverage_Package & "." & To_Lower (A'Image);
   end "+";

   function "+" (A : Attribute) return GPR2.Q_Attribute_Id is
   begin
      return (GPR2_Coverage_Package, GPR2."+" (GPR2.Name_Type (A'Image)));
   end "+";

   --------------
   -- Add_Unit --
   --------------

   procedure Add_Unit
     (Units          : in out Unit_Maps.Map;
      Cur            : out Unit_Maps.Cursor;
      Original_Name  : String;
      Source         : GPR2.Build.Source.Object;
      Language       : Some_Language)
   is
      Orig_Name : constant Unbounded_String :=
        +Fold_Filename_Casing (Original_Name);
      Unit_Name : constant Compilation_Unit := To_Compilation_Unit (Source);

      Ignored_Inserted : Boolean;
      Is_Header        : Boolean := False;
   begin
      --  Disable warnings for header files as they do not have a corresponding
      --  library file.

      if Language in C_Family_Language and then Source.Kind = GPR2.S_Spec then
         Is_Header := True;
      end if;

      Units.Insert
        (Key      => Unit_Name,
         New_Item => (Original_Name             => Orig_Name,
                      Present_In_Projects       => False,
                      Language                  => Language,
                      LI_Seen                   => Is_Header,
                      Warned_About_Missing_Info => Is_Header),
         Position => Cur,
         Inserted => Ignored_Inserted);
   end Add_Unit;

   -----------------------
   -- Warn_Missing_Info --
   -----------------------

   procedure Warn_Missing_Info (What_Info : String; Unit : in out Unit_Info) is
   begin
      --  The only way to perform code coverage on C++ units is to use
      --  instrumentation. With binary traces, C++ units cannot be units of
      --  interest, so do not warn about them in this specific case.

      if Unit.Warned_About_Missing_Info
         or else (Currently_Accepted_Trace_Kind = Binary_Trace_File
                  and then Unit.Language = CPP_Language)

        --  Ignore units in a language that is ignored through the
        --  --restricted-to-languages switch.

        or else not Src_Enabled_Languages (Unit.Language)
      then
         return;
      end if;

      Warn ("no " & What_Info & " file found for unit "
            & (+Unit.Original_Name));
      Unit.Warned_About_Missing_Info := True;
   end Warn_Missing_Info;

   ----------------------
   -- Iterate_Projects --
   ----------------------

   procedure Iterate_Projects
     (Root_Project             : GPR2.Project.View.Object;
      Process                  : access procedure
                                   (Prj : GPR2.Project.View.Object);
      Recursive                : Boolean;
      Include_Extended         : Boolean := False;
      Include_Externally_Built : Boolean :=
        Externally_Built_Projects_Processing_Enabled)
   is
      procedure Process_Candidate (Project : GPR2.Project.View.Object);
      --  Process Project, one project in the closure of Root_Project,
      --  according to the requested iteration settings.

      Visited_Projects : Project_Sets.Set;
      --  Set of already visited projects, used to make sure each project is
      --  visited at most once.

      -----------------------
      -- Process_Candidate --
      -----------------------

      procedure Process_Candidate (Project : GPR2.Project.View.Object) is
         P : GPR2.Project.View.Object := Project;
      begin
         --  We never want to consider the runtime project as a project of
         --  interest. As a subtle nuance, it is still possible to do coverage
         --  on runtime sources, but in this case the corresponding project
         --  must be loaded the usual way (i.e. not as a runtime project:
         --  --RTS, "for Runtime", ...).

         if Project.Is_Runtime then
            return;
         end if;

         --  If requested, go to the ultimate extending project: this is the
         --  "reference" project for chains of project extension (we care about
         --  the Coverage package of extending projects, their object dirs,
         --  etc.). Otherwise, also process extended projects.

         if not Include_Extended then
            P := Most_Extending (P);
         elsif P.Is_Extending then
            for EP of P.Extended loop
               Process_Candidate (EP);
            end loop;
         end if;

         declare
            Name : constant String := String (P.Name);
         begin
            --  Skip externally built projects unless they are explicitly
            --  requested.

            if (Include_Externally_Built
                or else not P.Is_Externally_Built)
               and then not Visited_Projects.Contains (Name)
            then
               Process (P);
               Visited_Projects.Insert (Name);
            end if;
         end;
      end Process_Candidate;

   --  Start of processing for Iterate_Projects

   begin
      if Recursive then
         for Project of Root_Project.Closure (Include_Self => True) loop
            Process_Candidate (Project);
         end loop;
      else
         Process_Candidate (Root_Project);
      end if;
   end Iterate_Projects;

   --------------------------
   -- Iterate_Source_Files --
   --------------------------

   procedure Iterate_Source_Files
     (Root_Project : GPR2.Project.View.Object;
      Process      : access procedure
        (Source : GPR2.Build.Source.Object; Unit_Name : String);
      Recursive    : Boolean)
   is
      procedure Process_Project (Prj : GPR2.Project.View.Object);
      --  Invoke Process on relevant sources in Prj

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project (Prj : GPR2.Project.View.Object) is
      begin
         for Source of Prj.Sources loop

            --  Process only source files in supported languages (Ada, C and
            --  C++). Never include Ada's "No_Body" units, as we cannot do
            --  anything useful with them, and GPR2 pretends they do not belong
            --  to any unit.

            if Source.Language in
                 GPR2.Ada_Language | GPR2.C_Language | GPR2.CPP_Language
               and then (not Source.Has_Units
                         or else First_Unit (Source).Kind /= GPR2.S_No_Body)
            then
               Process.all (Source, Get_Unit_Name (Source));
            end if;
         end loop;
      end Process_Project;

   --  Start of processing for Iterate_Source_Files

   begin
      --  Iterate on all the projects implied by our arguments: Root_Project,
      --  possibly with its closure.

      Iterate_Projects
        (Root_Project     => Root_Project,
         Process          => Process_Project'Access,
         Recursive        => Recursive);
   end Iterate_Source_Files;

   -----------------
   -- Add_Project --
   -----------------

   procedure Add_Project (Prj_Name : String) is
   begin
      Requested_Projects.Include (Prj_Name);
   end Add_Project;

   ---------------------------------
   -- Enumerate_Units_Of_Interest --
   ---------------------------------

   procedure Enumerate_Units_Of_Interest
     (Callback : access procedure (Name : Files_Table.Compilation_Unit))
   is
      use Unit_Maps;
   begin
      for Cur in Unit_Map.Iterate loop
         Callback (Key (Cur));
      end loop;
   end Enumerate_Units_Of_Interest;

   ------------------
   -- SCO_Filename --
   ------------------

   function SCO_Filename
     (Source : GPR2.Build.Source.Object;
      Kind   : Trace_File_Kind)
      return GPR2.Path_Name.Object
   is
      Is_Ada    : constant Boolean := Source.Language = GPR2.Ada_Language;
      Prefix    : GPR2.Simple_Name :=
        (if Is_Ada
         then Source.Path_Name.Base_Filename
         else Source.Path_Name.Simple_Name);
      Extension : constant GPR2.Simple_Name :=
        (case Kind is
         when Binary_Trace_File => (if Is_Ada
                                    then ".ali"
                                    else ".gli"),
         when Source_Trace_File => ".sid");
      Basename : constant GPR2.Simple_Name := Prefix & Extension;

      --  Depending on whether this unit belongs to a library project and
      --  whether it was installed, the SID file can be in the library
      --  directory or in the object directory: check the library directory and
      --  fallback to the object directory.

      View   : constant GPR2.Project.View.Object := Source.Owning_View;
      Result : GPR2.Path_Name.Object;
   begin
      if View.Is_Library then
         Result := View.Library_Directory.Compose (Basename);
         if Result.Exists then
            return Result;
         end if;
      end if;

      Result := View.Object_Directory.Compose (Basename);
      if Result.Exists then
         return Result;
      end if;

      return GPR2.Path_Name.Undefined;
   end SCO_Filename;

   -------------------------
   -- Enumerate_SCO_Files --
   -------------------------

   procedure Enumerate_SCOs_Files
     (Callback : access procedure (Lib_Name : String);
      Kind     : Trace_File_Kind)
   is
      Files_Found : String_Sets.Set;
   begin
      --  Go through all sources in all projects of interest

      for Prj_Info of Prj_Map loop
         for Source of Prj_Info.Project.Sources loop

            --  Skip sources for languages unknown to gnatcov. Also skip
            --  No_Body Ada unit, which cannot have SCOs by themselves, and
            --  GPR2 treats them as having no unit name, so we need to
            --  explicitly ignore them.

            if To_Language_Or_All (Source.Language) /= All_Languages
               and then (not Source.Has_Units
                         or else First_Unit (Source).Kind /= GPR2.S_No_Body)
            then
               declare
                  use Unit_Maps;
                  Cur : constant Cursor :=
                    Unit_Map.Find (To_Compilation_Unit (Source));
               begin
                  if Has_Element (Cur) then

                     --  If Sources belongs to a unit of interest, look for its
                     --  info file (ALI/SID).

                     declare
                        Info_File  : constant GPR2.Path_Name.Object :=
                          SCO_Filename (Source, Kind);
                        Unused_Cur : String_Sets.Cursor;
                        Inserted   : Boolean;
                     begin
                        if Info_File.Is_Defined then

                           --  The same SCO file may cover multiple source
                           --  files: invoke the callback only once per SCO
                           --  file.

                           declare
                              Filename : constant String :=
                                String (Info_File.Value);
                           begin
                              Files_Found.Insert
                                (+Filename, Unused_Cur, Inserted);
                              if Inserted then
                                 Callback.all (Filename);
                                 Unit_Map.Reference (Cur).LI_Seen := True;
                              end if;
                           end;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end loop;
      end loop;

      --  Now warn about units of interest that have no SCOs file

      for UI of Unit_Map loop
         if not UI.LI_Seen then
            case Kind is
               when Binary_Trace_File =>
                  Warn_Missing_Info ("ALI", UI);
               when Source_Trace_File =>
                  Warn_Missing_Info ("SID", UI);
            end case;
         end if;
      end loop;
   end Enumerate_SCOs_Files;

   -----------------------
   -- Enumerate_Sources --
   -----------------------

   procedure Enumerate_Sources
     (Callback  : access procedure
        (Project : GPR2.Project.View.Object;
         File    : GPR2.Build.Source.Object);
      Language  : Any_Language;
      Only_UOIs : Boolean := False)
   is
      procedure Process_Source_File
        (Source : GPR2.Build.Source.Object; Unit_Name : String);
      --  Callback for Iterate_Source_File. If Only_UOIs is set to true, call
      --  Callback the Unit_Name file is it a unit of interest. If Only_UOIs is
      --  set to False, call Callback on all sources.

      -------------------------
      -- Process_Source_File --
      -------------------------

      procedure Process_Source_File
        (Source : GPR2.Build.Source.Object; Unit_Name : String)
      is
         pragma Unreferenced (Unit_Name);
         Info_Lang : constant Some_Language :=
           To_Language_Or_All (Source.Language);
      begin
         --  If only sources for a specific language are requested and this
         --  is not the language for Source, skip it.

         if Language /= All_Languages and then Language /= Info_Lang then
            return;
         end if;

         --  If this is a header, consider it a unit of interest. For now, they
         --  can only be ignored through the --ignore-source-files switch.

         if (not Source.Has_Units and then Source.Kind = GPR2.S_Spec)

           --  Otherwise, check if the unit is in the units of interest map

           or else not Only_UOIs
           or else Unit_Map.Contains (To_Compilation_Unit (Source))
         then
            Callback (Source.Owning_View, Source);
         end if;
      end Process_Source_File;

   begin
      --  Go through all sources files in all projects of interest

      for Prj_Info of Prj_Map loop
         Iterate_Source_Files
           (Prj_Info.Project,
            Process_Source_File'Access,
            Recursive     => False);
      end loop;
   end Enumerate_Sources;

   ----------------------
   -- Find_Source_File --
   ----------------------

   function Find_Source_File (Simple_Name : String)
                              return GNAT.Strings.String_Access
   is
   begin
      if not Prj_Tree.Is_Defined then
         return null;
      end if;

      declare
         Result : constant GPR2.Build.Source.Object :=
           Prj_Tree.Root_Project.Visible_Source
             (GPR2.Simple_Name (Simple_Name));
      begin
         return (if Result.Is_Defined
                 then new String'(String (Result.Path_Name.Value))
                 else null);
      end;
   end Find_Source_File;

   --------------------
   -- Lookup_Project --
   --------------------

   function Lookup_Project (Prj_Name : String) return GPR2.Project.View.Object
   is
      Lower_Basename : constant String := To_Lower (Simple_Name (Prj_Name));
      Result         : GPR2.Project.View.Object;
   begin
      declare
         --  Strip optional Project_File_Extension

         Name : constant String :=
           (if Has_Suffix (Lower_Basename, Project_File_Extension)
            then Lower_Basename
                   (Lower_Basename'First
                    .. Lower_Basename'Last - Project_File_Extension'Length)
            else Lower_Basename);
      begin
         --  Look for a view in the project tree whose name matches the request

         for P of Prj_Tree loop
            if To_Lower (String (P.Name)) = Name then
               Result := P;
               exit;
            end if;
         end loop;
      end;

      if Result.Is_Defined then
         return Result;
      else
         Fatal_Error ("project " & Prj_Name & " not found");
      end if;
   end Lookup_Project;

   -------------------
   -- Build_Prj_Map --
   -------------------

   procedure Build_Prj_Map is

      procedure Process_Project (Project : GPR2.Project.View.Object);
      --  Callback for Iterate_Projects: add an entry in Prj_Map for Project

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project (Project : GPR2.Project.View.Object) is
         use Project_Maps;

         Success : Boolean;
         Cur     : Cursor;
      begin
         --  In recursive mode or when users pass the same project several
         --  times to --projects, Process_Project can be called on the same
         --  project multiple times. In theory, all projects are supposed to
         --  have a unique name, though: check this.

         Prj_Map.Insert
           (String (Project.Name),
            (Project => Project, others => False),
            Cur,
            Success);
         if not Success and then Prj_Map.Reference (Cur).Project /= Project
         then
            raise Program_Error with
               "homonym projects detected: " & String (Project.Name);
         end if;
      end Process_Project;

   --  Start of processing for Build_Prj_Map

   begin
      --  If no project was specified, consider the root one. If this root
      --  project has an Origin_Project attribute, actually use this project
      --  instead.

      if Requested_Projects.Is_Empty then
         declare
            Origin_Attr : constant GPR2.Project.Attribute.Object :=
              Prj_Tree.Root_Project.Attribute
                (GPR2.Project.Registry.Attribute.Origin_Project);
            Prj_Name    : constant String :=
              (if Origin_Attr.Is_Defined
               then Origin_Attr.Value.Text
               else String (Prj_Tree.Root_Project.Name));
         begin
            Requested_Projects.Insert (Prj_Name);
         end;
      end if;

      for Prj_Name of Requested_Projects loop
         Iterate_Projects
           (Most_Extending (Lookup_Project (Prj_Name)),
            Process_Project'Access,
            Standard.Switches.Recursive_Projects);
      end loop;
   end Build_Prj_Map;

   --------------------
   -- Build_Unit_Map --
   --------------------

   procedure Build_Unit_Map (Override_Units : String_Vectors.Vector) is
      Units_Specified : constant Boolean := not Override_Units.Is_Empty;
      --  Whether the user requested a specific set of units of interest
      --  through the --units command-line argument.

      Unit_Patterns : String_Vectors.Vector;
      --  Lower-cased version of Override_Units

      Units_Specified_Matcher : GNAT.Regexp.Regexp;
      Has_Matcher             : Boolean;
      --  Matcher for the list of units of interest

      procedure Process_Project (Project : GPR2.Project.View.Object);
      --  Compute the list of units of interest in Project and call
      --  Enumerate_In_Single_Projects for Project.

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project (Project : GPR2.Project.View.Object) is
         Inc_Units         : Unit_Maps.Map;
         Inc_Units_Defined : Boolean := False;
         --  Units to be included, as specified in project

         Exc_Units : Unit_Maps.Map;
         --  Units to be excluded, as specified in project

         Has_Unit_Selection_Attributes : Boolean := False;
         --  Whether this project defines attributes to select units of
         --  interest.

         Has_One_Unit_Of_Interest : Boolean := False;
         --  Whether this project has at least one unit of interest
      begin
         --  Unless the set of units of interest is overriden by --units, go
         --  through all units, taking into account Units and Excluded_Units
         --  attributes in the Coverage project package.

         if not Units_Specified then

            Units_From_Project
              (Project,
               List_Attr      => Units,
               List_File_Attr => Units_List,
               Units          => Inc_Units,
               Defined        => Inc_Units_Defined);

            declare
               Exc_Units_Defined : Boolean;
            begin
               Units_From_Project
                 (Project,
                  List_Attr       => Excluded_Units,
                  List_File_Attr  => Excluded_Units_List,
                  Units           => Exc_Units,
                  Defined         => Exc_Units_Defined);

               Has_Unit_Selection_Attributes :=
                  Inc_Units_Defined or else Exc_Units_Defined;
            end;
         end if;

         --  If Project does not specify units of interest thanks to project
         --  attributes, assume that all of its units are of interest.

         if not Inc_Units_Defined then
            declare
               procedure Process_Source_File
                 (Source : GPR2.Build.Source.Object; Unit_Name : String);
               --  Add Unit_Name to Inc_Units

               -------------------------
               -- Process_Source_File --
               -------------------------

               procedure Process_Source_File
                 (Source : GPR2.Build.Source.Object; Unit_Name : String)
               is
                  Cur : Unit_Maps.Cursor;
               begin
                  --  Never try to perform coverage on our coverage runtime
                  --  library (in one of the gnatcov_rts*.gpr projects) or on
                  --  our coverage buffer units (in user projects).

                  if Strings.Has_Prefix (Unit_Name, "gnatcov_rts.")
                    or else Strings.Has_Prefix (Unit_Name, "gcvrt")
                  then
                     return;
                  end if;

                  if not Units_Specified
                     or else (Has_Matcher
                              and then GNAT.Regexp.Match
                                (To_Lower (Unit_Name),
                                 Units_Specified_Matcher))
                  then
                     Add_Unit
                       (Inc_Units,
                        Cur,
                        Unit_Name,
                        Source,
                        To_Language (Source.Language));
                  end if;
               end Process_Source_File;

            begin
               --  Do not go through imported projects; the Excluded_Units /
               --  Units attributes only apply to the project itself.

               Iterate_Source_Files
                 (Project, Process_Source_File'Access, Recursive => False);
               Inc_Units_Defined := True;
            end;
         end if;

         --  Register as units of interest all units in Inc_Units which are not
         --  in Exc_Units.

         for Cur in Inc_Units.Iterate loop
            declare
               use Unit_Maps;
               K : constant Compilation_Unit := Key (Cur);
            begin
               if not Exc_Units.Contains (K) then
                  declare
                     Info : Unit_Info := Element (Cur);
                  begin
                     Has_One_Unit_Of_Interest := True;
                     Info.Present_In_Projects := True;
                     Unit_Map.Include (K, Info);
                  end;
               end if;
            end;
         end loop;

         --  Compute whether this project contributes to the selection of units
         --  of interest.

         Prj_Map.Reference (String (Project.Name)).Has_Units_Of_Interest :=
           (Has_One_Unit_Of_Interest
            or else (not Units_Specified
                     and then Has_Unit_Selection_Attributes));
      end Process_Project;

   --  Start of processing for Build_Unit_Map

   begin
      --  First, lower case all the units / patterns specified on the command
      --  line.

      for Pattern of Override_Units loop
         Unit_Patterns.Append (+To_Lower (+Pattern));
      end loop;

      --  Then, create a regexp matching all the patterns specified.
      --  Regardless of the current platform, the casing of unit names is not
      --  significant: the Foo unit is the same as the foo unit.

      Create_Matcher (Pattern_List     => Unit_Patterns,
                      Matcher          => Units_Specified_Matcher,
                      Has_Matcher      => Has_Matcher,
                      Case_Insensitive => True);

      --  Now go through all selected projects to find units of interest

      for Prj_Info of Prj_Map loop
         Process_Project (Prj_Info.Project);
      end loop;

      --  Compute the coverage of patterns specified with --units to warn if
      --  some patterns didn't match any unit.

      if Units_Specified then
         declare
            Units_Present : String_Vectors.Vector;
            --  All units that were included

            Patterns_Not_Covered : String_Sets.Set :=
              To_String_Set (Unit_Patterns);
            --  Patterns that do not match any of the present units

            procedure Add_To_Unit_Presents (C : Unit_Maps.Cursor);

            --------------------------
            -- Add_To_Unit_Presents --
            --------------------------

            procedure Add_To_Unit_Presents (C : Unit_Maps.Cursor)
            is
               P_Unit    : constant Compilation_Unit := Unit_Maps.Key (C);
               Unit_Name : constant Unbounded_String :=
                 (case P_Unit.Language is
                  when Traces_Source.File_Based_Language =>
                    +Ada.Directories.Simple_Name (+P_Unit.Unit_Name),
                  when Traces_Source.Unit_Based_Language => P_Unit.Unit_Name);
            begin
               Units_Present.Append (Unit_Name);
            end Add_To_Unit_Presents;

         begin
            Unit_Map.Iterate (Add_To_Unit_Presents'Access);
            Match_Pattern_List (Patterns_List        => Unit_Patterns,
                                Strings_List         => Units_Present,
                                Patterns_Not_Covered =>
                                  Patterns_Not_Covered);
            for Pattern of Patterns_Not_Covered loop
               Warn ("no unit " & (+Pattern) & " (from --units) in the"
                     & " projects of interest");
            end loop;
         end;
      end if;

      if Unit_Map.Is_Empty then
         Warn ("no unit of interest");
      end if;

      --  Warn about projects of interest that don't contribute to the
      --  selection of units of interest. Do this only in non-recursive mode as
      --  --recursive selects too many projects to make it practical for users
      --  to select only the projects that provide units of interest: the goal
      --  of this warning is to help users, pointing at "useless" projects.

      if not Standard.Switches.Recursive_Projects then
         for Prj_Info of Prj_Map loop
            if not Prj_Info.Has_Units_Of_Interest then
               Warn ("project " & String (Prj_Info.Project.Name)
                     & " provides no unit of interest");
            end if;
         end loop;
      end if;
   end Build_Unit_Map;

   -----------------------
   -- List_From_Project --
   -----------------------

   procedure List_From_Project
     (Prj            : GPR2.Project.View.Object;
      List_Attr      : List_Attribute;
      List_File_Attr : String_Attribute;
      Process_Item   : access procedure (Attr, Item : String);
      Defined        : out Boolean)
   is
      LA  : constant GPR2.Q_Attribute_Id := +List_Attr;
      LFA : constant GPR2.Q_Attribute_Id := +List_File_Attr;

      A : GPR2.Project.Attribute.Object;
   begin
      --  We check each attribute in sequence and the set we're filling
      --  is "defined" as soon as one is set explicitly.

      Defined := False;

      A := Prj.Attribute (LA);
      if A.Is_Defined then
         Defined := True;
         declare
            Attr : constant String := +List_Attr;
         begin
            for Value of A.Values loop
               Process_Item (Attr, Value.Text);
            end loop;
         end;
      end if;

      A := Prj.Attribute (LFA);
      if A.Is_Defined then
         Defined := True;
         declare
            Attr : constant String := +List_File_Attr;

            procedure Process_Item_Wrapper (Item : String);
            --  Wrapper around Process_Item for each file item

            --------------------------
            -- Process_Item_Wrapper --
            --------------------------

            procedure Process_Item_Wrapper (Item : String) is
            begin
               Process_Item.all (Attr, Item);
            end Process_Item_Wrapper;

         begin
            Read_List_From_File
              (+Full_Name
                 (Create_From_Base
                    (Base_Name => +A.Value.Text,
                     Base_Dir  => +String (Prj.Dir_Name.Value))),
               Process_Item_Wrapper'Access);
         end;
      end if;
   end List_From_Project;

   ------------------------
   -- Units_From_Project --
   ------------------------

   procedure Units_From_Project
     (Prj            : GPR2.Project.View.Object;
      List_Attr      : List_Attribute;
      List_File_Attr : String_Attribute;
      Units          : out Unit_Maps.Map;
      Defined        : out Boolean)
   is
      Unit_Patterns    : String_Vectors.Vector;
      Attr_For_Pattern : String_Maps.Map;
      --  Patterns identifying unit names and project attribute from which we
      --  got the pattern. We use Attr_For_Pattern for reporting purposes.

      Patterns_Not_Covered : String_Sets.Set;
      --  Patterns that do not match any of the present unit

      Cur : Unit_Maps.Cursor;

      procedure Add_Pattern (Attr, Item : String);
      --  Add Item to Unit_Patterns. Also save from which project attribute
      --  (Attr) the pattern comes from in Attr_For_Pattern.

      procedure Process_Source_File
        (Source : GPR2.Build.Source.Object; Unit_Name : String);
      --  Add Unit_Name to Units_Present

      -----------------
      -- Add_Pattern --
      -----------------

      procedure Add_Pattern (Attr, Item : String) is
      begin
         Unit_Patterns.Append (+Item);
         Attr_For_Pattern.Include (+Item, +Attr);
      end Add_Pattern;

      -------------------------
      -- Process_Source_File --
      -------------------------

      procedure Process_Source_File
        (Source : GPR2.Build.Source.Object; Unit_Name : String)
      is
         Actual_Unit_Name : String_Vectors.Vector :=
           String_Vectors.To_Vector (+Get_Unit_Name (Source), 1);
      begin
         Match_Pattern_List
           (Patterns_List        => Unit_Patterns,
            Strings_List         => Actual_Unit_Name,
            Patterns_Not_Covered => Patterns_Not_Covered);

         --  Add it only if it matches one of the patterns

         if not Actual_Unit_Name.Is_Empty then
            declare
               Lang : constant Some_Language := To_Language (Source.Language);
            begin
               Add_Unit (Units, Cur, Unit_Name, Source, Lang);
            end;
         end if;
      end Process_Source_File;

   --  Start of processing for Units_From_Project

   begin
      --  Get a list of patterns if List_Attr / List_File_Attr is defined, and
      --  create a globbing pattern for it. If none is defined, simply exit the
      --  procedure.

      List_From_Project
        (Prj, List_Attr, List_File_Attr, Add_Pattern'Access, Defined);
      Patterns_Not_Covered := To_String_Set (Unit_Patterns);

      if not Defined then
         return;
      end if;

      --  Now, fill Units with unit names matching Unit_Patterns. For languages
      --  not featuring the notion of unit (e.g. C), the unit name is the
      --  source file name.

      Iterate_Source_Files
        (Prj, Process_Source_File'Access, Recursive => False);

      for Pattern of Patterns_Not_Covered loop
         Warn
           ("no unit " & (+Pattern) & " in project " & String (Prj.Name)
            & " (" & (+Attr_For_Pattern.Element (Pattern)) & " attribute)");
      end loop;
   end Units_From_Project;

   -----------------------
   -- Load_Root_Project --
   -----------------------

   procedure Load_Root_Project
     (Prj_Name                   : String;
      Target, Runtime, CGPR_File : GNAT.Strings.String_Access;
      DB_Dir                     : String;
      From_Driver                : Boolean := False)
   is
      Opts : GPR2.Options.Object;
   begin
      if Prj_Tree.Is_Defined then
         Fatal_Error ("only one root project can be specified");
      end if;

      --  Allow activation of GNATcoll debug traces via configuration file,
      --  prior to initializing the project subsystem.

      GNATCOLL.Traces.Parse_Config_File (Filename => No_File);

      Opts.Add_Switch (GPR2.Options.P, Prj_Name);
      if Target /= null then
         Opts.Add_Switch (GPR2.Options.Target, Target.all);
      end if;
      if Runtime /= null then
         Opts.Add_Switch (GPR2.Options.RTS, Runtime.all);
      end if;
      if CGPR_File /= null then
         Opts.Add_Switch (GPR2.Options.Config, CGPR_File.all);
      end if;
      if DB_Dir /= "" then
         Opts.Add_Switch (GPR2.Options.Db, DB_Dir);
      end if;
      if Obj_Subdir /= "" then
         Opts.Add_Switch (GPR2.Options.Subdirs, +Obj_Subdir);
      end if;
      if Build_Tree_Dir /= No_File then
         Opts.Add_Switch
           (GPR2.Options.Relocate_Build_Tree, +Build_Tree_Dir.Full_Name);

         --  When the build tree directory is set, the root directory may be
         --  specified explicitly from the command line. Otherwise, the project
         --  file directory is used.

         if Root_Dir /= No_File then
            Opts.Add_Switch (GPR2.Options.Root_Dir, +Root_Dir.Full_Name);
         end if;
      end if;

      for Scv_C in S_Variables.Iterate loop
         Opts.Add_Switch
           (GPR2.Options.X,
            Key_Element_Maps.Key (Scv_C)
            & "="
            & Key_Element_Maps.Element (Scv_C));
      end loop;

      --  Make sure GPR_TOOL is initalized. There are several ways to
      --  initialize it: by decreasing order of precedence:
      --
      --    * explicitly set through a -X option;
      --    * set through an environment variable;
      --    * implicitly initialized by GNATcoverage.

      if S_Variables.Contains ("GPR_TOOL") then
         null;
      else
         declare
            GPR_Tool_Env_Var : GNAT.Strings.String_Access :=
               GNAT.OS_Lib.Getenv ("GPR_TOOL");
            GPR_Tool_Value   : constant String :=
              (if GPR_Tool_Env_Var.all = ""
               then "gnatcoverage"
               else GPR_Tool_Env_Var.all);
         begin
            Opts.Add_Switch (GPR2.Options.X, "GPR_TOOL=" & GPR_Tool_Value);
            Free (GPR_Tool_Env_Var);
         end;
      end if;

      if not Prj_Tree.Load
        (Opts,
         With_Runtime         => True,
         Reporter             => Create_Reporter,
         Artifacts_Info_Level => GPR2.Sources_Units,
         Absent_Dir_Error     => GPR2.No_Error)
      then
         Fatal_Error ("Could not load the project file, aborting.");
      end if;

      --  We do not support non-library aggregate projects, no need to go
      --  further.

      if Prj_Tree.Root_Project.Kind = GPR2.K_Aggregate then
         Fatal_Error ("non-library aggregate projects are not supported");
      end if;

      --  If we were asked only to load the project file, stop there (i.e.
      --  before computing the list of projects/units of interest).

      if From_Driver then
         return;
      end if;

      if not Externally_Built_Projects_Processing_Enabled
         and then Prj_Tree.Root_Project.Is_Externally_Built
      then
         Fatal_Error
           ("Root project is marked as externally built, while externally"
            & " built projects are ignored by default. Consider using"
            & " --externally-built-projects.");
      end if;
   end Load_Root_Project;

   -------------------------------
   -- Compute_Units_Of_Interest --
   -------------------------------

   procedure Compute_Units_Of_Interest (Override_Units : String_Vectors.Vector)
   is
   begin
      Build_Prj_Map;
      Build_Unit_Map (Override_Units);
   end Compute_Units_Of_Interest;

   -----------------------
   -- Is_Project_Loaded --
   -----------------------

   function Is_Project_Loaded return Boolean is
   begin
      return Prj_Tree.Is_Defined;
   end Is_Project_Loaded;

   ------------------------------
   -- Units_Of_Interest_Loaded --
   ------------------------------

   function Units_Of_Interest_Computed return Boolean is
   begin
      return not (Prj_Map.Is_Empty and then Unit_Map.Is_Empty);
   end Units_Of_Interest_Computed;

   ---------------------------
   -- Root_Project_Filename --
   ---------------------------

   function Root_Project_Filename return String is
   begin
      return String (Prj_Tree.Root_Project.Path_Name.Value);
   end Root_Project_Filename;

   --------------------------------
   -- Get_Single_Main_Executable --
   --------------------------------

   function Get_Single_Main_Executable return String is
      Execs : constant GPR2.Path_Name.Set.Object :=
        Prj_Tree.Root_Project.Executables;
   begin
      return (if Execs.Length = 1
              then String (Execs.First_Element.Value)
              else "");
   end Get_Single_Main_Executable;

   ---------------------
   -- Enumerate_Mains --
   ---------------------

   function Enumerate_Mains
     (Root_Project : GPR2.Project.View.Object;
      Language     : Any_Language)
      return GPR2.Build.Compilation_Unit.Unit_Location_Vector
   is
      Result : GPR2.Build.Compilation_Unit.Unit_Location_Vector;

      procedure Enumerate_Mains (Project : GPR2.Project.View.Object);
      --  Append the list of main source files found in Project to Result

      ---------------------
      -- Enumerate_Mains --
      ---------------------

      procedure Enumerate_Mains (Project : GPR2.Project.View.Object) is
      begin
         for Main of Project.Mains loop
            if Language = All_Languages
               or else Language
                       = To_Language_Or_All
                           (Project.Source (Main.Source.Simple_Name).Language)
            then
               Result.Append (Main);
            end if;
         end loop;
      end Enumerate_Mains;

   --  Start of processing for Enumerate_Mains

   begin
      Iterate_Projects
        (Root_Project, Enumerate_Mains'Access, Recursive => False);
      return Result;
   end Enumerate_Mains;

   function Enumerate_Mains
     (Language : Any_Language)
      return GPR2.Build.Compilation_Unit.Unit_Location_Vector is
   begin
      return Enumerate_Mains (Prj_Tree.Root_Project, Language);
   end Enumerate_Mains;

   ----------------
   -- Output_Dir --
   ----------------

   function Output_Dir return String is
   begin
      return String (Prj_Tree.Root_Project.Object_Directory.Value);
   end Output_Dir;

   --------------
   -- Switches --
   --------------

   function Switches (Op : String) return String_Vectors.Vector is
      Origin_Attr : constant GPR2.Project.Attribute.Object :=
        Prj_Tree.Root_Project.Attribute
          (GPR2.Project.Registry.Attribute.Origin_Project);
      Actual_Prj  : constant GPR2.Project.View.Object :=
        (if Origin_Attr.Is_Defined
         then Most_Extending (Lookup_Project (Origin_Attr.Value.Text))
         else Prj_Tree.Root_Project);
      Attr        : GPR2.Project.Attribute.Object;
   begin
      return Result : String_Vectors.Vector do
         Attr := Actual_Prj.Attribute
                   (Name  => +Switches,
                    Index => GPR2.Project.Attribute_Index.Create (Op));
         if Attr.Is_Defined then
            for S of Attr.Values loop
               Result.Append (+String (S.Text));
            end loop;
         end if;
      end return;
   end Switches;

   -----------------
   -- Set_Subdirs --
   -----------------

   procedure Set_Subdirs (Subdir : String) is
   begin
      Obj_Subdir := +Subdir;
   end Set_Subdirs;

   -------------------------------------------------
   -- Enable_Externally_Built_Projects_Processing --
   -------------------------------------------------

   procedure Enable_Externally_Built_Projects_Processing is
   begin
      Externally_Built_Projects_Processing_Enabled := True;
   end Enable_Externally_Built_Projects_Processing;

   -----------------------------------
   -- Set_Build_Tree_Dir_To_Current --
   -----------------------------------

   procedure Set_Build_Tree_Dir_To_Current is
   begin
      Build_Tree_Dir := Get_Current_Dir;
      Build_Tree_Dir.Normalize_Path;
   end Set_Build_Tree_Dir_To_Current;

   ------------------
   -- Set_Root_Dir --
   ------------------

   procedure Set_Root_Dir (Dir : String) is
   begin
      Root_Dir := Create (+Dir);
      Root_Dir.Normalize_Path;
   end Set_Root_Dir;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name (Full_Name : String) return String is
      Source : constant GPR2.Build.Source.Object := Lookup_Source (Full_Name);
   begin
      return (if Source.Is_Defined
              then String (Source.Owning_View.Name)
              else "");
   end Project_Name;

   --------------
   -- Language --
   --------------

   function Language (Full_Name : String) return Any_Language is
      Source : constant GPR2.Build.Source.Object := Lookup_Source (Full_Name);
   begin
      return (if Source.Is_Defined
              then To_Language_Or_All (Source.Language)
              else All_Languages);
   end Language;

   ------------
   -- Target --
   ------------

   function Target return String is
   begin
      return String (Prj_Tree.Target);
   end Target;

   -------------
   -- Runtime --
   -------------

   function Runtime return String is
   begin
      --  gnatcov is able to request only one runtime for all languages
      --  (--RTS:<lang> is not available), so looking for the first language
      --  that has a runtime should be fine.

      for L of Prj_Tree.Languages loop
         declare
            Result : constant String := String (Prj_Tree.Runtime (L));
         begin
            if Result /= "" then
               return Result;
            end if;
         end;
      end loop;
      return "";
   end Runtime;

   ------------------------------------
   -- Enumerate_Ignored_Source_Files --
   ------------------------------------

   procedure Enumerate_Ignored_Source_Files
     (Process : access procedure (Source_File : String))
   is
      procedure Enumerate (Prj : GPR2.Project.View.Object);
      --  Call Process on all ignored source files referenced by the
      --  Ignored_Source_Files(_List) project attributes.

      procedure Process_Wrapper (Attr, Source_File : String);
      --  Wrapper fo Process

      Dummy : Boolean;

      ---------------
      -- Enumerate --
      ---------------

      procedure Enumerate (Prj : GPR2.Project.View.Object) is
      begin
         List_From_Project
           (Prj,
            Ignored_Source_Files,
            Ignored_Source_Files_List,
            Process_Wrapper'Access,
            Dummy);
      end Enumerate;

      ---------------------
      -- Process_Wrapper --
      ---------------------

      procedure Process_Wrapper (Attr, Source_File : String) is
         pragma Unreferenced (Attr);
      begin
         Process.all (Source_File);
      end Process_Wrapper;

   --  Start of processing for Enumerate_Ignored_Source_Files

   begin
      for Prj_Info of Prj_Map loop
         Iterate_Projects
           (Root_Project => Prj_Info.Project,
            Process      => Enumerate'Access,
            Recursive    => Standard.Switches.Recursive_Projects);
      end loop;
   end Enumerate_Ignored_Source_Files;

   ------------------
   -- Runtime_Dirs --
   ------------------

   function Runtime_Dirs return String_Vectors.Vector
   is
      Runtime : constant GPR2.Project.View.Object := Prj_Tree.Runtime_Project;
   begin
      return Result : String_Vectors.Vector do
         if Runtime.Is_Defined then
            for Dir of Prj_Tree.Runtime_Project.Source_Directories loop
               Result.Append (+String (Dir.Value));
            end loop;
         end if;
      end return;
   end Runtime_Dirs;

   -------------
   -- Project --
   -------------

   function Project return GPR2.Project.Tree.Object is
   begin
      return Prj_Tree;
   end Project;

   -------------------
   -- Source_Suffix --
   -------------------

   function Source_Suffix
     (Lang    : Src_Supported_Language;
      Part    : GPR2.Valid_Unit_Kind;
      Project : GPR2.Project.View.Object) return String
   is
      L : constant GPR2.Language_Id := To_Language_Id (Lang);
   begin
      --  ??? Simplify once eng/gpr/gpr-issues#494 is implemented

      case Part is
         when GPR2.S_Spec =>
            return Project.Spec_Suffix (L).Value.Text;

         when GPR2.S_Body =>
            return Project.Body_Suffix (L).Value.Text;

         when GPR2.S_Separate =>
            return Project.Separate_Suffix.Value.Text;
      end case;
   end Source_Suffix;

   -------------------
   -- Lookup_Source --
   -------------------

   function Lookup_Source
     (Full_Name : String) return GPR2.Build.Source.Object
   is
      --  Full_Name can come from debug info or from another OS, so it may be
      --  an invalid filename for the current host OS (for instance
      --  "<built-in>"). Because of this, calling Ada.Directories.Simple_Name
      --  to extract the basename may trigger an exception. Use our own
      --  extraction helper (Platform_Independent_Basename) that will not crash
      --  in these legitimate cases.

      Basename : constant String := Platform_Independent_Basename (Full_Name);
   begin
      --  Likewise, GPR2 has restrictions on what a "Simple_Name" can be: do
      --  not bother performing a GPR lookup if the basename we have is invalid
      --  according to GPR2.

      if GPR2.Is_Simple_Name (GPR2.Filename_Type (Basename)) then
         declare
            Resolved : constant GPR2.Path_Name.Object :=
              GPR2.Path_Name.Create_File (GPR2.Filename_Type (Full_Name));
         begin
            for P of Prj_Tree.Root_Project.Closure (Include_Self => True) loop
               declare
                  Source : constant GPR2.Build.Source.Object :=
                    P.Source (GPR2.Simple_Name (Basename));
               begin
                  if Source.Is_Defined and then Source.Path_Name = Resolved
                  then
                     return Source;
                  end if;
               end;
            end loop;
         end;
      end if;

      return GPR2.Build.Source.Undefined;
   end Lookup_Source;

   ---------------
   -- Verbosity --
   ---------------

   overriding function Verbosity
     (Self : Reporter) return GPR2.Reporter.Verbosity_Level is
   begin
      return Self.Inner.Verbosity;
   end Verbosity;

   ---------------------
   -- Internal_Report --
   ---------------------

   overriding procedure Internal_Report
     (Self : in out Reporter; Message : GPR2.Message.Object) is
   begin
      if Message.Level in GPR2.Message.Warning | GPR2.Message.Error then
         Register_Warning;
      end if;
      Self.Inner.Internal_Report (Message);
   end Internal_Report;

   ---------------------
   -- Create_Reporter --
   ---------------------

   function Create_Reporter return GPR2.Reporter.Object'Class is
      Result : constant Reporter :=
        (GPR2.Reporter.Object with
         Inner => GPR2.Reporter.Console.Create);
   begin
      return Result;
   end Create_Reporter;

   --------------------
   -- Most_Extending --
   --------------------

   function Most_Extending
     (View : GPR2.Project.View.Object) return GPR2.Project.View.Object
   is
      Result : GPR2.Project.View.Object := View;
   begin
      while Result.Is_Extended loop
         Result := Result.Extending;
      end loop;
      return Result;
   end Most_Extending;

   --------------------
   -- Source_Closure --
   --------------------

   function Source_Closure
     (View                  : GPR2.Project.View.Object;
      With_Externally_Built : Boolean;
      With_Runtime          : Boolean) return Source_Vectors.Vector
   is
      Result : Source_Vectors.Vector;
      --  List of sources found so far
   begin
      for Source of View.Visible_Sources loop
         declare
            View : constant GPR2.Project.View.Object := Source.Owning_View;
         begin
            --  If we were asked to include this view in the closure, add its
            --  sources to Result. Note also that we need to ignore extended
            --  projects: only the sources of the corresponding extending
            --  project matter.

            if not View.Is_Extended
               and then (With_Externally_Built
                         or else not View.Is_Externally_Built)
               and then (With_Runtime or else not View.Is_Runtime)
            then
               Result.Append (Source);
            end if;
         end;
      end loop;
      return Result;
   end Source_Closure;

   ----------------
   -- First_Unit --
   ----------------

   function First_Unit
     (Source : GPR2.Build.Source.Object) return GPR2.Build.Unit_Info.Object is
   begin
      return (if Source.Has_Single_Unit
              then Source.Unit
              else Source.Unit (Index => 1));
   end First_Unit;

   --  Register the Coverage package and its attributes to GPR2

   package GPR2_RP renames GPR2.Project.Registry.Pack;
   package GPR2_RA renames GPR2.Project.Registry.Attribute;
begin
   GPR2_RP.Add (GPR2_Coverage_Package, GPR2_RP.Everywhere);
   GPR2_RP.Description.Set_Package_Description
     (GPR2_Coverage_Package,
      "Specifies options used when calling the 'gnatcov' program.");

   GPR2_RA.Add
     (Name                 => +Units,
      Index_Type           => GPR2_RA.No_Index,
      Value                => GPR2_RA.List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2_RA.Everywhere);
   GPR2_RA.Description.Set_Attribute_Description
     (+Units,
      "Names of units of interest to include in source coverage analysis.");

   GPR2_RA.Add
     (Name                 => +Excluded_Units,
      Index_Type           => GPR2_RA.No_Index,
      Value                => GPR2_RA.List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2_RA.Everywhere);
   GPR2_RA.Description.Set_Attribute_Description
     (+Excluded_Units,
      "Names of units of interest to exclude from source coverage analysis.");

   GPR2_RA.Add
     (Name                 => +Routines,
      Index_Type           => GPR2_RA.No_Index,
      Value                => GPR2_RA.List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2_RA.Everywhere);
   GPR2_RA.Description.Set_Attribute_Description
     (+Routines,
      "Symbol names for the routines to include in object coverage.");

   GPR2_RA.Add
     (Name                 => +Excluded_Routines,
      Index_Type           => GPR2_RA.No_Index,
      Value                => GPR2_RA.List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2_RA.Everywhere);
   GPR2_RA.Description.Set_Attribute_Description
     (+Excluded_Routines,
      "Symbol names for the routines to exclude from object coverage.");

   GPR2_RA.Add
     (Name                 => +Ignored_Source_Files,
      Index_Type           => GPR2_RA.No_Index,
      Value                => GPR2_RA.List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2_RA.Everywhere);
   GPR2_RA.Description.Set_Attribute_Description
     (+Ignored_Source_Files,
      "Source file names to exclude from source coverage analysis.");

   GPR2_RA.Add
     (Name                 => +Switches,
      Index_Type           => GPR2_RA.String_Index,
      Value                => GPR2_RA.List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2_RA.Everywhere);
   GPR2_RA.Description.Set_Attribute_Description
     (+Switches,
      "Command line switches to automatically include when running 'gnatcov',"
      & " indexed by gnatcov sub-command (""instrument"", ""coverage"", ...)");

   GPR2_RA.Add
     (Name                 => +Units_List,
      Index_Type           => GPR2_RA.No_Index,
      Value                => GPR2_RA.Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2_RA.Everywhere);
   GPR2_RA.Description.Set_Attribute_Description
     (+Units_List,
      "Text file that contains names of units of interest to include in source"
      & " coverage analysis.");

   GPR2_RA.Add
     (Name                 => +Excluded_Units_List,
      Index_Type           => GPR2_RA.No_Index,
      Value                => GPR2_RA.Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2_RA.Everywhere);
   GPR2_RA.Description.Set_Attribute_Description
     (+Excluded_Units_List,
      "Text file that contains names of units of interest to exclude from"
      & " source coverage analysis.");

   GPR2_RA.Add
     (Name                 => +Routines_List,
      Index_Type           => GPR2_RA.No_Index,
      Value                => GPR2_RA.Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2_RA.Everywhere);
   GPR2_RA.Description.Set_Attribute_Description
     (+Routines_List,
      "Text file that contains symbol names for the routines to include in"
      & " object coverage.");

   GPR2_RA.Add
     (Name                 => +Excluded_Routines_List,
      Index_Type           => GPR2_RA.No_Index,
      Value                => GPR2_RA.Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2_RA.Everywhere);
   GPR2_RA.Description.Set_Attribute_Description
     (+Excluded_Routines_List,
      "Text file that contains symbol names for the routines to exclude from"
      & " object coverage.");

   GPR2_RA.Add
     (Name                 => +Ignored_Source_Files_List,
      Index_Type           => GPR2_RA.No_Index,
      Value                => GPR2_RA.Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2_RA.Everywhere);
   GPR2_RA.Description.Set_Attribute_Description
     (+Ignored_Source_Files_List,
      "Text file that contains source file names to exclude from source"
      & " coverage analysis.");
end Project;
