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

--  GNAT projects support

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Vectors;

with GNAT.Strings; use GNAT.Strings;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Source;
with GPR2.Build.Unit_Info;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Reporter;

with Files_Table;
with Strings;      use Strings;
with Switches;     use Switches;
with Traces_Files; use Traces_Files;

package Project is

   Externally_Built_Projects_Processing_Enabled : Boolean := False;
   --  Whether to include projects marked as externally built to processings

   -----------------------
   -- Lifetime handling --
   -----------------------

   function Is_Project_Loaded return Boolean;
   --  Return whether Load_Root_Project was called and returned successfully

   function Units_Of_Interest_Computed return Boolean;
   --  Return whether the set of units of interest has been computed by calling
   --  the Compute_Units_Of_Interest procedure.

   -------------------------------
   -- Pre-loading configuration --
   -------------------------------

   procedure Enable_Externally_Built_Projects_Processing
      with Pre => not Is_Project_Loaded;
   --  Request that the projects marked as externally built are included in
   --  processings (they are excluded by default).

   procedure Set_Subdirs (Subdir : String)
      with Pre => not Is_Project_Loaded;
   --  Set the object subdir for all loaded projects

   procedure Add_Project (Prj_Name : String)
     with Pre => not Units_Of_Interest_Computed;
   --  Add Prj_Name to the list of projects for which coverage analysis is
   --  desired.
   --
   --  Prj_Name may optionally have a Project_File_Extension.
   --
   --  If Prj_Name maps to no loaded project, Compute_Units_Of_Interest will
   --  later emit an error.

   procedure Set_Build_Tree_Dir_To_Current
     with Pre => not Is_Project_Loaded;
   --  Set the root build directory to current directory, for out of tree
   --  builds.

   procedure Set_Root_Dir (Dir : String)
     with Pre => not Is_Project_Loaded;
   --  Set the directory to consider as topmost directory when relocating
   --  the build tree.

   procedure Load_Root_Project
     (Prj_Name                   : String;
      Target, Runtime, CGPR_File : GNAT.Strings.String_Access;
      DB_Dir                     : String;
      From_Driver                : Boolean := False)
      with Pre  => not Is_Project_Loaded,
           Post => Is_Project_Loaded;
   --  Load the project tree rooted at Prj_Name (with optional
   --  Project_File_Extension). Target is the target prefix, or NULL in the
   --  native case. Runtime is the Ada runtime to use, or NULL in the default
   --  runtime case. CGPR_File is the path to the configuration project file,
   --  if any. DB_Dir is the path to the additional knowledge base directory,
   --  if any.
   --
   --  If From_Driver is True, do not compute the list of projects/units of
   --  interest. This is meant to be used only in the gnatcov driver, where we
   --  just need to determine the target.

   procedure Compute_Units_Of_Interest
     (Override_Units : String_Vectors.Vector);
   --  Compute the sets of projects and units of interest from Add_Project,
   --  Override_Units and project files data.
   --
   --  If Override_Units is present, it overrides the set of units to be
   --  considered, else the set defined by the project through the Units,
   --  Units_List, Exclude_Units, and Exclude_Units_List attributes is used.

   ----------------------------
   -- Post-loading accessors --
   ----------------------------

   function Root_Project_Filename return String
      with Pre => Is_Project_Loaded;
   --  Return the file name for the loaded root project

   function Get_Single_Main_Executable return String
      with Pre => Is_Project_Loaded;
   --  If there is only one main source in the loaded project, return the full
   --  path of its main executable (including its suffix, for instance ".exe").
   --  Otherwise, return an empty string.

   function Get_Unit_Name (Source : GPR2.Build.Source.Object) return String;
   --  Return the gnatcov-style unit name for the given source. For Ada, it is
   --  the actual unit name (same meaning as in GPR2), but for other languages,
   --  it is the file basename.

   function To_Compilation_Unit
     (Source : GPR2.Build.Source.Object) return Files_Table.Compilation_Unit;
   --  Return the Compilation_Unit for Info

   --------------------------------------
   -- Accessors for project properties --
   --------------------------------------

   procedure Enumerate_Units_Of_Interest
     (Callback : access procedure (Name : Files_Table.Compilation_Unit));
   --  Call Callback once for every unit of interest. Name is the unit name

   procedure Enumerate_SCOs_Files
     (Callback : access procedure (Lib_Name : String);
      Kind     : Traces_Files.GNATcov_Trace_File_Kind)
      with Pre  => Is_Project_Loaded;
   --  Invoke Callback once for every SCOs file (SID / ALI depending on the
   --  trace mode) corresponding to a unit of interest. This emits a warning
   --  for all units of interest that have no SCOs file.

   procedure Enumerate_Sources
     (Callback      : access procedure
        (Project : GPR2.Project.View.Object;
         File    : GPR2.Build.Source.Object);
      Language      : Any_Language;
      Only_UOIs     : Boolean := False)
     with Pre => Is_Project_Loaded;
   --  Call Callback once for every source file of the given language
   --  mentionned in a previous Add_Project call. If Only_UOIs is set to True,
   --  only call Callback on sources that are units of interest. Override_Units
   --  has the same semantics as in Enumerate_LIs.

   function Enumerate_Mains
     (Language : Any_Language)
      return GPR2.Build.Compilation_Unit.Unit_Location_Vector
      with Pre => Is_Project_Loaded;
   --  Return the list of all main source files found in the project tree for
   --  the given Language, or for all languages if Language is All_Languages.
   --
   --  Note that this also returns source files for mains that are not units of
   --  interest.

   function Find_Source_File (Simple_Name : String) return String_Access;
   --  Look for the absolute path for the source file called Simple_Name in the
   --  loaded project tree. If no such source file is found, return null.
   --  Otherwise, a string is allocated and returned. In this case, the caller
   --  is responsible for deallocating the returned access.
   --
   --  If no project is loaded, just return null.

   function Switches (Op : String) return String_Vectors.Vector
      with Pre => Is_Project_Loaded;
   --  Return a list of gnatcov switches defined by the root project

   function Output_Dir return String
      with Pre => Is_Project_Loaded;
   --  Return the output directory of the root project

   function Project_Name (Full_Name : String) return String
      with Pre => Is_Project_Loaded;
   --  Return the name of the project containing the given source file. Return
   --  the empty string if Source_Name cannot be associated with a project
   --  name. This can happen for sources that belong eg. to the runtime.

   function Language (Full_Name : String) return Any_Language
      with Pre => Is_Project_Loaded;
   --  Return the language for the given source file, or All_Languages if the
   --  file is unknown.

   function Target return String
      with Pre => Is_Project_Loaded;
   --  Return the target in the root project, if any, and the empty string
   --  otherwise.

   function Runtime return String
      with Pre => Is_Project_Loaded;
   --  Return the runtime in the root project, if any, and the empty string
   --  otherwise. This concerns only the runtime for Ada.

   procedure Enumerate_Ignored_Source_Files
     (Process : access procedure (Source_File : String))
      with Pre => Is_Project_Loaded;
   --  Call Process on each name in the Coverage'Ignored_Source_File attribute

   function Runtime_Dirs return String_Vectors.Vector;
   --  Return the list of runtime directories for the currently loaded project

   ----------------------------------------
   -- Raw accessors for the project tree --
   ----------------------------------------

   function Project return GPR2.Project.Tree.Object
      with Pre => Is_Project_Loaded;

   procedure Iterate_Projects
     (Root_Project             : GPR2.Project.View.Object;
      Process                  : access procedure
                                   (Prj : GPR2.Project.View.Object);
      Recursive                : Boolean;
      Include_Extended         : Boolean := False;
      Include_Externally_Built : Boolean :=
        Externally_Built_Projects_Processing_Enabled)
      with Pre => Is_Project_Loaded;
   --  Call Process on Root_Project if Recursive is False, or on the whole
   --  project tree otherwise.
   --
   --  Set Include_Extended to True to process extended projects (otherwise,
   --  only process ultimate extending projects). Set Include_Externally_Built
   --  to True to process externally built projects.

   function Source_Suffix
     (Lang    : Src_Supported_Language;
      Part    : GPR2.Valid_Unit_Kind;
      Project : GPR2.Project.View.Object) return String;
   --  Return the filename suffix corresponding for Part files and Lang

   function Lookup_Source
     (Full_Name : String) return GPR2.Build.Source.Object;
   --  Look for a source file by full name in the loaded project tree

   -------------------
   -- GPR2 reporter --
   -------------------

   function Create_Reporter return GPR2.Reporter.Object'Class;
   --  Returns a wrapper around GPR2.Reporter.Console so that we detect when
   --  warnings are emitted, allowing us to implement --warning-as-errors.

   ---------------------
   -- GPR2 extensions --
   ---------------------

   function Most_Extending
     (View : GPR2.Project.View.Object) return GPR2.Project.View.Object;
   --  If View is an extended project, return Most_Extending (View.Extending).
   --  Return View itself otherwise.
   --
   --  ??? Remove once eng/gpr/gpr-issues#501 is implemented.

   package Source_Vectors is new Ada.Containers.Vectors
     (Positive, GPR2.Build.Source.Object, "=" => GPR2.Build.Source."=");

   function Source_Closure
     (View                  : GPR2.Project.View.Object;
      With_Externally_Built : Boolean;
      With_Runtime          : Boolean) return Source_Vectors.Vector;
   --  Return the set of all sources in the closure of View (its own sources
   --  included). Include sources from externally built projects iff
   --  With_Externally_Built is True, include sources from the runtime iff
   --  With_Runtime is True.

   function First_Unit
     (Source : GPR2.Build.Source.Object) return GPR2.Build.Unit_Info.Object;
   --  Assuming that Source has a unit-based language, return the first unit it
   --  contains.

end Project;
