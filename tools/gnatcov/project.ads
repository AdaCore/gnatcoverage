------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2012-2022, AdaCore                     --
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
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;

with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;

with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;

with Inputs;
with Paths;        use Paths;
with Strings;      use Strings;
with Switches;     use Switches;
with Traces_Files; use Traces_Files;

package Project is

   -----------------------
   -- Lifetime handling --
   -----------------------

   function Is_Project_Loaded return Boolean;
   --  Return whether Load_Root_Project was called and returned successfully

   function Units_Of_Interest_Computed return Boolean;
   --  Return whether the set of units of interest has been computed by calling
   --  the Compute_Units_Of_Interest procedure.

   procedure Finalize;
   --  Release all resources allocated by project handling. Must be called
   --  before leaving gnatcov.

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

   procedure Load_Root_Project
     (Prj_Name                   : String;
      Target, Runtime, CGPR_File : String_Access;
      From_Driver                : Boolean := False)
      with Pre  => not Is_Project_Loaded
                   and then ((Target = null and then Runtime = null)
                             or else CGPR_File = null),
           Post => Is_Project_Loaded;
   --  Load the project tree rooted at Prj_Name (with optional
   --  Project_File_Extension). Target is the target prefix, or NULL in the
   --  native case. Runtime is the Ada runtime to use, or NULL in the default
   --  runtime case. CGPR_File is the path to the configuration project file,
   --  if any.
   --
   --  Note that Target/Runtime must not be provided if a configuration project
   --  file is provided, and reciprocally.
   --
   --  If From_Driver is True, do not compute the list of projects/units of
   --  interest. This is meant to be used only in the gnatcov driver, where we
   --  just need to determine the target.

   procedure Compute_Units_Of_Interest (Override_Units : Inputs.Inputs_Type);
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

   -------------------------
   -- Unit identification --
   -------------------------

   type Unique_Name (Language : Any_Language_Kind := Unit_Based_Language) is
      record
         Unit_Name : Ada.Strings.Unbounded.Unbounded_String;
         case Language is
            when File_Based_Language =>
               Project_Name : Ada.Strings.Unbounded.Unbounded_String;
            when others => null;
         end case;
      end record;
   --  To uniquely identify a unit, we need its unit name (or base name for a C
   --  unit). For file-based languages such as C or C++, we might have homonym
   --  base file names in different projects so we keep track of the project
   --  name in addition.

   use type Ada.Strings.Unbounded.Unbounded_String;

   function To_Unique_Name
     (Unit_Name : String;
      Project   : Project_Type;
      Language  : Some_Language) return Unique_Name;

   function Image (U : Unique_Name) return String is
     (case U.Language is
         when Unit_Based_Language => To_Lower (+U.Unit_Name),
         when File_Based_Language =>
            +U.Project_Name & ":" & Fold_Filename_Casing (+U.Unit_Name));

   function "<" (L, R : Unique_Name) return Boolean is
     (Image (L) < Image (R));

   function "=" (L, R : Unique_Name) return Boolean is
     (Image (L) = Image (R));

   package Unit_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Unique_Name);

   --------------------------------------
   -- Accessors for project properties --
   --------------------------------------

   procedure Enumerate_Units_Of_Interest
     (Callback : access procedure (Name : Unique_Name; Is_Subunit : Boolean));
   --  Call Callback once for every unit of interest. Name is the unit name,
   --  and Is_Subunit corresponds to the Unit_Info.Is_Subunit field (see
   --  project.adb).

   function Is_Unit_Of_Interest
     (Project   : Project_Type;
      Unit_Name : String;
      Language  : Some_Language) return Boolean;
   --  Return whether the unit Unit_Name that belongs to the project Project
   --  is a unit of interest.

   function Is_Unit_Of_Interest (Full_Name : String) return Boolean;
   --  Same as above, but given a full name

   procedure Enumerate_SCOs_Files
     (Callback : access procedure (Lib_Name : String);
      Kind     : Traces_Files.Trace_File_Kind)
      with Pre  => Is_Project_Loaded;
   --  Invoke Callback once for every SCOs file (SID / ALI depending on the
   --  trace mode) corresponding to a unit of interest. This emits a warning
   --  for all units of interest that have no SCOs file.

   procedure Enumerate_Sources
     (Callback         : access procedure
        (Project : GNATCOLL.Projects.Project_Type;
         File    : GNATCOLL.Projects.File_Info);
      Language         : Any_Language;
      Include_Subunits : Boolean := False)
     with Pre => Is_Project_Loaded;
   --  Call Callback once for every source file of the given language
   --  mentionned in a previous Add_Project call. Override_Units has the same
   --  semantics as in Enumerate_LIs.
   --
   --  If Include_Subunits is false (the default) then Callback will skip
   --  sources that are subunits.

   type Main_Source_File is record
      File    : GNATCOLL.VFS.Virtual_File;
      --  Base name for the source file

      Project : GNATCOLL.Projects.Project_Type;
      --  The project this source files comes from
   end record;

   type Main_Source_File_Array is
      array (Positive range <>) of Main_Source_File;

   function Enumerate_Mains
     (Language : Any_Language) return Main_Source_File_Array
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

   function Runtime_Supports_Finalization return Boolean with
     Pre => Is_Project_Loaded;
   --  Return whether the configured runtime includes the Ada.Finalization unit

   function Runtime_Supports_Task_Termination return Boolean with
     Pre => Is_Project_Loaded;
   --  Return whether the configured runtime has necessary units to implement
   --  the ravenscar-task-termination dump trigger.

   function Switches (Op : String) return String_List_Access
      with Pre => Is_Project_Loaded;
   --  Return a list of gnatcov switches defined by the root project. Caller
   --  is responsible for deallocation.

   function Output_Dir return String
      with Pre => Is_Project_Loaded;
   --  Return the output directory of the root project

   function Project_Name (Source_Name : String) return String
      with Pre => Is_Project_Loaded;
   --  Return the name of the project containing the given source file. Return
   --  the empty string if Source_Name cannot be associated with a project
   --  name. This can happen for sources that belong eg. to the runtime.

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

   ----------------------------------------
   -- Raw accessors for the project tree --
   ----------------------------------------

   function Project return GNATCOLL.Projects.Project_Tree_Access
      with Pre => Is_Project_Loaded;

   procedure Iterate_Projects
     (Root_Project     : GNATCOLL.Projects.Project_Type;
      Process          : access procedure
                           (Prj : GNATCOLL.Projects.Project_Type);
      Recursive        : Boolean;
      Include_Extended : Boolean := False)
      with Pre => Is_Project_Loaded;
   --  Call Process on Root_Project if Recursive is False, or on the whole
   --  project tree otherwise.
   --
   --  Unless Include_Extended is True, only process ultimate extending
   --  projects.

end Project;
