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

with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with Inputs;

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

   --------------------------------------
   -- Accessors for project properties --
   --------------------------------------

   procedure Enumerate_Units_Of_Interest
     (Callback : access procedure (Name : String; Is_Subunit : Boolean));
   --  Call Callback once for every unit of interest. Name is the lower-case
   --  unit name, and Is_Subunit corresponds to the Unit_Info.Is_Subunit field
   --  (see project.adb).

   procedure Enumerate_LIs (LI_Cb : access procedure (LI_Name : String))
      with Pre  => Is_Project_Loaded and then not LIs_Enumerated,
           Post => LIs_Enumerated;
   --  Call LI_Cb once for every library information (ALI/GLI) file from a
   --  project mentioned in a previous Add_Project call.

   function LIs_Enumerated return Boolean with Pre => Is_Project_Loaded;
   --  Return whether Enumerate_LIs was called

   procedure Report_Units_Without_LI with Pre => LIs_Enumerated;
   --  Output a warning for all units of interest for which we saw no library
   --  information file.

   procedure Enumerate_SIDs (Callback : access procedure (SID_Name : String))
      with Pre  => Is_Project_Loaded and then not SIDs_Enumerated,
           Post => SIDs_Enumerated;
   --  Invoke Callback once for every SID file corresponding to a unit of
   --  interest. This emits a warning for all units of interest that have no
   --  SID file.

   function SIDs_Enumerated return Boolean with Pre => Is_Project_Loaded;
   --  Return whether Enumerate_SIDs was called

   procedure Enumerate_Sources
     (Callback         : access procedure
        (Project : GNATCOLL.Projects.Project_Type;
         File    : GNATCOLL.Projects.File_Info);
      Language         : String;
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

   function Enumerate_Ada_Mains return Main_Source_File_Array
      with Pre => Is_Project_Loaded;
   --  Return the list of all Ada main source files recursively found in the
   --  loaded project tree.

   function Enumerate_C_Mains return Main_Source_File_Array
      with Pre => Is_Project_Loaded;
   --  Return the list of all C main source files recursively found in the
   --  loaded project tree.

   function Find_Source_File (Simple_Name : String) return String_Access;
   --  Look for the absolute path for the source file called Simple_Name in the
   --  loaded project tree. If no such source file is found, return null.
   --  Otherwise, a string is allocated and returned. In this case, the caller
   --  is responsible for deallocating the returned access.
   --
   --  If no project is loaded, just return null.

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
     (Root_Project : GNATCOLL.Projects.Project_Type;
      Process      : access procedure (Prj : GNATCOLL.Projects.Project_Type);
      Recursive    : Boolean;
      Extended     : Boolean := False)
      with Pre => Is_Project_Loaded;
   --  Call Process on Root_Project if Recursive is False, or on the whole
   --  project tree otherwise.
   --
   --  Visit extended projects only if Extended is True. This is useful only in
   --  very specific cases, such as when needing to process all object
   --  direcotries in the project tree.

end Project;
