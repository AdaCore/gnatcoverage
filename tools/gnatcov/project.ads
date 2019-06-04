------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

   procedure Finalize;
   --  Release all resources allocated by project handling. Must be called
   --  before leaving gnatcov.

   procedure Load_Root_Project
     (Prj_Name : String; Target, Runtime, CGPR_File : String_Access)
      with Pre => (Target = null and then Runtime = null)
                  or else CGPR_File = null;
   --  Load the project tree rooted at Prj_Name (with optional
   --  Project_File_Extension). Target is the target prefix, or NULL in the
   --  native case. Runtime is the Ada runtime to use, or NULL in the default
   --  runtime case. CGPR_File is the path to the configuration project file,
   --  if any.
   --
   --  Note that Target/Runtime must not be provided if a configuration project
   --  file is provided, and reciprocally.

   --  All the following routines, except Is_Project_Loaded, may only be called
   --  after Load_Root_Project.

   function Is_Project_Loaded return Boolean;
   --  Return whether Load_Root_Project was called and returned successfully

   function Root_Project_Filename return String
      with Pre => Is_Project_Loaded;
   --  Return the file name for the loaded root project

   function Get_Single_Main_Executable return String;
   --  If there is only one main source in the loaded project, return the full
   --  path of its main executable (including its suffix, for instance ".exe").
   --  Otherwise, return an empty string.

   procedure Add_Project (Prj_Name : String);
   --  Add Prj_Name to the list of projects for which coverage analysis is
   --  desired. This must be a project in the closure of the previously loaded
   --  root project. Prj_Name may optionally have a Project_File_Extension.

   function Origin_Project return String;
   --  If the loaded root project has an Origin_Project attribute, return its
   --  value. Return an empty string otherwise.

   procedure Set_Subdirs (Subdir : String);
   --  Set the object subdir for all loaded projects

   procedure Enable_Externally_Built_Projects_Processing
      with Pre => not Is_Project_Loaded;
   --  Request that the projects marked as externally built are included in
   --  processings (they are excluded by default).

   --------------------------------------
   -- Accessors for project properties --
   --------------------------------------

   procedure Enumerate_LIs
     (LI_Cb          : access procedure (LI_Name : String);
      Override_Units : Inputs.Inputs_Type);
   --  Call LI_Cb once for every library information (ALI/GLI) file from a
   --  project mentioned in a previous Add_Project call. If Override_Units is
   --  present, it overrides the set of units to be considered, else the set
   --  defined by the project through the Units, Units_List, Exclude_Units, and
   --  Exclude_Units_List attributes is used.

   procedure Enumerate_Ada_Sources
     (Callback       : access procedure
        (Project : GNATCOLL.Projects.Project_Type;
         File    : GNATCOLL.Projects.File_Info);
      Override_Units : Inputs.Inputs_Type);
   --  Call Callback once for every Ada source file mentionned in a previous
   --  Add_Project call. Override_Units has the same semantics as in
   --  Enumerate_LIs.

   type Main_Source_File is record
      File    : GNATCOLL.VFS.Virtual_File;
      --  Base name for the source file

      Project : GNATCOLL.Projects.Project_Type;
      --  The project this source files comes from
   end record;

   type Main_Source_File_Array is
      array (Positive range <>) of Main_Source_File;

   function Enumerate_Ada_Mains return Main_Source_File_Array;
   --  Return the list of all Ada main source files recursively found in the
   --  loaded project tree.

   function Find_Source_File (Simple_Name : String) return String_Access;
   --  Look for the absolute path for the source file called Simple_Name. If no
   --  such source file is found, return null. Otherwise, a string is allocated
   --  and returned. In this case, the caller is responsible for deallocating
   --  the returned access.

   function Switches (Op : String) return String_List_Access;
   --  Return a list of gnatcov switches defined by the root project. Caller
   --  is responsible for deallocation.

   function Output_Dir return String;
   --  Return the output directory of the root project

   function Project_Name (Source_Name : String) return String;
   --  Return the name of the project containing the given source file. Return
   --  the empty string if Source_Name cannot be associated with a project
   --  name. This can happen for sources that belong eg. to the runtime.

   function Target return String;
   --  Return the target in the root project, if any, and the empty string
   --  otherwise.

   function Runtime return String;
   --  Return the runtime in the root project, if any, and the empty string
   --  otherwise. This concerns only the runtime for Ada.

   procedure Enumerate_Ignored_Source_Files
     (Process : access procedure (Source_File : String));
   --  Call Process on each name in the Coverage'Ignored_Source_File attribute

   ---------------------------------------
   -- Raw accessor for the project tree --
   ---------------------------------------

   function Project return GNATCOLL.Projects.Project_Tree_Access
      with Pre => Is_Project_Loaded;

end Project;
