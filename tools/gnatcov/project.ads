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

with Inputs;

package Project is

   procedure Load_Root_Project (Prj_Name : String; Target : String_Access);
   --  Load the project tree rooted at Prj_Name (with optional
   --  Project_File_Extension). Target is the target prefix, or NULL in the
   --  native case.

   --  All the following routines may only be called after Load_Root_Project.

   procedure Add_Project (Prj_Name : String);
   --  Add Prj_Name to the list of projects for which coverage analysis is
   --  desired. This must be a project in the closure of the previously loaded
   --  root project. Prj_Name may optionally have a Project_File_Extension.

   function Origin_Project return String;
   --  If the loaded root project has an Origin_Project attribute, return its
   --  value. Return an empty string otherwise.

   procedure Compute_Project_View;
   --  Recompute the view of the loaded project within the current scenario

   procedure Set_Subdirs (Subdir : String);
   --  Set the object subdir for all loaded projects

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
   --  Return the runtime in the root project, if any, and the empty string
   --  otherwise. This concerns only the runtime for Ada.

   procedure Enumerate_Ignored_Source_Files
     (Process : access procedure (Source_File : String));
   --  Call Process on each name in the Coverage'Ignored_Source_File attribute

end Project;
