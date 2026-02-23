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

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with GPR2.Build.Source;
with GPR2.Path_Name;
with GPR2.Project.View;

with GNAT.Regexp; use GNAT.Regexp;

with Files_Handling; use Files_Handling;

package Instrument.GPR_Utils is

   use type GPR2.Path_Name.Object;

   function Less (L, R : GPR2.Build.Source.Object) return Boolean
   is (L.Path_Name < R.Path_Name);
   function Equal (L, R : GPR2.Build.Source.Object) return Boolean
   is (L.Path_Name = R.Path_Name);

   package Source_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets
       (Element_Type => GPR2.Build.Source.Object,
        "<"          => Less,
        "="          => Equal);

   type Project_Info is record
      Project : GPR2.Project.View.Object;
      --  Project that this record describes

      Externally_Built : Boolean;
      --  Whether this project is externally built. In that case, we assume its
      --  units of interest have already been instrumented.

      Desc : Prj_Desc;
      --  Description containing all the project information needed for
      --  instrumentation purposes.

   end record;

   type Project_Info_Access is access all Project_Info;

   package Project_Info_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Project_Info_Access,
        Equivalent_Keys => "=",
        Hash            => Strings.Hash);
   --  Mapping from project name (as returned by GNATCOLL.Projects.Name) to
   --  Project_Info records. Project_Info records are owned by this map, and
   --  thus must be deallocated when maps are deallocated.

   type Inst_Context is limited record
      Ada_Default_Charset : Unbounded_String;
      --  Default charset to analyze Ada source code

      Mapping_File : Unbounded_String;
      --  File that describes the mapping of units to source files for all Ada
      --  units.

      Config_Pragmas_Mapping : Unbounded_String;
      --  File that describes the mapping of Ada source files to configuration
      --  pragma files. See the Save_Config_Pragmas_Mapping and
      --  Load_Config_Pragmas_Mapping procedures in Instrument.Ada_Unit for
      --  more information.

      Sources_Of_Interest_Response_File : Unbounded_String;
      --  File containing the list of units of interest, identified by their
      --  fullname. This is passed on to gnatcov instrument-source invokations
      --  (for Ada), to know which part of a unit (spec / body / separate) must
      --  be instrumented.

      Ada_Preprocessor_Data_File : Unbounded_String;
      --  JSON file that contains the preprocessor data necessary to analyze
      --  Ada sources (see Instrument.Ada_Unit.Create_Preprocessor_Data_File).

      Excluded_Source_Files_Present : Boolean;
      Excluded_Source_Files         : GNAT.Regexp.Regexp;
      --  If present, instrumentation will ignore files whose names match the
      --  accessed pattern.

      Project_Info_Map : Project_Info_Maps.Map;
      --  For each project that contains units of interest, this tracks a
      --  Project_Info record.

      Files_Of_Interest : File_Sets.Set;
      --  List of files of interest.
      --
      --  This is passed on to instrument-source invocations when instrumenting
      --  an Ada file (to know which part of a compilation unit must be
      --  instrumented, i.e. spec / body / separates). It is also passed to
      --  instrument-main invocations to know the full list of instrumented
      --  sources.

      Tag : Unbounded_String;
      --  Tag relative to the current instrumentation run

   end record;

   type Inst_Context_Acc is access all Inst_Context;

   function Load_Naming_Scheme
     (Prj : GPR2.Project.View.Object) return Naming_Scheme_Desc;
   --  Retrieve the naming scheme from the given project Prj

   type Library_Unit_Info is record
      Main_Part_Src : GPR2.Build.Source.Object;
      --  Main part for this compilation unit

      Is_UOI : Boolean;
      --  Whether the unit is a unit of interest

      Is_Main : Boolean;
      --  Whether the unit is a main

      Instr_Project : GPR2.Project.View.Object;
      --  Project in which instrumentation artifacts for this unit are
      --  generated.

      Language_Kind : Supported_Language_Kind;
      --  Higher level representation of a language (unit-based or file-based)

      Language : Src_Supported_Language;
      --  Actual language representation

      Spec_Project, Body_Project : GPR2.Project.View.Object;
      --  Track the owning project of this unit's spec source file (if present)
      --  and body source file (likewise).

      Sources : Source_Sets.Set;
      --  Set of sources that belong to this unit

   end record;

   package Unit_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Library_Unit_Info);
   --  Map to unit names to unit info of files implementing this unit. For
   --  file-based languages, the unit name is the full name (to simplify
   --  dealing with homonym in different projects).

   function SID_Filename
     (Main_Part_Src : GPR2.Build.Source.Object; In_Library_Dir : Boolean)
      return String;
   --  Return the filename of the SID file to create for the given compilation
   --  unit. If In_Library_Dir is True and the the unit lives in a library
   --  project, then return a filename located in the project library
   --  directory. Otherwise, the filename is located in the object directory.

   function Compilation_Unit_Options
     (IC   : Inst_Context;
      Prj  : Prj_Desc;
      Lang : Src_Supported_Language;
      Src  : GPR2.Build.Source.Object) return Command_Line_Args;
   --  Return the list of options to pass to a gnatcov instrument-source /
   --  instrument-main for the given compilation unit Unit_Name, belonging to
   --  the project Prj.

   function Load_From_Project (Prj : GPR2.Project.View.Object) return Prj_Desc;
   --  Load the project description from the given project

   function Get_Or_Create_Project_Info
     (Context : in out Inst_Context; Project : GPR2.Project.View.Object)
      return Project_Info_Access;
   --  Return the Project_Info record corresponding to Project. Create it if it
   --  does not exist.

   function Instrumentation_Artifacts
     (Main_Part_Src : GPR2.Build.Source.Object; Prj : Prj_Desc)
      return File_Sets.Set;
   --  Return the list of instrumentation artifacts generated by the source
   --  instrumentation of Unit. This is used by incremental instrumentation,
   --  to clean up instrumentation artifacts not needed by the current
   --  instrumentation run.

end Instrument.GPR_Utils;
