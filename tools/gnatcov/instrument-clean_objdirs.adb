------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2020, AdaCore                     --
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

with Ada.Containers.Ordered_Maps;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Projects; use GNATCOLL.Projects;

with Project;

procedure Instrument.Clean_Objdirs (IC : Inst_Context) is
   use Project_Info_Maps;

   --  The removal happens in two steps:
   --
   --  * First we list all directories in the project tree that can contain
   --    instrumented sources from previous runs of "gnatcov instrument".
   --    During this step, we also take note of which files this run
   --    generated (obviously we must not remove these).
   --
   --  * Then we go through these directories and remove all other files.
   --
   --  Having these steps is necessary since multiple projects can use the
   --  same object directory: we must be aware of all uses of each object
   --  directory (and in particular of all the instrumented source files to
   --  keep in them) before starting to remove files.
   --
   --  Note that we never delete anything from the object directory of
   --  externally built projects. It's the users' responsibility to instrument
   --  them appropriately (or not instrument them).

   type File_Set is access all File_Sets.Set;
   procedure Free is new Ada.Unchecked_Deallocation
     (File_Sets.Set, File_Set);

   package Preserved_Files_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => File_Set);
   Preserved_Files_Map : Preserved_Files_Maps.Map;
   --  Map object directories to the list of files in these directories not
   --  to be removed.

   function Register_Output_Dir
     (Output_Dir : Unbounded_String) return File_Set;
   --  Schedule the removal of files in Output_Dir and return the set of
   --  files to preserve associated to it.

   procedure Register_Project (Project : Project_Type);
   --  Callback for Project.Iterate_Projects. If Project is not externally
   --  built, schedule the removal of files from the "gnatcov-instr" folder in
   --  Project's object directory.

   procedure Remove_Files
     (Output_Dir      : String;
      Preserved_Files : File_Sets.Set);
   --  Remove all files in Output_Dir which are not present in
   --  Preserved_Files.

   -------------------------
   -- Register_Output_Dir --
   -------------------------

   function Register_Output_Dir
     (Output_Dir : Unbounded_String) return File_Set
   is
      Cur : constant Preserved_Files_Maps.Cursor :=
         Preserved_Files_Map.Find (Output_Dir);
   begin
      if Preserved_Files_Maps.Has_Element (Cur) then
         return Preserved_Files_Maps.Element (Cur);
      else
         return Preserved_Files : constant File_Set := new File_Sets.Set do
            Preserved_Files_Map.Insert (Output_Dir, Preserved_Files);
         end return;
      end if;
   end Register_Output_Dir;

   ----------------------
   -- Register_Project --
   ----------------------

   procedure Register_Project (Project : Project_Type) is
   begin
      --  Leave externally built projects out of the picture

      if Project.Externally_Built then
         return;
      end if;

      --  Plan to clean this project's output directory

      declare
         Output_Dir : constant Unbounded_String :=
            To_Unbounded_String (Project_Output_Dir (Project));
         Preserved_Files : constant File_Set :=
            Register_Output_Dir (Output_Dir);

         --  Lookup instrumentation information about this project

         Name : constant Unbounded_String :=
            To_Unbounded_String (Project.Name);
         Cur  : constant Cursor := IC.Project_Info_Map.Find (Name);
      begin
         if Has_Element (Cur) then

            --  We just instrumented sources for this project: preserve the
            --  generated source files (i.e. remove only obsolete instrumented
            --  sources).

            declare
               Prj_Info : Project_Info renames Element (Cur).all;
            begin
               Preserved_Files.Union (Prj_Info.Instr_Files);
            end;
         end if;
      end;
   end Register_Project;

   ------------------
   -- Remove_Files --
   ------------------

   procedure Remove_Files
     (Output_Dir      : String;
      Preserved_Files : File_Sets.Set)
   is
      To_Delete : File_Sets.Set;
      Search    : Search_Type;
      Dir_Entry : Directory_Entry_Type;
   begin
      --  Some projects don't have an object directory: ignore them as there
      --  is nothing to do.

      if Output_Dir'Length = 0
         or else not Exists (Output_Dir)
         or else Kind (Output_Dir) /= Directory
      then
         return;
      end if;

      Start_Search
        (Search,
         Directory => Output_Dir,
         Pattern   => "",
         Filter    => (Ordinary_File => True, others => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Dir_Entry);
         declare
            Name    : constant String := Simple_Name (Dir_Entry);
            UB_Name : constant Unbounded_String :=
               To_Unbounded_String (Name);
         begin
            if not Preserved_Files.Contains (UB_Name) then
               To_Delete.Insert (UB_Name);
            end if;
         end;
      end loop;
      End_Search (Search);

      for Name of To_Delete loop
         Delete_File (Output_Dir / To_String (Name));
      end loop;
   end Remove_Files;

--  Start of processing for Instrument.Clean_Objdirs

begin
   --  Look for source files we should remove in all loaded projects'
   --  "gnatcov-instr" directory.

   Project.Iterate_Projects
     (Root_Project => Project.Project.Root_Project,
      Process      => Register_Project'Access,
      Recursive    => True);

   --  We can now remove these files

   for Cur in Preserved_Files_Map.Iterate loop
      declare
         use Preserved_Files_Maps;
         Output_Dir : constant String := To_String (Key (Cur));
         Files      : File_Set := Element (Cur);
      begin
         Remove_Files (Output_Dir, Files.all);
         Free (Files);
      end;
   end loop;
end Instrument.Clean_Objdirs;