------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2022, AdaCore                     --
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

with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Projects; use GNATCOLL.Projects;

with Paths;   use Paths;
with Project;

procedure Instrument.Clean_Objdirs (IC : Inst_Context) is
   use Project_Info_Maps;

   All_Instr_Files : File_Sets.Set;
   --  Set of full names for all files that were written to output directories
   --  for all instrumented projects.

   procedure Clean_Subdir (Project : Project_Type);
   --  Callback for Project.Iterate_Projects. If Project is not externally
   --  built, remove all files from the "$project_name-gnatcov-instr" folder in
   --  Project's object directory that we did not just instrument.

   ------------------
   -- Clean_Subdir --
   ------------------

   procedure Clean_Subdir (Project : Project_Type) is
      Output_Dir : constant String := Project_Output_Dir (Project);

      --  Removing items in a directory and iterate on these items at the same
      --  time is not supported: first collect all files to remove (iteration)
      --  and then remove them.

      To_Delete  : File_Sets.Set;
      Search     : Search_Type;
      Dir_Entry  : Directory_Entry_Type;
   begin
      if
         --  Leave externally built projects out of the picture

         Project.Externally_Built

         --  Some projects don't have an object directory: ignore them as there
         --  is nothing to do.

         or else Output_Dir'Length = 0
         or else not Exists (Output_Dir)
         or else Kind (Output_Dir) /= Directory
      then
         return;
      end if;

      --  Collect the files to delete

      Start_Search
        (Search,
         Directory => Output_Dir,
         Pattern   => "",
         Filter    => (Ordinary_File => True, others => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Dir_Entry);
         declare
            Name      : constant String := Simple_Name (Dir_Entry);
            Full_Name : constant Unbounded_String :=
              To_Unbounded_String (Output_Dir / Name);
         begin
            if not All_Instr_Files.Contains (Full_Name) then
               To_Delete.Insert (Full_Name);
            end if;
         end;
      end loop;
      End_Search (Search);

      --  Do the deletion

      for Name of To_Delete loop
         Delete_File (To_String (Name));
      end loop;
   end Clean_Subdir;

--  Start of processing for Instrument.Clean_Objdirs

begin
   --  First initialize All_Instr_Files

   for Cur in IC.Project_Info_Map.Iterate loop
      All_Instr_Files.Union (Element (Cur).Instr_Files);
   end loop;

   --  Then go through all projects to clear up their object directories,
   --  ignoring files in All_Instr_Files. Also go through extended projects, as
   --  their object directories can interfere with the build of the extending
   --  project.

   Project.Iterate_Projects
     (Root_Project     => Project.Project.Root_Project,
      Process          => Clean_Subdir'Access,
      Recursive        => True,
      Include_Extended => True);
end Instrument.Clean_Objdirs;
