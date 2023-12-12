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

with GNATCOLL.Projects; use GNATCOLL.Projects;

with Instrument.Common; use Instrument.Common;
with Project;

procedure Instrument.Clean_Objdirs is

   procedure Clean_Subdir (Project : Project_Type);
   --  Callback for Project.Iterate_Projects. If Project is not externally
   --  built, remove all files from the "$project_name-gnatcov-instr" folder in
   --  Project's object directory.

   ------------------
   -- Clean_Subdir --
   ------------------

   procedure Clean_Subdir (Project : Project_Type) is
      Output_Dir : constant String := Project_Output_Dir (Project);
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

      Delete_Tree (Directory => Output_Dir);
   end Clean_Subdir;

--  Start of processing for Instrument.Clean_Objdirs

begin
   --  Go through all projects to clear up their object directories. Also go
   --  through extended projects, as their object directories can interfere
   --  with the build of the extending project.

   if not Save_Temps then
      Project.Iterate_Projects
        (Root_Project     => Project.Project.Root_Project,
         Process          => Clean_Subdir'Access,
         Recursive        => True,
         Include_Extended => True);
   end if;
end Instrument.Clean_Objdirs;
