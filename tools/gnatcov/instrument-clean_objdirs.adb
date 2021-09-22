------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2021, AdaCore                     --
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

with Project;
with Strings; use Strings;

procedure Instrument.Clean_Objdirs (IC : Inst_Context) is
   use Project_Info_Maps;

   procedure Clean_Subdir (Project : Project_Type);
   --  Callback for Project.Iterate_Projects. If Project is not externally
   --  built, remove all files from the "$project_name-gnatcov-instr" folder in
   --  Project's object directory that we did not just instrument.

   procedure Remove_Files
     (Output_Dir : String; Preserved_Files : File_Sets.Set);
   --  Remove all files in Output_Dir which are not present in
   --  Preserved_Files.

   ------------------
   -- Clean_Subdir --
   ------------------

   procedure Clean_Subdir (Project : Project_Type) is
   begin
      --  Leave externally built projects out of the picture

      if Project.Externally_Built then
         return;
      end if;

      --  Plan to clean this project's output directory

      declare
         Output_Dir : constant String := Project_Output_Dir (Project);

         --  Lookup instrumentation information about this project

         Name : constant Unbounded_String :=
            To_Unbounded_String (Project.Name);
         Cur  : constant Cursor := IC.Project_Info_Map.Find (Name);
      begin
         if Has_Element (Cur) then

            --  We just instrumented sources for this project: preserve the
            --  generated source files (i.e. remove only obsolete instrumented
            --  sources).

            Remove_Files (Output_Dir, Element (Cur).Instr_Files);
         else
            Remove_Files (Output_Dir, File_Sets.Empty_Set);
         end if;
      end;
   end Clean_Subdir;

   ------------------
   -- Remove_Files --
   ------------------

   procedure Remove_Files
     (Output_Dir : String; Preserved_Files : File_Sets.Set)
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
   Project.Iterate_Projects
     (Root_Project => Project.Project.Root_Project,
      Process      => Clean_Subdir'Access,
      Recursive    => True);
end Instrument.Clean_Objdirs;
