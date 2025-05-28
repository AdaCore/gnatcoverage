------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Ada.Directories; use Ada.Directories;

with GPR2.Path_Name;

with Instrument.Common; use Instrument.Common;
with Outputs;           use Outputs;
with Project;           use Project;

procedure Instrument.Clean_Objdirs is

   procedure Clean_Subdir (Project : GPR2.Project.View.Object);
   --  Callback for Project.Iterate_Projects. If Project is not externally
   --  built, remove all files from the "$project_name-gnatcov-instr" folder in
   --  Project's object directory, and remove all files ending in ".sid" from
   --  the object and library directory, if applicable.

   function Has_Regular_Files (Directory : String) return Boolean;
   --  Return whether Directory contains at least one regular file

   ------------------
   -- Clean_Subdir --
   ------------------

   procedure Clean_Subdir (Project : GPR2.Project.View.Object) is
      Output_Dir     : constant String := Project_Output_Dir (Project);
      Has_Output_Dir : Boolean := True;
   begin
      Clean_Objdirs_Trace.Increase_Indent
        ("Processing " & String (Project.Name));
      Clean_Objdirs_Trace.Trace
        ("GPR file: " & String (Project.Path_Name.Value));

      --  Some projects don't have an object directory: ignore them as there is
      --  nothing to do.

      if Output_Dir'Length = 0 then
         Has_Output_Dir := False;
         Clean_Objdirs_Trace.Trace ("no object directory");
      else
         Clean_Objdirs_Trace.Trace ("output directory: " & Output_Dir);
         if not Exists (Output_Dir) then
            Has_Output_Dir := False;
            Clean_Objdirs_Trace.Trace ("it does not exist");
         elsif Kind (Output_Dir) /= Directory then
            Has_Output_Dir := False;
            Clean_Objdirs_Trace.Trace ("it is not a directory");
         end if;
      end if;

      if not Has_Output_Dir then
         Clean_Objdirs_Trace.Decrease_Indent;
         return;
      end if;

      --  Unless externally built project processing has been enabled, having
      --  instrumented sources in externally built projects is very suspicious
      --  and could easily trigger build errors later on.
      --
      --  For instance, if:
      --
      --  * project P depends on project Lib
      --  * Lib is instrumented
      --  * P is instrumented while Lib is tagged as externally built
      --
      --  then auto-dump code for P's main will not try to dump coverage
      --  buffers for Lib, and thus the link of P's main executable will fail
      --  because Lib's units refer to their coverage buffers, which are not
      --  included in the link.
      --
      --  To avoid confusion, warn about such cases now, so that users have a
      --  clear lead on how to address such problems.

      if Project.Is_Externally_Built then
         Clean_Objdirs_Trace.Trace ("it is an externally built project");

         if not Externally_Built_Projects_Processing_Enabled
            and then Has_Regular_Files (Output_Dir)
         then
            --  Short of re-implementing non trivial project handling logic,
            --  there is no API that allows us to determine whether files in
            --  Output_Dir would be considered as sources with --src-subdirs:
            --  consider that all files there could be sources.

            Warn ("Project """ & String (Project.Name) & """ is externally"
                  & " built and does not contain units of interest, however it"
                  & " contains instrumented sources");
         end if;

         --  We should never try to modify externally built projects, so do not
         --  remove their instrumented source directory.

         Clean_Objdirs_Trace.Decrease_Indent;
         return;
      end if;

      Delete_Tree (Directory => Output_Dir);

      --  Remove the SID files if any

      if Project.Kind in GPR2.With_Object_Dir_Kind
        and then Project.Object_Directory.Exists
      then
         Clean_Dir
           (Project.Object_Directory.String_Value, Pattern => "*.sid");
      end if;
      if Project.Kind in GPR2.Library_Kind
        and then Project.Library_Directory.Exists
      then
         Clean_Dir
           (Project.Library_Directory.String_Value, Pattern => "*.sid");
      end if;

      Clean_Objdirs_Trace.Decrease_Indent;
   end Clean_Subdir;

   -----------------------
   -- Has_Regular_Files --
   -----------------------

   function Has_Regular_Files (Directory : String) return Boolean is
      S : Search_Type;
   begin
      return Result : Boolean do
         Start_Search
           (Search    => S,
            Directory => Directory,
            Pattern   => "",
            Filter    => (Ordinary_File => True, others => False));
         Result := More_Entries (S);
         End_Search (S);
      end return;
   end Has_Regular_Files;

--  Start of processing for Instrument.Clean_Objdirs

begin
   --  Go through all projects to clear up their object directories. Also go
   --  through extended projects, as their object directories can interfere
   --  with the build of the extending project.

   if not Save_Temps then
      Iterate_Projects
        (Root_Project             => Project.Project.Root_Project,
         Process                  => Clean_Subdir'Access,
         Recursive                => True,
         Include_Extended         => True,
         Include_Externally_Built => True);
   end if;
end Instrument.Clean_Objdirs;
