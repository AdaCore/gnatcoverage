------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Text_IO;               use Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.Strings; use GNAT.Strings;

with Command_Line; use Command_Line;
with Outputs;      use Outputs;
with Project;
with Support_Files;
with Switches;     use Switches;

procedure GNATcov is
   Target_64_Bit : Boolean := False;
begin
   --  Parse arguments just enough to determine the target, then run the
   --  corresponding bits-specific main (see gnatcov_arch_specific.adb),
   --  forwarding all arguments as-is.

   Parse_Arguments (From_Driver => True);
   Verbose := Args.Bool_Args (Opt_Verbose);

   --  Determine if the target is 32-bit or 64-bit: consider it's 32-bit unless
   --  we can find the "64" substring before the first dash.

   declare
      Target : String renames Target_Family.all;

      Last_Before_Dash : Natural;
      --  Index of the last character in Target before the first dash ('-'), or
      --  Target'Last if there is no dash.
   begin
      Last_Before_Dash := Index (Target, "-");
      if Last_Before_Dash = 0 then
         Last_Before_Dash := Target'Last;
      else
         Last_Before_Dash := Last_Before_Dash - 1;
      end if;

      Target_64_Bit :=
         Index (Target (Target'First .. Last_Before_Dash), "64") /= 0;
   end;

   --  Now run the correct arch-specific entry point

   declare
      Exec_Basename : constant String :=
         "gnatcov"
         & (if Target_64_Bit then "64" else "32")
         & GNAT.OS_Lib.Get_Executable_Suffix.all;
      Exec_Filename : constant String := Ada.Directories.Compose
        (Support_Files.Libexec_Dir, Exec_Basename);

      Args : String_List (1 .. Argument_Count);

      Success : Boolean;
   begin
      if Verbose then
         Put_Line ("Running: " & Exec_Filename);
      end if;

      --  Make sure that the arch-specific entry point knows what the prefix is
      --  so that it can locale support files. Also make sure that the name of
      --  the command that users ran is propagated.

      Set (Support_Files.Prefix_Envvar, Support_Files.Gnatcov_Prefix);
      Set (Support_Files.Command_Name_Envvar,
           Support_Files.Gnatcov_Command_Name);

      --  Just copy the command line

      for I in 1 .. Argument_Count loop
         Args (I) := new String'(Argument (I));
      end loop;

      GNAT.OS_Lib.Spawn (Exec_Filename, Args, Success);

      if not Success then
         Set_Exit_Status (Failure);

         --  Spawn's Success argument does not allow us to distinguish between
         --  "spawning the subprocess failed" (for instance: no such
         --  executable) and "we could run the executable, yet the subprocess
         --  exitted with an error". To avoid overly verbose output in the
         --  latter case (the bits-specific entry point is already supposed to
         --  print an explicit error message), restrict the following message
         --  to the verbose mode.

         if Verbose then
            Put_Line ("Could not spawn " & Exec_Filename & ": aborting");
         end if;
      end if;
   end;
exception
   when Xcov_Exit_Exc =>
      --  An error message has already been displayed

      Project.Finalize;
end GNATcov;
