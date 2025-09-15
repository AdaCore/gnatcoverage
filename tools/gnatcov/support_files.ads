------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2020-2024, AdaCore                     --
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
with Ada.Directories;           use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;

with GNAT.OS_Lib;

package Support_Files is
   use type GNAT.OS_Lib.String_Access;

   --  This package offers various helpers to locate support files in gnatcov's
   --  installation prefix.

   Prefix_Envvar : constant String := "GNATCOV_PREFIX";
   --  Name of the environment variable which, when defined, provides the
   --  directory that contains the installation prefix for gnatcov.

   Command_Name_Envvar : constant String := "GNATCOV_CMD";
   --  Name of the environment variable which, when defined, provides the name
   --  of the "gnatcov" command that the user ran.

   Not_Found : constant String := "GNATCOV_NOT_FOUND";
   --  Magic value propagated throughout the elaboration if we can't lookup
   --  gnatcov on PATH. An error will be reported after elaboration completes.

   Gnatcov_Command_Name : String :=
     (if Value (Command_Name_Envvar, "") = ""
      then Command_Name
      else Value (Command_Name_Envvar));

   Gnatcov_Exec : constant GNAT.OS_Lib.String_Access :=
     GNAT.OS_Lib.Locate_Exec_On_Path (Command_Name);

   Gnatcov_Dir : constant String :=
     (if Gnatcov_Exec /= null
      then Containing_Directory (Gnatcov_Exec.all)
      else Not_Found);
   --  Use a magic value when we can't locate gnatcov on path

   Gnatcov_Prefix : constant String :=
      (if Value (Prefix_Envvar, "") = ""
       then
         (if Gnatcov_Dir /= Not_Found
          then Containing_Directory (Gnatcov_Dir)
          else Not_Found)
       else Value (Prefix_Envvar));
   --  Installation prefix for gnatcov

   Lib_Dir : constant String := Gnatcov_Prefix & "/lib/gnatcoverage";
   --  GNATcoverage-specific directory in the installation prefix's "lib"
   --  directory.

   Libexec_Dir : constant String := Gnatcov_Prefix & "/libexec/gnatcoverage";
   --  GNATcoverage-specific directory in the installation prefix's "libexec"
   --  directory.

   Share_Dir : constant String := Gnatcov_Prefix & "/share/gnatcoverage";
   --  GNATcoverage-specific directory in the installation prefix's "share"
   --  directory.

   function In_Lib_Dir (File : String) return String is
     (Lib_Dir & "/" & File);
   --  Helper to build the name of a file inside the lib directory

   function In_Libexec_Dir (File : String) return String is
     (Libexec_Dir & "/" & File);
   --  Helper to build the name of a file inside the libexec directory

   function In_Share_Dir (File : String) return String is
     (Share_Dir & "/" & File);
   --  Helper to build the name of a file inside the share directory

end Support_Files;
