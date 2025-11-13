------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;  use Ada.Directories;

with GNAT.OS_Lib;

with Instrument.Setup_Config; use Instrument.Setup_Config;
with Outputs;                 use Outputs;
with Paths;                   use Paths;
with Strings;                 use Strings;
with Subprocesses;            use Subprocesses;

procedure Compiler_Wrappers.Gcc is

   --  Find the the instrumentation configuration that "setup-integration"
   --  generated along with this compiler wrapper in the Prefix directory.

   Prefix          : constant String :=
     Containing_Directory (GNAT.OS_Lib.Locate_Exec_On_Path (Command_Name).all);
   Config_Filename : constant String :=
     Prefix / Instrumentation_Config_Filename;

   --  Delegate the actual work to the "gnatcov gcc-wrapper" command

   Gnatcov_Exec : constant String :=
     "gnatcov" & GNAT.OS_Lib.Get_Executable_Suffix.all;
   Args         : String_Vectors.Vector;

   --  Load the configuration to enable verbose output if required

   Dummy : Instrumentation_Config := Load_Config (Config_Filename);
begin
   Args.Append (+"gcc-wrapper");
   Args.Append (+Config_Filename);
   Args.Append (+Command_Name);
   Args.Append (+"--cargs");
   for I in 1 .. Argument_Count loop
      Args.Append (+Argument (I));
   end loop;

   Run_Command
     (Command             => Gnatcov_Exec,
      Arguments           => Args,
      Origin_Command_Name => "gnatcov gcc-wrapper");

exception
   when Xcov_Exit_Exc =>
      --  An error message has already been displayed

      null;
end Compiler_Wrappers.Gcc;
