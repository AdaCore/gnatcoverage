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

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Files_Handling; use Files_Handling;

package Instrument.Setup_Config is

   type Instrumentation_Config is record
      Dump_Config : Any_Dump_Config;

      Compiler_Drivers : String_Maps.Map;
      --  Mapping from compiler driver simple names to full names

      Linkers : String_Maps.Map;
      --  Mapping from compiler driver simple names to associated linker
      --  executable

      Nms : String_Maps.Map;
      --  Mapping from compiler driver simple names to associated nm
      --  executable

      Tag : Unbounded_String;
      --  Tag for this instrumentation run

      GNATcov_RTS_Include_Dir : Unbounded_String;
      GNATcov_RTS_Object_Dir  : Unbounded_String;
      --  Location of the include / object directories of the installed
      --  gnatcov_rts. This is used by compiler wrappers to compile / link
      --  instrumented sources in the instrumented main executable. For now,
      --  we always link it statically.

      File_To_SID : File_To_String_Maps.Map;
      --  Maps a source file to its SID basename

   end record;

   Instrumentation_Config_Filename : constant String := "gnatcov_config.json";
   --  Simple name of the file containing the instrumentation configuration

   Every_File_Of_Interest : Boolean := False;
   --  Whether every file is of interest or if it the list of files of
   --  interest was specified by the user (using the --files switch).

   procedure Generate_Config
     (Files_Of_Interest : File_Sets.Set;
      Coverage_Level    : String;
      Dump_Config       : Any_Dump_Config;
      Compiler_Drivers  : String_Sets.Set;
      Output_Dir        : String;
      Runtime_Project   : String);
   --  Setup the configuration to use the integrated-mode of gnatcov.
   --
   --  Files_Of_Interest is the set of files that needs to be instrumented as
   --  units of interest.
   --
   --  Coverage_Level is the coverage level for which to instrument the files
   --  of interest.
   --
   --  Dump_Config is the dump configuration to use for the sources to
   --  instrument.
   --
   --  Compiler_Drivers is the set of names for the compiler drivers to wrap
   --  for instrumentation.
   --
   --  Output_Dir is the name of the directory in which the compiler wrappers
   --  and instrumentation artifacts are put.
   --
   --  Runtime_Project is the name of the coverage runtime project that was
   --  installed by "gnatcov setup".

   function Load_Config (Config_File : String) return Instrumentation_Config;
   --  Read the configuration file that "gnatcov setup-integration" created and
   --  return the instrumentation configuration that is decoded from it.

end Instrument.Setup_Config;
