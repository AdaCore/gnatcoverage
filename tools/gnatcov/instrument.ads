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

--  Support for source instrumentation

with Ada.Strings.Unbounded;

with GNAT.Regexp;

package Instrument is

   package US renames Ada.Strings.Unbounded;

   type Any_Dump_Trigger is
     (Manual, At_Exit, Ravenscar_Task_Termination, Main_End);
   --  Trigger to dump coverage buffers in instrumented programs. See the user
   --  documentation for the --dump-trigger command-line option.

   subtype Auto_Dump_Trigger is Any_Dump_Trigger range At_Exit .. Main_End;

   type Any_Dump_Channel is (Binary_File, Base64_Standard_Output);
   --  Channel where to dump coverage buffers. See the user documentation for
   --  the --dump-channel command-line option.

   type Any_Dump_Config (Channel : Any_Dump_Channel := Any_Dump_Channel'First)
   is record
      Trigger : Any_Dump_Trigger := Manual;

      case Channel is
         when Binary_File =>
            Filename_Simple : Boolean := False;
            --  Whether to generate source traces with simple filenames.
            --
            --  Controlled by --dump-filename-simple.

            Filename_Env_Var : US.Unbounded_String;
            --  Name of the environment variable which, if set, contains the
            --  default filename for created source traces. If empty, use the
            --  default one (see Default_Trace_Filename_Env_Var in
            --  GNATcov_RTS.Traces.Output.Files).
            --
            --  Controlled by --dump-filename-env-var.

            Filename_Prefix  : US.Unbounded_String;
            --  Prefix for the source trace filename. If empty, use the
            --  program's basename (see Default_Trace_Filename_Prefix in
            --  GNATcov_RTS.Traces.Output.Files).
            --
            --  Controlled by --dump-filename-prefix.
         when others =>
            null;
      end case;
   end record;
   --  Bundle for all configuration related to automatic dump of coverage
   --  buffers.

   type Any_Language_Version is (Ada_83, Ada_95, Ada_2005, Ada_2012);

   type Any_Language is (Ada_Language, C_Language);

   procedure Instrument_Units_Of_Interest
     (Dump_Config          : Any_Dump_Config;
      Language_Version     : Any_Language_Version;
      Ignored_Source_Files : access GNAT.Regexp.Regexp);
   --  Generate instrumented sources for the source files of all units of
   --  interest. Also save mappings between coverage buffers and SCOs for each
   --  library units to SID files (one per library unit).
   --
   --  Depending on Dump_Config, instrument mains to schedule a call to the
   --  dump procedure for the list of coverage buffers in all mains in the
   --  project.
   --
   --  Language_Version restricts what source constructs the instrumenter is
   --  allowed to use. For instance, if Ada_2005 (or a lower version) is
   --  passed, it will not be allowed to introduce expression functions, and
   --  thus will emit a warning when it needed to do so.
   --
   --  If Ignored_Source_File is non-null, ignore files whose names match the
   --  accessed pattern.

end Instrument;
