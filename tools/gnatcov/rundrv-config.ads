------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
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

with Subprocesses; use Subprocesses;

--  Driver selection configuration for Rundrv.
--
--  This package provides types to determine what gnatcov run executes for
--  a given target and what "built-in" targets (i.e. non GNATemulator-based)
--  are supported. The Rundrv.Handlers package provides the corresponding
--  constructors.

package Rundrv.Config is

   type Context_Type is record
      Exe_File : String_Access;
      --  Filename for the executable to run. Must not be null.

      Target_Family : String_Access;
      --  Target triplet for the execution environment. Computed from the
      --  --target command-line argument, or from the native target otherwise.
      --  Must not be null.

      Target_Board : String_Access;
      --  Board name, or null if irrelevant. Computed from the --target
      --  command-line argument.

      Kernel : String_Access;
      --  Filename for the kernel to use, or null if irrevelant. Computed from
      --  the --kernel command-line argument.

      Histmap : String_Access;
      --  Filename for history map description (useful for MC/DC), or null if
      --  irrelevant.

      Trace_File : String_Access;
      --  Filename for the output trace file. Must not be null.

      Eargs : String_List_Access;
      --  List of additional arguments to pass to the executable to run
   end record;
   --  Holder for various information used to instantiate a command to run

   function Available_Targets return String;
   --  Return a list of available targets

   procedure Lookup_Driver
     (Context : Context_Type;
      Found   : out Boolean;
      Cmd     : out Command_Type;
      Native  : out Boolean);
   --  Look for a command to run in order to produce a trace for Context.
   --
   --  Set Found to whether we manage to find an appropriate command. The other
   --  arguments are considered uninitialized if Found is False.
   --
   --  Cmd is set to the command that is found (if any). Native is set to
   --  whether this is to run a native program (i.e. a program that runs on the
   --  same host as gnatcov itself).
   --
   --  The command will be a <target>-gnatemu block if GNATemulator is
   --  available on PATH, or a low-level emulator block from our static
   --  configuration table otherwise.

end Rundrv.Config;
