------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2021, AdaCore                     --
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

with System_Commands; use System_Commands;

--  Driver selection configuration for Rundrv.
--
--  This package provides types to determine what gnatcov run executes for
--  a given target and what "built-in" targets (i.e. non GNATemulator-based)
--  are supported. The Rundrv.Handlers package provides the corresponding
--  constructors.

package Rundrv.Config is

   type Context_Type is record
      Exe_File      : String_Access;
      --  Filename for the executable to run. Must not be null.

      Target_Family : String_Access;
      --  Target triplet for the execution environment. Computed from the
      --  --target command-line argument, or from the native target otherwise.
      --  Must not be null.

      Target_Board  : String_Access;
      --  Board name, or null if irrelevant. Computed from the --target
      --  command-line argument.

      Kernel        : String_Access;
      --  Filename for the kernel to use, or null if irrevelant. Computed from
      --  the --kernel command-line argument.

      Histmap       : String_Access;
      --  Filename for history map description (useful for MC/DC), or null if
      --  irrelevant.

      Trace_File    : String_Access;
      --  Filename for the output trace file. Must not be null.

      Eargs         : String_List_Access;
      --  List of additional arguments to pass to the executable to run
   end record;
   --  Holder for various information used to instantiate a command to run

   function Available_Targets return String;
   --  Return a list of available targets

   function Lookup_Driver (Context : Context_Type) return Command_Access;
   --  Create a command to run in order to produce a trace for Context.
   --  Return null if we cannot figure out what command to run.
   --
   --  KERNEL is the command line --kernel argument, if any. TARGET is the
   --  command line --target value, which may feature an optional board
   --  specification (e.g. --target=powerpc-elf,prep).
   --
   --  This will be a <target>-gnatemu block if GNATemulator is
   --  available on PATH, or a low-level emulator block from our static
   --  configuration table otherwise.
   --
   --  The computation is split across the two helpers below.  TARGET_FAMILY
   --  and TARGET_BOARD are set to the base target and board extension of the
   --  original TARGET input.

end Rundrv.Config;
