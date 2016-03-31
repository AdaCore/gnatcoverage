------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2014, AdaCore                     --
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

private package Rundrv.Config is

   --  Configuration tables for Rundrv. This controls what gnatcov run does
   --  for a given --target argument when there is no <target>-gnatemulator in
   --  sight.

   --  There are two major tables exposed:
   --
   --  * A table of <target> -> commands associations, the Drivers array below
   --
   --  * A table of <target-alias> -> <target> pairs, the Aliases array below

   --  Each target has a Run command, performing the actual execution, expected
   --  to produce the execution trace.
   --
   --  Each command is allowed to be passed a sequence of arguments, described
   --  in the tables as well.

   --  A few builtin macros are available for Commands or Arguments. At most
   --  one reference per command or argument entry is allowed. For arguments,
   --  this at most one per item in the argument list for a configuration.
   --
   --  See the Rundrv.Expander unit spec for a description of the available
   --  macros.

   ----------------------------------
   -- Driver Target Configurations --
   ----------------------------------

   type Driver_Target is record
      --  Name of the target (triplet)

      Target        : String_Access;

      --  Run command and option list

      Run_Command   : String_Access;
      Run_Options   : String_List_Access;
   end record;

   type Driver_Target_Array is
     array (Natural range <>) of aliased Driver_Target;

   Drivers : constant Driver_Target_Array :=
     ((Target => new String'("(i686|x86_64).*linux"),
       Run_Command => new String'("%valgrind"),
       Run_Options => new String_List'(new String'("%set_valgrind_env"),
                                       new String'("--quiet"),
                                       new String'("--tool=coverage"),
                                       new String'("--cov-exec-file=%trace"),
                                       new String'("%exe"))
      ),
      (Target => new String'("(i686|x86_64).*mingw"),
       Run_Command => new String'("%drrun"),
       Run_Options => new String_List'(new String'("-quiet"),
                                       new String'("-no_follow_children"),
                                       new String'("-c"),
                                       new String'("%drclient"),
                                       new String'("-o"),
                                       new String'("%trace"),
                                       new String'("--"),
                                       new String'("%exe"))

       --  -quiet silences the warnings emitted by DynamoRIO on the assumption
       --  that it is invoked from an official release install tree.
      ),
      (Target => new String'("iSystem-5554"),
       Run_Command => new String'("../libexec/gnatcoverage/isys_drv"),
       Run_Options => new String_List'(
         new String'("5554"),
         new String'("%exe"),
         new String'("%trace")
        )
       ),
      (Target => new String'("iSystem-5634"),
       Run_Command => new String'("../libexec/gnatcoverage/isys_drv"),
       Run_Options => new String_List'(
         new String'("5634"),
         new String'("%exe"),
         new String'("%trace")
       )
      ),
      (Target => new String'("visium-elf"),
       Run_Command => new String'("visium-elf-run"),
       Run_Options => new String_List'(new String'("--trace=%tracefile"),
                                       new String'("%exe"))
      )
     );

end Rundrv.Config;
