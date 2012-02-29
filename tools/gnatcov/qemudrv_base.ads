------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

with GNAT.Strings; use GNAT.Strings;
with GNAT.OS_Lib;

package Qemudrv_Base is

   type Driver_Target is record
      --  Name of the target (triplet)

      Target        : String_Access;

      --  Post-build command and option list (may be null)

      Build_Command : String_Access;
      Build_Options : String_List_Access;

      --  Run command and option list

      Run_Command   : String_Access;
      Run_Options   : String_List_Access;
   end record;

   type Driver_Target_Array is
     array (Natural range <>) of aliased Driver_Target;

   Drivers : constant Driver_Target_Array :=
     ((Target => new String'("qemu-prep"),
       Build_Command => null,
       Build_Options => null,
       Run_Command => new String'("qemu-system-ppc"),
       Run_Options => new String_List'(new String'("-nographic"),
                                       new String'("-M"),
                                       new String'("prep"),
                                       new String'("-boot"),
                                       new String'("n"),
                                       new String'("-no-reboot"),
                                       new String'("-L"),
                                       new String'("%dir_exe"),
                                       new String'("-bios"),
                                       new String'("-"),
                                       new String'("-kernel"),
                                       new String'("%exe"),
                                       new String'("-exec-trace"),
                                       new String'("%trace"))
      ),
      (Target => new String'("qemu-sbc834x"),
       Build_Command => null,
       Build_Options => null,
       Run_Command => new String'("qemu-system-ppc"),
       Run_Options => new String_List'(new String'("-nographic"),
                                       new String'("-M"),
                                       new String'("SBC834x"),
                                       new String'("-no-reboot"),
                                       new String'("-kernel"),
                                       new String'("%exe"),
                                       new String'("-exec-trace"),
                                       new String'("%trace"))
      ),
      (Target => new String'("leon-elf"),
       Build_Command => null,
       Build_Options => null,
       Run_Command => new String'("qemu-system-sparc"),
       Run_Options => new String_List'(new String'("-nographic"),
                                       new String'("-M"),
                                       new String'("at697"),
                                       new String'("-kernel"),
                                       new String'("%exe"),
                                       new String'("-exec-trace"),
                                       new String'("%trace"))

      ),
      (Target => new String'("erc32-elf"),
       Build_Command => null,
       Build_Options => null,
       Run_Command => new String'("qemu-system-sparc"),
       Run_Options => new String_List'(new String'("-nographic"),
                                       new String'("-M"),
                                       new String'("tsc695"),
                                       new String'("-kernel"),
                                       new String'("%exe"),
                                       new String'("-exec-trace"),
                                       new String'("%trace"))

      ),
      (Target => new String'("i386-pok"),
       Build_Command => null,
       Build_Options => null,
       Run_Command => new String'("qemu"),
       Run_Options => new String_List'(new String'("-fda"),
                                       new String'(GNAT.OS_Lib.Getenv
                                                     ("POK_PATH").all
                                                     & "/misc"
                                                     & "/grub-boot-only.img"),
                                       new String'("-hda"),
                                       new String'("fat:."),
                                       new String'("-boot"),
                                       new String'("a"),
                                       new String'("-nographic"),
                                       new String'("-exec-trace"),
                                       new String'("%trace"))
      ),
      (Target => new String'("i386-linux"),
       Build_Command => null,
       Build_Options => null,
       Run_Command => new String'("qemu-i386"),
       Run_Options => new String_List'(new String'("-exec-trace"),
                                       new String'("%trace"),
                                       new String'("%exe"))
      ),
      (Target => new String'("i686-pc-linux-gnu"),
       Build_Command => null,
       Build_Options => null,
       Run_Command => new String'("valgrind"),
       Run_Options => new String_List'(
           new String'("--tool=coverage"),
           new String'("--cov-exec-file=%trace"),
           new String'("%exe"))
      ),
      (Target => new String'("prepare"),
       Build_Command => null,
       Build_Options => null,
       Run_Command => null,
       Run_Options => null
      )
     );

   --  Target aliases: names users may feed to --target and that resolve
   --  to one of the target definitions above

   type Target_Alias is record

      Alias  : String_Access;
      --  The name users may feed to --target, as an alias to designate ...

      Target : String_Access;
      --  The name of a real target definition

   end record;

   type Target_Aliases_Array is array (Natural range <>) of Target_Alias;

   Aliases : constant Target_Aliases_Array :=
     (1 => (Alias => new String'("powerpc-elf"),
            Target => new String'("qemu-prep")
           )
     );

end Qemudrv_Base;
