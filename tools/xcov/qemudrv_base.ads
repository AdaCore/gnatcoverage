------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2009, AdaCore                       --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Strings; use GNAT.Strings;

package Qemudrv_Base is
   type Driver_Target is record
      --  Name of the target (triplet).
      Target        : String_Access;

      --  Post-build command and option list (may be null).
      Build_Command : String_Access;
      Build_Options : String_List_Access;

      --  Run command and option list.
      Run_Command   : String_Access;
      Run_Options   : String_List_Access;
   end record;

   type Driver_Target_Array is array (Natural range <>) of Driver_Target;

   Drivers : constant Driver_Target_Array :=
     (
      (Target => new String'("powerpc-elf"),
       Build_Command => new String'("powerpc-elf-objcopy"),
       Build_Options => new String_List'(new String'("-O"),
                                         new String'("binary"),
                                         new String'("$exe"),
                                         new String'("$bin")),
       Run_Command => new String'("qemu-system-ppc"),
       Run_Options => new String_List'(new String'("-nographic"),
                                       new String'("-M"),
                                       new String'("prep"),
                                       new String'("-boot"),
                                       new String'("n"),
                                       new String'("-no-reboot"),
                                       new String'("-L"),
                                       new String'("$dir_exe"),
                                       new String'("-bios"),
                                       new String'("$base_bin"),
                                       new String'("-trace"),
                                       new String'("$trace"))
      ),
      (Target => new String'("leon-elf"),
       Build_Command => null,
       Build_Options => null,
       Run_Command => new String'("qemu-system-sparc"),
       Run_Options => new String_List'(new String'("-nographic"),
                                       new String'("-M"),
                                       new String'("at697"),
                                       new String'("-kernel"),
                                       new String'("$exe"),
                                       new String'("-trace"),
                                       new String'("$trace"))

      ),
      (Target => new String'("i386-linux"),
       Build_Command => null,
       Build_Options => null,
       Run_Command => new String'("qemu-i386"),
       Run_Options => new String_List'(new String'("-trace"),
                                       new String'("$trace"),
                                       new String'("$exe"))
      ),
      (Target => new String'("prepare"),
       Build_Command => null,
       Build_Options => null,
       Run_Command => null,
       Run_Options => null
      )
     );
end Qemudrv_Base;
