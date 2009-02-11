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
      Target : String_Access;
      Command : String_Access;
      Options : String_List_Access;
   end record;

   type Driver_Target_Array is array (Natural range <>) of Driver_Target;

   Drivers : constant Driver_Target_Array :=
     (
      (Target => new String'("powerpc-elf"),
       Command => new String'("qemu-system-ppc"),
       Options => new String_List'(new String'("-nographic"),
                                   new String'("-M"),
                                   new String'("prep"),
                                   new String'("$dir_exe"),
                                   new String'("-bios"),
                                   new String'("$base_exe"))
      ),
      (Target => new String'("leon-elf"),
       Command => new String'("qemu-system-sparc"),
       Options => new String_List'(new String'("-nographic"),
                                   new String'("-M"),
                                   new String'("at697"),
                                   new String'("-kernel"),
                                   new String'("$exe"))
      )
     );
end Qemudrv_Base;
