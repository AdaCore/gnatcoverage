------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
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
with Traces_Elf; use Traces_Elf;
with Traces_Dbase; use Traces_Dbase;
with Strings; use Strings;
with Elf_Files; use Elf_Files;

package Traces_Names is
   --  Add (or remove if EXCLUDE is true) routines read from ELF file
   --  Filename.
   --  Display errors on standard error.
   procedure Read_Routines_Name (Filename : String; Exclude : Boolean);

   --  Same but directly from an ELF_FILE.
   procedure Read_Routines_Name
     (Efile : Elf_File; Filename : String_Acc; Exclude : Boolean);

   --  Display the list of routines (on standard output).
   procedure Disp_All_Routines;

   --  Add traces for routine_name.
   --  Return null if we don't want coverage for it.
   function Add_Traces (Routine_Name : String_Acc;
                        Content : Binary_Content) return Traces_Base_Acc;

   procedure Dump_Routines_Traces;
end Traces_Names;
