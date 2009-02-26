------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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
   procedure Read_Routines_Name (Efile : Elf_File; Exclude : Boolean);

   --  Add a single name.
   --  Raise CE if the name is already in the base.
   procedure Add_Routine_Name (Name : String_Acc);

   --  Read a list of routines name from a text file.  The format is very
   --  simple:
   --  * lines starting with '#' are ignored
   --  * one name per line
   --  * no blanks allowed.
   procedure Read_Routines_Name_From_Text (Filename : String);

   --  Display the list of routines (on standard output).
   procedure Disp_All_Routines;

   --  Add traces for routine_name.
   --  Return null if we don't want coverage for it.
   function Add_Traces (Routine_Name : String_Acc;
                        Exec : Exe_File_Acc;
                        Content : Binary_Content) return Traces_Base_Acc;

   --  Raised if consolidation is not possible.
   --  (eg: different code for a function).
   Consolidation_Error : exception;

   procedure Build_Source_Lines;
   --  Go through the routine database and, for each routine,
   --  populate the source database (in traces_sources) with
   --  the routine's source information.

   procedure Build_Routines_Trace_State;
   --  Go through the routine database and, for each routine,
   --  compute the state of its trace. This should be used only
   --  when the subroutine database has been populated with its
   --  traces.

   procedure Dump_Routines_Traces (Exec : Exe_File_Type);
   --  ??? Obsolete. To be removed when the old options have been
   --  retired.

   procedure Dump_Routines_Traces;

end Traces_Names;
