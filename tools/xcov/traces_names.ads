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

--  Management of the routines database

with Traces_Dbase; use Traces_Dbase;
with Traces_Elf; use Traces_Elf;
with Strings; use Strings;

package Traces_Names is

   procedure Add_Routine_Name (Name : String_Acc);
   --  Add a routine name to the database, and allocate an associated
   --  Subprogram_Info record (see below). Constraint_Error is propagated if
   --  the name already exists.

   --  Information recorded about each subprogram in the routines database

   type Subprogram_Info is record
      Exec  : Exe_File_Acc;
      --  Pointer to the Exec file where this subprogram has first been
      --  found.

      Insns : Binary_Content_Acc;
      --  Subprogram binary content.

      Traces : Traces_Base_Acc;
      --  Traces for the subprogram.
   end record;

   procedure Remove_Routine_Name (Name : String_Acc);
   --  Remove a routine from the database

   procedure Iterate
     (Proc : access procedure (Subp_Name : String_Acc;
                               Subp_Info : in out Subprogram_Info));
   --  Execute Proc for each routine in the database

   procedure Read_Routines_Name_From_Text (Filename : String);
   --  Read a list of routines name from a text file in the following format:
   --  * lines starting with '#' are ignored
   --  * one name per line
   --  * no blanks allowed.

   --  Display the list of routines (on standard output).
   procedure Disp_All_Routines;

   function Add_Traces
     (Routine_Name : String_Acc;
      Exec         : Exe_File_Acc;
      Content      : Binary_Content) return Traces_Base_Acc;
   --  Add traces for routine_name.
   --  Return null if we don't want coverage for it.
   --  Need definition for each parameter and for return value???

   Consolidation_Error : exception;
   --  Raised if consolidation is not possible (eg different code for a
   --  function).

end Traces_Names;
