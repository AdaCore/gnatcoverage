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

--  This package provides ways to handle a set of elf files.

--  This set is represented as a hash table. The allocation of entries
--  is managed transparently; when doing a lookup, if no entry can be
--  found for a file, the entry is created and the newly created entry
--  is returned (cf Open_Entry).

with Strings; use Strings;
with Ada.Containers.Hashed_Maps;
with Traces_Elf; use Traces_Elf;

package Execs_Dbase is

   type Exec_Base_Type is limited private;
   --  type for handles on Exec databases.

   function Get_Exec_Base return Exec_Base_Type;
   pragma Inline (Get_Exec_Base);
   --  Return a handle on the current exec database.

   procedure Open_Exec
     (Execs     : Exec_Base_Type;
      File_Name : String;
      Exec      : out Exe_File_Acc);
   --  Search for a file named File_Name in the Exec database. If one
   --  found, return it; otherwise, open File_Name and add it to the
   --  database, then return it in Exec.
   --
   --  Sections and symbols are read.
   --  In case of error, exception is propagated (see trace_elf.ads)

   procedure Insert_Exec
     (Execs     : Exec_Base_Type;
      File_Name : String);
   --  Similar to Open_Exec, but does not return the result; just insert
   --  it in the Exec database if it does not already exists.

   Routine_Name_Ambiguity : exception;

   procedure Build_Routines_Names (Execs : Exec_Base_Type);
   --  Build the routine list from the Execs database. If there is
   --  more than one exec in Execs, this may not be possible; if it
   --  cannot be done, raise Routine_Name_Ambiguity.

private

   type Exec_Base_Entry is record
      --  Entry in the Exec base type.

      Exec_File_Name : File_Name;
      --  Name of the file associated to this entry. This should be
      --  a full path to an target executable on the host file system.

      Exec           : Exe_File_Acc;
      --  Access to the exe file information of the file whose name
      --  is Elf_File_Name.
   end record;

   function "=" (L, R : Exec_Base_Entry) return Boolean;
   --  Return True if L and R represent the same executable.

   package Execs_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => String_Acc,
      Element_Type => Exec_Base_Entry,
      Hash => Hash,
      Equivalent_Keys => Equal,
      "=" => "=");

   type Exec_Base_Type is access all Execs_Maps.Map;

end Execs_Dbase;
