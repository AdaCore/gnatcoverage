------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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

--  This package provides ways to handle a set of elf files.

--  This set is represented as a hash table. The allocation of entries
--  is managed transparently; when doing a lookup, if no entry can be
--  found for a file, the entry is created and the newly created entry
--  is returned (cf Open_Entry).

with GNAT.Strings; use GNAT.Strings;

with Strings; use Strings;
with Ada.Containers.Hashed_Maps;
with Traces_Elf; use Traces_Elf;
with Traces; use Traces;

package Execs_Dbase is

   procedure Open_Exec
     (File_Name  : String;
      Text_Start : Pc_Type;
      Exec       : out Exe_File_Acc);
   --  Search for a file named File_Name in the Exec database. If one found,
   --  return it; otherwise, open File_Name and add it to the database, then
   --  return it in Exec.
   --
   --  Sections and symbols are read.
   --  In case of error, exception is propagated (see trace_elf.ads)
   --
   --  Target is used to find the actual filename on target which have, for
   --  instance, implicit suffixes (like ".exe" on Windows).

   Routine_Name_Ambiguity : exception;

private

   type Exec_Base_Entry is record
      --  Entry in the Exec base type.

      Exec_File_Name : File_Name;
      --  Name of the file associated to this entry. This should be a full path
      --  to an target executable on the host file system.

      Exec           : Exe_File_Acc;
      --  Access to the exe file information of the file whose name is
      --  Elf_File_Name.
   end record;

   function "=" (L, R : Exec_Base_Entry) return Boolean;
   --  Return True if L and R represent the same executable.

   package Execs_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => String_Access,
      Element_Type    => Exec_Base_Entry,
      Hash            => Hash,
      Equivalent_Keys => Equal,
      "="             => "=");

   type Exec_Base_Type is access all Execs_Maps.Map;

end Execs_Dbase;
