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

with Ada.Containers; use Ada.Containers;

package body Execs_Dbase is

   Exec_Base : Execs_Maps.Map;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Exec_Base_Entry) return Boolean is
   begin
      return L.Exec_File_Name = R.Exec_File_Name;
   end "=";

   ---------------
   -- Open_Exec --
   ---------------

   procedure Open_Exec
     (File_Name  : String;
      Text_Start : Pc_Type;
      Exec       : out Exe_File_Acc)
   is
      use Execs_Maps;
      Base_Entry : Exec_Base_Entry;
      Position   : constant Cursor :=
        Exec_Base.Find (File_Name'Unrestricted_Access);

   begin
      if Position /= No_Element then
         Exec := Element (Position).Exec;
      else
         Exec := Open_File (File_Name, Text_Start);
         Base_Entry.Exec_File_Name := new String'(File_Name);
         Base_Entry.Exec := Exec;
         Exec_Base.Insert (Base_Entry.Exec_File_Name, Base_Entry);
         Build_Sections (Exec.all);
         Build_Symbols (Exec.all);
      end if;
   end Open_Exec;

end Execs_Dbase;
