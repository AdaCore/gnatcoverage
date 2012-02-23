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

   --------------------------
   -- Build_Routines_Names --
   --------------------------

   procedure Build_Routines_Names is
   begin
      --  If there is more than one exec in the base, return an error;
      --  otherwise, we may not know how to handle the ambiguities. We may want
      --  to be more subtle at some point; but for now it seems reasonable to
      --  refuse to deduce the function from several different exec files.

      if Exec_Base.Length /= 1 then
         raise Routine_Name_Ambiguity;
      end if;

      Read_Routines_Name
        (Execs_Maps.Element (Exec_Base.First).Exec, Exclude => False);
   end Build_Routines_Names;

   ---------------
   -- Open_Exec --
   ---------------

   procedure Open_Exec
     (File_Name  : String;
      Text_Start : Pc_Type;
      Exec       : out Exe_File_Acc)
   is
      use Execs_Maps;
      Exec_File_Name : String_Access := new String'(File_Name);
      Base_Entry     : Exec_Base_Entry;
      Position       : constant Cursor := Exec_Base.Find (Exec_File_Name);
   begin
      if Position /= No_Element then
         Exec := Element (Position).Exec;
         Free (Exec_File_Name);
      else
         Exec := new Exe_File_Type;
         Base_Entry.Exec_File_Name := Exec_File_Name;
         Base_Entry.Exec := Exec;
         Open_File (Exec.all,
                    Exec_File_Name.all,
                    Text_Start);
         Exec_Base.Insert (Exec_File_Name, Base_Entry);
         Build_Sections (Exec.all);
         Build_Symbols (Exec);
      end if;
   end Open_Exec;

end Execs_Dbase;
