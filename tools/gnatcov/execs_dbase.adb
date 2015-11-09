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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers;          use Ada.Containers;
with Ada.Strings.Fixed;

with GNAT.OS_Lib;

with Binary_Files;

package body Execs_Dbase is

   function Is_Windows_Target (Target : String) return Boolean;
   --  Return whether we are targetting native Windows

   function Has_Suffix (Name, Suffix : String) return Boolean;
   --  Return whether the last characters of Name are equal to Suffix (case
   --  insensitive equality).

   Exec_Base : Execs_Maps.Map;

   -----------------------
   -- Is_Windows_Target --
   -----------------------

   function Is_Windows_Target (Target : String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index (Target, "mingw") /= 0;
   end Is_Windows_Target;

   ----------------
   -- Has_Suffix --
   ----------------

   function Has_Suffix (Name, Suffix : String) return Boolean is
   begin
      if Suffix'Length > Name'Length then
         return False;
      end if;

      declare
         Actual_Suffix : String renames
           Name (Name'Last - Suffix'Length + 1 .. Name'Last);
      begin
         return To_Lower (Actual_Suffix) = To_Lower (Suffix);
      end;
   end Has_Suffix;

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
      Target     : String_Access;
      Exec       : out Exe_File_Acc)
   is

      function Get_Actual_Name return String_Access;
      --  Expand File_Name so that it is an actual executable file name

      ---------------------
      -- Get_Actual_Name --
      ---------------------

      function Get_Actual_Name return String_Access is
         Is_Windows : constant Boolean :=
           Is_Windows_Target (if Target = null
                              then Standard'Target_Name
                              else Target.all);
      begin
         --  On Windows, users can omit ".exe" suffixes for executables, so
         --  add it if it's missing on this platform. Be careful not to add it
         --  everytime since we sometimes have to deal with other kind of files
         --  (for instance object files).

         if Is_Windows
           and then
             not GNAT.OS_Lib.Is_Readable_File (File_Name)
           and then
             not Has_Suffix (File_Name, ".exe")
         then
            return new String'(File_Name & ".exe");
         else
            return new String'(File_Name);
         end if;
      end Get_Actual_Name;

      use Execs_Maps;
      Exec_File_Name : String_Access := Get_Actual_Name;
      Base_Entry     : Exec_Base_Entry;
      Position       : constant Cursor := Exec_Base.Find (Exec_File_Name);

   begin
      if Position /= No_Element then
         Exec := Element (Position).Exec;
         Free (Exec_File_Name);

      else
         begin
            Exec := Open_File (Exec_File_Name.all, Text_Start);
         exception
            when Binary_Files.Error =>
               Free (Exec_File_Name);
               raise;
         end;

         Base_Entry.Exec_File_Name := Exec_File_Name;
         Base_Entry.Exec := Exec;
         Exec_Base.Insert (Exec_File_Name, Base_Entry);
         Build_Sections (Exec.all);
         Build_Symbols (Exec.all);
      end if;
   end Open_Exec;

end Execs_Dbase;
