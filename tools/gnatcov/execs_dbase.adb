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

with GNAT.OS_Lib;

with Binary_Files;

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
      Target     : String_Access;
      Exec       : out Exe_File_Acc)
   is
      function Get_Path return String_Access;
      --  Try to locate File_Name as an executable and return the path to it

      --------------
      -- Get_Path --
      --------------

      function Get_Path return String_Access is
         Result : String_Access := null;

      begin
         --  Perform a platform-specific lookup only for native targets. For
         --  instance, it does not make sense on Windows to look for "foo.exe"
         --  for a "foo" executable for ARM ELF.

         if not GNAT.OS_Lib.Is_Executable_File (File_Name)
              and then
            (Target = null or else Target.all = Standard'Target_Name)
         then
            Result := GNAT.OS_Lib.Locate_Exec_On_Path (File_Name);
         end if;

         --  For other configurations or if we just cannot find the executable,
         --  let the usual machinery work with the user-given file name.

         if Result = null then
            return new String'(File_Name);
         else
            return Result;
         end if;
      end Get_Path;

      use Execs_Maps;
      Exec_File_Name : String_Access := Get_Path;
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
