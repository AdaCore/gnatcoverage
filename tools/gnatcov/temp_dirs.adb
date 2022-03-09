------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

with Interfaces; use Interfaces;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Hex_Images; use Hex_Images;
with Switches;   use Switches;

package body Temp_Dirs is

   --------------------------------
   -- Create_Temporary_Directory --
   --------------------------------

   procedure Create_Temporary_Directory
     (Self        : out Temporary_Directory;
      Prefix      : String;
      Auto_Delete : Boolean := True)
   is
      PID       : constant Unsigned_64 :=
        Unsigned_64 (Pid_To_Integer (Current_Process_Id));
      Base_Name : constant String :=
        Prefix & "-" & Strip_Zero_Padding (Hex_Image (PID));
      Name      : constant String := Compose (Current_Directory, Base_Name);
   begin
      if Verbose then
         Put_Line
           ("Creating temporary build directory: " & Name);
      end if;

      --  In directory creation fails and we raise an exception, make sure to
      --  leave a null directory name so that finalization does not try to
      --  remove it.

      Create_Directory (Name);
      Self.Name := To_Unbounded_String (Name);
      Self.Auto_Delete := Auto_Delete;
   end Create_Temporary_Directory;

   --------------------
   -- Directory_Name --
   --------------------

   function Directory_Name (Self : Temporary_Directory) return String is
   begin
      return To_String (Self.Name);
   end Directory_Name;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Temporary_Directory) is
   begin
      if Self.Name /= Null_Unbounded_String and then Self.Auto_Delete then
         Delete_Tree (To_String (Self.Name));
         Self.Name := Null_Unbounded_String;
      end if;
   end Finalize;

end Temp_Dirs;
