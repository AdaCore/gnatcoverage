------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2022-2024, AdaCore                     --
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

--  This package provides a simple helper to manage temporary directories.
--
--  TODO??? There is no proper way to do this in Ada for now. We should
--  add a package in GNATCOLL to take care of this, but in the meantime,
--  do a reasonable approximation (Prefix & PID) in the current
--  directory.

private with Ada.Finalization;

private with Strings;

package Temp_Dirs is

   type Temporary_Directory is tagged limited private;
   --  Handle to a temporary directory

   procedure Create_Temporary_Directory
     (Self        : out Temporary_Directory;
      Prefix      : String;
      Auto_Delete : Boolean := True);
   --  Create a temporary directory with a name starting with Prefix. If
   --  Auto_Delete is true, the temporary directory is automatically removed
   --  when Self is finalized.

   function Directory_Name (Self : Temporary_Directory) return String;
   --  Return the absolute name for this temporary directory

private

   use Strings;
   use type Unbounded_String;

   type Temporary_Directory is new Ada.Finalization.Limited_Controlled
   with record
      Name        : Unbounded_String;
      Auto_Delete : Boolean;
   end record;

   overriding
   procedure Finalize (Self : in out Temporary_Directory);
   --  Remove the temporary directory and clean up allocated resources

end Temp_Dirs;
