------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

with GNAT.OS_Lib;

package body Files_Handling is

   -----------------------
   -- Executable_Suffix --
   -----------------------

   function Executable_Suffix return String is
      use GNAT.OS_Lib;

      Value : String_Access := Get_Executable_Suffix;
   begin
      return Result : constant String := Value.all do
         Free (Value);
      end return;
   end Executable_Suffix;

   -----------------------
   -- Create_Normalized --
   -----------------------

   function Create_Normalized (Filename : String) return Virtual_File is
      Result : constant Virtual_File := Create_From_Base (+Filename);
   begin
      Result.Normalize_Path;
      return Result;
   end Create_Normalized;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name (File : Virtual_File) return Unbounded_String is
   begin
      return +(+File.Full_Name);
   end Full_Name;

end Files_Handling;
