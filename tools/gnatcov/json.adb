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

with GNAT.Strings; use GNAT.Strings;

with Strings; use Strings;
with Text_Files;

package body JSON is

   -----------
   -- Write --
   -----------

   procedure Write
     (Filename : String; Value : JSON_Value; Compact : Boolean := True)
   is
      File    : Text_Files.File_Type;
      Content : constant Unbounded_String := Value.Write (Compact => Compact);
   begin
      File.Create (Filename);
      File.Put (Content);
   end Write;

   ----------
   -- Read --
   ----------

   function Read (Filename : String) return Read_Result is
   begin
      return Read (Create (+Filename));
   end Read;

   function Read (File : Virtual_File) return Read_Result is
      Content : String_Access := File.Read_File;
   begin
      return Parsed_JSON : constant Read_Result :=
         GNATCOLL.JSON.Read (Content.all)
      do
         Free (Content);
      end return;
   end Read;

end JSON;
