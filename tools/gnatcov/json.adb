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

with Ada.Exceptions;

with GNAT.Strings; use GNAT.Strings;

with Outputs; use Outputs;
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
      File.Close;
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
      if Content = null then
         Fatal_Error ("Could not read file '"
                      & Display_Full_Name (File) & "'");
      end if;
      return Parsed_JSON : constant Read_Result :=
         GNATCOLL.JSON.Read (Content.all)
      do
         Free (Content);
      end return;
   exception
      when ex : Constraint_Error =>
         Fatal_Error (Ada.Exceptions.Exception_Message (ex));
   end Read;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (Filename : String) return JSON_Value is
      Parsed_JSON : constant Read_Result := JSON.Read (Filename);
   begin
      if not Parsed_JSON.Success then
         Fatal_Error
           ("Parsing error: " & Format_Parsing_Error (Parsed_JSON.Error));
      end if;
      return Parsed_JSON.Value;
   end Read_File;

  -------------------------------
  -- JSON_Value Read Utilities --
  -------------------------------

   function Get_Child (Value : JSON_Value; Field : String)
   return JSON_Value;

   function Get_Child (Value : JSON_Value; Field : String)
   return JSON_Value is
   begin
      if not Value.Has_Field (Field) then
         Fatal_Error ("Expected field '" & Field & "' is missing.");
      end if;
         return Value.Get (Field);
   end Get_Child;

   ---------------
   -- Child_Int --
   ---------------

   function Child_Int (Value : JSON_Value; Field : String)
   return Integer is
      Child : constant JSON_Value := Get_Child (Value, Field);
   begin
      if Child.Kind /= JSON_Int_Type then
         Fatal_Error
           ("Field is expected to be of type "
            & JSON_Value_Type'Image (JSON_Int_Type));
      end if;
      return Child.Get;
   end Child_Int;

   ------------------
   -- Child_String --
   ------------------

   function Child_String (Value : JSON_Value; Field : String)
   return UTF8_String is
      Child : constant JSON_Value := Get_Child (Value, Field);
   begin
      if Child.Kind /= JSON_String_Type then
         Fatal_Error
           ("Field is expected to be of type "
            & JSON_Value_Type'Image (JSON_String_Type));
      end if;
      return Child.Get;
   end Child_String;

   ------------------
   -- Child_Array --
   ------------------

   function Child_Array (Value : JSON_Value; Field : String)
   return JSON_Array is
      Child : constant JSON_Value := Get_Child (Value, Field);
   begin
      if Child.Kind /= JSON_Array_Type then
         Fatal_Error
           ("Field is expected to be of type "
            & JSON_Value_Type'Image (JSON_Array_Type));
      end if;
      return Child.Get;
   end Child_Array;

   -----------------------
   -- Array_Nth_Integer --
   -----------------------

   function Array_Nth_Integer (List : JSON_Array; N : Integer)
   return Integer is
      Child : constant JSON_Value := Get (List, N);
   begin
      if Child.Kind /= JSON_Int_Type then
         Fatal_Error
           ("Field is expected to be of type "
            & JSON_Value_Type'Image (JSON_Int_Type));
      end if;
      return Child.Get;
   end Array_Nth_Integer;

end JSON;
