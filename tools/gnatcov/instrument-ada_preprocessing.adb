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

with Ada.Exceptions; use Ada.Exceptions;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Langkit_Support.Errors;
with Libadalang.Preprocessing; use Libadalang.Preprocessing;

with JSON;
with Outputs; use Outputs;
with Project;

package body Instrument.Ada_Preprocessing is

   -----------------------------------
   -- Create_Preprocessor_Data_File --
   -----------------------------------

   procedure Create_Preprocessor_Data_File (Filename : String) is

      function Serialize (Value : Value_Type) return JSON_Value;
      --  Serialize Value to JSON

      function Serialize (Map : Definition_Maps.Map) return JSON_Value;
      --  Serialize Map to JSON

      function Serialize (Config : File_Config) return JSON_Value;
      --  Serialize Config to JSON

      ---------------
      -- Serialize --
      ---------------

      function Serialize (Value : Value_Type) return JSON_Value is
      begin
         case Value.Kind is
            when Libadalang.Preprocessing.Empty =>
               return Create;

            when String_Literal                 =>
               return Result : constant JSON_Value := Create_Object do
                  Result.Set_Field ("kind", "string");
                  Result.Set_Field ("value", Value.String_Value);
               end return;

            when Symbol                         =>
               return Result : constant JSON_Value := Create_Object do
                  Result.Set_Field ("kind", "symbol");
                  Result.Set_Field ("value", Value.Symbol_Value);
               end return;
         end case;
      end Serialize;

      function Serialize (Map : Definition_Maps.Map) return JSON_Value is
         Result : constant JSON_Value := Create_Object;
      begin
         for Cur in Map.Iterate loop
            declare
               Name  : constant Unbounded_String := Definition_Maps.Key (Cur);
               Value : Value_Type renames Map.Constant_Reference (Cur);
            begin
               Result.Set_Field (+Name, Serialize (Value));
            end;
         end loop;
         return Result;
      end Serialize;

      function Serialize (Config : File_Config) return JSON_Value is
      begin
         if not Config.Enabled then
            return Create;
         end if;

         return Result : constant JSON_Value := Create_Object do
            Result.Set_Field ("definitions", Serialize (Config.Definitions));
            Result.Set_Field ("line_mode", Config.Line_Mode'Image);
            Result.Set_Field ("print_symbols", Config.Print_Symbols);
            Result.Set_Field ("undefined_is_false", Config.Undefined_Is_False);
         end return;
      end Serialize;

      Default_Config : File_Config;
      File_Configs   : File_Config_Maps.Map;

      Result : constant JSON_Value := Create_Object;

      --  Start of processing for Create_Preprocessor_Data_File

   begin
      --  Extract preprocessor data from the loaded project

      begin
         Extract_Preprocessor_Data_From_Project
           (Tree           => Project.Project,
            Default_Config => Default_Config,
            File_Configs   => File_Configs);
      exception
         when
           Exc :
             Langkit_Support.Errors.File_Read_Error
             | Langkit_Support.Errors.Syntax_Error
         =>
            Outputs.Error
              ("error while loading preprocessor data from project");
            Fatal_Error (Exception_Message (Exc));
      end;

      --  Refine it to make sure line number are preserved in the preprocessor
      --  output.

      declare
         procedure Process (Config : in out File_Config);
         --  If Config enables preprocessing, set its line mode to Blank_Lines

         -------------
         -- Process --
         -------------

         procedure Process (Config : in out File_Config) is
         begin
            if Config.Enabled then
               Config.Line_Mode := Blank_Lines;
            end if;
         end Process;
      begin
         Iterate (Default_Config, File_Configs, Process'Access);
      end;

      --  Serialize this data to JSON

      Result.Set_Field ("default_config", Serialize (Default_Config));
      declare
         Map : constant JSON_Value := Create_Object;
      begin
         for Cur in File_Configs.Iterate loop
            declare
               Filename : constant Unbounded_String :=
                 File_Config_Maps.Key (Cur);
            begin
               Map.Set_Field
                 (+Filename,
                  Serialize (File_Configs.Constant_Reference (Cur)));
            end;
         end loop;
         Result.Set_Field ("file_configs", Map);
      end;

      --  Create the requested file

      JSON.Write (Filename, Result, Compact => False);
   end Create_Preprocessor_Data_File;

   -------------------------
   -- Create_Preprocessor --
   -------------------------

   function Create_Preprocessor
     (Filename : String)
      return Langkit_Support.File_Readers.File_Reader_Reference
   is
      Dummy : constant Context_Handle :=
        Create_Context
          ("Loading preprocessor data from temporary file " & Filename);

      function Deserialize (Desc : JSON_Value) return Value_Type;
      --  Deserialize Value from JSON

      function Deserialize (Desc : JSON_Value) return Definition_Maps.Map;
      --  Deserialize Map from JSON

      function Deserialize (Desc : JSON_Value) return File_Config;
      --  Deserialize Config from JSON

      -----------------
      -- Deserialize --
      -----------------

      function Deserialize (Desc : JSON_Value) return Value_Type is
      begin
         if Desc.Kind = JSON_Null_Type then
            return (Kind => Libadalang.Preprocessing.Empty);
         end if;

         declare
            Kind : constant String := Desc.Get ("kind");
         begin
            if Kind = "string" then
               return
                 (Kind => String_Literal, String_Value => Desc.Get ("value"));
            elsif Kind = "symbol" then
               return (Kind => Symbol, Symbol_Value => Desc.Get ("value"));
            else
               Fatal_Error ("invalid value kind: " & Kind);
            end if;
         end;
      end Deserialize;

      function Deserialize (Desc : JSON_Value) return Definition_Maps.Map is
      begin
         return Result : Definition_Maps.Map do
            declare
               procedure Process (Name : UTF8_String; Value : JSON_Value);
               --  Deserialize a definition map from Value and add it under the
               --  given Name to Result.

               -------------
               -- Process --
               -------------

               procedure Process (Name : UTF8_String; Value : JSON_Value) is
               begin
                  Result.Insert (+Name, Deserialize (Value));
               end Process;
            begin
               Desc.Map_JSON_Object (Process'Access);
            end;
         end return;
      end Deserialize;

      function Deserialize (Desc : JSON_Value) return File_Config is
      begin
         if Desc.Kind = JSON_Null_Type then
            return (Enabled => False);
         end if;

         return Result : File_Config (Enabled => True) do
            Result.Definitions := Deserialize (Desc.Get ("definitions"));
            Result.Line_Mode := Any_Line_Mode'Value (Desc.Get ("line_mode"));
            Result.Print_Symbols := Desc.Get ("print_symbols");
            Result.Undefined_Is_False := Desc.Get ("undefined_is_false");
         end return;
      end Deserialize;

      Default_Config : File_Config;
      File_Configs   : File_Config_Maps.Map;

      Parsed_JSON : Read_Result;

      --  Start of processing for Create_Preprocessor

   begin
      --  Parse the preprocessor data from the given filename

      Parsed_JSON := JSON.Read (Filename);
      if not Parsed_JSON.Success then
         Fatal_Error
           ("Parsing error: " & Format_Parsing_Error (Parsed_JSON.Error));
      end if;

      declare
         Dummy : constant Context_Handle :=
           Create_Context
             ("Loading preprocessor data from temporary file "
              & Filename
              & " for the default config");
      begin
         Default_Config :=
           Deserialize (Parsed_JSON.Value.Get ("default_config"));
      end;

      declare
         File_Configs_Desc : constant JSON_Value :=
           Parsed_JSON.Value.Get ("file_configs");

         procedure Process (Name : UTF8_String; Value : JSON_Value);
         --  Deserialize a file config from Value and add it under the given
         --  Name to File_Configs.

         -------------
         -- Process --
         -------------

         procedure Process (Name : UTF8_String; Value : JSON_Value) is
            Dummy : constant Context_Handle :=
              Create_Context
                ("Loading preprocessor data from temporary file "
                 & Filename
                 & " for "
                 & Name);
         begin
            File_Configs.Insert (+Name, Deserialize (Value));
         end Process;
      begin
         File_Configs_Desc.Map_JSON_Object (Process'Access);
      end;

      return Create_Preprocessor (Default_Config, File_Configs);
   end Create_Preprocessor;

end Instrument.Ada_Preprocessing;
