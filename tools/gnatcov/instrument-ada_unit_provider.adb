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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;

with GPR2.Build.Unit_Info;
with GPR2.Path_Name;
with Libadalang.Unit_Files;

with Outputs;
with Project;
with Text_Files;

package body Instrument.Ada_Unit_Provider is

   use type GPR2.Language_Id;

   package LALCO renames Libadalang.Common;

   function Unit_Key (Name : String; Part : GPR2.Valid_Unit_Kind) return String
   is (To_Lower (Name)
       & '%'
       & (case Part is
            when GPR2.S_Spec => 's',
            when others      => 'b'));
   --  Key used to find a source file in the provider map

   -------------------------
   -- Create_Mapping_File --
   -------------------------

   procedure Create_Mapping_File (Filename : String) is
      F : Text_Files.File_Type;
   begin
      F.Create (Filename);
      for Source of
        Project.Source_Closure
          (View                  => Project.Project.Root_Project,
           With_Externally_Built => True,
           With_Runtime          => True)
      loop
         if Source.Language = GPR2.Ada_Language then
            for Unit of Source.Units loop
               if Unit.Kind in GPR2.Valid_Unit_Kind then
                  F.Put_Line (Unit_Key (String (Unit.Full_Name), Unit.Kind));
                  F.Put_Line (String (Source.Path_Name.Simple_Name));
                  F.Put_Line (String (Source.Path_Name.Value));
               end if;
            end loop;
         end if;
      end loop;
   end Create_Mapping_File;

   ---------------------
   -- Create_Provider --
   ---------------------

   function Create_Provider (Mapping_File : String) return Provider_Type is
      Provider : Provider_Type;
      F        : File_Type;
   begin
      --  Parse the mapping file and fill Provider.Unit_Map

      Open (F, In_File, Mapping_File);

      --  The mapping file is a succession of triplets as such:
      --  <unit_name>{%b,%s}
      --  <file_basename>
      --  <file_fullname>

      begin
         while not End_Of_File (F) loop
            declare
               Unit_Name      : constant String := Get_Line (F);
               Dummy_Basename : constant String := Get_Line (F);
               File_Fullname  : constant String := Get_Line (F);
            begin
               Provider.Unit_Map.Include (To_Lower (Unit_Name), File_Fullname);
            end;
         end loop;
      exception
         when End_Error =>
            Outputs.Warn
              ("mapping file """ & Mapping_File & " "" is truncated");
      end;
      Close (F);

      return Provider;
   end Create_Provider;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding
   function Get_Unit_Filename
     (Provider : Provider_Type; Name : Text_Type; Kind : Analysis_Unit_Kind)
      return String
   is
      use String_Maps;

      Part          : constant GPR2.Valid_Unit_Kind :=
        (case Kind is
           when Libadalang.Common.Unit_Body          => GPR2.S_Body,
           when Libadalang.Common.Unit_Specification => GPR2.S_Spec);
      Key           : constant String :=
        Unit_Key (Libadalang.Unit_Files.Unit_String_Name (Name), Part);
      Unit_Name_Cur : constant Cursor := Provider.Unit_Map.Find (Key);
   begin
      return
        (if Has_Element (Unit_Name_Cur) then Element (Unit_Name_Cur) else "");
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding
   function Get_Unit
     (Provider : Provider_Type;
      Context  : Analysis_Context'Class;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Charset  : String := "";
      Reparse  : Boolean := False) return Analysis_Unit'Class
   is
      Filename : constant String := Provider.Get_Unit_Filename (Name, Kind);
   begin
      if Filename /= "" then
         return Context.Get_From_File (Filename, Charset, Reparse);
      else
         declare
            Dummy_File : constant String :=
              Libadalang.Unit_Files.File_From_Unit (Name, Kind);
            Kind_Name  : constant Text_Type :=
              (case Kind is
                 when LALCO.Unit_Specification => "specification file",
                 when LALCO.Unit_Body          => "body file");
            Error      : constant Text_Type :=
              "Could not find source file for "
              & Name
              & " ("
              & Kind_Name
              & ")";
         begin
            return
              Libadalang.Analysis.Get_With_Error
                (Context, Dummy_File, Error, Charset);
         end;
      end if;
   end Get_Unit;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Provider  : Provider_Type;
      Unit_Name : String;
      Unit_Part : GPR2.Valid_Unit_Kind) return Boolean is
   begin
      return Provider.Unit_Map.Contains (Unit_Key (Unit_Name, Unit_Part));
   end Has_Unit;

end Instrument.Ada_Unit_Provider;
