------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with GNATCOLL.Projects.Aux; use GNATCOLL.Projects.Aux;

with Libadalang.Unit_Files;

with Outputs;
with Project; use Project;

package body Instrument.Ada_Unit_Provider is

   ---------------------
   -- Create_Provider --
   ---------------------

   function Create_Provider
     (Runtime_Directories   : String_Vectors.Vector;
      Dependencies_Filename : String) return Provider_Type
   is
      use GNATCOLL.VFS;

      Provider : Provider_Type;

      Dependencies_File : File_Type;

   begin
      --  Parse the mapping file and fill Provider.Unit_Map

      Open (Dependencies_File, In_File, Dependencies_Filename);

      --  The mapping file is a succession of triplets as such:
      --  <unit_name>{%b,%s}
      --  <file_basename>
      --  <file_fullname>

      begin
         while not End_Of_File (Dependencies_File) loop
            declare
               Unit_Name      : constant String :=
                 Get_Line (Dependencies_File);
               Dummy_Basename : constant String :=
                 Get_Line (Dependencies_File);
               File_Fullname  : constant String :=
                 Get_Line (Dependencies_File);
            begin
               Provider.Unit_Map.Insert (To_Lower (Unit_Name), File_Fullname);
            end;
         end loop;
      exception
         when End_Error =>
            Outputs.Warn ("mapping file """ & Dependencies_Filename
                          & " "" is truncated");
      end;
      Close (Dependencies_File);

      --  Read the runtime files and fill Provider.Runtime_Files.
      --
      --  Note that the GPR system does not ensure that all runtime directories
      --  actually exist, so do not crash in this case.

      for Dirname of Runtime_Directories loop
         declare
            Runtime_Dir : constant Virtual_File := Create (+(+Dirname));
            Files       : File_Array_Access;
         begin
            if Runtime_Dir.Is_Directory then
               Files := Runtime_Dir.Read_Dir;
               for File of Files.all loop

                  --  It is allowed to have multiple version of the same file
                  --  in a project: the builder will pick the first one found.
                  --  Apply the same semantics here, and ignore all later
                  --  occurrences of a file already encountered.

                  if not Provider.Runtime_Files.Contains (+File.Base_Name) then
                     Provider.Runtime_Files.Insert
                       (+File.Base_Name, +File.Full_Name);
                  end if;
               end loop;
               Unchecked_Free (Files);
            end if;
         end;
      end loop;

      return Provider;
   end Create_Provider;

   ----------------------------------
   -- Create_Provider_From_Project --
   ----------------------------------

   function Create_Provider_From_Project return Unit_Provider_Reference
   is
      Mapping_File : constant String :=
        Create_Ada_Mapping_File (Project.Project.Root_Project);
      --  The mapping file will be removed by the Finalize procedure in
      --  project.adb.

   begin
      if Mapping_File = "" then
         Outputs.Fatal_Error
           ("Error when creating the mapping file for the project "
            & Project.Project.Root_Project.Name);
      end if;

      return Create_Unit_Provider_Reference
        (Create_Provider
           (Runtime_Directories   => Project.Runtime_Dirs,
            Dependencies_Filename => Mapping_File));
   end Create_Provider_From_Project;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Provider_Type;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String
   is
      use String_Maps;

      Part      : constant String :=
        (case Kind is
            when Libadalang.Common.Unit_Body          => "%b",
            when Libadalang.Common.Unit_Specification => "%s");
      Unit_Name : constant String :=
        Libadalang.Unit_Files.Unit_String_Name (Name) & Part;

      Unit_Name_Cur : Cursor := Provider.Unit_Map.Find (Unit_Name);
   begin
      if Has_Element (Unit_Name_Cur) then
         return Element (Unit_Name_Cur);
      end if;

      --  The requested unit does not belong to the project tree: look for a
      --  source file in the runtime.

      declare
         Basename : constant String :=
           Libadalang.Unit_Files.File_From_Unit (Name, Kind);
      begin
         Unit_Name_Cur := Provider.Runtime_Files.Find (Basename);
         if Has_Element (Unit_Name_Cur) then
            return Element (Unit_Name_Cur);
         end if;
      end;

      --  The unit could not be found. Return an empty string

      return "";
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Provider_Type;
      Context     : Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit'Class
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
               when Unit_Specification => "specification file",
               when Unit_Body          => "body file");
            Error      : constant Text_Type :=
               "Could not find source file for " & Name & " (" & Kind_Name
               & ")";
         begin
            return Libadalang.Analysis.Get_With_Error
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
      Unit_Part : Unit_Parts) return Boolean
   is
      Unit_Name_And_Part : constant String :=
        To_Lower (Unit_Name)
        & (case Unit_Part is
              when Unit_Spec => "%s",
              when others    => "%b");
   begin
      return Provider.Unit_Map.Contains (Unit_Name_And_Part);
   end Has_Unit;

end Instrument.Ada_Unit_Provider;
