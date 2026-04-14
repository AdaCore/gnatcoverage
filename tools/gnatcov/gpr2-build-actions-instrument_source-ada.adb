------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2026, AdaCore                       --
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

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Compilation_Unit;
with GPR2.Project.View.Set;

with Files_Table; use Files_Table;
with Text_Files;

package body GPR2.Build.Actions.Instrument_Source.Ada is

   -----------------------
   -- Compute_Signature --
   -----------------------

   procedure Compute_Signature
     (Self            : in out Object;
      Signature       : in out GPR2.Build.Signature.Object;
      Check_Checksums : Boolean)
   is
      Exit_Signature_Exception : exception;

      procedure Process_Part
        (Kind     : Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : Path_Name.Object;
         Index    : Unit_Index;
         Sep_Name : Optional_Name_Type);
      --  Add the given Path as an input of the signature. Raises
      --  Exit_Signature_Exception if the call to Self.Signature.Add_Input
      --  failed.

      ------------------
      -- Process_Part --
      ------------------

      procedure Process_Part
        (Kind     : Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : Path_Name.Object;
         Index    : Unit_Index;
         Sep_Name : Optional_Name_Type)
      is
         pragma Unreferenced (Kind, View, Index, Sep_Name);
      begin
         if not Signature.Add_Input
                  (GPR2.Build.Artifacts.Files.Create (Path), Check_Checksums)
         then
            raise Exit_Signature_Exception;
         end if;
      end Process_Part;

   begin
      --  Add all of the unit parts of interest as dependencies

      begin
         Self.LU_Info.Instr_Project.Own_Unit
           (Self.LU_Info.Main_Part_Src.Unit.Name)
           .For_All_Part (Process_Part'Access);
      exception
         when Exit_Signature_Exception =>
            return;
      end;

      --  Add the preprocessor data file as input

      if not Signature.Add_Input
               (GPR2.Build.Artifacts.Files.Create
                  (Filename_Type (+Self.IC.Ada_Preprocessor_Data_File)),
                Check_Checksums)
      then
         return;
      end if;

      --  Also add the configuration pragma file

      if not Signature.Add_Input
               (GPR2.Build.Artifacts.Files.Create
                  (Filename_Type (+Self.IC.Config_Pragmas_Mapping)),
                Check_Checksums)
      then
         return;
      end if;

      Self.Common_Compute_Signature (Signature, Check_Checksums);
   end Compute_Signature;

   ------------------
   -- Dependencies --
   ------------------

   function Dependencies (Self : in out Object) return Containers.Filename_Set
   is
      Result : Containers.Filename_Set;

      Namespace_Root : constant GPR2.Project.View.Set.Object :=
        Self.LU_Info.Instr_Project.Namespace_Roots;
      pragma Assert (Namespace_Root.Length = 1);
      CU             : constant GPR2.Build.Compilation_Unit.Object :=
        Self.LU_Info.Instr_Project.Own_Unit
          (Self.LU_Info.Main_Part_Src.Unit.Name);
   begin
      for Dep of CU.Known_Dependencies loop
         declare
            Dep_Unit : constant GPR2.Build.Compilation_Unit.Object :=
              Namespace_Root.First_Element.Unit (Dep);
         begin
            if Dep_Unit.Is_Defined and then Dep_Unit.Has_Part (S_Spec) then

               --  Use Include rather than Insert in case of a multi unit
               --  source dependency.

               Result.Include (Dep_Unit.Spec.Source.Name);
            end if;
         end;
      end loop;
      return Result;
   end Dependencies;

   ------------------------------
   -- Write_Instrumented_Files --
   ------------------------------

   procedure Write_Instrumented_Files_List (Self : in out Object) is
      use Text_Files;
      F : File_Type;

      procedure Process_Unit_Part
        (Kind     : GPR2.Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : GPR2.Path_Name.Object;
         Index    : GPR2.Unit_Index;
         Sep_Name : GPR2.Optional_Name_Type);

      -----------------------
      -- Process_Unit_Part --
      -----------------------

      procedure Process_Unit_Part
        (Kind     : GPR2.Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : GPR2.Path_Name.Object;
         Index    : GPR2.Unit_Index;
         Sep_Name : GPR2.Optional_Name_Type)
      is
         pragma Unreferenced (Kind, View, Index, Sep_Name);
      begin
         if Self.IC.Files_Of_Interest.Contains (Path.Virtual_File) then
            F.Put_Line (String (Path.Name));
         end if;
      end Process_Unit_Part;

      Prj : constant GPR2.Project.View.Object :=
        Self.LU_Info.Instr_Project.Namespace_Roots.First_Element;
      CU  : constant GPR2.Build.Compilation_Unit.Object :=
        Prj.Unit (Self.LU_Info.Main_Part_Src.Unit.Name);
   begin
      --  The instrumented files are all the unit parts that are of interest

      F.Create
        (Name =>
           Instrumented_Files_File
             (Self.Prj_Info.Desc,
              Self.LU_Info.Main_Part_Src.Path_Name.Virtual_File)
             .Display_Full_Name);
      CU.For_All_Part (Process_Unit_Part'Access);
      Close (F);
   end Write_Instrumented_Files_List;

end GPR2.Build.Actions.Instrument_Source.Ada;
