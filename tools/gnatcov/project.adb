------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2012, AdaCore                       --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Strings;      use GNAT.Strings;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Inputs;   use Inputs;
with Outputs;  use Outputs;
with Switches; use Switches;

package body Project is

   Coverage_Package      : aliased String := "coverage";
   Coverage_Package_List : aliased String_List :=
                             (1 => Coverage_Package'Access);

   type Attribute is
     (Default_Switches,
      Units,
      Excluded_Units,
      Routines,
      Excluded_Routines,

      Level,
      Output_Format,
      Units_List,
      Excluded_Units_List,
      Routines_List,
      Excluded_Routines_List);

   subtype List_Attribute is
     Attribute range Default_Switches .. Excluded_Routines;
   subtype String_Attribute is
     Attribute range Level .. Excluded_Routines_List;

   function "+" (A : String_Attribute) return Attribute_Pkg_String;
   function "+" (A : List_Attribute) return Attribute_Pkg_List;
   --  Build identifiers for attributes in package Coverage

   package Scv_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => String);
   Scv_Map : Scv_Maps.Map;
   --  All defined scenario variables

   package String_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets (Element_Type => String);

   Env      : Project_Environment_Access;
   Prj_Tree : Project_Tree_Access;

   procedure Initialize;
   --  Initialize project environment

   function List_From_Project
     (Prj            : Project_Type;
      List_Attr      : Attribute_Pkg_List;
      List_File_Attr : Attribute_Pkg_String) return String_Sets.Set;
   --  Return a vector containing each value of List_Attr (a list attribute),
   --  and each value from successive lines in the file denoted by
   --  List_File_Attr (a string attribute).

   ---------
   -- "+" --
   ---------

   function "+" (A : String_Attribute) return Attribute_Pkg_String is
   begin
      return Build (Coverage_Package, A'Img);
   end "+";

   function "+" (A : List_Attribute) return Attribute_Pkg_List is
   begin
      return Build (Coverage_Package, A'Img);
   end "+";

   ----------------------
   -- Add_Scenario_Var --
   ----------------------

   procedure Add_Scenario_Var (Key, Value : String) is
   begin
      Scv_Map.Include (Key, Value);
   end Add_Scenario_Var;

   --------------------------
   -- Compute_Project_View --
   --------------------------

   procedure Compute_Project_View is
      Vars     : Scenario_Variable_Array := Prj_Tree.Scenario_Variables;
      Changed  : Boolean := False;
      --  Set True if one scenario variable is specified explicitly

   begin
      --  First set values of all specified scenario variables

      for J in Vars'Range loop
         declare
            Ext_Name : constant String := External_Name (Vars (J));
         begin
            if Scv_Map.Contains (Ext_Name) then
               Set_Value (Vars (J), Scv_Map.Element (Ext_Name));
               Changed := True;
            end if;
         end;
      end loop;

      if Changed then
         Prj_Tree.Change_Environment (Vars);
      end if;

      --  Then compute project view

      Prj_Tree.Recompute_View;

   end Compute_Project_View;

   -------------------
   -- Enumerate_LIs --
   -------------------

   procedure Enumerate_LIs (LI_Cb : access procedure (LI_Name : String)) is
      Iter    : Project_Iterator := Start (Prj_Tree.Root_Project);
      Project : Project_Type;
   begin
      loop
         Project := Current (Iter);
         exit when Project = No_Project;

         declare
            Inc_Units : constant String_Sets.Set :=
                          List_From_Project
                            (Project,
                             List_Attr      => +Units,
                             List_File_Attr => +Units_List);
            Exc_Units : constant String_Sets.Set :=
                          List_From_Project
                            (Project,
                             List_Attr      => +Excluded_Units,
                             List_File_Attr => +Excluded_Units_List);

            use Library_Info_Lists;

            procedure Process_LI (C : Cursor);
            --  Add the LI file to SCO_Inputs, if it is meant to be included

            ----------------
            -- Process_LI --
            ----------------

            procedure Process_LI (C : Cursor) is
               U : constant String :=
                     Unit_Name (Prj_Tree.Info (Element (C).Source_File));
            begin
               if (Inc_Units.Is_Empty
                     or else Inc_Units.Contains (U))
                 and then not Exc_Units.Contains (U)
               then
                  LI_Cb (+Full_Name (Element (C).Library_File));
               end if;
            end Process_LI;

            Lib_Info : List;

         begin
            Prj_Tree.Root_Project.Library_Files (List => Lib_Info);
            Lib_Info.Iterate (Process_LI'Access);
         end;
         Next (Iter);
      end loop;
   end Enumerate_LIs;

   ---------------------
   -- Enumerate_Mains --
   ---------------------

   procedure Enumerate_Mains
     (Main_Cb : access procedure (Main_Name : String))
   is
      Exec_Dir : constant Virtual_File :=
                   Executables_Directory (Prj_Tree.Root_Project);
      Mains : String_List_Access :=
                 Attribute_Value (Prj_Tree.Root_Project, Main_Attribute);
   begin
      for J in Mains'Range loop
         Main_Cb
           (+Full_Name
              (Create_From_Dir
                 (Exec_Dir,
                  Prj_Tree.Root_Project.Executable_Name (+Mains (J).all))));
         Free (Mains (J));
      end loop;
      Free (Mains);
   end Enumerate_Mains;

   ---------------
   -- Get_Level --
   ---------------

   function Get_Level return String is
   begin
      return Prj_Tree
               .Root_Project
               .Attribute_Value
                  (Attribute      => Build (Coverage_Package, Level'Img),
                   Index          => "",
                   Default        => "",
                   Use_Extended   => False);
   end Get_Level;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialize (Env);
      for A in Attribute'Range loop
         declare
            Err : constant String :=
                    Register_New_Attribute
                      (Name    => A'Img,
                       Pkg     => Coverage_Package,
                       Is_List => A in List_Attribute);
         begin
            if Err /= "" then
               Fatal_Error (Err);
            end if;
         end;
      end loop;
      Prj_Tree := new Project_Tree;

      declare
         Gnatls_Version : GNAT.Strings.String_Access;
      begin
         Env.Set_Path_From_Gnatls
           (Gnatls       => "gnatls",
            GNAT_Version => Gnatls_Version,
            Errors       => null);
         if Verbose then
            if Gnatls_Version /= null then
               Put_Line
                 ("default paths set from GNATLS " & Gnatls_Version.all);
               Free (Gnatls_Version);
            end if;
         end if;
      end;
   end Initialize;

   -----------------------
   -- List_From_Project --
   -----------------------

   function List_From_Project
     (Prj            : Project_Type;
      List_Attr      : Attribute_Pkg_List;
      List_File_Attr : Attribute_Pkg_String) return String_Sets.Set
   is
      Result : String_Sets.Set;

      procedure Add_Line (S : String);
      --  Add S to Result

      --------------
      -- Add_Line --
      --------------

      procedure Add_Line (S : String) is
      begin
         Result.Include (S);
      end Add_Line;

      List_Attr_Value      : String_List_Access :=
                               Attribute_Value (Prj, List_Attr);
      List_File_Attr_Value : constant String :=
                               Attribute_Value (Prj, List_File_Attr);

   --  Start of processing for List_From_Project

   begin
      if List_Attr_Value /= null then
         for J in List_Attr_Value'Range loop
            Result.Include (List_Attr_Value (J).all);
            Free (List_Attr_Value (J));
         end loop;
         Free (List_Attr_Value);
      end if;

      if List_File_Attr_Value /= "" then
         Read_List_From_File
           (+Full_Name
              (Create_From_Base (Base_Name => +List_File_Attr_Value,
                                 Base_Dir  => Dir_Name (Project_Path (Prj)))),
            Add_Line'Access);
      end if;

      return Result;
   end List_From_Project;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project (Prj_Name : String) is
   begin
      pragma Assert (Env = null and then Prj_Tree = null);
      Initialize;

      pragma Assert (Env /= null);

      Prj_Tree.Load
        (Root_Project_Path => Create (+Prj_Name),
         Env               => Env,
         Packages_To_Check => Coverage_Package_List'Access,
         Recompute_View    => False);
   end Load_Project;

end Project;
