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
     (Units,
      Excluded_Units,
      Routines,
      Excluded_Routines,

      Units_List,
      Excluded_Units_List,
      Routines_List,
      Excluded_Routines_List);

   subtype List_Attribute is
     Attribute range Units .. Excluded_Routines;
   subtype String_Attribute is
     Attribute range Units_List .. Excluded_Routines_List;

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

   package Tree_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => Project_Tree_Access);
   Prj_Trees : Tree_Maps.Map;

   procedure Initialize;
   --  Initialize project environment

   function List_From_Project
     (Prj            : Project_Type;
      List_Attr      : Attribute_Pkg_List;
      List_File_Attr : Attribute_Pkg_String) return String_Sets.Set;
   --  Return a vector containing each value of List_Attr (a list attribute),
   --  and each value from successive lines in the file denoted by
   --  List_File_Attr (a string attribute).

   procedure Enumerate_LIs
     (Prj_Tree       : Project_Tree_Access;
      LI_Cb          : access procedure (LI_Name : String);
      Override_Units : String_Sets.Set);
   --  Enumerate LIs from a single project tree

   procedure Compute_Project_View (Prj_Tree : Project_Tree_Access);
   --  Compute view for a single project tree

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

      procedure Process_One_Tree (C : Tree_Maps.Cursor);
      --  Compute project view for tree rooted at C

      ----------------------
      -- Process_One_Tree --
      ----------------------

      procedure Process_One_Tree (C : Tree_Maps.Cursor) is
      begin
         Compute_Project_View (Tree_Maps.Element (C));
      end Process_One_Tree;

   --  Start of processing for Compute_Project_View

   begin
      Prj_Trees.Iterate (Process_One_Tree'Access);
   end Compute_Project_View;

   procedure Compute_Project_View (Prj_Tree : Project_Tree_Access) is
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

   procedure Enumerate_LIs
     (LI_Cb          : access procedure (LI_Name : String);
      Override_Units : Inputs.Inputs_Type)
   is
      Units_Set : String_Sets.Set;

      procedure Add_Override (U : String);
      --  Add U to Units_Set

      procedure Process_One_Tree (C : Tree_Maps.Cursor);
      --  Enumerate LIs from the tree rooted at C

      ------------------
      -- Add_Override --
      ------------------

      procedure Add_Override (U : String) is
      begin
         Units_Set.Include (U);
      end Add_Override;

      ----------------------
      -- Process_One_Tree --
      ----------------------

      procedure Process_One_Tree (C : Tree_Maps.Cursor) is
      begin
         Enumerate_LIs (Tree_Maps.Element (C), LI_Cb, Units_Set);
      end Process_One_Tree;

   --  Start of processing for Enumerate_LIs

   begin
      Inputs.Iterate (Override_Units, Add_Override'Access);
      Prj_Trees.Iterate (Process_One_Tree'Access);
   end Enumerate_LIs;

   procedure Enumerate_LIs
     (Prj_Tree       : Project_Tree_Access;
      LI_Cb          : access procedure (LI_Name : String);
      Override_Units : String_Sets.Set)
   is
      Iter    : Project_Iterator :=
                  Start
                     (Root_Project     => Prj_Tree.Root_Project,
                      Recursive        =>
                        Switches.Recursive_Projects
                          or else not Override_Units.Is_Empty,
                      Include_Extended => False);

      Project : Project_Type;
   begin
      loop
         Project := Current (Iter);
         exit when Project = No_Project;

         declare
            Inc_Units, Exc_Units : String_Sets.Set;

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
            if Override_Units.Is_Empty then
               Inc_Units :=
                 List_From_Project
                   (Project,
                    List_Attr      => +Units,
                    List_File_Attr => +Units_List);
               Exc_Units :=
                 List_From_Project
                   (Project,
                    List_Attr      => +Excluded_Units,
                    List_File_Attr => +Excluded_Units_List);

            else
               Inc_Units := Override_Units;
            end if;

            Current (Iter).Library_Files (List => Lib_Info);
            Lib_Info.Iterate (Process_LI'Access);
         end;
         Next (Iter);
      end loop;
   end Enumerate_LIs;

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
      Prj_Tree : Project_Tree_Access;
   begin
      if Env = null then
         Initialize;
         pragma Assert (Env /= null);
      end if;

      Prj_Tree := new Project_Tree;
      Prj_Tree.Load
        (Root_Project_Path => Create (+Prj_Name),
         Env               => Env,
         Packages_To_Check => Coverage_Package_List'Access,
         Recompute_View    => False);

      Prj_Trees.Include (Prj_Name, Prj_Tree);
   end Load_Project;

end Project;
