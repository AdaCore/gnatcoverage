------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;         use Ada.Directories;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with GNATCOLL.Traces;   use GNATCOLL.Traces;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Diagnostics; use Diagnostics;
with Inputs;      use Inputs;
with Outputs;     use Outputs;
with Switches;    use Switches;

package body Project is

   Coverage_Package      : aliased String := "coverage";
   Coverage_Package_List : aliased String_List :=
                             (1 => Coverage_Package'Access);

   type Attribute is
     (Units,
      Excluded_Units,
      Routines,
      Excluded_Routines,
      Switches,

      Units_List,
      Excluded_Units_List,
      Routines_List,
      Excluded_Routines_List);

   subtype List_Attribute is
     Attribute range Units .. Switches;
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

   type Unit_Info is record
      Original_Name : Unbounded_String;
      --  Units are referenced in unit maps under their lowercased name.
      --  Here we record the name with original casing (from the project or
      --  the command line).

      LI_Seen : Boolean;
      --  Set true if the LI file for this unit has been seen
   end record;

   package Unit_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => Unit_Info);

   Env      : Project_Environment_Access;
   Prj_Tree : Project_Tree_Access;

   package Project_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => Project_Type);
   Prj_Map : Project_Maps.Map;

   procedure Initialize (Target : GNAT.Strings.String_Access);
   --  Initialize project environment. Target is the target prefix, or NULL
   --  for the native case.

   procedure List_From_Project
     (Prj            : Project_Type;
      List_Attr      : Attribute_Pkg_List;
      List_File_Attr : Attribute_Pkg_String;
      Units          : out Unit_Maps.Map;
      Defined        : out Boolean);
   --  Return a vector containing each value of List_Attr (a list attribute),
   --  and each value from successive lines in the file denoted by
   --  List_File_Attr (a string attribute). Defined is set True if either
   --  List_Attr or List_File_Attr is defined explicitly in the project.

   procedure Report_Units_Without_LI (Units : Unit_Maps.Map; Origin : String);
   --  Output a warning for any element of Units that has LI_Seen set False.
   --  Origin indicates where the Units list comes from.

   procedure Enumerate_LIs
     (Root_Project       : Project_Type;
      LI_Cb              : access procedure (LI_Name : String);
      Override_Units_Map : in out Unit_Maps.Map;
      Recursive          : Boolean);
   --  Enumerate LI files for Root_Project. If Recursive is True, do so for
   --  the whole subtree rooted at Root_Project.

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

   -----------------
   -- Add_Project --
   -----------------

   procedure Add_Project (Prj_Name : String) is
      Prj         : Project_Type;
      Prj_Name_FS : constant GNATCOLL.VFS.Filesystem_String :=
        +Simple_Name (Prj_Name);
      Last        : Integer;
   begin
      --  Strip optional Project_File_Extension

      if Prj_Name_FS'Length >= Project_File_Extension'Length
           and then
         Prj_Name_FS (Prj_Name_FS'Last - Project_File_Extension'Length + 1
                      .. Prj_Name_FS'Last) = Project_File_Extension
      then
         Last := Prj_Name_FS'Last - Project_File_Extension'Length;
      else
         Last := Prj_Name_FS'Last;
      end if;

      --  Look up project from project tree

      Prj := Prj_Tree.Project_From_Name
        (+Prj_Name_FS (Prj_Name_FS'First .. Last));
      if Prj = No_Project then
         Fatal_Error ("project " & Prj_Name & " not found");
      end if;

      --  Add it to Prj_Map

      Prj_Map.Insert (Prj_Name, Prj);
   end Add_Project;

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

   procedure Enumerate_LIs
     (Root_Project       : Project_Type;
      LI_Cb              : access procedure (LI_Name : String);
      Override_Units_Map : in out Unit_Maps.Map;
      Recursive          : Boolean)
   is
      Iter    : Project_Iterator :=
                  Start
                     (Root_Project     => Root_Project,
                      Recursive        => Recursive,
                      Include_Extended => False);

      Project : Project_Type;

   begin
      loop
         Project := Current (Iter);
         exit when Project = No_Project;

         --  If project is extended, go to the ultimate extending project,
         --  which might override the Coverage package.

         Project := Extending_Project (Project, Recurse => True);

         Enumerate_Project : declare
            Lib_Info : Library_Info_Lists.List;

            Inc_Units         : Unit_Maps.Map;
            Inc_Units_Defined : Boolean;
            --  Units to be included, as specified in project

            Exc_Units         : Unit_Maps.Map;
            Exc_Units_Defined : Boolean;
            --  Units to be excluded, as specified in project

            procedure Filter_Lib_Info
              (Inc_Units : in out Unit_Maps.Map;
               Exc_Units : Unit_Maps.Map);
            --  Call LI_Cb for any LI file of the project that is in Inc_Units
            --  and not in Exc_Units.

            ---------------------
            -- Filter_Lib_Info --
            ---------------------

            procedure Filter_Lib_Info
              (Inc_Units : in out Unit_Maps.Map;
               Exc_Units : Unit_Maps.Map)
            is
            begin
               for LI of Lib_Info loop
                  Process_LI : declare
                     use Library_Info_Lists;
                     use Unit_Maps;

                     procedure Set_LI_Seen (U : String; UI : in out Unit_Info);
                     --  Record that the LI file for U was found

                     -----------------
                     -- Set_LI_Seen --
                     -----------------

                     procedure Set_LI_Seen
                       (U  : String;
                        UI : in out Unit_Info)
                     is
                        pragma Unreferenced (U);
                     begin
                        UI.LI_Seen := True;
                     end Set_LI_Seen;

                     U  : constant String :=
                       Unit_Name (Prj_Tree.Info (LI.Source_File));
                     UC : constant Unit_Maps.Cursor := Inc_Units.Find (U);

                  --  Start of processing for Process_LI

                  begin
                     if (UC /= Unit_Maps.No_Element
                         or else not Inc_Units_Defined)
                       and then not Exc_Units.Contains (U)
                     then
                        LI_Cb (+Full_Name (LI.Library_File));
                     end if;

                     --  Mark unit seen even if it is excluded

                     if UC /= Unit_Maps.No_Element then
                        Inc_Units.Update_Element (UC, Set_LI_Seen'Access);
                     end if;
                  end Process_LI;
               end loop;
            end Filter_Lib_Info;

         --  Start of processing for Enumerate_Project

         begin
            Project.Library_Files (List => Lib_Info);

            if Override_Units_Map.Is_Empty then
               List_From_Project
                 (Project,
                  List_Attr      => +Units,
                  List_File_Attr => +Units_List,
                  Units          => Inc_Units,
                  Defined        => Inc_Units_Defined);

               List_From_Project
                 (Project,
                  List_Attr       => +Excluded_Units,
                  List_File_Attr  => +Excluded_Units_List,
                  Units           => Exc_Units,
                  Defined         => Exc_Units_Defined);

               Filter_Lib_Info (Inc_Units, Exc_Units);
               Report_Units_Without_LI
                 (Inc_Units,
                  Origin => +Full_Name (Project.Project_Path));

            else

               --  Note: Exc_Units is intentionally left uninitialized (empty)
               --  in this call.

               Inc_Units_Defined := True;
               Filter_Lib_Info (Override_Units_Map, Exc_Units);
            end if;
         end Enumerate_Project;

         Next (Iter);
      end loop;
   end Enumerate_LIs;

   procedure Enumerate_LIs
     (LI_Cb          : access procedure (LI_Name : String);
      Override_Units : Inputs.Inputs_Type)
   is
      use Project_Maps;

      Override_Units_Map : Unit_Maps.Map;

      procedure Add_Override (U : String);
      --  Add U to Override_Units_Map

      ------------------
      -- Add_Override --
      ------------------

      procedure Add_Override (U : String) is
      begin
         Override_Units_Map.Include
           (To_Lower (U),
            (Original_Name => To_Unbounded_String (U), LI_Seen => False));
      end Add_Override;

   --  Start of processing for Enumerate_LIs

   begin
      Iterate (Override_Units, Add_Override'Access);

      if not Override_Units_Map.Is_Empty then

         --  If --units is specified, always traverse the complete project
         --  tree from the root.

         Enumerate_LIs
           (Root_Project (Prj_Tree.all),
            LI_Cb,
            Override_Units_Map,
            Recursive => True);

      else
         --  No --units: only considered selected projects

         for Prj of Prj_Map loop
            Enumerate_LIs
              (Prj,
               LI_Cb,
               Override_Units_Map,
               Recursive => Standard.Switches.Recursive_Projects);
         end loop;
      end if;

      Report_Units_Without_LI (Override_Units_Map, Origin => "<command line>");
   end Enumerate_LIs;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Target : GNAT.Strings.String_Access) is
   begin
      Initialize (Env);
      for A in Attribute'Range loop
         declare
            Err : constant String :=
                    Register_New_Attribute
                      (Name    => A'Img,
                       Pkg     => Coverage_Package,
                       Is_List => A in List_Attribute,
                       Indexed => (A = Switches));
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
           (Gnatls       => (if Target = null then "" else Target.all & '-')
                              & "gnatls",
            GNAT_Version => Gnatls_Version,
            Errors       => null);
         if Verbose and then Gnatls_Version /= null then
            Put_Line
              ("default paths set from GNATLS " & Gnatls_Version.all);
            Free (Gnatls_Version);
         end if;
      end;
   end Initialize;

   -----------------------
   -- List_From_Project --
   -----------------------

   procedure List_From_Project
     (Prj            : Project_Type;
      List_Attr      : Attribute_Pkg_List;
      List_File_Attr : Attribute_Pkg_String;
      Units          : out Unit_Maps.Map;
      Defined        : out Boolean)
   is
      procedure Add_Line (S : String);
      --  Add S to Result

      --------------
      -- Add_Line --
      --------------

      procedure Add_Line (S : String) is
      begin
         Units.Include
           (To_Lower (S),
            (Original_Name => To_Unbounded_String (S), LI_Seen => False));
      end Add_Line;

   --  Start of processing for List_From_Project

   begin

      --  We check each attribute in sequence and the set we're filling
      --  is "defined" as soon as one is set explicitly.

      Defined := False;

      if Has_Attribute (Prj, List_Attr) then
         Defined := True;
         declare
            List_Attr_Value : String_List_Access :=
              Attribute_Value (Prj, List_Attr);
         begin
            for J in List_Attr_Value'Range loop
               Add_Line (List_Attr_Value (J).all);
               Free (List_Attr_Value (J));
            end loop;
            Free (List_Attr_Value);
         end;
      end if;

      if Has_Attribute (Prj, List_File_Attr) then
         Defined := True;
         declare
            List_File_Attr_Value : constant String :=
              Attribute_Value (Prj, List_File_Attr);
         begin
            Read_List_From_File
              (+Full_Name
                 (Create_From_Base
                    (Base_Name => +List_File_Attr_Value,
                     Base_Dir  => Dir_Name (Project_Path (Prj)))),
               Add_Line'Access);
         end;
      end if;
   end List_From_Project;

   -----------------------
   -- Load_Root_Project --
   -----------------------

   procedure Load_Root_Project
     (Prj_Name : String;
      Target   : GNAT.Strings.String_Access)
   is
   begin
      if Prj_Tree /= null then
         Fatal_Error ("only one root project can be specified");
      end if;

      --  Allow activation of GNATcoll debug traces via configuration file,
      --  prior to initializing the project subsystem.

      GNATCOLL.Traces.Parse_Config_File (Filename => No_File);

      pragma Assert (Env = null);
      Initialize (Target);
      pragma Assert (Env /= null);

      Prj_Tree := new Project_Tree;
      Prj_Tree.Load
        (Root_Project_Path => Create (+Prj_Name),
         Env               => Env,
         Packages_To_Check => Coverage_Package_List'Access,
         Recompute_View    => False,
         Errors            => Outputs.Warning_Or_Error'Access);
   end Load_Root_Project;

   -----------------------------
   -- Report_Units_Without_LI --
   -----------------------------

   procedure Report_Units_Without_LI
     (Units  : Unit_Maps.Map;
      Origin : String)
   is
   begin
      for UI of Units loop
         if not UI.LI_Seen then
            Report
              (Origin & ": no information found for unit "
               & To_String (UI.Original_Name),
               Kind => Warning);
         end if;
      end loop;
   end Report_Units_Without_LI;

   ---------------------------
   -- Switches_From_Project --
   ---------------------------

   function Switches_From_Project (Op : String) return String_List_Access is
   begin
      return Attribute_Value
        (Prj_Tree.Root_Project, +Switches, Index => Op);
   end Switches_From_Project;

   -----------------
   -- Set_Subdirs --
   -----------------

   procedure Set_Subdirs (Subdir : String) is
   begin
      --  The --subdirs switch is relevant only if projects are used, otherwise
      --  it can safely be ignored.

      if Env /= null then
         Env.Set_Object_Subdir (+Subdir);
      end if;
   end Set_Subdirs;

end Project;
