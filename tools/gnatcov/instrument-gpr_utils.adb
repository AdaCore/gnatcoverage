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

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.VFS;  use GNATCOLL.VFS;

with GPR2; use GPR2;
with GPR2.Containers;
with GPR2.Build.Command_Line;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;

with Instrument.Common; use Instrument.Common;
with JSON;              use JSON;
with Paths;             use Paths;
with Project;           use Project;

package body Instrument.GPR_Utils is

   ------------------------
   -- Load_Naming_Scheme --
   ------------------------

   function Load_Naming_Scheme
     (Prj : GPR2.Project.View.Object) return Naming_Scheme_Desc
   is
      --  Load the naming scheme from project attributes

      package R renames GPR2.Project.Registry.Attribute;

      NS : Naming_Scheme_Desc;
   begin
      for Lang in Some_Language loop
         if Builtin_Support (Lang)
           and then Prj.Language_Ids.Contains (To_Language_Id (Lang))
         then
            declare
               Body_Suffix : constant String :=
                 Project.Source_Suffix (Lang, GPR2.S_Body, Prj);
               Spec_Suffix : constant String :=
                 Project.Source_Suffix (Lang, GPR2.S_Spec, Prj);
            begin
               NS.Body_Suffix (Lang) := +Body_Suffix;
               NS.Spec_Suffix (Lang) := +Spec_Suffix;
            end;
         end if;
      end loop;
      NS.Dot_Replacement :=
        +Prj.Attribute (Name => R.Naming.Dot_Replacement).Value.Text;
      NS.Casing :=
        Casing_From_String
          (Prj.Attribute (Name => R.Naming.Casing).Value.Text,
           "project " & String (Prj.Name));
      return NS;
   end Load_Naming_Scheme;

   ------------------
   -- SID_Filename --
   ------------------

   function SID_Filename
     (Main_Part_Src : GPR2.Build.Source.Object; In_Library_Dir : Boolean)
      return String
   is
      --  Determine in which project we will put this SID file, and the
      --  basename for the SID file to create. Mimic how GNAT creates ALI
      --  files: use the project of the main source of the library unit, start
      --  from the basename of that source file, replace the last extension
      --  with ".sid". Also make sure to use the most extending project in the
      --  hierarchy, which is where GPRbuild puts ALI/object files.

      SID_Basename : Unbounded_String;

      Prj              : GPR2.Project.View.Object renames
        Main_Part_Src.Owning_View;
      Output_Directory : constant GPR2.Path_Name.Object :=
        (if In_Library_Dir and then Prj.Is_Library
         then Prj.Library_Ali_Directory
         else Prj.Object_Directory);

   begin
      case Language_Kind (To_Language (Main_Part_Src.Language)) is
         when Unit_Based_Language =>
            SID_Basename :=
              +(String (Main_Part_Src.Path_Name.Base_Name) & ".sid");

         when File_Based_Language =>

            --  TODO (eng/toolchain/gnat#603)??? Ada.Directories.Simple_Name
            --  fails in edge cases. Use GNATCOLL.VFS instead for more reliable
            --  results.

            SID_Basename :=
              +Main_Part_Src.Path_Name.Virtual_File.Display_Base_Name & ".sid";
      end case;

      return String (Output_Directory.Value) / (+SID_Basename);
   end SID_Filename;

   ------------------------------
   -- Compilation_Unit_Options --
   ------------------------------

   function Compilation_Unit_Options
     (IC   : Inst_Context;
      Prj  : Prj_Desc;
      Lang : Src_Supported_Language;
      Src  : GPR2.Build.Source.Object) return Command_Line_Args
   is
      use GPR2.Build.Command_Line;
      Result : Command_Line_Args;
   begin
      --  Depending on the language, pass the right set of options

      if Lang = Switches.Ada_Language then
         Result.Append
           (Create ("--ada-default-charset=" & (+IC.Ada_Default_Charset)));
         Result.Append (Create ("--gnatem", Mode => Ignore));
         Result.Append (Create (Arg => +IC.Mapping_File, Mode => Ignore));
         Result.Append (Create ("--config-pragmas-mapping", Mode => Ignore));
         Result.Append
           (Create (Arg => +IC.Config_Pragmas_Mapping, Mode => Ignore));
         Result.Append (Create ("--ada-preprocessor-data", Mode => Ignore));
         Result.Append (Create (+IC.Ada_Preprocessor_Data_File));
      end if;

      --  Pass the list of sources of interest, to e.g. skip the
      --  instrumentation of the spec / body / subunit for an Ada unit if
      --  it was requested through a --ignored-source-files switch.

      Result.Append (Create ("--files", Mode => Ignore));
      Result.Append
        (Create
           (Arg  => "@" & (+IC.Sources_Of_Interest_Response_File),
            Mode => Ignore));

      --  Pass the right language

      Result.Append (Create ("--lang=" & Image (Lang)));

      --  Pass the project description options

      Result.Append (Command_Line_Args'(Unparse (Prj, Src, Lang)));
      return Result;
   end Compilation_Unit_Options;

   -----------------------
   -- Load_From_Project --
   -----------------------

   function Load_From_Project (Prj : GPR2.Project.View.Object) return Prj_Desc
   is
      package R renames GPR2.Project.Registry.Attribute;

      Languages : constant GPR2.Containers.Language_Set := Prj.Language_Ids;
      Result    : Prj_Desc;
   begin
      Result.Prj_Name := To_Qualified_Name (String (Prj.Name));

      --  Load the naming scheme from project attributes

      declare
         NS : Naming_Scheme_Desc renames Result.Naming_Scheme;
      begin
         for Lang in Some_Language loop
            if Builtin_Support (Lang)
              and then Languages.Contains (To_Language_Id (Lang))
            then
               declare
                  Body_Suffix : constant String :=
                    Project.Source_Suffix (Lang, GPR2.S_Body, Prj);
                  Spec_Suffix : constant String :=
                    Project.Source_Suffix (Lang, GPR2.S_Spec, Prj);
               begin
                  NS.Body_Suffix (Lang) := +Body_Suffix;
                  NS.Spec_Suffix (Lang) := +Spec_Suffix;
               end;
            end if;
         end loop;
         NS.Dot_Replacement :=
           +Prj.Attribute (Name => R.Naming.Dot_Replacement).Value.Text;
         NS.Casing :=
           Casing_From_String
             (Prj.Attribute (Name => R.Naming.Casing).Value.Text,
              "project " & String (Prj.Name));
      end;

      --  Register the source directories of the project tree

      for Lang in C_Family_Language loop
         for Dir_Path of Prj.Include_Path (To_Language_Id (Lang)) loop
            Result.Search_Paths (Lang).Append
              (+("-I" & String (Dir_Path.Name)));
         end loop;
      end loop;

      --  Load the set of compiler switches for languages requiring it

      for Lang in C_Family_Language loop
         if Languages.Contains (To_Language_Id (Lang)) then
            declare
               Lang_Index : constant GPR2.Project.Attribute_Index.Object :=
                 GPR2.Project.Attribute_Index.Create (Image (Lang));

               package R renames GPR2.Project.Registry.Attribute;

               Options         : Analysis_Options;
               Switches        : GPR2.Project.Attribute.Object;
               Compiler_Driver : constant GPR2.Project.Attribute.Object :=
                 Prj.Attribute
                   (Name => R.Compiler.Driver, Index => Lang_Index);

            begin
               --  Get the compiler switches from the project file. When
               --  registering a compilation unit for instrumentation, we also
               --  fill the compilation unit specific switches that will
               --  override the project defaults, if there are any (see
               --  Add_Instrumented_Unit).
               --
               --  Language specific switches can be specified through the
               --  Compiler.Switches or the Compiler.Default_Switches
               --  attribute, the former being prioritized over the latter.

               Switches :=
                 Prj.Attribute
                   (Name => R.Compiler.Switches, Index => Lang_Index);

               if not Switches.Is_Defined then
                  Switches :=
                    Prj.Attribute
                      (Name  => R.Compiler.Default_Switches,
                       Index => Lang_Index);
               end if;

               --  If we manage to find appropriate switches, convert them to a
               --  string vector import the switches.

               if Switches.Is_Defined then
                  declare
                     Args : String_Vectors.Vector;
                  begin
                     for S of Switches.Values loop
                        Args.Append (+S.Text);
                     end loop;
                     Import_From_Args (Options, Args);
                  end;
               end if;

               Add_Options
                 (Result.Compiler_Options (Lang),
                  Options,
                  Pass_Builtins => False);

               --  If we could not find a C compiler, stay silent at this
               --  point: the C instrumenter will complain if needed.

               Result.Compiler_Driver (Lang) :=
                 +(if Compiler_Driver.Is_Defined
                   then Compiler_Driver.Value.Text
                   else "");
            end;
         end if;
      end loop;

      return Result;
   end Load_From_Project;

   --------------------------------
   -- Get_Or_Create_Project_Info --
   --------------------------------

   function Get_Or_Create_Project_Info
     (Context : in out Inst_Context; Project : GPR2.Project.View.Object)
      return Project_Info_Access
   is
      use Project_Info_Maps;

      --  Look for an existing Project_Info record corresponding to Project

      Project_Name : constant Unbounded_String := +String (Project.Name);
      Position     : constant Cursor :=
        Context.Project_Info_Map.Find (Project_Name);
   begin
      if Has_Element (Position) then
         return Element (Position);

      else
         --  The requested Project_Info record does not exist yet. Create it,
         --  register it and return it.

         declare
            Result : constant Project_Info_Access :=
              new Project_Info'
                (Project          => Project,
                 Externally_Built => Project.Is_Externally_Built,
                 Desc             => Load_From_Project (Project));
         begin
            Result.Desc.Output_Dir := Project_Output_Dir (Project);
            Context.Project_Info_Map.Insert (Project_Name, Result);
            return Result;
         end;
      end if;
   end Get_Or_Create_Project_Info;

   -------------------------------
   -- Instrumentation_Artifacts --
   -------------------------------

   function Instrumentation_Artifacts
     (Main_Part_Src : GPR2.Build.Source.Object; Prj : Prj_Desc)
      return File_Sets.Set
   is
      Result : File_Sets.Set;

      Unit_Instr_Info_File : constant Virtual_File :=
        Files_Instrumentation_Info_File
          (Prj, Project.Get_Unit_Name (Main_Part_Src));
   begin
      Result.Insert (Unit_Instr_Info_File);
      if not Unit_Instr_Info_File.Is_Regular_File then
         return File_Sets.Empty_Set;
      end if;

      --  Source instrumentation artifacts generated by the source
      --  instrumentation command are to be found in the JSON.
      --
      --  Other instrumentation artifacts are stored in the project
      --  description.

      declare
         Unit_Instr_Info_JSON : constant JSON_Value :=
           JSON.Read_File (Unit_Instr_Info_File.Display_Full_Name);
         Artifacts            : constant JSON_Array :=
           Unit_Instr_Info_JSON.Get ("artifacts");
      begin
         for Art of Artifacts loop
            Result.Insert (Join (Prj.Output_Dir, +Art.Get));
         end loop;
      end;
      for Art of Prj.Instr_Artifacts loop
         Result.Insert (Art);
      end loop;
      return Result;
   end Instrumentation_Artifacts;

end Instrument.GPR_Utils;
