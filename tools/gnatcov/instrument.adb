------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2021, AdaCore                     --
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

--  Source instrumentation

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Characters.Conversions;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;

with GNAT.OS_Lib;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Project_Provider;
with Libadalang.Rewriting; use Libadalang.Rewriting;

with Checkpoints;
with Coverage;
with Instrument.Clean_Objdirs;
with Instrument.Common;  use Instrument.Common;
with Instrument.Find_Units;
with Instrument.Sources; use Instrument.Sources;
with Outputs;
with Project;
with SC_Obligations;
with Strings;
with Switches;
with Text_Files;

package body Instrument is

   package GPR renames GNATCOLL.Projects;

   package CU_Name_Vectors is new Ada.Containers.Vectors
     (Positive, Compilation_Unit_Name);

   type Library_Unit_Info is record
      CU_Names : CU_Name_Vectors.Vector;
      --  List of compilation units implementing this library unit

      Body_Project, Spec_Project : GPR.Project_Type;
      --  Projects that own the body/spec for this library unit
   end record;
   type Library_Unit_Info_Access is access Library_Unit_Info;

   package Library_Unit_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Library_Unit_Info_Access);
   --  Map to associate a list of compilation units to instrument to library
   --  units (indexed by library unit name).

   procedure Get_Or_Create
     (Map          : in out Library_Unit_Maps.Map;
      Library_Unit : String;
      Info         : out Library_Unit_Info_Access);
   --  Look for the info corresponding to Library_Unit in Map. Create it if
   --  it does not exist yet and put it in Info.

   function SID_Filename
     (Cur            : Library_Unit_Maps.Cursor;
      In_Library_Dir : Boolean) return String;
   --  Return the filename of the SID file to create for the given library
   --  unit. If In_Library_Dir is true, then return a filename located in the
   --  project library directory. Otherwise, the filename is located in the
   --  object directory.

   procedure Prepare_Output_Dirs (IC : Inst_Context);
   --  Make sure we have the expected tree of directories for the
   --  instrumentation output.

   procedure Emit_Buffer_Unit
     (Info : in out Project_Info; UIC : Unit_Inst_Context);
   --  Emit the unit to contain coverage buffers for the given instrumented
   --  unit.

   procedure Emit_Pure_Buffer_Unit
     (Info : in out Project_Info; UIC : Unit_Inst_Context);
   --  Emit the unit to contain addresses for the coverage buffers

   procedure Emit_Buffers_List_Unit
     (IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info);
   --  Emit in the root project a unit to contain the list of coverage buffers
   --  for all units of interest.

   procedure Auto_Dump_Buffers_In_Ada_Mains
     (IC    : in out Inst_Context;
      Mains : Main_To_Instrument_Vectors.Vector);
   --  Instrument source files in Mains to add a dump of coverage buffers

   -------------------
   -- Get_Or_Create --
   -------------------

   procedure Get_Or_Create
     (Map          : in out Library_Unit_Maps.Map;
      Library_Unit : String;
      Info         : out Library_Unit_Info_Access)
   is
      use Library_Unit_Maps;
      Cur : constant Cursor := Map.Find (Library_Unit);
   begin
      if Has_Element (Cur) then
         Info := Element (Cur);
      else
         Info := new Library_Unit_Info;
         Info.Body_Project := GPR.No_Project;
         Info.Spec_Project := GPR.No_Project;
         Map.Insert (Library_Unit, Info);
      end if;
   end Get_Or_Create;

   ------------------
   -- SID_Filename --
   ------------------

   function SID_Filename
     (Cur            : Library_Unit_Maps.Cursor;
      In_Library_Dir : Boolean) return String
   is
      use GNATCOLL.VFS;
      use type GPR.Project_Type;
      use all type GPR.Unit_Parts;
      use Library_Unit_Maps;

      LU_Name : constant String := Key (Cur);
      Info    : Library_Unit_Info renames Element (Cur).all;

      --  Determine in which project we will put this SID file, and the
      --  basename for the SID file to create. Mimic how GNAT creates ALI
      --  files: use the project of the main source of the library unit, start
      --  from the basename of that source file, replace the last extension
      --  with ".sid".

      Use_Spec : constant Boolean := Info.Body_Project = GPR.No_Project;
      Project  : constant GPR.Project_Type :=
        (if Use_Spec
         then Info.Spec_Project
         else Info.Body_Project);
      pragma Assert (Project /= GPR.No_Project);

      Src_Basename  : constant String := +Project.File_From_Unit
        (Unit_Name       => LU_Name,
         Part            => (if Use_Spec then Unit_Spec else Unit_Body),
         Language        => "Ada",
         File_Must_Exist => False);
      Src_Ext_Index : constant Positive :=
         Ada.Strings.Fixed.Index (Src_Basename, ".", Ada.Strings.Backward);
      SID_Basename  : constant String :=
         Src_Basename (Src_Basename'First ..  Src_Ext_Index) & "sid";

      Output_Directory : constant Virtual_File :=
        (if In_Library_Dir
         then Project.Library_Ali_Directory
         else Project.Object_Dir);
   begin
      return String'(+Output_Directory.Full_Name) / SID_Basename;
   end SID_Filename;

   -------------------------
   -- Prepare_Output_Dirs --
   -------------------------

   procedure Prepare_Output_Dirs (IC : Inst_Context) is
      use Project_Info_Maps;
   begin
      for Cur in IC.Project_Info_Map.Iterate loop
         declare
            Prj_Info   : Project_Info renames Element (Cur).all;
            Output_Dir : constant String :=
               +(Element (Cur).Output_Dir);
         begin
            --  Do not create output directories for externally built projects:
            --  we don't instrument them and we may not have filesystem
            --  permissions to create directories there.

            if not Prj_Info.Externally_Built
               and then not Ada.Directories.Exists (Output_Dir)
            then
               Ada.Directories.Create_Path (Output_Dir);
            end if;
         end;
      end loop;
   end Prepare_Output_Dirs;

   ----------------------
   -- Emit_Buffer_Unit --
   ----------------------

   procedure Emit_Buffer_Unit
     (Info : in out Project_Info; UIC : Unit_Inst_Context)
   is
      CU_Name : Compilation_Unit_Name renames UIC.Buffer_Unit;
      File    : Text_Files.File_Type;
   begin
      Create_File (Info, File, To_Filename (Info.Project, CU_Name));
      Put_Warnings_And_Style_Checks_Pragmas (File);

      declare
         Pkg_Name : constant String := To_Ada (CU_Name.Unit);

         Fingerprint : Unbounded_String;

         Unit_Name : constant String := Ada.Characters.Handling.To_Lower
           (To_Ada (UIC.Instrumented_Unit.Unit));

         Unit_Part : constant String :=
           (case UIC.Instrumented_Unit.Part is
            when GPR.Unit_Spec     => "Unit_Spec",
            when GPR.Unit_Body     => "Unit_Body",
            when GPR.Unit_Separate => "Unit_Separate");
         --  Do not use 'Image so that we use the original casing for the
         --  enumerators, and thus avoid compilation warnings/errors.

         Statement_Last_Bit : constant String := Img
           (UIC.Unit_Bits.Last_Statement_Bit);
         Decision_Last_Bit  : constant String := Img
           (UIC.Unit_Bits.Last_Outcome_Bit);
         MCDC_Last_Bit      : constant String := Img
           (UIC.Unit_Bits.Last_Path_Bit);

      begin
         --  Turn the fingerprint value into the corresponding Ada literal

         declare
            First : Boolean := True;
         begin
            Append (Fingerprint, "(");
            for Byte of SC_Obligations.Fingerprint (UIC.CU) loop
               if First then
                  First := False;
               else
                  Append (Fingerprint, ", ");
               end if;
               Append (Fingerprint, Strings.Img (Integer (Byte)));
            end loop;
            Append (Fingerprint, ")");
         end;

         File.Put_Line ("package " & Pkg_Name & " is");
         File.New_Line;
         File.Put_Line ("   pragma Preelaborate;");
         File.New_Line;
         File.Put_Line ("   Statement_Buffer : Coverage_Buffer_Type"
                        & " (0 .. " & Statement_Last_Bit & ") :="
                        & " (others => False);");
         File.Put_Line ("   Statement_Buffer_Address : constant System.Address"
                        & " := Statement_Buffer'Address;");
         File.Put_Line ("   pragma Export (Ada, Statement_Buffer_Address, """
                        & Statement_Buffer_Symbol (UIC.Instrumented_Unit)
                        & """);");
         File.New_Line;

         File.Put_Line ("   Decision_Buffer : Coverage_Buffer_Type"
                        & " (0 .. " & Decision_Last_Bit & ") :="
                        & " (others => False);");
         File.Put_Line ("   Decision_Buffer_Address : constant System.Address"
                        & " := Decision_Buffer'Address;");
         File.Put_Line ("   pragma Export (Ada, Decision_Buffer_Address, """
                        & Decision_Buffer_Symbol (UIC.Instrumented_Unit)
                        & """);");
         File.New_Line;

         File.Put_Line ("   MCDC_Buffer : Coverage_Buffer_Type"
                        & " (0 .. " & MCDC_Last_Bit & ") :="
                        & " (others => False);");
         File.Put_Line ("   MCDC_Buffer_Address : constant System.Address"
                        & " := MCDC_Buffer'Address;");
         File.Put_Line ("   pragma Export (Ada, MCDC_Buffer_Address, """
                        & MCDC_Buffer_Symbol (UIC.Instrumented_Unit)
                        & """);");
         File.New_Line;

         File.Put_Line ("   Buffers : aliased Unit_Coverage_Buffers :=");
         File.Put_Line ("     (Unit_Name_Length => "
                        & Strings.Img (Unit_Name'Length) & ",");
         File.Put_Line ("      Fingerprint => "
                        & To_String (Fingerprint) & ",");

         File.Put_Line ("      Unit_Part => " & Unit_Part & ",");
         File.Put_Line ("      Unit_Name => """ & Unit_Name & """,");

         File.Put_Line ("      Statement => Statement_Buffer'Address,");
         File.Put_Line ("      Decision  => Decision_Buffer'Address,");
         File.Put_Line ("      MCDC      => MCDC_Buffer'Address,");

         File.Put_Line ("      Statement_Last_Bit => " & Statement_Last_Bit
                        & ",");
         File.Put_Line ("      Decision_Last_Bit => " & Decision_Last_Bit
                        & ",");
         File.Put_Line ("      MCDC_Last_Bit => " & MCDC_Last_Bit & ");");
         File.New_Line;
         File.Put_Line ("end " & Pkg_Name & ";");
      end;
   end Emit_Buffer_Unit;

   ---------------------------
   -- Emit_Pure_Buffer_Unit --
   ---------------------------

   procedure Emit_Pure_Buffer_Unit
     (Info : in out Project_Info; UIC : Unit_Inst_Context)
   is
      use Ada.Characters.Conversions;
      use Ada.Strings.Wide_Wide_Unbounded;

      CU_Name  : Compilation_Unit_Name := UIC.Pure_Buffer_Unit;
      Pkg_Name : constant String := To_Ada (CU_Name.Unit);
      File     : Text_Files.File_Type;

      procedure Put_Language_Version_Pragma;
      --  If the instrumented unit has a language version configuration
      --  pragma, insert a consistent one here to ensure legality of
      --  degenerate subprograms supporting generics.

      ---------------------------------
      -- Put_Language_Version_Pragma --
      ---------------------------------

      procedure Put_Language_Version_Pragma is
      begin
         if Length (UIC.Language_Version_Pragma) > 0 then
            File.Put_Line
              ("pragma "
               & To_String (To_Wide_Wide_String (UIC.Language_Version_Pragma))
               & ";");
            File.New_Line;
         end if;
      end Put_Language_Version_Pragma;

   --  Start of processing for Emit_Pure_Buffer_Unit

   begin
      Create_File (Info, File, To_Filename (Info.Project, CU_Name));

      Put_Warnings_And_Style_Checks_Pragmas (File);
      Put_Language_Version_Pragma;
      File.Put_Line ("with System;");

      File.Put_Line ("with GNATcov_RTS;");
      File.Put_Line (Runtime_Version_Check);

      File.New_Line;
      File.Put_Line ("package " & Pkg_Name & " is");
      File.New_Line;
      File.Put_Line ("   pragma Pure;");
      File.New_Line;
      File.Put_Line ("   Statement_Buffer : constant System.Address;");
      File.Put_Line ("   pragma Import (Ada, Statement_Buffer, """
                     & Statement_Buffer_Symbol (UIC.Instrumented_Unit)
                     & """);");
      File.New_Line;
      File.Put_Line ("   Decision_Buffer : constant System.Address;");
      File.Put_Line ("   pragma Import (Ada, Decision_Buffer, """
                     & Decision_Buffer_Symbol (UIC.Instrumented_Unit)
                     & """);");
      File.New_Line;
      File.Put_Line ("   MCDC_Buffer : constant System.Address;");
      File.Put_Line ("   pragma Import (Ada, MCDC_Buffer, """
                     & MCDC_Buffer_Symbol (UIC.Instrumented_Unit)
                     & """);");
      File.New_Line;
      for G of UIC.Degenerate_Subprogram_Generics loop
         File.Put_Line
           ("   " & To_String (To_Wide_Wide_String (G.Generic_Subp_Decl)));
      end loop;
      File.Put_Line ("end " & Pkg_Name & ";");

      Text_Files.Close (File);

      if not UIC.Degenerate_Subprogram_Generics.Is_Empty then
         CU_Name.Part := GNATCOLL.Projects.Unit_Body;

         Create_File (Info, File, To_Filename (Info.Project, CU_Name));

         Put_Warnings_And_Style_Checks_Pragmas (File);
         Put_Language_Version_Pragma;
         File.Put_Line ("package body " & Pkg_Name & " is");
         File.New_Line;
         for G of UIC.Degenerate_Subprogram_Generics loop
            File.Put_Line
              ("   " & To_String (To_Wide_Wide_String (G.Generic_Subp_Body)));
         end loop;
         File.Put_Line ("end " & Pkg_Name & ";");

         Text_Files.Close (File);
      end if;
   end Emit_Pure_Buffer_Unit;

   ----------------------------
   -- Emit_Buffers_List_Unit --
   ----------------------------

   procedure Emit_Buffers_List_Unit
     (IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info)
   is
      use GPR;

      CU_Name : Compilation_Unit_Name := (Sys_Buffers_Lists, Unit_Spec);
      File    : Text_Files.File_Type;

      package Buffer_Vectors is new Ada.Containers.Vectors
        (Positive, Unbounded_String);

      Buffer_Units : Buffer_Vectors.Vector;
   begin
      CU_Name.Unit.Append (Ada_Identifier (IC.Project_Name));

      --  Compute the list of units that contain the coverage buffers to
      --  process.

      for Cur in IC.Instrumented_Units.Iterate loop
         declare
            I_Unit : constant Compilation_Unit_Name :=
               Instrumented_Unit_Maps.Key (Cur);
            B_Unit : constant String := To_Ada (Buffer_Unit (I_Unit));
         begin
            Buffer_Units.Append (+B_Unit);
         end;
      end loop;

      --  Now emit the unit to contain the list of buffers

      declare
         use type Ada.Containers.Count_Type;

         Unit_Name : constant String := To_Ada (CU_Name.Unit);
         First     : Boolean := True;
      begin
         Create_File
           (Root_Project_Info,
            File,
            To_Filename (Root_Project_Info.Project, CU_Name));
         Put_Warnings_And_Style_Checks_Pragmas (File);
         for Unit of Buffer_Units loop
            File.Put_Line ("with " & To_String (Unit) & ";");
         end loop;
         File.New_Line;
         File.Put_Line ("package " & Unit_Name & " is");
         File.New_Line;
         File.Put_Line ("   List : constant Unit_Coverage_Buffers_Array"
                        & " :=");
         File.Put ("     (");
         if Buffer_Units.Length = 1 then
            File.Put ("1 => ");
         end if;
         for Unit of Buffer_Units loop
            if First then
               First := False;
            else
               File.Put_Line (",");
               File.Put ((1 .. 6 => ' '));
            end if;
            File.Put (To_String (Unit) & ".Buffers'Access");
         end loop;
         File.Put_Line (");");
         File.New_Line;
         File.Put_Line ("end " & Unit_Name & ";");
      end;
   end Emit_Buffers_List_Unit;

   ------------------------------------
   -- Auto_Dump_Buffers_In_Ada_Mains --
   ------------------------------------

   procedure Auto_Dump_Buffers_In_Ada_Mains
     (IC    : in out Inst_Context;
      Mains : Main_To_Instrument_Vectors.Vector)
   is
   begin
      for Main of Mains loop
         declare
            use type GNATCOLL.VFS.Filesystem_String;

            Rewriter : Source_Rewriter;
         begin
            Rewriter.Start_Rewriting
              (IC, Main.Prj_Info.all, +Main.File.Full_Name);
            Add_Auto_Dump_Buffers
              (IC   => IC,
               Info => Main.Prj_Info.all,
               Main => Main.Unit,
               URH  => Libadalang.Rewriting.Handle (Rewriter.Rewritten_Unit));
            Rewriter.Apply;
         end;
      end loop;
   end Auto_Dump_Buffers_In_Ada_Mains;

   ----------------------------------
   -- Instrument_Units_Of_Interest --
   ----------------------------------

   procedure Instrument_Units_Of_Interest
     (Dump_Config          : Any_Dump_Config;
      Language_Version     : Any_Language_Version;
      Ignored_Source_Files : access GNAT.Regexp.Regexp)
   is
      use Libadalang.Analysis;

      --  First create the context for Libadalang

      Provider : constant Unit_Provider_Reference :=
         Libadalang.Project_Provider.Create_Project_Unit_Provider
           (Tree             => Project.Project,
            Project          => Project.Project.Root_Project,
            Env              => null,
            Is_Project_Owner => False);

      --  Create a map from library units to lists of compilation units to
      --  instrument for them.

      LU_Map          : Library_Unit_Maps.Map;
      Current_LU_Info : Library_Unit_Info_Access;

      Main_To_Instrument_Vector : Main_To_Instrument_Vectors.Vector;
      --  List of mains to instrument *which are not units of interest*. Always
      --  empty when Dump_Config.Trigger is Manual.
      --
      --  We need a separate list for these as mains which are units of
      --  interest are instrumented to dump coverage buffers at the same time
      --  they are instrumented to fill coverage buffers.

      --  Then create the instrumenter context

      IC                : Inst_Context := Create_Context
         (Provider, Dump_Config, Language_Version, Ignored_Source_Files);
      Root_Project_Info : constant Project_Info_Access :=
         Get_Or_Create_Project_Info (IC, Project.Project.Root_Project);

      procedure Find_Units_Wrapper
        (Project : GPR.Project_Type; Source_File : GPR.File_Info);
      --  Wrapper for Find_Units, callback for Enumerate_Ada_Sources

      procedure Instrument_Unit
        (CU_Name   : Compilation_Unit_Name;
         Unit_Info : in out Instrumented_Unit_Info);
      --  Instrument a single source file of interest from the project

      ------------------------
      -- Find_Units_Wrapper --
      ------------------------

      procedure Find_Units_Wrapper
        (Project : GPR.Project_Type; Source_File : GPR.File_Info)
      is
         procedure Add_Instrumented_Unit
           (CU_Name : Compilation_Unit_Name;
            Info    : GPR.File_Info);
         --  Wrapper for Instrument.Common.Add_Instrumented_Unit

         ---------------------------
         -- Add_Instrumented_Unit --
         ---------------------------

         procedure Add_Instrumented_Unit
           (CU_Name : Compilation_Unit_Name;
            Info    : GPR.File_Info)
         is
            use GNATCOLL.VFS;
         begin
            --  Skip this file if we were told to ignore it

            if Is_Ignored_Source_File (IC, +Info.File.Base_Name) then
               return;
            end if;

            Current_LU_Info.CU_Names.Append (CU_Name);
            Add_Instrumented_Unit (IC, Info.Project, Info);
         end Add_Instrumented_Unit;

         CU_Name : constant Compilation_Unit_Name :=
            To_Compilation_Unit_Name (Source_File);

      --  Start of processing for Find_Units_Wrapper

      begin
         --  Get the vector in which we will record the compilation units that
         --  the following call to Find_Units will list.

         Get_Or_Create (LU_Map, To_Ada (CU_Name.Unit), Current_LU_Info);

         --  Keep track of projects that own this library unit's source files
         --  for its spec/body.

         case Source_File.Unit_Part is
            when GPR.Unit_Body => Current_LU_Info.Body_Project := Project;
            when GPR.Unit_Spec => Current_LU_Info.Spec_Project := Project;

            --  Subunits cannot be units of interest, so Enumerate_Ada_Sources
            --  should not be able to call Find_Units_Wrapper with a subunit.
            --  Hence, the following should be unreachable.

            when GPR.Unit_Separate =>
               raise Program_Error with "unreachable code";
         end case;

         Find_Units
           (IC, CU_Name, Source_File, Add_Instrumented_Unit'Access);
      end Find_Units_Wrapper;

      ---------------------
      -- Instrument_Unit --
      ---------------------

      procedure Instrument_Unit
        (CU_Name   : Compilation_Unit_Name;
         Unit_Info : in out Instrumented_Unit_Info)
      is
         Prj_Info : Project_Info renames Unit_Info.Prj_Info.all;
         UIC      : Unit_Inst_Context;
      begin
         --  Instrument the source file and create a unit to contain its
         --  coverage buffers.

         Instrument_Source_File
           (CU_Name   => CU_Name,
            Unit_Info => Unit_Info,
            Prj_Info  => Prj_Info,
            IC        => IC,
            UIC       => UIC);
         Emit_Buffer_Unit (Prj_Info, UIC);
         Emit_Pure_Buffer_Unit (Prj_Info, UIC);

         --  Track which CU_Id maps to which instrumented unit

         Instrumented_Unit_CUs.Insert (CU_Name, UIC.CU);
      exception
         when E : Libadalang.Common.Property_Error =>
            Outputs.Fatal_Error
              ("internal error while instrumenting "
               & To_String (Unit_Info.Filename) & ": "
               & Ada.Exceptions.Exception_Information (E));
      end Instrument_Unit;

   --  Start of processing for Instrument_Units_Of_Interest

   begin
      --  First get the list of all units of interest

      Project.Enumerate_Ada_Sources (Find_Units_Wrapper'Access);

      --  If we need to instrument all Ada mains, also go through them now, so
      --  that we can prepare output directories for their projects later on.

      if Dump_Config.Trigger /= Manual then
         for Main of Project.Enumerate_Ada_Mains loop
            Register_Main_To_Instrument
              (IC, Main_To_Instrument_Vector, Main.File, Main.Project);
         end loop;
      end if;

      --  Know that we know all the sources we need to instrument, prepare
      --  output directories.

      Prepare_Output_Dirs (IC);

      --  For each library unit...

      for Cur in LU_Map.Iterate loop

         --  Instrument compilation units (only the ones this library unit
         --  owns).

         declare
            LU_Info              : constant Library_Unit_Info_Access :=
               Library_Unit_Maps.Element (Cur);
            All_Externally_Built : Boolean := True;
         begin
            for CU of LU_Info.CU_Names loop
               declare
                  Unit_Info : Instrumented_Unit_Info renames
                     IC.Instrumented_Units.Element (CU).all;
               begin
                  --  Do not instrument units from externally built projects

                  if not Unit_Info.Prj_Info.Externally_Built then
                     All_Externally_Built := False;
                     Instrument_Unit (CU, Unit_Info);
                  end if;
               end;
            end loop;

            --  Except for units entirely externally built (the spec, the body,
            --  and potential subunits all belong to externally built
            --  projects), emit a SID file to contain mappings between bits in
            --  coverage buffers and SCOs.

            if not All_Externally_Built then
               declare
                  Context : aliased Coverage.Context := Coverage.Get_Context;
                  Obj_SID : constant String :=
                     SID_Filename (Cur, In_Library_Dir => False);
                  Lib_SID : constant String :=
                     SID_Filename (Cur, In_Library_Dir => True);
                  Success : Boolean;
               begin
                  Checkpoints.Checkpoint_Save
                    (Obj_SID,
                     Context'Access,
                     Purpose => Checkpoints.Instrumentation);

                  --  If the object directory is different from the library
                  --  directory, copy the SID file to the library directory.
                  --  This allows "gnatcov coverage" to automatically pick it
                  --  up if the project is later made externally built.

                  if Obj_SID /= Lib_SID then

                     --  Unlike the object directory, which GNATCOLL.Project
                     --  creates automatically, the library directory may not
                     --  exist: create it if needed.

                     declare
                        use GNATCOLL.VFS;
                     begin
                        Create (Create (+Lib_SID).Dir_Name).Make_Dir;
                     exception
                        when Exc : VFS_Directory_Error =>
                           Outputs.Fatal_Error
                             (Ada.Exceptions.Exception_Message (Exc));
                     end;

                     GNAT.OS_Lib.Copy_File
                       (Name     => Obj_SID,
                        Pathname => Lib_SID,
                        Success  => Success,
                        Mode     => GNAT.OS_Lib.Overwrite);
                     if not Success then
                        Outputs.Fatal_Error
                          ("Error while copying " & Obj_SID
                           & " to the library directory: " & Lib_SID);
                     end if;
                  end if;
               end;

               if Switches.Verbose then
                  SC_Obligations.Dump_All_SCOs;
               end if;
            end if;
         end;

         Checkpoints.Checkpoint_Clear;
      end loop;

      Emit_Buffers_List_Unit (IC, Root_Project_Info.all);

      --  Instrument all Ada mains that are not unit of interest to add the
      --  dump of coverage buffers: Instrument_Unit already took care of mains
      --  that are units of interest.

      Auto_Dump_Buffers_In_Ada_Mains (IC, Main_To_Instrument_Vector);

      --  Remove sources in IC.Output_Dir that we did not generate this time.
      --  They are probably left overs from previous instrumentations for units
      --  that are no longer of interest. It is crucial not to make them part
      --  of next builds.

      Clean_Objdirs (IC);
   end Instrument_Units_Of_Interest;

end Instrument;
