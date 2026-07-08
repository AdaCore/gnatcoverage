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
with GNATCOLL.VFS;  use GNATCOLL.VFS;

with Checkpoints;       use Checkpoints;
with Coverage;
with Files_Handling;    use Files_Handling;
with Instrument.Ada_Unit;
with Instrument.Common; use Instrument.Common;
with JSON;
with Outputs;
with Traces_Files;      use Traces_Files;

--  Implementation of the gnatcov instrument-source command.
--
--  The process for source instrumentation is 3-fold, and vary according to the
--  compilation unit language.
--
--  For C/C++ source instrumentation, start by processing the source for
--  manual annotation replacements. It has to be done first, as external
--  annotations, which may contain dump / reset annotations, operate on
--  unpreprocessed sources.
--
--  Then, if Is_UOI is set to True, instrument the source as a unit of
--  interest. According to whether the user uses the manual dump trigger,
--  it may operate on a file which has already been processed for C/C++
--  sources. In any case, we always pass the original source version to the
--  Instrument_Unit subprogram to make sure this is the one referenced in the
--  generated SID file (whose name is SID_Name).
--
--  Then, if Is_Main is set to True, instrument the file as a main. This file
--  may already have been instrumented, in which case pass the instrumented
--  version.
--
--  Finally, process Ada sources for manual annotation replacements.

procedure Instrument.Source
  (Unit_Name         : String;
   SID_Name          : String;
   Instrumenter      : in out Language_Instrumenter'Class;
   Files_Of_Interest : File_Sets.Set;
   Prj_Actual        : Prj_Desc;
   Is_UOI            : Boolean;
   Is_Main           : Boolean;
   Dump_Config       : Any_Dump_Config)
is
   procedure Copy_SID_To_Lib_Dir;
   --  Copy the SID file to Prj_Actual.Lib_Dir, if the former is set

   -------------------------
   -- Copy_SID_To_Lib_Dir --
   -------------------------

   procedure Copy_SID_To_Lib_Dir is
      Obj_SID : constant Virtual_File := Create (+SID_Name);
      Lib_SID : constant Virtual_File :=
        Join (Prj_Actual.Lib_Dir, Obj_SID.Base_Name);
   begin
      if Obj_SID /= Lib_SID then

         --  Unlike the object directory, which GPR2 creates automatically, the
         --  library directory may not exist: create it if needed.

         begin
            Prj_Actual.Lib_Dir.Make_Dir;
         exception
            when Exc : VFS_Directory_Error =>
               Outputs.Fatal_Error (Exception_Message (Exc));
         end;

         Files_Handling.Copy_File
           (Obj_SID.Display_Full_Name, Lib_SID.Display_Full_Name);
      end if;
   end Copy_SID_To_Lib_Dir;

   Context : aliased Coverage.Context := Coverage.Get_Context;

   Prj : Prj_Desc := Prj_Actual;
   --  Modifiable copy of the project description parameter

   Instrumented_Files : File_Sets.Set;
   --  Set of instrumented files

   Dump_Indications, Reset_Indications, Instr_Artifacts : JSON_Array :=
     Empty_Array;
   --  Unit instrumentation information

   Unit_Instr_Info_JSON : constant JSON_Value := Create_Object;
   Unit_Instr_Info_File : constant Virtual_File :=
     Instrument.Common.Files_Instrumentation_Info_File (Prj, Unit_Name);
   --  File holding instrumentation information that should be reported back
   --  to the parent instrumentation process.

   Main_Part_File : constant GNATCOLL.VFS.Virtual_File :=
     Instrumenter.Get_Main_File (Unit_Name);
   --  Filename for the main part of the compilation unit

   function Source_To_Instrument (File : Virtual_File) return Virtual_File;
   --  If File has already been instrumented, return its instrumented version
   --  otherwise return File unchanged.

   procedure Replace_Manual_Indications (File : Virtual_File);
   --  Replace manual indications in File and write to Files_Info_JSON whether
   --  there were manual indications (dump / reset) in the file.

   procedure Mark_As_Instrumented (File : Virtual_File);
   --  Add the given file to Instrumented_Files

   --------------------------
   -- Source_To_Instrument --
   --------------------------

   function Source_To_Instrument (File : Virtual_File) return Virtual_File is
   begin
      if Instrumented_Files.Contains (File) then
         return Instrumentation_File (Prj, File);
      end if;
      return File;
   end Source_To_Instrument;

   --------------------
   -- Process_Source --
   --------------------

   procedure Replace_Manual_Indications (File : Virtual_File) is
      Had_Dump_Indication, Had_Reset_Indication : Boolean;
   begin
      if Dump_Config.Manual_Indication_Files.Is_Empty
        or else Dump_Config.Manual_Indication_Files.Contains (File)
      then
         Instrumenter.Replace_Manual_Indications
           (Prj                  => Prj,
            Source               => Source_To_Instrument (File),
            Has_Dump_Indication  => Had_Dump_Indication,
            Has_Reset_Indication => Had_Reset_Indication);

         --  C/C++ instrumentation always preprocesses the file, thus mark it
         --  as instrumented in this case.

         if Instrumenter.Language in C_Family_Language then
            Mark_As_Instrumented (File);

         --  Ada instrumentation generates an instrumented file iff it contains
         --  a dump / reset indication.

         elsif Had_Dump_Indication or else Had_Reset_Indication then
            Mark_As_Instrumented (File);
         end if;

         --  Write whether it has dump indication / reset indications to the
         --  files instrumentation information file.

         if Had_Dump_Indication then
            Append (Dump_Indications, Create (File.Display_Full_Name));
         end if;

         if Had_Reset_Indication then
            Append (Reset_Indications, Create (File.Display_Full_Name));
         end if;
      end if;
   end Replace_Manual_Indications;

   --------------------------
   -- Mark_As_Instrumented --
   --------------------------

   procedure Mark_As_Instrumented (File : Virtual_File) is
   begin
      Instrumented_Files.Include (File);
   end Mark_As_Instrumented;

begin
   --  Preemptively clear checkpoints structures to make sure not to write
   --  files table entries not related to this source instrumentation.

   Checkpoints.Checkpoint_Clear;

   --  Preemptively clear instrumentation artifacts

   Prj.Instr_Artifacts.Clear;

   --  Add the file containing the list of instrumented files. Note that this
   --  is exclusively used in the context of incrementality, thus the file
   --  is generated in the GPR2.Build.Actions.Instrument_Source unit but it
   --  is more convenient to add it to the instrumentation artifacts here.

   Prj.Instr_Artifacts.Include (Instrumented_Files_File (Prj, Main_Part_File));

   if Instrumenter.Language in C_Family_Language then
      if Dump_Config.Manual_Trigger then
         Replace_Manual_Indications (Create (+Unit_Name));
      end if;
   end if;

   if Is_UOI then

      --  Even though instrumentation does not create any traces, the structure
      --  of a SID file is basically a checkpoint, so it has a Trace_Kind field
      --  in its header. Instead of leaving it to Unknown (default value) mark
      --  it as Source_Trace_File so that when the .sid file is loaded, it will
      --  set gnatcov in "source trace mode" and it will be rejected if binary
      --  traces have already been loaded.

      Update_Current_Trace_Kind (Source_Trace_File);

      --  Instrument all of the source files implementing the compilation unit.
      --  For Ada, this means instrumenting the body / spec / separates, and
      --  for C/C++, this means instrumenting the .c file and the included
      --  headers.

      --  C/C++ sources may have been processed already by the manual
      --  instrumentation process.

      Instrumenter.Instrument_Unit
        (Unit_Name,
         Prj,
         Files_Of_Interest,
         Instrumented_Files => Instrumented_Files);

      --  Mark the unit as instrumented

      Instrumenter.For_All_Part (Unit_Name, Mark_As_Instrumented'Access);

      --  Save the SCOs for the unit in the SID file

      Checkpoints.Checkpoint_Save
        (SID_Name, Context'Access, Purpose => Checkpoints.Instrumentation);

      if SC_Obligations.SCOs_Trace.Is_Active then
         SC_Obligations.Dump_All_SCOs;
      end if;

      Checkpoints.Checkpoint_Clear;
   end if;

   if Is_Main and then Dump_Config.Auto_Trigger /= None then
      Instrumenter.Auto_Dump_Buffers_In_Main
        (Filename    => Source_To_Instrument (Main_Part_File),
         Dump_Config => Dump_Config,
         Prj         => Prj);
   end if;

   --  For Ada, replace manual indications at the end of the source
   --  instrumentation process.

   if Dump_Config.Manual_Trigger and then Instrumenter.Language = Ada_Language
   then
      Instrumenter.For_All_Part (Unit_Name, Replace_Manual_Indications'Access);

      --  Also make sure to have the buffers list unit in the main closure when
      --  using the manual dump trigger

      if Is_Main then
         declare
            Main_Filename : constant GNATCOLL.VFS.Virtual_File :=
              Instrumenter.Get_Main_File (Unit_Name);
         begin
            Instrument.Ada_Unit.Insert_With_Dump_Helper
              (Self   =>
                 Instrument.Ada_Unit.Ada_Instrumenter_Type (Instrumenter),
               Source => Source_To_Instrument (Main_Filename),
               Prj    => Prj);
         end;
      end if;
   end if;

   --  Write unit instrumentation information

   Unit_Instr_Info_JSON.Set_Field ("dump_indications", Dump_Indications);
   Unit_Instr_Info_JSON.Set_Field ("reset_indications", Reset_Indications);
   for Art of Prj.Instr_Artifacts loop
      Append (Instr_Artifacts, Create (Art.Display_Base_Name));
   end loop;
   Unit_Instr_Info_JSON.Set_Field ("artifacts", Instr_Artifacts);
   JSON.Write (Unit_Instr_Info_File.Display_Full_Name, Unit_Instr_Info_JSON);

   --  If needed, copy the SID file to the library directory

   if Prj_Actual.Lib_Dir /= No_File then
      Copy_SID_To_Lib_Dir;
   end if;
end Instrument.Source;
