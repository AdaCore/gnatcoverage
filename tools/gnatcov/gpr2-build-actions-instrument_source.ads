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

--  Unit implementing the source instrumentation action. This action derives
--  from GPR2.Build.Actions.Object.

with Ada.Containers.Hashed_Sets;

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Command_Line;
with GPR2.Project.View;

with Instrument;
with Instrument.Common; use Instrument.Common;
with Switches;          use Switches;
with Strings;           use Strings;

package GPR2.Build.Actions.Instrument_Source is

   --  See the documentation in GPR2.Build.Actions for overriding subprograms

   type Instrument_Id (Name_Len : Natural) is new GPR2.Build.Actions.Action_Id
   with record
      Lang     : Language_Id;
      Ctxt     : GPR2.Project.View.Object;
      Src_Name : Simple_Name (1 .. Name_Len);
   end record;

   overriding
   function View (Self : Instrument_Id) return Project.View.Object
   is (Self.Ctxt);

   overriding
   function Action_Class (Self : Instrument_Id) return Value_Type
   is ("Instrument");

   overriding
   function Language (Self : Instrument_Id) return Language_Id
   is (Self.Lang);

   overriding
   function Action_Parameter (Self : Instrument_Id) return Value_Type
   is (Value_Type (Self.Src_Name));

   function Hash
     (Art : GPR2.Build.Artifacts.Files.Object) return Ada.Containers.Hash_Type
   is (Art.Hash);

   package Artifact_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => GPR2.Build.Artifacts.Files.Object,
        Hash                => Hash,
        Equivalent_Elements => GPR2.Build.Artifacts.Files."=",
        "="                 => GPR2.Build.Artifacts.Files."=");

   type Object is tagged record
      Deps_Cache : GPR2.Containers.Filename_Set;
      --  Cache for dependencies, to avoid recomputing them several times

      IC : Inst_Context_Acc;
      --  Instrumentation context

      Instrumenter : Instrument.Common.Language_Instrumenter_Acc;
      --  Instrumenter to use when instrumenting the source

      Prj_Info : Project_Info_Access;
      --  Project info for the unit to instrument

      Dump_Config : Any_Dump_Config;
      --  Dump config to pass to main instrumentation if this is also a main

      LU_Info : Library_Unit_Info;
      --  Instrumentation information for the compilation unit

      Artifacts : Artifact_Sets.Set;
      --  Artifacts for this source instrumentation action. This varies
      --  according to whether this is a unit of interest / a main.

      Cmd_Line : GPR2.Build.Command_Line.Object;
      --  Command line used to run the action when spawning a process. Also
      --  used also in the signature both for process and in-thread execution.

   end record;

   type Object_Acc is access Object'Class;

   procedure Initialize
     (Self         : in out Object;
      LU_Info      : Library_Unit_Info;
      IC           : Inst_Context_Acc;
      Instrumenter : Instrument.Common.Language_Instrumenter_Acc;
      Prj_Info     : Project_Info_Access;
      Dump_Config  : Any_Dump_Config);
   --  Initialize a source instrumentation action

   procedure Common_Compute_Signature
     (Self            : in out Object;
      Signature       : in out GPR2.Build.Signature.Object;
      Check_Checksums : Boolean);
   --  Code shared between Compute_Signature implementations

   procedure Compute_Signature
     (Self            : in out Object;
      Signature       : in out GPR2.Build.Signature.Object;
      Check_Checksums : Boolean);
   --  Compute inputs and outputs for the given unit instrumentation action.
   --
   --  The inputs are the source file, its dependencies, and the files
   --  instrumented in the context of this unit instrumentation action.
   --
   --  The outputs are the instrumentation artifacts resulting from the
   --  source instrumentation command.

   function Compute_Command
     (Self : in out Object) return GPR2.Build.Command_Line.Object;
   --  Compute the command for the given source instrumentation action

   function Post_Execution
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : US.Unbounded_String := US.Null_Unbounded_String;
      Stderr : US.Unbounded_String := US.Null_Unbounded_String) return Boolean;
   --  Copy the SID file generated by the source instrumentation action to the
   --  library directory if needed.

   function UID (Self : Object) return Actions.Action_Id'Class;

   function Dependencies (Self : in out Object) return Containers.Filename_Set;
   --  Return the file dependencies for the instrument source action

   procedure Write_Instrumented_Files_List (Self : in out Object);
   --  Write the list of files instrumented in the context of this source
   --  instrumentation action. This shall consist of the main source file
   --  (Self.Src), but also of any header / unit part that is also of
   --  interest.
   --
   --  Take the following scenario: consider a project containing a pkg.c file,
   --  including a pkg.h header. The user instruments it and excludes pkg.h
   --  from coverage analysis. Then, it runs the instrumentation a second time,
   --  now including pkg.h: gnatcov thus needs to reinstrument pkg.c. In the
   --  first instrumentation, the instrumented dep file would contain only
   --  pkg.c, and in the second, it shall contain both pkg.c and pkg.h.

   function Unit_Name (Self : Object) return String;
   --  Return the unit to instrument as a source.
   --
   --  For Ada, this would be the unit name.
   --
   --  For C/C++, it is the full name to the original source when it has not
   --  been processed for manual / external annotations replacement priorly.
   --  Otherwise, it is the instrumented version of the source.

   function SID_Path (Self : Object) return GPR2.Path_Name.Object
   is (GPR2.Path_Name.Create_File
         (GPR2.Filename_Type
            (SID_Filename
               (Self.LU_Info.Main_Part_Src, In_Library_Dir => False))));
   --  Return the path for the SID file for this unit instrumentation action

end GPR2.Build.Actions.Instrument_Source;
