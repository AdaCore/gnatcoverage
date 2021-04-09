------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2021, AdaCore                     --
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

--  This unit controls the generation and processing of coverage state
--  checkpoint files for incremental coverage.

with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;

with GNAT.Regexp;
with Types; use Types;

with Coverage;
with Files_Table; use Files_Table;
with SC_Obligations; use SC_Obligations;

package Checkpoints is

   subtype Checkpoint_Version is Interfaces.Unsigned_32 range 1 .. 6;
   Default_Checkpoint_Version : constant Checkpoint_Version := 6;
   --  For compatibility with previous Gnatcov versions, the checkpoint
   --  file format is versioned.
   --
   --  1 -- initial version of checkpoint support
   --  2 -- support for source instrumentation
   --  3 -- support for dumping names for units of interest
   --  4 -- gnatcov's own Pragma_Id type
   --  5 -- one CU_Info per source file and revamped fingerprint computation
   --       and support for listing ignored source files with --dump-units-to
   --  6 -- Add the kind of trace (Source, Binary or mixed) in the checkpoint

   type Checkpoint_Purpose is (Instrumentation, Consolidation);
   --  Purpose of checkpoint can be to provide:
   --    * Instrumentation: SCO information from instrumentation of source
   --      files (SID files);
   --    * Consolidation: a snapshot of an intermediate state in consolidated
   --      coverage (regular checkpoints).

   function Purpose_Name (Purpose : Checkpoint_Purpose) return String
   is (case Purpose is
       when Instrumentation => "Source Instrumentation Data (SID)",
       when Consolidation   => "checkpoint");
   --  Return a user-level name to designate a checkpoint created for the given
   --  Purpose.

   type Checkpoint_Relocations is private;

   procedure Allocate_SFI_Maps
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : Source_File_Index);
   procedure Allocate_CU_Id_Maps
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : CU_Id);
   procedure Allocate_Inst_Id_Map
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : Inst_Id);
   procedure Allocate_BDD_Node_Id_Map
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : BDD_Node_Id);
   procedure Allocate_SCO_Id_Map
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : SCO_Id);
   --  Allocate the various tables in the checkpoint relocations.

   procedure Ignore_SFI
     (Relocs : in out Checkpoint_Relocations;
      CP_SFI : Source_File_Index);
   procedure Ignore_CU_Id
     (Relocs   : in out Checkpoint_Relocations;
      CP_CU_Id : CU_Id);
   --  Mark the source file index or compilation unit as ignored in the
   --  checkpoint relocation. Tying to remap that source file index or
   --  compilation unit will result in an error.

   procedure Ignore_SCO
     (Relocs    : in out Checkpoint_Relocations;
      CP_SCO_Id : SCO_Id);
   --  Mark the SCO_Id as being removed in the checkpoint relocation.
   --  No information relative to this SCO should be loaded. In particular,
   --  trying to remap this SCO_Id will result in an error.

   function SFI_Ignored
     (Relocs : Checkpoint_Relocations;
      CP_SFI : Source_File_Index) return Boolean;
   --  Return whether CP_SFI is ignored in the checkpoint or not.
   --  For convenience, No_Source_File is considered as never ignored.

   function CU_Id_Ignored
     (Relocs   : Checkpoint_Relocations;
      CP_CU_Id : CU_Id) return Boolean;
   --  Return whether CP_CU_Id is ignored in the checkpoint or not.
   --  For convenience, No_CU_Id is never considered ignored.

   function SCO_Ignored
     (Relocs    : Checkpoint_Relocations;
      CP_SCO_Id : SCO_Id) return Boolean;
   --  Return whether CP_SCO_Id was marked as removed in this checkpoint.
   --  For convenience, No_SCO_Id is never considered ignored.

   procedure Set_SFI_Map
     (Relocs                 : in out Checkpoint_Relocations;
      Source_SFI, Target_SFI : Valid_Source_File_Index) with
     Pre => not SFI_Ignored (Relocs, Source_SFI);
   --  Associate Source_SFI to Target_SFI in the relocations map.
   --  Source_SFI must not have been previously marked as ignored with
   --  Ignore_SFI.

   procedure Set_CU_Id_Map
     (Relocs                     : in out Checkpoint_Relocations;
      Source_CU_Id, Target_CU_Id : Valid_CU_Id) with
     Pre => not CU_Id_Ignored (Relocs, Source_CU_Id);
   --  Associate Source_CU_ID to Target_CU_ID in the relocations map.
   --  Source_CU_Id must not have been previously marked as ignored with
   --  Ignore_CU_Id.

   procedure Set_Inst_Id_Map
     (Relocs                         : in out Checkpoint_Relocations;
      Source_Inst_Id, Target_Inst_Id : Valid_Inst_Id);
   --  Associate Source_Inst_Id to Target_Inst_Id in the relocations map

   procedure Set_BDD_Node_Id_Map
     (Relocs                                 : in out Checkpoint_Relocations;
      Source_BDD_Node_Id, Target_BDD_Node_Id : Valid_BDD_Node_Id);
   --  Associate Source_BDD_Node_Id to Target_BDD_Node_ID in the relocation map

   procedure Set_SCO_Id_Map
     (Relocs                       : in out Checkpoint_Relocations;
      Source_SCO_Id, Target_SCO_Id : Valid_SCO_Id) with
     Pre => not SCO_Ignored (Relocs, Source_SCO_Id);
   --  Associate Source_SCO_ID to Target_SCO_ID in the relocations map

   procedure Free (Relocs : in out Checkpoint_Relocations);
   --  Relase allocated maps in Relocs

   function Remap_SFI
     (Relocs : Checkpoint_Relocations;
      CP_SFI : Source_File_Index) return Source_File_Index with
     Pre => not SFI_Ignored (Relocs, CP_SFI);
  --  Remap one source file index.
  --
  --  If CP_SFI is No_Source_File then it's returned unchanged. If it is
  --  any other value, then it is remapped to the corresponding value in
  --  the current run.

   function Remap_CU_Id
     (Relocs   : Checkpoint_Relocations;
      CP_CU_Id : CU_Id) return CU_Id with
     Pre => not CU_Id_Ignored (Relocs, CP_CU_Id);
   --  Remap one CU_Id.
   --
   --  If CP_CU_Id is No_CU_Id then it's returned unchanged. If it is
   --  any other value, then it is remapped to the corresponding value in
   --  the current run.

   function Remap_Inst_Id
     (Relocs     : Checkpoint_Relocations;
      CP_Inst_Id : Inst_Id) return Inst_Id;
   --  Remap one Inst_Id.
   --
   --  If CP_Inst_Id is No_Inst_Id then it's returned unchanged. If it is
   --  any other value, then it is remapped to the corresponding value in
   --  the current run.

   function Remap_BDD_Node_Id
     (Relocs         : Checkpoint_Relocations;
      CP_BDD_Node_Id : BDD_Node_Id) return BDD_Node_Id;
   --  Remap one BDD_Node_Id.
   --
   --  If CP_BDD_Node_Id is No_BDD_Node_Id then it's returned unchanged. If it
   --  is any other value, then it is remapped to the corresponding value in
   --  the current run.

   function Remap_SCO_Id
     (Relocs    : Checkpoint_Relocations;
      CP_SCO_Id : SCO_Id) return SCO_Id with
     Pre => not SCO_Ignored (Relocs, CP_SCO_Id);
   --  Remap one SCO_Id.
   --
   --  If CP_SCO_Id is No_SCO_Id then it's returned unchanged. If it
   --  is any other value, then it is remapped to the corresponding value in
   --  the current run.

   procedure Remap_SFI
     (Relocs             : Checkpoint_Relocations;
      CP_SFI             : in out Source_File_Index) with
     Pre => not SFI_Ignored (Relocs, CP_SFI);
   --  Remap one source file index.
   --
   --  If CP_SFI is No_Source_File then it's returned unchanged. If it is
   --  any other value, then it is remapped to the corresponding value in
   --  the current run.

   --  A stream associated with global state shared across phases of a
   --  checkpoint load or save.

   type Stateful_Stream (Stream : access Root_Stream_Type'Class) is abstract
     new Root_Stream_Type with
      record
         Filename : Unbounded_String;
         --  Name of the checkpoint being written/read

         Version : Checkpoint_Version;
         --  Format version for the checkpoint being written/read

         Purpose : Checkpoint_Purpose;
         --  Purpose for the checkpoint being written/read
      end record;

   procedure Read
     (Stream : in out Stateful_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   procedure Write
     (Stream : in out Stateful_Stream;
      Item   : Stream_Element_Array);
   --  Read/write from/to underlying stream

   use type Interfaces.Unsigned_32;
   function Version_Less
     (CS : access Root_Stream_Type'Class; Than : Checkpoint_Version)
      return Boolean is (Stateful_Stream (CS.all).Version < Than)
     with Inline;
   --  This is provided as a function to prevent the compiler from generating
   --  "can never be greater than" warnings.

   function Purpose_Of
     (CS : access Root_Stream_Type'Class) return Checkpoint_Purpose
   is (Stateful_Stream (CS.all).Purpose)
     with Inline;
   --  Shortcut to get the purpose of a stream that is known to be an instance
   --  of Stateful_Stream.

   --  Global state shared across phases of a checkpoint save

   type Checkpoint_Save_State is new Stateful_Stream with null record;

   --  Global state shared across phases of a checkpoint load

   type Checkpoint_Load_State is new Stateful_Stream with record
      Relocations : Checkpoint_Relocations;
   end record;

   procedure Checkpoint_Save
     (Filename : String;
      Context  : access Coverage.Context;
      Purpose  : Checkpoint_Purpose);
   --  Dump internal data structures to a checkpoint file

   procedure Checkpoint_Clear;
   --  Clear the internal data structures used to create checkpoints

   procedure SID_Load
     (Filename             : String;
      Ignored_Source_Files : access GNAT.Regexp.Regexp);
   --  Load an SID file into internal data structures, ignoring files
   --  that match Ignored_Source_Files.

   procedure Checkpoint_Load (Filename : String);
   --  Load a checkpoint file into internal data structures

private

   type SFI_Map_Array is
     array (Source_File_Index range <>) of Source_File_Index;
   type SFI_Map_Acc is access all SFI_Map_Array;

   type CU_Id_Map_Array is array (CU_Id range <>) of CU_Id;
   type CU_Id_Map_Acc is access all CU_Id_Map_Array;

   type Inst_Id_Map_Array is array (Inst_Id range <>) of Inst_Id;
   type Inst_Id_Map_Acc is access all Inst_Id_Map_Array;

   type BDD_Node_Id_Map_Array is array (BDD_Node_Id range <>) of BDD_Node_Id;
   type BDD_Node_Id_Map_Acc is access all BDD_Node_Id_Map_Array;

   type SCO_Id_Map_Array is array (SCO_Id range <>) of SCO_Id;
   type SCO_Id_Map_Acc is access all SCO_Id_Map_Array;

   type SCO_Ignored_Map_Array is array (SCO_Id range <>) of Boolean;
   type SCO_Ignored_Map_Access is access all SCO_Ignored_Map_Array;

   type SFI_Ignored_Map_Array is array (Source_File_Index range <>) of Boolean;
   type SFI_Ignored_Map_Access is access all SFI_Ignored_Map_Array;

   type CU_Id_Ignored_Map_Array is array (CU_Id range <>) of Boolean;
   type CU_Id_Ignored_Access is access all CU_Id_Ignored_Map_Array;

   type Checkpoint_Relocations is  record
      SFI_Map  : SFI_Map_Acc;
      CU_Map   : CU_Id_Map_Acc;
      Inst_Map : Inst_Id_Map_Acc;
      BDD_Map  : BDD_Node_Id_Map_Acc;
      SCO_Map  : SCO_Id_Map_Acc;
      --  Maps to replace checkpoint identifiers with local table identifiers
      --  after merging the checkpoint in local tables.

      Ignored_SCOs : SCO_Ignored_Map_Access;
      --  Map of SCOs that were ignored and thus are not remapped when loading
      --  the checkpoint.

      Ignored_SFIs : SFI_Ignored_Map_Access;
      Ignored_CUs  : CU_Id_Ignored_Access;
      --  Represents the ignored Source files and compilation units that
      --  we should not load from the SID file.
      --  Unused when loading data from a consolidation checkpoint.

   end record;

end Checkpoints;
