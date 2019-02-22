------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2016, AdaCore                     --
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

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;

with Types; use Types;

with Coverage;
with SC_Obligations; use SC_Obligations;

package Checkpoints is

   subtype Checkpoint_Version is Interfaces.Unsigned_32 range 1 .. 2;
   Default_Checkpoint_Version : constant Checkpoint_Version := 1;
   --  For compatibility with previous Gnatcov versions, the checkpoint
   --  file format is versioned.

   --  1 -- initial version of checkpoint support
   --  2 -- support for source instrumentation (WIP, subject to change)

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

   --  Global state shared across phases of a checkpoint load or save

   type Checkpoint_State is abstract tagged limited record
      Stream  : Stream_Access;
      Version : Checkpoint_Version;
   end record;

   use type Interfaces.Unsigned_32;
   function Version_Less
     (CS : Checkpoint_State'Class; Than : Checkpoint_Version)
      return Boolean is (CS.Version < Than)
     with Inline;
   --  This is provided as a function to prevent the compiler from generating
   --  "can never be greater than" warnings.

   --  Global state shared across phases of a checkpoint save

   type Checkpoint_Save_State is new Checkpoint_State with null record;

   --  Global state shared across phases of a checkpoint load

   type Checkpoint_Load_State is new Checkpoint_State with record
      Filename : Unbounded_String;
      SFI_Map  : SFI_Map_Acc;
      CU_Map   : CU_Id_Map_Acc;
      Inst_Map : Inst_Id_Map_Acc;
      BDD_Map  : BDD_Node_Id_Map_Acc;
      SCO_Map  : SCO_Id_Map_Acc;
   end record;

   procedure Checkpoint_Save
     (Filename : String;
      Context  : access Coverage.Context;
      Version  : Checkpoint_Version := Default_Checkpoint_Version);
   procedure Checkpoint_Load (Filename : String);

   procedure Remap_SFI
     (CLS                : Checkpoint_Load_State'Class;
      CP_SFI             : in out Source_File_Index;
      Require_Valid_File : Boolean := True);
   --  Remap one source file index.
   --  If CP_SFI is No_Source_File then it's returned unchanged. If it is
   --  any other value, then it is remapped to the corresponding value in
   --  the current run. If Require_Valid_File is True, then a check is made
   --  that the remapped value is not No_Source_File.

end Checkpoints;
