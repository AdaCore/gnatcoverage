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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Types; use Types;

with Coverage;
with SC_Obligations; use SC_Obligations;

package Checkpoints is

   type SFI_Map_Array is
     array (Source_File_Index range <>) of Source_File_Index;
   type SFI_Map_Acc is access all SFI_Map_Array;

   type Inst_Id_Map_Array is array (Inst_Id range <>) of Inst_Id;
   type Inst_Id_Map_Acc is access all Inst_Id_Map_Array;

   type BDD_Node_Id_Map_Array is array (BDD_Node_Id range <>) of BDD_Node_Id;
   type BDD_Node_Id_Map_Acc is access all BDD_Node_Id_Map_Array;

   type SCO_Id_Map_Array is array (SCO_Id range <>) of SCO_Id;
   type SCO_Id_Map_Acc is access all SCO_Id_Map_Array;

   --  Object use to convey state between the various phases of a checkpoint
   --  load (to support remapping checkpointed identifiers to current ones).

   type Checkpoint_State is limited record
      Filename : Unbounded_String;
      SFI_Map  : SFI_Map_Acc;
      Inst_Map : Inst_Id_Map_Acc;
      BDD_Map  : BDD_Node_Id_Map_Acc;
      SCO_Map  : SCO_Id_Map_Acc;
   end record;

   procedure Checkpoint_Save
     (Filename : String;
      Context  : access Coverage.Context);
   procedure Checkpoint_Load (Filename : String);

end Checkpoints;
