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

with Ada.Streams.Stream_IO; use Ada.Streams, Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

with Interfaces;

with Coverage.Source;
with Files_Table;
with Outputs;           use Outputs;
with Traces_Files_List;

package body Checkpoints is

   Checkpoint_Magic   : constant String := "GNATcov checkpoint" & ASCII.NUL;
   Checkpoint_Version : constant := 0;

   type Checkpoint_Header is record
      Magic   : String (1 .. Checkpoint_Magic'Length) := Checkpoint_Magic;
      Version : Interfaces.Unsigned_32 := Checkpoint_Version;
   end record;

   procedure Free is
     new Ada.Unchecked_Deallocation (SFI_Map_Array, SFI_Map_Acc);
   procedure Free is
     new Ada.Unchecked_Deallocation (SCO_Id_Map_Array, SCO_Id_Map_Acc);
   procedure Free is
     new Ada.Unchecked_Deallocation (Inst_Id_Map_Array, Inst_Id_Map_Acc);

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save
     (Filename : String;
      Context  : access Coverage.Context)
   is
      SF : Ada.Streams.Stream_IO.File_Type;
      S  : Stream_Access;

   begin
      Create (SF, Out_File, Filename);
      S := Stream (SF);
      Checkpoint_Header'Write (S, (others => <>));
      Files_Table.Checkpoint_Save (S);
      SC_Obligations.Checkpoint_Save (S);
      Coverage.Source.Checkpoint_Save (S);
      Traces_Files_List.Checkpoint_Save (S, Context);
      Close (SF);
   end Checkpoint_Save;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load (Filename : String) is
      use type Interfaces.Unsigned_32;

      SF : Ada.Streams.Stream_IO.File_Type;
      S  : Stream_Access;
      CS : aliased Checkpoint_State;

      Expected_Header, CP_Header : Checkpoint_Header;

   begin
      CS.Filename := To_Unbounded_String (Filename);

      Open (SF, In_File, Filename);
      S := Stream (SF);

      Checkpoint_Header'Read (S, CP_Header);
      if CP_Header.Magic /= Expected_Header.Magic then
         Fatal_Error ("invalid checkpoint file " & Filename);

      elsif CP_Header.Version /= Expected_Header.Version then
         Fatal_Error
           ("incompatible checkpoint version "
            & "(expected" & Expected_Header.Version'Img
            & ", found"   & CP_Header.Version'Img & ")");

      else
         Files_Table.Checkpoint_Load (S, CS'Access);
         SC_Obligations.Checkpoint_Load (S, CS'Access);
         Coverage.Source.Checkpoint_Load (S, CS'Access);
         Traces_Files_List.Checkpoint_Load (S, CS'Access);
         Free (CS.SFI_Map);
         Free (CS.SCO_Map);
         Free (CS.Inst_Map);
      end if;

      Close (SF);
   end Checkpoint_Load;

end Checkpoints;
