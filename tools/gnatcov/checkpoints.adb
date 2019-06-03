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

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

with Coverage.Source;
with Files_Table;
with Instrument.Common;
with Outputs;           use Outputs;
with Traces_Files_List;

package body Checkpoints is

   Checkpoint_Magic : constant String := "GNATcov checkpoint" & ASCII.NUL;

   type Checkpoint_Header is record
      Magic   : String (1 .. Checkpoint_Magic'Length) := Checkpoint_Magic;
      Version : Interfaces.Unsigned_32;
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
      Context  : access Coverage.Context;
      Purpose  : Checkpoint_Purpose;
      Version  : Checkpoint_Version := Default_Checkpoint_Version)
   is
      SF  : Ada.Streams.Stream_IO.File_Type;
   begin
      if Purpose = Instrumentation and then Version < 2 then
         raise Program_Error
           with "instrumentation checkpoints must be at least version 2";
      end if;

      Create (SF, Out_File, Filename);

      declare
         use Coverage;

         CSS              : aliased Checkpoint_Save_State :=
           (Root_Stream_Type with Stream => Stream (SF), Version => Version);
         Supported_Levels : Levels_Type := Current_Levels;
      begin
         Checkpoint_Header'Write
           (CSS.Stream, (Version => Version, others => <>));

         --  Instrumentation is the same for all MC/DC variants, so a
         --  checkpoint generated for any of them supports all of them.

         if Purpose = Instrumentation and then MCDC_Coverage_Enabled then
            Supported_Levels (MCDC_Coverage_Level'Range) := (others => True);
         end if;
         Coverage.Levels_Type'Write (CSS.Stream, Supported_Levels);

         Files_Table.Checkpoint_Save (CSS'Access);
         SC_Obligations.Checkpoint_Save (CSS'Access);
         if not Version_Less (CSS'Access, Than => 2) then
            Instrument.Common.Checkpoint_Save (CSS'Access);
         end if;
         Coverage.Source.Checkpoint_Save (CSS'Access);
         Traces_Files_List.Checkpoint_Save (CSS'Access, Context);
      end;

      Close (SF);
   end Checkpoint_Save;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load (Filename : String) is
      SF        : Ada.Streams.Stream_IO.File_Type;
      CP_Header : Checkpoint_Header;
      Levels    : Coverage.Levels_Type;

   begin
      Open (SF, In_File, Filename);

      declare
         CLS : aliased Checkpoint_Load_State :=
           (Root_Stream_Type with Stream => Stream (SF), others => <>);
      begin
         CLS.Filename := To_Unbounded_String (Filename);

         Checkpoint_Header'Read (CLS.Stream, CP_Header);
         if CP_Header.Magic /= Checkpoint_Magic then
            Fatal_Error ("invalid checkpoint file " & Filename);

         elsif CP_Header.Version not in Checkpoint_Version then
            Fatal_Error
              ("invalid checkpoint version" & CP_Header.Version'Img);

         else
            CLS.Version := CP_Header.Version;
            Coverage.Levels_Type'Read (CLS.Stream, Levels);
            declare
               Error_Msg : constant String :=
                 Coverage.Is_Load_Allowed (Filename, Levels);
            begin
               if Error_Msg'Length > 0 then
                  Fatal_Error (Error_Msg);
               end if;
            end;

            Files_Table.Checkpoint_Load (CLS'Access);
            SC_Obligations.Checkpoint_Load (CLS'Access);
            if not Version_Less (CLS'Access, Than => 2) then
               Instrument.Common.Checkpoint_Load (CLS'Access);
            end if;
            Coverage.Source.Checkpoint_Load (CLS'Access);
            Traces_Files_List.Checkpoint_Load (CLS'Access);

            Free (CLS.SFI_Map);
            Free (CLS.SCO_Map);
            Free (CLS.Inst_Map);
         end if;
      end;

      Close (SF);
   end Checkpoint_Load;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Stateful_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
   begin
      Stream.Stream.Read (Item, Last);
   end Read;

   ---------------
   -- Remap_SFI --
   ---------------

   procedure Remap_SFI
     (CLS                : Checkpoint_Load_State'Class;
      CP_SFI             : in out Source_File_Index;
      Require_Valid_File : Boolean := True)
   is
   begin
      if CP_SFI /= No_Source_File then
         CP_SFI := CLS.SFI_Map (CP_SFI);
         pragma Assert
           (not Require_Valid_File or else CP_SFI /= No_Source_File);
      end if;
   end Remap_SFI;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Stateful_Stream;
      Item   : Stream_Element_Array)
   is
   begin
      Stream.Stream.Write (Item);
   end Write;

end Checkpoints;
