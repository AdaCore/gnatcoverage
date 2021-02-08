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

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

with Coverage.Source;
with Instrument.Common;
with Outputs;           use Outputs;
with Traces_Files_Registry;

package body Checkpoints is

   Checkpoint_Magic : constant String := "GNATcov checkpoint" & ASCII.NUL;

   type Checkpoint_Header is record
      Magic   : String (1 .. Checkpoint_Magic'Length) := Checkpoint_Magic;
      Version : Interfaces.Unsigned_32;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (SFI_Map_Array, SFI_Map_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
     (CU_Id_Map_Array, CU_Id_Map_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
     (Inst_Id_Map_Array, Inst_Id_Map_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
     (BDD_Node_Id_Map_Array, BDD_Node_Id_Map_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
     (SCO_Id_Map_Array, SCO_Id_Map_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
     (SFI_Ignored_Map_Array, SFI_Ignored_Map_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (CU_Id_Ignored_Map_Array, CU_Id_Ignored_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (SCO_Ignored_Map_Array, SCO_Ignored_Map_Access);

   -----------------------
   -- Allocate_SFI_Maps --
   -----------------------

   procedure Allocate_SFI_Maps
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : Source_File_Index)
   is
   begin
      pragma Assert
        (Relocs.SFI_Map = null and then Relocs.Ignored_SFIs = null);

      Relocs.SFI_Map :=
        new SFI_Map_Array'(First .. Last => No_Source_File);
      Relocs.Ignored_SFIs :=
        new SFI_Ignored_Map_Array'(First .. Last => False);
   end Allocate_SFI_Maps;

   -------------------------
   -- Allocate_CU_Id_Maps --
   -------------------------

   procedure Allocate_CU_Id_Maps
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : CU_Id)
   is
   begin
      pragma Assert (Relocs.CU_Map = null and then Relocs.Ignored_CUs = null);

      Relocs.CU_Map :=
        new CU_Id_Map_Array'(First .. Last => No_CU_Id);
      Relocs.Ignored_CUs :=
        new CU_Id_Ignored_Map_Array'(First  .. Last => False);
   end Allocate_CU_Id_Maps;

   ---------------------------
   -- Allocate_Inst_Id_Maps --
   ---------------------------

   procedure Allocate_Inst_Id_Map
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : Inst_Id)
   is
   begin
      pragma Assert (Relocs.Inst_Map = null);
      Relocs.Inst_Map :=
        new Inst_Id_Map_Array'(First .. Last => No_Inst_Id);
   end Allocate_Inst_Id_Map;

   -------------------------------
   -- Allocate_BDD_Node_Id_Maps --
   -------------------------------

   procedure Allocate_BDD_Node_Id_Map
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : BDD_Node_Id)
   is
   begin
      pragma Assert (Relocs.BDD_Map = null);
      Relocs.BDD_Map :=
        new BDD_Node_Id_Map_Array'(First .. Last => No_BDD_Node_Id);
   end Allocate_BDD_Node_Id_Map;

   --------------------------
   -- Allocate_SCO_Id_Maps --
   --------------------------

   procedure Allocate_SCO_Id_Map
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : SCO_Id)
   is
   begin
      pragma Assert
        (Relocs.SCO_Map = null and then Relocs.Ignored_SCOs = null);
      Relocs.SCO_Map :=
        new SCO_Id_Map_Array'(First .. Last => No_SCO_Id);
      Relocs.Ignored_SCOs :=
        new SCO_Ignored_Map_Array'(First .. Last => False);
   end Allocate_SCO_Id_Map;

   ----------------
   -- Ignore_SFI --
   ----------------

   procedure Ignore_SFI
     (Relocs : in out Checkpoint_Relocations;
      CP_SFI : Source_File_Index)
   is
   begin
      pragma Assert (Relocs.SFI_Map (CP_SFI) = No_Source_File);
      Relocs.Ignored_SFIs (CP_SFI) := True;
   end Ignore_SFI;

   ------------------
   -- Ignore_CU_Id --
   ------------------

   procedure Ignore_CU_Id
     (Relocs   : in out Checkpoint_Relocations;
      CP_CU_Id : CU_Id)
   is
   begin
      pragma Assert (Relocs.CU_Map (CP_CU_Id) = No_CU_Id);
      Relocs.Ignored_CUs (CP_CU_Id) := True;
   end Ignore_CU_Id;

   ----------------
   -- Ignore_SCO --
   ----------------

   procedure Ignore_SCO
     (Relocs    : in out Checkpoint_Relocations;
      CP_SCO_Id : SCO_Id)
   is
   begin
      pragma Assert (Relocs.SCO_Map (CP_SCO_Id) = No_SCO_Id);
      Relocs.Ignored_SCOs (CP_SCO_Id) := True;
   end Ignore_SCO;

   -----------------
   -- SFI_Ignored --
   -----------------

   function SFI_Ignored
     (Relocs : Checkpoint_Relocations;
      CP_SFI : Source_File_Index) return Boolean
   is
   begin
      return CP_SFI /= No_Source_File and then Relocs.Ignored_SFIs (CP_SFI);
   end SFI_Ignored;

   -------------------
   -- CU_Id_Ignored --
   -------------------

   function CU_Id_Ignored
     (Relocs   : Checkpoint_Relocations;
      CP_CU_Id : CU_Id) return Boolean
   is
   begin
      return CP_CU_Id /= No_CU_Id and then Relocs.Ignored_CUs (CP_CU_Id);
   end CU_Id_Ignored;

   -----------------
   -- SCO_Ignored --
   -----------------

   function SCO_Ignored
     (Relocs    : Checkpoint_Relocations;
      CP_SCO_Id : SCO_Id) return Boolean
   is
   begin
      return CP_SCO_Id /= No_SCO_Id and then Relocs.Ignored_SCOs (CP_SCO_Id);
   end SCO_Ignored;

   -----------------
   -- Set_SFI_Map --
   -----------------

   procedure Set_SFI_Map
     (Relocs                 : in out Checkpoint_Relocations;
      Source_SFI, Target_SFI : Valid_Source_File_Index)
   is
   begin
      Relocs.SFI_Map (Source_SFI) := Target_SFI;
   end Set_SFI_Map;

   -------------------
   -- Set_CU_Id_Map --
   -------------------

   procedure Set_CU_Id_Map
     (Relocs                     : in out Checkpoint_Relocations;
      Source_CU_Id, Target_CU_Id : Valid_CU_Id)
   is
   begin
      Relocs.CU_Map (Source_CU_Id) := Target_CU_Id;
   end Set_CU_Id_Map;

   ---------------------
   -- Set_Inst_Id_Map --
   ---------------------

   procedure Set_Inst_Id_Map
     (Relocs                         : in out Checkpoint_Relocations;
      Source_Inst_Id, Target_Inst_Id : Valid_Inst_Id)
   is
   begin
      Relocs.Inst_Map (Source_Inst_Id) := Target_Inst_Id;
   end Set_Inst_Id_Map;

   -------------------------
   -- Set_BDD_Node_Id_Map --
   -------------------------

   procedure Set_BDD_Node_Id_Map
     (Relocs                                 : in out Checkpoint_Relocations;
      Source_BDD_Node_Id, Target_BDD_Node_Id : Valid_BDD_Node_Id)
   is
   begin
      Relocs.BDD_Map (Source_BDD_Node_Id) := Target_BDD_Node_Id;
   end Set_BDD_Node_Id_Map;

   --------------------
   -- Set_SCO_Id_Map --
   --------------------

   procedure Set_SCO_Id_Map
     (Relocs                       : in out Checkpoint_Relocations;
      Source_SCO_Id, Target_SCO_Id : Valid_SCO_Id)
   is
   begin
      Relocs.SCO_Map (Source_SCO_Id) := Target_SCO_Id;
   end Set_SCO_Id_Map;

   ---------------
   -- Remap_SFI --
   ---------------

   procedure Remap_SFI
     (Relocs : Checkpoint_Relocations;
      CP_SFI : in out Source_File_Index)
   is
   begin
      if CP_SFI /= No_Source_File then
         CP_SFI := Relocs.SFI_Map (CP_SFI);
         pragma Assert (CP_SFI /= No_Source_File);
      end if;
   end Remap_SFI;

   ---------------
   -- Remap_SFI --
   ---------------

   function Remap_SFI
     (Relocs : Checkpoint_Relocations;
      CP_SFI : Source_File_Index) return Source_File_Index
   is
   begin
      if CP_SFI /= No_Source_File then
         pragma Assert (Relocs.SFI_Map (CP_SFI) /= No_Source_File);
         return Relocs.SFI_Map (CP_SFI);
      end if;
      return CP_SFI;
   end Remap_SFI;

   -----------------
   -- Remap_CU_Id --
   -----------------

   function Remap_CU_Id
     (Relocs   : Checkpoint_Relocations;
      CP_CU_Id : CU_Id) return CU_Id
   is
   begin
      if CP_CU_Id /= No_CU_Id then
         pragma Assert (Relocs.CU_Map (CP_CU_Id) /= No_CU_Id);
         return Relocs.CU_Map (CP_CU_Id);
      end if;
      return CP_CU_Id;
   end Remap_CU_Id;

   -------------------
   -- Remap_Inst_Id --
   -------------------

   function Remap_Inst_Id
     (Relocs     : Checkpoint_Relocations;
      CP_Inst_Id : Inst_Id) return Inst_Id
   is
   begin
      if CP_Inst_Id /= No_Inst_Id then
         pragma Assert (Relocs.Inst_Map (CP_Inst_Id) /= No_Inst_Id);
         return Relocs.Inst_Map (CP_Inst_Id);
      end if;
      return CP_Inst_Id;
   end Remap_Inst_Id;

   -----------------------
   -- Remap_BDD_Node_Id --
   -----------------------

   function Remap_BDD_Node_Id
     (Relocs         : Checkpoint_Relocations;
      CP_BDD_Node_Id : BDD_Node_Id) return BDD_Node_Id
   is
   begin
      if CP_BDD_Node_Id /= No_BDD_Node_Id then
         pragma Assert (Relocs.BDD_Map (CP_BDD_Node_Id) /= No_BDD_Node_Id);
         return Relocs.BDD_Map (CP_BDD_Node_Id);
      end if;
      return CP_BDD_Node_Id;
   end Remap_BDD_Node_Id;

   ------------------
   -- Remap_SCO_Id --
   ------------------

   function Remap_SCO_Id
     (Relocs    : Checkpoint_Relocations;
      CP_SCO_Id : SCO_Id) return SCO_Id
   is
   begin
      if CP_SCO_Id /= No_SCO_Id then
         pragma Assert (Relocs.SCO_Map (CP_SCO_Id) /= No_SCO_Id);
         return Relocs.SCO_Map (CP_SCO_Id);
      end if;
      return CP_SCO_Id;
   end Remap_SCO_Id;

   procedure Checkpoint_Load
     (Filename             : String;
      Purpose              : Checkpoint_Purpose;
      Ignored_Source_Files : access GNAT.Regexp.Regexp) with
      Pre => Purpose = Instrumentation or else Ignored_Source_Files = null;
   --  Common implementation for SID_Load and Checkpoint_Load

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save
     (Filename : String;
      Context  : access Coverage.Context;
      Purpose  : Checkpoint_Purpose)
   is
      Version : constant Checkpoint_Version := Default_Checkpoint_Version;
      SF      : Ada.Streams.Stream_IO.File_Type;
   begin
      Create (SF, Out_File, Filename);

      declare
         use Coverage;

         CSS              : aliased Checkpoint_Save_State :=
           (Root_Stream_Type with
            Stream   => Stream (SF),
            Filename => To_Unbounded_String (Filename),
            Version  => Default_Checkpoint_Version,
            Purpose  => Purpose);
         Supported_Levels : Levels_Type := Current_Levels;
      begin
         Checkpoint_Header'Write
           (CSS.Stream, (Version => Version, others => <>));

         Checkpoint_Purpose'Write (CSS.Stream, Purpose);

         --  Instrumentation is the same for all MC/DC variants, so a
         --  checkpoint generated for any of them supports all of them.

         if Purpose = Instrumentation and then MCDC_Coverage_Enabled then
            Supported_Levels (MCDC_Coverage_Level'Range) := (others => True);
         end if;
         Coverage.Levels_Type'Write (CSS.Stream, Supported_Levels);

         Files_Table.Checkpoint_Save (CSS'Access);
         SC_Obligations.Checkpoint_Save (CSS'Access);
         Instrument.Common.Checkpoint_Save (CSS'Access);
         Coverage.Source.Checkpoint_Save (CSS'Access);
         Traces_Files_Registry.Checkpoint_Save (CSS'Access, Context);
      end;

      Close (SF);
   end Checkpoint_Save;

   ----------------------
   -- Checkpoint_Clear --
   ----------------------

   procedure Checkpoint_Clear is
   begin
      Files_Table.Checkpoint_Clear;
      SC_Obligations.Checkpoint_Clear;
      Instrument.Common.Checkpoint_Clear;
      Coverage.Source.Checkpoint_Clear;
      Traces_Files_Registry.Checkpoint_Clear;
   end Checkpoint_Clear;

   --------------
   -- SID_Load --
   --------------

   procedure SID_Load
     (Filename             : String;
      Ignored_Source_Files : access GNAT.Regexp.Regexp) is
   begin
      Checkpoint_Load (Filename, Instrumentation, Ignored_Source_Files);
   end SID_Load;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load (Filename : String) is
   begin
      Checkpoint_Load (Filename, Consolidation, null);
   end Checkpoint_Load;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load
     (Filename             : String;
      Purpose              : Checkpoint_Purpose;
      Ignored_Source_Files : access GNAT.Regexp.Regexp)
   is
      SF        : Ada.Streams.Stream_IO.File_Type;
      CP_Header : Checkpoint_Header;
      Levels    : Coverage.Levels_Type;
   begin
      Open (SF, In_File, Filename);

      declare
         CLS : aliased Checkpoint_Load_State :=
           (Root_Stream_Type with
            Stream   => Stream (SF),
            Filename => To_Unbounded_String (Filename),
            Purpose  => Purpose,
            others   => <>);
      begin
         Checkpoint_Header'Read (CLS.Stream, CP_Header);
         if CP_Header.Magic /= Checkpoint_Magic then
            Fatal_Error ("invalid checkpoint file " & Filename);

         elsif CP_Header.Version not in Checkpoint_Version then
            Fatal_Error
              ("invalid checkpoint version" & CP_Header.Version'Img);

         else
            CLS.Version := CP_Header.Version;

            --  Check that we are loading the kind of checkpoint we are
            --  expecting (Purpose).

            if not Version_Less (CLS'Access, Than => 2) then
               declare
                  CP_Purpose : constant Checkpoint_Purpose :=
                     Checkpoint_Purpose'Input (CLS.Stream);
               begin
                  if CP_Purpose /= Purpose then
                     Fatal_Error
                       (Filename & " is a " & Purpose_Name (CP_Purpose)
                        & " while a " & Purpose_Name (Purpose)
                        & " was expected");
                  end if;
               end;
            end if;

            Coverage.Levels_Type'Read (CLS.Stream, Levels);
            declare
               Error_Msg : constant String :=
                 Coverage.Is_Load_Allowed (Filename, Levels);
            begin
               if Error_Msg'Length > 0 then
                  Fatal_Error (Error_Msg);
               end if;
            end;

            Files_Table.Checkpoint_Load (CLS'Access, Ignored_Source_Files);
            SC_Obligations.Checkpoint_Load (CLS'Access);
            if not Version_Less (CLS'Access, Than => 2) then
               Instrument.Common.Checkpoint_Load (CLS'Access);
            end if;
            Coverage.Source.Checkpoint_Load (CLS'Access);
            Traces_Files_Registry.Checkpoint_Load (CLS'Access);

            Free (CLS.Relocations);
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

   ----------
   -- Free --
   ----------

   procedure Free (Relocs : in out Checkpoint_Relocations) is
   begin
      Free (Relocs.SFI_Map);
      Free (Relocs.CU_Map);
      Free (Relocs.Inst_Map);
      Free (Relocs.BDD_Map);
      Free (Relocs.SCO_Map);
      Free (Relocs.Ignored_SFIs);
      Free (Relocs.Ignored_CUs);
      Free (Relocs.Ignored_SCOs);
   end Free;

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
