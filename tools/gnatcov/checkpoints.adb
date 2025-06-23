------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
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

pragma Warnings (Off, "* is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "* is an internal GNAT unit");

with Interfaces; use Interfaces;

with Coverage.Source;
with Coverage_Options; use Coverage_Options;
with Instrument.Checkpoints;
with Outputs;          use Outputs;
with Traces_Files;     use Traces_Files;
with Traces_Files_Registry;

package body Checkpoints is

   Checkpoint_Magic : constant String := "GNATcov checkpoint" & ASCII.NUL;

   procedure Free is new Ada.Unchecked_Deallocation
     (SFI_Map_Array, SFI_Map_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
     (CU_Id_Map_Array, CU_Id_Map_Acc);
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

   type Binary_Traces_Bits is (Undetermined, Bits_32, Bits_64);
   --  Describe the nature of binary traces that contributed to create a
   --  checkpoint.
   --
   --  Undetermined
   --
   --    No binary trace was used (as far as gnatcov knows).
   --
   --  Bits_32
   --
   --    Only 32-bit traces were used.
   --
   --  Bits_64
   --
   --    Only 64-bit traces were used.
   --
   --  Remember that it is invalid to mix 32-bit and 64-bit traces.

   subtype Determined_Binary_Traces_Bits is
     Binary_Traces_Bits range Bits_32 ..  Bits_64;

   Supported_Bits : constant Determined_Binary_Traces_Bits :=
     (if Pc_Type_Size = 4 then Bits_32 else Bits_64);
   --  Kind of binary traces that this instance of gnatcov can read

   function Image (Bits : Determined_Binary_Traces_Bits) return String
   is (case Bits is
       when Bits_32 => "32-bit traces",
       when Bits_64 => "64-bit traces");
   --  Helper to format error messages about binary traces bits. Return the
   --  name of the kind of traces described by Bits.

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
      Relocs.SFI_Simple_Filenames :=
        new SFI_Simple_Name_Map_Array'(First .. Last => <>);
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

   ---------------------
   -- Get_Simple_Name --
   ---------------------

   function Get_Simple_Name
     (Relocs : Checkpoint_Relocations;
      CP_SFI : Valid_Source_File_Index) return Unbounded_String
   is
   begin
      return Relocs.SFI_Simple_Filenames.all (CP_SFI);
   end Get_Simple_Name;

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

   -------------------------
   -- Set_SFI_Simple_Name --
   -------------------------

   procedure Set_SFI_Simple_Name
     (Relocs      : in out Checkpoint_Relocations;
      SFI         : Valid_Source_File_Index;
      Simple_Name : Unbounded_String)
   is
   begin
      Relocs.SFI_Simple_Filenames.all (SFI) := Simple_Name;
   end Set_SFI_Simple_Name;

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

   ---------------------------
   -- Remap_ALI_Annotations --
   ---------------------------

   procedure Remap_ALI_Annotations
     (Relocs             : Checkpoint_Relocations;
      CP_ALI_Annotations : in out ALI_Annotation_Maps.Map)
   is
      use ALI_Annotation_Maps;
      Result : Map;
   begin
      for Cur in CP_ALI_Annotations.Iterate loop
         declare
            Sloc : Source_Location := Key (Cur);
         begin
            Remap_SFI (Relocs, Sloc.Source_File);
            if Sloc.Source_File /= No_Source_File then
               Result.Include (Sloc, Element (Cur));
            end if;
         end;
      end loop;
      CP_ALI_Annotations := Result;
   end Remap_ALI_Annotations;

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
      SF : Ada.Streams.Stream_IO.File_Type;
   begin
      Create (SF, Out_File, Filename);

      declare
         use Coverage;

         CSS              : aliased Checkpoint_Save_State :=
           (Root_Stream_Type with
            Stream   => Stream (SF),
            Filename => +Filename,
            Purpose  => Purpose);
         Supported_Levels : Levels_Type := Current_Levels;
      begin
         --  Write the checkpoint header: magic, version and purpose

         CSS.Write (Checkpoint_Magic);
         CSS.Write_U32 (Checkpoint_Version'Last);
         CSS.Write_U8 (Checkpoint_Purpose'Pos (Purpose));

         --  Describe the binary traces (if any) that contributed to the
         --  creation of this checkpoint.

         declare
            Bits : constant Binary_Traces_Bits :=
              (case Currently_Accepted_Trace_Kind is
               when Unknown | Source_Trace_File         => Undetermined,
               when Binary_Trace_File | All_Trace_Files => Supported_Bits);
         begin
            CSS.Write_U8 (Binary_Traces_Bits'Pos (Bits));
         end;

         --  Instrumentation is the same for all MC/DC variants, so a
         --  checkpoint generated for any of them supports all of them.
         --  Instrumentation for MC/DC also provides everything needed
         --  for decision coverage analysis.

         if Purpose = Instrumentation and then MCDC_Coverage_Enabled then
            Supported_Levels (MCDC_Coverage_Level'Range) := (others => True);
            Supported_Levels (Decision) := True;
         end if;
         for Level in Insn .. GExpr loop
            CSS.Write (Supported_Levels (Level));
         end loop;

         CSS.Write_U8
           (Traces_Files.Any_Accepted_Trace_Kind'Pos
              (Traces_Files.Currently_Accepted_Trace_Kind));

         Files_Table.Checkpoint_Save (CSS'Access);
         SC_Obligations.Checkpoint_Save (CSS'Access);
         Instrument.Checkpoints.Checkpoint_Save (CSS'Access);
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
      Instrument.Checkpoints.Checkpoint_Clear;
      Coverage.Source.Checkpoint_Clear;
      Traces_Files_Registry.Checkpoint_Clear;
   end Checkpoint_Clear;

   --------------
   -- SID_Load --
   --------------

   procedure SID_Load
     (Filename             : String;
      Ignored_Source_Files : access GNAT.Regexp.Regexp)
   is
      SID_Re : constant GNAT.Regexp.Regexp := GNAT.Regexp.Compile (".*\.sid");
   begin
      if not GNAT.Regexp.Match (Filename, SID_Re) then
         Fatal_Error ("invalid "
                      & Purpose_Name (Instrumentation)
                      & " file "
                      & Filename
                      & ", name of file should have .sid extension");
      end if;

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
      Dummy : constant Context_Handle :=
        Create_Context ("Loading " & Filename);
      SF    : Ada.Streams.Stream_IO.File_Type;
   begin
      Open (SF, In_File, Filename);

      --  If requested, create an artificial internal error when loading
      --  checkpoints.

      Raise_Stub_Internal_Error_For (Load_Checkpoint);

      declare
         CLS : Checkpoint_Load_State :=
           (Root_Stream_Type with
            Stream   => Stream (SF),
            Filename => +Filename,
            Purpose  => Purpose,
            others   => <>);

         Magic : String (Checkpoint_Magic'Range);
      begin
         CLS.Read (Magic);
         if Magic /= Checkpoint_Magic then
            Fatal_Error ("invalid checkpoint file " & Filename);
         end if;

         CLS.Version := CLS.Read_U32;
         if CLS.Version /= Checkpoint_Version'Last then
            Fatal_Error
              (Filename & ": invalid checkpoint version" & CLS.Version'Img);
         end if;

         --  Check that we are loading the kind of checkpoint we are
         --  expecting (Purpose).

         declare
            CP_Purpose : constant Checkpoint_Purpose :=
              Checkpoint_Purpose'Val (CLS.Read_U8);
         begin
            if CP_Purpose /= Purpose then
               Fatal_Error
                 (Filename & " is a " & Purpose_Name (CP_Purpose)
                  & " while a " & Purpose_Name (Purpose)
                  & " was expected");
            end if;
         end;

         --  Check the kind of binary traces that were used to create this
         --  checkpoint.

         declare
            Bits : constant Binary_Traces_Bits :=
              Binary_Traces_Bits'Val (CLS.Read_U8);
         begin
            if Bits not in Undetermined | Supported_Bits then
               Fatal_Error
                 (Filename & " was created with " & Image (Bits)
                  & " whereas the selected target requires "
                  & Image (Supported_Bits));
            end if;
         end;

         --  Check that the checkpoint to load covers all the coverage levels
         --  that are selected for this run.
         --
         --  See the note on checkpoint version compatibility for the tedious
         --  reading code.

         declare
            CP_Levels : U8_Array (1 .. 10);
            Levels    : Levels_Type;
         begin
            CLS.Read (CP_Levels);
            Levels :=
              (Insn     => Boolean'Val (CP_Levels (1)),
               Branch   => Boolean'Val (CP_Levels (2)),
               Stmt     => Boolean'Val (CP_Levels (3)),
               Decision => Boolean'Val (CP_Levels (4)),
               MCDC     => Boolean'Val (CP_Levels (5)),
               UC_MCDC  => Boolean'Val (CP_Levels (6)),
               ATC      => Boolean'Val (CP_Levels (7)),
               ATCC     => Boolean'Val (CP_Levels (8)),
               Fun_Call => Boolean'Val (CP_Levels (9)),
               GExpr    => Boolean'Val (CP_Levels (10)));
            declare
               Error_Msg : constant String :=
                 Coverage.Is_Load_Allowed (Filename, Levels);
            begin
               if Error_Msg'Length > 0 then
                  Fatal_Error (Error_Msg);
               end if;
            end;
         end;

         Update_Current_Trace_Kind
           (Any_Accepted_Trace_Kind'Val (CLS.Read_U8));

         Files_Table.Checkpoint_Load (CLS, Ignored_Source_Files);
         SC_Obligations.Checkpoint_Load (CLS);
         Instrument.Checkpoints.Checkpoint_Load (CLS);
         Coverage.Source.Checkpoint_Load (CLS);
         Traces_Files_Registry.Checkpoint_Load (CLS);

         Free (CLS.Relocations);
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

   --  Note for the Read* procedures below: using stream attributes for
   --  well-defined elementary data types (String, Unsigned_*) can be
   --  considered stable enough that it will preserve backwards and platform
   --  compatibility, except maybe for endianity. For now, gnatcov is supported
   --  on little-endian platforms only, so we should be fine.

   -------------------
   -- Read_BDD_Node --
   -------------------

   function Read_BDD_Node
     (Self : in out Checkpoint_Load_State) return BDD_Node_Id is
   begin
      return BDD_Node_Id (Self.Read_I32);
   end Read_BDD_Node;

   -----------------
   -- Read_Bit_Id --
   -----------------

   function Read_Bit_Id
     (Self : in out Checkpoint_Load_State) return Any_Bit_Id
   is
   begin
      return Any_Bit_Id (Self.Read_I32);
   end Read_Bit_Id;

   ------------------
   -- Read_Boolean --
   ------------------

   function Read_Boolean (Self : in out Checkpoint_Load_State) return Boolean
   is
   begin
      return Boolean'Val (Self.Read_U8);
   end Read_Boolean;

   -------------
   -- Read_CU --
   -------------

   function Read_CU (Self : in out Checkpoint_Load_State) return CU_Id is
   begin
      return CU_Id (Self.Read_I32);
   end Read_CU;

   ---------------------------
   -- Read_Compilation_Unit --
   ---------------------------

   function Read_Compilation_Unit
     (Self : in out Checkpoint_Load_State) return Compilation_Unit
   is
      Language : constant Supported_Language_Kind := Self.Read_Language_Kind;
   begin
      return (Language => Language, Unit_Name => Self.Read_Unbounded_String);
   end Read_Compilation_Unit;

   --------------------
   -- Read_Condition --
   --------------------

   function Read_Condition
     (Self : in out Checkpoint_Load_State) return Any_Condition_Index is
   begin
      return Any_Condition_Index (Self.Read_I32);
   end Read_Condition;

   ----------------------
   -- Read_Fingerprint --
   ----------------------

   function Read_Fingerprint
     (Self : in out Checkpoint_Load_State)
      return SC_Obligations.Fingerprint_Type
   is
      Buffer : String (1 .. SC_Obligations.Fingerprint_Type'Length);
      I      : Positive := 1;
   begin
      Self.Read (Buffer);
      return Result : SC_Obligations.Fingerprint_Type do
         for Digit of Result loop
            Digit := Character'Pos (Buffer (I));
            I := I + 1;
         end loop;
      end return;
   end Read_Fingerprint;

   --------------
   -- Read_I32 --
   --------------

   function Read_I32
     (Self : in out Checkpoint_Load_State) return Interfaces.Integer_32 is
   begin
      return Interfaces.Integer_32'Input (Self.Stream);
   end Read_I32;

   ------------------
   -- Read_Integer --
   ------------------

   function Read_Integer (Self : in out Checkpoint_Load_State) return Integer
   is
   begin
      return Integer (Self.Read_I32);
   end Read_Integer;

   ------------------------
   -- Read_Language_Kind --
   ------------------------

   function Read_Language_Kind
     (Self : in out Checkpoint_Load_State) return Supported_Language_Kind is
   begin
      return Supported_Language_Kind'Val (Self.Read_U32);
   end Read_Language_Kind;

   ---------------------
   -- Read_Line_State --
   ---------------------

   function Read_Line_State
     (Self : in out Checkpoint_Load_State) return Any_Line_State is
   begin
      return Any_Line_State'Val (Self.Read_U8);
   end Read_Line_State;

   --------------------------------
   -- Read_Local_Source_Location --
   --------------------------------

   function Read_Local_Source_Location
     (Self : in out Checkpoint_Load_State) return Local_Source_Location
   is
      Line   : constant Natural := Self.Read_Integer;
      Column : constant Integer := Self.Read_Integer;
   begin
      return (Line, Column);
   end Read_Local_Source_Location;

   --------------------------------------
   -- Read_Local_Source_Location_Range --
   --------------------------------------

   function Read_Local_Source_Location_Range
     (Self : in out Checkpoint_Load_State) return Local_Source_Location_Range
   is
      First : constant Local_Source_Location :=
        Self.Read_Local_Source_Location;
      Last  : constant Local_Source_Location :=
        Self.Read_Local_Source_Location;
   begin
      return (First, Last);
   end Read_Local_Source_Location_Range;

   -------------
   -- Read_PC --
   -------------

   function Read_PC (Self : in out Checkpoint_Load_State) return Pc_Type is
   begin
      case Pc_Type'Size is
         when 32 =>
            return Pc_Type (Self.Read_U32);
         when 64 =>
            return Pc_Type (Self.Read_U64);
         when others =>
            raise Program_Error;
      end case;
   end Read_PC;

   --------------
   -- Read_SCO --
   --------------

   function Read_SCO (Self : in out Checkpoint_Load_State) return SCO_Id is
   begin
      return SCO_Id (Self.Read_I32);
   end Read_SCO;

   --------------
   -- Read_SFI --
   --------------

   function Read_SFI
     (Self : in out Checkpoint_Load_State) return Source_File_Index is
   begin
      return Source_File_Index (Self.Read_I32);
   end Read_SFI;

   --------------------------
   -- Read_Source_Location --
   --------------------------

   function Read_Source_Location
     (Self : in out Checkpoint_Load_State) return Source_Location
   is
   begin
      return Result : Source_Location do
         Self.Read (Result);
      end return;
   end Read_Source_Location;

   --------------------------------
   -- Read_Source_Location_Range --
   --------------------------------

   function Read_Source_Location_Range
     (Self : in out Checkpoint_Load_State) return Source_Location_Range is
   begin
      return Result : Source_Location_Range do
         Result.Source_File := Self.Read_SFI;
         Result.L := Self.Read_Local_Source_Location_Range;
      end return;
   end Read_Source_Location_Range;

   -----------------
   -- Read_String --
   -----------------

   function Read_String (Self : in out Checkpoint_Load_State) return String is
      First : constant Positive := Self.Read_Integer;
      Last  : constant Natural := Self.Read_Integer;
   begin
      return Result : String (First .. Last) do
         Self.Read (Result);
      end return;
   end Read_String;

   -------------------
   -- Read_Tristate --
   -------------------

   function Read_Tristate
     (Self : in out Checkpoint_Load_State) return Tristate is
   begin
      return Tristate'Val (Self.Read_U8);
   end Read_Tristate;

   -------------
   -- Read_U8 --
   -------------

   function Read_U8
     (Self : in out Checkpoint_Load_State) return Interfaces.Unsigned_8 is
   begin
      return Interfaces.Unsigned_8'Input (Self.Stream);
   end Read_U8;

   --------------
   -- Read_U16 --
   --------------

   function Read_U16
     (Self : in out Checkpoint_Load_State) return Interfaces.Unsigned_16 is
   begin
      return Interfaces.Unsigned_16'Input (Self.Stream);
   end Read_U16;

   --------------
   -- Read_U32 --
   --------------

   function Read_U32
     (Self : in out Checkpoint_Load_State) return Interfaces.Unsigned_32 is
   begin
      return Interfaces.Unsigned_32'Input (Self.Stream);
   end Read_U32;

   --------------
   -- Read_U64 --
   --------------

   function Read_U64
     (Self : in out Checkpoint_Load_State) return Interfaces.Unsigned_64 is
   begin
      return Interfaces.Unsigned_64'Input (Self.Stream);
   end Read_U64;

   ---------------------------
   -- Read_Unbounded_String --
   ---------------------------

   function Read_Unbounded_String
     (Self : in out Checkpoint_Load_State) return Unbounded_String
   is
   begin
      return Result : Unbounded_String do
         Read (Self, Result);
      end return;
   end Read_Unbounded_String;

   --------------
   -- Read_Map --
   --------------

   procedure Read_Map
     (Self : in out Checkpoint_Load_State; Map : out Map_Type)
   is
      Count : constant Interfaces.Integer_32 := Self.Read_I32;
   begin
      Clear (Map);
      for I in 1 .. Count loop
         declare
            Key     : Key_Type;
            Element : Element_Type;
         begin
            Read_Key (Self, Key);
            Read_Element (Self, Element);
            Insert (Map, Key, Element);
         end;
      end loop;
   end Read_Map;

   --------------
   -- Read_Set --
   --------------

   procedure Read_Set
     (Self : in out Checkpoint_Load_State; Set : out Set_Type)
   is
      Count : constant Interfaces.Integer_32 := Self.Read_I32;
   begin
      Clear (Set);
      for I in 1 .. Count loop
         declare
            Element : Element_Type;
         begin
            Read_Element (Self, Element);
            Insert (Set, Element);
         end;
      end loop;
   end Read_Set;

   ---------------
   -- Read_Tree --
   ---------------

   procedure Read_Tree
     (Self : in out Checkpoint_Load_State; Tree : out Multiway_Trees.Tree)
   is
      procedure Read_Children (Parent : Multiway_Trees.Cursor);
      --  Read a list of child subtrees and append them to Parent

      procedure Read_Subtree (Parent : Multiway_Trees.Cursor);
      --  Read a subtree and append it to Parent

      -------------------
      -- Read_Children --
      -------------------

      procedure Read_Children (Parent : Multiway_Trees.Cursor) is
         Count : constant Natural := Self.Read_Integer;
      begin
         for I in 1 .. Count loop
            Read_Subtree (Parent);
         end loop;
      end Read_Children;

      ------------------
      -- Read_Subtree --
      ------------------

      procedure Read_Subtree (Parent : Multiway_Trees.Cursor) is
         Element : Element_Type;
      begin
         Read_Element (Self, Element);
         Tree.Append_Child (Parent, Element);
         Read_Children (Multiway_Trees.Last_Child (Parent));
      end Read_Subtree;

      Count : constant Natural := Self.Read_Integer;

   --  Start of processing for Read

   begin
      Tree.Clear;
      if Count > 0 then
         Read_Children (Tree.Root);
      end if;
   end Read_Tree;

   -----------------
   -- Read_Vector --
   -----------------

   procedure Read_Vector
     (Self : in out Checkpoint_Load_State; Vector : out Vectors.Vector)
   is
      procedure Process (Element : in out Element_Type);
      --  Call Read_Element on Element

      -------------
      -- Process --
      -------------

      procedure Process (Element : in out Element_Type) is
      begin
         Read_Element (Self, Element);
      end Process;

      Length : constant Interfaces.Integer_32 := Self.Read_I32;

   --  Start of processing for Read_Vector

   begin
      Vector.Clear;
      Vector.Set_Length (Ada.Containers.Count_Type (Length));
      for Cur in Vector.Iterate loop
         Vector.Update_Element (Cur, Process'Access);
      end loop;
   end Read_Vector;

   ----------
   -- Read --
   ----------

   procedure Read (Self : in out Checkpoint_Load_State; Value : out U8_Array)
   is
   begin
      U8_Array'Read (Self.Stream, Value);
   end Read;

   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out CU_Id) is
   begin
      Value := Self.Read_CU;
   end Read;

   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out Pc_Type) is
   begin
      Value := Self.Read_PC;
   end Read;

   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out SCO_Id) is
   begin
      Value := Self.Read_SCO;
   end Read;

   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out Source_File_Index) is
   begin
      Value := Self.Read_SFI;
   end Read;

   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out Source_Location) is
   begin
      Value.Source_File := Self.Read_SFI;
      Value.L := Self.Read_Local_Source_Location;
   end Read;

   procedure Read (Self : in out Checkpoint_Load_State; Value : out String) is
   begin
      String'Read (Self.Stream, Value);
   end Read;

   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out Tristate) is
   begin
      Value := Self.Read_Tristate;
   end Read;

   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out Unbounded_String)
   is
      procedure Set (S : out String);
      --  Callback for Set_String: set S with bytes read from Self

      ---------
      -- Set --
      ---------

      procedure Set (S : out String) is
      begin
         Self.Read (S);
      end Set;

      First : constant Positive := Self.Read_Integer;
      Last  : constant Natural := Self.Read_Integer;
      subtype Fixed_String is String (First .. Last);
      Length : constant Natural := Fixed_String'Length;

   --  Start of processing for Read

   begin
      if Length = 0 then
         Value := Null_Unbounded_String;
      else
         Set_String (Value, Length, Set'Access);
      end if;
   end Read;

   --------------------
   -- Write_BDD_Node --
   --------------------

   procedure Write_BDD_Node
     (Self : in out Checkpoint_Save_State; Value : BDD_Node_Id) is
   begin
      Self.Write_I32 (Interfaces.Integer_32 (Value));
   end Write_BDD_Node;

   ------------------
   -- Write_Bit_Id --
   ------------------

   procedure Write_Bit_Id
     (Self : in out Checkpoint_Save_State; Value : Any_Bit_Id) is
   begin
      Self.Write_I32 (Interfaces.Integer_32 (Value));
   end Write_Bit_Id;

   --------------
   -- Write_CU --
   --------------

   procedure Write_CU (Self : in out Checkpoint_Save_State; Value : CU_Id) is
   begin
      Self.Write_I32 (Interfaces.Integer_32 (Value));
   end Write_CU;

   ---------------------
   -- Write_Condition --
   ---------------------

   procedure Write_Condition
     (Self : in out Checkpoint_Save_State; Value : Any_Condition_Index) is
   begin
      Self.Write_I32 (Interfaces.Integer_32 (Value));
   end Write_Condition;

   -----------------
   -- Write_Count --
   -----------------

   procedure Write_Count
     (Self : in out Checkpoint_Save_State; Value : Count_Type) is
   begin
      Self.Write_I32 (Interfaces.Integer_32 (Value));
   end Write_Count;

   ---------------
   -- Write_I32 --
   ---------------

   procedure Write_I32
     (Self : in out Checkpoint_Save_State; Value : Interfaces.Integer_32) is
   begin
      Interfaces.Integer_32'Write (Self.Stream, Value);
   end Write_I32;

   -------------------
   -- Write_Integer --
   -------------------

   procedure Write_Integer
     (Self : in out Checkpoint_Save_State; Value : Integer) is
   begin
      Self.Write_I32 (Interfaces.Integer_32 (Value));
   end Write_Integer;

   --------------
   -- Write_PC --
   --------------

   procedure Write_PC (Self : in out Checkpoint_Save_State; Value : Pc_Type) is
   begin
      case Pc_Type'Size is
         when 32 =>
            Self.Write_U32 (Interfaces.Unsigned_32 (Value));
         when 64 =>
            Self.Write_U64 (Interfaces.Unsigned_64 (Value));
         when others =>
            raise Program_Error;
      end case;
   end Write_PC;

   ---------------
   -- Write_SCO --
   ---------------

   procedure Write_SCO (Self : in out Checkpoint_Save_State; Value : SCO_Id) is
   begin
      Self.Write_I32 (Interfaces.Integer_32 (Value));
   end Write_SCO;

   ---------------
   -- Write_SFI --
   ---------------

   procedure Write_SFI
     (Self : in out Checkpoint_Save_State; Value : Source_File_Index) is
   begin
      Self.Write_I32 (Interfaces.Integer_32 (Value));
   end Write_SFI;

   --------------
   -- Write_U8 --
   --------------

   procedure Write_U8
     (Self : in out Checkpoint_Save_State; Value : Interfaces.Unsigned_8) is
   begin
      Interfaces.Unsigned_8'Write (Self.Stream, Value);
   end Write_U8;

   ---------------
   -- Write_U16 --
   ---------------

   procedure Write_U16
     (Self : in out Checkpoint_Save_State; Value : Interfaces.Unsigned_16) is
   begin
      Interfaces.Unsigned_16'Write (Self.Stream, Value);
   end Write_U16;

   ---------------
   -- Write_U32 --
   ---------------

   procedure Write_U32
     (Self : in out Checkpoint_Save_State; Value : Interfaces.Unsigned_32) is
   begin
      Interfaces.Unsigned_32'Write (Self.Stream, Value);
   end Write_U32;

   ---------------
   -- Write_U64 --
   ---------------

   procedure Write_U64
     (Self : in out Checkpoint_Save_State; Value : Interfaces.Unsigned_64) is
   begin
      Interfaces.Unsigned_64'Write (Self.Stream, Value);
   end Write_U64;

   -----------
   -- Write --
   -----------

   procedure Write (Self : in out Checkpoint_Save_State; Value : Boolean) is
   begin
      Self.Write_U8 (Boolean'Pos (Value));
   end Write;

   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Compilation_Unit) is
   begin
      Self.Write (Value.Language);
      Self.Write (Value.Unit_Name);
   end Write;

   procedure Write
     (Self  : in out Checkpoint_Save_State;
      Value : SC_Obligations.Fingerprint_Type)
   is
      Buffer : String (1 .. SC_Obligations.Fingerprint_Type'Length);
      I      : Positive := 1;
   begin
      for Digit of Value loop
         Buffer (I) := Character'Val (Digit);
         I := I + 1;
      end loop;
      Self.Write (Buffer);
   end Write;

   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Supported_Language_Kind) is
   begin
      Self.Write_U32 (Supported_Language_Kind'Pos (Value));
   end Write;

   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Any_Line_State) is
   begin
      Self.Write_U8 (Any_Line_State'Pos (Value));
   end Write;

   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Local_Source_Location) is
   begin
      Self.Write_Integer (Value.Line);
      Self.Write_Integer (Value.Column);
   end Write;

   procedure Write
     (Self  : in out Checkpoint_Save_State;
      Value : Local_Source_Location_Range) is
   begin
      Self.Write (Value.First_Sloc);
      Self.Write (Value.Last_Sloc);
   end Write;

   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Source_Location) is
   begin
      Self.Write_SFI (Value.Source_File);
      Self.Write (Value.L);
   end Write;

   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Source_Location_Range) is
   begin
      Self.Write_SFI (Value.Source_File);
      Self.Write (Value.L);
   end Write;

   procedure Write (Self : in out Checkpoint_Save_State; Value : String) is
   begin
      String'Write (Self.Stream, Value);
   end Write;

   procedure Write (Self : in out Checkpoint_Save_State; Value : Tristate) is
   begin
      Self.Write_U8 (Tristate'Pos (Value));
   end Write;

   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Unbounded_String)
   is
      S : Big_String_Access;
      L : Natural;
   begin
      Get_String (Value, S, L);
      Self.Write_Unbounded (S.all (1 .. L));
   end Write;

   ---------------------
   -- Write_Unbounded --
   ---------------------

   procedure Write_Unbounded
     (Self : in out Checkpoint_Save_State; Value : String) is
   begin
      Self.Write_Integer (Value'First);
      Self.Write_Integer (Value'Last);
      Self.Write (Value);
   end Write_Unbounded;

   ---------------
   -- Write_Map --
   ---------------

   procedure Write_Map
     (Self : in out Checkpoint_Save_State; Map : Map_Type)
   is
      procedure Process (Position : Cursor_Type);
      --  Write the key/element couple designated by Position to the checkpoint

      procedure Process (Key : Key_Type; Element : Element_Type);
      --  Write the Key/Element couple to the checkpoint

      -------------
      -- Process --
      -------------

      procedure Process (Position : Cursor_Type) is
      begin
         Query_Element (Position, Process'Access);
      end Process;

      procedure Process (Key : Key_Type; Element : Element_Type) is
      begin
         Write_Key (Self, Key);
         Write_Element (Self, Element);
      end Process;

   --  Start of processing for Write_Map

   begin
      Self.Write_Count (Length (Map));
      Iterate (Map, Process'Access);
   end Write_Map;

   ---------------
   -- Write_Set --
   ---------------

   procedure Write_Set
     (Self : in out Checkpoint_Save_State; Set : Set_Type)
   is
      procedure Process (Position : Cursor_Type);
      --  Write the element designated by Position to the checkpoint

      procedure Process (Element : Element_Type);
      --  Write Element to the checkpoint

      -------------
      -- Process --
      -------------

      procedure Process (Position : Cursor_Type) is
      begin
         Query_Element (Position, Process'Access);
      end Process;

      procedure Process (Element : Element_Type) is
      begin
         Write_Element (Self, Element);
      end Process;

   --  Start of processing for Write_Set

   begin
      Self.Write_Count (Length (Set));
      Iterate (Set, Process'Access);
   end Write_Set;

   ----------------
   -- Write_Tree --
   ----------------

   procedure Write_Tree
     (Self : in out Checkpoint_Save_State; Tree : Multiway_Trees.Tree)
   is
      procedure Write_Children (Parent : Multiway_Trees.Cursor);
      --  Write the list of Parent's child subtrees

      procedure Write_Subtree (Parent : Multiway_Trees.Cursor);
      --  Write the subtree rooted at Parent

      --------------------
      -- Write_Children --
      --------------------

      procedure Write_Children (Parent : Multiway_Trees.Cursor) is
      begin
         Self.Write_Count (Multiway_Trees.Child_Count (Parent));
         for Child in Tree.Iterate_Children (Parent) loop
            Write_Subtree (Child);
         end loop;
      end Write_Children;

      -------------------
      -- Write_Subtree --
      -------------------

      procedure Write_Subtree (Parent : Multiway_Trees.Cursor) is
      begin
         Write_Element (Self, Tree.Constant_Reference (Parent));
         Write_Children (Parent);
      end Write_Subtree;

   --  Start of processing for Write_Tree

   begin
      Self.Write_Count (Tree.Node_Count - 1);
      if not Tree.Is_Empty then
         Write_Children (Tree.Root);
      end if;
   end Write_Tree;

   ------------------
   -- Write_Vector --
   ------------------

   procedure Write_Vector
     (Self : in out Checkpoint_Save_State; Vector : Vectors.Vector) is
   begin
      Self.Write_I32 (Interfaces.Integer_32 (Vector.Length));
      for Element of Vector loop
         Write_Element (Self, Element);
      end loop;
   end Write_Vector;

end Checkpoints;
