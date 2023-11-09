------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2022, AdaCore                     --
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

--  Source Coverage Obligations

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Streams;                 use Ada.Streams;
with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Interfaces;

with Aspects; use Aspects;
with Namet;   use Namet;
with SCOs;
with Snames;

with ALI_Files;     use ALI_Files;
with Checkpoints;   use Checkpoints;
with Coverage.Source;
with Coverage.Tags; use Coverage, Coverage.Tags;
with Diagnostics;   use Diagnostics;
with Files_Table;   use Files_Table;
with Outputs;       use Outputs;
with SC_Obligations.BDD;
with Switches;      use Switches;
with Traces_Elf;    use Traces_Elf;

package body SC_Obligations is

   Instrumented_Units_Present : Boolean := False;

   subtype Source_Location is Slocs.Source_Location;
   No_Location : Source_Location renames Slocs.No_Location;
   --  (not SCOs.Source_Location)

   function SCOs_Nested_And_Ordered
     (Tree : Scope_Entities_Trees.Tree) return Boolean;
   --  Return whether nodes in Tree are:
   --
   --  * properly nested: SCO ranges (Element.From .. Element.To) are disjoint
   --    for two sibling elements, and all nodes' SCO ranges are included in
   --    its parents';
   --
   --  * properly ordered: if E1 and E2 are consecutive siblings, E1.To must be
   --    smaller than E2.From.

   function Covers_SCO (SE : Scope_Entity; SCO : SCO_Id) return Boolean
   is (SCO in SE.From .. SE.To);
   --  Return whether SCO is covered by SE's SCO range

   function Covers_SCO
     (Cur : Scope_Entities_Trees.Cursor; SCO : SCO_Id) return Boolean
   is (Covers_SCO (Scope_Entities_Trees.Element (Cur), SCO));
   --  Return whether SCO is covered by that element's SCO range

   ---------------
   -- Instances --
   ---------------

   type Inst_Info is record
      Sloc               : Source_Location;
      --  Instantiation location

      Enclosing_Instance : Inst_Id;
      --  Index of enclosing instance, or No_Inst_Id if instance is not nested

      Comp_Unit          : CU_Id;
      --  Originating compilation unit, for sanity checking purposes
   end record;

   package Inst_Info_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Valid_Inst_Id,
        Element_Type => Inst_Info);

   ------------------------
   -- Source units table --
   ------------------------

   type CU_Info (Provider : SCO_Provider := SCO_Provider'First) is record
      Origin : Source_File_Index;
      --  File from which this unit's SCO info comes from.
      --  For compiler-based analysis, this is the LI file; for instrumented
      --  sources, this is the original source file.

      Main_Source : Source_File_Index;
      --  Name of main source file.
      --  For Ada this is a simple name; for C this is either a simple name
      --  or full name, depending on whether the information is available.

      First_SCO, Last_SCO : SCO_Id := No_SCO_Id;
      --  First and last SCO ids for this unit

      First_Instance, Last_Instance : Inst_Id := No_Inst_Id;
      --  First and last index of Inst_Vector entries for this unit

      Deps : SFI_Vector;
      --  Mapping of this unit's dependency numbers to source file indices

      Has_Code : Boolean := False;
      --  Set True when object code for some source file in this unit is seen

      Fingerprint : Fingerprint_Type;
      --  Hash of SCO info in ALI, for incremental coverage consistency check

      PP_Info_Map : SCO_PP_Info_Maps.Map;
      --  Information about preprocessing

      Scope_Entities : Scope_Entities_Tree := Scope_Entities_Trees.Empty_Tree;
      --  Scope tree, used to output e.g. subprogram metrics

      case Provider is
         when Compiler =>
            null;

         when Instrumenter =>
            Bit_Maps : CU_Bit_Maps;
            --  Mapping of bits in coverage buffers to SCOs

            Bit_Maps_Fingerprint : Fingerprint_Type;
            --  Hash of Bit_Maps, for consistency checks with source traces
      end case;

   end record;

   procedure Free (CU : in out CU_Info);

   pragma Warnings (Off, "* is not referenced");
   procedure Read
     (S : access Root_Stream_Type'Class;
      V : out CU_Info);
   procedure Write
     (S : access Root_Stream_Type'Class;
      V : CU_Info);
   pragma Warnings (On, "* is not referenced");

   for CU_Info'Read use Read;
   for CU_Info'Write use Write;

   function Has_SCOs (CUI : CU_Info) return Boolean is
     (CUI.First_SCO <= CUI.Last_SCO);

   package CU_Info_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_CU_Id,
      Element_Type => CU_Info);

   package CU_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Source_File_Index,
      Element_Type => Valid_CU_Id);

   CU_Map : CU_Maps.Map;
   --  Map of source file names to CU_Vector indices. Note: there may be
   --  multiple CU_Map entries designating the same LI file (case of an
   --  extended main source unit comprising more than one source file).
   --  Also note that for any source file containing SCOs, the simple name
   --  appears as a key in this map. In addition, for C files, the *full*
   --  name of each main source file also appears as a key.

   package CU_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Valid_CU_Id);

   package Origin_To_CUs_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Source_File_Index,
      Element_Type => CU_Sets.Set,
      "="          => CU_Sets."=");

   Origin_To_CUs_Map : Origin_To_CUs_Maps.Map;
   --  For each compilation unit Id CU, CU is present in the
   --  Origin_To_CU_Map (CU_Map (CU).Origin) set. This data structure allows us
   --  to efficiently find all units that come from the same origin (useful in
   --  Set_Unit_Has_Code).

   procedure Register_CU (CU : CU_Id);
   --  Register CU in Origin_To_CUs_Map. This assumes that CU's Origin is
   --  properly initialized.

   function Instance_Loc (Inst_Index : Inst_Id) return String;
   --  Return a string representation of the instantiation location denoted
   --  by Inst_Index, which must be in Comp_Unit's instance range.

   -----------------------------------
   -- Low level SCO tables handling --
   -----------------------------------

   --  Low level SCO tables can contain several unit entries
   --  (SCOs.SCO_Unit_Table_Entry) that relate to the same source file. This
   --  can happen in C, as the stream of SCOs may interleave obligations from
   --  the main compilation unit (e.g. foo.c) and obligations from headers
   --  (e.g. foo.h, bar.h, ...).
   --
   --  One the one hand, we want to create exactly one CU_Unit per source file
   --  (i.e. for potentially several low level units). On the other hand, high
   --  level SCOs for one CU_Unit must be contiguous, as the SCOs for one
   --  CU_Info are the ones designated by a range of SCO_Id (CU_Info.First_SCO,
   --  CU_Info.Last_SCO). This means that we need to import low level SCOs in
   --  two passes: first group SCOs per unit, and then load SCOs for each unit.
   --
   --  The following helpers (data structures and subprograms) are dedicated to
   --  this first pass, and the public SC_Obligations.Process_Low_Level_SCOs
   --  procedure implements the second pass.

   type Nat_Range is record
      First, Last : Nat;
   end record;
   --  Inclusive range of indexes in SCOs.SCO_Table (i.e. a range of
   --  SCOs.SCO_Table_Entry objects).

   package Nat_Range_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Nat_Range);

   type CU_Load_Info is record
      File_Name_Ptr : Types.String_Ptr;
      --  File name for this unit, from low level tables, used for SCO
      --  fingerprint computation.
      --
      --  Tracking this is necessary since the Source_File component below may
      --  contain full path while low level tables only had a simple name.
      --  Using the simple name in that case is key to have fingerprint
      --  determinism.

      Source_File : Source_File_Index;
      --  Main source file for this unit, used to initialize
      --  CU_Info.Main_Source.

      Entries : Nat_Range_Vectors.Vector;
      --  Sequence of SCO entries to load from low level tables for this unit

      Fingerprint_Context : GNAT.SHA1.Context;
      --  Context to compute the fingerprint of SCOs for this unit.
      --
      --  The aim is to include in the hash all information for which
      --  inconsistency during consolidation would make coverage analysis
      --  nonsensical.

      Fingerprint_Buffer : Unbounded_String;
      --  In verbose mode, buffer to hold the bytes used to compute the
      --  fingerprint.
   end record;
   --  Information about a compilation unit to load, i.e. to create a CU_Info
   --  record and the associated information.

   type CU_Load_Info_Access is access all CU_Load_Info;

   package CU_Load_Info_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => CU_Load_Info_Access);

   procedure Free (Infos : in out CU_Load_Info_Vectors.Vector);
   --  Free all resources allocated in Infos and make it empty

   function Main_Source_For
     (Unit : SCOs.SCO_Unit_Table_Entry;
      Deps : SFI_Vector) return Source_File_Index;
   --  Return the source file corresponding to the low level unit at the given
   --  index (Unit). Create it if needed. Return No_Source_File if this
   --  source file is ignored (case where .Dep_Num = Missing_Dep_Num).
   --
   --  See Process_Low_Level_SCOs for Deps' semantics.

   procedure Append_For_Fingerprint
     (Unit_Info : in out CU_Load_Info; S : String);
   --  Append S to the fingerprint computation for Unit_Info

   procedure Append_For_Fingerprint
     (Unit_Info : in out CU_Load_Info; Sloc : SCOs.Source_Location);
   --  Append Sloc to the fingerprint computation for Unit_Info

   procedure Build_CU_Load_Info
     (Infos : out CU_Load_Info_Vectors.Vector;
      Deps  : SFI_Vector := SFI_Vectors.Empty_Vector);
   --  Fill in Infos with information suitable to load low level SCO tables.
   --
   --  See Process_Low_Level_SCOs for Deps' semantics.

   function Allocate_CU
     (Provider            : SCO_Provider;
      Origin, Main_Source : Source_File_Index;
      Fingerprint         : Fingerprint_Type;
      Created_Units       : in out Created_Unit_Maps.Map) return CU_Id;
   --  If there is already a compilation unit for the given Main_Source that is
   --  not in Created_Units, emit a warning to say the we ignore duplicate SCOs
   --  for it and return No_CU_Id.
   --
   --  If there is such a compilation unit in Created_Units, return its Id.
   --
   --  Otherwise, create a new Compiled-provided compulation unit with the
   --  given Main_Source and Fingerprint, keep track of it in Created_Units,
   --  and return its unit Id.

   package Ignored_Slocs_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Source_Location);

   type CU_Load_State is record
      Last_Line : Natural := 0;
      --  Highest line number involved in SCOs for this unit

      Current_Decision : SCO_Id := No_SCO_Id;
      --  Decision whose conditions are being processed

      Current_Condition_Index : Any_Condition_Index;
      --  Index of current condition within the current decision (0-based, set
      --  to No_Condition_Index, i.e. -1, before the first condition of the
      --  decision is seen).

      Current_BDD : BDD.BDD_Type;
      --  BDD of current decision

      Dom_SCO               : SCO_Id          := No_SCO_Id;
      Dom_Sloc              : Source_Location := No_Location;
      Dom_Val               : Tristate        := Unknown;
      Current_Handler_Range : Source_Location_Range := No_Range;
      --  Dominant information for basic block chaining
   end record;
   --  State for the loading of SCOs for a specific CU. Process_Low_Level_SCOs
   --  creates one instance for each CU, and passes it to each call to
   --  Process_Low_Level_Entry for that CU.

   procedure Process_Low_Level_Entry
     (CU            : CU_Id;
      SCO_Index     : Nat;
      State         : in out CU_Load_State;
      Ignored_Slocs : in out Ignored_Slocs_Sets.Set;
      SCO_Map       : access LL_HL_SCO_Map := null;
      Count_Paths   : Boolean;
      Provider      : SCO_Provider);
   --  Load the low level SCO at SCO_Index into our Internal table, to be part
   --  of the CU compilation unit.
   --
   --  Use and update State according to the semantics of its members. See
   --  Process_Low_Level_SCOs for the semantics of SCO_Map, Count_Paths and
   --  Provider.

   -------------------------------
   -- Main SCO descriptor table --
   -------------------------------

   function To_Statement_Kind (C : Character) return Statement_Kind;
   --  Convert character code for statement kind to corresponding enum value

   function To_Decision_Kind (C : Character) return Decision_Kind;
   --  Convert character code for decision kind to corresponding enum value

   type Operand_Pair is array (Operand_Position) of SCO_Id;

   type SCO_Descriptor (Kind : Any_SCO_Kind := SCO_Kind'First) is record
      case Kind is
      when Removed =>
         null;

      when others =>
         Origin : CU_Id;
         --  Compilation unit whose LI file containing this SCO

         Sloc_Range : Source_Location_Range := No_Range;
         --  For a decision, cumulative range from all conditions

         Parent : SCO_Id := No_SCO_Id;
         --  For a decision, pointer to the enclosing statement (or condition
         --  in the case of a nested decision), unset if decision is part of a
         --  flow control structure.
         --
         --  For a condition or operator, pointer to the enclosing operator, or
         --  to enclosing decision if at top level.

         case Kind is
         when Removed =>
            null;

         when Statement =>
            S_Kind         : Statement_Kind := Statement_Kind'First;
            --  Statement kind indication

            Dominant       : SCO_Id   := No_SCO_Id;
            Dominant_Value : Tristate := Unknown;
            --  Previous statement in sequence, or dominant decision. See
            --  comment for function Dominant. Dominant_Value is Unknown for
            --  a statement dominant, or a valid boolean value for a decision
            --  dominant.

            Dominant_Sloc  : Source_Location := No_Location;
            --  While SCOs are being read, we only get the sloc of the dominant
            --  and store it here. We set the Dominant component later on after
            --  the Sloc -> SCO map has been constructed.

            Handler_Range : Source_Location_Range := No_Range;
            --  Sloc range of the exception handler of which this is the first
            --  statement.

            Pragma_Name : Pragma_Id := Pragma_Id'First;
            --  For a Pragma_Statement, corresponding pragma identifier

         when Condition =>
            Value : Tristate;
            --  Indicates whether this condition is always true, always false,
            --  or tested at run time (Unknown).

            PC_Set : PC_Sets.Set;
            --  Addresses of conditional branches testing this condition
            --  (if Value = Unknown).

            BDD_Node : BDD_Node_Id;
            --  Associated node in the decision's BDD

            Index : Condition_Index;
            --  Index of this condition in the decision

         when Decision =>
            Expression : SCO_Id;
            --  Top expression node for this decision

            D_Kind : Decision_Kind;
            --  Decision kind indication

            Control_Location : Source_Location := No_Location;
            --  For a decision other than an Expression, sloc of the execution
            --  flow control construct.

            Last_Cond_Index : Any_Condition_Index;
            --  Index of last condition in decision (should be > 0 for complex
            --  decisions, = 0 otherwise).

            Decision_BDD : BDD.BDD_Type;
            --  BDD of the decision

            Degraded_Origins : Boolean := False;
            --  Set True for the case of a single-condition decision, whose
            --  conditional branch instructions have origins (i.e. condition
            --  value labels) set modulo an arbitrary negation.

            Aspect_Name : Aspect_Id := No_Aspect;
            --  For an aspect decision, name of the aspect

            Path_Count : Natural := 0;
            --  Count of distinct paths through the BDD from the root condition
            --  to any outcome.

         when Operator =>
            Operands : Operand_Pair := (others => No_SCO_Id);
            --  Operands of this operator

            Op_Kind : Operator_Kind;
            --  Kind of operation this node represents
         end case;
      end case;
   end record;

   pragma Warnings (Off, "* is not referenced");
   procedure Read (S : access Root_Stream_Type'Class; V : out SCO_Descriptor);
   procedure Write (S : access Root_Stream_Type'Class; V : SCO_Descriptor);
   pragma Warnings (On, "* is not referenced");

   for SCO_Descriptor'Read use Read;
   for SCO_Descriptor'Write use Write;

   Removed_SCO_Descriptor : constant SCO_Descriptor := (Kind => Removed);

   package SCO_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Valid_SCO_Id,
        Element_Type => SCO_Descriptor);

   function Next_BDD_Node
     (SCO   : SCO_Id;
      Value : Boolean) return BDD_Node_Id;
   --  Given a Condition SCO and the value of the condition, return the
   --  corresponding target node in the decision's BDD.

   procedure Dump_Decision (SCO : SCO_Id);
   --  Display image of decision in reconstructed expression form (for
   --  debugging purposes).

   function Enclosing (What : SCO_Kind; SCO : SCO_Id) return SCO_Id;
   --  Return the innermost enclosing SCO with the given Kind (if SCO has the
   --  given Kind, returns SCO itself).

   function Nested (Left, Right : SCO_Descriptor) return Boolean;
   --  Return whether R is nested in L, exclusive at boundaries

   function Invalid_Overlap
     (SCOD          : SCO_Descriptor;
      Enclosing_SCO : SCO_Id) return Boolean;
   --  If Enclosing_SCO is No_SCO_Id, return False. Otherwise, assume that
   --  SCOD and Enclosing_SCO share part of their sloc range. Issue a warning
   --  and return True if this is an invalid case of overlap (if both SCOs
   --  overlap without nesting).

   procedure Prealloc_Lines
     (Cur_Source_File : Source_File_Index;
      Last_Line       : in out Natural);
   --  Pre-allocate line table entries for Cur_Source_File to accomodate
   --  Last_Line (optimization only). Last_Line is reset to 0.

   procedure Add_SCO_To_Lines (SCO : SCO_Id; SCOD : SCO_Descriptor);
   --  Link the given SCO and SCOD to the corresponding entries in line tables

   -----------------------
   -- Instance coverage --
   -----------------------

   type Instance_Tag_Provider_Type is new Tag_Provider_Type with null record;

   overriding function Get_Slocs_And_Tags
     (TP : access Instance_Tag_Provider_Type;
      PC : Pc_Type) return Tagged_Slocs;

   overriding function Tag_Name
     (TP  : access Instance_Tag_Provider_Type;
      Tag : SC_Tag) return String;

   overriding function Map_Tag
     (TP     : access Instance_Tag_Provider_Type;
      Relocs : Checkpoint_Relocations;
      CP_Tag : SC_Tag) return SC_Tag;

   package R is new Tag_Providers.Register_Factory
     (Name => "instance", T => Instance_Tag_Provider_Type);
   pragma Unreferenced (R);

   --  Source_Coverage_Vectors holds all SCO-related data. This holder can
   --  contain data loaded from a checkpoint.

   type Source_Coverage_Vectors is record
      CU_Vector           : CU_Info_Vectors.Vector;
      ALI_Annotations     : ALI_Annotation_Maps.Map;
      Inst_Vector         : Inst_Info_Vectors.Vector;
      BDD_Vector          : BDD.BDD_Vectors.Vector;
      SCO_Vector          : SCO_Vectors.Vector;
      Non_Instr_SCOs      : SCO_Sets.Set;
      Non_Instr_MCDC_SCOs : SCO_Sets.Set;
   end record;

   -----------------------------------------
   -- Helper routines for Checkpoint_Load --
   -----------------------------------------

   procedure Checkpoint_Load_Merge_Unit
     (CLS        : access Checkpoint_Load_State;
      CP_Vectors : Source_Coverage_Vectors;
      CP_CU      : CU_Info;
      Real_CU_Id : CU_Id);
   --  Load CU from checkpoint that corresponds to a current unit of interest
   --  whose ID is Real_CU_Id.

   procedure Checkpoint_Load_New_Unit
     (CLS        : access Checkpoint_Load_State;
      CP_Vectors : Source_Coverage_Vectors;
      CP_CU      : in out CU_Info;
      CP_CU_Id   : CU_Id;
      New_CU_Id  : out CU_Id);
   --  Load CU from checkpoint that does not correspond to a current unit of
   --  interest. The newly assigned CU_Id is returned in New_CU_Id.

   procedure Checkpoint_Load_Unit
     (CLS        : access Checkpoint_Load_State;
      CP_Vectors : Source_Coverage_Vectors;
      CP_CU      : in out CU_Info;
      CP_CU_Id   : CU_Id;
      New_CU_Id  : out CU_Id);
   --  Process one compilation unit from a checkpoint.
   --  CP_CU_Id is the CU_Id in the checkpoint.
   --  New_CU_Id is the corresponding CU_Id in the current context, and is
   --  either an already existing CU_Id (if the unit was already known),
   --  or a newly assigned one (if not).

   ------------------
   -- Local tables --
   ------------------

   SC_Vectors : Source_Coverage_Vectors;
   --  Vectors for all currently loaded source coverage information

   CU_Vector : CU_Info_Vectors.Vector renames SC_Vectors.CU_Vector;
   --  Vector of compilation unit info (one entry per LI file). Note that the
   --  Comp_Unit procedure that computes the owning compilation unit of a SCO
   --  assumes that the compilation unit vector is naturally ordered by SCO
   --  ranges. This is true by construction, as we remap SCOs while we load
   --  new units.

   Inst_Vector : Inst_Info_Vectors.Vector renames SC_Vectors.Inst_Vector;
   --  Vector of info for generic instantiations

   BDD_Vector : BDD.BDD_Vectors.Vector renames SC_Vectors.BDD_Vector;
   --  Vector for BDD nodes (one per BDD node, all BDDs considered)

   SCO_Vector : SCO_Vectors.Vector renames SC_Vectors.SCO_Vector;
   --  Vector of high-level Source Coverage Obligations (for all units)

   Non_Instr_SCOs : SCO_Sets.Set renames
     SC_Vectors.Non_Instr_SCOs;
   --  Set of SCOs that were not instrumented, either for stmt or decision.
   --
   --  ??? This should really be part of the SCO_Descriptor record, but doing
   --  so would break backwards compatibility with checkpoints. This set should
   --  be removed once manual serialization of checkpoints/SID files is
   --  implemented.

   Non_Instr_MCDC_SCOs : SCO_Sets.Set renames
     SC_Vectors.Non_Instr_MCDC_SCOs;
   --  Set of decsion SCOs that were not instrumented for MCDC.
   --
   --  ??? Same comment as above.

   -----------
   -- Image --
   -----------

   function Image (SE : Scope_Entity) return String is
   begin
      return
        "Scope for "
        & Ada.Strings.Unbounded.To_String (SE.Name)
        & "[" & Slocs.Image (SE.Sloc)
        & "], identifier at "
        & Get_Simple_Name (SE.Identifier.Decl_SFI)
        & ":" & Img (SE.Identifier.Decl_Line);
   end Image;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Scope_Entities : Scope_Entities_Trees.Tree; Line_Prefix : String := "")
   is
      use Scope_Entities_Trees;
   begin
      for Cur in Scope_Entities.Iterate loop
         declare
            Prefix : constant String :=
              Line_Prefix & (1 .. 2 * (Natural (Depth (Cur)) - 2) => ' ');
            SE     : Scope_Entity renames
              Scope_Entities.Constant_Reference (Cur);
         begin
            Put_Line (Prefix & Image (SE));
            Put_Line (Prefix & "... from " & Image (SE.From));
            Put_Line (Prefix & "    to   " & Image (SE.To));
         end;
      end loop;
   end Dump;

   ---------------------
   -- Scope_Traversal --
   ---------------------

   function Scope_Traversal (CU : CU_Id) return Scope_Traversal_Type is
      Result : Scope_Traversal_Type;
   begin
      if CU = No_CU_Id then
         return No_Scope_Traversal;
      end if;
      Result.It :=
        new Tree_Iterator'(CU_Vector.Reference (CU).Scope_Entities.Iterate);
      Result.Last_SCO := No_SCO_Id;
      Result.Current_SE := Scope_Entities_Trees.No_Element;
      Result.Next_SE := Result.It.First;
      return Result;
   end Scope_Traversal;

   ------------------
   -- Traverse_SCO --
   ------------------

   procedure Traverse_SCO (ST : in out Scope_Traversal_Type; SCO : SCO_Id) is
      use Scope_Entities_Trees;
      Progressed : Boolean := False;
   begin
      ST.Last_SCO := SCO;

      --  Move Next_SE forward in the iterator until we go past the deepest
      --  scope that covers SCO. Update Current_SE along the way.

      while Has_Element (ST.Next_SE) and then Covers_SCO (ST.Next_SE, SCO) loop
         ST.Current_SE := ST.Next_SE;
         ST.Next_SE := ST.It.Next (ST.Next_SE);
         Progressed := True;
      end loop;

      --  If we have not found a more specific scope for SCO, we still may need
      --  to update Current_SE in case the requested SCO is not covered anymore
      --  by Current_SE.

      if not Progressed then
         while Has_Element (ST.Current_SE)
               and then not Covers_SCO (ST.Current_SE, SCO)
         loop
            ST.Current_SE := Parent (ST.Current_SE);
         end loop;
      end if;
   end Traverse_SCO;

   --------------
   -- Last_SCO --
   --------------

   function Last_SCO (ST : Scope_Traversal_Type) return SCO_Id is
   begin
      return ST.Last_SCO;
   end Last_SCO;

   --------------------------
   -- In_Scope_Of_Interest --
   --------------------------

   function In_Scope_Of_Interest (ST : Scope_Traversal_Type) return Boolean is
      use Scope_Entities_Trees;
      Cur : Cursor;
   begin
      --  If no subprogram of interest was requested, consider that they are
      --  all of interest.

      if Subps_Of_Interest.Is_Empty then
         return True;
      end if;

      --  Otherwise, find at least one scope that covers SCO and that is a
      --  subprogram of interest.

      Cur := ST.Current_SE;
      while Has_Element (Cur) loop
         if Subps_Of_Interest.Contains (Element (Cur).Identifier) then
            return True;
         end if;
         Cur := Parent (Cur);
      end loop;
      return False;
   end In_Scope_Of_Interest;

   -----------------
   -- Add_Address --
   -----------------

   procedure Add_Address (SCO : SCO_Id; Address : Pc_Type) is
   begin
      SCO_Vector.Reference (SCO).PC_Set.Include (Address);
   end Add_Address;

   ----------------------
   -- Add_SCO_To_Lines --
   ----------------------

   procedure Add_SCO_To_Lines (SCO : SCO_Id; SCOD : SCO_Descriptor) is
   begin
      for L in SCOD.Sloc_Range.L.First_Sloc.Line
            .. SCOD.Sloc_Range.L.Last_Sloc.Line
      loop
         Add_Line_For_Source_Coverage
           (SCOD.Sloc_Range.Source_File, L, SCO);
      end loop;
   end Add_SCO_To_Lines;

   --------------
   -- Bit_Maps --
   --------------

   function Bit_Maps (CU : CU_Id) return CU_Bit_Maps is
   begin
      return CU_Vector.Reference (CU).Element.Bit_Maps;
   end Bit_Maps;

   -----------------
   -- Has_PP_Info --
   -----------------

   function Has_PP_Info (SCO : SCO_Id) return Boolean is
      CU : constant CU_Id := Comp_Unit (SCO);
   begin
      if CU = No_CU_Id then
         return False;
      end if;
      return CU_Vector.Reference (CU).Element.PP_Info_Map.Contains (SCO);
   end Has_PP_Info;

   -----------------
   -- Get_PP_Info --
   -----------------

   function Get_PP_Info (SCO : SCO_Id) return PP_Info is
      CU : constant CU_Id := Comp_Unit (SCO);
   begin
      return CU_Vector.Reference (CU).Element.PP_Info_Map.Element (SCO);
   end Get_PP_Info;

   --------------------------------
   -- Checkpoint_Load_Merge_Unit --
   --------------------------------

   procedure Checkpoint_Load_Merge_Unit
     (CLS        : access Checkpoint_Load_State;
      CP_Vectors : Source_Coverage_Vectors;
      CP_CU      : CU_Info;
      Real_CU_Id : CU_Id)
   is
      Relocs  : Checkpoint_Relocations renames CLS.Relocations;
      Real_CU : CU_Info renames CU_Vector.Reference (Real_CU_Id).Element.all;
      S       : constant access Root_Stream_Type'Class := CLS.all'Access;
   begin
      --  Here we already have loaded full SCO information for this CU, so
      --  all we need to do is to populate the tables mapping the SCO and
      --  instance IDs for this unit in the checkpoint to their counterparts
      --  in the current context, and merge non-instrumented SCO information
      --  if available.

      --  SCOs

      pragma Assert (CP_CU.Last_SCO - CP_CU.First_SCO
                       =
                     Real_CU.Last_SCO - Real_CU.First_SCO);

      for Old_SCO_Id in CP_CU.First_SCO .. CP_CU.Last_SCO loop
         Set_SCO_Id_Map (Relocs, Old_SCO_Id,
                         Old_SCO_Id
                         + Real_CU.First_SCO
                         - CP_CU.First_SCO);
      end loop;

      --  Instances

      pragma Assert
        (CP_CU.Last_Instance - CP_CU.First_Instance
         =
           Real_CU.Last_Instance - Real_CU.First_Instance);

      for Old_Inst_Id in CP_CU.First_Instance
        .. CP_CU.Last_Instance
      loop
         Set_Inst_Id_Map (Relocs, Old_Inst_Id,
                          Old_Inst_Id
                          + Real_CU.First_Instance
                          - CP_CU.First_Instance);
      end loop;

      --  Has_Code indication

      Real_CU.Has_Code := Real_CU.Has_Code or CP_CU.Has_Code;

      --  Non-Instrumented SCO sets

      if Version_Less (S, Than => 9) then

         --  Nothing to do
         return;
      end if;

      declare
         use SCO_Sets;
         CU_CP_Set : Set;
         CU_Set    : Set;
         Cur       : Cursor;
         Prev      : Cursor;
      begin
         --  First process the non instrumented SCOs for stmt and decision
         --
         --  Since the CU we are loading from the CP is already in the current
         --  execution tables, a SCO is non-instrumented iff it was marked as
         --  non-instrumented in the current execution, as well as in the CP.
         --
         --  We need to take care not to process any SCO outside of the one of
         --  this CU, so we build "local" sets containing only the SCOs in the
         --  current CU to compute the intersection.

         for SCO of CP_Vectors.Non_Instr_SCOs loop
            if SCO /= No_SCO_Id
              and then SCO in CP_CU.First_SCO .. CP_CU.Last_SCO
            then
               CU_CP_Set.Insert (Remap_SCO_Id (Relocs, SCO));
            end if;
         end loop;

         Cur := Non_Instr_SCOs.Ceiling (Real_CU.First_SCO);
         while Has_Element (Cur) and then Element (Cur) <= Real_CU.Last_SCO
         loop
            CU_Set.Insert (Element (Cur));
            Prev := Cur;
            Next (Cur);

            --  Remove the element from the main set so we can insert the
            --  intersection of the CU-specific sets later.

            Non_Instr_SCOs.Delete (Prev);
         end loop;
         Non_Instr_SCOs.Union (CU_Set.Intersection (CU_CP_Set));

         --  Same processing for MCDC SCOs

         CU_Set.Clear;
         CU_CP_Set.Clear;

         for SCO of CP_Vectors.Non_Instr_MCDC_SCOs loop
            if SCO /= No_SCO_Id
              and then SCO in CP_CU.First_SCO .. CP_CU.Last_SCO
            then
               CU_CP_Set.Insert (Remap_SCO_Id (Relocs, SCO));
            end if;
         end loop;

         Cur := Non_Instr_MCDC_SCOs.Ceiling (Real_CU.First_SCO);
         while Has_Element (Cur) and then Element (Cur) <= Real_CU.Last_SCO
         loop
            CU_Set.Insert (Element (Cur));
            Prev := Cur;
            Next (Cur);
            Non_Instr_MCDC_SCOs.Delete (Prev);
         end loop;
         Non_Instr_MCDC_SCOs.Union (CU_Set.Intersection (CU_CP_Set));
      end;
   end Checkpoint_Load_Merge_Unit;

   ------------------------------
   -- Checkpoint_Load_New_Unit --
   ------------------------------

   procedure Checkpoint_Load_New_Unit
     (CLS        : access Checkpoint_Load_State;
      CP_Vectors : Source_Coverage_Vectors;
      CP_CU      : in out CU_Info;
      CP_CU_Id   : CU_Id;
      New_CU_Id  : out CU_Id)
   is
      Relocs : Checkpoint_Relocations renames CLS.Relocations;
      S      : constant access Root_Stream_Type'Class := CLS.all'Access;

      New_First_Instance : Inst_Id;
      New_First_SCO      : SCO_Id;

      Cur_Source_File : Source_File_Index := No_Source_File;
      Last_Line       : Natural := 0;

      procedure Remap_BDD (Decision_BDD : in out BDD.BDD_Type);
      --  Remap a sequence of BDD nodes, for a whole decision BDD

      procedure Remap_BDD_Node (B : in out BDD_Node_Id);
      --  Remap a BDD node id

      procedure Remap_SCO_Id (S : in out SCO_Id);
      --  Remap a SCO_Id. Note: this assumes possible forward references, and
      --  does not rely on SCO_Map.

      ---------------
      -- Remap_BDD --
      ---------------

      procedure Remap_BDD (Decision_BDD : in out BDD.BDD_Type) is
         CP_First  : constant BDD_Node_Id := Decision_BDD.First_Node;
         CP_Last   : constant BDD_Node_Id := Decision_BDD.Last_Node;
         New_First : constant BDD_Node_Id := BDD_Vector.Last_Index + 1;
      begin
         --  Import the relevant BDD nodes from CP_Vectors.BDD_Vector

         for Old_BDD_Node_Id in CP_First .. CP_Last loop
            declare
               --  We are supposed to remap individual BDD nodes only once

               New_BDD_Node : BDD.BDD_Node :=
                 CP_Vectors.BDD_Vector.Element (Old_BDD_Node_Id);

               procedure Remap_BDD_Node_Id (S : in out BDD_Node_Id);
               --  Remap a BDD node id

               -----------------------
               -- Remap_BDD_Node_Id --
               -----------------------

               procedure Remap_BDD_Node_Id (S : in out BDD_Node_Id) is
               begin
                  if S /= No_BDD_Node_Id then
                     S := S - CP_First + New_First;
                  end if;
               end Remap_BDD_Node_Id;

            begin
               case New_BDD_Node.Kind is
                  when BDD.Condition =>
                     Remap_BDD_Node_Id (New_BDD_Node.Parent);
                     for Valuation in New_BDD_Node.Dests'Range loop
                        Remap_BDD_Node_Id
                          (New_BDD_Node.Dests (Valuation));
                     end loop;

                     --  Note that we leave New_BDD_Node.C_SCO unremapped here:
                     --  the loading of the corresponding SCO condition will
                     --  take care of it (see below).

                  when BDD.Jump =>
                     Remap_BDD_Node_Id (New_BDD_Node.Dest);

                  when others =>
                     null;
               end case;

               BDD_Vector.Append (New_BDD_Node);
               Set_BDD_Node_Id_Map
                 (Relocs, Old_BDD_Node_Id, BDD_Vector.Last_Index);
            end;
         end loop;

         --  Remap IDs in Decision_BDD

         Remap_SCO_Id (Decision_BDD.Decision);

         Remap_BDD_Node (Decision_BDD.Root_Condition);
         Remap_BDD_Node (Decision_BDD.First_Node);
         Remap_BDD_Node (Decision_BDD.Last_Node);
         Remap_BDD_Node (Decision_BDD.First_Multipath_Condition);
      end Remap_BDD;

      --------------------
      -- Remap_BDD_Node --
      --------------------

      procedure Remap_BDD_Node (B : in out BDD_Node_Id) is
      begin
         if B /= No_BDD_Node_Id then
            B := Remap_BDD_Node_Id (Relocs, B);
            pragma Assert (B /= No_BDD_Node_Id);
         end if;
      end Remap_BDD_Node;

      ------------------
      -- Remap_SCO_Id --
      ------------------

      procedure Remap_SCO_Id (S : in out SCO_Id) is
      begin
         if S /= No_SCO_Id then
            S := New_First_SCO + S - CP_CU.First_SCO;
            pragma Assert (S /= No_SCO_Id);
         end if;
      end Remap_SCO_Id;

   --  Start of processing for Checkpoint_Load_New_Unit

   begin
      New_CU_Id := CU_Vector.Last_Index + 1;

      CU_Map.Insert (CP_CU.Main_Source, New_CU_Id);

      --  Remap instance ids

      New_First_Instance := Inst_Vector.Last_Index + 1;
      for Old_Inst_Id
        in CP_CU.First_Instance .. CP_CU.Last_Instance
      loop
         Remap_Inst : declare
            New_Inst : Inst_Info :=
              CP_Vectors.Inst_Vector.Element (Old_Inst_Id);

            procedure Remap_Inst_Id (S : in out Inst_Id);
            --  Remap an Inst_Id. Note: this assumes possible
            --  forward references, and does not rely on Inst_Map.

            -------------------
            -- Remap_Inst_Id --
            -------------------

            procedure Remap_Inst_Id (S : in out Inst_Id) is
            begin
               if S /= No_Inst_Id then
                  S := New_First_Instance
                    + S
                    - CP_CU.First_Instance;
               end if;
            end Remap_Inst_Id;

         --  Start of processing for Remap_Inst

         begin
            Remap_SFI (Relocs, New_Inst.Sloc.Source_File);
            Remap_Inst_Id (New_Inst.Enclosing_Instance);
            pragma Assert (New_Inst.Comp_Unit = CP_CU_Id);
            New_Inst.Comp_Unit := New_CU_Id;

            Inst_Vector.Append (New_Inst);
            Set_Inst_Id_Map (Relocs, Old_Inst_Id, Inst_Vector.Last_Index);
         end Remap_Inst;
      end loop;

      --  Remap SCO ids. Note that BDD nodes are imported (and remapped) as
      --  needed during the process.

      New_First_SCO := SCO_Vector.Last_Index + 1;
      for Old_SCO_Id in CP_CU.First_SCO .. CP_CU.Last_SCO loop
         declare
            New_SCOD : SCO_Descriptor :=
              CP_Vectors.SCO_Vector.Element (Old_SCO_Id);
         begin
            if New_SCOD.Kind = Removed then
               Ignore_SCO (Relocs, Old_SCO_Id);
               goto Next_SCO;
            end if;

            New_SCOD.Origin := New_CU_Id;

            --  Remap SFIs in all source locations

            Remap_SFI (Relocs, New_SCOD.Sloc_Range.Source_File);

            --  Preallocate line table entries for previous unit

            if New_SCOD.Sloc_Range.Source_File /= Cur_Source_File then
               Prealloc_Lines (Cur_Source_File, Last_Line);
               Cur_Source_File := New_SCOD.Sloc_Range.Source_File;
               CU_Map.Include (Cur_Source_File, New_CU_Id);
            end if;

            Last_Line := Natural'Max
              (Last_Line,
               New_SCOD.Sloc_Range.L.Last_Sloc.Line);

            --  Remap SCO_Ids

            Remap_SCO_Id (New_SCOD.Parent);

            --  Make further adjustments based on SCO kind
            --  In particular reset all components that reference
            --  data that is not saved to checkpoint files (such as
            --  BDD information).

            case SCO_Kind (New_SCOD.Kind) is
               when Statement =>
                  Remap_SFI (Relocs, New_SCOD.Dominant_Sloc.Source_File);
                  Remap_SFI (Relocs, New_SCOD.Handler_Range.Source_File);

                  Remap_SCO_Id (New_SCOD.Dominant);

               when Decision =>
                  Remap_SCO_Id (New_SCOD.Expression);
                  Remap_SFI (Relocs, New_SCOD.Control_Location.Source_File);
                  Remap_BDD (New_SCOD.Decision_BDD);

               when Operator =>
                  for Op_SCO in New_SCOD.Operands'Range loop
                     Remap_SCO_Id (New_SCOD.Operands (Op_SCO));
                  end loop;

               when Condition =>
                  Remap_BDD_Node (New_SCOD.BDD_Node);
                  Remap_SCO_Id
                    (BDD_Vector.Reference (New_SCOD.BDD_Node).C_SCO);

                  New_SCOD.PC_Set.Clear;

            end case;

            --  Append new SCOD and record mapping

            SCO_Vector.Append (New_SCOD);
            Set_SCO_Id_Map (Relocs, Old_SCO_Id, SCO_Vector.Last_Index);
            if Verbose then
               Put_Line
                 ("Loaded from checkpoint: "
                  & Image (SCO_Vector.Last_Index)
                  & " (was #" & Trim (Old_SCO_Id'Img, Side => Ada.Strings.Both)
                  & " in checkpoint)");
            end if;
         end;

         <<Next_SCO>> null;
      end loop;

      --  Remap SCO_Ids in source trace bit maps

      if CP_CU.Provider = Instrumenter then
         if CP_CU.Bit_Maps.Statement_Bits /= null then
            for S_SCO of CP_CU.Bit_Maps.Statement_Bits.all loop
               Remap_SCO_Id (S_SCO);
            end loop;
         end if;

         if CP_CU.Bit_Maps.Decision_Bits /= null then
            for D_Outcome of CP_CU.Bit_Maps.Decision_Bits.all loop
               Remap_SCO_Id (D_Outcome.D_SCO);
            end loop;
         end if;

         if CP_CU.Bit_Maps.MCDC_Bits /= null then
            for D_Path of CP_CU.Bit_Maps.MCDC_Bits.all loop
               Remap_SCO_Id (D_Path.D_SCO);
            end loop;
         end if;

         --  Remap macro information

         declare
            use SCO_PP_Info_Maps;
            Remapped_PP_Info_Map : SCO_PP_Info_Maps.Map;
         begin
            for Cur in CP_CU.PP_Info_Map.Iterate loop
               declare
                  Info : PP_Info := Element (Cur);
               begin
                  if Info.Kind = In_Expansion then
                     for Expansion of Info.Expansion_Stack loop
                        Remap_SFI
                          (Relocs,
                           Expansion.Sloc.Source_File);
                     end loop;
                     Remap_SFI
                       (Relocs,
                        Info.Definition_Loc.Sloc.Source_File);
                  end if;
                  declare
                     SCO : SCO_Id := Key (Cur);
                  begin
                     Remap_SCO_Id (SCO);
                     Remapped_PP_Info_Map.Insert (SCO, Info);
                  end;
               end;
            end loop;
            CP_CU.PP_Info_Map := Remapped_PP_Info_Map;
         end;

         --  Remap SCOs span for scope entities

         for Scope_Ent of CP_CU.Scope_Entities loop
            Remap_SCO_Id (Scope_Ent.From);
            Remap_SCO_Id (Scope_Ent.To);
            Remap_SFI (Relocs, Scope_Ent.Identifier.Decl_SFI);

            --  Register each scope identifiers to make them available to users
            --  on the command line.

            Available_Subps_Of_Interest.Include (Scope_Ent.Identifier);
         end loop;
         pragma Assert (SCOs_Nested_And_Ordered (CP_CU.Scope_Entities));

      end if;

      --  Preallocate line table entries for last file

      Prealloc_Lines (Cur_Source_File, Last_Line);

      --  Link new SCOs to source line tables

      for SCO in New_First_SCO .. SCO_Vector.Last_Index loop
         declare
            SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
         begin
            if SCOD.Kind in Statement | Decision then
               Add_SCO_To_Lines (SCO, SCOD);
            end if;
         end;
      end loop;

      --  If we have information about non instrumented units, import them as
      --  is. Only import SCOs that belong to this CU, as others SCOs might
      --  belong to a CU already present in the current execution, and which
      --  would not be simply imported as is.

      if not Version_Less (S, Than => 9) then
         for SCO of CP_Vectors.Non_Instr_SCOs loop
            if SCO in CP_CU.First_SCO .. CP_CU.Last_SCO then
               Non_Instr_SCOs.Insert (Remap_SCO_Id (Relocs, SCO));
            end if;
         end loop;

         for SCO of CP_Vectors.Non_Instr_MCDC_SCOs loop
            if SCO in CP_CU.First_SCO .. CP_CU.Last_SCO then
               Non_Instr_MCDC_SCOs.Insert (Remap_SCO_Id (Relocs, SCO));
            end if;
         end loop;
      end if;

      --  Perform final fixups and insert CU

      CP_CU.Last_Instance :=
        New_First_Instance
          + CP_CU.Last_Instance
        - CP_CU.First_Instance;
      CP_CU.First_Instance := New_First_Instance;

      CP_CU.Last_SCO :=
        New_First_SCO
          + CP_CU.Last_SCO
        - CP_CU.First_SCO;
      CP_CU.First_SCO := New_First_SCO;

      CU_Vector.Append (CP_CU);
      Register_CU (New_CU_Id);

      --  If we are loading a SID file, create source coverage data structures.
      --  There is no need to do it when loading a checkpoint: that checkpoint
      --  was created loading a SID file, and thus already has the
      --  corresponding SCI tables populated.

      if CLS.Purpose = Instrumentation then
         Coverage.Source.Initialize_SCI_For_Instrumented_CU (New_CU_Id);
      end if;
   end Checkpoint_Load_New_Unit;

   --------------------------
   -- Checkpoint_Load_Unit --
   --------------------------

   procedure Checkpoint_Load_Unit
     (CLS        : access Checkpoint_Load_State;
      CP_Vectors : Source_Coverage_Vectors;
      CP_CU      : in out CU_Info;
      CP_CU_Id   : CU_Id;
      New_CU_Id  : out CU_Id)
   is
      Relocs : Checkpoint_Relocations renames CLS.Relocations;
   begin
      if CP_CU.Provider = Instrumenter then
         Instrumented_Units_Present := True;
      end if;

      --  Remap source file indices

      Remap_SFI (Relocs, CP_CU.Origin);
      Remap_SFI (Relocs, CP_CU.Main_Source);
      for Dep_SFI of CP_CU.Deps loop

         --  Units of interest can depend on units outside of the
         --  scope of code coverage analysis. Keeping track of these
         --  introduces clashes between stubbed units and the real
         --  one, so they are excluded from checkpoints. Hence, allow
         --  them to be missing here.
         if not SFI_Ignored (Relocs, Dep_SFI) then
            Remap_SFI (Relocs, Dep_SFI);
         end if;
      end loop;

      --  Next check whether this unit is already known

      New_CU_Id := Comp_Unit (CP_CU.Main_Source);

      if Verbose then
         Put_Line ("Remapped CU: id " & New_CU_Id'Img
                   & ", main source" & CP_CU.Main_Source'Img
                   & " " & Get_Full_Name (CP_CU.Main_Source));
      end if;

      --  Case 1: CU not already present. Load all SCO information
      --  from checkpoint.

      if New_CU_Id = No_CU_Id then
         Checkpoint_Load_New_Unit
           (CLS,
            CP_Vectors,
            CP_CU,
            CP_CU_Id  => CP_CU_Id,
            New_CU_Id => New_CU_Id);

      --  Case 2: CU already loaded from LI info. Perform consistency checks,
      --  skipping the checkpointed unit altogether and emitting a warning if
      --  there is a mismatch. Record mapping of checkpoint identifiers (SCOs
      --  and instances) otherwise.

      else
         declare
            CU_Record : CU_Info renames CU_Vector.Reference (New_CU_Id);

            function Provider_Image (Provider : SCO_Provider) return String is
              (case Provider is
               when Compiler     => "ALI file",
               when Instrumenter => "instrumentation");
            --  Helper to designate SCO providers in an error message

            function CU_Image return String is
              (Get_Simple_Name (CP_CU.Origin)
               & " (from " & To_String (CLS.Filename) & ")");
            --  Helper to refer to the compilation unit in an error message

         begin
            --  Ignore CU when the provenance of SCOs is inconsistent

            if CP_CU.Provider /= CU_Record.Provider then
               Warn ("inconsistent coverage method for " & CU_Image);
               Warn ("SCOs for this unit come from both "
                     & Provider_Image (CP_CU.Provider)
                     & " and from " & Provider_Image (CU_Record.Provider));

            --  Ignore also when the fingerprints do not match.
            --
            --  Note that only recent enough SID files contain buffer bit maps
            --  and their fingerprints. Bit_Maps_Fingerprint is left to
            --  No_Fingerprint for checkpoints. Skip the consistency check for
            --  these cases, and if the loaded CU has these fingerprints,
            --  record them for later consistency checks.

            elsif CP_CU.Fingerprint /= CU_Record.Fingerprint
                    or else
                  (CP_CU.Provider = Instrumenter
                   and then CP_CU.Bit_Maps_Fingerprint /= No_Fingerprint
                   and then CU_Record.Bit_Maps_Fingerprint /= No_Fingerprint
                   and then CP_CU.Bit_Maps_Fingerprint
                            /= CU_Record.Bit_Maps_Fingerprint)
            then
               Warn ("unexpected fingerprint, cannot merge coverage"
                     & " information for " & CU_Image);

            else
               if CU_Record.Provider = Instrumenter
                  and then CU_Record.Bit_Maps_Fingerprint = No_Fingerprint
               then
                  CU_Record.Bit_Maps_Fingerprint := CP_CU.Bit_Maps_Fingerprint;
               end if;
               Checkpoint_Load_Merge_Unit
                 (CLS,
                  CP_CU      => CP_CU,
                  CP_Vectors => CP_Vectors,
                  Real_CU_Id => New_CU_Id);
            end if;
         end;
      end if;
   end Checkpoint_Load_Unit;

   ----------
   -- Free --
   ----------

   procedure Free (CU : in out CU_Info) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Statement_Bit_Map, Statement_Bit_Map_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (Decision_Bit_Map, Decision_Bit_Map_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (MCDC_Bit_Map, MCDC_Bit_Map_Access);
   begin
      if CU.Provider = Instrumenter then
         Free (CU.Bit_Maps.Statement_Bits);
         Free (CU.Bit_Maps.Decision_Bits);
         Free (CU.Bit_Maps.MCDC_Bits);
      end if;
   end Free;

   --  Procedures below use comparisons on checkpoint format versions

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Root_Stream_Type'Class;
      V : out CU_Info)
   is
      Provider : constant SCO_Provider :=
        (if Version_Less (S, Than => 2)
         then Compiler else SCO_Provider'Input (S));
      --  Discriminant for v2 data

      New_CUI  : CU_Info (Provider);
      pragma Warnings (Off, New_CUI);
      --  Used only for discriminant and default initialization

   begin
      --  Set CUI's discriminant

      V := New_CUI;

      --  Checkpoint version 1 data

      Source_File_Index'Read (S, V.Origin);
      Source_File_Index'Read (S, V.Main_Source);
      SCO_Id'Read            (S, V.First_SCO);
      SCO_Id'Read            (S, V.Last_SCO);
      Inst_Id'Read           (S, V.First_Instance);
      Inst_Id'Read           (S, V.Last_Instance);
      SFI_Vector'Read        (S, V.Deps);
      Boolean'Read           (S, V.Has_Code);
      Fingerprint_Type'Read  (S, V.Fingerprint);

      --  Checkpoint version 8 preprocessing information

      if not Version_Less (S, Than => 8) then
         SCO_PP_Info_Maps.Map'Read (S, V.PP_Info_Map);
      end if;

      case V.Provider is
         when Compiler =>
            null;
         when Instrumenter =>

            --  Checkpoint version 2 data (instrumentation support)

            --  By default, use "no fingerprint" for buffer bit maps
            --  fingerprints: either the SID file we load is too old to have
            --  such fingerprints, either we are loading a checkpoint, and by
            --  design checkpoints contains neither bit maps nor their
            --  fingerprints (all source traces are processed before loading
            --  checkpoints, and bit maps are needed only to interpret source
            --  traces).

            V.Bit_Maps_Fingerprint := No_Fingerprint;

            if not Version_Less (S, Than => 2)
               and then Purpose_Of (S) = Instrumentation
            then
               V.Bit_Maps.Statement_Bits :=
                 new Statement_Bit_Map'(Statement_Bit_Map'Input (S));
               V.Bit_Maps.Decision_Bits :=
                 new Decision_Bit_Map'(Decision_Bit_Map'Input (S));
               V.Bit_Maps.MCDC_Bits :=
                 new MCDC_Bit_Map'(MCDC_Bit_Map'Input (S));
               if not Version_Less (S, Than => 11) then
                  Fingerprint_Type'Read (S, V.Bit_Maps_Fingerprint);
               end if;
            end if;
      end case;

      --  Checkpoint version 8 data (scoped metrics support)

      if not Version_Less (S, Than => 8) then
         Scope_Entities_Tree'Read (S, V.Scope_Entities);
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Root_Stream_Type'Class;
      V : CU_Info)
   is
   begin
      SCO_Provider'Write (S, V.Provider);

      --  Checkpoint version 1 data

      Source_File_Index'Write (S, V.Origin);
      Source_File_Index'Write (S, V.Main_Source);
      SCO_Id'Write            (S, V.First_SCO);
      SCO_Id'Write            (S, V.Last_SCO);
      Inst_Id'Write           (S, V.First_Instance);
      Inst_Id'Write           (S, V.Last_Instance);
      SFI_Vector'Write        (S, V.Deps);
      Boolean'Write           (S, V.Has_Code);
      Fingerprint_Type'Write  (S, V.Fingerprint);

      SCO_PP_Info_Maps.Map'Write (S, V.PP_Info_Map);

      case V.Provider is
         when Compiler =>
            null;
         when Instrumenter =>
            if Purpose_Of (S) = Instrumentation then
               Statement_Bit_Map'Output
                 (S, V.Bit_Maps.Statement_Bits.all);
               Decision_Bit_Map'Output
                 (S, V.Bit_Maps.Decision_Bits.all);
               MCDC_Bit_Map'Output
                 (S, V.Bit_Maps.MCDC_Bits.all);
               Fingerprint_Type'Write (S, V.Bit_Maps_Fingerprint);
            end if;
      end case;
      Scope_Entities_Tree'Write (S, V.Scope_Entities);
   end Write;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load (CLS : access Checkpoint_Load_State) is
      CP_Vectors : Source_Coverage_Vectors;
      S          : constant access Root_Stream_Type'Class := CLS.all'Access;
      Relocs     : Checkpoint_Relocations renames CLS.Relocations;
   begin
      --  Load data from stream
      --  This part must be kept consistent with Checkpoint_Save

      CU_Info_Vectors.Vector'Read   (S, CP_Vectors.CU_Vector);
      ALI_Annotation_Maps.Map'Read  (S, CP_Vectors.ALI_Annotations);
      Inst_Info_Vectors.Vector'Read (S, CP_Vectors.Inst_Vector);
      BDD.BDD_Vectors.Vector'Read   (S, CP_Vectors.BDD_Vector);
      SCO_Vectors.Vector'Read       (S, CP_Vectors.SCO_Vector);

      --  Load non-instrumented information

      if not Version_Less (S, Than => 9) then
         SCO_Sets.Set'Read (S, CP_Vectors.Non_Instr_SCOs);
         SCO_Sets.Set'Read
           (S, CP_Vectors.Non_Instr_MCDC_SCOs);
      end if;

      --  Allocate mapping tables for SCOs, instance identifiers and BDD nodes

      Allocate_CU_Id_Maps (Relocs,
                           CP_Vectors.CU_Vector.First_Index,
                           CP_Vectors.CU_Vector.Last_Index);
      Allocate_SCO_Id_Map (Relocs,
                           CP_Vectors.SCO_Vector.First_Index,
                           CP_Vectors.SCO_Vector.Last_Index);
      Allocate_Inst_Id_Map (Relocs,
                            CP_Vectors.Inst_Vector.First_Index,
                            CP_Vectors.Inst_Vector.Last_Index);
      Allocate_BDD_Node_Id_Map (Relocs,
                                CP_Vectors.BDD_Vector.First_Index,
                                CP_Vectors.BDD_Vector.Last_Index);

      declare
         Last_Existing_CU_Id : constant CU_Id := CU_Vector.Last_Index;

      begin
         --  Remap and merge into current tables

         for Cur in CP_Vectors.CU_Vector.Iterate loop
            declare
               use CU_Info_Vectors;

               CP_CU_Id  : constant CU_Id := To_Index (Cur);
               CP_CU     : CU_Info := Element (Cur);
               New_CU_Id : CU_Id := No_CU_Id;

               --  If the CU Origin or its Main_Source files are ignored, we
               --  cannot load this CU.

               Origin_Ignored      : constant Boolean :=
                 SFI_Ignored (Relocs, CP_CU.Origin);
               Main_Source_Ignored : constant Boolean :=
                 SFI_Ignored (Relocs, CP_CU.Main_Source);
            begin
               if Origin_Ignored or else Main_Source_Ignored then
                  if Switches.Verbose then
                     Put_Line ("Ignoring CU from SID file: Id" & CP_CU_Id'Img);
                  end if;

                  --  If we cannot load this CU *not* because its main source
                  --  is ignored, but rather because the origin is ignored,
                  --  warn the user: they probably did not want to ignore this
                  --  CU, but we have to in order not to break our data
                  --  structure invariants: Origin cannot be null.

                  if not Main_Source_Ignored then
                     Warn
                       ("gnatcov limitation: ignoring unit "
                        & Get_Simple_Name
                            (Remap_SFI (Relocs, CP_CU.Main_Source))
                        & " from " & To_String (CLS.Filename)
                        & " because "
                        & To_String (Get_Simple_Name (Relocs, CP_CU.Origin))
                        & " is ignored");
                  end if;

                  Ignore_CU_Id (Relocs, CP_CU_Id);

               else
                  Checkpoint_Load_Unit
                    (CLS,
                     CP_Vectors,
                     CP_CU,
                     CP_CU_Id  => CP_CU_Id,
                     New_CU_Id => New_CU_Id);
                  Set_CU_Id_Map (Relocs, CP_CU_Id, New_CU_Id);
               end if;
            end;
         end loop;

         --  Remap annotations

         for Cur in CP_Vectors.ALI_Annotations.Iterate loop
            declare
               use ALI_Annotation_Maps;
               Annotation_Sloc : Source_Location := Key (Cur);
               Annotation      : ALI_Annotation  := Element (Cur);

            begin
               if not CU_Id_Ignored (Relocs, Annotation.CU) then
                  --  If this annotation comes from a compilation unit whose
                  --  data is being imported from this checkpoint (i.e. whose
                  --  CU id is higher than the last existing one upon entry),
                  --  add it now (else it is assumed to be already present in
                  --  the ALI_Annotation map).

                  pragma Assert
                    (Remap_CU_Id (Relocs, Annotation.CU) /= No_CU_Id);
                  Annotation.CU := Remap_CU_Id (Relocs, Annotation.CU);
                  if Annotation.CU > Last_Existing_CU_Id then
                     Remap_SFI (Relocs, Annotation_Sloc.Source_File);
                     ALI_Annotations.Insert (Annotation_Sloc, Annotation);
                  end if;
               end if;
            end;
         end loop;

      end;
   end Checkpoint_Load;

   ----------------------
   -- Checkpoint_Clear --
   ----------------------

   procedure Checkpoint_Clear is
   begin
      for CU of CU_Vector loop
         Free (CU);
      end loop;
      CU_Map.Clear;
      Origin_To_CUs_Map.Clear;
      CU_Vector.Clear;
      ALI_Annotations.Clear;
      Inst_Vector.Clear;
      BDD_Vector.Clear;
      SCO_Vector.Clear;
   end Checkpoint_Clear;

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save (CSS : access Checkpoint_Save_State) is
      S : constant access Root_Stream_Type'Class := CSS.all'Access;
   begin
      CU_Info_Vectors.Vector'Write        (S, CU_Vector);
      ALI_Annotation_Maps.Map'Write       (S, ALI_Annotations);
      Inst_Info_Vectors.Vector'Write      (S, Inst_Vector);
      BDD.BDD_Vectors.Vector'Write        (S, BDD_Vector);
      SCO_Vectors.Vector'Write            (S, SCO_Vector);
      SCO_Sets.Set'Write (S, Non_Instr_SCOs);
      SCO_Sets.Set'Write (S, Non_Instr_MCDC_SCOs);
   end Checkpoint_Save;

   ---------------
   -- Comp_Unit --
   ---------------

   function Comp_Unit (Src_File : Source_File_Index) return CU_Id is
      use CU_Maps;
      Cur : constant Cursor := CU_Map.Find (Src_File);
   begin
      if Cur = CU_Maps.No_Element then
         return No_CU_Id;
      else
         return Element (Cur);
      end if;
   end Comp_Unit;

   ---------------
   -- First_SCO --
   ---------------

   function First_SCO (CU : CU_Id) return SCO_Id is
   begin
      if CU = No_CU_Id then
         return No_SCO_Id;
      else
         return CU_Vector.Constant_Reference (CU).First_SCO;
      end if;
   end First_SCO;

   --------------
   -- Last_SCO --
   --------------

   function Last_SCO (CU : CU_Id) return SCO_Id is
   begin
      if CU = No_CU_Id then
         return No_SCO_Id;
      else
         return CU_Vector.Constant_Reference (CU).Last_SCO;
      end if;
   end Last_SCO;

   ---------------
   -- Condition --
   ---------------

   function Condition (SCO : SCO_Id; Index : Condition_Index) return SCO_Id is
      use BDD;

      SCOD  : SCO_Descriptor renames SCO_Vector.Reference (SCO);
      First : constant BDD_Node_Id := SCOD.Decision_BDD.First_Node;
      Last  : constant BDD_Node_Id := SCOD.Decision_BDD.Last_Node;

      Current_Condition_Index : Any_Condition_Index := No_Condition_Index;

   begin
      --  Find J'th (0-based) condition in decision by scanning the BDD vector

      for J in First .. Last loop
         declare
            BDDN : BDD_Node renames BDD_Vector.Constant_Reference (J);
         begin
            if BDDN.Kind = Condition then
               Current_Condition_Index := Current_Condition_Index + 1;
               if Current_Condition_Index = Index then
                  return C_SCO : constant SCO_Id := BDDN.C_SCO
                  do
                     pragma Assert (Enclosing_Decision (C_SCO) = SCO);
                     pragma Assert (SC_Obligations.Index (C_SCO) = Index);
                     null;
                  end return;
               end if;
            end if;
         end;
      end loop;
      raise Constraint_Error with "condition index out of range";
   end Condition;

   ----------------------
   -- Condition_Values --
   ----------------------

   function Condition_Values
     (SCO        : SCO_Id;
      Path_Index : Natural;
      Outcome    : out Boolean) return Condition_Values_Array
   is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);

      Last_Cond_Index : constant Condition_Index := SCOD.Last_Cond_Index;
      --  Index of last condition in decision

      Node : BDD_Node_Id := SCOD.Decision_BDD.Root_Condition;
      --  Current BDD node

      Tail_Index : Natural := Path_Index;
      --  Path index in the sub-BDD rooted at Node
   begin
      return Result : Condition_Values_Array :=
        (Condition_Index'First .. Last_Cond_Index => Unknown)
      do
         loop
            --  Determine value of current condition based on current node and
            --  path index at that node.

            declare
               use type BDD.BDD_Node_Kind;
               BDDN : BDD.BDD_Node renames BDD_Vector.Reference (Node);
            begin
               if BDDN.Kind = BDD.Outcome then
                  Outcome := BDDN.Decision_Outcome;
                  exit;
               end if;

               declare
                  pragma Assert (BDDN.Kind = BDD.Condition);

                  Cond_Index : constant Condition_Index := Index (BDDN.C_SCO);
                  Cond_Value : constant Boolean :=
                     Tail_Index >= BDDN.Path_Offset;
               begin
                  Result (Cond_Index) := To_Tristate (Cond_Value);
                  if Cond_Value then
                     Tail_Index := Tail_Index - BDDN.Path_Offset;
                  end if;
                  Node := BDDN.Dests (Cond_Value);
               end;
            end;
         end loop;
      end return;
   end Condition_Values;

   ----------------------
   -- Decision_Outcome --
   ----------------------

   function Decision_Outcome (SCO : SCO_Id) return Tristate is
      use BDD;

      SCOD               : SCO_Descriptor renames SCO_Vector.Reference (SCO);
      Reachable_Outcomes : constant Reachability :=
         SCOD.Decision_BDD.Reachable_Outcomes;
   begin
      --  If exactly one outcome is reachable, then decision is always True or
      --  always False, else Unknown.

      return (if Reachable_Outcomes (False) /= Reachable_Outcomes (True)
              then To_Tristate (Reachable_Outcomes (True))
              else Unknown);
   end Decision_Outcome;

   -------------------
   -- Decision_Type --
   -------------------

   function Decision_Type (SCO : SCO_Id) return Decision_Kind
   is
      D : constant SCO_Descriptor := SCO_Vector.Reference (SCO);
   begin
      pragma Assert (D.Kind = Decision);

      return D.D_Kind;
   end Decision_Type;

   ----------------------
   -- Degraded_Origins --
   ----------------------

   function Degraded_Origins (SCO : SCO_Id) return Boolean is
   begin
      return SCO_Vector.Reference (SCO).Degraded_Origins;
   end Degraded_Origins;

   --------------
   -- Dominant --
   --------------

   procedure Dominant
     (SCO     : SCO_Id;
      Dom_SCO : out SCO_Id;
      Dom_Val : out Boolean)
   is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
   begin
      Dom_SCO := SCOD.Dominant;

      if Dom_SCO /= No_SCO_Id and then Kind (Dom_SCO) = Decision then
         Dom_Val := To_Boolean (SCOD.Dominant_Value);
      else
         Dom_Val := False;
      end if;
   end Dominant;

   ----------------------
   -- Expression_Image --
   ----------------------

   function Expression_Image (Op_SCO : SCO_Id) return Unbounded_String is
   begin
      case Kind (Op_SCO) is
         when Condition =>
            return To_Unbounded_String ('C' & Img (Integer (Index (Op_SCO))));

         when Decision =>
            return Expression_Image (SCO_Vector.Reference (Op_SCO).Expression);

         when Operator =>
            declare
               Result : Unbounded_String := To_Unbounded_String ("(");
               Binary : constant Boolean := Op_Kind (Op_SCO) /= Op_Not;
            begin
               for J in Operand_Position'Range loop
                  declare
                     Opnd_SCO : constant SCO_Id := Operand (Op_SCO, J);
                  begin
                     if J = Right then
                        case Op_Kind (Op_SCO) is
                        when Op_Not      => Append (Result, "not ");
                        when Op_And_Then => Append (Result, " and then ");
                        when Op_Or_Else  => Append (Result, " or else ");
                        end case;
                     end if;

                     if Opnd_SCO = No_SCO_Id then
                        pragma Assert (J = Left and then not Binary);
                        null;
                     else
                        pragma Assert (J = Right or else Binary);
                        Append (Result, Expression_Image (Opnd_SCO));
                     end if;
                  end;
               end loop;

               return Result & ')';
            end;

         when others =>
            return To_Unbounded_String
              ("Expected expression SCO kind (Decision, Condition or Operator)"
               & ", but got " & SCO_Kind'Image (Kind (Op_SCO)));
      end case;
   end Expression_Image;

   -------------------
   -- Dump_Decision --
   -------------------

   procedure Dump_Decision (SCO : SCO_Id) is
   begin
      Put_Line ("Reconstructed expression for " & Image (SCO));
      Put_Line (To_String (Expression_Image (SCO)));
   end Dump_Decision;

   ---------------
   -- Enclosing --
   ---------------

   function Enclosing (What : SCO_Kind; SCO : SCO_Id) return SCO_Id is
   begin
      return Result : SCO_Id := SCO do
         while Result /= No_SCO_Id loop
            declare
               SCOD : SCO_Descriptor renames SCO_Vector.Reference (Result);
            begin
               if SCOD.Kind = What then
                  return;
               end if;
               Result := SCOD.Parent;
            end;
         end loop;
      end return;
   end Enclosing;

   ------------
   -- Nested --
   ------------

   function Nested (Left, Right : SCO_Descriptor) return Boolean
   is
      L : Local_Source_Location_Range renames Left.Sloc_Range.L;
      R : Local_Source_Location_Range renames Right.Sloc_Range.L;
   begin
      return L.First_Sloc < R.First_Sloc and then R.Last_Sloc < L.Last_Sloc;
   end Nested;

   ---------------------
   -- Invalid_Overlap --
   ---------------------

   function Invalid_Overlap
     (SCOD          : SCO_Descriptor;
      Enclosing_SCO : SCO_Id) return Boolean is
   begin
      if Enclosing_SCO /= No_SCO_Id then
         declare
            Enclosing_SCOD : SCO_Descriptor renames
              SCO_Vector.Reference (Enclosing_SCO);
         begin
            if not (Nested (SCOD, Enclosing_SCOD)
                    or else Nested (Enclosing_SCOD, SCOD))
            then
               Report
                 (First_Sloc (SCOD.Sloc_Range),
                  "unexpected SCO overlapping with "
                  & Image (Enclosing_SCO)
                  & ", discarding overlapping SCO");
               return True;
            end if;
         end;
      end if;
      return False;
   end Invalid_Overlap;

   ------------------------
   -- Enclosing_Decision --
   ------------------------

   function Enclosing_Decision (SCO : SCO_Id) return SCO_Id is
      pragma Assert (Kind (SCO) = Condition);
   begin
      return Enclosing (Decision, SCO);
   end Enclosing_Decision;

   -------------------------
   -- Enclosing_Statement --
   -------------------------

   function Enclosing_Statement (SCO : SCO_Id) return SCO_Id is
   begin
      return Enclosing (Statement, SCO);
   end Enclosing_Statement;

   ----------------
   -- First_Sloc --
   ----------------

   function First_Sloc (SCO : SCO_Id) return Source_Location
   is
      Sloc : Source_Location :=
        First_Sloc (SCO_Vector.Reference (SCO).Sloc_Range);
   begin
      if Has_PP_Info (SCO) then
         Sloc.L := Get_PP_Info (SCO).Actual_Source_Range.First_Sloc;
      end if;
      return Sloc;
   end First_Sloc;

   ----------------
   -- Get_Origin --
   ----------------

   procedure Get_Origin
     (SCO        : SCO_Id;
      Prev_SCO   : out SCO_Id;
      Prev_Value : out Boolean)
   is
      use BDD;

      SCOD : SCO_Descriptor renames SCO_Vector (SCO);
      BDDN : BDD_Node renames BDD_Vector (SCOD.BDD_Node);
   begin
      if BDDN.Parent = No_BDD_Node_Id then
         Prev_SCO := No_SCO_Id;
      else
         Prev_SCO   := BDD_Vector.Constant_Reference (BDDN.Parent).C_SCO;
         Prev_Value := BDDN.Parent_Value;
      end if;
   end Get_Origin;

   -------------
   -- Get_Tag --
   -------------

   overriding function Get_Slocs_And_Tags
     (TP : access Instance_Tag_Provider_Type;
      PC : Pc_Type) return Tagged_Slocs
   is
      use type Pc_Type;
      use type Interfaces.Unsigned_32;

      Line_Infos : constant Address_Info_Arr :=
        Get_Address_Infos (TP.Current_Subp.Lines, Line_Addresses, PC);

      Tslocs : Tagged_Slocs (1 .. Integer (Line_Infos'Length));
      Last   : Natural := Tslocs'First - 1;

      Global_Instance_Index : Inst_Id;

      CU  : CU_Id renames TP.Current_Subp.Subprogram_CU;
      CUI : CU_Info;
      Has_Instances : Boolean;

   begin
      pragma Assert
        (PC in TP.Current_Routine.Insns.First + TP.Current_Routine.Offset
            .. TP.Current_Routine.Insns.Last  + TP.Current_Routine.Offset);

      if CU /= No_CU_Id then
         CUI := CU_Vector.Constant_Reference (CU);
         Has_Instances := CUI.First_Instance <= CUI.Last_Instance;
      else
         Has_Instances := False;
      end if;

      for Line_Info of Line_Infos loop
         if Line_Info.Last >= Line_Info.First then
            Last := Last + 1;
            Tslocs (Last).Sloc := Line_Info.Sloc;

            --  Discriminator is an instance index if instance table is present
            --  (SCOs loaded) and not empty.

            if Has_Instances and then Line_Info.Disc /= 0 then

               --  Non-zero discriminator found: it is an instance index within
               --  the current compilation unit. Convert it to a global
               --  instance index, and cast to tag.

               Global_Instance_Index :=
                 CUI.First_Instance + Inst_Id (Line_Info.Disc - 1);

               pragma Assert
                 (Global_Instance_Index <= CUI.Last_Instance);

               pragma Assert
                 (Inst_Vector.Constant_Reference
                    (Global_Instance_Index).Comp_Unit
                    = TP.Current_Subp.Subprogram_CU);

               Tslocs (Last).Tag := Valid_SC_Tag (Global_Instance_Index);
            else
               Tslocs (Last).Tag := No_SC_Tag;
            end if;
         end if;
      end loop;
      return Tslocs (Tslocs'First .. Last);
   end Get_Slocs_And_Tags;

   -------------------
   -- Handler_Range --
   -------------------

   function Handler_Range (SCO : SCO_Id) return Source_Location_Range is
      S_SCO : SCO_Id := SCO;
   begin
      while S_SCO /= No_SCO_Id loop
         declare
            SCOD : SCO_Descriptor renames SCO_Vector (S_SCO);
         begin
            if SCOD.Handler_Range /= No_Range then
               return SCOD.Handler_Range;
            end if;
         end;
         S_SCO := Previous (S_SCO);
      end loop;
      return No_Range;
   end Handler_Range;

   -----------------------------
   -- Has_Multipath_Condition --
   -----------------------------

   function Has_Multipath_Condition (SCO : SCO_Id) return Boolean is
   begin
      return SCO_Vector.Reference (SCO).Decision_BDD.First_Multipath_Condition
             /= No_BDD_Node_Id;
   end Has_Multipath_Condition;

   -------------
   -- Has_SCO --
   -------------

   function Has_SCO
     (Sloc_Begin : Source_Location;
      Sloc_End   : Source_Location) return Boolean
   is
      function Has_SCO (Kind : SCO_Kind) return Boolean;
      --  Return if there is at least one SCO of the given Kind whose range has
      --  a non-null intersection with Sloc_Begin .. Sloc_End.

      -------------
      -- Has_SCO --
      -------------

      function Has_SCO (Kind : SCO_Kind) return Boolean is
         use Sloc_To_SCO_Maps;

         Position : Cursor :=
                      Sloc_To_SCO_Map (Sloc_End.Source_File, Kind).Floor
                        ((Sloc_End.L, No_Local_Location));
      begin
         while Position /= No_Element loop
            declare
               SCOD : SCO_Descriptor renames
                  SCO_Vector.Reference (Element (Position));
            begin
               if Sloc_End < First_Sloc (SCOD.Sloc_Range) then

                  --  Negative match, and no chance to have a positive match
                  --  in the next SCOs: they all have a higher First_Sloc.

                  return False;

               elsif Last_Sloc (SCOD.Sloc_Range) < Sloc_Begin then

                  --  Negative match, but we may reach a positive match in
                  --  the next SCO. Continue.

                  null;

               else
                  --  The two possible negative matches have been dealt with
                  --  earlier. We have a positive match.

                  return True;

               end if;
            end;
            Next (Position);
         end loop;

         return False;
      end Has_SCO;

   --  Start of processing for Has_SCO

   begin
      return Has_SCO (Statement) or else Has_SCO (Condition);
   end Has_SCO;

   ----------------
   -- Ignore_SCO --
   ----------------

   function Ignore_SCO (SCO : SCO_Id) return Boolean is
      pragma Assert (Kind (SCO) = Statement);
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
   begin
      --  We ignore remnants of internal processing phases which leave dummy
      --  SCO entries in SCO_Vector, as well as SCOs for pragmas known not to
      --  generate code, as they are not really statements in the Ada sense and
      --  need not be assessed for coverage.

      return
        --  Not a real SCO any more?

        SCOD.Origin = No_CU_Id

        or else

        --  Pragma not generating code?

        (SCOD.S_Kind = Pragma_Statement
           and then not Pragma_Might_Generate_Code (SCOD.Pragma_Name))

        or else

        --  Disabled pragma?

        SCOD.S_Kind = Disabled_Pragma_Statement;

   end Ignore_SCO;

   ----------------------------
   -- Set_Stmt_SCO_Non_Instr --
   ----------------------------

   procedure Set_Stmt_SCO_Non_Instr (SCO : SCO_Id) is
   begin
      Non_Instr_SCOs.Include (SCO);
   end Set_Stmt_SCO_Non_Instr;

   --------------------------------
   -- Set_Decision_SCO_Non_Instr --
   --------------------------------

   procedure Set_Decision_SCO_Non_Instr (SCO : SCO_Id) is
   begin
      Non_Instr_SCOs.Include (SCO);
   end Set_Decision_SCO_Non_Instr;

   -----------------------------------------
   -- Set_Decision_SCO_Non_Instr_For_MCDC --
   -----------------------------------------

   procedure Set_Decision_SCO_Non_Instr_For_MCDC (SCO : SCO_Id) is
   begin
      Non_Instr_MCDC_SCOs.Include (SCO);
   end Set_Decision_SCO_Non_Instr_For_MCDC;

   ---------------------------
   -- Stmt_SCO_Instrumented --
   ---------------------------

   function Stmt_SCO_Instrumented (SCO : SCO_Id) return Boolean is
     (not Non_Instr_SCOs.Contains (SCO));

   -------------------------------
   -- Decision_SCO_Instrumented --
   -------------------------------

   function Decision_SCO_Instrumented (SCO : SCO_Id) return Boolean is
      (not Non_Instr_SCOs.Contains (SCO));

   ----------------------------------------
   -- Decision_SCO_Instrumented_For_MCDC --
   ----------------------------------------

   function Decision_SCO_Instrumented_For_MCDC
     (SCO : SCO_Id) return Boolean  is
     (not Non_Instr_MCDC_SCOs.Contains (SCO));

   -----------
   -- Image --
   -----------

   function Image (SCO : SCO_Id; With_Sloc : Boolean := True) return String is

      function Op_Kind_Image return String;
      --  For an operator SCO, image of the operator kind

      function Sloc_Image (Sloc_Range : Source_Location_Range) return String;
      --  Return sloc information suffix, or empty string if no sloc known,
      --  or if no sloc information is desired.

      -------------------
      -- Op_Kind_Image --
      -------------------

      function Op_Kind_Image return String is
      begin
         if Kind (SCO) = Operator then
            return ' ' & Operator_Kind'Image (Op_Kind (SCO));
         else
            return "";
         end if;
      end Op_Kind_Image;

      ----------------
      -- Sloc_Image --
      ----------------

      function Sloc_Image (Sloc_Range : Source_Location_Range) return String is
      begin
         if Sloc_Range.L.First_Sloc = No_Local_Location
           or else not With_Sloc
         then
            return "";
         else
            return " at " & Image (Sloc_Range);
         end if;
      end Sloc_Image;

   --  Start of processing for Image

   begin
      if SCO = No_SCO_Id then
         return "<no SCO>";
      else
         declare
            SCOD : constant SCO_Descriptor := SCO_Vector (SCO);
         begin
            return "SCO #" & Trim (SCO'Img, Side => Ada.Strings.Both) & ": "
              & SCO_Kind'Image (SCOD.Kind)
              & Op_Kind_Image
              & Sloc_Image (SCOD.Sloc_Range);
         end;
      end if;
   end Image;

   ---------------
   -- Comp_Unit --
   ---------------

   function Comp_Unit (SCO : SCO_Id) return CU_Id is
      LB, UB, Middle : Valid_CU_Id;
   begin
      --  Assume that compilation units in CU_Vector are ordered by SCO range
      --  to look up efficiently (by dichotomy) the compilation unit for the
      --  SCO.

      LB := CU_Vector.First_Index;
      UB := CU_Vector.Last_Index;
      while LB <= UB loop
         Middle := LB + (UB - LB) / 2;
         declare
            CU : CU_Info renames CU_Vector.Constant_Reference (Middle);
         begin
            if SCO in CU.First_SCO .. CU.Last_SCO then
               return Middle;
            elsif SCO < CU.First_SCO then
               UB := Middle - 1;
            else
               LB := Middle + 1;
            end if;
         end;
      end loop;
      return No_CU_Id;
   end Comp_Unit;

   -----------
   -- Index --
   -----------

   function Index (SCO : SCO_Id) return Condition_Index is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
   begin
      pragma Assert (SCOD.Kind = Condition);
      return SCOD.Index;
   end Index;

   -----------------
   -- Register_CU --
   -----------------

   procedure Register_CU (CU : CU_Id) is
      Origin   : constant Source_File_Index := CU_Vector.Reference (CU).Origin;
      Cur      : Origin_To_CUs_Maps.Cursor :=
         Origin_To_CUs_Map.Find (Origin);
      Inserted : Boolean;
   begin
      if not Origin_To_CUs_Maps.Has_Element (Cur) then
         Origin_To_CUs_Map.Insert (Origin, CU_Sets.Empty_Set, Cur, Inserted);
         pragma Assert (Inserted);
      end if;

      Origin_To_CUs_Map.Reference (Cur).Insert (CU);
   end Register_CU;

   ------------------
   -- Instance_Loc --
   ------------------

   function Instance_Loc (Inst_Index : Inst_Id) return String
   is
      II : Inst_Info renames Inst_Vector.Constant_Reference (Inst_Index);
   begin
      return
        Image (II.Sloc)
          & (if II.Enclosing_Instance = No_Inst_Id
             then ""
             else " [" & Instance_Loc (II.Enclosing_Instance) & "]");
   end Instance_Loc;

   ------------------
   -- Is_Assertion --
   ------------------

   function Is_Assertion (SCO : SCO_Id) return Boolean is
      SCOD : SCO_Descriptor renames SCO_Vector (SCO);
   begin
      pragma Assert (SCOD.Kind = Decision);
      case SCOD.D_Kind is
         when Pragma_Decision =>
            --  False for pragma Debug, True for all others (i.e. Assert,
            --  Pre/Postcondition, Check). Note: the pragma name is stored
            --  in the enclosing statement SCO.

            return SCO_Vector (Enclosing_Statement (SCO)).Pragma_Name
              /= Pragma_Debug;

         when Aspect =>
            --  Always True for aspects (Pre/Post/Predicate/Invariant)

            return True;

         when others =>
            return False;
      end case;

   end Is_Assertion;

   ---------------------------
   -- Is_Assertion_To_Cover --
   ---------------------------

   function Is_Assertion_To_Cover (SCO : SCO_Id) return Boolean
   is
      function Is_Pragma_Stmt_To_Cover (SCOD : SCO_Descriptor) return Boolean;
      --  True if the pragma statement of SCOD belongs to the list of pragmas
      --  supported by assertion coverage.

      function Is_Pragma_Stmt_To_Cover (SCOD : SCO_Descriptor) return Boolean
      is
      begin
         pragma Assert
           (SCOD.Kind = Statement and then SCOD.S_Kind = Pragma_Statement);

         return SCOD.Pragma_Name in
           Pragma_Assert
            | Pragma_Assert_And_Cut
            | Pragma_Assume
            | Pragma_Check
            | Pragma_Loop_Invariant
            | Pragma_Type_Invariant
            | Pragma_Precondition
            | Pragma_Postcondition;
      end Is_Pragma_Stmt_To_Cover;

      SCOD : SCO_Descriptor renames SCO_Vector (SCO);
   begin
      if SCOD.Kind = Statement and then SCOD.S_Kind = Pragma_Statement then
         return Is_Pragma_Stmt_To_Cover (SCOD);

      elsif SCOD.Kind = Decision then
         case SCOD.D_Kind is
         when Pragma_Decision =>
            return
              Is_Pragma_Stmt_To_Cover (SCO_Vector (Enclosing_Statement (SCO)));

         when Aspect =>
            return SCOD.Aspect_Name in
              Aspect_Type_Invariant
            | Aspect_Pre
            | Aspect_Post;

         when others =>
            return False;
         end case;

      elsif SCOD.Kind = Condition then
         return Is_Assertion_To_Cover (Enclosing_Decision (SCO));

      else
         return False;
      end if;
   end Is_Assertion_To_Cover;

   -------------------
   -- Is_Expression --
   -------------------

   function Is_Expression (SCO : SCO_Id) return Boolean is
      D_Kind : Decision_Kind;
      S_SCO  : SCO_Id;
   begin
      pragma Assert (Kind (SCO) = Decision);

      --  Check for expression outside of control structure

      D_Kind := SCO_Vector.Reference (SCO).D_Kind;
      if D_Kind = Expression then
         return True;
      end if;

      --  Check for pragma Assert/Check/Pre/Post

      if D_Kind /= Pragma_Decision then
         return False;
      end if;

      S_SCO := Enclosing_Statement (SCO);
      if S_SCO = No_SCO_Id then
         return False;
      end if;

      declare
         S_SCOD : SCO_Descriptor renames SCO_Vector.Reference (S_SCO);
      begin
         --  Return whether S_SCOD is a pragma Assert/Check/Pre/Post

         return (S_SCOD.S_Kind = Disabled_Pragma_Statement
                 or else S_SCOD.S_Kind = Pragma_Statement)
           and then S_SCOD.Pragma_Name in
             Pragma_Assert
               | Pragma_Check
               | Pragma_Precondition
               | Pragma_Postcondition
               | Pragma_Loop_Invariant;
      end;
   end Is_Expression;

   ----------------------
   -- Is_If_Expression --
   ----------------------

   function Is_If_Expression (SCO : SCO_Id) return Boolean is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
      pragma Assert (SCOD.Kind = Decision);
      Enclosing_S_SCO : constant SCO_Id := Enclosing_Statement (SCO);
   begin
      return SCOD.D_Kind = If_Statement
        and then not
          (Enclosing_S_SCO /= No_SCO_Id
             and then
           S_Kind (Enclosing_S_SCO) = If_Statement
             and then
           First_Sloc (Enclosing_S_SCO) = SCOD.Control_Location);
   end Is_If_Expression;

   ------------------------------
   -- Is_Quantified_Expression --
   ------------------------------

   function Is_Quantified_Expression (SCO : SCO_Id) return Boolean is
      SCOD            : SCO_Descriptor;
      Enclosing_C_SCO : SCO_Id;
      Next_SCO        : SCO_Id := SCO + 1;
   begin
      --  Condition SCOS don't discriminate quantified expression specifically.
      --  They are nevertheless characterized by the mandatory presence of a
      --  'W' decision sub-SCO for their predicate.
      --
      --  We don't have direct links to sub-SCOS. We have enclosing-SCO links
      --  still, and know that sub-SCOs are listed after their parent.
      --
      --  Iterate to see if we find a sub-SCO of our SCO argument (according
      --  to the enclosing-SCO links) that is a 'W' decision. If the
      --  immediately enclosing condition of that decision corresponds to the
      --  SCO argument, then it means that the SCO argument is a quantified
      --  expression.

      while Next_SCO <= Last_SCO loop
         SCOD := SCO_Vector.Reference (Next_SCO);

         if SCOD.Kind = Decision and then SCOD.D_Kind = While_Loop then

            --  We found a quantified expression. Now check whether it
            --  corresponds to the given SCO.

            Enclosing_C_SCO := Enclosing (Condition, SCO);

            if Enclosing_C_SCO = SCO then
               return True;
            end if;

         --  We reached the end of the decision

         elsif SCOD.Kind = Statement then
            return False;
         end if;
         Next_SCO := Next_SCO + 1;
      end loop;

      return False;
   end Is_Quantified_Expression;

   ----------------------------------
   -- Is_Pragma_Pre_Post_Condition --
   ----------------------------------

   function Is_Pragma_Pre_Post_Condition (SCO : SCO_Id) return Boolean is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
      pragma Assert (SCOD.Kind = Statement);
   begin
      return SCOD.S_Kind in Pragma_Statement | Disabled_Pragma_Statement
        and then
          SCOD.Pragma_Name in Pragma_Precondition | Pragma_Postcondition;
   end Is_Pragma_Pre_Post_Condition;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (P : access procedure (SCO : SCO_Id)) is
   begin
      for J in SCO_Vector.First_Index .. SCO_Vector.Last_Index loop
         P (J);
      end loop;
   end Iterate;

   ----------
   -- Kind --
   ----------

   function Kind (SCO : SCO_Id) return Any_SCO_Kind is
   begin
      return SCO_Vector.Reference (SCO).Kind;
   end Kind;

   ---------------------
   -- Last_Cond_Index --
   ---------------------

   function Last_Cond_Index (SCO : SCO_Id) return Condition_Index is
   begin
      pragma Assert (Kind (SCO) = Decision);
      return SCO_Vector (SCO).Last_Cond_Index;
   end Last_Cond_Index;

   --------------
   -- Last_SCO --
   --------------

   function Last_SCO return SCO_Id is
     (SCO_Vector.Last_Index);

   ----------------------------
   -- Has_Instrumented_Units --
   ----------------------------

   function Has_Instrumented_Units return Boolean is
   begin
      return Instrumented_Units_Present;
   end Has_Instrumented_Units;

   ---------------
   -- Last_Sloc --
   ---------------

   function Last_Sloc (SCO : SCO_Id) return Source_Location
   is
      Sloc : Source_Location :=
        Last_Sloc (SCO_Vector.Reference (SCO).Sloc_Range);
   begin
      if Has_PP_Info (SCO) then
         Sloc.L := Get_PP_Info (SCO).Actual_Source_Range.Last_Sloc;
      end if;
      return Sloc;
   end Last_Sloc;

   -----------
   -- Image --
   -----------

   function Image (CU : CU_Id) return String is
   begin
      return
        (if CU = No_CU_Id
         then "No CU"
         else "CU "
              & Get_Full_Name (CU_Vector.Constant_Reference (CU).Main_Source));
   end Image;

   -------------
   -- Last_CU --
   -------------

   function Last_CU return CU_Id is
   begin
      return CU_Vector.Last_Index;
   end Last_CU;

   --------------
   -- Provider --
   --------------

   function Provider (CU : CU_Id) return SCO_Provider is
   begin
      return CU_Vector.Reference (CU).Provider;
   end Provider;

   -----------------
   -- Fingerprint --
   -----------------

   function Fingerprint (CU : CU_Id) return Fingerprint_Type is
   begin
      return CU_Vector.Reference (CU).Fingerprint;
   end Fingerprint;

   --------------------------
   -- Bit_Maps_Fingerprint --
   --------------------------

   function Bit_Maps_Fingerprint (CU : CU_Id) return Fingerprint_Type is
   begin
      return CU_Vector.Reference (CU).Bit_Maps_Fingerprint;
   end Bit_Maps_Fingerprint;

   ---------------
   -- Load_SCOs --
   ---------------

   procedure Load_SCOs
     (ALI_Filename         : String;
      Ignored_Source_Files : access GNAT.Regexp.Regexp)
   is
      Units, Deps : SFI_Vector;
      --  Units and dependencies of this compilation

      Created_Units : Created_Unit_Maps.Map;
      Main_Source   : Source_File_Index;

      Temp_ALI_Annotations : ALI_Annotation_Maps.Map;

      ALI_Index : constant Source_File_Index := Load_ALI
        (ALI_Filename, Ignored_Source_Files, Units, Deps,
         Temp_ALI_Annotations, With_SCOs => True);
      --  Load ALI file and update the last SCO and instance indices

   begin
      if ALI_Index = No_Source_File then
         return;
      end if;

      --  Determine main source name. This is the name for the file denoted
      --  by the first U line. For Ada we keep the simple name. For C we try
      --  to get the full name by scanning the D lines, if available.

      Main_Source := Units.First_Element;
      declare
         Main_Source_Simple_Name : constant String :=
           Get_Simple_Name (Main_Source);
      begin
         for Dep of Deps loop
            if Get_Simple_Name (Dep) = Main_Source_Simple_Name then
               Main_Source := Dep;

               --  Files designated by D lines are loaded as stub files. Here,
               --  we decide to treat one as a source file, so make sure we
               --  have registered it as one.

               Consolidate_File_Kind (Main_Source, Source_File);
               exit;
            end if;
         end loop;
      end;

      --  Make sure that Main_Source is the main source of only one LI file

      if Get_File (Main_Source).LI /= No_Source_File then
         Outputs.Fatal_Error
           ("error: the following source file:"
            & ASCII.LF & "  " & Get_Full_Name (Main_Source, True)
            & ASCII.LF & "appears as the main source file in:"
            & ASCII.LF & "  " & ALI_Filename
            & ASCII.LF & "  "
            & Get_Full_Name (Get_File (Main_Source).LI, True)
            & ASCII.LF & "Is the same ALI file provided twice?");
      end if;

      --  Check whether this unit is already known. If not, we can finally
      --  allocate a compilation unit record for it.

      Process_Low_Level_SCOs
        (Provider      => Compiler,

         --  For compiler-provided units, the origin of SCO information is the
         --  ALI file.

         Origin        => ALI_Index,

         Deps          => Deps,
         Created_Units => Created_Units,

         --  An ALI file is involved: we are in binary traces mode, and so
         --  there is no need to compute the number of BDD execution paths for
         --  decisions.
         Count_Paths   => False);

      --  For the units we successfully loaded, copy annotations from the ALI
      --  file to our internal table, filling in the compilation unit.

      for Cur in Temp_ALI_Annotations.Iterate loop
         declare
            A : ALI_Annotation := ALI_Annotation_Maps.Element (Cur);

            --  This annotation comes from a specific source file. Check if
            --  there is a CU that we just created for that source file.

            S      : constant Source_Location := ALI_Annotation_Maps.Key (Cur);
            CU_Cur : constant Created_Unit_Maps.Cursor :=
               Created_Units.Find (S.Source_File);
         begin
            if Created_Unit_Maps.Has_Element (CU_Cur) then
               A.CU := Created_Unit_Maps.Element (CU_Cur);
               ALI_Annotations.Insert (S, A);
            end if;
         end;
      end loop;

      --  Initialize the relevant ".LI" and ".Main_Source" fields in the files
      --  table.

      for Cur in Created_Units.Iterate loop
         declare
            Source_File : constant Source_File_Index :=
               Created_Unit_Maps.Key (Cur);
         begin
            Get_File (Source_File).LI := ALI_Index;
         end;
      end loop;

      declare
         FE : File_Info renames Get_File (ALI_Index).all;
      begin
         if FE.Main_Source = No_Source_File then
            FE.Main_Source := Main_Source;
         end if;
      end;
   end Load_SCOs;

   ----------
   -- Free --
   ----------

   procedure Free (Infos : in out CU_Load_Info_Vectors.Vector) is
      procedure Free is new Ada.Unchecked_Deallocation
        (CU_Load_Info, CU_Load_Info_Access);
   begin
      for Info of Infos loop
         Free (Info);
      end loop;
      Infos.Clear;
   end Free;

   ---------------------
   -- Main_Source_For --
   ---------------------

   function Main_Source_For
     (Unit : SCOs.SCO_Unit_Table_Entry;
      Deps : SFI_Vector) return Source_File_Index
   is
      Result : Source_File_Index;
   begin
      --  No dependency number => internal encoding to mean that we must ignore
      --  this source file.

      if Unit.Dep_Num = SCOs.Missing_Dep_Num then
         return No_Source_File;
      end if;

      Result :=
        (if Deps.Is_Empty

         --  For C, old compilers did not provide a proper deps table: in that
         --  case, fallback on the inlined file name.

         then Get_Index_From_Generic_Name
                (Name                => Unit.File_Name.all,
                 Kind                => Source_File,
                 Indexed_Simple_Name => True)

         --  Get source file name from deps table

         else Deps.Element (Unit.Dep_Num));

      --  We are going to add coverage obligations for this file, so mark it as
      --  a Source_File in the file table, and mark it as never ignored.

      Consolidate_File_Kind (Result, Source_File);
      Consolidate_Ignore_Status (Result, Never);
      return Result;
   end Main_Source_For;

   ----------------------------
   -- Append_For_Fingerprint --
   ----------------------------

   procedure Append_For_Fingerprint
     (Unit_Info : in out CU_Load_Info; S : String)
   is
   begin
      GNAT.SHA1.Update (Unit_Info.Fingerprint_Context, S);
      if Verbose then
         Append (Unit_Info.Fingerprint_Buffer, S);
      end if;
   end Append_For_Fingerprint;

   procedure Append_For_Fingerprint
     (Unit_Info : in out CU_Load_Info; Sloc : SCOs.Source_Location)
   is
   begin
      Append_For_Fingerprint
        (Unit_Info,
         ":" & Logical_Line_Number'Image (Sloc.Line)
         & ":" & Column_Number'Image (Sloc.Col));
   end Append_For_Fingerprint;

   ------------------------
   -- Build_CU_Load_Info --
   ------------------------

   procedure Build_CU_Load_Info
     (Infos : out CU_Load_Info_Vectors.Vector;
      Deps  : SFI_Vector := SFI_Vectors.Empty_Vector)
   is
      package Unit_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type     => Source_File_Index,
         Element_Type => CU_Load_Info_Access);
      Units : Unit_Maps.Map;
      --  Map source file indexes to corresponding units in Infos
   begin
      for Unit_Index in 1 .. SCOs.SCO_Unit_Table.Last loop
         declare
            Source_File : Source_File_Index;
            Unit        : SCOs.SCO_Unit_Table_Entry renames
               SCOs.SCO_Unit_Table.Table (Unit_Index);
            Cur         : Unit_Maps.Cursor;
            Unit_Info   : CU_Load_Info_Access;
         begin

            --  Process each low level unit, except the ones whose source file
            --  must be ignored.

            Source_File := Main_Source_For (Unit, Deps);
            if Source_File /= No_Source_File then

               --  Lookup or create a CU_Load_Info record for this source file

               Cur := Units.Find (Source_File);
               if Unit_Maps.Has_Element (Cur) then
                  Unit_Info := Unit_Maps.Element (Cur);
               else
                  Unit_Info := new CU_Load_Info'
                    (File_Name_Ptr       => Unit.File_Name,
                     Source_File         => Source_File,
                     Entries             => <>,
                     Fingerprint_Context => <>,
                     Fingerprint_Buffer  => <>);
                  Infos.Append (Unit_Info);
                  Units.Insert (Source_File, Unit_Info);

                  --  Start  the computation of the fingerprint for this unit
                  --  with the name of this unit.

                  Append_For_Fingerprint
                    (Unit_Info.all, Get_File (Source_File).Simple_Name.all);
               end if;

               --  Plan to load the range of SCO entries for this low level
               --  unit.

               if Unit.From <= Unit.To then
                  Unit_Info.Entries.Append (Nat_Range'(Unit.From, Unit.To));
               end if;

               --  Make data for SCO entries contribute to the computation of
               --  the fingerprint.

               for SCO_Index in Unit.From .. Unit.To loop
                  declare
                     E : SCOs.SCO_Table_Entry renames
                        SCOs.SCO_Table.Table (SCO_Index);
                  begin
                     Append_For_Fingerprint (Unit_Info.all, E.From);
                     Append_For_Fingerprint (Unit_Info.all, E.To);
                     Append_For_Fingerprint (Unit_Info.all,
                                             String'((E.C1, E.C2)));
                     if E.Last then
                        Append_For_Fingerprint (Unit_Info.all, "Last");
                     end if;

                     --  Directly streaming E.Pragma_Aspect_Name (an enumerated
                     --  type) to the hash stream would make the fingerprint
                     --  computation depend on the enum representation, and
                     --  thus the fingerprint could change when adding a new
                     --  pragma. Instead, use human-readable (and thus stable)
                     --  values.

                     if E.Pragma_Aspect_Name /= No_Name then
                        Append_For_Fingerprint
                          (Unit_Info.all,
                           Get_Name_String (E.Pragma_Aspect_Name));
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;

      --  If requested, show the string used to compute fingerprint for each
      --  unit.

      if Verbose then
         for Unit_Info of Infos loop
            Put_Line ("Computing fingerprint for "
                      & Unit_Info.File_Name_Ptr.all & " SCOs from:");
            Put_Line ("BEGIN ...");
            Put_Line (To_String (Unit_Info.Fingerprint_Buffer));
            Put_Line ("... END");
         end loop;
      end if;
   end Build_CU_Load_Info;

   -----------------
   -- Allocate_CU --
   -----------------

   function Allocate_CU
     (Provider            : SCO_Provider;
      Origin, Main_Source : Source_File_Index;
      Fingerprint         : Fingerprint_Type;
      Created_Units       : in out Created_Unit_Maps.Map) return CU_Id
   is
      use Created_Unit_Maps;

      New_CU_Info : CU_Info (Provider);

      CU_Index : constant CU_Id := Comp_Unit (Main_Source);
      Cur      : constant Cursor := Created_Units.Find (Main_Source);
   begin
      --  Check whether there is already a compilation unit for this main
      --  source.

      if CU_Index /= No_CU_Id then

         --  If we already have a compilation unit for Main_Source and that we
         --  actually created it while loading Origin, just return the existing
         --  CU to signify to the caller that we must proceed with the loading.

         if Has_Element (Cur) then
            return CU_Index;
         end if;

         --  However, if the existing CU was created for another origin, check
         --  if it has the same fingerprint: in that case, we can just ignore
         --  this duplicate, and return no CU to signify to the caller that we
         --  must abort the loading.

         if CU_Vector.Reference (CU_Index).Fingerprint = Fingerprint then
            return No_CU_Id;
         end if;

         --  We reach this point if there is a fingerprint mismatch: warn the
         --  user that we are ignoring this unit, as we were probably passed
         --  inconsistent inputs.

         declare
            CU : CU_Info renames CU_Vector.Reference (CU_Index);

            --  Unless the two origins are homonyms, designate origin files
            --  with their base names.

            Old_Origin      : constant String := Get_Simple_Name (CU.Origin);
            New_Origin      : constant String := Get_Simple_Name (Origin);
            Old_Origin_Full : constant String :=
              (if Old_Origin = New_Origin
               then Get_Full_Name (CU.Origin)
               else Old_Origin);
            New_Origin_Full : constant String :=
              (if Old_Origin = New_Origin
               then Get_Full_Name (Origin)
               else New_Origin);

            Origin_Action : constant String :=
              (case CU.Provider is
               when Compiler => "loading",
               when Instrumenter => "instrumenting");
         begin
            Warn ("ignoring duplicate SCOs for "
                  & Get_Simple_Name (Main_Source)
                  & " (from " & New_Origin_Full & ")");
            Warn ("previous SCOs for this unit came from "
                  & Origin_Action & " " & Old_Origin_Full);
            return No_CU_Id;
         end;
      end if;

      --  Initialize the new CU and register it where needed

      New_CU_Info.Origin := Origin;
      New_CU_Info.Main_Source := Main_Source;
      New_CU_Info.First_SCO := Valid_SCO_Id'First;
      New_CU_Info.Last_SCO := No_SCO_Id;
      New_CU_Info.First_Instance := Valid_Inst_Id'First;
      New_CU_Info.Last_Instance := No_Inst_Id;
      New_CU_Info.Fingerprint := Fingerprint;
      CU_Vector.Append (New_CU_Info);

      return Result : constant CU_Id := CU_Vector.Last_Index do

         --  Register Result in Created_Units

         Created_Units.Insert (Main_Source, Result);

         --  Register Result in Origin_To_CUs_Map

         Register_CU (Result);

         --  Record the main source/CU mapping, used for consolidation
         --
         --  Note: for C files, the same source file may be encountered several
         --  times, hence the use of Include rather than Insert.

         CU_Map.Include (Main_Source, Result);
      end return;
   end Allocate_CU;

   -----------------------------
   -- Process_Low_Level_Entry --
   -----------------------------

   procedure Process_Low_Level_Entry
     (CU            : CU_Id;
      SCO_Index     : Nat;
      State         : in out CU_Load_State;
      Ignored_Slocs : in out Ignored_Slocs_Sets.Set;
      SCO_Map       : access LL_HL_SCO_Map := null;
      Count_Paths   : Boolean;
      Provider      : SCO_Provider)
   is
      Unit : CU_Info renames CU_Vector.Reference (CU);
      SCOE : SCOs.SCO_Table_Entry renames SCOs.SCO_Table.Table (SCO_Index);

      function Make_Condition_Value return Tristate;
      --  Map condition value code (t/f/c) in SCOE.C2 to Tristate

      function New_Operator_SCO (Kind : Operator_Kind) return SCO_Id;
      --  Allocate a new SCO for an operator

      procedure Update_Decision_BDD (SCOD : in out SCO_Descriptor);
      --  Set BDD of decision to Current_BDD

      procedure Update_Decision_Sloc (SCOD : in out SCO_Descriptor);
      --  Update the slocs of a decision SCOD from those of the condition in
      --  the current SCOE.

      function Make_Sloc
        (SCO_Source_Loc : SCOs.Source_Location)
         return Local_Source_Location;
      --  Build a Slocs.Source_Location record from the low-level SCO Sloc info

      function Add_SCO (SCOD : SCO_Descriptor) return SCO_Id;
      --  Add a high level SCO descriptor for this low level SCO

      -------------
      -- Add_SCO --
      -------------

      function Add_SCO (SCOD : SCO_Descriptor) return SCO_Id is
         New_SCO : SCO_Id;
      begin
         SCO_Vector.Append (SCOD);
         New_SCO := SCO_Vector.Last_Index;
         if SCO_Map /= null then
            SCO_Map (SCO_Index) := New_SCO;
         end if;
         return New_SCO;
      end Add_SCO;

      ---------------
      -- Make_Sloc --
      ---------------

      function Make_Sloc
        (SCO_Source_Loc : SCOs.Source_Location)
         return Local_Source_Location
      is
         use type SCOs.Source_Location;
      begin
         if SCO_Source_Loc = SCOs.No_Source_Location then
            return No_Local_Location;
         end if;

         return
           (Line   => Natural (SCO_Source_Loc.Line),
            Column => Natural (SCO_Source_Loc.Col));
      end Make_Sloc;

      From_Sloc : constant Local_Source_Location := Make_Sloc (SCOE.From);
      To_Sloc   : constant Local_Source_Location := Make_Sloc (SCOE.To);
      SCO_Range : constant Source_Location_Range :=
                    (Unit.Main_Source, (From_Sloc, To_Sloc));

      --------------------------
      -- Make_Condition_Value --
      --------------------------

      function Make_Condition_Value return Tristate is
      begin
         case SCOE.C2 is
            when 'f' => return False;
            when 't' => return True;
            when 'c' => return Unknown;

            when others => raise Program_Error with
                 "invalid SCO condition value code: " & SCOE.C2;
         end case;
      end Make_Condition_Value;

      ----------------------
      -- New_Operator_SCO --
      ----------------------

      function New_Operator_SCO (Kind : Operator_Kind) return SCO_Id is
      begin
         pragma Assert (State.Current_Decision /= No_SCO_Id);
         return Add_SCO ((Kind       => Operator,
                          Origin     => CU,
                          Sloc_Range => SCO_Range,
                          Op_Kind    => Kind,
                          others     => <>));
      end New_Operator_SCO;

      -------------------------
      -- Update_Decision_BDD --
      -------------------------

      procedure Update_Decision_BDD (SCOD : in out SCO_Descriptor) is
      begin
         SCOD.Decision_BDD    := State.Current_BDD;
         SCOD.Last_Cond_Index := State.Current_Condition_Index;
      end Update_Decision_BDD;

      --------------------------
      -- Update_Decision_Sloc --
      --------------------------

      procedure Update_Decision_Sloc (SCOD : in out SCO_Descriptor) is
      begin
         if SCOD.Sloc_Range.Source_File = No_Source_File then
            SCOD.Sloc_Range.Source_File := Unit.Main_Source;
         end if;

         if SCOD.Sloc_Range.L.First_Sloc = No_Local_Location then
            SCOD.Sloc_Range.L.First_Sloc := From_Sloc;
         end if;

         if SCOD.Sloc_Range.L.Last_Sloc = No_Local_Location
           or else SCOD.Sloc_Range.L.Last_Sloc < To_Sloc
         then
            SCOD.Sloc_Range.L.Last_Sloc := To_Sloc;
         end if;
      end Update_Decision_Sloc;

      New_SCO : SCO_Id;

   --  Start of processing for Process_Low_Level_Entry

   begin
      if To_Sloc.Line > State.Last_Line then
         State.Last_Line := To_Sloc.Line;
      end if;

      case SCOE.C1 is
         when '>' =>
            --  Dominance marker: processed in conjunction with following 'S'
            --  entry.

            pragma Assert (State.Dom_SCO = No_SCO_Id);
            if SCOE.Last then
               --  Ignore dominance marker because all S entries in its
               --  sequence have been suppressed.

               null;

            else
               case SCOE.C2 is
                  when 'S' =>
                     State.Dom_Sloc :=
                        Slocs.To_Sloc (Unit.Main_Source, From_Sloc);
                     State.Dom_Val  := Unknown;

                  when 'T' | 'F' =>
                     State.Dom_Sloc :=
                        Slocs.To_Sloc (Unit.Main_Source, From_Sloc);
                     State.Dom_Val  := To_Tristate (SCOE.C2 = 'T');

                  when 'E' =>
                     State.Current_Handler_Range := SCO_Range;

                  when others =>
                     raise Program_Error;
               end case;
            end if;

         when 'S' | 's' =>
            pragma Assert (State.Current_Decision = No_SCO_Id);

            New_SCO := Add_SCO
              (SCO_Descriptor'
                 (Kind           => Statement,
                  Origin         => CU,
                  Sloc_Range     => SCO_Range,
                  S_Kind         => To_Statement_Kind (SCOE.C2),
                  Dominant       => State.Dom_SCO,
                  Dominant_Sloc  => State.Dom_Sloc,
                  Dominant_Value => State.Dom_Val,
                  Handler_Range  => State.Current_Handler_Range,
                  Pragma_Name    => Case_Insensitive_Get_Pragma_Id
                    (SCOE.Pragma_Aspect_Name),
                  others         => <>));

            State.Current_Handler_Range := No_Range;
            State.Dom_Val  := Unknown;
            State.Dom_Sloc := No_Location;
            if SCOE.Last then
               State.Dom_SCO := No_SCO_Id;
            else
               State.Dom_SCO := SCO_Vector.Last_Index;
            end if;

         when 'E' | 'G' | 'I' | 'P' | 'W' | 'X' | 'A' =>
            --  Decision

            pragma Assert (State.Current_Decision = No_SCO_Id);
            State.Current_Decision := Add_SCO
              (SCO_Descriptor'
                 (Kind                => Decision,
                  Origin              => CU,
                  Control_Location    =>

                  --  Control locations are only useful for dominance
                  --  markers, which are only used with binary traces. As
                  --  it is impractical to get the correct location with
                  --  the C/C++ instrumenter, and as using incorrect slocs
                  --  can create conflicts, ignore those in the
                  --  instrumentation case.

                    (if Provider = Compiler
                     then Slocs.To_Sloc (Unit.Main_Source, From_Sloc)
                     else No_Location),

                  D_Kind              => To_Decision_Kind (SCOE.C1),
                  Last_Cond_Index     => 0,
                  Aspect_Name         =>
                    Get_Aspect_Id (SCOE.Pragma_Aspect_Name),
                  others              => <>));
            pragma Assert (not SCOE.Last);

            State.Current_BDD :=
              BDD.Create (BDD_Vector, State.Current_Decision);
            State.Current_Condition_Index := No_Condition_Index;

         when ' ' =>
            --  Condition

            pragma Assert (State.Current_Decision /= No_SCO_Id);

            SCO_Vector.Update_Element
              (Index   => State.Current_Decision,
               Process => Update_Decision_Sloc'Access);

            State.Current_Condition_Index := State.Current_Condition_Index + 1;

            New_SCO := Add_SCO
              (SCO_Descriptor'(Kind       => Condition,
                               Origin     => CU,
                               Sloc_Range => SCO_Range,
                               Value      => Make_Condition_Value,
                               Index      => State.Current_Condition_Index,
                               others     => <>));
            BDD.Process_Condition (BDD_Vector, State.Current_BDD, New_SCO);

            if SCOE.Last then
               BDD.Completed (BDD_Vector, State.Current_BDD, Count_Paths);
               SCO_Vector.Update_Element
                 (State.Current_BDD.Decision, Update_Decision_BDD'Access);

               if Verbose then
                  Dump_Decision (State.Current_Decision);
               end if;
               State.Current_Decision := No_SCO_Id;
            end if;

         when '!' =>
            BDD.Process_Not (New_Operator_SCO (Op_Not), State.Current_BDD);

         when '&' =>
            BDD.Process_And_Then (BDD_Vector,
                                  New_Operator_SCO (Op_And_Then),
                                  State.Current_BDD);
         when '|' =>
            BDD.Process_Or_Else (BDD_Vector,
                                 New_Operator_SCO (Op_Or_Else),
                                 State.Current_BDD);

         when 'H' =>
            --  Chaining indicator: not used yet

            null;

         when others =>
            raise Program_Error
              with "unexpected SCO entry code: " & SCOE.C1;
      end case;
   end Process_Low_Level_Entry;

   ----------------------------
   -- Process_Low_Level_SCOs --
   ----------------------------

   procedure Process_Low_Level_SCOs
     (Provider      : SCO_Provider;
      Origin        : Source_File_Index;
      Deps          : SFI_Vector := SFI_Vectors.Empty_Vector;
      Created_Units : out Created_Unit_Maps.Map;
      SCO_Map       : access LL_HL_SCO_Map := null;
      Count_Paths   : Boolean)
   is
      use SCOs;

      LI_First_SCO : constant SCO_Id := SCO_Vector.Last_Index + 1;
      --  Index of the first high level SCO we are going to create

      Ignored_Slocs_Set : Ignored_Slocs_Sets.Set;

      CU_Load_Infos : CU_Load_Info_Vectors.Vector;
   begin
      --  Build a list of units to load, and gather associated SCO entries

      Build_CU_Load_Info (CU_Load_Infos, Deps);

      for Info of CU_Load_Infos loop
         declare
            --  Record entry high water mark in high level SCO tables

            First_SCO      : constant SCO_Id := SCO_Vector.Last_Index + 1;
            First_Instance : constant Inst_Id := Inst_Vector.Last_Index + 1;

            Fingerprint : constant Fingerprint_Type := Fingerprint_Type
              (GNAT.SHA1.Binary_Message_Digest'
                 (GNAT.SHA1.Digest (Info.Fingerprint_Context)));

            CU    : constant CU_Id := Allocate_CU
              (Provider, Origin, Info.Source_File, Fingerprint, Created_Units);
            State : CU_Load_State;
         begin
            State.Last_Line := 0;

            --  If we already have a CU for this unit, skip the information we
            --  have. Allocate_CU has emitted a warning in that case.

            if CU = No_CU_Id then
               goto Skip_Unit;
            end if;

            --  Impport all SCO entries for this unit

            for SCO_Range of Info.Entries loop
               State.Current_Decision      := No_SCO_Id;
               State.Dom_SCO               := No_SCO_Id;
               State.Dom_Sloc              := No_Location;
               State.Dom_Val               := Unknown;
               State.Current_Handler_Range := No_Range;

               for SCO_Index in SCO_Range.First .. SCO_Range.Last loop
                  Process_Low_Level_Entry
                    (CU,
                     SCO_Index,
                     State,
                     Ignored_Slocs_Set,
                     SCO_Map,
                     Count_Paths,
                     Provider);
               end loop;
            end loop;

            --  Prealloc line table entries for this unit

            Prealloc_Lines (Info.Source_File, State.Last_Line);

            --  Import unit instance table into global table. Even though all
            --  units created for this LI file have in principle the same
            --  instance, it is necessary to duplicate them in our internal
            --  tables so that they can be filtered out/consolidated
            --  separately later on.

            for LL_Inst_Id in SCO_Instance_Table.First
                           .. SCO_Instance_Table.Last
            loop
               declare
                  SIE : SCO_Instance_Table_Entry renames
                     SCO_Instance_Table.Table (LL_Inst_Id);

                  Sloc : constant Source_Location :=
                    (Source_File => Deps.Element (SIE.Inst_Dep_Num),
                     L           => (Line   => Natural (SIE.Inst_Loc.Line),
                                     Column => Natural (SIE.Inst_Loc.Col)));

                  Enclosing_Inst : Inst_Id := No_Inst_Id;
                  --  Index of the enclosing instance for SIE
               begin
                  --  If there is an enclosing instance, compute the index of
                  --  the high-level one from the index of the low-level one.

                  if SIE.Enclosing_Instance /= 0 then
                     Enclosing_Inst :=
                        First_Instance + Inst_Id (SIE.Enclosing_Instance) - 1;
                  end if;

                  Inst_Vector.Append (Inst_Info'(Sloc, Enclosing_Inst, CU));
               end;
            end loop;

            --  Finally update CU info

            declare
               Unit : CU_Info renames CU_Vector.Reference (CU);
            begin
               Unit.Deps := Deps;

               Unit.First_SCO := First_SCO;
               Unit.Last_SCO  := SCO_Vector.Last_Index;

               Unit.First_Instance := First_Instance;
               Unit.Last_Instance  := Inst_Vector.Last_Index;
            end;
         end;

         <<Skip_Unit>> null;
      end loop;
      Free (CU_Load_Infos);

      --  Build Sloc -> SCO index and set up Parent links

      for SCO in LI_First_SCO .. SCO_Vector.Last_Index loop
         declare
            function Equivalent (L, R : SCO_Descriptor) return Boolean;
            --  Return if L and R can be considered as the same SCOs. This
            --  is used to avoid duplicate SCOs coming from static inline
            --  functions in C headers included in many places: SCOs for such
            --  functions are duplicated across compile units, and we want to
            --  eliminate them but still emit warnings for SCOs that are too
            --  different.

            procedure Process_Descriptor (SCOD : in out SCO_Descriptor);
            --  Set up Parent link for SCOD at index SCO, and insert
            --  Sloc -> SCO map entry.

            ----------------
            -- Equivalent --
            ----------------

            function Equivalent (L, R : SCO_Descriptor) return Boolean is
            begin
               if L.Kind /= R.Kind
                 or else L.Sloc_Range /= R.Sloc_Range
               then
                  return False;
               end if;

               return
                 (case L.Kind is
                     when Removed =>
                        raise Program_Error with "unreachable code",
                     when Statement =>
                        L.S_Kind = R.S_Kind,
                     when Condition =>
                        L.Value = R.Value and then L.Index = R.Index,
                     when Decision =>
                        (L.D_Kind = R.D_Kind
                         and then L.Control_Location = R.Control_Location),
                     when Operator =>
                        L.Op_Kind = R.Op_Kind);
            end Equivalent;

            ------------------------
            -- Process_Descriptor --
            ------------------------

            procedure Process_Descriptor (SCOD : in out SCO_Descriptor) is
               Sloc_Range : Source_Location_Range := SCOD.Sloc_Range;
               --  Map Sloc_range to the SCO. By default, this range is the one
               --  associated to the SCOD.

               First : constant Source_Location := First_Sloc (Sloc_Range);
               --  First sloc of SCO

               Enclosing_SCO : constant SCO_Id := Sloc_To_SCO (First);
               --  SCO containing First

            begin
               if Verbose then
                  Put ("Processing: " & Image (SCO));
                  if SCOD.Kind = Decision then
                     if SCOD.Last_Cond_Index > 0 then
                        Put (" (complex)");
                     else
                        Put (" (simple)");
                     end if;
                  end if;
                  New_Line;
               end if;

               case SCOD.Kind is
                  when Removed =>
                     raise Program_Error with "unreachable code";

                  when Decision =>
                     --  A Decision SCO must have a statement or (in the case
                     --  of a nested decision) a Condition SCO as its parent,
                     --  or no parent at all.

                     pragma Assert (Enclosing_SCO = No_SCO_Id
                                      or else
                                    Kind (Enclosing_SCO) /= Decision);
                     SCOD.Parent := Enclosing_SCO;

                     if SCOD.Control_Location /= No_Location then
                        Sloc_Range :=
                          To_Range (SCOD.Control_Location, No_Location);
                     end if;

                     Add_SCO_To_Lines (SCO, SCOD);

                  when Statement =>

                     if (Enclosing_SCO /= No_SCO_Id
                         and then Equivalent
                           (SCOD, SCO_Vector (Enclosing_SCO)))

                        --  The only form of SCO overlapping we allow is
                        --  SCO nesting. A statement can contain nested
                        --  statements, e.g. with C++ lambda expressions.
                        --  We reject every other kind of overlapping.
                        --
                        --  TODO???: with C headers, we can have multiple
                        --  times the same SCO if the header is included
                        --  multiple times. This will result in a buggy
                        --  behavior if the included code expansion varies (as
                        --  we may accept nested SCO that come from the second
                        --  inclusion, but that are nested in a SCO from the
                        --  first inclusion, which makes no sense). Consider
                        --  this as a marginal use case for now.

                        or else Invalid_Overlap (SCOD, Enclosing_SCO)
                        or else Invalid_Overlap
                          (SCOD, Sloc_To_SCO (Last_Sloc (Sloc_Range)))
                     then
                        return;
                     end if;
                     Add_SCO_To_Lines (SCO, SCOD);

                  when Condition | Operator =>
                     --  Parent is already set to the enclosing decision or
                     --  operator.

                     null;

               end case;

               declare
                  use Sloc_To_SCO_Maps;

                  Map      : constant access Sloc_To_SCO_Maps.Map :=
                    Writeable_Sloc_To_SCO_Map
                       (Sloc_Range.Source_File, SCOD.Kind);
                  Cur      : Sloc_To_SCO_Maps.Cursor;
                  Inserted : Boolean;
               begin
                  --  If we have equivalent SCOs, wipe them out of the table.

                  --  Note: we used to handle Constraint_Error here to account
                  --  for old compilers that generated junk SCOs with the same
                  --  source locations. These bugs have now been fixed, so the
                  --  work-around was removed, and if this happened again we'd
                  --  propagate the exception.

                  Map.Insert (Sloc_Range.L, SCO, Cur, Inserted);
                  if not Inserted then
                     if not Equivalent (SCOD, SCO_Vector (Element (Cur))) then
                        Report
                          (First,
                           "sloc range conflict for SCOs "
                           & Image (Element (Cur)));
                     end if;

                     --  Reset SCOD to Removed_SCO_Descriptor, which acts as a
                     --  placeholder to cancel the entry.

                     SCOD := Removed_SCO_Descriptor;
                  end if;
               end;
            end Process_Descriptor;

         begin
            SCO_Vector.Update_Element (SCO, Process_Descriptor'Access);
         end;
      end loop;

      --  Now that all decisions and statements have been entered in the
      --  sloc -> SCO map, set the Dominant information.

      for SCO in LI_First_SCO .. SCO_Vector.Last_Index loop
         declare
            SCOD         : SCO_Descriptor renames SCO_Vector.Reference (SCO);
            Dom_Sloc_SCO : SCO_Id;
         begin
            --  Set SCOD.Dominant (if unset) to the innermost SCO containing
            --  SCOD.Dominant_Sloc.

            if SCOD.Kind = Statement
                 and then SCOD.Dominant_Sloc /= No_Location
            then
               pragma Assert (SCOD.Dominant = No_SCO_Id);

               --  Retrieve innermost SCO at designated sloc

               if SCOD.Dominant_Value = Unknown then
                  --  Case of >S: dominant SCO is a statement

                  Dom_Sloc_SCO := Sloc_To_SCO (SCOD.Dominant_Sloc);

                  --  In C, conditionals (IF blocks or ternary
                  --  expressions) have the same sloc as their embedded
                  --  condition/decision. In such cases, Sloc_To_SCO returns
                  --  the condition SCO, whereas we are interested in the
                  --  enclosing statement SCO.

                  if Dom_Sloc_SCO /= No_SCO_Id
                     and then Kind (Dom_Sloc_SCO) = Condition
                  then
                     Dom_Sloc_SCO := Enclosing_Statement (Dom_Sloc_SCO);
                  end if;

                  --  Dom_Sloc_SCO is permitted to be No_SCO_Id because
                  --  for a dominant that is a disabled pragma Debug, older
                  --  compiler versions used to omit the statement SCO.

                  pragma Assert
                    (Dom_Sloc_SCO = No_SCO_Id
                       or else Kind (Dom_Sloc_SCO) = Statement);

               else
                  --  Case of >T / >F: dominant SCO is a decision

                  if Sloc_To_SCO_Map
                       (SCOD.Dominant_Sloc.Source_File, Decision)
                     .Contains ((SCOD.Dominant_Sloc.L, No_Local_Location))
                  then
                     Dom_Sloc_SCO :=
                       Sloc_To_SCO_Map
                         (SCOD.Dominant_Sloc.Source_File, Decision)
                       .Element ((SCOD.Dominant_Sloc.L, No_Local_Location));
                     pragma Assert (Kind (Dom_Sloc_SCO) = Decision);
                  elsif Ignored_Slocs_Set.Contains (SCOD.Dominant_Sloc) then

                     --  If the dominant sloc was purposefully ignored,
                     --  there is no need to warn about it.

                     Dom_Sloc_SCO := No_SCO_Id;
                  else
                     Report
                       (First_Sloc (SCOD.Sloc_Range),
                        "dominant decision of statement " & Image (SCO)
                        & " has no associated SCO"
                        & ", discarding dominance information");
                     Dom_Sloc_SCO := No_SCO_Id;
                  end if;
               end if;

               SCOD.Dominant := Dom_Sloc_SCO;
            end if;
         end;
      end loop;
   end Process_Low_Level_SCOs;

   -------------
   -- Map_Tag --
   -------------

   overriding function Map_Tag
     (TP     : access Instance_Tag_Provider_Type;
      Relocs : Checkpoint_Relocations;
      CP_Tag : SC_Tag) return SC_Tag
   is
      pragma Unreferenced (TP);
   begin
      --  Remap CP_Tag interpreted as a global instance id. No remapping is
      --  needed for No_SC_Tag as it designates the absence of instance.

      if CP_Tag = No_SC_Tag then
         return No_SC_Tag;
      else
         return SC_Tag (Remap_Inst_Id (Relocs, Inst_Id (CP_Tag)));
      end if;
   end Map_Tag;

   ----------
   -- Read --
   ----------

   procedure Read (S : access Root_Stream_Type'Class; V : out SCO_Descriptor)
   is
      SCOD : SCO_Descriptor (SCO_Kind'Input (S));
   begin
      if SCOD.Kind = Removed then
         V := Removed_SCO_Descriptor;
         return;
      end if;

      SCOD.Origin     := CU_Id'Input (S);
      SCOD.Sloc_Range := Source_Location_Range'Input (S);
      SCOD.Parent     := SCO_Id'Input (S);

      case SCOD.Kind is
      when Removed =>
         raise Program_Error with "unreachable code";

      when Statement =>
         SCOD.S_Kind         := Statement_Kind'Input (S);
         SCOD.Dominant       := SCO_Id'Input (S);
         SCOD.Dominant_Value := Tristate'Input (S);
         SCOD.Dominant_Sloc  := Source_Location'Input (S);
         SCOD.Handler_Range  := Source_Location_Range'Input (S);
         SCOD.Pragma_Name    := Pragma_Id'Input (S);

      when Condition =>
         SCOD.Value    := Tristate'Input (S);
         SCOD.PC_Set   := PC_Sets.Set'Input (S);
         SCOD.BDD_Node := BDD_Node_Id'Input (S);
         SCOD.Index    := Condition_Index'Input (S);

      when Decision =>

         --  Before version 2, decisions shared Operations's Operand member,
         --  and stored the expression as its Right array item.

         if Version_Less (S, Than => 2) then
            declare
               Operands : constant Operand_Pair := Operand_Pair'Input (S);
            begin
               SCOD.Expression := Operands (Right);
            end;
         else
            SCOD.Expression       := SCO_Id'Input (S);
         end if;

         SCOD.D_Kind           := Decision_Kind'Input (S);
         SCOD.Control_Location := Source_Location'Input (S);
         SCOD.Last_Cond_Index  := Any_Condition_Index'Input (S);
         SCOD.Decision_BDD     := BDD.BDD_Type'Input (S);
         SCOD.Degraded_Origins := Boolean'Input (S);
         SCOD.Aspect_Name      := Aspect_Id'Input (S);
         SCOD.Path_Count       := Natural'Input (S);

      when Operator =>
         SCOD.Operands := Operand_Pair'Input (S);
         SCOD.Op_Kind := Operator_Kind'Input (S);
      end case;

      V := SCOD;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (S : access Root_Stream_Type'Class; V : SCO_Descriptor) is
   begin
      SCO_Kind'Output (S, V.Kind);
      if V.Kind = Removed then
         return;
      end if;

      CU_Id'Output                 (S, V.Origin);
      Source_Location_Range'Output (S, V.Sloc_Range);
      SCO_Id'Output                (S, V.Parent);

      case V.Kind is
      when Removed =>
         raise Program_Error with "unreachable code";

      when Statement =>
         Statement_Kind'Output        (S, V.S_Kind);
         SCO_Id'Output                (S, V.Dominant);
         Tristate'Output              (S, V.Dominant_Value);
         Source_Location'Output       (S, V.Dominant_Sloc);
         Source_Location_Range'Output (S, V.Handler_Range);
         Pragma_Id'Output             (S, V.Pragma_Name);

      when Condition =>
         Tristate'Output        (S, V.Value);
         PC_Sets.Set'Output     (S, V.PC_Set);
         BDD_Node_Id'Output     (S, V.BDD_Node);
         Condition_Index'Output (S, V.Index);

      when Decision | Operator =>
         case V.Kind is
         when Decision =>
            SCO_Id'Output              (S, V.Expression);
            Decision_Kind'Output       (S, V.D_Kind);
            Source_Location'Output     (S, V.Control_Location);
            Any_Condition_Index'Output (S, V.Last_Cond_Index);
            BDD.BDD_Type'Output        (S, V.Decision_BDD);
            Boolean'Output             (S, V.Degraded_Origins);
            Aspect_Id'Output           (S, V.Aspect_Name);
            Natural'Output             (S, V.Path_Count);

         when Operator =>
            Operand_Pair'Output  (S, V.Operands);
            Operator_Kind'Output (S, V.Op_Kind);

         when others =>
            null;
         end case;
      end case;
   end Write;

   -------------------
   -- Next_BDD_Node --
   -------------------

   function Next_BDD_Node
     (SCO   : SCO_Id;
      Value : Boolean) return BDD_Node_Id
   is
      use BDD;
      BDD_Node : constant BDD_Node_Id := SCO_Vector.Reference (SCO).BDD_Node;
   begin
      return BDD_Vector.Reference (BDD_Node).Dests (Value);
   end Next_BDD_Node;

   --------------------
   -- Next_Condition --
   --------------------

   function Next_Condition (SCO : SCO_Id; Value : Boolean) return SCO_Id is
      use BDD;
      BDDN : constant BDD_Node :=
               BDD_Vector.Element (Next_BDD_Node (SCO, Value));
   begin
      if BDDN.Kind = Condition then
         return BDDN.C_SCO;
      else
         return No_SCO_Id;
      end if;
   end Next_Condition;

   ---------------------
   -- Offset_For_True --
   ---------------------

   function Offset_For_True (SCO : SCO_Id) return Natural is
      use BDD;
      BDD_Node : constant BDD_Node_Id := SCO_Vector.Reference (SCO).BDD_Node;
   begin
      return BDD_Vector.Reference (BDD_Node).Path_Offset;
   end Offset_For_True;

   -------------
   -- Op_Kind --
   -------------

   function Op_Kind (SCO : SCO_Id) return Operator_Kind is
   begin
      return SCO_Vector (SCO).Op_Kind;
   end Op_Kind;

   -------------
   -- Operand --
   -------------

   function Operand (SCO : SCO_Id; Position : Operand_Position) return SCO_Id
   is
   begin
      return SCO_Vector.Reference (SCO).Operands (Position);
   end Operand;

   -------------
   -- Outcome --
   -------------

   function Outcome (SCO : SCO_Id; Value : Boolean) return Tristate is
      use BDD;
      Cond_SCO   : SCO_Id := SCO;
      Cond_Value : Boolean := Value;
   begin
      loop
         declare
            BDDN : constant BDD_Node :=
              BDD_Vector.Constant_Reference
                (Next_BDD_Node (Cond_SCO, Cond_Value));
         begin
            case BDDN.Kind is
               when Outcome =>
                  return To_Tristate (BDDN.Decision_Outcome);

               when Condition =>
                  declare
                     Next_Value : constant Tristate :=
                       SC_Obligations.Value (BDDN.C_SCO);
                  begin
                     if Next_Value = Unknown then
                        return Unknown;
                     end if;
                     Cond_SCO := BDDN.C_SCO;
                     Cond_Value := To_Boolean (Next_Value);
                  end;

               when others =>
                  raise Program_Error;
            end case;
         end;
      end loop;
   end Outcome;

   -----------
   -- Value --
   -----------

   function Value (SCO : SCO_Id) return Tristate is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
      pragma Assert (SCOD.Kind = Condition);
   begin
      return SCOD.Value;
   end Value;

   ------------
   -- Parent --
   ------------

   function Parent (SCO : SCO_Id) return SCO_Id is
   begin
      return SCO_Vector.Reference (SCO).Parent;
   end Parent;

   ----------------
   -- Path_Count --
   ----------------

   function Path_Count (SCO : SCO_Id) return Natural is
   begin
      return SCO_Vector.Reference (SCO).Decision_BDD.Path_Count;
   end Path_Count;

   --------------------------
   -- Set_Path_Count_Limit --
   --------------------------

   procedure Set_Path_Count_Limit (Limit : Natural) is
   begin
      SC_Obligations.BDD.Path_Count_Limit := Limit;
   end Set_Path_Count_Limit;

   --------------------------
   -- Get_Path_Count_Limit --
   --------------------------

   function Get_Path_Count_Limit return Natural is
     (SC_Obligations.BDD.Path_Count_Limit);

   --------------------
   -- Prealloc_Lines --
   --------------------

   procedure Prealloc_Lines
     (Cur_Source_File : Source_File_Index;
      Last_Line       : in out Natural)
   is
   begin
      if Cur_Source_File /= No_Source_File
        and then Last_Line > 0
      then
         Expand_Line_Table (Cur_Source_File, Last_Line);
         Last_Line := 0;
      end if;
   end Prealloc_Lines;

   --------------
   -- Previous --
   --------------

   function Previous (SCO : SCO_Id) return SCO_Id is
      Dom_SCO : SCO_Id;
      Dom_Val : Boolean;
   begin
      Dominant (SCO, Dom_SCO, Dom_Val);
      return Enclosing_Statement (Dom_SCO);
   end Previous;

   --------------------------------
   -- Report_Multipath_Decisions --
   --------------------------------

   procedure Report_Multipath_Decisions is
   begin
      for SCO in SCO_Vector.First_Index .. SCO_Vector.Last_Index loop
         declare
            use BDD, BDD.BDD_Vectors;
            SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
            DB   : BDD_Node_Id;
         begin
            if SCOD.Kind = Decision then
               DB := SCOD.Decision_BDD.First_Multipath_Condition;
               if DB /= No_BDD_Node_Id then
                  Report
                    (First_Sloc (BDD_Vector (DB).C_SCO),
                     "condition is reachable through multiple paths",
                     Kind => Warning);
               end if;
            end if;
         end;
      end loop;
   end Report_Multipath_Decisions;

   ------------------------------
   -- Report_SCOs_Without_Code --
   ------------------------------

   procedure Report_SCOs_Without_Code is
   begin
      for SCO in SCO_Vector.First_Index .. SCO_Vector.Last_Index loop
         --  Check whether SCO is a Condition, and if so, warn if it has no
         --  associated conditional branch instruction.

         declare
            SCOD  : SCO_Descriptor renames SCO_Vector.Reference (SCO);
            D_SCO : SCO_Id;
         begin
            if SCOD.Kind /= Condition then
               goto Skip_SCO;
            end if;

            --  Report a static analysis error if one condition has no
            --  associated conditional branch, and the enclosing decision is
            --  not compile time known.

            D_SCO := Enclosing_Decision (SCO);
            if SCOD.PC_Set.Length = 0
              and then Decision_Outcome (D_SCO) = Unknown
            then
               Report
                 (First_Sloc (SCOD.Sloc_Range),
                  Msg  => "no conditional branch (in "
                            & Decision_Kind'Image (SCO_Vector (D_SCO).D_Kind)
                            & ")",
                  Kind => Diagnostics.Error);

               --  Interesting property: we can never do without a condition
               --  using inference of condition values from BDD position,
               --  because that would require that both outgoing edges from the
               --  condition also are conditions (not outcomes), and that can't
               --  happen in a short circuit expression without a multipath
               --  condition; this would require a BDD involving the Sel
               --  ternary operator:
               --    Sel (A, B, C) = (A and then B) or else (not A and then C)

            end if;
         end;

         <<Skip_SCO>>
      end loop;
   end Report_SCOs_Without_Code;

   -------------------------------
   -- Report_Units_Without_Code --
   -------------------------------

   procedure Report_Units_Without_Code is

      package SFI_Sets is new Ada.Containers.Ordered_Sets
        (Valid_Source_File_Index);
      Reported_Origins : SFI_Sets.Set;
      --  Keep track of origins that we reported, so that we don't report the
      --  same origin twice. This happens when multiple compilation units share
      --  the same origin.

   begin
      for CU_Id in CU_Vector.First_Index .. CU_Vector.Last_Index loop
         declare
            CUI    : CU_Info renames CU_Vector.Reference (CU_Id);
            Origin : constant Valid_Source_File_Index := CUI.Origin;
         begin
            if not CUI.Has_Code
              and then Has_SCOs (CUI)
              and then not Reported_Origins.Contains (Origin)
            then
               Report
                 (Msg  => "no object code for " & Get_Simple_Name (Origin),
                  Kind => Diagnostics.Error);
               Reported_Origins.Insert (Origin);
            end if;
         end;
      end loop;
   end Report_Units_Without_Code;

   ------------
   -- S_Kind --
   ------------

   function S_Kind (SCO : SCO_Id) return Any_Statement_Kind is
   begin
      if SCO = No_SCO_Id then
         return No_Statement;
      end if;

      return SCO_Vector.Reference (SCO).S_Kind;
   end S_Kind;

   --------------------------
   -- Set_Degraded_Origins --
   --------------------------

   procedure Set_Degraded_Origins (SCO : SCO_Id; Val : Boolean := True) is
   begin
      SCO_Vector.Reference (SCO).Degraded_Origins := Val;
   end Set_Degraded_Origins;

   ------------------
   -- Set_BDD_Node --
   ------------------

   procedure Set_BDD_Node (C_SCO : SCO_Id; BDD_Node : BDD_Node_Id) is
   begin
      SCO_Vector.Reference (C_SCO).BDD_Node := BDD_Node;
   end Set_BDD_Node;

   ------------------
   -- Set_Bit_Maps --
   ------------------

   procedure Set_Bit_Maps (CU : CU_Id; Bit_Maps : CU_Bit_Maps) is
      use GNAT.SHA1;

      Info : CU_Info renames CU_Vector.Reference (CU);
      Ctx : GNAT.SHA1.Context;
      LF  : constant String := (1 => ASCII.LF);
   begin
      Info.Bit_Maps := Bit_Maps;

      --  Compute the fingerprint for these bit maps

      Update (Ctx, "stmt:");
      for Id of Bit_Maps.Statement_Bits.all loop
         Update (Ctx, Id'Image);
      end loop;
      Update (Ctx, LF);

      Update (Ctx, "dc:");
      for D of Bit_Maps.Decision_Bits.all loop
         Update (Ctx, D.D_SCO'Image & ":" & D.Outcome'Image);
      end loop;
      Update (Ctx, LF);

      Update (Ctx, "mcdc:");
      for M of Bit_Maps.MCDC_Bits.all loop
         Update (Ctx, M.D_SCO'Image & ":" & M.Path_Index'Image);
      end loop;
      Update (Ctx, LF);

      Info.Bit_Maps_Fingerprint := Fingerprint_Type
        (GNAT.SHA1.Binary_Message_Digest'(GNAT.SHA1.Digest (Ctx)));
   end Set_Bit_Maps;

   ------------------------
   -- Get_Scope_Entities --
   ------------------------

   function Get_Scope_Entities (CU : CU_Id) return Scope_Entities_Tree is
   begin
      if CU /= No_CU_Id then
         return CU_Vector.Reference (CU).Element.Scope_Entities;
      end if;
      return Scope_Entities_Trees.Empty_Tree;
   end Get_Scope_Entities;

   -----------------------------
   -- SCOs_Nested_And_Ordered --
   -----------------------------

   function SCOs_Nested_And_Ordered
     (Tree : Scope_Entities_Trees.Tree) return Boolean
   is
      use Scope_Entities_Trees;

      Failure : exception;
      --  Exception raised when the nesting/ordering invariant is found to be
      --  broken.

      Lower_Bound : SCO_Id := No_SCO_Id;
      --  At every step of the check, this designates the minimum possible SCO
      --  value for the .From component for the next element to inspect.

      procedure Check_Element (Cur : Cursor);
      --  Check that Cur's From/To SCOs range is not empty and
      --  Parent_From .. Parent_To range and that they are correctly ordered.

      -------------------
      -- Check_Element --
      -------------------

      procedure Check_Element (Cur : Cursor) is
         SE    : Scope_Entity renames Tree.Constant_Reference (Cur);
         Child : Cursor := First_Child (Cur);

         Last : SCO_Id;
         --  SCO range upper bound for Cur's last child, or SE.From if there is
         --  no child.
      begin
         --  Check that SCO ranges are never empty

         if SE.From > SE.To then
            raise Failure with "empty SCO range for " & Image (SE);
         end if;

         --  Check that the SCO range lower bound is both:
         --
         --  * greater or equal to the parent's lower bound (this is the first
         --    half of the nesting check;
         --
         --  * greater than the previous sibling (if any: this checks the
         --    ordering).

         if SE.From < Lower_Bound then
            raise Failure with "SCO lower bound too low for " & Image (SE);
         end if;
         Lower_Bound := SE.From;
         Last := SE.From;

         while Has_Element (Child) loop
            Check_Element (Child);
            Child := Next_Sibling (Child);
            Last := Lower_Bound;

            --  The next sibling's SCO range cannot overlap with the current's

            Lower_Bound := Lower_Bound + 1;
         end loop;

         --  Check that the SCO range upper bound is greater or equal to
         --  the upper bound of the last child's upper bound (this is the
         --  second half of the nesting check).

         if SE.To < Last then
            raise Failure with "SCO higher bound too low for " & Image (SE);
         end if;
         Lower_Bound := SE.To;
      end Check_Element;

      Cur : Cursor := First_Child (Tree.Root);

   --  Start of processing for SCOs_Nested_And_Ordered

   begin
      while Has_Element (Cur) loop
         Check_Element (Cur);
         Cur := Next_Sibling (Cur);
      end loop;
      return True;

   exception
      when Exc : Failure =>

         --  In case of failure, be helpful and print the offending tree for
         --  the verbose mode.

         if Verbose then
            Put_Line
              ("The following tree of scopes breaks the nesting/ordering"
               & " invariant:");
            Put_Line (Ada.Exceptions.Exception_Message (Exc));
            Dump (Tree, "| ");
         end if;
         return False;
   end SCOs_Nested_And_Ordered;

   ------------------------
   -- Set_Scope_Entities --
   ------------------------

   procedure Set_Scope_Entities
     (CU : CU_Id; Scope_Entities : Scope_Entities_Trees.Tree)
   is
      SE : Scope_Entities_Trees.Tree renames
        CU_Vector.Reference (CU).Scope_Entities;
   begin
      --  Scopes are supposed to be set only once per compilation unit

      pragma Assert (SE.Is_Empty);

      pragma Assert (SCOs_Nested_And_Ordered (Scope_Entities));
      SE := Scope_Entities;

      if Verbose then
         Put_Line ("Setting scopes for " & Image (CU) & ":");
         Dump (SE, Line_Prefix => "| ");
      end if;
   end Set_Scope_Entities;

   -------------------------------
   -- Set_Operand_Or_Expression --
   -------------------------------

   procedure Set_Operand_Or_Expression
     (SCO      : SCO_Id;
      Position : Operand_Position;
      Expr     : SCO_Id)
   is
      SCOD      : SCO_Descriptor renames SCO_Vector.Reference (SCO);
      Expr_SCOD : SCO_Descriptor renames SCO_Vector.Reference (Expr);
   begin
      Expr_SCOD.Parent := SCO;

      case SCOD.Kind is
         when Operator =>
            SCOD.Operands (Position) := Expr;

         when Decision =>
            SCOD.Expression := Expr;

         when others =>
            raise Program_Error with "unreachable code";
      end case;
   end Set_Operand_Or_Expression;

   -----------------------
   -- Set_Unit_Has_Code --
   -----------------------

   procedure Set_Unit_Has_Code (CU : CU_Id) is
      Origin : constant Source_File_Index := CU_Vector.Reference (CU).Origin;
   begin
      --  Just like loading some LI creates all the units that have this LI as
      --  their origin, loading a program that has code for one such unit is
      --  also supposed to load code for all units with the same origin.
      --
      --  Set the Has_Code flag on all units that have the same origin as CU.

      for CU of Origin_To_CUs_Map.Reference (Origin) loop
         CU_Vector.Reference (CU).Has_Code := True;
      end loop;
   end Set_Unit_Has_Code;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range (SCO : SCO_Id) return Source_Location_Range is
   begin
      return SCO_Vector.Reference (SCO).Sloc_Range;
   end Sloc_Range;

   ------------------
   -- Slocs_To_SCO --
   ------------------

   function Sloc_To_SCO
     (Sloc              : Source_Location;
      Include_Decisions : Boolean := False) return SCO_Id
   is
      use Sloc_To_SCO_Maps;

      L_Sloc    : Source_Location := Sloc;
      Cur       : Cursor;
      SCO       : SCO_Id;
      SCO_Sloc  : Local_Source_Location_Range;

   begin
      if Sloc.Source_File = No_Source_File then
         return No_SCO_Id;
      end if;

      --  If looking up the sloc of a NOT operator, return SCO of innermost
      --  operand, if it is a condition.

      Cur := Sloc_To_SCO_Map (Sloc.Source_File, Operator).Find
               ((Sloc.L, No_Local_Location));
      if Cur /= No_Element then
         SCO := Element (Cur);
         while Kind (SCO) = Operator and then Op_Kind (SCO) = Op_Not loop
            SCO := Operand (SCO, Position => Right);
         end loop;
         if Kind (SCO) = Condition then
            return SCO;
         end if;
      end if;

      SCO := No_SCO_Id;

      if L_Sloc.L.Column = 0 then
         --  For the case of a lookup with a column of 0, we want a SCO
         --  starting before the end of the given line.

         L_Sloc.L.Column := Natural'Last;
      end if;

      --  Get the innermost condition or statement SCO. This relies on the fact
      --  that for nested sloc ranges, inner always sorts higher. So, in order
      --  to find the innermost range containing a given sloc, we just find
      --  the last one that starts no later than that sloc (i.e.
      --  Floor (Sloc, Sloc)).

      Cur := Sloc_To_SCO_Map (L_Sloc.Source_File, Condition).Floor
        ((L_Sloc.L, L_Sloc.L));
      if Cur /= No_Element then
         SCO := Element (Cur);
         SCO_Sloc := Key (Cur);
      end if;

      --  Now we have a candidate condition SCO. Look for a better match
      --  with a statement.

      Cur := Sloc_To_SCO_Map (L_Sloc.Source_File, Statement).Floor
        ((L_Sloc.L, L_Sloc.L));
      if Cur /= No_Element
            and then
         (SCO = No_SCO_Id or else SCO_Sloc < Key (Cur))
      then
         SCO := Element (Cur);
      end if;

      --  Climb up the SCO tree until an adequate match is found

      Climb_SCO_Tree : while SCO /= No_SCO_Id loop
         Climb_Operators :
         while SCO /= No_SCO_Id and then Kind (SCO) = Operator loop
            SCO := Parent (SCO);
            exit Climb_SCO_Tree when SCO = No_SCO_Id;
         end loop Climb_Operators;

         declare
            SCOD       : SCO_Descriptor renames SCO_Vector.Reference (SCO);
            Kind       : constant SCO_Kind := SCOD.Kind;
            Sloc_Range : constant Source_Location_Range := SCOD.Sloc_Range;
         begin
            if Sloc.L.Column = 0 then
               --  For a fuzzy match, never return a decision/condition SCO,
               --  always go up to the enclosing statement.

               exit Climb_SCO_Tree when
                 Sloc.L.Line in Sloc_Range.L.First_Sloc.Line
                             .. Sloc_Range.L.Last_Sloc.Line
                 and then (Kind = Statement
                             or else
                           (Include_Decisions and then Kind = Decision));
            else
               --  Do not return a decision, even with exact match, if
               --  Include_Decisions is False

               exit Climb_SCO_Tree when
                 Sloc_Range.L.First_Sloc <= Sloc.L
                 and then Sloc.L <= Sloc_Range.L.Last_Sloc
                 and then (Kind /= Decision or else Include_Decisions);
            end if;
         end;

         SCO := Parent (SCO);
      end loop Climb_SCO_Tree;

      --  Check for decision (exact match only)

      if Include_Decisions
           and then
         (SCO = No_SCO_Id or else Kind (SCO) = Statement)
      then
         Cur := Sloc_To_SCO_Map (Sloc.Source_File, Decision)
                  .Find ((Sloc.L, No_Local_Location));

         if Cur = No_Element then
            SCO := No_SCO_Id;
         else
            pragma Assert
              (SCO = No_SCO_Id
                 or else SCO = Enclosing_Statement (Element (Cur)));
            SCO := Element (Cur);
         end if;
      end if;

      --  A fuzzy match is specified as never returning a condition

      pragma Assert (not (Sloc.L.Column = 0
                            and then SCO /= No_SCO_Id
                            and then Kind (SCO) = Condition));
      return SCO;
   end Sloc_To_SCO;

   --------------
   -- Tag_Name --
   --------------

   overriding function Tag_Name
     (TP  : access Instance_Tag_Provider_Type;
      Tag : SC_Tag) return String
   is
      pragma Unreferenced (TP);
   begin
      return Instance_Loc (Inst_Id (Tag));
   end Tag_Name;

   ----------------------
   -- To_Decision_Kind --
   ----------------------

   function To_Decision_Kind (C : Character) return Decision_Kind is
   begin
      case C is
         when 'E'    => return Exit_Statement;
         when 'G'    => return Entry_Guard;
         when 'I'    => return If_Statement;
         when 'P'    => return Pragma_Decision;
         when 'W'    => return While_Loop;
         when 'X'    => return Expression;
         when 'A'    => return Aspect;
         when others => raise Constraint_Error;
      end case;
   end To_Decision_Kind;

   -----------------------
   -- To_Statement_Kind --
   -----------------------

   function To_Statement_Kind (C : Character) return Statement_Kind is
   begin
      case C is
         when 't'    => return Type_Declaration;
         when 's'    => return Subtype_Declaration;
         when 'o'    => return Object_Declaration;
         when 'r'    => return Renaming_Declaration;
         when 'i'    => return Generic_Instantiation;
         when 'd'    => return Other_Declaration;
         when 'A'    => return Accept_Statement;
         when 'C'    => return Case_Statement;
         when 'E'    => return Exit_Statement;
         when 'F'    => return For_Loop_Statement;
         when 'I'    => return If_Statement;
         when 'P'    => return Pragma_Statement;
         when 'p'    => return Disabled_Pragma_Statement;
         when 'R'    => return Extended_Return_Statement;
         when 'S'    => return Select_Statement;
         when 'W'    => return While_Loop_Statement;
         when 'X'    => return Degenerate_Subprogram_Statement;
         when ' '    => return Other_Statement;
         when others => raise Constraint_Error;
      end case;
   end To_Statement_Kind;

   -------------------
   -- Unit_Has_Code --
   -------------------

   function Unit_Has_Code (SCO : SCO_Id) return Boolean is
   begin
      if SCO = No_SCO_Id then
         return False;
      end if;

      declare
         Origin : constant CU_Id := SCO_Vector.Reference (SCO).Origin;
      begin
         return CU_Vector.Reference (Origin).Has_Code;
      end;
   end Unit_Has_Code;

   -------------------
   -- Dump_All_SCOs --
   -------------------

   procedure Dump_All_SCOs is
   begin
      for SCO in Valid_SCO_Id'First .. Last_SCO loop
         Put_Line (Image (SCO));
         New_Line;
      end loop;
   end Dump_All_SCOs;

   ------------------------------------
   -- Case_Insensitive_Get_Pragma_Id --
   ------------------------------------

   function Case_Insensitive_Get_Pragma_Id
     (Pragma_Name : Name_Id) return Pragma_Id
   is
   begin
      if Pragma_Name = No_Name then
         return Unknown_Pragma;
      end if;

      --  Retrieve the pragma name as a string

      Get_Name_String (Pragma_Name);
      Name_Buffer (1 .. Name_Len) :=
        To_Lower (Name_Buffer (1 .. Name_Len));

      --  Try to get the Pragma_Id value corresponding to that name. If there
      --  is no such value, return Unknown_Pragma.

      declare
         Enum_Name : constant String :=
            "Pragma_" & Name_Buffer (1 .. Name_Len);
      begin
         return Pragma_Id'Value (Enum_Name);
      exception
         when Constraint_Error =>
            return Unknown_Pragma;
      end;
   end Case_Insensitive_Get_Pragma_Id;

   ------------------
   -- Add_PP_Info --
   ------------------

   procedure Add_PP_Info (SCO : SCO_Id; Info : PP_Info) is
      CU : constant CU_Id := Comp_Unit (SCO);
   begin
      CU_Vector.Reference (CU).Element.PP_Info_Map.Insert (SCO, Info);
   end Add_PP_Info;

begin
   Snames.Initialize;
end SC_Obligations;
