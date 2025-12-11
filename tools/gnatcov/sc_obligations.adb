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

--  Source Coverage Obligations

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS;
with Interfaces;

with Aspects;     use Aspects;
with Get_SCOs;
with GNAT.Regpat; use GNAT.Regpat;
with Namet;       use Namet;
with SCOs;
with Snames;

with Checkpoints;           use Checkpoints;
with Coverage;              use Coverage;
with Diagnostics;           use Diagnostics;
with Files_Table;           use Files_Table;
with Inputs;                use Inputs;
with LLVM_JSON_Checkpoints; use LLVM_JSON_Checkpoints;
with Outputs;               use Outputs;
with SC_Obligations.BDD;
with Switches;              use Switches;
with Traces_Elf;            use Traces_Elf;
with Traces_Files;

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

   function Floor
     (Tree : Scope_Entities_Trees.Tree; Sloc : Source_Location)
      return Scope_Entities_Trees.Cursor;
   --  Return the innermost scope containing Sloc

   function Covers_SCO (SE : Scope_Entity; SCO : SCO_Id) return Boolean
   is (In_Range (First_Sloc (SCO), SE.Source_Range));
   --  Return whether SCO is covered by SE's SCO range

   function Covers_SCO
     (ST : Scope_Traversal_Type; SCO : SCO_Id) return Boolean;
   --  Return whether SCO is covered by that element's SCO range. Id the SCO is
   --  a function SCO, then the scope can be the root of the tree.

   function Contains_SCO (SCO : SCO_Id; CU : CU_Id) return Boolean;
   --  Return whether the given CU contains the given SCO

   procedure Traverse_SCO (ST : in out Scope_Traversal_Type; SCO : SCO_Id)
   with Pre => Get_CU (ST) = No_CU_Id or else Contains_SCO (SCO, Get_CU (ST));
   --  Position ST on the inner-most scope that contains SCO.
   --
   --  This does nothing on a scope traversal type not initialized, or
   --  initialized on a CU with no scopes attached to it.
   --
   --  NOTE: Traverse_SCO will browse the scope tree structure at each new
   --  given SCO, making it a very EXPENSIVE call.
   --  For now, it memoizes the result for each SCO in its SCO descriptor,
   --  so the browsing is not duplicated.

   ------------------------
   -- Source units table --
   ------------------------

   type SID_Info is record
      Blocks : SCO_Id_Vector_Vector;
      --  List of blocks. A block is a list of statement SCOs. Note that
      --  this is specific to source instrumentation, but a container cannot
      --  be a discriminant-dependent component.

      Bit_Maps : CU_Bit_Maps;
      --  Mapping of bits in coverage buffers to SCOs

      Bit_Maps_Fingerprint : Fingerprint_Type;
      --  Hash of Bit_Maps, for consistency checks with source traces

      Annotations_Fingerprint : Fingerprint_Type := No_Fingerprint;
      --  Hash of ALI_Annotations, for consistency checks with source traces

   end record;
   --  Part of the information stored in a SID file, and needed to compute
   --  coverage results.

   package SID_Info_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Fingerprint_Type,
        Element_Type => SID_Info);

   type CU_Info (Provider : SCO_Provider := SCO_Provider'First) is record
      Origin : Source_File_Index;
      --  File from which this unit's SCO info comes from.
      --  For compiler-based analysis, this is the LI file; for instrumented
      --  sources, this is the original source file.

      Main_Source : Source_File_Index;
      --  Name of main source file.
      --  For Ada this is a simple name; for C this is either a simple name
      --  or full name, depending on whether the information is available.

      Deps : SFI_Vector;
      --  Mapping of this unit's dependency numbers to source file indices

      Has_Code : Boolean := False;
      --  Set True when object code for some source file in this unit is seen

      PP_Info_Map : SCO_PP_Info_Maps.Map;
      --  Information about preprocessing

      Scope_Entities : Scope_Entities_Tree := Scope_Entities_Trees.Empty_Tree;
      --  Scope tree, used to output e.g. subprogram metrics

      ALI_Annotations : ALI_Annotation_Maps.Map;
      --  List of annotations for the unit

      SCOs : SCO_Range_Vectors.Vector;
      --  List of SCOs for this unit

      SIDs_Info : SID_Info_Maps.Map;
      --  Mapping from SCOs fingerprint to SID information. A compilation
      --  unit has various SID versions when it has varying SCOs: this can
      --  happen when including a header file with varying inclusion
      --  contexts (e.g. macro definition varying at the inclusion point).
      --  Note that this is exclusively computing coverage results (e.g.
      --  analyzing a trace along with SID files).
      --
      --  Note that we only use the SID_Info when computing coverage results
      --  resulting from instrumentation (and thus analyzing traces with SID
      --  files). When retrieving coverage results from checkpoint, we only
      --  need the list of SCOs fingerprints for quicker consolidation, so
      --  this map elements will be empty.

      case Provider is
         when Compiler | LLVM =>
            SCOs_Fingerprint : Fingerprint_Type;
            --  Hash of SCO info in checkpoint, for incremental coverage
            --  consistency check.

         when Instrumenter =>
            Source_Fingerprint : Fingerprint_Type;
            --  Hash of source code in checkpoint, for incremental coverage
            --  consistency check. Note that contrarily to binary traces, we
            --  can have a varying set of SCOs from a checkpoint to another,
            --  so we rely on the source code fingerprint rather than the
            --  SCOs.

            True_Static_SCOs  : SCO_Sets.Set;
            False_Static_SCOs : SCO_Sets.Set;

      end case;
   end record;

   procedure Free (CU : in out CU_Info);

   function Has_SCOs (CUI : CU_Info) return Boolean
   is (CUI.SCOs.Length > 0
       and then CUI.SCOs.First_Element.First <= CUI.SCOs.First_Element.Last);

   function Are_Bit_Maps_In_Range
     (Bit_Maps : CU_Bit_Maps; CU : CU_Info) return Boolean;
   --  Return whether all SCOs referenced in Bit_Maps belong to CU

   package CU_Info_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Valid_CU_Id,
        Element_Type => CU_Info);

   package CU_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Source_File_Index,
        Element_Type => Valid_CU_Id);

   CU_Map : CU_Maps.Map;
   --  Map of source file names to CU_Vector indices. Note: there may be
   --  multiple CU_Map entries designating the same LI file (case of an
   --  extended main source unit comprising more than one source file).
   --  Also note that for any source file containing SCOs, the simple name
   --  appears as a key in this map. In addition, for C files, the *full*
   --  name of each main source file also appears as a key.

   package CU_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Valid_CU_Id);

   package Origin_To_CUs_Maps is new
     Ada.Containers.Ordered_Maps
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

   package Nat_Range_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Nat_Range);

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
      --  When SCOs_Trace is active, buffer to hold the bytes used to compute
      --  the fingerprint.
   end record;
   --  Information about a compilation unit to load, i.e. to create a CU_Info
   --  record and the associated information.

   type CU_Load_Info_Access is access all CU_Load_Info;

   package CU_Load_Info_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => CU_Load_Info_Access);

   procedure Free (Infos : in out CU_Load_Info_Vectors.Vector);
   --  Free all resources allocated in Infos and make it empty

   function Main_Source_For
     (Unit : SCOs.SCO_Unit_Table_Entry; Deps : SFI_Vector)
      return Source_File_Index;
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

   package Ignored_Slocs_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Source_Location);

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

      Dom_SCO               : SCO_Id := No_SCO_Id;
      Dom_Sloc              : Source_Location := No_Location;
      Dom_Val               : Tristate := Unknown;
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
      Provider      : SCO_Provider;
      Attached_Ctx  : Instr_Attached_Ctx := No_Attached_Ctx);
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
            --  For a decision, pointer to the enclosing statement (or
            --  condition in the case of a nested decision), unset if decision
            --  is part of a flow control structure.
            --
            --  For a condition or operator, pointer to the enclosing operator,
            --  or to enclosing decision if at top level.

            Scope : Scope_Entities_Trees.Cursor :=
              Scope_Entities_Trees.No_Element;
            --  This fields's purpose is to memoize the result of Traverse_SCO
            --  for finding the scope of the given SCO.
            --  It should not be used nor modified outside the function.
            --  It should not be written to checkpoint files.

            case Kind is
               when Removed =>
                  null;

               when Statement =>
                  S_Kind : Statement_Kind := Statement_Kind'First;
                  --  Statement kind indication

                  Dominant       : SCO_Id := No_SCO_Id;
                  Dominant_Value : Tristate := Unknown;
                  --  Previous statement in sequence, or dominant decision. See
                  --  comment for function Dominant. Dominant_Value is Unknown
                  --  for a statement dominant, or a valid boolean value for a
                  --  decision dominant.

                  Dominant_Sloc : Source_Location := No_Location;
                  --  While SCOs are being read, we only get the sloc of the
                  --  dominant and store it here. We set the Dominant component
                  --  later on after the Sloc -> SCO map has been constructed.

                  Handler_Range : Source_Location_Range := No_Range;
                  --  Sloc range of the exception handler of which this is the
                  --  first statement.

                  Pragma_Name : Pragma_Id := Pragma_Id'First;
                  --  For a Pragma_Statement, corresponding pragma identifier

                  Stmt_Instrumented : Boolean := True;
                  --  Whether this SCO was instrumented

               when Condition =>
                  Value : Tristate;
                  --  Indicates whether this condition is always true, always
                  --  false, or tested at run time (Unknown).

                  PC_Set : PC_Sets.Set;
                  --  Addresses of conditional branches testing this condition
                  --  (if Value = Unknown).

                  BDD_Node : BDD_Node_Id;
                  --  Associated node in the decision's BDD

                  Index : Condition_Index;
                  --  Index of this condition in the decision

               when Decision =>
                  Expression : SCO_Id := No_SCO_Id;
                  --  Top expression node for this decision. This refers to the
                  --  first condition SCO of the decision.

                  D_Kind : Decision_Kind;
                  --  Decision kind indication

                  Control_Location : Source_Location := No_Location;
                  --  For a decision other than an Expression, sloc of the
                  --  execution flow control construct.

                  Last_Cond_Index : Any_Condition_Index;
                  --  Index of last condition in decision (should be > 0 for
                  --  complex decisions, = 0 otherwise).

                  Decision_BDD : BDD.BDD_Type;
                  --  BDD of the decision

                  Degraded_Origins : Boolean := False;
                  --  Set True for the case of a single-condition decision,
                  --  whose conditional branch instructions have origins (i.e.
                  --  condition value labels) set modulo an arbitrary negation.

                  Aspect_Name : Aspect_Id := No_Aspect;
                  --  For an aspect decision, name of the aspect

                  Path_Count : Natural := 0;
                  --  Count of distinct paths through the BDD from the root
                  --  condition to any outcome.

                  Decision_Instrumented : Boolean := True;
                  --  Whether this SCO was instrumented for decision coverage

                  Decision_Instrumented_For_MCDC : Boolean := True;
                  --  Whether this SCO was instrumented for MC/DC coverage

               when Operator =>
                  Operands : Operand_Pair := (others => No_SCO_Id);
                  --  Operands of this operator

                  Op_Kind : Operator_Kind;
                  --  Kind of operation this node represents

               when Fun_Call_SCO_Kind =>
                  Is_Expr : Boolean := False;
                  --  Set to true if it is a call and an expression

                  Fun_Call_Instrumented : Boolean := True;
                  --  Whether this function or call SCO was instrumented

               when Guarded_Expr =>
                  GExpr_Instrumented : Boolean := True;
            end case;
      end case;
   end record;

   Removed_SCO_Descriptor : constant SCO_Descriptor := (Kind => Removed);

   package SCO_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Valid_SCO_Id,
        Element_Type => SCO_Descriptor);

   package SCO_To_CU_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Valid_SCO_Id,
        Element_Type => CU_Id);

   function Next_BDD_Node (SCO : SCO_Id; Value : Boolean) return BDD_Node_Id;
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
     (SCOD : SCO_Descriptor; Enclosing_SCO : SCO_Id) return Boolean;
   --  If Enclosing_SCO is No_SCO_Id, return False. Otherwise, assume that
   --  SCOD and Enclosing_SCO share part of their sloc range. Issue a warning
   --  and return True if this is an invalid case of overlap (if both SCOs
   --  overlap without nesting).

   procedure Prealloc_Lines
     (Cur_Source_File : Source_File_Index; Last_Line : in out Natural);
   --  Pre-allocate line table entries for Cur_Source_File to accomodate
   --  Last_Line (optimization only). Last_Line is reset to 0.

   procedure Add_SCO_To_Lines (SCO : SCO_Id; SCOD : SCO_Descriptor);
   --  Link the given SCO and SCOD to the corresponding entries in line tables

   --  Source_Coverage_Vectors holds all SCO-related data. This holder can
   --  contain data loaded from a checkpoint.

   type Source_Coverage_Vectors is record
      CU_Vector        : CU_Info_Vectors.Vector;
      BDD_Vector       : BDD.BDD_Vectors.Vector;
      SCO_Vector       : SCO_Vectors.Vector;
      SCO_To_CU_Vector : SCO_To_CU_Vectors.Vector;
   end record;

   function Index
     (Vectors : Source_Coverage_Vectors; SCO : SCO_Id) return Condition_Index;
   --  Internal definition of the function free from global variables

   function Condition
     (Vectors : Source_Coverage_Vectors; SCO : SCO_Id; Index : Condition_Index)
      return SCO_Id;
   --  Internal definition of the function free from global variables

   function Enclosing
     (Vectors : Source_Coverage_Vectors; What : SCO_Kind; SCO : SCO_Id)
      return SCO_Id;
   --  Internal definition of the function free from global variables

   function Next_BDD_Node
     (Vectors : Source_Coverage_Vectors; SCO : SCO_Id; Value : Boolean)
      return BDD_Node_Id;
   --  Internal definition of the function free from global variables

   function Outcome
     (Vectors : Source_Coverage_Vectors; SCO : SCO_Id; Value : Boolean)
      return Tristate;
   --  Internal definition of the function free from global variables

   function Value
     (Vectors : Source_Coverage_Vectors; SCO : SCO_Id) return Tristate;
   --  Internal definition of the function free from global variables

   -----------------------------------------
   -- Helper routines for Checkpoint_Load --
   -----------------------------------------

   procedure Read is new
     Read_Vector
       (Index_Type   => Pos,
        Element_Type => Source_File_Index,
        "="          => "=",
        Vectors      => SFI_Vectors,
        Read_Element => Read);

   procedure Read
     (CLS : in out Checkpoint_Load_State; Value : out Expansion_Info);
   --  Read an Expansion_Info from CLS

   procedure Read (CLS : in out Checkpoint_Load_State; Value : out PP_Info);
   --  Read a PP_Info from CLS

   procedure Read
     (CLS : in out Checkpoint_Load_State; Fingerprint : out Fingerprint_Type);
   --  Wrapper around CLS.Read_Fingerprint to accomodate for the Read_Map
   --  instantiation below.

   procedure Read is new
     Read_Map
       (Key_Type     => SCO_Id,
        Element_Type => PP_Info,
        Map_Type     => SCO_PP_Info_Maps.Map,
        Clear        => SCO_PP_Info_Maps.Clear,
        Insert       => SCO_PP_Info_Maps.Insert,
        Read_Key     => Read,
        Read_Element => Read);

   procedure Read
     (CLS : in out Checkpoint_Load_State; Value : out Scope_Entity);
   --  Read a Scope_Entity from CLS

   procedure Read is new
     Read_Tree
       (Element_Type   => Scope_Entity,
        "="            => "=",
        Multiway_Trees => Scope_Entities_Trees,
        Read_Element   => Read);

   procedure Read (CLS : in out Checkpoint_Load_State; Value : out SCO_Range);
   --  Read a SCO_Range from CLS

   procedure Read is new
     Read_Vector
       (Index_Type   => Positive,
        Element_Type => SCO_Range,
        "="          => "=",
        Vectors      => SCO_Range_Vectors,
        Read_Element => Read);

   procedure Read (CLS : in out Checkpoint_Load_State; Value : out SID_Info);
   --  Read a SID_Info from CLS

   procedure Read is new
     Read_Map
       (Key_Type     => Fingerprint_Type,
        Element_Type => SID_Info,
        Map_Type     => SID_Info_Maps.Map,
        Clear        => SID_Info_Maps.Clear,
        Insert       => SID_Info_Maps.Insert,
        Read_Key     => Read,
        Read_Element => Read);
   procedure Read (CLS : in out Checkpoint_Load_State; Value : out CU_Info);
   --  Read a CU_Info from CLS

   procedure Read is new
     Read_Vector (Valid_CU_Id, CU_Info, "=", CU_Info_Vectors, Read);
   --  Read a vector of CU_Info records from CLS and append them to CU_Vector

   procedure Read is new
     Read_Set
       (Element_Type => Pc_Type,
        Set_Type     => PC_Sets.Set,
        Clear        => PC_Sets.Clear,
        Insert       => PC_Sets.Insert,
        Read_Element => Read);

   procedure Read
     (CLS : in out Checkpoint_Load_State; Element : out SCO_Descriptor);
   --  Read a SCO_Descriptor from CLS

   procedure Read is new
     Read_Set
       (Element_Type => SCO_Id,
        Set_Type     => SCO_Sets.Set,
        Clear        => SCO_Sets.Clear,
        Insert       => SCO_Sets.Insert,
        Read_Element => Read);

   procedure Read is new
     Read_Vector
       (Index_Type   => Valid_SCO_Id,
        Element_Type => SCO_Descriptor,
        "="          => "=",
        Vectors      => SCO_Vectors,
        Read_Element => Read);

   procedure Read is new
     Read_Vector
       (Index_Type   => Positive,
        Element_Type => SCO_Id,
        Vectors      => SCO_Id_Vectors,
        Read_Element => Read);

   procedure Read is new
     Read_Vector
       (Index_Type   => Positive,
        Element_Type => SCO_Id_Vectors.Vector,
        "="          => SCO_Id_Vectors."=",
        Vectors      => SCO_Id_Vector_Vectors,
        Read_Element => Read);

   procedure Read
     (CLS : in out Checkpoint_Load_State; Value : out ALI_Annotation);
   --  Read a ALI_Annotation from CLS

   procedure Read is new
     Checkpoints.Read_Map
       (Key_Type     => Source_Location,
        Element_Type => ALI_Annotation,
        Map_Type     => ALI_Annotation_Maps.Map,
        Clear        => ALI_Annotation_Maps.Clear,
        Insert       => ALI_Annotation_Maps.Insert,
        Read_Key     => Read,
        Read_Element => Read);
   --  Read a ALI_Annotation_Maps.Map from CLS

   procedure Remap_BDD
     (CP_Vectors   : Source_Coverage_Vectors;
      Relocs       : in out Checkpoint_Relocations;
      Decision_BDD : in out BDD.BDD_Type);
   --  Remap a sequence of BDD nodes, for a whole decision BDD

   procedure Remap_BDD_Node
     (Relocs : Checkpoint_Relocations; B : in out BDD_Node_Id);
   --  Remap a BDD node id

   procedure Remap_SCO_Id (Relocs : Checkpoint_Relocations; S : in out SCO_Id);
   --  Remap a SCO_Id. Note: this assumes possible forward references, and
   --  does not rely on SCO_Map.

   procedure Remap_SCO_Descriptor
     (CP_Vectors : Source_Coverage_Vectors;
      Relocs     : in out Checkpoint_Relocations;
      SCOD       : in out SCO_Descriptor);
   --  Remap one SCO_Descriptor. Note that this assumes that
   --  SCOD.Sloc_Range.Source_File has already been remapped.
   --
   --  Note that this expects condition SCOs of a decision to have been
   --  remapped calling Remap_SCO_Descriptor

   function Sloc_Range_For_SCO
     (SCOD : SCO_Descriptor) return Source_Location_Range;
   --  Get the Source_Location_Range to search for in the Sloc_To_SCO map for
   --  the given SCO, accounting for the specific case of decision with
   --  a control location (when using binary traces): in this case, the
   --  decision are located at their control location rather than the actual
   --  decision location.

   function Check_SCOs_Consistency
     (CLS        : in out Checkpoint_Load_State;
      CP_Vectors : Source_Coverage_Vectors;
      CP_CU      : in out CU_Info) return Boolean;
   --  Check the consistency of the SCO in CP_CU wrt. SCOs previously loaded
   --  for the a different version of the same unit. When a new SCO overlaps
   --  with an existing one, this breaks the consistency of SCOs. Note that we
   --  accept no SCO overlaps (thus no nesting) across compilation unit
   --  versions.

   procedure Checkpoint_Load_SCOs
     (CLS        : in out Checkpoint_Load_State;
      CP_Vectors : Source_Coverage_Vectors;
      CP_CU      : in out CU_Info;
      Real_CU    : in out CU_Info;
      Real_CU_Id : CU_Id);
   --  Load the SCOs in CP_CU and add them to Real_CU, which is the CU entry
   --  for Real_CU_Id.

   procedure Checkpoint_Load_SID_Info
     (CLS     : in out Checkpoint_Load_State;
      CP_CU   : in out CU_Info;
      Real_CU : in out CU_Info);
   --  Load the SID information entries in SIDs_Info and add them to Real_CU

   procedure Checkpoint_Load_PP_Info
     (CLS     : in out Checkpoint_Load_State;
      CP_CU   : in out CU_Info;
      Real_CU : in out CU_Info);
   --  Load the preprocessing information in CP_CU and add them to Real_CU

   procedure Checkpoint_Load_Scopes
     (CLS     : in out Checkpoint_Load_State;
      CP_CU   : in out CU_Info;
      Real_CU : in out CU_Info);
   --  Load the scopes in CP_CU and add them to Real_CU

   procedure Checkpoint_Load_Unit
     (CLS        : in out Checkpoint_Load_State;
      CP_Vectors : Source_Coverage_Vectors;
      CP_CU      : in out CU_Info;
      CP_CU_Id   : CU_Id);
   --  Process one compilation unit from a checkpoint.
   --  CP_CU_Id is the CU_Id in the checkpoint.

   -----------------------------------------
   -- Helper routines for Checkpoint_Save --
   -----------------------------------------

   procedure Write is new
     Write_Vector
       (Index_Type    => Pos,
        Element_Type  => Source_File_Index,
        "="           => "=",
        Vectors       => SFI_Vectors,
        Write_Element => Write_SFI);

   procedure Write
     (CSS : in out Checkpoint_Save_State; Value : Expansion_Info);
   --  Write an Expansion_Info to CSS

   procedure Write (CSS : in out Checkpoint_Save_State; Value : PP_Info);
   --  Write a PP_Info to CSS

   procedure Write is new
     Write_Map
       (Key_Type      => SCO_Id,
        Element_Type  => PP_Info,
        Map_Type      => SCO_PP_Info_Maps.Map,
        Cursor_Type   => SCO_PP_Info_Maps.Cursor,
        Length        => SCO_PP_Info_Maps.Length,
        Iterate       => SCO_PP_Info_Maps.Iterate,
        Query_Element => SCO_PP_Info_Maps.Query_Element,
        Write_Key     => Write_SCO,
        Write_Element => Write);

   procedure Write (CSS : in out Checkpoint_Save_State; Value : Scope_Entity);
   --  Write a Scope_Entity to CSS

   procedure Write is new
     Write_Tree
       (Element_Type   => Scope_Entity,
        "="            => "=",
        Multiway_Trees => Scope_Entities_Trees,
        Write_Element  => Write);

   procedure Write (CSS : in out Checkpoint_Save_State; Value : SID_Info);
   --  Write a SID_Info to CSS

   procedure Write is new
     Write_Map
       (Key_Type      => Fingerprint_Type,
        Element_Type  => SID_Info,
        Map_Type      => SID_Info_Maps.Map,
        Cursor_Type   => SID_Info_Maps.Cursor,
        Length        => SID_Info_Maps.Length,
        Iterate       => SID_Info_Maps.Iterate,
        Query_Element => SID_Info_Maps.Query_Element,
        Write_Key     => Write,
        Write_Element => Write);
   --  Write a vector of SID_Info records to CSS

   procedure Write (CSS : in out Checkpoint_Save_State; Value : SCO_Range);
   --  Write a SCO_Range to CSS

   procedure Write is new
     Write_Vector
       (Index_Type    => Positive,
        Element_Type  => SCO_Range,
        "="           => "=",
        Vectors       => SCO_Range_Vectors,
        Write_Element => Write);
   --  Write a vector of SCO_Range records to CSS

   procedure Write (CSS : in out Checkpoint_Save_State; Value : CU_Info);
   --  Write a CU_Info to CSS

   procedure Write is new
     Write_Vector (Valid_CU_Id, CU_Info, "=", CU_Info_Vectors, Write);
   --  Write a vector of CU_Info records to CSS

   procedure Write is new
     Write_Set
       (Element_Type  => Pc_Type,
        Set_Type      => PC_Sets.Set,
        Cursor_Type   => PC_Sets.Cursor,
        Length        => PC_Sets.Length,
        Iterate       => PC_Sets.Iterate,
        Query_Element => PC_Sets.Query_Element,
        Write_Element => Write_PC);

   procedure Write
     (CSS : in out Checkpoint_Save_State; Value : SCO_Descriptor);
   --  Write a SCO_Descriptor to CSS

   procedure Write is new
     Write_Set
       (Element_Type  => SCO_Id,
        Set_Type      => SCO_Sets.Set,
        Cursor_Type   => SCO_Sets.Cursor,
        Length        => SCO_Sets.Length,
        Iterate       => SCO_Sets.Iterate,
        Query_Element => SCO_Sets.Query_Element,
        Write_Element => Write_SCO);

   procedure Write is new
     Write_Vector
       (Index_Type    => Valid_SCO_Id,
        Element_Type  => SCO_Descriptor,
        "="           => "=",
        Vectors       => SCO_Vectors,
        Write_Element => Write);

   procedure Write is new
     Write_Vector
       (Index_Type    => Positive,
        Element_Type  => SCO_Id,
        Vectors       => SCO_Id_Vectors,
        Write_Element => Write_SCO);

   procedure Write is new
     Write_Vector
       (Index_Type    => Positive,
        Element_Type  => SCO_Id_Vectors.Vector,
        "="           => SCO_Id_Vectors."=",
        Vectors       => SCO_Id_Vector_Vectors,
        Write_Element => Write);

   procedure Write
     (CSS : in out Checkpoint_Save_State; Value : ALI_Annotation);
   --  Write a ALI_Annotation to CSS

   procedure Write is new
     Checkpoints.Write_Map
       (Key_Type      => Source_Location,
        Element_Type  => ALI_Annotation,
        Map_Type      => ALI_Annotation_Maps.Map,
        Cursor_Type   => ALI_Annotation_Maps.Cursor,
        Length        => ALI_Annotation_Maps.Length,
        Iterate       => ALI_Annotation_Maps.Iterate,
        Query_Element => ALI_Annotation_Maps.Query_Element,
        Write_Key     => Write,
        Write_Element => Write);
   --  Write a ALI_Annotation_Maps.Map to CSS

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

   BDD_Vector : BDD.BDD_Vectors.Vector renames SC_Vectors.BDD_Vector;
   --  Vector for BDD nodes (one per BDD node, all BDDs considered)

   SCO_Vector : SCO_Vectors.Vector renames SC_Vectors.SCO_Vector;
   --  Vector of high-level Source Coverage Obligations (for all units)

   SCO_To_CU_Vector : SCO_To_CU_Vectors.Vector renames
     SC_Vectors.SCO_To_CU_Vector;
   --  Mapping of SCO_Id to CU, for performance purposes

   -----------
   -- Image --
   -----------

   function Image (SE : Scope_Entity) return String is
      Identifier_Image : constant String :=
        (if SE.Identifier.Decl_SFI = No_Source_File
         then "ignored"
         else
           "at "
           & Get_Simple_Name (SE.Identifier.Decl_SFI)
           & ":"
           & Img (SE.Identifier.Decl_Line));
   begin
      return
        "Scope for "
        & (+SE.Name)
        & "["
        & Slocs.Image (SE.Sloc)
        & "], identifier "
        & Identifier_Image;
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
            Put_Line
              (Prefix
               & Image (SE)
               & " source range "
               & Image (SE.Source_Range));
         end;
      end loop;
   end Dump;

   ---------------------
   -- Scope_Traversal --
   ---------------------

   function Scope_Traversal (CU : CU_Id) return Scope_Traversal_Type is
      use Scope_Entities_Trees;
   begin
      if CU = No_CU_Id then
         return No_Scope_Traversal;
      end if;
      return
        (CU  => CU,
         Cur => First_Child (CU_Vector.Reference (CU).Scope_Entities.Root));
   end Scope_Traversal;

   ------------
   -- Get_CU --
   ------------

   function Get_CU (ST : Scope_Traversal_Type) return CU_Id
   is (ST.CU);

   ------------------
   -- Traverse_SCO --
   ------------------

   procedure Traverse_SCO (ST : in out Scope_Traversal_Type; SCO : SCO_Id) is
      use Scope_Entities_Trees;
      SCO_Sloc : constant Local_Source_Location := First_Sloc (SCO).L;
   begin
      --  Don't do anything if there is no scope attached to ST.

      if not Has_Element (ST.Cur) then
         return;
      end if;

      declare
         SCOD : SCO_Descriptor renames SCO_Vector (SCO);
      begin

         --  Early return if the SCO's Scope was memoized.

         if SCOD.Scope /= No_Element then
            ST.Cur := SCOD.Scope;
            return;
         end if;

         --  Check whether the current scope covers SCO. If not, find the first
         --  one that does in the parent chain. There is always a scope for the
         --  compilation unit / file, so this is guaranteed to finish.

         while not Covers_SCO (ST, SCO) loop
            ST.Cur := Parent (ST.Cur);

            --  If we reach the root of the tree, this means this SCO is not in
            --  any scope, which is clearly a bug. Unless this is a function
            --  SCO which can be at the root of the tree.

            if Is_Root (ST.Cur) and then SCOD.Kind /= Fun then
               raise Program_Error with "No scope found for " & Image (SCO);
            end if;
         end loop;

         --  Descend into the tree until we are on a leaf, or no child of the
         --  current scope entity covers the SCO.

         Depth_Descent : while not Is_Leaf (ST.Cur) loop

            --  Otherwise, find if there is a child that covers SCO, this is a
            --  linear search. We could search by dichotomy as the children are
            --  sorted, but this seems overkill.
            --  Actually, the Multiway_Tree structure does not allow access
            --  to an arbitrary nth_child (children are a linked list),
            --  so a binary search would need a rewrite of the Scope handling.

            declare
               Child    : Cursor := First_Child (ST.Cur);
               In_Child : Boolean := False;
               --  Whether the low bound of Child is lower than SCO (and thus
               --  Child covers SCO when exiting the loop).
            begin
               Child_Search : while Has_Element (Child) loop
                  declare
                     SE : constant Scope_Entity := Element (Child);
                  begin
                     In_Child := SE.Source_Range.L.First_Sloc <= SCO_Sloc;
                     exit Child_Search when
                       SCO_Sloc <= SE.Source_Range.L.Last_Sloc;
                     Child := Next_Sibling (Child);
                  end;
               end loop Child_Search;

               --  If we found a child containing SCO, keep descending into its
               --  children, otherwise we have found the inner most scope.

               exit Depth_Descent when not In_Child or else Child = No_Element;
               ST.Cur := Child;
               pragma Loop_Invariant (Covers_SCO (ST, SCO));
            end;
         end loop Depth_Descent;

         --  Memoize the Scope in the SCO descriptor.

         SCOD.Scope := ST.Cur;
      end;
   end Traverse_SCO;

   --------------------------
   -- In_Scope_Of_Interest --
   --------------------------

   function In_Scope_Of_Interest
     (ST : in out Scope_Traversal_Type; SCO : SCO_Id) return Boolean
   is
      use Scope_Entities_Trees;
      Cur : Cursor;
   begin
      Traverse_SCO (ST, SCO);

      --  If no subprogram of interest was requested, consider that they are
      --  all of interest.

      if Subps_Of_Interest.Is_Empty then
         return True;
      end if;

      --  Otherwise, find at least one scope that covers SCO and that is a
      --  subprogram of interest.

      Cur := ST.Cur;
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
      for L in
        SCOD.Sloc_Range.L.First_Sloc.Line .. SCOD.Sloc_Range.L.Last_Sloc.Line
      loop
         Add_Line_For_Source_Coverage (SCOD.Sloc_Range.Source_File, L, SCO);
      end loop;
   end Add_SCO_To_Lines;

   ---------------------
   -- Has_Fingerprint --
   ---------------------

   function Has_Fingerprint
     (CU : CU_Id; SCO_Fingerprint : Fingerprint_Type) return Boolean is
   begin
      return
        CU_Vector.Constant_Reference (CU).SIDs_Info.Contains (SCO_Fingerprint);
   end Has_Fingerprint;

   ------------------
   -- Fingerprints --
   ------------------

   function Fingerprints (CU : CU_Id) return Fingerprint_Vectors.Vector is
      Result : Fingerprint_Vectors.Vector;
   begin
      for Cur in CU_Vector.Reference (CU).SIDs_Info.Iterate loop
         Result.Append (SID_Info_Maps.Key (Cur));
      end loop;
      return Result;
   end Fingerprints;

   --------------
   -- Bit_Maps --
   --------------

   function Bit_Maps
     (CU : CU_Id; SCO_Fingerprint : Fingerprint_Type) return CU_Bit_Maps is
   begin
      return
        CU_Vector.Reference (CU).SIDs_Info.Element (SCO_Fingerprint).Bit_Maps;
   end Bit_Maps;

   --------------------------
   -- Bit_Maps_Fingerprint --
   --------------------------

   function Bit_Maps_Fingerprint
     (CU : CU_Id; SCO_Fingerprint : Fingerprint_Type) return Fingerprint_Type
   is
   begin
      return
        CU_Vector.Constant_Reference (CU).SIDs_Info.Element (SCO_Fingerprint)
          .Bit_Maps_Fingerprint;
   end Bit_Maps_Fingerprint;

   -----------------------------
   -- Annotations_Fingerprint --
   -----------------------------

   function Annotations_Fingerprint
     (CU : CU_Id; SCO_Fingerprint : Fingerprint_Type) return Fingerprint_Type
   is
   begin
      return
        CU_Vector.Constant_Reference (CU).SIDs_Info.Element (SCO_Fingerprint)
          .Annotations_Fingerprint;
   end Annotations_Fingerprint;

   ------------
   -- Blocks --
   ------------

   function Blocks
     (CU : CU_Id; SCO_Fingerprint : Fingerprint_Type)
      return SCO_Id_Vector_Vector is
   begin
      return
        CU_Vector.Reference (CU).SIDs_Info.Element (SCO_Fingerprint).Blocks;
   end Blocks;

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

   ----------
   -- Read --
   ----------

   procedure Read
     (CLS : in out Checkpoint_Load_State; Value : out Expansion_Info) is
   begin
      Value.Macro_Name := CLS.Read_Unbounded_String;
      Value.Sloc := CLS.Read_Source_Location;
   end Read;

   procedure Read (CLS : in out Checkpoint_Load_State; Value : out PP_Info) is
      Result : PP_Info (SCO_PP_Kind'Val (CLS.Read_U8));
      Count  : Interfaces.Integer_32;
   begin
      Result.Actual_Source_Range := CLS.Read_Local_Source_Location_Range;
      Result.PP_Source_Range := CLS.Read_Local_Source_Location_Range;

      Result.Expansion_Stack.Clear;
      Count := CLS.Read_I32;
      for I in 1 .. Count loop
         declare
            EI : Expansion_Info;
         begin
            Read (CLS, EI);
            Result.Expansion_Stack.Append (EI);
         end;
      end loop;

      case Result.Kind is
         when In_Expansion =>
            Read (CLS, Result.Definition_Loc);

         when others       =>
            null;
      end case;

      Value := Result;
   end Read;

   procedure Read
     (CLS : in out Checkpoint_Load_State; Fingerprint : out Fingerprint_Type)
   is
   begin
      Fingerprint := CLS.Read_Fingerprint;
   end Read;

   procedure Read (CLS : in out Checkpoint_Load_State; Value : out SID_Info) is
      Stmt_First, DC_First, MCDC_First : Bit_Id;
      Stmt_Last, DC_Last, MCDC_Last    : Any_Bit_Id;
   begin
      --  Read block information

      Read (CLS, Value.Blocks);

      --  Read bit maps

      Stmt_First := CLS.Read_Bit_Id;
      Stmt_Last := CLS.Read_Bit_Id;
      Value.Bit_Maps.Statement_Bits :=
        new Statement_Bit_Map (Stmt_First .. Stmt_Last);
      for SCO of Value.Bit_Maps.Statement_Bits.all loop
         SCO := CLS.Read_SCO;
      end loop;

      DC_First := CLS.Read_Bit_Id;
      DC_Last := CLS.Read_Bit_Id;
      Value.Bit_Maps.Decision_Bits :=
        new Decision_Bit_Map (DC_First .. DC_Last);
      for Info of Value.Bit_Maps.Decision_Bits.all loop
         Info.D_SCO := CLS.Read_SCO;
         Info.Outcome := CLS.Read_Boolean;
      end loop;

      MCDC_First := CLS.Read_Bit_Id;
      MCDC_Last := CLS.Read_Bit_Id;
      Value.Bit_Maps.MCDC_Bits := new MCDC_Bit_Map (MCDC_First .. MCDC_Last);
      for Info of Value.Bit_Maps.MCDC_Bits.all loop
         Info.D_SCO := CLS.Read_SCO;
         Info.Path_Index := CLS.Read_Integer;
      end loop;
      Value.Bit_Maps_Fingerprint := CLS.Read_Fingerprint;
      Value.Annotations_Fingerprint := CLS.Read_Fingerprint;
   end Read;

   procedure Read
     (CLS : in out Checkpoint_Load_State; Value : out Scope_Entity) is
   begin
      Value.Source_Range := CLS.Read_Source_Location_Range;

      Value.Name := CLS.Read_Unbounded_String;
      Value.Sloc := CLS.Read_Local_Source_Location;

      Value.Identifier.Decl_SFI := CLS.Read_SFI;
      Value.Identifier.Decl_Line := CLS.Read_Integer;
   end Read;

   procedure Read (CLS : in out Checkpoint_Load_State; Value : out SCO_Range)
   is
   begin
      Value.First := CLS.Read_SCO;
      Value.Last := CLS.Read_SCO;
   end Read;

   procedure Read (CLS : in out Checkpoint_Load_State; Value : out CU_Info) is
      Element_Template : CU_Info (SCO_Provider'Val (CLS.Read_U8));
   begin
      Value := Element_Template;
      Value.Origin := CLS.Read_SFI;
      Value.Main_Source := CLS.Read_SFI;
      Read (CLS, Value.Deps);
      Value.Has_Code := CLS.Read_Boolean;
      Read (CLS, Value.PP_Info_Map);
      Read (CLS, Value.Scope_Entities);
      Read (CLS, Value.ALI_Annotations);
      Read (CLS, Value.SCOs);
      case Element_Template.Provider is
         when Compiler | LLVM =>
            Value.SCOs_Fingerprint := CLS.Read_Fingerprint;

         when Instrumenter    =>
            Read (CLS, Value.SIDs_Info);
            Value.Source_Fingerprint := CLS.Read_Fingerprint;
      end case;
   end Read;

   procedure Read
     (CLS : in out Checkpoint_Load_State; Element : out SCO_Descriptor)
   is
      SCOD : SCO_Descriptor (SCO_Kind'Val (CLS.Read_U8));
   begin
      if SCOD.Kind = Removed then
         Element := Removed_SCO_Descriptor;
         return;
      end if;

      SCOD.Origin := CLS.Read_CU;
      SCOD.Sloc_Range := CLS.Read_Source_Location_Range;
      SCOD.Parent := CLS.Read_SCO;

      case SCOD.Kind is
         when Removed           =>
            raise Program_Error with "unreachable code";

         when Statement         =>
            SCOD.S_Kind := Statement_Kind'Val (CLS.Read_U8);
            SCOD.Dominant := CLS.Read_SCO;
            SCOD.Dominant_Value := CLS.Read_Tristate;
            SCOD.Dominant_Sloc := CLS.Read_Source_Location;
            SCOD.Handler_Range := CLS.Read_Source_Location_Range;
            SCOD.Pragma_Name := Pragma_Id'Val (CLS.Read_U16);

            --  See the TODO in the declaration of Checkpoint_Version

            SCOD.Stmt_Instrumented := True;

         when Condition         =>
            SCOD.Value := CLS.Read_Tristate;
            Read (CLS, SCOD.PC_Set);
            SCOD.BDD_Node := CLS.Read_BDD_Node;
            SCOD.Index := CLS.Read_Condition;

         when Decision          =>
            SCOD.Expression := CLS.Read_SCO;
            SCOD.D_Kind := Decision_Kind'Val (CLS.Read_U8);
            SCOD.Control_Location := CLS.Read_Source_Location;
            SCOD.Last_Cond_Index := CLS.Read_Condition;
            SC_Obligations.BDD.Read (CLS, SCOD.Decision_BDD);
            SCOD.Degraded_Origins := CLS.Read_Boolean;
            SCOD.Aspect_Name := Aspect_Id'Val (CLS.Read_U8);
            SCOD.Path_Count := CLS.Read_Integer;

            --  See the TODO in the declaration of Checkpoint_Version

            SCOD.Decision_Instrumented := True;
            SCOD.Decision_Instrumented_For_MCDC := True;

         when Operator          =>
            declare
               First : constant Operand_Position :=
                 Operand_Position'Val (CLS.Read_U8);
               Last  : constant Operand_Position :=
                 Operand_Position'Val (CLS.Read_U8);
            begin
               pragma Assert (First = Left);
               pragma Assert (Last = Right);

               SCOD.Operands (Left) := CLS.Read_SCO;
               SCOD.Operands (Right) := CLS.Read_SCO;
            end;
            SCOD.Op_Kind := Operator_Kind'Val (CLS.Read_U8);

         when Fun_Call_SCO_Kind =>
            SCOD.Is_Expr := CLS.Read_Boolean;
            SCOD.Fun_Call_Instrumented := CLS.Read_Boolean;

         when Guarded_Expr      =>
            SCOD.GExpr_Instrumented := CLS.Read_Boolean;
      end case;

      Element := SCOD;
   end Read;

   procedure Read
     (CLS : in out Checkpoint_Load_State; Value : out ALI_Annotation) is
   begin
      Value.Kind := ALI_Annotation_Kind'Val (CLS.Read_U8);

      declare
         Msg : constant String := CLS.Read_String;
      begin
         if Msg'Length > 0 then
            Value.Message := new String'(Msg);
         end if;
      end;

      Value.Violation_Count := 0;
      Value.Undetermined_Cov_Count := 0;
   end Read;

   function "<" (L, R : Static_Decision_Evaluation) return Boolean is
   begin
      if L.Outcome /= R.Outcome then
         return L.Outcome < R.Outcome;
      end if;

      for J in R.Values.First_Index .. R.Values.Last_Index loop
         if J > L.Values.Last_Index then
            return True;

         elsif L.Values.Element (J) < R.Values.Element (J) then
            return True;

         elsif L.Values.Element (J) > R.Values.Element (J) then
            return False;
         end if;
      end loop;

      return False;
   end "<";

   ------------------------
   -- Sloc_Range_For_SCO --
   ------------------------

   function Sloc_Range_For_SCO
     (SCOD : SCO_Descriptor) return Source_Location_Range
   is
      Sloc_Range : Source_Location_Range := SCOD.Sloc_Range;
   begin
      if SCOD.Kind = Decision and then SCOD.Control_Location /= No_Location
      then
         Sloc_Range := To_Range (SCOD.Control_Location, No_Location);
      end if;
      return Sloc_Range;
   end Sloc_Range_For_SCO;

   ----------------------------
   -- Check_SCOs_Consistency --
   ----------------------------

   function Check_SCOs_Consistency
     (CLS        : in out Checkpoint_Load_State;
      CP_Vectors : Source_Coverage_Vectors;
      CP_CU      : in out CU_Info) return Boolean is
   begin
      for SCO_Range of CP_CU.SCOs loop
         for Old_SCO_Id in SCO_Range.First .. SCO_Range.Last loop
            declare
               New_SCOD : constant SCO_Descriptor :=
                 CP_Vectors.SCO_Vector.Element (Old_SCO_Id);
               Kind     : Any_SCO_Kind renames New_SCOD.Kind;

               Sloc_Range : Source_Location_Range :=
                 Sloc_Range_For_SCO (New_SCOD);

               Real_SCO : SCO_Id := No_SCO_Id;
               --  Value of the SCO if it already exists, otherwise left to
               --  No_SCO_Id.

            begin
               --  Remap SFIs in all source locations

               Remap_SFI (CLS.Relocations, Sloc_Range.Source_File);

               --  Only consider statement / decision / condition coverage
               --  obligations as other SCO kinds are not considered by
               --  Sloc_To_SCO and this is a good enough condition to ensure
               --  SCOs consistency.

               if Kind not in Statement .. Condition then
                  goto Continue;
               end if;

               --  Try to find if the SCO already exists, or if there is an
               --  existing SCO at the same location but not with the same
               --  source range.

               --  First, try to find if we have a SCO of the exact same kind
               --  and range.

               declare
                  use Sloc_To_SCO_Maps;
                  Cur : constant Cursor :=
                    Sloc_To_SCO_Map (Sloc_Range.Source_File, Kind).Find
                      (Sloc_Range.L);
               begin
                  if Has_Element (Cur)
                    and then SCO_Vector.Element (Element (Cur)).Kind = Kind
                  then
                     goto Continue;
                  end if;
               end;

               --  If we did not find a SCO with the exact same range, check
               --  whether there is an overlapping SCO.

               Real_SCO :=
                 Sloc_To_SCO
                   (Slocs.Source_Location'
                      (Source_File => Sloc_Range.Source_File,
                       L           => New_SCOD.Sloc_Range.L.First_Sloc),
                    Include_Decisions => True);

               if Real_SCO /= No_SCO_Id then
                  return False;
               end if;
            end;
            <<Continue>>
         end loop;
      end loop;
      return True;
   end Check_SCOs_Consistency;

   --------------------------
   -- Checkpoint_Load_SCOs --
   --------------------------

   procedure Checkpoint_Load_SCOs
     (CLS        : in out Checkpoint_Load_State;
      CP_Vectors : Source_Coverage_Vectors;
      CP_CU      : in out CU_Info;
      Real_CU    : in out CU_Info;
      Real_CU_Id : CU_Id)
   is
      Relocs : Checkpoint_Relocations renames CLS.Relocations;

      Cur_Source_File : Source_File_Index := No_Source_File;
      Last_Line       : Natural := 0;

      New_First_SCO : constant SCO_Id := SCO_Vector.Last_Index + 1;
   begin
      --  When loading a SCO, we check if it already exists:
      --     * If it is the case, merge it and adequately set the bit map
      --       relocation to the existing SCO.
      --     * Otherwise, create it.
      --
      --  The heuristic to consider that a SCO already exists or not is to
      --  check for a SCO with the exact same source location.

      for SCO_Range of CP_CU.SCOs loop
         for Old_SCO_Id in SCO_Range.First .. SCO_Range.Last loop
            declare
               New_SCOD   : SCO_Descriptor :=
                 CP_Vectors.SCO_Vector.Element (Old_SCO_Id);
               Sloc_Range : Source_Location_Range :=
                 Sloc_Range_For_SCO (New_SCOD);
               Real_SCO   : SCO_Id := No_SCO_Id;
               --  Value of the SCO if it already exists, otherwise left to
               --  No_SCO_Id.

            begin
               if New_SCOD.Kind = Removed then
                  Ignore_SCO (Relocs, Old_SCO_Id);
                  goto Next_SCO;
               end if;

               --  Remap SFIs in all source locations

               Remap_SFI (Relocs, Sloc_Range.Source_File);

               --  Try to find if the SCO already exists

               declare
                  use Sloc_To_SCO_Maps;
                  Cur : constant Cursor :=
                    Sloc_To_SCO_Map (Sloc_Range.Source_File, New_SCOD.Kind)
                      .Find (Sloc_Range.L);
               begin
                  if Has_Element (Cur) then
                     Real_SCO := Element (Cur);
                  end if;
               end;

               --  If the SCO already exists, only add an entry to remap the
               --  Old_SCO_Id to the actual SCO.

               if Real_SCO /= No_SCO_Id then
                  Set_SCO_Id_Map (Relocs, Old_SCO_Id, Real_SCO);
                  goto Next_SCO;
               end if;

               --  At this point, if Real_SCO is No_SCO_Id, this is a new SCO:
               --  it cannot overlap with an existing SCO as this was checked
               --  by Check_SCOs_Consistency.

               if Real_SCO = No_SCO_Id then
                  Set_SCO_Id_Map
                    (Relocs, Old_SCO_Id, SCO_Vector.Last_Index + 1);

                  --  Append new SCOD and record mapping

                  Remap_SCO_Descriptor (CP_Vectors, Relocs, New_SCOD);

                  --  Preallocate line table entries for previous unit

                  if New_SCOD.Sloc_Range.Source_File /= Cur_Source_File then
                     Prealloc_Lines (Cur_Source_File, Last_Line);
                     Cur_Source_File := New_SCOD.Sloc_Range.Source_File;
                     CU_Map.Include (Cur_Source_File, Real_CU_Id);
                  end if;
                  if New_SCOD.Kind
                     in Statement | Decision | Fun_Call_SCO_Kind | Guarded_Expr
                  then
                     Add_SCO_To_Lines (SCO_Vector.Last_Index + 1, New_SCOD);
                  end if;

                  Last_Line :=
                    Natural'Max
                      (Files_Table.Last_Line (Get_File (Cur_Source_File)),
                       New_SCOD.Sloc_Range.L.Last_Sloc.Line);
                  SCO_Vector.Append (New_SCOD);
                  SCO_To_CU_Vector.Append (Real_CU_Id);

                  --  Add it into the Sloc_To_SCO_Map

                  declare
                     Map : constant access Sloc_To_SCO_Maps.Map :=
                       Writeable_Sloc_To_SCO_Map
                         (Sloc_Range.Source_File, New_SCOD.Kind);
                  begin
                     Map.Insert (Sloc_Range.L, SCO_Vector.Last_Index);
                  end;

                  if SCOs_Trace.Is_Active then
                     SCOs_Trace.Trace
                       ("Loaded from checkpoint: "
                        & Image (SCO_Vector.Last_Index)
                        & " (was #"
                        & Trim (Old_SCO_Id'Img, Side => Ada.Strings.Both)
                        & " in checkpoint)");
                  end if;
               end if;
            end;
            <<Next_SCO>>
         end loop;
      end loop;

      --  Add the newly created SCOs to Real_CU.SCOs

      if SCO_Vector.Last_Index >= New_First_SCO then
         Real_CU.SCOs.Append
           (SCO_Range'(First => New_First_SCO, Last => SCO_Vector.Last_Index));
      end if;
   end Checkpoint_Load_SCOs;

   ------------------------------
   -- Checkpoint_Load_SID_Info --
   ------------------------------

   procedure Checkpoint_Load_SID_Info
     (CLS     : in out Checkpoint_Load_State;
      CP_CU   : in out CU_Info;
      Real_CU : in out CU_Info)
   is
      Relocs : Checkpoint_Relocations renames CLS.Relocations;
   begin
      for Cur in CP_CU.SIDs_Info.Iterate loop
         declare
            SID_Fingerprint : constant Fingerprint_Type :=
              SID_Info_Maps.Key (Cur);
            SID_Version     : SID_Info := SID_Info_Maps.Element (Cur);
         begin
            if not Real_CU.SIDs_Info.Contains (SID_Fingerprint) then

               --  Remap bit map buffers

               if SID_Version.Bit_Maps.Statement_Bits /= null then
                  for S_SCO of SID_Version.Bit_Maps.Statement_Bits.all loop
                     Remap_SCO_Id (Relocs, S_SCO);
                  end loop;
               end if;

               if SID_Version.Bit_Maps.Decision_Bits /= null then
                  for D_Outcome of SID_Version.Bit_Maps.Decision_Bits.all loop
                     Remap_SCO_Id (Relocs, D_Outcome.D_SCO);
                  end loop;
               end if;

               if SID_Version.Bit_Maps.MCDC_Bits /= null then
                  for D_Path of SID_Version.Bit_Maps.MCDC_Bits.all loop
                     Remap_SCO_Id (Relocs, D_Path.D_SCO);
                  end loop;
               end if;

               --  Remap blocks information

               for Block_Cur in SID_Version.Blocks.Iterate loop
                  declare
                     Block_Ref :
                       constant SCO_Id_Vector_Vectors.Reference_Type :=
                         SID_Version.Blocks.Reference (Block_Cur);
                  begin
                     for SCO_Cur in Block_Ref.Iterate loop
                        Remap_SCO_Id (Relocs, Block_Ref.Reference (SCO_Cur));
                     end loop;
                  end;
               end loop;

               Real_CU.SIDs_Info.Insert (SID_Fingerprint, SID_Version);
            end if;
         end;
      end loop;
   end Checkpoint_Load_SID_Info;

   -----------------------------
   -- Checkpoint_Load_PP_Info --
   -----------------------------

   procedure Checkpoint_Load_PP_Info
     (CLS     : in out Checkpoint_Load_State;
      CP_CU   : in out CU_Info;
      Real_CU : in out CU_Info)
   is
      use SCO_PP_Info_Maps;
      Relocs : Checkpoint_Relocations renames CLS.Relocations;
   begin
      for Cur in CP_CU.PP_Info_Map.Iterate loop
         declare
            SCO  : SCO_Id := Key (Cur);
            Info : PP_Info := Element (Cur);
         begin
            if not Real_CU.PP_Info_Map.Contains (SCO) then
               if Info.Kind = In_Expansion then
                  for Expansion of Info.Expansion_Stack loop
                     Remap_SFI (Relocs, Expansion.Sloc.Source_File);
                  end loop;
                  Remap_SFI (Relocs, Info.Definition_Loc.Sloc.Source_File);
               end if;
               Remap_SCO_Id (Relocs, SCO);
               Real_CU.PP_Info_Map.Insert (SCO, Info);
            end if;
         end;
      end loop;
   end Checkpoint_Load_PP_Info;

   ----------------------------
   -- Checkpoint_Load_Scopes --
   ----------------------------

   procedure Checkpoint_Load_Scopes
     (CLS     : in out Checkpoint_Load_State;
      CP_CU   : in out CU_Info;
      Real_CU : in out CU_Info)
   is
      Relocs : Checkpoint_Relocations renames CLS.Relocations;
   begin
      --  For scope entities, only add those that do not violate the
      --  nesting and ordering of the structure

      for Scope_Ent of CP_CU.Scope_Entities loop

         --  Scopes whose identifier references excluded source files will
         --  lose their identifier: such scopes will remain, but users
         --  will not be able to mark them of interest.

         if SFI_Ignored (Relocs, Scope_Ent.Identifier.Decl_SFI) then
            Scope_Ent.Identifier := No_Scope_Entity_Identifier;
         else
            Remap_SFI (Relocs, Scope_Ent.Identifier.Decl_SFI);
         end if;

         Remap_SFI (Relocs, Scope_Ent.Source_Range.Source_File);

         declare
            use Scope_Entities_Trees;

            Real_Scope : constant Cursor :=
              Floor
                (Real_CU.Scope_Entities,
                 Source_Location'
                   (Source_File => Scope_Ent.Source_Range.Source_File,
                    L           => Scope_Ent.Source_Range.L.First_Sloc));

            Added_Scope : Boolean := False;
            --  Whether the current Scope_Ent was added to the list of scopes
            --  (it is a new scope that does not clash with existing scopes).

         begin
            if not Scope_Entities_Trees.Has_Element (Real_Scope) then

               --  This is a new scope that do not nest with an existing
               --  scope (basically the case of a new compilation unit).

               Real_CU.Scope_Entities.Insert_Child
                 (Parent   => Real_CU.Scope_Entities.Root,
                  Before   => No_Element,
                  New_Item => Scope_Ent);
               Added_Scope := True;
            else
               declare
                  Found_Scope_Ent : constant Scope_Entity :=
                    Real_CU.Scope_Entities.Reference (Real_Scope);
                  Child           : Cursor := First_Child (Real_Scope);
                  Src_Range       : constant Source_Location_Range :=
                    Found_Scope_Ent.Source_Range;
               begin

                  if Scope_Ent.Source_Range = Src_Range then

                     --  The scope already exists: do nothing

                     null;

                  elsif Src_Range.L.First_Sloc
                    < Scope_Ent.Source_Range.L.First_Sloc
                    and then
                      Scope_Ent.Source_Range.L.Last_Sloc
                      < Src_Range.L.Last_Sloc
                  then
                     --  Check if it nests with the innermost scope entity. If
                     --  this is the case, insert it as a child.

                     while Has_Element (Child)
                       and then
                         Element (Child).Source_Range.L.First_Sloc
                         < Scope_Ent.Source_Range.L.First_Sloc
                     loop
                        Child := Next_Sibling (Child);
                     end loop;
                     Added_Scope := True;

                     if not Has_Element (Child) then

                        --  Insert it as the last children

                        Real_CU.Scope_Entities.Insert_Child
                          (Parent   => Real_Scope,
                           Before   => No_Element,
                           New_Item => Scope_Ent);

                     else
                        --  Insert it before the identified child

                        Real_CU.Scope_Entities.Insert_Child
                          (Parent   => Real_Scope,
                           Before   => Child,
                           New_Item => Scope_Ent);

                     end if;
                  else
                     --  Discard overlapping cases

                     Outputs.Fatal_Error
                       ("Scope "
                        & (+Scope_Ent.Name)
                        & " at source location: "
                        & Image (Scope_Ent.Source_Range)
                        & " clashes with scope "
                        & (+Found_Scope_Ent.Name)
                        & " at location: "
                        & Image (Found_Scope_Ent.Source_Range));
                  end if;
               end;
            end if;

            --  Register each scope identifiers to make them available to
            --  users on the command line.

            if Added_Scope then
               Available_Subps_Of_Interest.Include (Scope_Ent.Identifier);
            end if;
         end;
      end loop;
      pragma Assert (SCOs_Nested_And_Ordered (Real_CU.Scope_Entities));
   end Checkpoint_Load_Scopes;

   ---------------
   -- Remap_BDD --
   ---------------

   procedure Remap_BDD
     (CP_Vectors   : Source_Coverage_Vectors;
      Relocs       : in out Checkpoint_Relocations;
      Decision_BDD : in out BDD.BDD_Type)
   is
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
                     Remap_BDD_Node_Id (New_BDD_Node.Dests (Valuation));
                  end loop;

               --  Note that we leave New_BDD_Node.C_SCO unremapped here:
               --  the loading of the corresponding SCO condition will
               --  take care of it (see below).

               when BDD.Jump      =>
                  Remap_BDD_Node_Id (New_BDD_Node.Dest);

               when others        =>
                  null;
            end case;

            BDD_Vector.Append (New_BDD_Node);
            Set_BDD_Node_Id_Map
              (Relocs, Old_BDD_Node_Id, BDD_Vector.Last_Index);
         end;
      end loop;

      --  Remap IDs in Decision_BDD

      Remap_SCO_Id (Relocs, Decision_BDD.Decision);

      Remap_BDD_Node (Relocs, Decision_BDD.Root_Condition);
      Remap_BDD_Node (Relocs, Decision_BDD.First_Node);
      Remap_BDD_Node (Relocs, Decision_BDD.Last_Node);
      Remap_BDD_Node (Relocs, Decision_BDD.First_Multipath_Condition);
   end Remap_BDD;

   --------------------
   -- Remap_BDD_Node --
   --------------------

   procedure Remap_BDD_Node
     (Relocs : Checkpoint_Relocations; B : in out BDD_Node_Id) is
   begin
      if B /= No_BDD_Node_Id then
         B := Remap_BDD_Node_Id (Relocs, B);
         pragma Assert (B /= No_BDD_Node_Id);
      end if;
   end Remap_BDD_Node;

   ------------------
   -- Remap_SCO_Id --
   ------------------

   procedure Remap_SCO_Id (Relocs : Checkpoint_Relocations; S : in out SCO_Id)
   is
   begin
      S := Checkpoints.Remap_SCO_Id (Relocs, S);
   end Remap_SCO_Id;

   --------------------------
   -- Remap_SCO_Descriptor --
   --------------------------

   procedure Remap_SCO_Descriptor
     (CP_Vectors : Source_Coverage_Vectors;
      Relocs     : in out Checkpoint_Relocations;
      SCOD       : in out SCO_Descriptor)
   is
      New_First_SCO : SCO_Id := SCO_Vector.Last_Index + 1;
   begin
      Remap_SFI (Relocs, SCOD.Sloc_Range.Source_File);

      --  If this is a decision, start by recording all of the operator
      --  and condition SCOs in the relocation map, before relocating all
      --  of the components of the SCO_Descriptor.

      case SCOD.Kind is
         when Decision =>
            declare
               Expr_SCO  : SCO_Id := SCOD.Expression - 1;
               Expr_SCOD : SCO_Descriptor :=
                 CP_Vectors.SCO_Vector.Element (Expr_SCO);
            begin
               loop
                  Expr_SCO := Expr_SCO + 1;
                  exit when Expr_SCO > CP_Vectors.SCO_Vector.Last_Index;
                  Expr_SCOD := CP_Vectors.SCO_Vector.Element (Expr_SCO);
                  exit when Expr_SCOD.Kind not in Condition | Operator;
                  New_First_SCO := New_First_SCO + 1;
                  Set_SCO_Id_Map (Relocs, Expr_SCO, New_First_SCO);
               end loop;
            end;

         when others   =>
            null;
      end case;

      SCOD.Origin := Remap_CU_Id (Relocs, SCOD.Origin);

      --  Remap SCO_Ids

      Remap_SCO_Id (Relocs, SCOD.Parent);

      --  Make further adjustments based on SCO kind
      --  In particular reset all components that reference
      --  data that is not saved to checkpoint files (such as
      --  BDD information).

      case SCO_Kind (SCOD.Kind) is
         when Statement                        =>
            Remap_SFI (Relocs, SCOD.Dominant_Sloc.Source_File);
            Remap_SFI (Relocs, SCOD.Handler_Range.Source_File);

            Remap_SCO_Id (Relocs, SCOD.Dominant);

         when Decision                         =>
            Remap_SCO_Id (Relocs, SCOD.Expression);
            Remap_SFI (Relocs, SCOD.Control_Location.Source_File);
            Remap_BDD (CP_Vectors, Relocs, SCOD.Decision_BDD);

         when Operator                         =>
            for Op_SCO in SCOD.Operands'Range loop
               Remap_SCO_Id (Relocs, SCOD.Operands (Op_SCO));
            end loop;

         when Condition                        =>
            Remap_BDD_Node (Relocs, SCOD.BDD_Node);
            Remap_SCO_Id (Relocs, BDD_Vector.Reference (SCOD.BDD_Node).C_SCO);

            SCOD.PC_Set.Clear;

         when Fun_Call_SCO_Kind | Guarded_Expr =>
            null;

      end case;
   end Remap_SCO_Descriptor;

   --------------------------
   -- Checkpoint_Load_Unit --
   --------------------------

   procedure Checkpoint_Load_Unit
     (CLS        : in out Checkpoint_Load_State;
      CP_Vectors : Source_Coverage_Vectors;
      CP_CU      : in out CU_Info;
      CP_CU_Id   : CU_Id)
   is
      Relocs : Checkpoint_Relocations renames CLS.Relocations;

      procedure Ignore_SCOs;
      --  Mark all the SCOs in CP_CU as being ignored. This is useful when
      --  skipping coverage information for a unit because the consistency
      --  checks failed.

      procedure Merge_Decision_SCOs (Old_SCO_Id, New_SCO_Id : SCO_Id)
      with Pre => Kind (New_SCO_Id) = Decision;

      -------------------------
      -- Merge_Decision_SCOs --
      -------------------------

      procedure Merge_Decision_SCOs (Old_SCO_Id, New_SCO_Id : SCO_Id) is
         use SC_Obligations.BDD;

         Old_SCOD : SCO_Descriptor renames CP_Vectors.SCO_Vector (Old_SCO_Id);
         New_SCOD : SCO_Descriptor renames SCO_Vector (New_SCO_Id);

         Old_Reachable : Reachability renames
           Old_SCOD.Decision_BDD.Reachable_Outcomes;
         New_Reachable : Reachability renames
           New_SCOD.Decision_BDD.Reachable_Outcomes;

         function Decision_Static_Eval
           (Vectors : Source_Coverage_Vectors;
            SCO_Dec : SCO_Id;
            Eval    : out Static_Decision_Evaluation) return Boolean;

         procedure Register_Static_Evaluation
           (SCO : SCO_Id; Eval : Static_Decision_Evaluation);

         --------------------------
         -- Decision_Static_Eval --
         --------------------------

         function Decision_Static_Eval
           (Vectors : Source_Coverage_Vectors;
            SCO_Dec : SCO_Id;
            Eval    : out Static_Decision_Evaluation) return Boolean
         is
            SCOD : SCO_Descriptor renames Vectors.SCO_Vector (SCO_Dec);

            Reachable : constant Reachability :=
              SCOD.Decision_BDD.Reachable_Outcomes;

            Outcome : constant Tristate :=
              (if Reachable (False) /= Reachable (True)
               then To_Tristate (Reachable (True))
               else Unknown);

            E : Static_Decision_Evaluation;
         begin

            --  Do not process evaluations if the decision is not at least
            --  partially static.

            if Outcome = Unknown then
               return False;
            end if;

            E.Outcome := To_Boolean (Outcome);

            for J in Condition_Index'First .. SCOD.Last_Cond_Index loop
               declare
                  SCO_C  : constant SCO_Id := Condition (Vectors, SCO_Dec, J);
                  SCOD_C : SCO_Descriptor renames Vectors.SCO_Vector (SCO_C);
               begin

                  --  If an encountered Condition has no Value, then the
                  --  Decision is not fully static, abort processing

                  if SCOD_C.Value = Unknown then
                     E.Values.Clear;
                     return False;
                  end if;

                  E.Values.Append (To_Boolean (SCOD_C.Value));
               end;
            end loop;

            Eval := E;
            return True;
         end Decision_Static_Eval;

         --------------------------------
         -- Register_Static_Evaluation --
         --------------------------------

         procedure Register_Static_Evaluation
           (SCO : SCO_Id; Eval : Static_Decision_Evaluation) is
         begin
            if not CLS.Static_Decision_Evaluations.Contains (SCO) then
               CLS.Static_Decision_Evaluations.Insert
                 (SCO, Static_Decision_Evaluation_Sets.Empty_Set);
            end if;
            CLS.Static_Decision_Evaluations.Reference (SCO).Include (Eval);
         end Register_Static_Evaluation;

         --  Start processing of Merge_Decision_SCOs

      begin
         if Old_SCOD.Decision_Instrumented then
            New_SCOD.Decision_Instrumented := True;
         end if;
         if Old_SCOD.Decision_Instrumented_For_MCDC then
            New_SCOD.Decision_Instrumented_For_MCDC := True;
         end if;

         --  The following code handles merging Decision SCOs that have a
         --  different staticness over the 2 checkpoints that are being merged.
         --
         --  If the reachability of the decision in one of the checkpoints
         --  differs from `Both_Reachable`, it means that at least one of the
         --  two checkpoints has some static conditions and should be handled
         --  with a specific treatment.
         --
         --  Upon encountering a fully-static decision, we need to register its
         --  conditions' values so they can be used as a complementary
         --  evaluation for MC/DC analysis.

         if Old_Reachable /= Both_Reachable
           or else New_Reachable /= Both_Reachable
         then
            SCOs_Trace.Trace
              ("Consolidation encountered a decision SCO"
               & " whose staticness may differ at"
               & Image (New_SCOD.Sloc_Range));
            declare
               Old_Eval : Static_Decision_Evaluation;
               --  Holds the result of the static evaluation of Old_SCO
               --  if Old_Static is True. Otherwise, it is invalid.

               New_Eval : Static_Decision_Evaluation;
               --  Holds the result of the static evaluation of New_SCO
               --  if New_Static is True. Otherwise, it is invalid.

               Old_Static : constant Boolean :=
                 Decision_Static_Eval (CP_Vectors, Old_SCO_Id, Old_Eval);
               New_Static : constant Boolean :=
                 Decision_Static_Eval (SC_Vectors, New_SCO_Id, New_Eval);

            begin

               --  No matter the staticness of the SCOs, we update the
               --  reachability of each outcome by OR-ing the two checkpoints.

               New_Reachable (True) :=
                 New_Reachable (True) or else Old_Reachable (True);

               New_Reachable (False) :=
                 New_Reachable (False) or else Old_Reachable (False);

               if Old_Static then

                  --  If the decision in the Old checkpoint is static,
                  --  add an evaluation to the SCIs corresponding to it.

                  Register_Static_Evaluation (New_SCO_Id, Old_Eval);
               end if;

               if New_Static then

                  --  If the decision in the New checkpoint is static,
                  --  add an evaluation to the SCIs corresponding to it.

                  Register_Static_Evaluation (New_SCO_Id, New_Eval);
               end if;
            end;
         end if;
      end Merge_Decision_SCOs;

      -----------------
      -- Ignore_SCOs --
      -----------------

      procedure Ignore_SCOs is
      begin
         for SCO_Range of CP_CU.SCOs loop
            for SCO in SCO_Range.First .. SCO_Range.Last loop
               Ignore_SCO (Relocs, SCO);
            end loop;
         end loop;
      end Ignore_SCOs;

      Actual_CU_Id : CU_Id;
   begin
      if CP_CU.Provider = Instrumenter then
         Instrumented_Units_Present := True;
      end if;

      --  Remap source file indices

      Remap_SFI (Relocs, CP_CU.Origin);
      Remap_SFI (Relocs, CP_CU.Main_Source);
      for Dep_SFI of CP_CU.Deps loop

         --  Units of interest can depend on units outside of the scope of
         --  code coverage analysis. Keeping track of these introduces clashes
         --  between stubbed units and the real one, so they are excluded from
         --  checkpoints. Hence, allow them to be missing here.

         if not SFI_Ignored (Relocs, Dep_SFI) then
            Remap_SFI (Relocs, Dep_SFI);
         end if;
      end loop;

      --  Next check whether this unit is already known

      Actual_CU_Id := Comp_Unit (CP_CU.Main_Source);

      SCOs_Trace.Trace
        ("Remapped CU: id "
         & Actual_CU_Id'Img
         & ", main source"
         & CP_CU.Main_Source'Img
         & " "
         & Get_Full_Name (CP_CU.Main_Source, Or_Simple => True));

      --  If the CU was already loaded, perform consistency checks prior to
      --  loading it.

      if Actual_CU_Id /= No_CU_Id then
         declare
            CU_Record : CU_Info renames CU_Vector.Reference (Actual_CU_Id);

            function Provider_Image (Provider : SCO_Provider) return String
            is (case Provider is
                  when Compiler     => "ALI file",
                  when Instrumenter => "instrumentation",
                  when LLVM         => "LLVM dump");
            --  Helper to designate SCO providers in an error message

            function CU_Image return String
            is (Get_Simple_Name (CP_CU.Origin)
                & " (from "
                & (+CLS.Filename)
                & ")");
            --  Helper to refer to the compilation unit in an error message

         begin
            Set_CU_Id_Map (Relocs, CP_CU_Id, Actual_CU_Id);

            --  Ignore CU when the provenance of SCOs is inconsistent

            if CP_CU.Provider /= CU_Record.Provider then
               Warn ("inconsistent coverage method for " & CU_Image);
               Warn
                 ("SCOs for this unit come from both "
                  & Provider_Image (CP_CU.Provider)
                  & " and from "
                  & Provider_Image (CU_Record.Provider));
               Ignore_SCOs;
               return;

            --  Ignore also when the fingerprints do not match

            elsif (CP_CU.Provider = Compiler
                   and then
                     CP_CU.SCOs_Fingerprint /= CU_Record.SCOs_Fingerprint)
              or else
                (CP_CU.Provider = Instrumenter
                 and then
                   CP_CU.Source_Fingerprint /= CU_Record.Source_Fingerprint)
            then
               Warn
                 ("unexpected fingerprint, cannot merge coverage"
                  & " information for "
                  & CU_Image);
               Ignore_SCOs;
               return;
            end if;
         end;
      end if;

      --  Load the checkpointed information

      declare
         Relocs : Checkpoint_Relocations renames CLS.Relocations;

         Is_New_CU : constant Boolean :=
           not CU_Map.Contains (CP_CU.Main_Source);
         --  Whether this is a new compilation unit

         Has_New_SCOs : Boolean;
         --  Whether the loaded checkpoint contains additional SCOs for the CU.
         --  In this case, load SCOs and scopes from the loaded checkpoint.

         Real_CU_Id : constant CU_Id :=
           (if Is_New_CU
            then CU_Vector.Last_Index + 1
            else CU_Map.Element (CP_CU.Main_Source));

         type CU_Info_Access is access all CU_Info;
         Real_CU : CU_Info_Access;
         --  Pointer to the update CU_Info in CU_Vector. This is a new entry
         --  into CU_Vector if Is_New_CU, otherwise an existing one.

      begin
         Set_CU_Id_Map (Relocs, CP_CU_Id, Real_CU_Id);

         --  If this is a new compilation unit, add a new entry into the
         --  CU_Vector.

         if Is_New_CU then
            declare
               New_CU : CU_Info (CP_CU.Provider);
            begin
               CU_Vector.Append (New_CU);
            end;
         end if;

         --  Then, retrieve the newly created (or existing) CU

         Real_CU := CU_Info_Access (CU_Vector.Reference (Real_CU_Id).Element);

         --  Check if the unit has new SCOs

         Has_New_SCOs := Is_New_CU;
         if not Is_New_CU then
            for SID_Info in CP_CU.SIDs_Info.Iterate loop
               if not Real_CU.SIDs_Info.Contains (SID_Info_Maps.Key (SID_Info))
               then
                  Has_New_SCOs := True;
                  exit;
               end if;
            end loop;
         end if;

         --  If this is a new CU, initialize the CU fields shared for all
         --  versions.

         if Is_New_CU then
            Real_CU.Origin := CP_CU.Origin;
            Real_CU.Main_Source := CP_CU.Main_Source;
            case Real_CU.Provider is
               when Compiler | LLVM =>
                  Real_CU.SCOs_Fingerprint := CP_CU.SCOs_Fingerprint;

               when Instrumenter    =>
                  Real_CU.Source_Fingerprint := CP_CU.Source_Fingerprint;
            end case;
            CU_Map.Insert (CP_CU.Main_Source, Real_CU_Id);
            for Dep_SFI of Real_CU.Deps loop

               --  Units of interest can depend on units outside of the
               --  scope of code coverage analysis. Keeping track of these
               --  introduces clashes between stubbed units and the real
               --  one, so they are excluded from checkpoints. Hence, allow
               --  them to be missing here.

               if not SFI_Ignored (Relocs, Dep_SFI) then
                  Remap_SFI (Relocs, Dep_SFI);
               end if;
            end loop;
            Register_CU (Real_CU_Id);

         else
            --  Otherwise, check that the SCOs in the new version are
            --  consistent with those previously loaded.

            if not Check_SCOs_Consistency (CLS, CP_Vectors, CP_CU) then
               Outputs.Warn
                 ("Discarding source coverage data for unit "
                  & Get_Full_Name (Real_CU.Main_Source)
                  & " (from "
                  & Get_Full_Name (Real_CU.Origin)
                  & "), loaded from "
                  & (+CLS.Filename));
               return;
            end if;
         end if;

         --  In all cases, load the SCOs: if they already exist in Real_CU,
         --  we will remap the SCOs in the loaded checkpoint to the already
         --  existing ones.

         Checkpoint_Load_SCOs
           (CLS        => CLS,
            CP_Vectors => CP_Vectors,
            CP_CU      => CP_CU,
            Real_CU    => Real_CU.all,
            Real_CU_Id => Real_CU_Id);

         --  If this is a new unit / it contains new SCOs, load additional
         --  information (SID information, preprocessing information, and
         --  scopes).

         if Has_New_SCOs then

            --  Process SID information

            Checkpoint_Load_SID_Info
              (CLS => CLS, CP_CU => CP_CU, Real_CU => Real_CU.all);

            --  Process macro information

            Checkpoint_Load_PP_Info
              (CLS => CLS, CP_CU => CP_CU, Real_CU => Real_CU.all);

            --  Process scopes

            Checkpoint_Load_Scopes
              (CLS => CLS, CP_CU => CP_CU, Real_CU => Real_CU.all);
         end if;

         --  Read uninstrumented SCOs for stmt/decision

         for SCO_Range of CP_CU.SCOs loop
            for Old_SCO_Id in SCO_Range.First .. SCO_Range.Last loop
               declare
                  Old_SCOD   : SCO_Descriptor renames
                    CP_Vectors.SCO_Vector (Old_SCO_Id);
                  New_SCO_Id : constant SCO_Id :=
                    Remap_SCO_Id (Relocs, Old_SCO_Id);
                  SCOD       : SCO_Descriptor renames SCO_Vector (New_SCO_Id);
               begin
                  case SCOD.Kind is
                     when Statement         =>
                        if Old_SCOD.Stmt_Instrumented then
                           SCOD.Stmt_Instrumented := True;
                        end if;

                     when Decision          =>
                        Merge_Decision_SCOs (Old_SCO_Id, New_SCO_Id);

                     when Fun_Call_SCO_Kind =>
                        if Old_SCOD.Fun_Call_Instrumented then
                           SCOD.Fun_Call_Instrumented := True;
                        end if;

                     when Guarded_Expr      =>
                        if Old_SCOD.GExpr_Instrumented then
                           SCOD.GExpr_Instrumented := True;
                        end if;

                     when others            =>
                        null;
                  end case;
               end;
            end loop;
         end loop;

         --  Has_Code indication

         Real_CU.Has_Code := Real_CU.Has_Code or CP_CU.Has_Code;

         --  Remap ALI annotations and then merge them

         declare
            Remapped_Annotations : ALI_Annotation_Maps.Map :=
              CP_CU.ALI_Annotations;
         begin
            Remap_ALI_Annotations (Relocs, Remapped_Annotations);
            for Cur in Remapped_Annotations.Iterate loop
               Real_CU.ALI_Annotations.Include
                 (ALI_Annotation_Maps.Key (Cur),
                  ALI_Annotation_Maps.Element (Cur));
            end loop;
         end;
      end;
   end Checkpoint_Load_Unit;

   -----------
   -- Write --
   -----------

   procedure Write (CSS : in out Checkpoint_Save_State; Value : Expansion_Info)
   is
   begin
      CSS.Write (Value.Macro_Name);
      CSS.Write (Value.Sloc);
   end Write;

   procedure Write (CSS : in out Checkpoint_Save_State; Value : PP_Info) is
   begin
      CSS.Write_U8 (SCO_PP_Kind'Pos (Value.Kind));
      CSS.Write (Value.Actual_Source_Range);
      CSS.Write (Value.PP_Source_Range);

      CSS.Write_Count (Value.Expansion_Stack.Length);
      for EI of Value.Expansion_Stack loop
         Write (CSS, EI);
      end loop;

      case Value.Kind is
         when In_Expansion =>
            Write (CSS, Value.Definition_Loc);

         when others       =>
            null;
      end case;
   end Write;

   procedure Write (CSS : in out Checkpoint_Save_State; Value : Scope_Entity)
   is
   begin
      CSS.Write (Value.Source_Range);

      CSS.Write (Value.Name);
      CSS.Write (Value.Sloc);

      CSS.Write_SFI (Value.Identifier.Decl_SFI);
      CSS.Write_Integer (Value.Identifier.Decl_Line);
   end Write;

   procedure Write (CSS : in out Checkpoint_Save_State; Value : SCO_Range) is
   begin
      CSS.Write_SCO (Value.First);
      CSS.Write_SCO (Value.Last);
   end Write;

   procedure Write (CSS : in out Checkpoint_Save_State; Value : SID_Info) is
   begin
      Write (CSS, Value.Blocks);
      CSS.Write_Bit_Id (Value.Bit_Maps.Statement_Bits.all'First);
      CSS.Write_Bit_Id (Value.Bit_Maps.Statement_Bits.all'Last);
      for SCO of Value.Bit_Maps.Statement_Bits.all loop
         CSS.Write_SCO (SCO);
      end loop;

      CSS.Write_Bit_Id (Value.Bit_Maps.Decision_Bits.all'First);
      CSS.Write_Bit_Id (Value.Bit_Maps.Decision_Bits.all'Last);
      for Info of Value.Bit_Maps.Decision_Bits.all loop
         CSS.Write_SCO (Info.D_SCO);
         CSS.Write (Info.Outcome);
      end loop;

      CSS.Write_Bit_Id (Value.Bit_Maps.MCDC_Bits.all'First);
      CSS.Write_Bit_Id (Value.Bit_Maps.MCDC_Bits.all'Last);
      for Info of Value.Bit_Maps.MCDC_Bits.all loop
         CSS.Write_SCO (Info.D_SCO);
         CSS.Write_Integer (Info.Path_Index);
      end loop;

      CSS.Write (Value.Bit_Maps_Fingerprint);
      CSS.Write (Value.Annotations_Fingerprint);
   end Write;

   procedure Write (CSS : in out Checkpoint_Save_State; Value : CU_Info) is
   begin
      CSS.Write_U8 (SCO_Provider'Pos (Value.Provider));
      CSS.Write_SFI (Value.Origin);
      CSS.Write_SFI (Value.Main_Source);
      Write (CSS, Value.Deps);
      CSS.Write (Value.Has_Code);
      Write (CSS, Value.PP_Info_Map);
      Write (CSS, Value.Scope_Entities);
      Write (CSS, Value.ALI_Annotations);
      Write (CSS, Value.SCOs);

      case Value.Provider is
         when Compiler | LLVM =>
            CSS.Write (Value.SCOs_Fingerprint);

         when Instrumenter    =>
            Write (CSS, Value.SIDs_Info);
            CSS.Write (Value.Source_Fingerprint);
      end case;
   end Write;

   procedure Write (CSS : in out Checkpoint_Save_State; Value : SCO_Descriptor)
   is
   begin
      CSS.Write_U8 (SCO_Kind'Pos (Value.Kind));
      if Value.Kind = Removed then
         return;
      end if;

      CSS.Write_CU (Value.Origin);
      CSS.Write (Value.Sloc_Range);
      CSS.Write_SCO (Value.Parent);

      case Value.Kind is
         when Removed           =>
            raise Program_Error with "unreachable code";

         when Statement         =>
            CSS.Write_U8 (Statement_Kind'Pos (Value.S_Kind));
            CSS.Write_SCO (Value.Dominant);
            CSS.Write (Value.Dominant_Value);
            CSS.Write (Value.Dominant_Sloc);
            CSS.Write (Value.Handler_Range);
            CSS.Write_U16 (Pragma_Id'Pos (Value.Pragma_Name));

         when Condition         =>
            CSS.Write (Value.Value);
            Write (CSS, Value.PC_Set);
            CSS.Write_BDD_Node (Value.BDD_Node);
            CSS.Write_Condition (Value.Index);

         when Decision          =>
            CSS.Write_SCO (Value.Expression);
            CSS.Write_U8 (Decision_Kind'Pos (Value.D_Kind));
            CSS.Write (Value.Control_Location);
            CSS.Write_Condition (Value.Last_Cond_Index);
            BDD.Write (CSS, Value.Decision_BDD);
            CSS.Write (Value.Degraded_Origins);
            CSS.Write_U8 (Aspect_Id'Pos (Value.Aspect_Name));
            CSS.Write_Integer (Value.Path_Count);

         when Operator          =>
            CSS.Write_U8 (Operand_Position'Pos (Left));
            CSS.Write_U8 (Operand_Position'Pos (Right));
            CSS.Write_SCO (Value.Operands (Left));
            CSS.Write_SCO (Value.Operands (Right));
            CSS.Write_U8 (Operator_Kind'Pos (Value.Op_Kind));

         when Fun_Call_SCO_Kind =>
            CSS.Write (Value.Is_Expr);
            CSS.Write (Value.Fun_Call_Instrumented);

         when Guarded_Expr      =>
            CSS.Write (Value.GExpr_Instrumented);
      end case;
   end Write;

   procedure Write (CSS : in out Checkpoint_Save_State; Value : ALI_Annotation)
   is
   begin
      CSS.Write_U8 (ALI_Annotation_Kind'Pos (Value.Kind));
      CSS.Write_Unbounded
        (if Value.Message = null then "" else Value.Message.all);
   end Write;

   ----------
   -- Free --
   ----------

   procedure Free (CU : in out CU_Info) is
      procedure Free is new
        Ada.Unchecked_Deallocation
          (Statement_Bit_Map,
           Statement_Bit_Map_Access);

      procedure Free is new
        Ada.Unchecked_Deallocation (Decision_Bit_Map, Decision_Bit_Map_Access);

      procedure Free is new
        Ada.Unchecked_Deallocation (MCDC_Bit_Map, MCDC_Bit_Map_Access);
   begin
      for SID_Info of CU.SIDs_Info loop
         Free (SID_Info.Bit_Maps.Statement_Bits);
         Free (SID_Info.Bit_Maps.Decision_Bits);
         Free (SID_Info.Bit_Maps.MCDC_Bits);
      end loop;
   end Free;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load (CLS : in out Checkpoint_Load_State) is
      CP_Vectors : Source_Coverage_Vectors;
      Relocs     : Checkpoint_Relocations renames CLS.Relocations;
   begin
      --  Load data from stream
      --  This part must be kept consistent with Checkpoint_Save

      Read (CLS, CP_Vectors.CU_Vector);
      SC_Obligations.BDD.Read (CLS, CP_Vectors.BDD_Vector);
      Read (CLS, CP_Vectors.SCO_Vector);

      --  Initialize all the *Instrument* SCO_Descriptor components from "maps"
      --  in CLS. See the TODO in the declaration of Checkpoint_Version.

      declare
         SCOs : SCO_Sets.Set;
      begin
         --  First read uninstrumented SCOs for stmt/decision

         Read (CLS, SCOs);
         for SCO of SCOs loop
            declare
               SCOD : SCO_Descriptor renames CP_Vectors.SCO_Vector (SCO);
            begin
               case SCOD.Kind is
                  when Statement =>
                     SCOD.Stmt_Instrumented := False;

                  when Decision  =>
                     SCOD.Decision_Instrumented := False;

                  when others    =>
                     Fatal_Error
                       ("invalid non-instrumented SCO: " & SCOD.Kind'Image);
               end case;
            end;
         end loop;

         --  Then read uninstrumented SCOs for MC/DC

         Read (CLS, SCOs);
         for SCO of SCOs loop
            declare
               SCOD : SCO_Descriptor renames CP_Vectors.SCO_Vector (SCO);
            begin
               case SCOD.Kind is
                  when Decision =>
                     SCOD.Decision_Instrumented_For_MCDC := False;

                  when others   =>
                     Fatal_Error
                       ("invalid non-instrumented SCO for MC/DC: "
                        & SCOD.Kind'Image);
               end case;
            end;
         end loop;
      end;

      --  Allocate mapping tables for SCOs and BDD nodes

      Allocate_CU_Id_Maps
        (Relocs,
         CP_Vectors.CU_Vector.First_Index,
         CP_Vectors.CU_Vector.Last_Index);
      Allocate_SCO_Id_Map
        (Relocs,
         CP_Vectors.SCO_Vector.First_Index,
         CP_Vectors.SCO_Vector.Last_Index);
      Allocate_BDD_Node_Id_Map
        (Relocs,
         CP_Vectors.BDD_Vector.First_Index,
         CP_Vectors.BDD_Vector.Last_Index);

      --  Remap and merge into current tables

      for Cur in CP_Vectors.CU_Vector.Iterate loop
         declare
            use CU_Info_Vectors;

            CP_CU_Id : constant CU_Id := To_Index (Cur);
            CP_CU    : CU_Info := Element (Cur);

            --  If the CU Origin or its Main_Source files are ignored, we
            --  cannot load this CU.

            Origin_Ignored      : constant Boolean :=
              SFI_Ignored (Relocs, CP_CU.Origin);
            Main_Source_Ignored : constant Boolean :=
              SFI_Ignored (Relocs, CP_CU.Main_Source);
         begin
            if Origin_Ignored or else Main_Source_Ignored then
               SCOs_Trace.Trace
                 ("Ignoring CU from SID file: Id" & CP_CU_Id'Img);

               --  If we cannot load this CU *not* because its main source is
               --  ignored, but rather because the origin is ignored, warn the
               --  user: they probably did not want to ignore this CU, but we
               --  have to in order not to break our data structure invariants:
               --  Origin cannot be null.

               if not Main_Source_Ignored then
                  Warn
                    ("gnatcov limitation: ignoring unit "
                     & Get_Simple_Name (Remap_SFI (Relocs, CP_CU.Main_Source))
                     & " from "
                     & (+CLS.Filename)
                     & " because "
                     & (+Get_Simple_Name (Relocs, CP_CU.Origin))
                     & " is ignored");
               end if;

               Ignore_CU_Id (Relocs, CP_CU_Id);

            else
               Checkpoint_Load_Unit
                 (CLS, CP_Vectors, CP_CU, CP_CU_Id => CP_CU_Id);
            end if;
         end;
      end loop;

   end Checkpoint_Load;

   --------------------
   -- LLVM_JSON_Load --
   --------------------

   procedure LLVM_JSON_Load (Ckpt : access LLVM_Coverage_Ckpt) is
   begin
      LLVM_Trace.Trace ("SC_Obligations.LLVM_JSON_Load");

      for File_Report_Cur in Ckpt.File_Reports.Iterate loop
         declare
            use LLVM_Coverage_File_Ckpt_Vector;
            File_Report : LLVM_Coverage_File_Ckpt renames
              Ckpt.File_Reports.Reference (File_Report_Cur);

            Created_Units : Created_Unit_Maps.Map;

            SFI       : constant Source_File_Index :=
              Get_Index_From_Full_Name (+File_Report.Filename, Source_File);
            JSON_FI   : constant Source_File_Index :=
              Get_Index_From_Full_Name (+Ckpt.JSON_Filename, Library_File);
            CUID      : constant CU_Id :=
              Allocate_CU
                (Provider      => LLVM,
                 Origin        => JSON_FI,
                 Main_Source   => SFI,
                 Fingerprint   => No_Fingerprint,
                 Created_Units => Created_Units);
            First_SCO : constant Valid_SCO_Id := SCO_Vector.Last_Index + 1;

            Last_SCO : Valid_SCO_Id;
         begin

            --  If a compilation unit was already registered for this file,
            --  skip it. It is expected that only 1 JSON file is given to
            --  gnatcov. Aggregating source files should be done beforehand,
            --  at the LLVM level.

            if CUID = No_CU_Id then
               Warn
                 ("[LLVM-JSON] A compilation unit already exists for "
                  & (+File_Report.Filename)
                  & ".");
               return;
            end if;

            --  Register the SCOs

            for Fct_Cur in File_Report.Functions.Iterate loop
               declare
                  use LLVM_Coverage_Function_Ckpt_Vector;
                  Fct : LLVM_Coverage_Function_Ckpt renames
                    File_Report.Functions.Reference (Fct_Cur);
               begin
                  for Region_Cur in Fct.Regions.Iterate loop
                     declare
                        use LLVM_Region_Vector;
                        Region : LLVM_Region renames
                          Fct.Regions.Reference (Region_Cur);

                        Location : constant Source_Location_Range :=
                          (Source_File => SFI, L => Region.Span);

                        SCOD                : SCO_Descriptor;
                        MCDC_Num_Conditions : Any_Condition_Index;
                        SCO                 : SCO_Id;
                     begin
                        case Region.Kind is
                           when Decision  =>
                              MCDC_Num_Conditions :=
                                Any_Condition_Index
                                  (Region.Num_Conditions - 1);
                              SCOD :=
                                (Kind            => Decision,
                                 Origin          => CUID,
                                 Sloc_Range      => Location,
                                 D_Kind          => If_Statement,
                                 Last_Cond_Index => MCDC_Num_Conditions,
                                 others          => <>);

                           when Condition =>
                              pragma
                                Assert
                                  (Fct.Regions (Region.Parent_Id).SCO
                                     /= No_SCO_Id);
                              SCOD :=
                                (Kind       => Condition,
                                 Origin     => CUID,
                                 Sloc_Range => Location,
                                 Value      => Unknown,
                                 BDD_Node   => No_BDD_Node_Id,
                                 Parent     =>
                                   Fct.Regions (Region.Parent_Id).SCO,
                                 Index      => Region.Index,
                                 others     => <>);

                           when Statement =>
                              SCOD :=
                                (Kind       => Statement,
                                 Origin     => CUID,
                                 Sloc_Range => Location,
                                 S_Kind     => Other_Statement,
                                 others     => <>);
                        end case;
                        SCO_Vector.Append (SCOD);
                        SCO_To_CU_Vector.Append (CUID);
                        SCO := SCO_Vector.Last_Index;

                        --  Keep a reference to the just added SCO id.
                        --  Useful for condition -> decision referencing.

                        Region.SCO := SCO;

                        if SCOD.Kind /= Condition then
                           Add_SCO_To_Lines (SCO, SCOD);
                        end if;
                     end;
                  end loop;
               end;
            end loop;

            if SCO_Vector.Last_Index = No_SCO_Id then
               return;  --  The SCO vector is completely empty.

            end if;

            --  Check that Last_SCO >= First_SCO.
            --  otherwise there were no regions in the report.

            Last_SCO := SCO_Vector.Last_Index;

            if Last_SCO < First_SCO then
               Warn ("[LLVM-JSON] No regions to process");
            else
               CU_Vector (CUID).SCOs.Append
                 (SCO_Range'(First => First_SCO, Last => Last_SCO));
            end if;
         end;
      end loop;
   end LLVM_JSON_Load;

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
      BDD_Vector.Clear;
      SCO_Vector.Clear;
      SCO_To_CU_Vector.Clear;
   end Checkpoint_Clear;

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save (CSS : access Checkpoint_Save_State) is
   begin
      Write (CSS.all, CU_Vector);
      BDD.Write (CSS.all, BDD_Vector);
      Write (CSS.all, SCO_Vector);

      --  Write sets of uninstrumented SCOs. See the TODO in the declaration of
      --  Checkpoint_Version.
      --
      --  First build sets of non instrumented SCOs from SCO_Vector, then write
      --  these sets to the checkpoint.

      declare
         Non_Instr_SCOs, Non_Instr_MCDC_SCOs : SCO_Sets.Set;
      begin
         for SCO in 1 .. SCO_Vector.Last_Index loop
            declare
               SCOD              : SCO_Descriptor renames SCO_Vector (SCO);
               Instr, Instr_MCDC : Boolean := True;
            begin
               case SCOD.Kind is
                  when Statement =>
                     Instr := SCOD.Stmt_Instrumented;

                  when Decision  =>
                     Instr := SCOD.Decision_Instrumented;
                     Instr_MCDC := SCOD.Decision_Instrumented_For_MCDC;

                  when others    =>
                     null;
               end case;

               --  Beware that the booleans in SCO_Descriptor are true for
               --  *instrumented* SCOs while set elements indicate
               --  *non-instrumented* SCOs.

               if not Instr then
                  Non_Instr_SCOs.Insert (SCO);
               end if;
               if not Instr_MCDC then
                  Non_Instr_MCDC_SCOs.Insert (SCO);
               end if;
            end;
         end loop;

         Write (CSS.all, Non_Instr_SCOs);
         Write (CSS.all, Non_Instr_MCDC_SCOs);
      end;
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

   ----------------
   -- SCO_Ranges --
   ----------------

   function SCO_Ranges (CU : CU_Id) return SCO_Range_Vectors.Vector is
   begin
      if CU = No_CU_Id then
         return SCO_Range_Vectors.Empty_Vector;
      else
         return CU_Vector.Constant_Reference (CU).SCOs;
      end if;
   end SCO_Ranges;

   -----------
   -- In_CU --
   -----------

   function In_CU (CU : CU_Id; SCO : SCO_Id) return Boolean is
   begin
      for SCO_Range of SCO_Ranges (CU) loop
         if SCO >= SCO_Range.First and then SCO <= SCO_Range.Last then
            return True;
         end if;
      end loop;
      return False;
   end In_CU;

   -----------
   -- Index --
   -----------

   function Index
     (Vectors : Source_Coverage_Vectors; SCO : SCO_Id) return Condition_Index
   is
      SCOD : SCO_Descriptor renames
        Vectors.SCO_Vector.Constant_Reference (SCO);
   begin
      pragma Assert (SCOD.Kind = Condition);
      return SCOD.Index;
   end Index;

   ---------------
   -- Condition --
   ---------------

   function Condition
     (Vectors : Source_Coverage_Vectors; SCO : SCO_Id; Index : Condition_Index)
      return SCO_Id
   is
      use BDD;

      SCOD : SCO_Descriptor renames
        Vectors.SCO_Vector.Constant_Reference (SCO);

      First : constant BDD_Node_Id := SCOD.Decision_BDD.First_Node;
      Last  : constant BDD_Node_Id := SCOD.Decision_BDD.Last_Node;
      CU    : constant CU_Id := Comp_Unit (SCO);

      Current_Condition_Index : Any_Condition_Index := No_Condition_Index;

   begin
      --  Do not check the BDD if the SCO was provided by LLVM.

      if Provider (CU) = LLVM then

         --  TODO??? Make this search more efficient.
         --  (For now it just browse all SCOs of the CU).
         --  Ideas :
         --    - Ensure both invariants always hold. That way, we can get rid
         --      of the double while loop.
         --    - Find a way to reference the SCO_ID of the first condition in
         --      the SCOD of the decision, so we just have to add the SCO_ID
         --      with the index.

         declare
            C_SCO : SCO_Id := SCO + 1;
            --  Condition SCOs are always after their Decision, and all the
            --  condition SCOs of a decision should appear consecutively.
            --
            --  The second invariant may not hold in the case of
            --  nested decisions.

            Index_Mut : Condition_Index := Index;
         begin
            --  Search for the first Condition of the Decision.

            while In_CU (CU, C_SCO)
              and then
                (Kind (C_SCO) /= Condition or else Parent (C_SCO) /= SCO)
            loop
               C_SCO := C_SCO + 1;
            end loop;

            --  Search for the Index-th Condition.
            --  We need a loop because the invariant may not hold with
            --  nested decisions.

            while Index_Mut > 0 loop
               Index_Mut := Index_Mut - 1;
               loop
                  C_SCO := C_SCO + 1;
                  exit when not In_CU (CU, C_SCO);
                  exit when
                    Kind (C_SCO) = Condition and then Parent (C_SCO) = SCO;
               end loop;
            end loop;

            if not In_CU (CU, C_SCO) then
               Fatal_Error
                 ("Malformed SCO Vector, a condition SCO" & " is missing");
            end if;

            pragma Assert (Kind (C_SCO) = Condition);
            pragma Assert (Parent (C_SCO) = SCO);
            return C_SCO;
         end;
      end if;

      --  Find J'th (0-based) condition in decision by scanning the BDD vector

      for J in First .. Last loop
         declare
            BDDN : BDD_Node renames Vectors.BDD_Vector.Constant_Reference (J);
         begin
            if BDDN.Kind = Condition then
               Current_Condition_Index := Current_Condition_Index + 1;
               if Current_Condition_Index = Index then
                  return C_SCO : constant SCO_Id := BDDN.C_SCO do
                     pragma
                       Assert (Enclosing (Vectors, Decision, C_SCO) = SCO);
                     pragma
                       Assert (SC_Obligations.Index (Vectors, C_SCO) = Index);
                  end return;
               end if;
            end if;
         end;
      end loop;
      raise Constraint_Error with "condition index out of range";
   end Condition;

   function Condition (SCO : SCO_Id; Index : Condition_Index) return SCO_Id
   is (Condition (SC_Vectors, SCO, Index));

   ----------------------
   -- Condition_Values --
   ----------------------

   function Condition_Values
     (SCO : SCO_Id; Path_Index : Natural; Outcome : out Boolean)
      return Condition_Values_Array
   is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);

      Last_Cond_Index : constant Condition_Index := SCOD.Last_Cond_Index;
      --  Index of last condition in decision

      Node : BDD_Node_Id := SCOD.Decision_BDD.Root_Condition;
      --  Current BDD node

      Tail_Index : Natural := Path_Index;
      --  Path index in the sub-BDD rooted at Node
   begin
      return
         Result : Condition_Values_Array :=
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

      return
        (if Reachable_Outcomes (False) /= Reachable_Outcomes (True)
         then To_Tristate (Reachable_Outcomes (True))
         else Unknown);
   end Decision_Outcome;

   -------------------
   -- Decision_Type --
   -------------------

   function Decision_Type (SCO : SCO_Id) return Decision_Kind is
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
     (SCO : SCO_Id; Dom_SCO : out SCO_Id; Dom_Val : out Boolean)
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
      if Op_SCO = No_SCO_Id then
         return (+"<Expression Unavailable>");
      end if;
      case Kind (Op_SCO) is
         when Condition =>
            return +('C' & Img (Integer (Index (Op_SCO))));

         when Decision  =>
            return Expression_Image (SCO_Vector.Reference (Op_SCO).Expression);

         when Operator  =>
            declare
               Result : Unbounded_String := +"(";
               Binary : constant Boolean := Op_Kind (Op_SCO) /= Op_Not;
            begin
               for J in Operand_Position'Range loop
                  declare
                     Opnd_SCO : constant SCO_Id := Operand (Op_SCO, J);
                  begin
                     if J = Right then
                        case Op_Kind (Op_SCO) is
                           when Op_Not      =>
                              Append (Result, "not ");

                           when Op_And_Then =>
                              Append (Result, " and then ");

                           when Op_Or_Else  =>
                              Append (Result, " or else ");
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

               Append (Result, ')');
               return Result;
            end;

         when others    =>
            return
              +("Expected expression SCO kind (Decision, Condition or"
                & " Operator), but got "
                & SCO_Kind'Image (Kind (Op_SCO)));
      end case;
   end Expression_Image;

   -------------------
   -- Dump_Decision --
   -------------------

   procedure Dump_Decision (SCO : SCO_Id) is
   begin
      Put_Line ("Reconstructed expression for " & Image (SCO));
      Put_Line (+Expression_Image (SCO));
   end Dump_Decision;

   ---------------
   -- Enclosing --
   ---------------

   function Enclosing
     (Vectors : Source_Coverage_Vectors; What : SCO_Kind; SCO : SCO_Id)
      return SCO_Id is
   begin
      return Result : SCO_Id := SCO do
         while Result /= No_SCO_Id loop
            declare
               SCOD : SCO_Descriptor renames
                 Vectors.SCO_Vector.Constant_Reference (Result);
            begin
               if SCOD.Kind = What then
                  return;
               end if;
               Result := SCOD.Parent;
            end;
         end loop;
      end return;
   end Enclosing;

   function Enclosing (What : SCO_Kind; SCO : SCO_Id) return SCO_Id
   is (Enclosing (SC_Vectors, What, SCO));

   ------------
   -- Nested --
   ------------

   function Nested (Left, Right : SCO_Descriptor) return Boolean is
      L : Local_Source_Location_Range renames Left.Sloc_Range.L;
      R : Local_Source_Location_Range renames Right.Sloc_Range.L;
   begin
      --  For call coverage, we allow Left and Right to share their First_Sloc
      --  as for a call statement the statement SCO will be on the whole
      --  statement, and the call SCO will start at the beginning of the
      --  statement and end before the semi-colon marking its end.
      --  For any other SCOs, neither the first nor the last location have a
      --  reason to be shared.
      return
        (if Left.Kind = Call and then Right.Kind = Call
         then L.First_Sloc <= R.First_Sloc and then R.Last_Sloc < L.Last_Sloc
         else L.First_Sloc < R.First_Sloc and then R.Last_Sloc < L.Last_Sloc);
   end Nested;

   ---------------------
   -- Invalid_Overlap --
   ---------------------

   function Invalid_Overlap
     (SCOD : SCO_Descriptor; Enclosing_SCO : SCO_Id) return Boolean is
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

   function First_Sloc (SCO : SCO_Id) return Source_Location is
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

   function Get_Origin (SCO : SCO_Id) return Maybe_SCO_Value is
      use BDD;

      SCOD : SCO_Descriptor renames SCO_Vector (SCO);
      BDDN : BDD_Node renames BDD_Vector (SCOD.BDD_Node);
   begin
      if BDDN.Parent = No_BDD_Node_Id then
         return Maybe_SCO_Value'(Present => False);
      else
         return
           Maybe_SCO_Value'
             (Present => True,
              SCO     => BDD_Vector.Constant_Reference (BDDN.Parent).C_SCO,
              Value   => BDDN.Parent_Value);
      end if;
   end Get_Origin;

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
      return
        SCO_Vector.Reference (SCO).Decision_BDD.First_Multipath_Condition
        /= No_BDD_Node_Id;
   end Has_Multipath_Condition;

   -------------
   -- Has_SCO --
   -------------

   function Has_SCO
     (Sloc_Begin : Source_Location; Sloc_End : Source_Location) return Boolean
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

            SCOD
            .S_Kind
          = Disabled_Pragma_Statement;

   end Ignore_SCO;

   ----------------------------
   -- Set_Stmt_SCO_Non_Instr --
   ----------------------------

   procedure Set_Stmt_SCO_Non_Instr (SCO : SCO_Id) is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
   begin
      SCOD.Stmt_Instrumented := False;
   end Set_Stmt_SCO_Non_Instr;

   --------------------------------
   -- Set_Decision_SCO_Non_Instr --
   --------------------------------

   procedure Set_Decision_SCO_Non_Instr (SCO : SCO_Id) is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
   begin
      SCOD.Decision_Instrumented := False;
   end Set_Decision_SCO_Non_Instr;

   -----------------------------------------
   -- Set_Decision_SCO_Non_Instr_For_MCDC --
   -----------------------------------------

   procedure Set_Decision_SCO_Non_Instr_For_MCDC (SCO : SCO_Id) is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
   begin
      SCOD.Decision_Instrumented_For_MCDC := False;
   end Set_Decision_SCO_Non_Instr_For_MCDC;

   ----------------------------
   -- Set_Call_SCO_Non_Instr --
   ----------------------------

   procedure Set_Fun_Call_SCO_Non_Instr (SCO : SCO_Id) is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
   begin
      SCOD.Fun_Call_Instrumented := False;
   end Set_Fun_Call_SCO_Non_Instr;

   -----------------------------
   -- Set_GExpr_SCO_Non_Instr --
   -----------------------------

   procedure Set_GExpr_SCO_Non_Instr (SCO : SCO_Id) is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
   begin
      SCOD.GExpr_Instrumented := False;
   end Set_GExpr_SCO_Non_Instr;

   ---------------------------
   -- Stmt_SCO_Instrumented --
   ---------------------------

   function Stmt_SCO_Instrumented (SCO : SCO_Id) return Boolean
   is (SCO_Vector (SCO).Stmt_Instrumented);

   -------------------------------
   -- Decision_SCO_Instrumented --
   -------------------------------

   function Decision_SCO_Instrumented (SCO : SCO_Id) return Boolean
   is (SCO_Vector (SCO).Decision_Instrumented);

   ----------------------------------------
   -- Decision_SCO_Instrumented_For_MCDC --
   ----------------------------------------

   function Decision_SCO_Instrumented_For_MCDC (SCO : SCO_Id) return Boolean
   is (SCO_Vector (SCO).Decision_Instrumented_For_MCDC);

   -------------------------------
   -- Fun_Call_SCO_Instrumented --
   -------------------------------

   function Fun_Call_SCO_Instrumented (SCO : SCO_Id) return Boolean
   is (SCO_Vector (SCO).Fun_Call_Instrumented);

   ----------------------------
   -- GExpr_SCO_Instrumented --
   ----------------------------

   function GExpr_SCO_Instrumented (SCO : SCO_Id) return Boolean
   is (SCO_Vector (SCO).GExpr_Instrumented);

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
         if Sloc_Range.L.First_Sloc = No_Local_Location or else not With_Sloc
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
            SCOD           : SCO_Descriptor renames SCO_Vector.Reference (SCO);
            From_Assertion : constant Boolean :=
              Assertion_Coverage_Enabled
              and then
                ((SCOD.Kind = Decision
                  and then SCOD.D_Kind in Pragma_Decision | Aspect)
                 or else
                   (SCOD.Kind = Condition
                    and then
                      SCO_Vector.Reference (Enclosing_Decision (SCO)).D_Kind
                      in Pragma_Decision | Aspect));
         begin
            return
              "SCO #"
              & Trim (SCO'Img, Side => Ada.Strings.Both)
              & ": "
              & (if From_Assertion then "ASSERTION " else "")
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
   begin
      if SCO = No_SCO_Id then
         return No_CU_Id;
      end if;
      return SCO_To_CU_Vector.Element (SCO);
   end Comp_Unit;

   -----------
   -- Index --
   -----------

   function Index (SCO : SCO_Id) return Condition_Index is
   begin
      return Index (SC_Vectors, SCO);
   end Index;

   ----------------------------
   -- Decision_Has_Influence --
   ----------------------------

   function Decision_Has_Influence (SCO : SCO_Id) return Boolean is
      use BDD;
      SCOD  : SCO_Descriptor renames SCO_Vector.Constant_Reference (SCO);
      First : constant BDD_Node_Id := SCOD.Decision_BDD.First_Node;
      Last  : constant BDD_Node_Id := SCOD.Decision_BDD.Last_Node;

   begin
      if Traces_Files.Currently_Accepted_Trace_Kind
         in Traces_Files.Source_Trace_File .. Traces_Files.All_Trace_Files
      then
         return True;
      end if;

      --  Assume that LLVM SCO decisions always have influence.
      --  Also, for LLVM SCOs, the BDD is not populated, so we can't browse it.

      if Provider (Comp_Unit (SCO)) = LLVM then
         return True;
      end if;

      --  Iterate over all BDD nodes to more efficiently iterate over the
      --  condition SCOs, and record whether we have found a branch for it.

      for J in First .. Last loop
         declare
            BDDN : BDD_Node renames BDD_Vector.Constant_Reference (J);
         begin
            if BDDN.Kind = Condition
              and then
                not SCO_Vector.Constant_Reference (BDDN.C_SCO).PC_Set.Is_Empty
            then
               return True;
            end if;
         end;
      end loop;

      --  Here if the decision has no conditional branches associated, check
      --  whether it dominates some statement.

      for S_SCO_Cur in SCO_Vector.Iterate loop
         declare
            S_SCOD : SCO_Descriptor renames
              SCO_Vector.Constant_Reference (S_SCO_Cur);
         begin
            if S_SCOD.Kind = Statement and then S_SCOD.Dominant = SCO then
               return True;
            end if;
         end;
      end loop;

      --  If not, this decision has no impact over the control flow of the
      --  program.

      return False;
   end Decision_Has_Influence;

   -----------------
   -- Register_CU --
   -----------------

   procedure Register_CU (CU : CU_Id) is
      Origin   : constant Source_File_Index := CU_Vector.Reference (CU).Origin;
      Cur      : Origin_To_CUs_Maps.Cursor := Origin_To_CUs_Map.Find (Origin);
      Inserted : Boolean;
   begin
      if not Origin_To_CUs_Maps.Has_Element (Cur) then
         Origin_To_CUs_Map.Insert (Origin, CU_Sets.Empty_Set, Cur, Inserted);
         pragma Assert (Inserted);
      end if;

      Origin_To_CUs_Map.Reference (Cur).Insert (CU);
   end Register_CU;

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

            return
              SCO_Vector (Enclosing_Statement (SCO)).Pragma_Name
              /= Pragma_Debug;

         when Aspect          =>
            --  Always True for aspects (Pre/Post/Predicate/Invariant)

            return True;

         when others          =>
            return False;
      end case;

   end Is_Assertion;

   ---------------------------
   -- Is_Assertion_To_Cover --
   ---------------------------

   function Is_Assertion_To_Cover (SCO : SCO_Id) return Boolean is
      function Is_Pragma_Stmt_To_Cover (SCOD : SCO_Descriptor) return Boolean;
      --  True if the pragma statement of SCOD belongs to the list of pragmas
      --  supported by assertion coverage.

      function Is_Pragma_Stmt_To_Cover (SCOD : SCO_Descriptor) return Boolean
      is
      begin
         pragma
           Assert
             (SCOD.Kind = Statement and then SCOD.S_Kind = Pragma_Statement);

         return
           SCOD.Pragma_Name
           in Pragma_Assert
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
                 Is_Pragma_Stmt_To_Cover
                   (SCO_Vector (Enclosing_Statement (SCO)));

            when Aspect          =>
               return
                 SCOD.Aspect_Name
                 in Aspect_Type_Invariant | Aspect_Pre | Aspect_Post;

            when others          =>
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

         return
           (S_SCOD.S_Kind = Disabled_Pragma_Statement
            or else S_SCOD.S_Kind = Pragma_Statement)
           and then
             S_SCOD.Pragma_Name
             in Pragma_Assert
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
      SCOD            : SCO_Descriptor renames SCO_Vector.Reference (SCO);
      pragma Assert (SCOD.Kind = Decision);
      Enclosing_S_SCO : constant SCO_Id := Enclosing_Statement (SCO);
   begin
      return
        SCOD.D_Kind = If_Statement
        and then
          not (Enclosing_S_SCO /= No_SCO_Id
               and then S_Kind (Enclosing_S_SCO) = If_Statement
               and then First_Sloc (Enclosing_S_SCO) = SCOD.Control_Location);
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

   ------------------
   -- Is_Call_Stmt --
   ------------------

   function Is_Call_Stmt (SCO : SCO_Id) return Boolean is
      SCOD : constant SCO_Descriptor := SCO_Vector.Reference (SCO);
   begin
      return Kind (SCO) = Call and then not SCOD.Is_Expr;
   end Is_Call_Stmt;

   ----------------------------------
   -- Is_Pragma_Pre_Post_Condition --
   ----------------------------------

   function Is_Pragma_Pre_Post_Condition (SCO : SCO_Id) return Boolean is
      SCOD : SCO_Descriptor renames SCO_Vector.Reference (SCO);
      pragma Assert (SCOD.Kind = Statement);
   begin
      return
        SCOD.S_Kind in Pragma_Statement | Disabled_Pragma_Statement
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

   function Last_SCO return SCO_Id
   is (SCO_Vector.Last_Index);

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

   function Last_Sloc (SCO : SCO_Id) return Source_Location is
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
         else
           "CU "
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
      SID_Infos : constant SID_Info_Maps.Map :=
        CU_Vector.Element (CU).SIDs_Info;
   begin
      if SID_Infos.Length /= 1 then
         Outputs.Fatal_Error
           ("Found multiple versions for "
            & Get_Full_Name (CU_Vector.Element (CU).Main_Source));
      end if;
      pragma Assert (SID_Infos.Length = 1);
      return SID_Infos.First_Key;
   end Fingerprint;

   ---------------
   -- Load_SCOs --
   ---------------

   procedure Load_SCOs
     (ALI_Filename : String; Excluded_Source_Files : access GNAT.Regexp.Regexp)
   is
      Units, Deps : SFI_Vector;
      --  Units and dependencies of this compilation

      Created_Units : Created_Unit_Maps.Map;
      Main_Source   : Source_File_Index;

      Temp_ALI_Annotations : ALI_Annotation_Maps.Map;

      ALI_Index : constant Source_File_Index :=
        Load_ALI
          (ALI_Filename,
           Excluded_Source_Files,
           Units,
           Deps,
           Temp_ALI_Annotations,
           With_SCOs => True);
      --  Load ALI file and update the last SCO

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
            & ASCII.LF
            & "  "
            & Get_Full_Name (Main_Source, True)
            & ASCII.LF
            & "appears as the main source file in:"
            & ASCII.LF
            & "  "
            & ALI_Filename
            & ASCII.LF
            & "  "
            & Get_Full_Name (Get_File (Main_Source).LI, True)
            & ASCII.LF
            & "Is the same ALI file provided twice?");
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

      Set_Annotations (Temp_ALI_Annotations);

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
      procedure Free is new
        Ada.Unchecked_Deallocation (CU_Load_Info, CU_Load_Info_Access);
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
     (Unit : SCOs.SCO_Unit_Table_Entry; Deps : SFI_Vector)
      return Source_File_Index
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

           --  For C, old compilers did not provide a proper deps table: in
           --  that case, fallback on the inlined file name.

           then
           Get_Index_From_Generic_Name
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
     (Unit_Info : in out CU_Load_Info; S : String) is
   begin
      GNAT.SHA1.Update (Unit_Info.Fingerprint_Context, S);
      if SCOs_Trace.Is_Active then
         Append (Unit_Info.Fingerprint_Buffer, S);
      end if;
   end Append_For_Fingerprint;

   procedure Append_For_Fingerprint
     (Unit_Info : in out CU_Load_Info; Sloc : SCOs.Source_Location) is
   begin
      Append_For_Fingerprint
        (Unit_Info,
         ":"
         & Logical_Line_Number'Image (Sloc.Line)
         & ":"
         & Column_Number'Image (Sloc.Col));
   end Append_For_Fingerprint;

   ------------------------
   -- Build_CU_Load_Info --
   ------------------------

   procedure Build_CU_Load_Info
     (Infos : out CU_Load_Info_Vectors.Vector;
      Deps  : SFI_Vector := SFI_Vectors.Empty_Vector)
   is
      package Unit_Maps is new
        Ada.Containers.Ordered_Maps
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
                  Unit_Info :=
                    new CU_Load_Info'
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
                     Append_For_Fingerprint
                       (Unit_Info.all, String'((E.C1, E.C2)));
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

      if SCOs_Trace.Is_Active then
         for Unit_Info of Infos loop
            SCOs_Trace.Trace
              ("Computing fingerprint for "
               & Unit_Info.File_Name_Ptr.all
               & " SCOs from:");
            SCOs_Trace.Trace ("BEGIN ...");
            SCOs_Trace.Trace (+Unit_Info.Fingerprint_Buffer);
            SCOs_Trace.Trace ("... END");
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
      use GNATCOLL.VFS;

      New_CU_Info : CU_Info (Provider);

      CU_Index   : constant CU_Id := Comp_Unit (Main_Source);
      CU_Version : SID_Info;
      Cur        : constant Cursor := Created_Units.Find (Main_Source);

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

         if SC_Obligations.Fingerprint (CU_Index) = Fingerprint then
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
                 when Compiler | LLVM => "loading",
                 when Instrumenter    => "instrumenting");
         begin
            Warn
              ("ignoring duplicate SCOs for "
               & Get_Simple_Name (Main_Source)
               & " (from "
               & New_Origin_Full
               & ")");
            Warn
              ("previous SCOs for this unit came from "
               & Origin_Action
               & " "
               & Old_Origin_Full);
            return No_CU_Id;
         end;
      end if;

      --  Initialize the new CU and register it where needed

      New_CU_Info.Origin := Origin;
      New_CU_Info.Main_Source := Main_Source;
      New_CU_Info.SIDs_Info.Insert (Fingerprint, CU_Version);

      --  Also create the fingerprint: the SCOs fingerprint for binary traces,
      --  which is still used for consistency checks, and the source
      --  fingerprint for source traces.

      case Provider is
         when Compiler | LLVM =>
            New_CU_Info.SCOs_Fingerprint := Fingerprint;

         when Instrumenter    =>
            declare
               Source_Fingerprint_Context : GNAT.SHA1.Context;
               Contents                   : GNAT.Strings.String_Access :=
                 Read_File (Create (+Get_Full_Name (Main_Source)));
            begin
               GNAT.SHA1.Update (Source_Fingerprint_Context, Contents.all);
               Free (Contents);
               New_CU_Info.Source_Fingerprint :=
                 Fingerprint_Type
                   (GNAT.SHA1.Binary_Message_Digest'
                      (GNAT.SHA1.Digest (Source_Fingerprint_Context)));
            end;
      end case;

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
      Provider      : SCO_Provider;
      Attached_Ctx  : Instr_Attached_Ctx := No_Attached_Ctx)
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
        (SCO_Source_Loc : SCOs.Source_Location) return Local_Source_Location;
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
         SCO_To_CU_Vector.Append (CU);
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
        (SCO_Source_Loc : SCOs.Source_Location) return Local_Source_Location
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
         use SCO_Sets;
      begin
         case SCOE.C2 is
            when 'f'    =>
               return False;

            when 't'    =>
               return True;

            when 'c'    =>
               return
                 (if Attached_Ctx.True_Static_SCOs.Contains
                       (SCO_Id (SCO_Index))
                  then True
                  elsif Attached_Ctx.False_Static_SCOs.Contains
                          (SCO_Id (SCO_Index))
                  then False
                  else Unknown);

            when others =>
               raise Program_Error
                 with "invalid SCO condition value code: " & SCOE.C2;
         end case;
      end Make_Condition_Value;

      ----------------------
      -- New_Operator_SCO --
      ----------------------

      function New_Operator_SCO (Kind : Operator_Kind) return SCO_Id is
      begin
         pragma Assert (State.Current_Decision /= No_SCO_Id);
         return
           Add_SCO
             ((Kind       => Operator,
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
         SCOD.Decision_BDD := State.Current_BDD;
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
         when '>'                                     =>
            --  Dominance marker: processed in conjunction with following 'S'
            --  entry.

            pragma Assert (State.Dom_SCO = No_SCO_Id);
            if SCOE.Last then
               --  Ignore dominance marker because all S entries in its
               --  sequence have been suppressed.

               null;

            else
               case SCOE.C2 is
                  when 'S'       =>
                     State.Dom_Sloc :=
                       Slocs.To_Sloc (Unit.Main_Source, From_Sloc);
                     State.Dom_Val := Unknown;

                  when 'T' | 'F' =>
                     State.Dom_Sloc :=
                       Slocs.To_Sloc (Unit.Main_Source, From_Sloc);
                     State.Dom_Val := To_Tristate (SCOE.C2 = 'T');

                  when 'E'       =>
                     State.Current_Handler_Range := SCO_Range;

                  when others    =>
                     raise Program_Error;
               end case;
            end if;

         when 'S' | 's' | 'C' | 'c' | 'g'             =>
            pragma Assert (State.Current_Decision = No_SCO_Id);

            if SCOE.C1 = 'c' then

               if SCOE.C2 = 'F' then
                  New_SCO :=
                    Add_SCO
                      (SCO_Descriptor'
                         (Kind       => Fun,
                          Origin     => CU,
                          Sloc_Range => SCO_Range,
                          Is_Expr    => False,
                          others     => <>));
               else
                  New_SCO :=
                    Add_SCO
                      (SCO_Descriptor'
                         (Kind       => Call,
                          Origin     => CU,
                          Sloc_Range => SCO_Range,
                          Is_Expr    => SCOE.C2 = 'E',
                          others     => <>));
               end if;
            elsif SCOE.C1 = 'g' then
               --  Guarded expression
               New_SCO :=
                 Add_SCO
                   (SCO_Descriptor'
                      (Kind       => Guarded_Expr,
                       Origin     => CU,
                       Sloc_Range => SCO_Range,
                       others     => <>));
            else
               New_SCO :=
                 Add_SCO
                   (SCO_Descriptor'
                      (Kind           => Statement,
                       Origin         => CU,
                       Sloc_Range     => SCO_Range,
                       S_Kind         => To_Statement_Kind (SCOE.C2),
                       Dominant       => State.Dom_SCO,
                       Dominant_Sloc  => State.Dom_Sloc,
                       Dominant_Value => State.Dom_Val,
                       Handler_Range  => State.Current_Handler_Range,
                       Pragma_Name    =>
                         Case_Insensitive_Get_Pragma_Id
                           (SCOE.Pragma_Aspect_Name),
                       others         => <>));
            end if;

            State.Current_Handler_Range := No_Range;
            State.Dom_Val := Unknown;
            State.Dom_Sloc := No_Location;
            if SCOE.Last then
               State.Dom_SCO := No_SCO_Id;
            else
               State.Dom_SCO := SCO_Vector.Last_Index;
            end if;

         when 'E' | 'G' | 'I' | 'P' | 'W' | 'X' | 'A' =>
            --  Decision

            pragma Assert (State.Current_Decision = No_SCO_Id);
            State.Current_Decision :=
              Add_SCO
                (SCO_Descriptor'
                   (Kind             => Decision,
                    Origin           => CU,
                    Control_Location =>

                      --  Control locations are only useful for dominance
                      --  markers, which are only used with binary traces. As
                      --  it is impractical to get the correct location with
                      --  the C/C++ instrumenter, and as using incorrect slocs
                      --  can create conflicts, ignore those in the
                      --  instrumentation case.

                      (if Provider = Compiler
                       then Slocs.To_Sloc (Unit.Main_Source, From_Sloc)
                       else No_Location),

                    D_Kind           => To_Decision_Kind (SCOE.C1),
                    Last_Cond_Index  => 0,
                    Aspect_Name      =>
                      Get_Aspect_Id (SCOE.Pragma_Aspect_Name),
                    others           => <>));
            pragma Assert (not SCOE.Last);

            State.Current_BDD :=
              BDD.Create (BDD_Vector, State.Current_Decision);
            State.Current_Condition_Index := No_Condition_Index;

         when ' '                                     =>
            --  Condition

            pragma Assert (State.Current_Decision /= No_SCO_Id);

            SCO_Vector.Update_Element
              (Index   => State.Current_Decision,
               Process => Update_Decision_Sloc'Access);

            State.Current_Condition_Index := State.Current_Condition_Index + 1;

            New_SCO :=
              Add_SCO
                (SCO_Descriptor'
                   (Kind       => Condition,
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

               if SCOs_Trace.Is_Active then
                  Dump_Decision (State.Current_Decision);
               end if;
               State.Current_Decision := No_SCO_Id;
            end if;

         when '!'                                     =>
            BDD.Process_Not (New_Operator_SCO (Op_Not), State.Current_BDD);

         when '&'                                     =>
            BDD.Process_And_Then
              (BDD_Vector, New_Operator_SCO (Op_And_Then), State.Current_BDD);

         when '|'                                     =>
            BDD.Process_Or_Else
              (BDD_Vector, New_Operator_SCO (Op_Or_Else), State.Current_BDD);

         when 'H'                                     =>
            --  Chaining indicator: not used yet

            null;

         when others                                  =>
            raise Program_Error with "unexpected SCO entry code: " & SCOE.C1;
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
      Count_Paths   : Boolean;
      Attached_Ctx  : Instr_Attached_Ctx := No_Attached_Ctx)
   is
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

            First_SCO : constant SCO_Id := SCO_Vector.Last_Index + 1;

            Fingerprint : constant Fingerprint_Type :=
              Fingerprint_Type
                (GNAT.SHA1.Binary_Message_Digest'
                   (GNAT.SHA1.Digest (Info.Fingerprint_Context)));

            CU    : constant CU_Id :=
              Allocate_CU
                (Provider,
                 Origin,
                 Info.Source_File,
                 Fingerprint,
                 Created_Units);
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
               State.Current_Decision := No_SCO_Id;
               State.Dom_SCO := No_SCO_Id;
               State.Dom_Sloc := No_Location;
               State.Dom_Val := Unknown;
               State.Current_Handler_Range := No_Range;

               for SCO_Index in SCO_Range.First .. SCO_Range.Last loop
                  Process_Low_Level_Entry
                    (CU,
                     SCO_Index,
                     State,
                     Ignored_Slocs_Set,
                     SCO_Map,
                     Count_Paths,
                     Provider,
                     Attached_Ctx);
               end loop;
            end loop;

            --  Prealloc line table entries for this unit

            Prealloc_Lines (Info.Source_File, State.Last_Line);

            --  Finally update CU info

            declare
               Unit : CU_Info renames CU_Vector.Reference (CU);
            begin
               Unit.Deps := Deps;

               Unit.SCOs.Append
                 (SCO_Range'
                    (First => First_SCO, Last => SCO_Vector.Last_Index));
            end;
         end;

         <<Skip_Unit>>
         null;
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
               if L.Kind /= R.Kind or else L.Sloc_Range /= R.Sloc_Range then
                  return False;
               end if;

               return
                 (case L.Kind is
                    when Removed      =>
                      raise Program_Error with "unreachable code",
                    when Statement    => L.S_Kind = R.S_Kind,
                    when Condition    =>
                      L.Value = R.Value and then L.Index = R.Index,
                    when Decision     =>
                      (L.D_Kind = R.D_Kind
                       and then L.Control_Location = R.Control_Location),
                    when Operator     => L.Op_Kind = R.Op_Kind,
                    when Fun          => True,
                    when Call         => L.Is_Expr = R.Is_Expr,
                    when Guarded_Expr => True);
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
               SCOs_Trace.Trace
                 ("Processing: "
                  & Image (SCO)
                  & (if SCOD.Kind = Decision
                     then
                       (if SCOD.Last_Cond_Index > 0
                        then " (complex)"
                        else " (simple)")
                     else ""));

               case SCOD.Kind is
                  when Removed              =>
                     raise Program_Error with "unreachable code";

                  when Decision             =>
                     --  A Decision SCO must have a statement or (in the case
                     --  of a nested decision) a Condition SCO as its parent,
                     --  or no parent at all.

                     pragma
                       Assert
                         (Enclosing_SCO = No_SCO_Id
                            or else Kind (Enclosing_SCO) /= Decision);
                     SCOD.Parent := Enclosing_SCO;

                     if SCOD.Control_Location /= No_Location then
                        Sloc_Range :=
                          To_Range (SCOD.Control_Location, No_Location);
                     end if;

                     Add_SCO_To_Lines (SCO, SCOD);

                  when Statement            =>

                     if (Enclosing_SCO /= No_SCO_Id
                         and then
                           Equivalent (SCOD, SCO_Vector (Enclosing_SCO)))
                       --  The only form of SCO overlapping we allow is SCO
                       --  nesting. A statement can contain nested statements,
                       --  e.g. with C++ lambda expressions.  We reject every
                       --  other kind of overlapping.
                       --
                       --  TODO??? With C headers, we can have multiple times
                       --  the same SCO if the header is included multiple
                       --  times. This will result in a buggy behavior if the
                       --  included code expansion varies (as we may accept
                       --  nested SCO that come from the second inclusion, but
                       --  that are nested in a SCO from the first inclusion,
                       --  which makes no sense). Consider this as a marginal
                       --  use case for now.

                       or else Invalid_Overlap (SCOD, Enclosing_SCO)
                       or else
                         Invalid_Overlap
                           (SCOD, Sloc_To_SCO (Last_Sloc (Sloc_Range)))
                     then
                        return;
                     end if;
                     Add_SCO_To_Lines (SCO, SCOD);

                  when Condition | Operator =>
                     --  Parent is already set to the enclosing decision or
                     --  operator.

                     null;

                  when Fun_Call_SCO_Kind    =>
                     SCOD.Parent := Enclosing_SCO;
                     Add_SCO_To_Lines (SCO, SCOD);

                  when Guarded_Expr         =>
                     SCOD.Parent := Enclosing_SCO;
                     Add_SCO_To_Lines (SCO, SCOD);

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

            if SCOD.Kind = Statement and then SCOD.Dominant_Sloc /= No_Location
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

                  pragma
                    Assert
                      (Dom_Sloc_SCO = No_SCO_Id
                         or else Kind (Dom_Sloc_SCO) = Statement);

               else
                  --  Case of >T / >F: dominant SCO is a decision

                  if Sloc_To_SCO_Map (SCOD.Dominant_Sloc.Source_File, Decision)
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
                        "dominant decision of statement "
                        & Image (SCO)
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

   -------------------
   -- Next_BDD_Node --
   -------------------

   function Next_BDD_Node
     (Vectors : Source_Coverage_Vectors; SCO : SCO_Id; Value : Boolean)
      return BDD_Node_Id
   is
      use BDD;
      BDD_Node : constant BDD_Node_Id :=
        Vectors.SCO_Vector.Constant_Reference (SCO).BDD_Node;
   begin
      return Vectors.BDD_Vector.Constant_Reference (BDD_Node).Dests (Value);
   end Next_BDD_Node;

   function Next_BDD_Node (SCO : SCO_Id; Value : Boolean) return BDD_Node_Id
   is (Next_BDD_Node (SC_Vectors, SCO, Value));

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

   function Outcome
     (Vectors : Source_Coverage_Vectors; SCO : SCO_Id; Value : Boolean)
      return Tristate
   is
      use BDD;
      Cond_SCO   : SCO_Id := SCO;
      Cond_Value : Boolean := Value;
   begin
      loop
         declare
            BDDN : constant BDD_Node :=
              Vectors.BDD_Vector.Constant_Reference
                (Next_BDD_Node (Vectors, Cond_SCO, Cond_Value));
         begin
            case BDDN.Kind is
               when Outcome   =>
                  return To_Tristate (BDDN.Decision_Outcome);

               when Condition =>
                  declare
                     Next_Value : constant Tristate :=
                       SC_Obligations.Value (Vectors, BDDN.C_SCO);
                  begin
                     if Next_Value = Unknown then
                        return Unknown;
                     end if;
                     Cond_SCO := BDDN.C_SCO;
                     Cond_Value := To_Boolean (Next_Value);
                  end;

               when others    =>
                  raise Program_Error;
            end case;
         end;
      end loop;
   end Outcome;

   function Outcome (SCO : SCO_Id; Value : Boolean) return Tristate
   is (Outcome (SC_Vectors, SCO, Value));

   -----------
   -- Value --
   -----------

   function Value
     (Vectors : Source_Coverage_Vectors; SCO : SCO_Id) return Tristate
   is
      SCOD : SCO_Descriptor renames
        Vectors.SCO_Vector.Constant_Reference (SCO);
      pragma Assert (SCOD.Kind = Condition);
   begin
      return SCOD.Value;
   end Value;

   function Value (SCO : SCO_Id) return Tristate
   is (Value (SC_Vectors, SCO));

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

   function Get_Path_Count_Limit return Natural
   is (SC_Obligations.BDD.Path_Count_Limit);

   --------------------
   -- Prealloc_Lines --
   --------------------

   procedure Prealloc_Lines
     (Cur_Source_File : Source_File_Index; Last_Line : in out Natural) is
   begin
      if Cur_Source_File /= No_Source_File and then Last_Line > 0 then
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
                  Msg  =>
                    "no conditional branch (in "
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

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector
     (Cond_Values : Condition_Values_Array)
      return Condition_Evaluation_Vectors.Vector
   is
      Result : Condition_Evaluation_Vectors.Vector :=
        Condition_Evaluation_Vectors.To_Vector
          (Unknown, Length => Cond_Values'Length);

   begin
      for J in Cond_Values'Range loop
         Result.Replace_Element (J, Cond_Values (J));
      end loop;
      return Result;
   end To_Vector;

   --------------------------------------
   -- Populate_From_Static_Eval_Vector --
   --------------------------------------

   procedure Populate_From_Static_Eval_Vector
     (SCO        : SCO_Id;
      Static_Vec : Static_Condition_Values_Vectors.Vector;
      Vec        : out Condition_Evaluation_Vectors.Vector)
   is
      use BDD; -- For using '=' for BDD_Node_Kind

      SCOD  : SCO_Descriptor renames SCO_Vector.Constant_Reference (SCO);
      D_BDD : constant BDD.BDD_Type := SCOD.Decision_BDD;

      Cur : BDD_Node_Id := D_BDD.Root_Condition;
   begin

      --  First, fill the vector with 'unknown'

      Vec := Condition_Evaluation_Vectors.Empty;
      for B of Static_Vec loop
         Vec.Append (Unknown);
      end loop;

      --  Then, walk the BDD from the root and only follow the path of the
      --  constant value. Unencountered nodes will stay unknown, to preserve
      --  short-circuit semantics of operators when performing MCDC analysis.

      while BDD_Vector.Constant_Reference (Cur).Kind = Condition loop
         declare
            C_SCO : constant SCO_Id :=
              BDD_Vector.Constant_Reference (Cur).C_SCO;
            Index : constant Condition_Index :=
              SCO_Vector.Element (C_SCO).Index;
            Value : constant Boolean := Static_Vec (Index);
         begin
            Vec (Index) := To_Tristate (Value);

            Cur := BDD_Vector.Constant_Reference (Cur).Dests (Value);
         end;
      end loop;
   end Populate_From_Static_Eval_Vector;

   ---------------------------
   -- Are_Bit_Maps_In_Range --
   ---------------------------

   function Are_Bit_Maps_In_Range
     (Bit_Maps : CU_Bit_Maps; CU : CU_Info) return Boolean
   is
      subtype SCO_Range is
        SCO_Id range CU.SCOs.First_Element.First .. CU.SCOs.First_Element.Last;
   begin
      return
        (for all SCO of Bit_Maps.Statement_Bits.all => SCO in SCO_Range)
        and then
          (for all Info of Bit_Maps.Decision_Bits.all =>
             Info.D_SCO in SCO_Range)
        and then
          (for all Info of Bit_Maps.MCDC_Bits.all => Info.D_SCO in SCO_Range);
   end Are_Bit_Maps_In_Range;

   ------------------
   -- Set_Bit_Maps --
   ------------------

   procedure Set_Bit_Maps (CU : CU_Id; Bit_Maps : CU_Bit_Maps) is
      use GNAT.SHA1;

      Info       : CU_Info renames CU_Vector.Reference (CU);
      CU_Version : SID_Info renames
        Info.SIDs_Info.Reference (Info.SIDs_Info.First);
      Ctx        : GNAT.SHA1.Context;
      LF         : constant String := (1 => ASCII.LF);

      procedure Update (SCO : SCO_Id);
      --  Helper for fingerprint computation: update Ctx to include a reference
      --  to the given SCO. That infomation is made relative to CU's first SCO,
      --  so that during consolidation, bit maps are treated as equivalent
      --  modulo SCO relocation. For instance, the following units/maps should
      --  be equivalent:
      --
      --    CU #1
      --      First_SCO => 10
      --      Last_SCO  => 12
      --      Statement_Bits => (1 => 10, 2 => 11, 3 => 12)
      --
      --    CU #2
      --      First_SCO => 20
      --      Last_SCO  => 22
      --      Statement_Bits => (1 => 20, 2 => 21, 3 => 22)
      --
      --  Because when we attempt to consolidate CUs #1 and #2, the bit maps
      --  will be equal after the relocation of SCOs #20..#22 to #10..#12.

      ------------
      -- Update --
      ------------

      procedure Update (SCO : SCO_Id) is
         Relative_SCO : constant SCO_Id := SCO - Info.SCOs.First_Element.First;
      begin
         Update (Ctx, Relative_SCO'Image);
      end Update;

   begin
      pragma Assert (Are_Bit_Maps_In_Range (Bit_Maps, Info));

      CU_Version.Bit_Maps := Bit_Maps;

      --  Compute the fingerprint for these bit maps

      Update (Ctx, "stmt:");
      for Id of Bit_Maps.Statement_Bits.all loop
         Update (Id);
      end loop;
      Update (Ctx, LF);

      Update (Ctx, "dc:");
      for D of Bit_Maps.Decision_Bits.all loop
         Update (D.D_SCO);
         Update (Ctx, ":" & D.Outcome'Image);
      end loop;
      Update (Ctx, LF);

      Update (Ctx, "mcdc:");
      for M of Bit_Maps.MCDC_Bits.all loop
         Update (M.D_SCO);
         Update (Ctx, ":" & M.Path_Index'Image);
      end loop;
      Update (Ctx, LF);

      CU_Version.Bit_Maps_Fingerprint :=
        Fingerprint_Type
          (GNAT.SHA1.Binary_Message_Digest'(GNAT.SHA1.Digest (Ctx)));
   end Set_Bit_Maps;

   ----------------
   -- Set_Blocks --
   ----------------

   procedure Set_Blocks (CU : CU_Id; Blocks : SCO_Id_Vector_Vector) is
      Info       : CU_Info renames CU_Vector.Reference (CU);
      CU_Version : SID_Info renames
        Info.SIDs_Info.Reference (Info.SIDs_Info.First);
   begin
      CU_Version.Blocks := Blocks;
   end Set_Blocks;

   ---------------------
   -- Set_Annotations --
   ---------------------

   procedure Set_Annotations (Annotations : ALI_Annotation_Maps.Map) is
      use GNAT.SHA1;

      Current_SFI : Source_File_Index := No_Source_File;
      Current_CU  : CU_Id := No_CU_Id;
      Current_Ctx : GNAT.SHA1.Context := Initial_Context;
      --  Current file being processed

      procedure Set_Annotations_Fingerprint;
      --  Set the annotations fingerprint for Current_CU if it is not null,
      --  stored in Current_Ctx.

      ---------------------------------
      -- Set_Annotations_Fingerprint --
      ---------------------------------

      procedure Set_Annotations_Fingerprint is
      begin
         if Current_CU /= No_CU_Id then
            declare
               SID_Maps : SID_Info_Maps.Map renames
                 CU_Vector.Reference (Current_CU).SIDs_Info;
               SID      : SID_Info renames SID_Maps.Reference (SID_Maps.First);
            begin
               SID.Annotations_Fingerprint :=
                 Fingerprint_Type
                   (GNAT.SHA1.Binary_Message_Digest'
                      (GNAT.SHA1.Digest (Current_Ctx)));
            end;
         end if;
      end Set_Annotations_Fingerprint;

   begin
      --  As a reminder, Source_Location sort on the file index first, so we
      --  are guaranteed to have annotations grouped by source files.

      for Cur in Annotations.Iterate loop
         declare
            use ALI_Annotation_Maps;
            Sloc : constant Source_Location := Key (Cur);
            Ann  : constant ALI_Annotation := Element (Cur);
         begin
            --  If we are entering a new file, reset the hashing context and
            --  dump the annotations fingerprint that was processed.

            if Sloc.Source_File /= Current_SFI then
               Set_Annotations_Fingerprint;
               Current_Ctx := Initial_Context;
               Current_SFI := Sloc.Source_File;
               Current_CU := Comp_Unit (Current_SFI);
            end if;
            Update (Current_Ctx, Image (Sloc));
            Update (Current_Ctx, ALI_Annotation_Kind'Image (Ann.Kind));
            if Ann.Message /= null then
               Update (Current_Ctx, Ann.Message.all);
            end if;
            CU_Vector.Reference (Current_CU).ALI_Annotations.Include
              (Sloc, Ann);
         end;
      end loop;
      Set_Annotations_Fingerprint;
   end Set_Annotations;

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

      Lower_Bound : Local_Source_Location :=
        Local_Source_Location'(Line => 1, Column => 0);
      --  At every step of the check, this designates the minimum possible
      --  source location value for the .Source_Range.L.First_Sloc component
      --  for the next element to inspect.

      procedure Check_Element (Cur : Cursor);
      --  Check that Cur's From/To SCOs range is not empty and
      --  Parent_From .. Parent_To range and that they are correctly ordered.

      -------------------
      -- Check_Element --
      -------------------

      procedure Check_Element (Cur : Cursor) is
         SE    : Scope_Entity renames Tree.Constant_Reference (Cur);
         Child : Cursor := First_Child (Cur);

         Last : Local_Source_Location;
         --  Source_Location upper bound for Cur's last child, or
         --  SE.Source_Range.First_Sloc if there is no child.
      begin
         --  Check that source ranges are never empty

         if SE.Source_Range.L.Last_Sloc < SE.Source_Range.L.First_Sloc then
            raise Failure with "empty source range for " & Image (SE);
         end if;

         --  Check that the SCO range lower bound is both:
         --
         --  * greater or equal to the parent's lower bound (this is the first
         --    half of the nesting check;
         --
         --  * greater than the previous sibling (if any: this checks the
         --    ordering).

         if SE.Source_Range.L.First_Sloc < Lower_Bound then
            raise Failure
              with "source range lower bound too low for " & Image (SE);
         end if;
         Lower_Bound := SE.Source_Range.L.First_Sloc;
         Last := SE.Source_Range.L.First_Sloc;

         while Has_Element (Child) loop
            Check_Element (Child);
            Child := Next_Sibling (Child);
            Last := Lower_Bound;

            --  The next sibling's SCO range cannot overlap with the current's

            Lower_Bound :=
              Local_Source_Location'
                (Line => Lower_Bound.Line, Column => Lower_Bound.Column + 1);
         end loop;

         --  Check that the SCO range upper bound is greater or equal to
         --  the upper bound of the last child's upper bound (this is the
         --  second half of the nesting check).

         if SE.Source_Range.L.Last_Sloc < Last then
            raise Failure
              with "Source location bound too low for " & Image (SE);
         end if;
         Lower_Bound := SE.Source_Range.L.Last_Sloc;
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

         if Scope_Entities_Trace.Is_Active then
            Scope_Entities_Trace.Trace
              ("The following tree of scopes breaks the nesting/ordering"
               & " invariant:");
            Scope_Entities_Trace.Trace (Switches.Exception_Info (Exc));
            Dump (Tree, "| ");
         end if;
         return False;
   end SCOs_Nested_And_Ordered;

   -----------
   -- Floor --
   -----------

   function Floor
     (Tree : Scope_Entities_Trees.Tree; Sloc : Source_Location)
      return Scope_Entities_Trees.Cursor
   is
      use Scope_Entities_Trees;

      Result : Scope_Entities_Trees.Cursor := Scope_Entities_Trees.No_Element;

      procedure Process_Node (Cur : Cursor);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Cur : Cursor) is
      begin
         if In_Range (Sloc, Element (Cur).Source_Range) then
            Result := Cur;
            Iterate_Children (Cur, Process_Node'Access);
         end if;
      end Process_Node;

   begin
      Iterate_Children (Tree.Root, Process_Node'Access);
      return Result;
   end Floor;

   ----------------
   -- Covers_SCO --
   ----------------

   function Covers_SCO (ST : Scope_Traversal_Type; SCO : SCO_Id) return Boolean
   is ((SCO_Vector (SCO).Kind = Fun
        and then Scope_Entities_Trees.Is_Root (ST.Cur))
       or else Covers_SCO (Scope_Entities_Trees.Element (ST.Cur), SCO));

   ------------------
   -- Contains_SCO --
   ------------------

   function Contains_SCO (SCO : SCO_Id; CU : CU_Id) return Boolean is
      Unit : CU_Info renames CU_Vector.Reference (CU);
   begin
      for SCO_Range of Unit.SCOs loop
         if SCO in SCO_Range.First .. SCO_Range.Last then
            return True;
         end if;
      end loop;
      return False;
   end Contains_SCO;

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

      if Scope_Entities_Trace.Is_Active then
         Scope_Entities_Trace.Trace ("Setting scopes for " & Image (CU) & ":");
         Dump (SE, Line_Prefix => "| ");
      end if;
   end Set_Scope_Entities;

   -------------------------------
   -- Set_Operand_Or_Expression --
   -------------------------------

   procedure Set_Operand_Or_Expression
     (SCO : SCO_Id; Position : Operand_Position; Expr : SCO_Id)
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

         when others   =>
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

   ---------------
   -- ALI files --
   ---------------

   -----------------------------------------------
   -- Regular expressions for ALI files parsing --
   -----------------------------------------------

   D_Regexp  : constant String := "(([^""\t ]+)|(""([^""]|"""")+""))[\t ]";
   D_Matcher : constant Pattern_Matcher := Compile (D_Regexp);

   N_Regexp  : constant String :=
     "A([0-9]*):([0-9]*)(:[^ ]*)? xcov ([^ ]*)( ""(.*)"")?";
   N_Matcher : constant Pattern_Matcher := Compile (N_Regexp);

   U_Regexp  : constant String := "^[^\t ]*[\t ]+([^\t ]*)";
   U_Matcher : constant Pattern_Matcher := Compile (U_Regexp);

   V_Regexp  : constant String := "^V ""(.*)""$";
   V_Matcher : constant Pattern_Matcher := Compile (V_Regexp);

   function Unquote (Filename : String) return String;
   --  If needed, unquote a filename, such as the ones that can be found on D
   --  lines.

   procedure Mark_Ignored_Units
     (Excluded_Source_Files : access GNAT.Regexp.Regexp; Deps : SFI_Vector);
   --  Mark SCOs.SCO_Unit_Table entries to be ignored by setting their Dep_Num
   --  to Missing_Dep_Num.

   -------------
   -- Unquote --
   -------------

   function Unquote (Filename : String) return String is
      Result   : Unbounded_String;
      In_Quote : Boolean := False;
      --  True when we just met a double quote inside a quoted filename. False
      --  otherwise.

   begin
      if Filename (Filename'First) /= '"' then
         return Filename;
      else
         --  To unquote, just copy the string removing consecutive double
         --  quotes.

         for C of Filename (Filename'First + 1 .. Filename'Last - 1) loop
            if C = '"' then
               if not In_Quote then
                  Append (Result, C);
               end if;
               In_Quote := not In_Quote;
            else
               Append (Result, C);
            end if;
         end loop;
         return +Result;
      end if;
   end Unquote;

   ------------------------
   -- Mark_Ignored_Units --
   ------------------------

   procedure Mark_Ignored_Units
     (Excluded_Source_Files : access GNAT.Regexp.Regexp; Deps : SFI_Vector)
   is
      use SCOs;
      Deps_Present : constant Boolean := not Deps.Is_Empty;
   begin
      if Excluded_Source_Files = null then
         return;
      end if;

      for J in SCO_Unit_Table.First + 1 .. SCO_Unit_Table.Last loop
         declare
            U : SCO_Unit_Table_Entry renames SCO_Unit_Table.Table (J);

            SFI : constant Source_File_Index :=
              (if Deps_Present
               then Deps.Element (U.Dep_Num)
               else
                 Get_Index_From_Generic_Name (U.File_Name.all, Source_File));
            --  Source file corresponding to this unit. Use the name from the
            --  SCO "C" line if there are no list of dependencies ("D" lines,
            --  missing for old GLI files for C).

            Match_Name : constant String := Get_Simple_Name (SFI);
            --  Name to use for matching against the pattern of ignored files
         begin
            --  In case we got SFI from the Deps vector, make sure it is
            --  considered as a source file.

            Consolidate_File_Kind (SFI, Source_File);

            --  Do the ignored file matching itself

            if GNAT.Regexp.Match (Match_Name, Excluded_Source_Files.all) then
               Consolidate_Ignore_Status (SFI, Always);
               U.Dep_Num := Missing_Dep_Num;
            else
               Consolidate_Ignore_Status (SFI, Never);
            end if;
         end;
      end loop;
   end Mark_Ignored_Units;

   ---------------------
   -- Get_Annotations --
   ---------------------

   function Get_Annotations (CU : CU_Id) return ALI_Annotation_Maps.Map is
   begin
      if CU = No_CU_Id then
         return ALI_Annotation_Maps.Empty_Map;
      end if;
      return CU_Vector.Element (CU).ALI_Annotations;
   end Get_Annotations;

   function Get_Annotations
     (SFI : Source_File_Index) return ALI_Annotation_Maps.Map is
   begin
      return Get_Annotations (Comp_Unit (SFI));
   end Get_Annotations;

   --------------------
   -- Get_Annotation --
   --------------------

   function Get_Annotation
     (Sloc : Source_Location) return ALI_Annotation_Maps.Cursor
   is
      CU : constant CU_Id := Comp_Unit (Sloc.Source_File);
   begin
      if CU = No_CU_Id then
         return ALI_Annotation_Maps.No_Element;
      end if;
      return CU_Vector.Constant_Reference (CU).ALI_Annotations.Find (Sloc);
   end Get_Annotation;

   -------------------------
   -- Get_All_Annotations --
   -------------------------

   function Get_All_Annotations return ALI_Annotation_Maps.Map is
      use ALI_Annotation_Maps;
      Result : Map;
   begin
      for CU of CU_Vector loop
         for Cur in CU.ALI_Annotations.Iterate loop
            Result.Insert (Key (Cur), Element (Cur));
         end loop;
      end loop;
      return Result;
   end Get_All_Annotations;

   -----------------------------------
   -- Inc_Violation_Exemption_Count --
   -----------------------------------

   procedure Inc_Violation_Exemption_Count (Sloc : Source_Location) is
      E : ALI_Annotation renames
        CU_Vector.Reference (Comp_Unit (Sloc.Source_File))
          .ALI_Annotations
          .Reference (Sloc);
   begin
      E.Violation_Count := E.Violation_Count + 1;
   end Inc_Violation_Exemption_Count;

   -----------------------------------
   -- Inc_Undet_Cov_Exemption_Count --
   -----------------------------------

   procedure Inc_Undet_Cov_Exemption_Count (Sloc : Source_Location) is
      E : ALI_Annotation renames
        CU_Vector.Reference (Comp_Unit (Sloc.Source_File))
          .ALI_Annotations
          .Reference (Sloc);
   begin
      E.Undetermined_Cov_Count := E.Undetermined_Cov_Count + 1;
   end Inc_Undet_Cov_Exemption_Count;

   ------------------------------
   -- Reset_Exemption_Counters --
   ------------------------------

   procedure Reset_Exemption_Counters is
      use ALI_Annotation_Maps;
   begin
      for CU of CU_Vector loop
         for Cur in CU.ALI_Annotations.Iterate loop
            declare
               A : ALI_Annotation renames CU.ALI_Annotations.Reference (Cur);
            begin
               A.Violation_Count := 0;
               A.Undetermined_Cov_Count := 0;
            end;
         end loop;
      end loop;
   end Reset_Exemption_Counters;

   --------------
   -- Load_ALI --
   --------------

   procedure Load_ALI (ALI_Filename : String) is
      Discard_ALI                 : Source_File_Index;
      Discard_Units, Discard_Deps : SFI_Vector;
      Discard_Created_Units       : Created_Unit_Maps.Map;

      ALI_Annotations : ALI_Annotation_Maps.Map;
      ALI_Index       : constant Source_File_Index :=
        Get_Index_From_Full_Name (ALI_Filename, Library_File, Insert => False);
   begin
      Discard_ALI :=
        Load_ALI
          (ALI_Filename          => ALI_Filename,
           Excluded_Source_Files => null,
           Units                 => Discard_Units,
           Deps                  => Discard_Deps,
           ALI_Annotations       => ALI_Annotations,
           With_SCOs             => False);

      --  Add the annotations to the internal CU_Vector vector

      for Cur in ALI_Annotations.Iterate loop
         declare
            Sloc         : constant Source_Location :=
              ALI_Annotation_Maps.Key (Cur);
            Ignore_CU_Id : CU_Id;
         begin
            if not CU_Map.Contains (Sloc.Source_File) then
               Ignore_CU_Id :=
                 Allocate_CU
                   (Provider      => Compiler,
                    Origin        => ALI_Index,
                    Main_Source   => Sloc.Source_File,
                    Fingerprint   => Fingerprint_Type'(others => 0),
                    Created_Units => Discard_Created_Units);
            end if;
            CU_Vector.Reference (CU_Map.Element (Sloc.Source_File))
              .ALI_Annotations
              .Include (Sloc, ALI_Annotation_Maps.Element (Cur));
         end;
      end loop;
   end Load_ALI;

   --------------
   -- Load_ALI --
   --------------

   function Load_ALI
     (ALI_Filename          : String;
      Excluded_Source_Files : access GNAT.Regexp.Regexp;
      Units                 : out SFI_Vector;
      Deps                  : out SFI_Vector;
      ALI_Annotations       : in out ALI_Annotation_Maps.Map;
      With_SCOs             : Boolean) return Source_File_Index
   is
      ALI_File  : File_Type;
      ALI_Index : Source_File_Index;

      Line  : String_Access;
      Index : Natural;

      Matches : Match_Array (0 .. 10);
      --  For regex matching

      function Match (Index : Integer) return String;
      --  Return Index'th match in Line

      function Get_Stripped_Line (F : File_Type) return String;
      --  Like Get_Line but strip trailing CR, to allow for processing Windows
      --  LI files on a UNIX host.

      function Getc return Character;
      --  Consume and return next character from Line.
      --  Load next line if at end of line. Return ^Z if at end of file.

      function Nextc return Character;
      --  Peek at next character in Line. Return ^Z if at end of file.

      procedure Skipc;
      --  Skip one character in Line

      function Check_Message (M1, M2 : String_Access) return Boolean;
      --  Return True if either M1 or M2 is null or designates an empty string,
      --  else return True if M1 and M2 designate identical strings.

      -------------------
      -- Check_Message --
      -------------------

      function Check_Message (M1, M2 : String_Access) return Boolean is
      begin
         return
           False
           or else M1 = null
           or else M1.all = ""
           or else M2 = null
           or else M2.all = ""
           or else M1.all = M2.all;
      end Check_Message;

      ----------
      -- Getc --
      ----------

      function Getc return Character is
         Next_Char : constant Character := Nextc;
      begin
         Index := Index + 1;
         if Index > Line'Last + 1 then

            --  Note: normally we should just read the next line from ALI_File
            --  and reset Index. However some older versions of the compiler
            --  generated duplicated SCOs in some cases, so if we get two
            --  successive identical lines, we ignore them and keep reading.

            while not End_Of_File (ALI_File) loop
               declare
                  Next_Line : constant String := Get_Stripped_Line (ALI_File);
               begin
                  if Next_Line = Line.all then
                     Report
                       ("ignoring duplicate line in ALI file " & ALI_Filename,
                        Kind => Warning);

                  else
                     Free (Line);
                     Line := new String'(Next_Line);
                     Index := 1;

                     exit;
                  end if;
               end;
            end loop;
         end if;
         return Next_Char;
      end Getc;

      -----------------------
      -- Get_Stripped_Line --
      -----------------------

      function Get_Stripped_Line (F : File_Type) return String is
         Line : constant String := Get_Line (F);
         Last : Integer := Line'Last;
      begin
         if Last in Line'Range and then Line (Last) = ASCII.CR then
            Last := Last - 1;
         end if;
         return Line (Line'First .. Last);
      end Get_Stripped_Line;

      -----------
      -- Match --
      -----------

      function Match (Index : Integer) return String is
      begin
         if Matches (Index) = No_Match then
            return "";
         else
            return Line (Matches (Index).First .. Matches (Index).Last);
         end if;
      end Match;

      -----------
      -- Nextc --
      -----------

      function Nextc return Character is
      begin
         if Index = Line'Last + 1 then
            return ASCII.LF;

         elsif Index in Line'Range then
            return Line (Index);

         else
            return Character'Val (16#1a#);
         end if;
      end Nextc;

      -----------
      -- Skipc --
      -----------

      procedure Skipc is
         C : Character;
         pragma Unreferenced (C);
      begin
         C := Getc;
      end Skipc;

      procedure Get_SCOs_From_ALI is new Get_SCOs;

      --  Local variables

      No_Object : Boolean := False;
      --  Set True if the P line contains the NO flag

      Preserve_Control_Flow_Seen : Boolean := False;
      --  Set True if unit has been compiled with -fpreserve-control-flow

      Dump_SCOs_Seen : Boolean := False;
      --  Set True if unit has been compiled with -fdump-scos (or -gnateS)

      Debug_Seen : Boolean := False;
      --  Set True if unit has been compiled with -g

      Profile_Arcs_Seen : Boolean := False;
      --  Set True if unit has been compiled with -fprofile-arcs

      Expected_Annotation_Kind : ALI_Annotation_Kind;
      Expected_Annotation_Msg  : String_Access;
      --  Variables for checking of annotation validity: annotations must
      --  come in (Exempt_On, Exempt_Off) pairs, nesting forbidden, and
      --  the Exempt_Off message must be either empty or identical to the
      --  Exempt_On one.

      --  Start of processing for Load_ALI

   begin
      pragma Assert (Deps.Last_Index = 0);

      --  First check whether this ALI has been already loaded. We identify
      --  this by the fact that it already has an assigned Source_File_Index.

      ALI_Index :=
        Get_Index_From_Full_Name (ALI_Filename, Library_File, Insert => False);
      if ALI_Index /= No_Source_File then
         Report
           ("ignoring duplicate ALI file " & ALI_Filename, Kind => Warning);
         return No_Source_File;
      end if;

      ALI_Index :=
        Get_Index_From_Full_Name (ALI_Filename, Library_File, Insert => True);
      Log_File_Open (ALI_Filename);
      Open (ALI_File, In_File, ALI_Filename);

      --  Check that the first line is a valid ALI V line.

      declare
         V_Line    : constant String := Get_Stripped_Line (ALI_File);
         Error_Msg : Unbounded_String;
      begin
         Match (V_Matcher, V_Line, Matches);
         if Matches (0) = No_Match then
            Error_Msg := +("malformed ALI file """ & ALI_Filename & """");

            if V_Line'Length > 3
              and then
                To_Lower (V_Line (V_Line'Last - 3 .. V_Line'Last)) = ".ali"
            then
               Append
                 (Error_Msg,
                  ASCII.LF
                  & "to load ALIs from list use ""--scos=@"
                  & ALI_Filename
                  & """");
            end if;
            Fatal_Error (+Error_Msg);
         end if;
      end;

      --  Here once the ALI file has been succesfully opened

      SCOs_Trace.Trace ("Loading SCOs from " & ALI_Filename);

      Expected_Annotation_Kind := Exempt_On;
      Expected_Annotation_Msg := null;

      Scan_ALI : while not End_Of_File (ALI_File) loop
         loop
            Free (Line);
            Line := new String'(Get_Stripped_Line (ALI_File));
            exit when Line'Length > 0;
         end loop;

         case Line (1) is
            when 'A'    =>
               if Line.all = "A -fpreserve-control-flow" then
                  Preserve_Control_Flow_Seen := True;

               elsif Line.all = "A -fdump-scos" or else Line.all = "A -gnateS"
               then
                  Dump_SCOs_Seen := True;

               elsif Line.all = "A -g" then
                  Debug_Seen := True;

               elsif Line.all = "A -fprofile-arcs" then
                  Profile_Arcs_Seen := True;
               end if;

            when 'P'    =>
               declare
                  P_Start : Integer;
               begin
                  P_Start := 2;
                  loop
                     while P_Start <= Line'Last and then Line (P_Start) = ' '
                     loop
                        P_Start := P_Start + 1;
                     end loop;
                     exit when P_Start > Line'Last - 1;

                     declare
                        Param : constant String (1 .. 2) :=
                          Line (P_Start .. P_Start + 1);
                     begin
                        if Param = "NO" then
                           No_Object := True;
                        end if;
                     end;

                     P_Start := P_Start + 2;
                  end loop;
               end;

            when 'U'    =>
               Match (U_Matcher, Line (3 .. Line'Last), Matches);
               if Matches (0) /= No_Match then
                  Units.Append
                    (Get_Index_From_Generic_Name (Match (1), Source_File));
               end if;

            when 'D'    =>
               Match (D_Matcher, Line (3 .. Line'Last), Matches);
               if Matches (0) /= No_Match then

                  --  Dependency files are source files. However, in order to
                  --  avoid unnecessary conflicts at consolidation time, we
                  --  don't want to consider them for coverage analysis unless
                  --  they are in the units of interest. So consider them as
                  --  stubs at this stage.

                  Deps.Append
                    (Get_Index_From_Generic_Name
                       (Unquote (Match (1)), Stub_File));
               end if;

            when 'N'    =>
               declare
                  Annotation : ALI_Annotation;
                  Valid      : Boolean;
                  Sloc       : Source_Location;
               begin
                  Match (N_Matcher, Line (3 .. Line'Last), Matches);
                  if Matches (0) /= No_Match then
                     declare
                        Note_SFN : constant String := Match (3);
                        Note_SFI : Source_File_Index := Units.Last_Element;

                     begin
                        if Note_SFN'Length > 0 then

                           --  Case of a separate: the source file is not the
                           --  current compilation unit but some other one
                           --  identified explicitly.

                           Note_SFI :=
                             Get_Index_From_Generic_Name
                               (Note_SFN (Note_SFN'First + 1 .. Note_SFN'Last),
                                Source_File);
                        end if;

                        Sloc :=
                          (Source_File => Note_SFI,
                           L           =>
                             (Line   => Integer'Value (Match (1)),
                              Column => Integer'Value (Match (2))));
                     end;

                     Valid := True;

                     declare
                        Msg : constant String := Match (6);
                     begin
                        Annotation :=
                          (Kind    => ALI_Annotation_Kind'Value (Match (4)),
                           Message =>
                             (if Msg'Length > 0
                              then new String'(Msg)
                              else null),
                           others  => <>);
                     exception
                        when Constraint_Error =>
                           Report (Sloc, "bad annotation " & Match (4));
                           Valid := False;
                     end;

                     if Valid then
                        if Annotation.Kind /= Expected_Annotation_Kind then
                           Report
                             (Sloc,
                              "unexpected "
                              & Annotation.Kind'Img
                              & " "
                              & Annotation.Message.all
                              & " (expected "
                              & Expected_Annotation_Kind'Img
                              & ")");
                        elsif not Check_Message
                                    (Annotation.Message,
                                     Expected_Annotation_Msg)
                        then
                           Report
                             (Sloc,
                              "unexpected EXEMPT_OFF "
                              & Annotation.Message.all
                              & " (expected "
                              & Expected_Annotation_Msg.all
                              & ")");
                        end if;

                        if Annotation.Kind = Exempt_On then
                           if Annotation.Message = null then
                              Report (Sloc, "empty message for EXEMPT_ON");
                           end if;

                           Expected_Annotation_Kind := Exempt_Off;
                           Expected_Annotation_Msg := Annotation.Message;

                        else
                           Expected_Annotation_Kind := Exempt_On;
                           Expected_Annotation_Msg := null;
                        end if;

                        ALI_Annotations.Insert
                          (Key => Sloc, New_Item => Annotation);
                     end if;
                  end if;
               end;

            when 'C'    =>
               exit Scan_ALI;

            when others =>
               null;
         end case;
      end loop Scan_ALI;

      if Expected_Annotation_Kind = Exempt_Off then
         declare
            use ALI_Annotation_Maps;
            Last_Ann_Cursor : constant Cursor := ALI_Annotations.Last;
            Last_Ann_Sloc   : constant Source_Location :=
              Key (Last_Ann_Cursor);
            Last_Ann        : constant ALI_Annotation :=
              Element (Last_Ann_Cursor);
         begin
            Report
              (Last_Ann_Sloc, "missing Exempt_Off " & Last_Ann.Message.all);
         end;
      end if;

      if With_SCOs then
         if No_Object then
            Warn ("no object generated for " & ALI_Filename);

         else
            if not Preserve_Control_Flow_Seen then
               Warn
                 (ALI_Filename
                  & ": unit compiled without -fpreserve-control-flow");
            end if;

            if not Dump_SCOs_Seen then
               Warn
                 (ALI_Filename
                  & ": unit compiled without SCO generation (-fdump-scos)");
            end if;

            if not Debug_Seen then
               Warn
                 (ALI_Filename
                  & ": unit compiled without debug information (-g)");
            end if;

            if Profile_Arcs_Seen then
               Warn
                 (ALI_Filename
                  & ": unit compiled with instrumentation (-fprofile-arcs)");
            end if;
         end if;

         if not End_Of_File (ALI_File)
           and then Dump_SCOs_Seen
           and then not No_Object
         then
            Index := 1;
            Get_SCOs_From_ALI;
            Mark_Ignored_Units (Excluded_Source_Files, Deps);

         else
            --  In this case, we will not parse SCOs: reset parsing tables so
            --  that further processing don't start using stale SCO entries.

            SCOs.Initialize;
         end if;
      end if;

      Close (ALI_File);

      if Line /= null then
         Free (Line);
      end if;
      return ALI_Index;
   end Load_ALI;

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
     (Sloc : Source_Location; Include_Decisions : Boolean := False)
      return SCO_Id
   is
      use Sloc_To_SCO_Maps;

      L_Sloc   : Source_Location := Sloc;
      Cur      : Cursor;
      SCO      : SCO_Id;
      SCO_Sloc : Local_Source_Location_Range;

   begin
      if Sloc.Source_File = No_Source_File then
         return No_SCO_Id;
      end if;

      --  If looking up the sloc of a NOT operator, return SCO of innermost
      --  operand, if it is a condition.

      Cur :=
        Sloc_To_SCO_Map (Sloc.Source_File, Operator).Find
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

      Cur :=
        Sloc_To_SCO_Map (L_Sloc.Source_File, Condition).Floor
          ((L_Sloc.L, L_Sloc.L));
      if Cur /= No_Element then
         SCO := Element (Cur);
         SCO_Sloc := Key (Cur);
      end if;

      --  Now we have a candidate condition SCO. Look for a better match
      --  with a statement.

      Cur :=
        Sloc_To_SCO_Map (L_Sloc.Source_File, Statement).Floor
          ((L_Sloc.L, L_Sloc.L));
      if Cur /= No_Element
        and then (SCO = No_SCO_Id or else SCO_Sloc < Key (Cur))
      then
         SCO := Element (Cur);
      end if;

      --  Climb up the SCO tree until an adequate match is found

      Climb_SCO_Tree : while SCO /= No_SCO_Id loop
         Climb_Operators : while SCO /= No_SCO_Id
           and then Kind (SCO) = Operator
         loop
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
                 Sloc.L.Line
                 in Sloc_Range.L.First_Sloc.Line .. Sloc_Range.L.Last_Sloc.Line
                 and then
                   (Kind = Statement
                    or else (Include_Decisions and then Kind = Decision));
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
        and then (SCO = No_SCO_Id or else Kind (SCO) = Statement)
      then
         Cur :=
           Sloc_To_SCO_Map (Sloc.Source_File, Decision).Find
             ((Sloc.L, No_Local_Location));

         if Cur /= No_Element then
            pragma
              Assert
                (SCO = No_SCO_Id
                   or else SCO = Enclosing_Statement (Element (Cur)));
            SCO := Element (Cur);
         end if;
      end if;

      --  A fuzzy match is specified as never returning a condition

      pragma
        Assert
          (not (Sloc.L.Column = 0
                and then SCO /= No_SCO_Id
                and then Kind (SCO) = Condition));
      return SCO;
   end Sloc_To_SCO;

   ----------------------
   -- To_Decision_Kind --
   ----------------------

   function To_Decision_Kind (C : Character) return Decision_Kind is
   begin
      case C is
         when 'E'    =>
            return Exit_Statement;

         when 'G'    =>
            return Entry_Guard;

         when 'I'    =>
            return If_Statement;

         when 'P'    =>
            return Pragma_Decision;

         when 'W'    =>
            return While_Loop;

         when 'X'    =>
            return Expression;

         when 'A'    =>
            return Aspect;

         when others =>
            raise Constraint_Error;
      end case;
   end To_Decision_Kind;

   -----------------------
   -- To_Statement_Kind --
   -----------------------

   function To_Statement_Kind (C : Character) return Statement_Kind is
   begin
      case C is
         when 't'    =>
            return Type_Declaration;

         when 's'    =>
            return Subtype_Declaration;

         when 'o'    =>
            return Object_Declaration;

         when 'r'    =>
            return Renaming_Declaration;

         when 'i'    =>
            return Generic_Instantiation;

         when 'd'    =>
            return Other_Declaration;

         when 'c'    =>
            return Call_Stmt;

         when 'e'    =>
            return Call_Expr;

         when 'A'    =>
            return Accept_Statement;

         when 'C'    =>
            return Case_Statement;

         when 'E'    =>
            return Exit_Statement;

         when 'F'    =>
            return For_Loop_Statement;

         when 'I'    =>
            return If_Statement;

         when 'P'    =>
            return Pragma_Statement;

         when 'p'    =>
            return Disabled_Pragma_Statement;

         when 'R'    =>
            return Extended_Return_Statement;

         when 'S'    =>
            return Select_Statement;

         when 'W'    =>
            return While_Loop_Statement;

         when 'X'    =>
            return Degenerate_Subprogram_Statement;

         when ' '    =>
            return Other_Statement;

         when others =>
            raise Constraint_Error;
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
     (Pragma_Name : Name_Id) return Pragma_Id is
   begin
      if Pragma_Name = No_Name then
         return Pragma_Unknown;
      end if;

      --  Retrieve the pragma name as a string

      Get_Name_String (Pragma_Name);
      Name_Buffer (1 .. Name_Len) := To_Lower (Name_Buffer (1 .. Name_Len));

      --  Try to get the Pragma_Id value corresponding to that name. If there
      --  is no such value, return Unknown_Pragma.

      declare
         Enum_Name : constant String :=
           "Pragma_" & Name_Buffer (1 .. Name_Len);
      begin
         return Pragma_Id'Value (Enum_Name);
      exception
         when Constraint_Error =>
            return Pragma_Unknown;
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
