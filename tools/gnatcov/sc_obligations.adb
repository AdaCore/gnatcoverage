------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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
with Ada.Streams;             use Ada.Streams;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.SHA1;

with ALI_Files;     use ALI_Files;
with Aspects;       use Aspects;
with Checkpoints;   use Checkpoints;
with Coverage.Tags; use Coverage, Coverage.Tags;
with Diagnostics;   use Diagnostics;
with Files_Table;   use Files_Table;
with Interfaces;
with Namet;         use Namet;
with Outputs;       use Outputs;
with SCOs;
with Strings;       use Strings;
with Switches;      use Switches;
with Traces_Elf;    use Traces_Elf;

package body SC_Obligations is

   subtype Source_Location is Slocs.Source_Location;
   No_Location : Source_Location renames Slocs.No_Location;
   --  (not SCOs.Source_Location)

   -------------------------------------
   -- Low-level SCO tables management --
   -------------------------------------

   type SCOs_Hash is new GNAT.SHA1.Binary_Message_Digest;

   function SCO_Tables_Fingerprint return SCOs_Hash;
   --  Return a fingerprint for all low-level SCO tables

   --------------------------------------------
   -- Management of binary decision diagrams --
   --------------------------------------------

   package BDD is
      --  Outgoing arcs from a BDD node

      subtype Valid_BDD_Node_Id is BDD_Node_Id
        range No_BDD_Node_Id + 1 .. BDD_Node_Id'Last;

      type Destinations is array (Boolean) of BDD_Node_Id;

      --  BDD node kinds

      type BDD_Node_Kind is
        (Outcome,
         --  Leaf (decision outcome is determined)

         Condition,
         --  Evaluate condition

         Jump);
         --  Indirect reference to another BDD node

      type BDD_Node (Kind : BDD_Node_Kind := Outcome) is record
         case Kind is
            when Outcome =>
               Decision_Outcome : Boolean := False;
               --  Value of the decision when this node is reached

            when Condition =>
               Parent : BDD_Node_Id := No_BDD_Node_Id;
               --  Previous BDD node, if known and unique. Set to
               --  No_BDD_Node_Id for the root node and for any node reachable
               --  through multiple paths.

               Parent_Value : Boolean := False;
               --  When Parent /= No_BDD_Node_Id, value of Parent for which
               --  this node is the successor.

               C_SCO : SCO_Id;
               --  Condition SCO

               Dests : Destinations;
               --  Outgoing arcs depending on this condition

            when Jump =>
               Dest : BDD_Node_Id := No_BDD_Node_Id;
               --   Next BDD node
         end case;
      end record;

      --  Stream attributes for BDD_Node, aware of checkpoint format versioning

      pragma Warnings (Off, "* is not referenced");
      --  Work around compiler bug: bogus warning???

      procedure Read
        (S : access Root_Stream_Type'Class;
         V : out BDD_Node);
      procedure Write
        (S : access Root_Stream_Type'Class;
         V : BDD_Node);
      pragma Warnings (On, "* is not referenced");

      for BDD_Node'Read use Read;
      for BDD_Node'Write use Write;

      package BDD_Vectors is
        new Ada.Containers.Vectors
          (Index_Type   => Valid_BDD_Node_Id,
           Element_Type => BDD_Node);
      BDD_Vector : BDD_Vectors.Vector;

      type Reachability is array (Boolean) of Boolean;

      type BDD_Type is record
         Decision       : SCO_Id;

         Root_Condition : BDD_Node_Id := No_BDD_Node_Id;
         --  First tested condition

         First_Node     : BDD_Node_Id := No_BDD_Node_Id;
         Last_Node      : BDD_Node_Id := No_BDD_Node_Id;
         --  Index range of BDD nodes of this BDD

         Diamond_Base   : BDD_Node_Id := No_BDD_Node_Id;
         --  If set, designates a node that is reachable through multiple paths
         --  (see Check_Diamonds).

         Reachable_Outcomes : Reachability := (others => False);
         --  Indicates whether each outcome is reachable (an outcome may be
         --  unreachable due to constant conditions).
      end record;

      pragma Warnings (Off, "* is not referenced");
      procedure Read
        (S : access Root_Stream_Type'Class;
         V : out BDD_Type);
      procedure Write
        (S : access Root_Stream_Type'Class;
         V : BDD_Type);
      pragma Warnings (On, "* is not referenced");

      for BDD_Type'Read use Read;
      for BDD_Type'Write use Write;

      procedure Allocate
        (BDD     : in out BDD_Type;
         Node    : BDD_Node;
         Node_Id : out BDD_Node_Id);
      --  Allocate a node within the given BDD with the given properties

      ------------------------------------
      -- Building the BDD of a decision --
      ------------------------------------

      --  The BDD is built while scanning the various items (conditions and
      --  operators) that make up a decision. The BDD is rooted at the first
      --  condition; each node is either the evaluation of a condition
      --  (with two outgoing arcs pointing to the continuation of the
      --  evaluation, depending on the condition's value), or a leaf indicating
      --  that the outcome of the decision has been fully determined. During
      --  BDD construction, a third type of node can appear, which is a Jump
      --  to another node (i.e. a node that has just one outgoing arc).

      --  The BDD is built by maintaining a stack of triples of BDD node ids.
      --  The node at the top of the stack designates the destinations that
      --  shall be assigned to the True and False outcomes of the subtree
      --  that is about to be scanned. The third node id, if not null, is
      --  the id of a Jump node that shall be connected to the root of the
      --  subtree about to be read.

      --  Initially, the destinations are the decision outcome leaves, and
      --  the origin is No_BDD_Node_Id.

      --  When a NOT operator is read, the True and False destinations of the
      --  top stack item are swapped.

      --  When an AND THEN operator is read, the top item is popped, and two
      --  items are pushed (corresponding to the two subtrees for the two
      --  operands). A new Jump node is allocated. The arcs for the right
      --  operand are:
      --
      --    Dest_True => Popped_Dest_True
      --      (if right op is True then overall subtree is True)
      --    Dest_False => Popped_Dest_False
      --      (if right op is True then overall subtree is False)
      --    Origin => Jump_Node
      --      (evaluation of right operand is attached as a destination
      --       of the left operand test)
      --
      --   and those for the left operand are:
      --    Dest_True => Jump_Node
      --      (if left op is True then evaluate right op)
      --    Dest_False => Popped_Dest_False
      --      (if right op is False then overall subtree is False)
      --    Origin => Popped_Origin
      --
      --  When an OR ELSE operator is read, a similar processing occurs.
      --
      --  When a condition is read, the top item is popped and a new Condition
      --  node is allocated. Its destinations are set from the popped item,
      --  and if an origin Jump node is present, then its destination is set
      --  to the id of the newly-allocated condition.
      --
      --  At the end of the processing for a decision, the stack is empty,
      --  and the BDD is simplified by replacing all references to jump nodes
      --  with direct references to their destinations.

      type Arcs is record
         Dests  : Destinations;
         --  Outgoing arcs for next condition

         Origin : BDD_Node_Id := No_BDD_Node_Id;
         --  Jump node referencing next condition

         O_SCO : SCO_Id;
         O_Pos : Operand_Position;
         --  Reference to parent operator and position (Left or Right) in it
      end record;

      procedure Push (A : Arcs);
      function Pop return Arcs;
      --  Manage a stack of Arcs

      --  Construction of a BDD

      function Create (Decision : SCO_Id) return BDD_Type;
      --  Start construction of a new BDD for the given decision

      procedure Process_Not      (O_SCO : SCO_Id; BDD : BDD_Type);
      procedure Process_And_Then (O_SCO : SCO_Id; BDD : in out BDD_Type);
      procedure Process_Or_Else  (O_SCO : SCO_Id; BDD : in out BDD_Type);
      --  Process NOT, AND THEN, OR ELSE operators

      procedure Process_Condition
        (BDD          : in out BDD_Type;
         Condition_Id : SCO_Id);
      --  Process condition

      procedure Completed (BDD : in out BDD_Type);
      --  Called when all items in decision have been processed

      procedure Dump_BDD (BDD : BDD_Type);
      --  Display BDD for debugging purposes

   end BDD;

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

   Inst_Vector : Inst_Info_Vectors.Vector;

   ------------------------
   -- Source units table --
   ------------------------

   type CU_Info (Provider : SCO_Provider := SCO_Provider'First) is record
      Origin      : Source_File_Index;
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

      Deps        : SFI_Vector;
      --  Mapping of this unit's dependency numbers to source file indices

      Has_Code    : Boolean := False;
      --  Set True when object code for some source file in this unit is seen

      Fingerprint : SCOs_Hash;
      --  Hash of SCO info in ALI, for incremental coverage consistency check

      case Provider is
         when Compiler =>
            null;

         when Instrumenter =>
            Bit_Maps : CU_Bit_Maps;
      end case;
   end record;

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

   CU_Vector : CU_Info_Vectors.Vector;
   --  Vector of compilation unit info (one entry per LI file)

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

   function Instance_Loc (Inst_Index : Inst_Id) return String;
   --  Return a string representation of the instantiation location denoted
   --  by Inst_Index, which must be in Comp_Unit's instance range.

   -------------------------------
   -- Main SCO descriptor table --
   -------------------------------

   function To_Statement_Kind (C : Character) return Statement_Kind;
   --  Convert character code for statement kind to corresponding enum value

   --  Decision_Kind denotes the various decision kinds identified in SCOs

   type Decision_Kind is
     (If_Statement,
      Exit_Statement,
      Entry_Guard,
      Pragma_Decision,
      While_Loop,
      Expression,
      Aspect);

   function To_Decision_Kind (C : Character) return Decision_Kind;
   --  Convert character code for decision kind to corresponding enum value

   type Operand_Pair is array (Operand_Position) of SCO_Id;

   type SCO_Descriptor (Kind : SCO_Kind := SCO_Kind'First) is record
      Origin : CU_Id;
      --  Compilation unit whose LI file containing this SCO

      Sloc_Range : Source_Location_Range := No_Range;
      --  For a decision, cumulative range from all conditions

      Parent : SCO_Id := No_SCO_Id;
      --  For a decision, pointer to the enclosing statement (or condition in
      --  the case of a nested decision), unset if decision is part of a
      --  flow control structure.
      --  For a condition or operator, pointer to the enclosing operator, or to
      --  enclosing decision if at top level.

      case Kind is
         when Statement =>
            S_Kind         : Statement_Kind;
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

            Pragma_Name : Pragma_Id;
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

         when Decision | Operator =>
            Operands : Operand_Pair := (others => No_SCO_Id);
            --  Operands of this operator (for a decision, only the right
            --  operand is set, and it points to the top expression node.

            case Kind is
               when Decision =>
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

               when Operator =>
                  Op_Kind : Operator_Kind;

               when others =>
                  null;
            end case;
      end case;
   end record;

   No_SCO_Descriptor : constant SCO_Descriptor :=
     (Kind   => Statement,
      Origin => No_CU_Id,
      others => <>);

   package SCO_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Valid_SCO_Id,
        Element_Type => SCO_Descriptor);
   SCO_Vector : SCO_Vectors.Vector;

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
      CLS    : Checkpoints.Checkpoint_Load_State;
      CP_Tag : SC_Tag) return SC_Tag;

   package R is new Tag_Providers.Register_Factory
     (Name => "instance", T => Instance_Tag_Provider_Type);
   pragma Unreferenced (R);

   -----------------------------------------
   -- Helper routines for Checkpoint_Load --
   -----------------------------------------

   --  CP_SCO_Vectors holds all SCO-related data loaded from a checkpoint

   type CP_SCO_Vectors is record
      CU_Vector       : CU_Info_Vectors.Vector;
      ALI_Annotations : ALI_Annotation_Maps.Map;
      Inst_Vector     : Inst_Info_Vectors.Vector;
      BDD_Vector      : BDD.BDD_Vectors.Vector;
      SCO_Vector      : SCO_Vectors.Vector;
   end record;

   procedure Checkpoint_Load_Merge_Unit
     (CLS        : access Checkpoint_Load_State;
      CP_CU      : CU_Info;
      Real_CU_Id : CU_Id);
   --  Load CU from checkpoint that corresponds to a current unit of interest
   --  whose ID is Real_CU_Id.

   procedure Checkpoint_Load_New_Unit
     (CLS        : access Checkpoint_Load_State;
      CP_SCOV   : CP_SCO_Vectors;
      CP_CU     : in out CU_Info;
      CP_CU_Id  : CU_Id;
      New_CU_Id : out CU_Id);
   --  Load CU from checkpoint that does not correspond to a current unit of
   --  interest. The newly assigned CU_Id is returned in New_CU_Id.

   procedure Checkpoint_Load_Unit
     (CLS        : access Checkpoint_Load_State;
      CP_SCOV   : CP_SCO_Vectors;
      CP_CU     : in out CU_Info;
      CP_CU_Id  : CU_Id;
      New_CU_Id : out CU_Id);
   --  Process one compilation unit from a checkpoint.
   --  CP_CU_Id is the CU_Id in the checkpoint.
   --  New_CU_Id is the corresponding CU_Id in the current context, and is
   --  either an already existing CU_Id (if the unit was already known),
   --  or a newly assigned one (if not).

   ---------
   -- BDD --
   ---------

   package body BDD is

      package Arcs_Stacks is
        new Ada.Containers.Vectors
          (Index_Type   => Natural,
           Element_Type => Arcs);

      Arcs_Stack : Arcs_Stacks.Vector;

      procedure Check_Reachability (BDD : in out BDD_Type);
      --  Look for diamonds in BDD (conditions that can be reached through more
      --  than one path from the root condition). MC/DC coverage is equivalent
      --  to object branch coverage if, and only if, there is no diamond.
      --  Also determine whether each outcome is reachable.

      procedure Set_Operand
        (Operator : SCO_Id;
         Position : Operand_Position;
         Operand  : SCO_Id);
      --  Set the operand slot indicated by Position in Operator to Operand

      --------------
      -- Allocate --
      --------------

      procedure Allocate
        (BDD     : in out BDD_Type;
         Node    : BDD_Node;
         Node_Id : out BDD_Node_Id)
      is
      begin
         BDD_Vector.Append (Node);
         Node_Id := BDD_Vector.Last_Index;

         pragma Assert ((BDD.First_Node = No_BDD_Node_Id)
                           = (BDD.Last_Node = No_BDD_Node_Id));
         if BDD.First_Node = No_BDD_Node_Id then
            BDD.First_Node := Node_Id;
            BDD.Last_Node  := Node_Id;
         else
            pragma Assert (Node_Id = BDD.Last_Node + 1);
            BDD.Last_Node := Node_Id;
         end if;
      end Allocate;

      ----------
      -- Read --
      ----------

      procedure Read
        (S : access Root_Stream_Type'Class;
         V : out BDD_Type)
      is
      begin
         SCO_Id'Read       (S, V.Decision);
         BDD_Node_Id'Read  (S, V.Root_Condition);
         BDD_Node_Id'Read  (S, V.First_Node);
         BDD_Node_Id'Read  (S, V.Last_Node);
         BDD_Node_Id'Read  (S, V.Diamond_Base);
         Reachability'Read (S, V.Reachable_Outcomes);
      end Read;

      procedure Read
        (S : access Root_Stream_Type'Class;
         V : out BDD_Node)
      is
         New_BDDN : BDD_Node (BDD_Node_Kind'Input (S));
         pragma Warnings (Off, New_BDDN);

      begin
         --  Set discriminant

         V := New_BDDN;

         case V.Kind is
            when Outcome =>
               Boolean'Read (S, V.Decision_Outcome);

            when Condition =>
               BDD_Node_Id'Read  (S, V.Parent);
               Boolean'Read      (S, V.Parent_Value);
               SCO_Id'Read       (S, V.C_SCO);
               Destinations'Read (S, V.Dests);

            when Jump =>
               BDD_Node_Id'Read (S, V.Dest);
         end case;
      end Read;

      -----------
      -- Write --
      -----------

      procedure Write
        (S : access Root_Stream_Type'Class;
         V : BDD_Type)
      is
      begin
         SCO_Id'Write       (S, V.Decision);
         BDD_Node_Id'Write  (S, V.Root_Condition);
         BDD_Node_Id'Write  (S, V.First_Node);
         BDD_Node_Id'Write  (S, V.Last_Node);
         BDD_Node_Id'Write  (S, V.Diamond_Base);
         Reachability'Write (S, V.Reachable_Outcomes);
      end Write;

      procedure Write
        (S : access Root_Stream_Type'Class;
         V : BDD_Node)
      is
      begin
         BDD_Node_Kind'Write (S, V.Kind);

         case V.Kind is
            when Outcome =>
               Boolean'Write (S, V.Decision_Outcome);

            when Condition =>
               BDD_Node_Id'Write  (S, V.Parent);
               Boolean'Write      (S, V.Parent_Value);
               SCO_Id'Write       (S, V.C_SCO);
               Destinations'Write (S, V.Dests);

            when Jump =>
               BDD_Node_Id'Write (S, V.Dest);
         end case;
      end Write;

      procedure Check_Reachability (BDD : in out BDD_Type) is
         Visited : array (BDD_Node_Id range BDD.First_Node .. BDD.Last_Node)
                     of Boolean := (others => False);

         procedure Visit
           (Node_Id      : BDD_Node_Id;
            Origin_Id    : BDD_Node_Id;
            Origin_Value : Boolean);
         --  Visit one node. If it was already seen, note presence of a diamond

         -----------
         -- Visit --
         -----------

         procedure Visit
           (Node_Id      : BDD_Node_Id;
            Origin_Id    : BDD_Node_Id;
            Origin_Value : Boolean)
         is
            Parent_Id : BDD_Node_Id := Origin_Id;
            C_Value   : Tristate;

            procedure Visit_One (Node : in out BDD_Node);
            --  Set Node's parent to Parent_Id. Set C_Value to Node's condition
            --  value. Also markes outcome as reachable if Node is an outcome.

            ---------------
            -- Visit_One --
            ---------------

            procedure Visit_One (Node : in out BDD_Node) is
            begin
               case Node.Kind is
                  when Condition =>
                     C_Value           := Value (Node.C_SCO);
                     Node.Parent       := Parent_Id;
                     Node.Parent_Value := Origin_Value;

                  when Outcome =>
                     BDD.Reachable_Outcomes (Node.Decision_Outcome) := True;

                  when others =>
                     raise Program_Error;
               end case;
            end Visit_One;

            Node : BDD_Node renames BDD_Vector.Element (Node_Id);

         --  Start of processing for Visit

         begin
            --  Nothing to do if a diamond has already been identified and
            --  both outcomes have been found to be reachable

            if BDD.Diamond_Base /= No_BDD_Node_Id
                 and then
               BDD.Reachable_Outcomes = (True, True)
            then
               return;
            end if;

            --  Record first condition that is reachable through multiple paths
            --  ("diamond base").

            if Node.Kind = Condition
                 and then
               Visited (Node_Id)
                 and then
               BDD.Diamond_Base = No_BDD_Node_Id
            then
               BDD.Diamond_Base := Node_Id;
               Parent_Id := No_BDD_Node_Id;
            end if;

            Visited (Node_Id) := True;
            BDD_Vector.Update_Element (Node_Id, Visit_One'Access);

            if Node.Kind = Condition then
               for J in Boolean'Range loop
                  --  Visit destination J if C_Value is Unknown or J

                  if C_Value /= To_Tristate (not J) then
                     Visit
                       (Node.Dests (J),
                        Origin_Id    => Node_Id,
                        Origin_Value => J);
                  end if;
               end loop;
            end if;
         end Visit;

      --  Start of processing for Check_Reachability

      begin
         Visit
           (BDD.Root_Condition,
            Origin_Id    => No_BDD_Node_Id,
            Origin_Value => False);
      end Check_Reachability;

      ---------------
      -- Completed --
      ---------------

      procedure Completed (BDD : in out BDD_Type) is
         use BDD_Vectors;

         procedure Patch_Jumps (Node : in out BDD_Node);
         --  Replace all destinations of Node that denote Jump nodes with
         --  the jump destination.

         -----------------
         -- Patch_Jumps --
         -----------------

         procedure Patch_Jumps (Node : in out BDD_Node) is
            procedure Patch_Jump (Dest : in out BDD_Node_Id);
            --  If Dest denotes a Jump node, replace it with its destination

            ----------------
            -- Patch_Jump --
            ----------------

            procedure Patch_Jump (Dest : in out BDD_Node_Id) is
               Dest_Node : constant BDD_Node := BDD_Vector.Element (Dest);
            begin
               if Dest_Node.Kind = Jump then
                  Dest := Dest_Node.Dest;
               end if;
               pragma Assert (BDD_Vector.Element (Dest).Kind /= Jump);
            end Patch_Jump;

         --  Start of processing for Patch_Jumps

         begin
            case Node.Kind is
               when Jump =>
                  Patch_Jump (Node.Dest);

               when Condition =>
                  Patch_Jump (Node.Dests (False));
                  Patch_Jump (Node.Dests (True));

               when others =>
                  null;
            end case;
         end Patch_Jumps;

         use type Ada.Containers.Count_Type;

      --  Start of processing for Completed

      begin
         --  Check that all arcs have been consumed

         pragma Assert (Arcs_Stack.Length = 0);

         --  Check that the root condition has been set

         pragma Assert (BDD.Root_Condition /= No_BDD_Node_Id);

         --  Iterate backwards on BDD nodes, replacing references to jump nodes
         --  with references to their destination.

         for J in reverse BDD.First_Node .. BDD.Last_Node loop
            BDD_Vector.Update_Element (J, Patch_Jumps'Access);
         end loop;

         --  Look for diamonds in BDD

         Check_Reachability (BDD);

         if Verbose then
            Dump_BDD (BDD);
         end if;
      end Completed;

      --------------
      -- Dump_BDD --
      --------------

      procedure Dump_BDD (BDD : BDD_Type) is
         procedure Dump_Condition (N : BDD_Node_Id);
         --  Display one condition

         --------------------
         -- Dump_Condition --
         --------------------

         procedure Dump_Condition (N : BDD_Node_Id) is
            use Ada.Strings;

            Node : BDD_Node renames BDD_Vector.Element (N);
            Next_Condition : BDD_Node_Id := N + 1;

            procedure Put_Dest (Origin : Boolean; Dest : BDD_Node_Id);
            --  Dump one destination

            --------------
            -- Put_Dest --
            --------------

            procedure Put_Dest (Origin : Boolean; Dest : BDD_Node_Id) is
               Dest_Node : BDD_Node renames BDD_Vector.Element (Dest);
            begin
               if Origin then
                  Put ("    if TRUE then ");
               else
                  Put ("            else ");
               end if;

               case Dest_Node.Kind is
                  when Outcome =>
                     Put_Line ("return " & Dest_Node.Decision_Outcome'Img);

                  when Condition =>
                     if Dest = Next_Condition then
                        Put_Line ("fallthrough");
                     else
                        Put_Line ("goto" & Dest'Img);
                     end if;

                  when others =>
                     raise Program_Error with "malformed BDD";
               end case;
            end Put_Dest;

         --  Start of processing for Dump_Condition

         begin
            pragma Assert (Node.Kind = Condition);

            while Next_Condition <= BDD_Vector.Last_Index
              and then BDD_Vector.Element (Next_Condition).Kind /= Condition
            loop
               Next_Condition := Next_Condition + 1;
            end loop;

            Put ("@" & Trim (N'Img, Side => Both)
                 & ": test " & Image (Node.C_SCO));

            case SCO_Vector (Node.C_SCO).Value is
               when False =>
                  --  Static known False
                  Put_Line (" (always False)");

               when True =>
                  --  Static known True
                  Put_Line (" (always True)");

               when Unknown =>
                  --  Real runtime test
                  New_Line;
            end case;

            Put_Dest (True,  Node.Dests (True));
            Put_Dest (False, Node.Dests (False));

            if Next_Condition <= BDD.Last_Node then
               New_Line;
               Dump_Condition (Next_Condition);
            end if;
         end Dump_Condition;

      --  Start of processing for Dump_BDD

      begin
         Put_Line ("----- BDD for decision " & Image (BDD.Decision));
         Put_Line ("--- Root condition:" & BDD.Root_Condition'Img);
         if BDD.Diamond_Base /= No_BDD_Node_Id then
            Report
              (First_Sloc (BDD.Decision),
               "BDD node" & BDD.Diamond_Base'Img
               & " reachable through multiple paths",
               Kind => Notice);
            Report ("OBC does not imply MC/DC coverage", Kind => Notice);
         end if;
         Dump_Condition (BDD.Root_Condition);
         New_Line;
      end Dump_BDD;

      ------------
      -- Create --
      ------------

      function Create (Decision : SCO_Id) return BDD_Type is
         Exit_False_Id, Exit_True_Id : BDD_Node_Id;
      begin
         return BDD : BDD_Type do
            BDD.Decision := Decision;

            Allocate
              (BDD, BDD_Node'(Kind => Outcome, Decision_Outcome => False),
               Exit_False_Id);
            Allocate
              (BDD, BDD_Node'(Kind => Outcome, Decision_Outcome => True),
               Exit_True_Id);

            Push
              (((False => Exit_False_Id,
                 True  => Exit_True_Id),
               Origin => No_BDD_Node_Id,
               O_SCO  => Decision,
               O_Pos  => Right));
         end return;
      end Create;

      ----------------------
      -- Process_And_Then --
      ----------------------

      procedure Process_And_Then (O_SCO : SCO_Id; BDD : in out BDD_Type) is
         A : constant Arcs := Pop;
         L : BDD_Node_Id;
      begin
         Set_Operand
           (Operator => A.O_SCO,
            Position => A.O_Pos,
            Operand  => O_SCO);

         Allocate (BDD, BDD_Node'(Kind => Jump,
                                  Dest => No_BDD_Node_Id), L);

         --  Arcs for right operand: subtree is reached through label L if
         --  left operand is True.

         Push
           (((False => A.Dests (False),
              True  => A.Dests (True)),
            Origin => L,
            O_SCO  => O_SCO,
            O_Pos  => Right));

         --  Arcs for left operand

         Push
           (((False => A.Dests (False),
              True  => L),
            Origin => A.Origin,
            O_SCO  => O_SCO,
            O_Pos  => Left));
      end Process_And_Then;

      -----------------
      -- Process_Not --
      -----------------

      procedure Process_Not (O_SCO : SCO_Id; BDD : BDD_Type) is
         pragma Unreferenced (BDD);

         A : constant Arcs := Pop;
      begin
         Set_Operand
           (Operator => A.O_SCO,
            Position => A.O_Pos,
            Operand  => O_SCO);

         --  Swap destinations of top arcs

         Push
           (((False => A.Dests (True),
              True  => A.Dests (False)),
            Origin => A.Origin,
            O_SCO  => O_SCO,
            O_Pos  => Right));
      end Process_Not;

      ---------------------
      -- Process_Or_Else --
      ---------------------

      procedure Process_Or_Else (O_SCO : SCO_Id; BDD : in out BDD_Type) is
         A : constant Arcs := Pop;
         L : BDD_Node_Id;
      begin
         Set_Operand
           (Operator => A.O_SCO,
            Position => A.O_Pos,
            Operand  => O_SCO);
         Allocate (BDD, BDD_Node'(Kind => Jump, Dest => No_BDD_Node_Id), L);

         --  Arcs for right operand: subtree is reached through label L if
         --  left operand is False.

         Push
           (((False => A.Dests (False),
              True  => A.Dests (True)),
            Origin => L,
            O_SCO  => O_SCO,
            O_Pos  => Right));

         --  Arcs for left operand

         Push
           (((False => L,
              True  => A.Dests (True)),
            Origin => A.Origin,
            O_SCO  => O_SCO,
            O_Pos  => Left));
      end Process_Or_Else;

      -----------------------
      -- Process_Condition --
      -----------------------

      procedure Process_Condition
        (BDD          : in out BDD_Type;
         Condition_Id : SCO_Id)
      is
         A : constant Arcs := Pop;
         N : BDD_Node_Id;

         procedure Update_Condition (SCOD : in out SCO_Descriptor);
         --  Set associated node of (Condition) SCOD to N

         procedure Set_Dest (Origin_Node : in out BDD_Node);
         --  Set destination of Origin_Node to N

         --------------
         -- Set_Dest --
         --------------

         procedure Set_Dest (Origin_Node : in out BDD_Node) is
         begin
            Origin_Node.Dest := N;
         end Set_Dest;

         ----------------------
         -- Update_Condition --
         ----------------------

         procedure Update_Condition (SCOD : in out SCO_Descriptor) is
         begin
            SCOD.BDD_Node := N;
            Set_Operand
              (Operator => A.O_SCO,
               Position => A.O_Pos,
               Operand  => Condition_Id);
         end Update_Condition;

      --  Start of processing for Process_Condition

      begin
         Allocate
           (BDD, (Kind         => Condition,
                  C_SCO        => Condition_Id,
                  Dests        => A.Dests,
                  Parent       => No_BDD_Node_Id,
                  Parent_Value => False), N);

         if A.Origin /= No_BDD_Node_Id then
            BDD_Vector.Update_Element (A.Origin, Set_Dest'Access);

         else
            pragma Assert (BDD.Root_Condition = No_BDD_Node_Id);
            BDD.Root_Condition := N;
         end if;

         SCO_Vector.Update_Element (Condition_Id, Update_Condition'Access);
      end Process_Condition;

      ---------
      -- Pop --
      ---------

      function Pop return Arcs is
      begin
         return Top : constant Arcs := Arcs_Stack.Last_Element do
            Arcs_Stack.Delete_Last;
         end return;
      end Pop;

      ----------
      -- Push --
      ----------

      procedure Push (A : Arcs) is
      begin
         Arcs_Stack.Append (A);
      end Push;

      -----------------
      -- Set_Operand --
      -----------------

      procedure Set_Operand
        (Operator : SCO_Id;
         Position : Operand_Position;
         Operand  : SCO_Id)
      is
         procedure Set_Operand (SCOD : in out SCO_Descriptor);
         --  Set operand in parent (operator/decision) SCOD

         procedure Set_Parent (SCOD : in out SCO_Descriptor);
         --  Set parent in child (operator/condition) SCOD

         -----------------
         -- Set_Operand --
         -----------------

         procedure Set_Operand (SCOD : in out SCO_Descriptor) is
         begin
            SCOD.Operands (Position) := Operand;
         end Set_Operand;

         ----------------
         -- Set_Parent --
         ----------------

         procedure Set_Parent (SCOD : in out SCO_Descriptor) is
         begin
            SCOD.Parent := Operator;
         end Set_Parent;

      --  Start of processing for Set_Operand

      begin
         SCO_Vector.Update_Element (Operator, Set_Operand'Access);
         SCO_Vector.Update_Element (Operand,  Set_Parent'Access);
      end Set_Operand;

   end BDD;

   -----------------
   -- Add_Address --
   -----------------

   procedure Add_Address (SCO : SCO_Id; Address : Pc_Type) is
      procedure Update (SCOD : in out SCO_Descriptor);
      --  Add Address to SCOD's PC_Set

      ------------
      -- Update --
      ------------

      procedure Update (SCOD : in out SCO_Descriptor) is
      begin
         SCOD.PC_Set.Include (Address);
      end Update;

   --  Start of processing for Add_Address

   begin
      SCO_Vector.Update_Element (SCO, Update'Access);
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
      Result : CU_Bit_Maps;

      procedure Q (CUI : CU_Info);

      -------
      -- Q --
      -------

      procedure Q (CUI : CU_Info) is
      begin
         Result := CUI.Bit_Maps;
      end Q;

   --  Start of processing for Bit_Maps

   begin
      CU_Vector.Query_Element (CU, Q'Access);
      return Result;
   end Bit_Maps;

   --------------------------------
   -- Checkpoint_Load_Merge_Unit --
   --------------------------------

   procedure Checkpoint_Load_Merge_Unit
     (CLS        : access Checkpoint_Load_State;
      CP_CU      : CU_Info;
      Real_CU_Id : CU_Id)
   is
      Real_CU : CU_Info renames CU_Vector.Reference (Real_CU_Id).Element.all;

   begin
      --  Here we already have loaded full SCO information for this CU, so
      --  all we need to do is to populate the tables mapping the SCO and
      --  instance IDs for this unit in the checkpoint to their counterparts
      --  in the current context.

      --  SCOs

      pragma Assert (CP_CU.Last_SCO - CP_CU.First_SCO
                       =
                     Real_CU.Last_SCO - Real_CU.First_SCO);

      for Old_SCO_Id in CP_CU.First_SCO .. CP_CU.Last_SCO loop
         CLS.SCO_Map (Old_SCO_Id) :=
           Old_SCO_Id
             + Real_CU.First_SCO
           - CP_CU.First_SCO;
      end loop;

      --  Instances

      pragma Assert
        (CP_CU.Last_Instance - CP_CU.First_Instance
         =
           Real_CU.Last_Instance - Real_CU.First_Instance);

      for Old_Inst_Id in CP_CU.First_Instance
        .. CP_CU.Last_Instance
      loop
         CLS.Inst_Map (Old_Inst_Id) :=
           Old_Inst_Id
             + Real_CU.First_Instance
           - CP_CU.First_Instance;
      end loop;

      --  Has_Code indication

      Real_CU.Has_Code := Real_CU.Has_Code or CP_CU.Has_Code;
   end Checkpoint_Load_Merge_Unit;

   ------------------------------
   -- Checkpoint_Load_New_Unit --
   ------------------------------

   procedure Checkpoint_Load_New_Unit
     (CLS        : access Checkpoint_Load_State;
      CP_SCOV   : CP_SCO_Vectors;
      CP_CU     : in out CU_Info;
      CP_CU_Id  : CU_Id;
      New_CU_Id : out CU_Id)

   is
      New_First_Instance : Inst_Id;
      New_First_BDD_Node : BDD_Node_Id;
      New_First_SCO      : SCO_Id;

      Cur_Source_File    : Source_File_Index := No_Source_File;
      Last_Line          : Natural := 0;

      procedure Remap_BDD_Node (B : in out BDD_Node_Id);
      --  Remap a BDD node id

      procedure Remap_SCO_Id (S : in out SCO_Id);
      --  Remap a SCO_Id. Note: this assumes possible forward references, and
      --  does not rely on SCO_Map.

      --------------------
      -- Remap_BDD_Node --
      --------------------

      procedure Remap_BDD_Node (B : in out BDD_Node_Id) is
      begin
         if B /= No_BDD_Node_Id then
            B := CLS.BDD_Map (B);
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
              CP_SCOV.Inst_Vector.Element (Old_Inst_Id);

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
            Remap_SFI (CLS.all, New_Inst.Sloc.Source_File);
            Remap_Inst_Id (New_Inst.Enclosing_Instance);
            pragma Assert (New_Inst.Comp_Unit = CP_CU_Id);
            New_Inst.Comp_Unit := New_CU_Id;

            Inst_Vector.Append (New_Inst);
            CLS.Inst_Map (Old_Inst_Id) := Inst_Vector.Last_Index;
         end Remap_Inst;
      end loop;

      --  Remap BDD node ids

      New_First_BDD_Node := BDD.BDD_Vector.Last_Index + 1;
      CLS.BDD_Map :=
        new BDD_Node_Id_Map_Array (CP_SCOV.BDD_Vector.First_Index
                                   .. CP_SCOV.BDD_Vector.Last_Index);
      for Old_BDD_Node_Id in CLS.BDD_Map'Range loop
         declare
            New_BDD_Node : BDD.BDD_Node :=
              CP_SCOV.BDD_Vector.Element (Old_BDD_Node_Id);

            procedure Remap_BDD_Node_Id (S : in out BDD_Node_Id);
            --  Remap a BDD node id

            -----------------------
            -- Remap_BDD_Node_Id --
            -----------------------

            procedure Remap_BDD_Node_Id (S : in out BDD_Node_Id) is
            begin
               if S /= No_BDD_Node_Id then
                  S := New_First_BDD_Node
                    + S
                    - CP_SCOV.BDD_Vector.First_Index;
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

               when BDD.Jump =>
                  Remap_BDD_Node_Id (New_BDD_Node.Dest);

               when others =>
                  null;
            end case;

            BDD.BDD_Vector.Append (New_BDD_Node);
            CLS.BDD_Map (Old_BDD_Node_Id) :=
              BDD.BDD_Vector.Last_Index;
         end;
      end loop;

      --  Remap SCO ids

      New_First_SCO := SCO_Vector.Last_Index + 1;
      for Old_SCO_Id in CP_CU.First_SCO .. CP_CU.Last_SCO loop
         declare
            New_SCOD : SCO_Descriptor :=
              CP_SCOV.SCO_Vector.Element (Old_SCO_Id);
         begin
            New_SCOD.Origin := New_CU_Id;

            --  Remap SFIs in all source locations

            Remap_SFI (CLS.all, New_SCOD.Sloc_Range.Source_File);

            --  Preallocate line table entries for previous unit

            if New_SCOD.Sloc_Range.Source_File
              /= Cur_Source_File
            then
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

            case New_SCOD.Kind is
               when Statement =>
                  Remap_SFI (CLS.all, New_SCOD.Dominant_Sloc.Source_File);
                  Remap_SFI (CLS.all, New_SCOD.Handler_Range.Source_File);

                  Remap_SCO_Id (New_SCOD.Dominant);

               when Operator | Decision =>
                  for Op_SCO in New_SCOD.Operands'Range loop
                     Remap_SCO_Id (New_SCOD.Operands (Op_SCO));
                  end loop;

                  if New_SCOD.Kind = Decision then
                     Remap_SFI
                       (CLS.all, New_SCOD.Control_Location.Source_File);

                     --  Decision BDD

                     Remap_SCO_Id (New_SCOD.Decision_BDD.Decision);

                     Remap_BDD_Node
                       (New_SCOD.Decision_BDD.Root_Condition);
                     Remap_BDD_Node
                       (New_SCOD.Decision_BDD.First_Node);
                     Remap_BDD_Node
                       (New_SCOD.Decision_BDD.Last_Node);
                     Remap_BDD_Node
                       (New_SCOD.Decision_BDD.Diamond_Base);
                  end if;

               when Condition =>
                  Remap_BDD_Node (New_SCOD.BDD_Node);
                  Remap_Condition_SCO : declare
                     procedure Remap_C_SCO
                       (BDDN : in out BDD.BDD_Node);
                     --  For a Condition BDD node, remap Condition
                     --  SCO pointer

                     -----------------
                     -- Remap_C_SCO --
                     -----------------

                     procedure Remap_C_SCO
                       (BDDN : in out BDD.BDD_Node)
                     is
                     begin
                        Remap_SCO_Id (BDDN.C_SCO);
                     end Remap_C_SCO;

                     --  Start of processing for Remap_Condition_SCO

                  begin
                     BDD.BDD_Vector.Update_Element
                       (New_SCOD.BDD_Node, Remap_C_SCO'Access);
                  end Remap_Condition_SCO;

                  New_SCOD.PC_Set.Clear;

            end case;

            --  Append new SCOD and record mapping

            SCO_Vector.Append (New_SCOD);
            CLS.SCO_Map (Old_SCO_Id) := SCO_Vector.Last_Index;
            if Verbose then
               Put_Line
                 ("Loaded from checkpoint: "
                  & Image (SCO_Vector.Last_Index));
            end if;
         end;
      end loop;

      --  Remap SCO_Ids in source trace bit maps

      if CP_CU.Provider = Instrumenter then
         for S_SCO of CP_CU.Bit_Maps.Statement_Bits.all loop
            Remap_SCO_Id (S_SCO);
         end loop;

         for D_Outcome of CP_CU.Bit_Maps.Decision_Bits.all loop
            Remap_SCO_Id (D_Outcome.D_SCO);
         end loop;
      end if;

      --  Preallocate line table entries for last file

      Prealloc_Lines (Cur_Source_File, Last_Line);

      --  Link new SCOs to source line tables

      for SCO in New_First_SCO .. SCO_Vector.Last_Index loop
         declare
            SCOD : SCO_Descriptor renames SCO_Vector.Element (SCO);
         begin
            if SCOD.Kind in Statement | Decision then
               Add_SCO_To_Lines (SCO, SCOD);
            end if;
         end;
      end loop;

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
   end Checkpoint_Load_New_Unit;

   --------------------------
   -- Checkpoint_Load_Unit --
   --------------------------

   procedure Checkpoint_Load_Unit
     (CLS        : access Checkpoint_Load_State;
      CP_SCOV   : CP_SCO_Vectors;
      CP_CU     : in out CU_Info;
      CP_CU_Id  : CU_Id;
      New_CU_Id : out CU_Id)
   is
   begin
      --  Remap source file indices

      Remap_SFI (CLS.all, CP_CU.Origin);
      Remap_SFI (CLS.all, CP_CU.Main_Source);
      for Dep_SFI of CP_CU.Deps loop

         --  Units of interest can depend on units outside of the
         --  scope of code coverage analysis. Keeping track of these
         --  introduces clashes between stubbed units and the real
         --  one, so they are excluded from checkpoints. Hence, allow
         --  them to be missing here.

         Remap_SFI (CLS.all, Dep_SFI, Require_Valid_File => False);
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
            CP_SCOV,
            CP_CU,
            CP_CU_Id  => CP_CU_Id,
            New_CU_Id => New_CU_Id);

      --  Case 2: CU already loaded from LI info. Record mapping of checkpoint
      --  identifiers (SCOs and instances).

      elsif CP_CU.Fingerprint
        = CU_Vector.Element (New_CU_Id).Fingerprint
      then
         Checkpoint_Load_Merge_Unit
           (CLS,
            CP_CU      => CP_CU,
            Real_CU_Id => New_CU_Id);

      --  Case 3: Checkpointed CU is not consistent with existing info from LI
      --  files. Skip checkpointed unit altogether (and display a warning).

      else
         Warn ("cannot merge coverage information "
               & "from " & To_String (CLS.Filename)
               & " for " & Get_Simple_Name (CP_CU.Origin));
      end if;
   end Checkpoint_Load_Unit;

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
      SCOs_Hash'Read         (S, V.Fingerprint);

      --  Checkpoint version 2 data (instrumentation support)

      if not Version_Less (S, Than => 2) then
         case V.Provider is
            when Compiler =>
               null;
            when Instrumenter =>
               V.Bit_Maps.Statement_Bits :=
                 new Statement_Bit_Map'(Statement_Bit_Map'Input (S));
               V.Bit_Maps.Decision_Bits :=
                 new Decision_Bit_Map'(Decision_Bit_Map'Input (S));
         end case;
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
      --  Discriminant for v2 data

      if not Version_Less (S, Than => 2) then
         SCO_Provider'Write (S, V.Provider);
      end if;

      --  Checkpoint version 1 data

      Source_File_Index'Write (S, V.Origin);
      Source_File_Index'Write (S, V.Main_Source);
      SCO_Id'Write            (S, V.First_SCO);
      SCO_Id'Write            (S, V.Last_SCO);
      Inst_Id'Write           (S, V.First_Instance);
      Inst_Id'Write           (S, V.Last_Instance);
      SFI_Vector'Write        (S, V.Deps);
      Boolean'Write           (S, V.Has_Code);
      SCOs_Hash'Write         (S, V.Fingerprint);

      --  Checkpoint version 2 data (instrumentation support)

      if not Version_Less (S, Than => 2) then
         case V.Provider is
            when Compiler =>
               null;
            when Instrumenter =>
               Statement_Bit_Map'Output
                 (S, V.Bit_Maps.Statement_Bits.all);
               Decision_Bit_Map'Output
                 (S, V.Bit_Maps.Decision_Bits.all);
         end case;
      end if;
   end Write;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load (CLS : access Checkpoint_Load_State) is
      CP_SCOV : CP_SCO_Vectors;
      S : constant access Root_Stream_Type'Class := CLS.all'Access;
   begin
      --  Load data from stream
      --  This part must be kept consistent with Checkpoint_Save

      CU_Info_Vectors.Vector'Read   (S, CP_SCOV.CU_Vector);
      ALI_Annotation_Maps.Map'Read  (S, CP_SCOV.ALI_Annotations);
      Inst_Info_Vectors.Vector'Read (S, CP_SCOV.Inst_Vector);
      BDD.BDD_Vectors.Vector'Read   (S, CP_SCOV.BDD_Vector);
      SCO_Vectors.Vector'Read       (S, CP_SCOV.SCO_Vector);

      --  Allocate mapping tables for SCOs and instance identifiers

      CLS.CU_Map :=
        new CU_Id_Map_Array'(CP_SCOV.CU_Vector.First_Index
                             .. CP_SCOV.CU_Vector.Last_Index => No_CU_Id);
      CLS.SCO_Map :=
        new SCO_Id_Map_Array'(CP_SCOV.SCO_Vector.First_Index
                           .. CP_SCOV.SCO_Vector.Last_Index => No_SCO_Id);
      CLS.Inst_Map :=
        new Inst_Id_Map_Array'(CP_SCOV.Inst_Vector.First_Index
                            .. CP_SCOV.Inst_Vector.Last_Index => No_Inst_Id);

      declare
         Last_Existing_CU_Id : constant CU_Id := CU_Vector.Last_Index;

      begin
         --  Remap and merge into current tables

         for Cur in CP_SCOV.CU_Vector.Iterate loop
            declare
               use CU_Info_Vectors;

               CP_CU_Id : constant CU_Id := To_Index (Cur);
               CP_CU    : CU_Info := Element (Cur);
            begin
               Checkpoint_Load_Unit
                 (CLS,
                  CP_SCOV,
                  CP_CU,
                  CP_CU_Id  => CP_CU_Id,
                  New_CU_Id => CLS.CU_Map (CP_CU_Id));
            end;
         end loop;

         --  Remap annotations

         for Cur in CP_SCOV.ALI_Annotations.Iterate loop
            declare
               use ALI_Annotation_Maps;
               Annotation_Sloc : Source_Location := Key (Cur);
               Annotation      : ALI_Annotation  := Element (Cur);

            begin
               --  If this annotation comes from a compilation unit whose data
               --  is being imported from this checkpoint (i.e. whose CU id
               --  is higher than the last existing one upon entry), add it
               --  now (else it is assumed to be already present in the
               --  ALI_Annotation map).

               pragma Assert (CLS.CU_Map (Annotation.CU) /= No_CU_Id);
               Annotation.CU := CLS.CU_Map (Annotation.CU);
               if Annotation.CU > Last_Existing_CU_Id then
                  Remap_SFI (CLS.all, Annotation_Sloc.Source_File);
                  ALI_Annotations.Insert (Annotation_Sloc, Element (Cur));
               end if;
            end;
         end loop;
      end;
   end Checkpoint_Load;

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save (CSS : access Checkpoint_Save_State) is
      S : constant access Root_Stream_Type'Class := CSS.all'Access;
   begin
      CU_Info_Vectors.Vector'Write   (S, CU_Vector);
      ALI_Annotation_Maps.Map'Write  (S, ALI_Annotations);
      Inst_Info_Vectors.Vector'Write (S, Inst_Vector);
      BDD.BDD_Vectors.Vector'Write   (S, BDD.BDD_Vector);
      SCO_Vectors.Vector'Write       (S, SCO_Vector);
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
   -- Condition --
   ---------------

   function Condition (SCO : SCO_Id; Index : Condition_Index) return SCO_Id is
      use BDD;

      First, Last : BDD_Node_Id;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set First and Last to the first and last BDD node ids of SCOD

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         First := SCOD.Decision_BDD.First_Node;
         Last  := SCOD.Decision_BDD.Last_Node;
      end Q;

      Current_Condition_Index : Any_Condition_Index := No_Condition_Index;

   --  Start of processing for Condition

   begin
      --  Find J'th (0-based) condition in decision by scanning the BDD vector

      SCO_Vector.Query_Element (SCO, Q'Access);
      for J in First .. Last loop
         declare
            BDDN : BDD_Node renames BDD_Vector.Element (J);
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
   -- Decision_Outcome --
   ----------------------

   function Decision_Outcome (SCO : SCO_Id) return Tristate is
      Result : Tristate;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result to indicate whether SCOD is decision-coverable

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
         use BDD;

         Reachable_Outcomes : Reachability
           renames SCOD.Decision_BDD.Reachable_Outcomes;

      begin
         --  If excactly one outcome is reachable, then decision is always True
         --  or always False, else Unknown.

         if Reachable_Outcomes (False) /= Reachable_Outcomes (True) then
            Result := To_Tristate (Reachable_Outcomes (True));
         else
            Result := Unknown;
         end if;
      end Q;

   --  Start of processing for Decision_Outcome

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
   end Decision_Outcome;

   ----------------------
   -- Degraded_Origins --
   ----------------------

   function Degraded_Origins (SCO : SCO_Id) return Boolean is
      Result : Boolean;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result to SCOD.Degraded_Origins

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         Result := SCOD.Degraded_Origins;
      end Q;

   --  Start of processing for Degraded_Origins

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
   end Degraded_Origins;

   --------------
   -- Dominant --
   --------------

   procedure Dominant
     (SCO     : SCO_Id;
      Dom_SCO : out SCO_Id;
      Dom_Val : out Boolean)
   is
      procedure Q (SCOD : SCO_Descriptor);
      --  Set Dom_SCO and Dom_Val

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         Dom_SCO := SCOD.Dominant;

         if Dom_SCO /= No_SCO_Id and then Kind (Dom_SCO) = Decision then
            Dom_Val := To_Boolean (SCOD.Dominant_Value);
         else
            Dom_Val := False;
         end if;
      end Q;

   --  Start of processing for Dominant

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
   end Dominant;

   -------------------
   -- Dump_Decision --
   -------------------

   procedure Dump_Decision (SCO : SCO_Id) is

      procedure Visit (Op_SCO : SCO_Id);
      --  Recursively visit Op_SCO and display expression

      -----------
      -- Visit --
      -----------

      procedure Visit (Op_SCO : SCO_Id) is
         Binary : Boolean;
      begin
         case Kind (Op_SCO) is
            when Condition =>
               Put ('C' & Img (Integer (Index (Op_SCO))));

            when Decision | Operator =>
               if Kind (Op_SCO) = Operator then
                  Put ('(');
               end if;

               Binary := Kind (Op_SCO) = Operator
                           and then Op_Kind (Op_SCO) /= Op_Not;

               for J in Operand_Position'Range loop
                  declare
                     Opnd_SCO : constant SCO_Id := Operand (Op_SCO, J);
                  begin
                     if Kind (Op_SCO) = Operator and then J = Right then
                        case Op_Kind (Op_SCO) is
                           when Op_Not      => Put ("not ");
                           when Op_And_Then => Put (" and then ");
                           when Op_Or_Else  => Put (" or else ");
                        end case;
                     end if;

                     if Opnd_SCO = No_SCO_Id then
                        pragma Assert (J = Left and then not Binary);
                        null;
                     else
                        pragma Assert (J = Right or else Binary);
                        Visit (Opnd_SCO);
                     end if;
                  end;
               end loop;

               if Kind (Op_SCO) = Operator then
                  Put (')');
               end if;

            when others =>
               raise Program_Error;
         end case;
      end Visit;

   --  Start of processing for Dump_Decision

   begin
      Put_Line ("Reconstructed expression for " & Image (SCO));
      Visit (SCO);
      New_Line;
   end Dump_Decision;

   ---------------
   -- Enclosing --
   ---------------

   function Enclosing (What : SCO_Kind; SCO : SCO_Id) return SCO_Id is
      Cur_SCO : SCO_Id := SCO;
      --  Current SCO

      Kind  : SCO_Kind;
      P_SCO : SCO_Id;
      --  Kind and Parent of Cur_SCO

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Kind and P_SCO

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         Kind  := SCOD.Kind;
         P_SCO := SCOD.Parent;
      end Q;

   --  Start of processing for Enclosing

   begin
      loop
         exit when Cur_SCO = No_SCO_Id;
         SCO_Vector.Query_Element (Cur_SCO, Q'Access);
         exit when Kind = What;
         Cur_SCO := P_SCO;
      end loop;
      return Cur_SCO;
   end Enclosing;

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
      Result : Source_Location;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         Result := First_Sloc (SCOD.Sloc_Range);
      end Q;

   --  Start of processing for First_Sloc

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
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
         Prev_SCO   := BDD_Vector.Element (BDDN.Parent).C_SCO;
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
         CUI := CU_Vector.Element (CU);
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
                 (Inst_Vector.Element (Global_Instance_Index).Comp_Unit
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

   -----------------
   -- Has_Diamond --
   -----------------

   function Has_Diamond (SCO : SCO_Id) return Boolean is
      Result : Boolean;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result to indicate whether SCO has conditions reachable through
      --  multiple paths.

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         Result := SCOD.Decision_BDD.Diamond_Base /= No_BDD_Node_Id;
      end Q;

   --  Start of processing for Has_Diamond

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
   end Has_Diamond;

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
               Result : Tristate;

               procedure Q (SCOD : SCO_Descriptor);
               --  Set Result according to sloc range of SCOD

               -------
               -- Q --
               -------

               procedure Q (SCOD : SCO_Descriptor) is
               begin
                  if Sloc_End < First_Sloc (SCOD.Sloc_Range) then
                     --  Negative match, and no chance to have a positive match
                     --  in the next SCOs: they all have a higher First_Sloc.

                     Result := False;

                  elsif Last_Sloc (SCOD.Sloc_Range) < Sloc_Begin then
                     --  Negative match, but we may reach a positive match in
                     --  the next SCO. Continue.

                     Result := Unknown;

                  else
                     --  The two possible negative matches have been dealt with
                     --  earlier. We have a positive match.

                     Result := True;

                  end if;
               end Q;

            begin
               SCO_Vector.Query_Element (Element (Position), Q'Access);
               if Result /= Unknown then
                  return To_Boolean (Result);
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

      Result : Boolean;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result from the given SCO descriptor, accessed in-place
      --  from SCO_Vector.

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         --  We ignore remnants of internal processing phases which leave
         --  dummy SCO entries in SCO_Vector, as well as SCOs for pragmas
         --  known not to generate code, as they are not really statements in
         --  the Ada sense and need not be assessed for coverage.

         Result :=
           --  Not a real SCO any more ?

           SCOD.Origin = No_CU_Id

           or else

           --  Pragma not generating code ?

           (SCOD.S_Kind = Pragma_Statement
              and then not Pragma_Might_Generate_Code (SCOD.Pragma_Name))

           or else

           --  Disabled pragma ?

           SCOD.S_Kind = Disabled_Pragma_Statement;

      end Q;

   --  Start of processing for Ignore_SCO

   begin
      pragma Assert (Kind (SCO) = Statement);

      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
   end Ignore_SCO;

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

   -----------
   -- Index --
   -----------

   function Index (SCO : SCO_Id) return Condition_Index is
      Result : Condition_Index;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result to SCOD.Index

      procedure Q (SCOD : SCO_Descriptor) is
         pragma Assert (SCOD.Kind = Condition);
      begin
         Result := SCOD.Index;
      end Q;

   --  Start of processing for Index

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
   end Index;

   ------------------
   -- Instance_Loc --
   ------------------

   function Instance_Loc (Inst_Index : Inst_Id) return String
   is
      II : Inst_Info renames Inst_Vector.Element (Inst_Index);
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
      SCOD  : SCO_Descriptor renames SCO_Vector (SCO);
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

   -------------------
   -- Is_Expression --
   -------------------

   function Is_Expression (SCO : SCO_Id) return Boolean is
      D_Kind : Decision_Kind;
      Result : Boolean;

      procedure Get_D_Kind (SCOD : SCO_Descriptor);
      --  Set D_Kind to SCOD.D_Kind

      procedure Check_Pragma_Assert_PPC (S_SCOD : SCO_Descriptor);
      --  Set Result to True if SCOD is a pragma Assert/Check/Pre/Post

      ----------------
      -- Get_D_Kind --
      ----------------

      procedure Get_D_Kind (SCOD : SCO_Descriptor) is
      begin
         D_Kind := SCOD.D_Kind;
      end Get_D_Kind;

      -----------------------------
      -- Check_Pragma_Assert_PPC --
      -----------------------------

      procedure Check_Pragma_Assert_PPC (S_SCOD : SCO_Descriptor) is
      begin
         Result :=  (S_SCOD.S_Kind = Pragma_Statement
                       or else
                     S_SCOD.S_Kind = Disabled_Pragma_Statement)
           and then (S_SCOD.Pragma_Name = Pragma_Assert
                       or else
                     S_SCOD.Pragma_Name = Pragma_Check
                       or else
                     S_SCOD.Pragma_Name = Pragma_Precondition
                       or else
                     S_SCOD.Pragma_Name = Pragma_Postcondition);
      end Check_Pragma_Assert_PPC;

   --  Start of processing for Is_Expression

   begin
      pragma Assert (Kind (SCO) = Decision);

      --  Check for expression outside of control structure

      SCO_Vector.Query_Element (SCO, Get_D_Kind'Access);
      if D_Kind = Expression then
         return True;
      end if;

      --  Check for pragma Assert/Check/Pre/Post

      if D_Kind /= Pragma_Decision then
         return False;
      end if;

      declare
         S_SCO : constant SCO_Id := Enclosing_Statement (SCO);
      begin
         if S_SCO = No_SCO_Id then
            return False;
         end if;
         SCO_Vector.Query_Element (S_SCO, Check_Pragma_Assert_PPC'Access);
         return Result;
      end;
   end Is_Expression;

   ----------------------------------
   -- Is_Pragma_Pre_Post_Condition --
   ----------------------------------

   function Is_Pragma_Pre_Post_Condition (SCO : SCO_Id) return Boolean is
      Result : Boolean;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result to True if SCOD is for a pragma Pre/Post-condition

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
         pragma Assert (SCOD.Kind = Statement);
      begin
         Result := (SCOD.S_Kind = Pragma_Statement
                      or else
                    SCOD.S_Kind = Disabled_Pragma_Statement)
                   and then (SCOD.Pragma_Name = Pragma_Precondition
                               or else
                             SCOD.Pragma_Name = Pragma_Postcondition);
      end Q;

   --  Start of processing for Is_Pragma_Pre_Post_Condition

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
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

   function Kind (SCO : SCO_Id) return SCO_Kind is
      Result : SCO_Kind;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result to SCOD.Kind

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         Result := SCOD.Kind;
      end Q;

   --  Start of processing for Kind

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
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

   ---------------
   -- Last_Sloc --
   ---------------

   function Last_Sloc (SCO : SCO_Id) return Source_Location is
      Result : Source_Location;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result to Last_Sloc (SCOD.Sloc_Range)

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         Result := Last_Sloc (SCOD.Sloc_Range);
      end Q;

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
   end Last_Sloc;

   -----------------
   -- Allocate_CU --
   -----------------

   function Allocate_CU
     (Provider : SCO_Provider;
      Origin   : Source_File_Index := No_Source_File) return CU_Id
   is
      New_CU_Info : CU_Info (Provider);

   begin
      New_CU_Info.Origin := Origin;
      CU_Vector.Append (New_CU_Info);
      return CU_Vector.Last_Index;
   end Allocate_CU;

   ---------------
   -- Load_SCOs --
   ---------------

   procedure Load_SCOs
     (ALI_Filename         : String;
      Ignored_Source_Files : access GNAT.Regexp.Regexp)
   is
      Units, Deps : SFI_Vector;
      --  Units and dependencies of this compilation

      CU_Index  : constant CU_Id := Allocate_CU (Provider => Compiler);
      --  Compilation unit for this ALI

      ALI_Index : constant Source_File_Index :=
        Load_ALI (ALI_Filename, CU_Index, Ignored_Source_Files,
                  Units, Deps, With_SCOs => True);
      --  Load ALI file and update the last SCO and instance indices

      Main_Source  : Source_File_Index;

      procedure Set_Origin (CUI : in out CU_Info);
      --  Set CUI's origin to ALI_Index (in the compiler-based scenario case,
      --  the origin of SCO information is the ALI file).

      procedure Set_Origin (CUI : in out CU_Info) is
      begin
         CUI.Origin := ALI_Index;
      end Set_Origin;

   --  Start of porocessing for Load_SCOs

   begin
      if ALI_Index = No_Source_File then
         return;
      end if;
      CU_Vector.Update_Element (CU_Index, Set_Origin'Access);

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

      Get_File (Main_Source).LI := ALI_Index;
      Get_File (ALI_Index).Main_Source := Main_Source;

      Process_Low_Level_SCOs (CU_Index, Main_Source, Deps);
   end Load_SCOs;

   ----------------------------
   -- Process_Low_Level_SCOs --
   ----------------------------

   procedure Process_Low_Level_SCOs
     (CU_Index     : CU_Id;
      Main_Source  : Source_File_Index;
      Deps         : SFI_Vector := SFI_Vectors.Empty_Vector;
      LL_Unit_Bits : LL_Unit_Bit_Maps := No_LL_Unit_Bit_Maps)
   is
      use SCOs;

      --  Record entry high water mark in high level SCO tables

      Last_Instance_Upon_Entry : constant Inst_Id := Inst_Vector.Last_Index;
      Last_SCO_Upon_Entry      : constant SCO_Id  := SCO_Vector.Last_Index;

      Cur_Source_File        : Source_File_Index := No_Source_File;
      Cur_SCO_Unit           : SCO_Unit_Index;
      Last_Entry_In_Cur_Unit : Int;
      Last_Entry_Last_Line   : Natural := 0;
      --  Line number of high bound of sloc range of last processed entry

      Skip_Current_File : Boolean := False;
      --  Whether the SCOs referring to the current source file should be
      --  ignored.

      Dom_SCO  : SCO_Id          := No_SCO_Id;
      Dom_Sloc : Source_Location := No_Location;
      Dom_Val  : Tristate        := Unknown;
      Current_Handler_Range : Source_Location_Range := No_Range;
      --  Dominant information for basic block chaining

      Current_Decision : SCO_Id := No_SCO_Id;
      --  Decision whose conditions are being processed

      Current_Condition_Index : Any_Condition_Index;
      --  Index of current condition within the current decision (0-based, set
      --  to No_Condition_Index, i.e. -1, before the first condition of the
      --  decision is seen).

      Current_BDD : BDD.BDD_Type;
      --  BDD of current decision

      Statement_Bits : Statement_Bit_Map_Access;
      Decision_Bits  : Decision_Bit_Map_Access;

      procedure Fixup_CU (CUI : in out CU_Info);
      --  Update CU entry with information gathered from the tables

      --------------
      -- Fixup_CU --
      --------------

      procedure Fixup_CU (CUI : in out CU_Info) is
      begin
         CUI.Main_Source    := Main_Source;
         CUI.Deps           := Deps;

         CUI.First_SCO      := Last_SCO_Upon_Entry + 1;
         CUI.Last_SCO       := SCO_Vector.Last_Index;

         CUI.First_Instance := Last_Instance_Upon_Entry + 1;
         CUI.Last_Instance  := Inst_Vector.Last_Index;

         case CUI.Provider is
            when Compiler =>
               null;

            when Instrumenter =>
               CUI.Bit_Maps := (Statement_Bits, Decision_Bits);

         end case;

         CUI.Fingerprint    := SCO_Tables_Fingerprint;
      end Fixup_CU;

      Deps_Present : constant Boolean := not Deps.Is_Empty;
      Provider     : constant SCO_Provider :=
                       CU_Vector.Element (CU_Index).Provider;

   --  Start of processing for Process_Low_Level_SCOs

   begin
      --  Make sure we have a CU_Map entry for the main source file, even if no
      --  SCOs are present, as it is used for checkpoint consolidation.

      CU_Map.Insert (Main_Source, CU_Index);

      --  Allocate bitmaps if necessary

      if Provider = Instrumenter then
         Statement_Bits :=
           new Statement_Bit_Map'
             (Bit_Id'First .. LL_Unit_Bits.Last_Statement_Bit => No_SCO_Id);
         Decision_Bits  :=
           new Decision_Bit_Map'
             (Bit_Id'First .. LL_Unit_Bits.Last_Decision_Bit =>
                (No_SCO_Id, False));
      end if;

      --  Walk low-level SCO table for this unit and populate high-level tables

      Cur_SCO_Unit := SCO_Unit_Table.First;
      Last_Entry_In_Cur_Unit := SCOs.SCO_Table.First - 1;
      --  Note, the first entry in the SCO_Unit_Table is unused

      for Cur_SCO_Entry in
        SCOs.SCO_Table.First .. SCOs.SCO_Table.Last
      loop
         if Cur_SCO_Entry > Last_Entry_In_Cur_Unit then
            --  Prealloc line table entries for previous units

            Prealloc_Lines (Cur_Source_File, Last_Entry_Last_Line);

            --  Enter new unit

            Cur_SCO_Unit := Cur_SCO_Unit + 1;
            pragma Assert
              (Cur_SCO_Unit in SCOs.SCO_Unit_Table.First
                            .. SCOs.SCO_Unit_Table.Last);
            declare
               SCOUE : SCO_Unit_Table_Entry
                         renames SCOs.SCO_Unit_Table.Table (Cur_SCO_Unit);
            begin
               pragma Assert (Cur_SCO_Entry in SCOUE.From .. SCOUE.To);
               Last_Entry_In_Cur_Unit := SCOUE.To;
               Skip_Current_File := SCOUE.Dep_Num = Missing_Dep_Num;
               if not Skip_Current_File then
                  if Deps_Present then
                     --  Get source file name from deps table. Note that this
                     --  is a simple name for Ada, but a full path for C.

                     Cur_Source_File := Deps.Element (SCOUE.Dep_Num);

                  else
                     --  For C, GLI files from older compilers did not provide
                     --  a proper deps table.

                     Cur_Source_File := Get_Index_From_Simple_Name
                       (SCOUE.File_Name.all, Source_File);
                  end if;

                  --  We are going to add coverage obligations for this file,
                  --  so mark it as a Source_File in the file table.

                  Consolidate_File_Kind (Cur_Source_File, Source_File);
               end if;
            end;
         end if;

         --  If asked to, ignore this entry

         if Skip_Current_File then
            goto Skip_Entry;
         end if;

         --  Record source file -> compilation unit mapping for non-main
         --  sources. Note: for C files, the same source file may be
         --  encountered several times, hence the use of Include rather
         --  than Insert.

         if Cur_Source_File /= Main_Source then
            CU_Map.Include (Cur_Source_File, CU_Index);
         end if;

         pragma Assert (Cur_Source_File /= No_Source_File);
         Process_Entry : declare
            SCOE : SCOs.SCO_Table_Entry renames
                                     SCOs.SCO_Table.Table (Cur_SCO_Entry);

            function Make_Condition_Value return Tristate;
            --  Map condition value code (t/f/c) in SCOE.C2 to Tristate

            function New_Operator_SCO (Kind : Operator_Kind) return SCO_Id;
            --  Allocate a new SCO for an operator

            procedure Update_Decision_BDD (SCOD : in out SCO_Descriptor);
            --  Set BDD of decision to Current_BDD

            procedure Update_Decision_Sloc (SCOD : in out SCO_Descriptor);
            --  Update the slocs of a decision SCOD from those of the condition
            --  in the current SCOE.

            function Make_Sloc
              (SCO_Source_Loc : SCOs.Source_Location)
               return Local_Source_Location;
            --  Build a Slocs.Source_Location record from the low-level
            --  SCO Sloc info.

            ---------------
            -- Make_Sloc --
            ---------------

            function Make_Sloc
              (SCO_Source_Loc : SCOs.Source_Location)
               return Local_Source_Location
            is
            begin
               if SCO_Source_Loc = SCOs.No_Source_Location then
                  return No_Local_Location;
               end if;

               return
                 (Line   => Natural (SCO_Source_Loc.Line),
                  Column => Natural (SCO_Source_Loc.Col));
            end Make_Sloc;

            From_Sloc : constant Local_Source_Location :=
                          Make_Sloc (SCOE.From);
            To_Sloc   : constant Local_Source_Location :=
                          Make_Sloc (SCOE.To);
            SCO_Range : constant Source_Location_Range :=
                          (Cur_Source_File, (From_Sloc, To_Sloc));

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
               pragma Assert (Current_Decision /= No_SCO_Id);
               SCO_Vector.Append
                 (SCO_Descriptor'(Kind       => Operator,
                                  Origin     => CU_Index,
                                  Sloc_Range => SCO_Range,
                                  Op_Kind    => Kind,
                                  others     => <>));

               return SCO_Vector.Last_Index;
            end New_Operator_SCO;

            -------------------------
            -- Update_Decision_BDD --
            -------------------------

            procedure Update_Decision_BDD (SCOD : in out SCO_Descriptor) is
            begin
               SCOD.Decision_BDD    := Current_BDD;
               SCOD.Last_Cond_Index := Current_Condition_Index;
            end Update_Decision_BDD;

            --------------------------
            -- Update_Decision_Sloc --
            --------------------------

            procedure Update_Decision_Sloc (SCOD : in out SCO_Descriptor) is
            begin
               if SCOD.Sloc_Range.Source_File = No_Source_File then
                  SCOD.Sloc_Range.Source_File := Cur_Source_File;
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

         --  Start of processing for Process_Entry

         begin
            if To_Sloc.Line > Last_Entry_Last_Line then
               Last_Entry_Last_Line := To_Sloc.Line;
            end if;

            case SCOE.C1 is
               when '>' =>
                  --  Dominance marker: processed in conjunction with following
                  --  'S' entry.

                  pragma Assert (Dom_SCO = No_SCO_Id);
                  if SCOE.Last then
                     --  Ignore dominance marker because all S entries in its
                     --  sequence have been suppressed.

                     null;

                  else
                     case SCOE.C2 is
                        when 'S' =>
                           Dom_Sloc := Slocs.To_Sloc
                                         (Cur_Source_File, From_Sloc);
                           Dom_Val  := Unknown;

                        when 'T' | 'F' =>
                           Dom_Sloc := Slocs.To_Sloc
                                         (Cur_Source_File, From_Sloc);
                           Dom_Val  := To_Tristate (SCOE.C2 = 'T');

                        when 'E' =>
                           Current_Handler_Range := SCO_Range;

                        when others =>
                           raise Program_Error;
                     end case;
                  end if;

               when 'S' | 's' =>
                  pragma Assert (Current_Decision = No_SCO_Id);

                  SCO_Vector.Append
                    (SCO_Descriptor'
                       (Kind           => Statement,
                        Origin         => CU_Index,
                        Sloc_Range     => SCO_Range,
                        S_Kind         =>
                           To_Statement_Kind (SCOE.C2),
                        Dominant       => Dom_SCO,
                        Dominant_Sloc  => Dom_Sloc,
                        Dominant_Value => Dom_Val,
                        Handler_Range  => Current_Handler_Range,
                        Pragma_Name    => Case_Insensitive_Get_Pragma_Id
                          (SCOE.Pragma_Aspect_Name),
                        others         => <>));
                  if Statement_Bits /= null then
                     Statement_Bits (LL_Unit_Bits.Statement_Bits.Element
                                     (Cur_SCO_Entry)) := SCO_Vector.Last_Index;
                  end if;

                  Current_Handler_Range := No_Range;
                  Dom_Val  := Unknown;
                  Dom_Sloc := No_Location;
                  if SCOE.Last then
                     Dom_SCO := No_SCO_Id;
                  else
                     Dom_SCO := SCO_Vector.Last_Index;
                  end if;

               when 'E' | 'G' | 'I' | 'P' | 'W' | 'X' | 'A' =>
                  --  Decision

                  pragma Assert (Current_Decision = No_SCO_Id);
                  SCO_Vector.Append
                    (SCO_Descriptor'
                       (Kind                => Decision,
                        Origin              => CU_Index,
                        Control_Location    =>
                           Slocs.To_Sloc
                          (Cur_Source_File, From_Sloc),
                        D_Kind              =>
                           To_Decision_Kind (SCOE.C1),
                        Last_Cond_Index     => 0,
                        Aspect_Name         =>
                           Get_Aspect_Id (SCOE.Pragma_Aspect_Name),
                        others              => <>));
                  Current_BDD := BDD.Create (SCO_Vector.Last_Index);

                  pragma Assert (not SCOE.Last);

                  Current_Decision        := SCO_Vector.Last_Index;
                  Current_Condition_Index := No_Condition_Index;

                  --  Record the SCO/outcome association of both bits
                  --  associated to this decision obligation.

                  if Enabled (Decision) and then Decision_Bits /= null then
                     declare
                        This_Decision_Bits : constant Outcome_Bit_Ids :=
                          LL_Unit_Bits.Decision_Bits.Element (Cur_SCO_Entry);
                     begin
                        for Outcome in Boolean loop
                           Decision_Bits (This_Decision_Bits (Outcome)) :=
                             (Current_Decision, Outcome);
                        end loop;
                     end;
                  end if;

               when ' ' =>
                  --  Condition

                  pragma Assert (Current_Decision /= No_SCO_Id);

                  SCO_Vector.Update_Element
                    (Index   => Current_Decision,
                     Process => Update_Decision_Sloc'Access);

                  Current_Condition_Index := Current_Condition_Index + 1;

                  SCO_Vector.Append
                    (SCO_Descriptor'(Kind       => Condition,
                                     Origin     => CU_Index,
                                     Sloc_Range => SCO_Range,
                                     Value      => Make_Condition_Value,
                                     Index      => Current_Condition_Index,
                                     others     => <>));
                  BDD.Process_Condition (Current_BDD, SCO_Vector.Last_Index);

                  if SCOE.Last then
                     BDD.Completed (Current_BDD);
                     SCO_Vector.Update_Element
                       (Current_BDD.Decision, Update_Decision_BDD'Access);

                     if Verbose then
                        Dump_Decision (Current_Decision);
                     end if;
                     Current_Decision := No_SCO_Id;
                  end if;

               when '!' =>
                  BDD.Process_Not
                    (New_Operator_SCO (Op_Not), Current_BDD);

               when '&' =>
                  BDD.Process_And_Then
                    (New_Operator_SCO (Op_And_Then), Current_BDD);

               when '|' =>
                  BDD.Process_Or_Else
                    (New_Operator_SCO (Op_Or_Else), Current_BDD);

               when 'H' =>
                  --  Chaining indicator: not used yet

                  null;

               when others =>
                  raise Program_Error
                    with "unexpected SCO entry code: " & SCOE.C1;
            end case;
         end Process_Entry;

         <<Skip_Entry>>
      end loop;

      --  Import unit instance table into global table

      for J in SCO_Instance_Table.First .. SCO_Instance_Table.Last loop
         declare
            SIE : SCO_Instance_Table_Entry
                    renames SCO_Instance_Table.Table (J);
         begin
            Inst_Vector.Append
              ((Sloc                =>
                  (Source_File => Deps.Element (SIE.Inst_Dep_Num),
                   L           => (Line   => Natural (SIE.Inst_Loc.Line),
                                   Column => Natural (SIE.Inst_Loc.Col))),
                Enclosing_Instance =>
                  (if SIE.Enclosing_Instance = 0
                   then No_Inst_Id
                   else Last_Instance_Upon_Entry
                      + Inst_Id (SIE.Enclosing_Instance)),
                Comp_Unit          => CU_Vector.Last_Index));
         end;
      end loop;

      --  Prealloc line table entries for last unit

      Prealloc_Lines (Cur_Source_File, Last_Entry_Last_Line);

      --  Build Sloc -> SCO index and set up Parent links

      for SCO in Last_SCO_Upon_Entry + 1 .. SCO_Vector.Last_Index loop
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
                     --  A SCO for a (simple) statement is never nested

                     --  pragma Assert (Enclosing_SCO = No_SCO_Id);
                     --  For now generate explicit diagnostic, ignore nested
                     --  SCO and proceed???

                     if Enclosing_SCO /= No_SCO_Id then
                        if not Equivalent
                          (SCOD, SCO_Vector (Enclosing_SCO))
                        then
                           Report
                             (First,
                              "unexpected SCO nesting in "
                              & Image (Enclosing_SCO)
                              & ", discarding nested SCO");
                        end if;
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

                     --  Reset SCOD to No_SCO_Descriptor, which acts as a
                     --  placeholder to cancel the entry. The corresponding
                     --  SCO is a (disabled) statement with no origin.

                     SCOD := No_SCO_Descriptor;
                  end if;
               end;
            end Process_Descriptor;

         begin
            SCO_Vector.Update_Element (SCO, Process_Descriptor'Access);
         end;
      end loop;

      --  Now that all decisions and statements have been entered in the
      --  sloc -> SCO map, set the Dominant information.

      for SCO in Last_SCO_Upon_Entry + 1 .. SCO_Vector.Last_Index loop
         declare
            procedure Set_Dominant_From_Sloc (SCOD : in out SCO_Descriptor);
            --  Set SCOD.Dominant (if unset) to the innermost SCO containing
            --  SCOD.Dominant_Sloc.

            ------------------
            -- Set_Dominant --
            ------------------

            procedure Set_Dominant_From_Sloc (SCOD : in out SCO_Descriptor) is
               Dom_Sloc_SCO : SCO_Id;
            begin
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

                     Dom_Sloc_SCO :=
                       Sloc_To_SCO_Map
                         (SCOD.Dominant_Sloc.Source_File, Decision)
                       .Element ((SCOD.Dominant_Sloc.L, No_Local_Location));
                     pragma Assert (Kind (Dom_Sloc_SCO) = Decision);
                  end if;

                  SCOD.Dominant := Dom_Sloc_SCO;
               end if;
            end Set_Dominant_From_Sloc;

         begin
            SCO_Vector.Update_Element (SCO, Set_Dominant_From_Sloc'Access);
         end;
      end loop;

      --  Finally update entry in CU vector

      CU_Vector.Update_Element (CU_Vector.Last, Fixup_CU'Access);
   end Process_Low_Level_SCOs;

   -------------
   -- Map_Tag --
   -------------

   overriding function Map_Tag
     (TP     : access Instance_Tag_Provider_Type;
      CLS    : Checkpoints.Checkpoint_Load_State;
      CP_Tag : SC_Tag) return SC_Tag
   is
      pragma Unreferenced (TP);
   begin
      --  Remap CP_Tag interpreted as a global instance id

      return SC_Tag (CLS.Inst_Map (Inst_Id (CP_Tag)));
   end Map_Tag;

   -------------------
   -- Next_BDD_Node --
   -------------------

   function Next_BDD_Node
     (SCO   : SCO_Id;
      Value : Boolean) return BDD_Node_Id
   is
      use BDD;

      Result : BDD_Node_Id;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result to the requested next BDD node id

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
         procedure QB (BDDN : BDD_Node);
         --  Set Result to the requested next BDD node id

         --------
         -- QB --
         --------

         procedure QB (BDDN : BDD_Node) is
         begin
            Result := BDDN.Dests (Value);
         end QB;

      --  Start of processing for Q

      begin
         BDD_Vector.Query_Element (SCOD.BDD_Node, QB'Access);
      end Q;

   --  Start of processing for Next_BDD_Node

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
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

   function Operand
     (SCO      : SCO_Id;
      Position : Operand_Position) return SCO_Id
   is
      Result : SCO_Id;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result to SCOD.Operand (Position)

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         Result := SCOD.Operands (Position);
      end Q;

   --  Start of processing for Operand

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
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
              BDD_Vector.Element (Next_BDD_Node (Cond_SCO, Cond_Value));
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
      Result : Tristate;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result to SCOD.Value

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
         pragma Assert (SCOD.Kind = Condition);
      begin
         Result := SCOD.Value;
      end Q;

   --  Start of processing for Value

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
   end Value;

   ------------
   -- Parent --
   ------------

   function Parent (SCO : SCO_Id) return SCO_Id is
      Result : SCO_Id;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result to SCOD.Parent

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         Result := SCOD.Parent;
      end Q;

   --  Start of processing for Parent

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
   end Parent;

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
      use SCO_Vectors;

      procedure Report_Multipath (Cur : Cursor);
      --  If Cur is a Decision with multiple paths, report it

      ----------------------
      -- Report_Multipath --
      ----------------------

      procedure Report_Multipath (Cur : Cursor) is
         use BDD, BDD.BDD_Vectors;

         SCOD : SCO_Descriptor renames Element (Cur);
         DB   : BDD_Node_Id;
      begin
         if SCOD.Kind = Decision then
            DB := SCOD.Decision_BDD.Diamond_Base;
            if DB /= No_BDD_Node_Id then
               Report
                 (First_Sloc (BDD_Vector (DB).C_SCO),
                  "condition is reachable through multiple paths",
                  Kind => Warning);
            end if;
         end if;
      end Report_Multipath;

   --  Start of processing for Report_Multipath_Decisions

   begin
      SCO_Vector.Iterate (Report_Multipath'Access);
   end Report_Multipath_Decisions;

   ------------------------------
   -- Report_SCOs_Without_Code --
   ------------------------------

   procedure Report_SCOs_Without_Code is
      use SCO_Vectors;

      procedure Check_Condition (Cur : Cursor);
      --  Check whether the SCO at Cur is a Condition, and if so, warn if
      --  it has no associated conditional branch instruction.

      ---------------------
      -- Check_Condition --
      ---------------------

      procedure Check_Condition (Cur : Cursor) is
         use Ada.Containers;

         SCOD  : SCO_Descriptor renames Element (Cur);
         D_SCO : SCO_Id;

      begin
         if SCOD.Kind /= Condition then
            return;
         end if;

         --  Report a static analysis error if one condition has no associated
         --  conditional branch, and the enclosing decision is not compile time
         --  known.

         D_SCO := Enclosing_Decision (To_Index (Cur));
         if SCOD.PC_Set.Length = 0
           and then Decision_Outcome (D_SCO) = Unknown
         then
            Report
              (First_Sloc (SCOD.Sloc_Range),
               Msg  => "no conditional branch (in "
                         & Decision_Kind'Image (SCO_Vector (D_SCO).D_Kind)
                         & ")",
               Kind => Diagnostics.Error);

            --  Interesting property: we can never do without a condition using
            --  inference of condition values from BDD position, because that
            --  would require that both outgoing edges from the condition also
            --  are conditions (not outcomes), and that can't happen in a short
            --  circuit expression without a diamond; this would require a BDD
            --  involving the Sel ternary operator:
            --    Sel (A, B, C) = (A and then B) or else (not A and then C)

         end if;
      end Check_Condition;

   --  Start of processing for Report_SCOs_Without_Code

   begin
      SCO_Vector.Iterate (Check_Condition'Access);
   end Report_SCOs_Without_Code;

   -------------------------------
   -- Report_Units_Without_Code --
   -------------------------------

   procedure Report_Units_Without_Code is
      use CU_Info_Vectors;

      procedure Check_Unit (Cur : Cursor);
      --  Warn if the unit at Cur has no associated object code

      ----------------
      -- Check_Unit --
      ----------------

      procedure Check_Unit (Cur : Cursor) is
         CUI : CU_Info renames Element (Cur);
      begin
         if not CUI.Has_Code and then Has_SCOs (CUI) then
            Report
              (Msg  => "no object code for " & Get_Simple_Name (CUI.Origin),
               Kind => Diagnostics.Error);
         end if;
      end Check_Unit;

   --  Start of processing for Report_Units_Without_Code

   begin
      CU_Vector.Iterate  (Check_Unit'Access);
   end Report_Units_Without_Code;

   ------------
   -- S_Kind --
   ------------

   function S_Kind (SCO : SCO_Id) return Any_Statement_Kind is
      Result : Statement_Kind;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result to SCOD.S_Kind

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         Result := SCOD.S_Kind;
      end Q;

   --  Start of processing for S_Kind

   begin
      if SCO = No_SCO_Id then
         return No_Statement;
      end if;

      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
   end S_Kind;

   --------------------------
   -- Set_Degraded_Origins --
   --------------------------

   procedure Set_Degraded_Origins (SCO : SCO_Id; Val : Boolean := True) is

      procedure Set_SCOD_Degraded_Origins (SCOD : in out SCO_Descriptor);
      --  Set SCOD.Degraded_Origins to Val

      -------------------------------
      -- Set_SCOD_Degraded_Origins --
      -------------------------------

      procedure Set_SCOD_Degraded_Origins (SCOD : in out SCO_Descriptor) is
      begin
         SCOD.Degraded_Origins := Val;
      end Set_SCOD_Degraded_Origins;

   --  Start of processing for Set_Degraded_Origins

   begin
      SCO_Vector.Update_Element (SCO, Set_SCOD_Degraded_Origins'Access);
   end Set_Degraded_Origins;

   -----------------------
   -- Set_Unit_Has_Code --
   -----------------------

   procedure Set_Unit_Has_Code (CU : CU_Id) is
      procedure Set_CUI_Has_Code (CUI : in out CU_Info);
      --  Set CUI.Has_Code True

      ----------------------
      -- Set_CUI_Has_Code --
      ----------------------

      procedure Set_CUI_Has_Code (CUI : in out CU_Info) is
      begin
         CUI.Has_Code := True;
      end Set_CUI_Has_Code;

   --  Start of processing for Set_Unit_Has_Code

   begin
      CU_Vector.Update_Element (CU, Set_CUI_Has_Code'Access);
   end Set_Unit_Has_Code;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range (SCO : SCO_Id) return Source_Location_Range is
      Result : Source_Location_Range;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result

      -------
      -- Q --
      -------

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         Result := SCOD.Sloc_Range;
      end Q;

   --  Start of processing for Sloc_Range

   begin
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
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
            Kind       : SCO_Kind;
            Sloc_Range : Source_Location_Range;

            procedure Q (SCOD : SCO_Descriptor);
            --  Set Kind and Sloc_Range from SCOD

            -------
            -- Q --
            -------

            procedure Q (SCOD : SCO_Descriptor) is
            begin
               Kind       := SCOD.Kind;
               Sloc_Range := SCOD.Sloc_Range;
            end Q;

         begin
            SCO_Vector.Query_Element (SCO, Q'Access);

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
         when ' '    => return Other_Statement;
         when others => raise Constraint_Error;
      end case;
   end To_Statement_Kind;

   -------------------
   -- Unit_Has_Code --
   -------------------

   function Unit_Has_Code (SCO : SCO_Id) return Boolean is
      Result : Boolean;

      procedure QS (SCOD : SCO_Descriptor);
      --  Set Result from SCOD.Origin

      procedure QC (CUI  : CU_Info);
      --  Set Result from CUI

      --------
      -- QS --
      --------

      procedure QS (SCOD : SCO_Descriptor) is
      begin
         CU_Vector.Query_Element (SCOD.Origin, QC'Access);
      end QS;

      --------
      -- QC --
      --------

      procedure QC (CUI : CU_Info) is
      begin
         Result := CUI.Has_Code;
      end QC;

   --  Start of processing for Unit_Has_Code

   begin
      if SCO = No_SCO_Id then
         return False;
      end if;

      SCO_Vector.Query_Element (SCO, QS'Access);
      return Result;
   end Unit_Has_Code;

   ----------------------------
   -- SCO_Tables_Fingerprint --
   ----------------------------

   function SCO_Tables_Fingerprint return SCOs_Hash is
      use GNAT.SHA1;
      use SCOs;

      procedure Update (S : String);
      --  Shortcut for Update (Hash_Ctx, S)

      procedure Update (Sloc : SCOs.Source_Location);
      --  Update Hash_Ctx with Sloc

      Hash_Ctx  : GNAT.SHA1.Context;

      ------------
      -- Update --
      ------------

      procedure Update (S : String) is
      begin
         Update (Hash_Ctx, S);
      end Update;

      ------------
      -- Update --
      ------------

      procedure Update (Sloc : SCOs.Source_Location) is
      begin
         Update (Hash_Ctx, ":" & Logical_Line_Number'Image (Sloc.Line)
                           & ":" & Column_Number'Image (Sloc.Col));
      end Update;

   --  Start of processing for SCO_Tables_Fingerprint

   begin
      --  The aim is to include in the hash all information for which
      --  inconsistency during consolidation would make coverage analysis
      --  nonsensical.

      for I in SCO_Unit_Table.First + 1 .. SCO_Unit_Table.Last loop
         declare
            U : SCO_Unit_Table_Entry renames SCO_Unit_Table.Table (I);
         begin
            if U.Dep_Num /= Missing_Dep_Num then
               --
               --  Directly streaming U to the hash stream would make the
               --  fingerprint computation depend on compiler internals (here,
               --  pragma representation values). Instead, use human-readable
               --  and compiler-independant values.

               Update (U.File_Name.all);
               Update (Nat'Image (U.Dep_Num));

               for S in U.From .. U.To loop
                  declare
                     E : SCO_Table_Entry renames SCO_Table.Table (S);
                  begin
                     Update (E.From);
                     Update (E.To);
                     Update (String'((E.C1, E.C2)));
                     if E.Last then
                        Update ("Last");
                     end if;
                     if E.Pragma_Aspect_Name /= No_Name then
                        Update (Get_Name_String (E.Pragma_Aspect_Name));
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;

      return SCOs_Hash (Binary_Message_Digest'(Digest (Hash_Ctx)));
   end SCO_Tables_Fingerprint;

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
      Name : Name_Id := Pragma_Name;
   begin
      if Name /= No_Name then
         Get_Name_String (Name);
         Name_Buffer (1 .. Name_Len) :=
           To_Lower (Name_Buffer (1 .. Name_Len));
         Name := Name_Find;
      end if;

      return Get_Pragma_Id (Name);
   end Case_Insensitive_Get_Pragma_Id;

begin
   Snames.Initialize;
end SC_Obligations;
