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

--  Binary Decision Diagrams

private package SC_Obligations.BDD is

   Path_Count_Limit : Natural := 1024;
   --  Limit beyond which BDD path enumeration should stop.
   --  TODO: Arbitrary, to be fine tuned.

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

            Path_Offset : Natural := 0;
            --  Contribution to be added to the BDD path index if this
            --  condition is True.

         when Jump =>
            Dest : BDD_Node_Id := No_BDD_Node_Id;
            --   Next BDD node
      end case;
   end record;

   --  Stream attributes for BDD_Node, aware of checkpoint format versioning

   pragma Warnings (Off, "* is not referenced");
   --  Work around compiler bug: bogus warning???

   package BDD_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Valid_BDD_Node_Id,
        Element_Type => BDD_Node);

   procedure Read
     (CLS    : in out Checkpoints.Checkpoint_Load_State;
      Vector : out BDD_Vectors.Vector);
   --  Read a BDD_Vectors.Vector from CLS

   procedure Write
     (CSS    : in out Checkpoints.Checkpoint_Save_State;
      Vector : BDD_Vectors.Vector);
   --  Write a BDD_Vectors.Vector to CSS

   type Reachability is array (Boolean) of Boolean;

   Both_Reachable : constant Reachability := (others => True);

   type BDD_Type is record
      Decision       : SCO_Id;

      Root_Condition : BDD_Node_Id := No_BDD_Node_Id;
      --  First tested condition

      First_Node     : BDD_Node_Id := No_BDD_Node_Id;
      Last_Node      : BDD_Node_Id := No_BDD_Node_Id;
      --  Index range of BDD nodes of this BDD

      First_Multipath_Condition : BDD_Node_Id := No_BDD_Node_Id;
      --  If set, designates a node that is reachable through multiple paths
      --  (see Enumerate_Paths).

      Reachable_Outcomes : Reachability := (others => False);
      --  Indicates whether each outcome is reachable (an outcome may be
      --  unreachable due to constant conditions).

      Path_Count : Natural := 0;
      --  Count of paths from root condition to any outcome
   end record;

   procedure Read
     (CLS : in out Checkpoints.Checkpoint_Load_State; Value : out BDD_Type);
   --  Read a BDD_Type from CLS

   procedure Write
     (CSS : in out Checkpoints.Checkpoint_Save_State; Value : BDD_Type);
   --  Write a BDD_Type to CSS

   procedure Allocate
     (BDD_Vector : in out BDD_Vectors.Vector;
      BDD        : in out BDD_Type;
      Node       : BDD_Node;
      Node_Id    : out BDD_Node_Id);
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
      Dests      : Destinations;
      --  Outgoing arcs for next condition

      Origin     : BDD_Node_Id := No_BDD_Node_Id;
      --  Jump node referencing next condition

      Parent_SCO : SCO_Id;
      --  Reference to parent operator or decision

      O_Pos      : Operand_Position;
      --  Position of this node within the enclosing operator (Left or Right)
   end record;

   procedure Push (A : Arcs);
   function Pop return Arcs;
   --  Manage a stack of Arcs

   --  Construction of a BDD

   function Create
     (BDD_Vector : in out BDD_Vectors.Vector;
      Decision   : SCO_Id) return BDD_Type;
   --  Start construction of a new BDD for the given decision

   procedure Process_Not (O_SCO : SCO_Id; BDD   : BDD_Type);
   procedure Process_And_Then
     (BDD_Vector : in out BDD_Vectors.Vector;
      O_SCO      : SCO_Id;
      BDD        : in out BDD_Type);
   procedure Process_Or_Else
     (BDD_Vector : in out BDD_Vectors.Vector;
      O_SCO      : SCO_Id;
      BDD        : in out BDD_Type);
   --  Process NOT, AND THEN, OR ELSE operators

   procedure Process_Condition
     (BDD_Vector   : in out BDD_Vectors.Vector;
      BDD          : in out BDD_Type;
      Condition_Id : SCO_Id);
   --  Process condition

   procedure Completed
     (BDD_Vector  : in out BDD_Vectors.Vector;
      BDD         : in out BDD_Type;
      Count_Paths : Boolean);
   --  Called when all items in decision have been processed

   procedure Dump_BDD
     (BDD_Vector : BDD_Vectors.Vector;
      BDD        : BDD_Type);
   --  Display BDD for debugging purposes

end SC_Obligations.BDD;
