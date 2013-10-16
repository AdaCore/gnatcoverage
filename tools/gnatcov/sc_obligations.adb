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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;         use Ada.Directories;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;

with ALI_Files;     use ALI_Files;
with Aspects;       use Aspects;
with Coverage.Tags; use Coverage, Coverage.Tags;
with Diagnostics;   use Diagnostics;
with Files_Table;   use Files_Table;
with Interfaces;
with Namet;         use Namet;
with SCOs;
with Snames;        use Snames;
with Strings;       use Strings;
with Switches;      use Switches;
with Traces_Elf;    use Traces_Elf;
with Types;         use Types;

package body SC_Obligations is

   subtype Source_Location is Slocs.Source_Location;
   No_Location : Source_Location renames Slocs.No_Location;
   --  (not SCOs.Source_Location)

   --------------------------------------------
   -- Management of binary decision diagrams --
   --------------------------------------------

   package BDD is
      --  Outgoing arcs from a BDD node

      type BDD_Node_Id is new Natural;
      No_BDD_Node_Id : constant BDD_Node_Id := 0;
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

      package BDD_Vectors is
        new Ada.Containers.Vectors
          (Index_Type   => Valid_BDD_Node_Id,
           Element_Type => BDD_Node);
      BDD_Vector : BDD_Vectors.Vector;

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
      end record;

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

   type Inst_Id is new Natural;
   No_Inst_Id : constant Inst_Id := 0;
   subtype Valid_Inst_Id is Inst_Id range No_Inst_Id + 1 .. Inst_Id'Last;

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

   type CU_Info is record
      First_Instance, Last_Instance : Inst_Id;
      --  First and last index of SCO_Instance_Table entries for this unit

      Deps : SFI_Vector;
      --  Mapping of this unit's dependency numbers to source file indices
   end record;

   package CU_Info_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_CU_Id,
      Element_Type => CU_Info);

   package CU_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Valid_CU_Id);

   CU_Map    : CU_Maps.Map;
   CU_Vector : CU_Info_Vectors.Vector;

   procedure New_CU (Base_Name : String; Info : CU_Info);
   --  Enter a new source unit, identified by the base name of its LI/object
   --  files, and return its SU identifier.

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
      Origin : Source_File_Index;
      --  ALI file containing this SCO

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
            S_Kind   : Statement_Kind;
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

            BDD_Node : BDD.BDD_Node_Id;
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

   package SCO_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Valid_SCO_Id,
        Element_Type => SCO_Descriptor);
   SCO_Vector : SCO_Vectors.Vector;

   function Next_BDD_Node
     (SCO   : SCO_Id;
      Value : Boolean) return BDD.BDD_Node_Id;
   --  Given a Condition SCO and the value of the condition, return the
   --  corresponding target node in the decision's BDD.

   procedure Dump_Decision (SCO : SCO_Id);
   --  Display image of decision in reconstructed expression form (for
   --  debugging purposes).

   function Enclosing (What : SCO_Kind; SCO : SCO_Id) return SCO_Id;
   --  Return the innermost enclosing SCO with the given Kind

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

   package R is new Tag_Providers.Register_Factory
     (Name => "instance", T => Instance_Tag_Provider_Type);
   pragma Unreferenced (R);

   ---------
   -- BDD --
   ---------

   package body BDD is

      package Arcs_Stacks is
        new Ada.Containers.Vectors
          (Index_Type   => Natural,
           Element_Type => Arcs);

      Arcs_Stack : Arcs_Stacks.Vector;

      procedure Check_Diamonds (BDD : in out BDD_Type);
      --  Look for diamonds in BDD (conditions that can be reached through more
      --  than one path from the root condition). MC/DC coverage is equivalent
      --  to object branch coverage if, and only if, there is no diamond.

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

      --------------------
      -- Check_Diamonds --
      --------------------

      procedure Check_Diamonds (BDD : in out BDD_Type) is
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

            procedure Set_Parent (Node : in out BDD_Node);
            --  Set Node's parent to Parent_Id

            ----------------
            -- Set_Parent --
            ----------------

            procedure Set_Parent (Node : in out BDD_Node) is
            begin
               Node.Parent       := Parent_Id;
               Node.Parent_Value := Origin_Value;
            end Set_Parent;

            Node : BDD_Node renames BDD_Vector.Element (Node_Id);

         --  Start of processing for Visit

         begin
            --  Nothing to do if a diamond has already been identified

            if BDD.Diamond_Base /= No_BDD_Node_Id then
               return;
            end if;

            if Visited (Node_Id) then
               BDD.Diamond_Base := Node_Id;
               Parent_Id := No_BDD_Node_Id;
               BDD_Vector.Update_Element (Node_Id, Set_Parent'Access);
               return;
            end if;

            Visited (Node_Id) := True;
            BDD_Vector.Update_Element (Node_Id, Set_Parent'Access);

            for J in Boolean'Range loop
               if BDD_Vector.Element (Node.Dests (J)).Kind = Condition then
                  Visit
                    (Node.Dests (J),
                     Origin_Id    => Node_Id,
                     Origin_Value => J);
               end if;
            end loop;
         end Visit;

      begin
         Visit
           (BDD.Root_Condition,
            Origin_Id    => No_BDD_Node_Id,
            Origin_Value => False);
      end Check_Diamonds;

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

         Check_Diamonds (BDD);

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

   ---------------
   -- Comp_Unit --
   ---------------

   function Comp_Unit (LI_Name : String) return CU_Id is
      use CU_Maps;
      Cur : constant Cursor := CU_Map.Find (LI_Name);
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

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         First := SCOD.Decision_BDD.First_Node;
         Last  := SCOD.Decision_BDD.Last_Node;
      end Q;

      Current_Condition_Index : Any_Condition_Index := No_Condition_Index;
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
      use SCOs;
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
      use BDD;
   begin
      return SCO_Vector (SCO).Decision_BDD.Diamond_Base
               /= No_BDD_Node_Id;
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
      use SCOs;
      use Ada.Strings;

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

   ---------------------------
   -- Is_Disabled_Statement --
   ---------------------------

   function Is_Disabled_Statement (SCO : SCO_Id) return Boolean is
      SCOD  : SCO_Descriptor renames SCO_Vector (SCO);
   begin
      pragma Assert (SCOD.Kind = Statement);
      return SCOD.S_Kind = Disabled_Pragma_Statement;
   end Is_Disabled_Statement;

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

   -----------------------------
   -- Is_Pragma_Annotate_Xcov --
   -----------------------------

   function Is_Pragma_Annotate_Xcov (SCO : SCO_Id) return Boolean is
      Result : Boolean;

      procedure Q (SCOD : SCO_Descriptor);
      --  Set Result

      procedure Q (SCOD : SCO_Descriptor) is
      begin
         Result := SCOD.S_Kind = Pragma_Statement
           and then ALI_Annotations.Contains (First_Sloc (SCOD.Sloc_Range));
      end Q;

   --  Start of processing for Is_Pragma_Annotate_Xcov

   begin
      pragma Assert (Kind (SCO) = Statement);
      SCO_Vector.Query_Element (SCO, Q'Access);
      return Result;
   end Is_Pragma_Annotate_Xcov;

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

   ---------------
   -- Load_SCOs --
   ---------------

   procedure Load_SCOs (ALI_Filename : String) is
      use SCOs;

      Cur_Source_File        : Source_File_Index := No_Source_File;
      Cur_SCO_Unit           : SCO_Unit_Index;
      Last_Entry_In_Cur_Unit : Int;
      Last_Entry_Last_Line   : Natural := 0;
      --  Line number of high bound of sloc range of last processed entry

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

      --  Record last SCO and instance identifiers prior to loading SCOs

      Last_SCO_Upon_Entry      : constant SCO_Id  := SCO_Vector.Last_Index;
      Last_Instance_Upon_Entry : constant Inst_Id := Inst_Vector.Last_Index;

      Deps      : SFI_Vector;
      --  Dependencies of this compilation unit

      ALI_Index : constant Source_File_Index :=
                             Load_ALI (ALI_Filename, Deps, With_SCOs => True);
      --  Load ALI file and update the last SCO and instance indices

      Deps_Present : constant Boolean := not Deps.Is_Empty;

      procedure Prealloc_Lines;
      --  Pre-allocate line table entries for Cur_Source_File to accomodate
      --  Last_Entry_Last_Line (optimization only).

      procedure Prealloc_Lines is
      begin
         if Cur_Source_File /= No_Source_File
           and then Last_Entry_Last_Line > 0
         then
            Expand_Line_Table (Cur_Source_File, Last_Entry_Last_Line);
            Last_Entry_Last_Line := 0;
         end if;
      end Prealloc_Lines;

   --  Start of processing for Load_SCOs

   begin
      if ALI_Index = No_Source_File then
         return;
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

            Prealloc_Lines;

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
               if Deps_Present then
                  Cur_Source_File := Deps.Element (SCOUE.Dep_Num);

               else
                  --  For C, the GLI file does not provide a proper deps table

                  Cur_Source_File := Get_Index_From_Simple_Name
                    (SCOUE.File_Name.all);
               end if;
            end;
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
                                  Origin     => ALI_Index,
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

            Pragma_Aspect_Name : Name_Id := SCOE.Pragma_Aspect_Name;

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

                  --  Older compilers produced pragma names in uppercase.
                  --  We now expect them in lowercase (canonical form for
                  --  Get_Pragma_Id), so convert here.

                  if Pragma_Aspect_Name /= No_Name then
                     Get_Name_String (Pragma_Aspect_Name);
                     Name_Buffer (1 .. Name_Len) :=
                       To_Lower (Name_Buffer (1 .. Name_Len));
                     Pragma_Aspect_Name := Name_Find;
                  end if;

                  SCO_Vector.Append
                    (SCO_Descriptor'(Kind                 => Statement,
                                     Origin               => ALI_Index,
                                     Sloc_Range           => SCO_Range,
                                     S_Kind               =>
                                       To_Statement_Kind (SCOE.C2),
                                     Dominant             => Dom_SCO,
                                     Dominant_Sloc        => Dom_Sloc,
                                     Dominant_Value       => Dom_Val,
                                     Handler_Range        =>
                                       Current_Handler_Range,
                                     Pragma_Name          =>
                                       Get_Pragma_Id (Pragma_Aspect_Name),
                                     others               => <>));

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
                    (SCO_Descriptor'(Kind                => Decision,
                                     Origin              => ALI_Index,
                                     Control_Location    =>
                                        Slocs.To_Sloc
                                          (Cur_Source_File, From_Sloc),
                                     D_Kind              =>
                                       To_Decision_Kind (SCOE.C1),
                                     Last_Cond_Index     => 0,
                                     Aspect_Name         =>
                                       Get_Aspect_Id (Pragma_Aspect_Name),
                                     others              => <>));
                  Current_BDD := BDD.Create (SCO_Vector.Last_Index);

                  pragma Assert (not SCOE.Last);

                  Current_Decision        := SCO_Vector.Last_Index;
                  Current_Condition_Index := No_Condition_Index;

               when ' ' =>
                  --  Condition

                  pragma Assert (Current_Decision /= No_SCO_Id);

                  SCO_Vector.Update_Element
                    (Index   => Current_Decision,
                     Process => Update_Decision_Sloc'Access);

                  Current_Condition_Index := Current_Condition_Index + 1;

                  SCO_Vector.Append
                    (SCO_Descriptor'(Kind       => Condition,
                                     Origin     => ALI_Index,
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
      end loop;

      --  Record compilation unit and instance range

      New_CU (Base_Name (ALI_Filename),
        (First_Instance => Last_Instance_Upon_Entry + 1,
         Last_Instance  => Last_Instance_Upon_Entry + 1
                             + Inst_Id (SCO_Instance_Table.Last)
                             - Inst_Id (SCO_Instance_Table.First),
         Deps           => Deps));

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

      Prealloc_Lines;

      --  Build Sloc -> SCO index and set up Parent links

      for SCO in Last_SCO_Upon_Entry + 1 .. SCO_Vector.Last_Index loop
         declare
            procedure Process_Descriptor (SCOD : in out SCO_Descriptor);
            --  Set up Parent link for SCOD at index SCO, and insert
            --  Sloc -> SCO map entry.

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

                     for L in SCOD.Sloc_Range.L.First_Sloc.Line
                           .. SCOD.Sloc_Range.L.Last_Sloc.Line
                     loop
                        Add_Line_For_Source_Coverage
                          (SCOD.Sloc_Range.Source_File, L, SCO);
                     end loop;

                  when Statement =>
                     --  A SCO for a (simple) statement is never nested

                     --  pragma Assert (Enclosing_SCO = No_SCO_Id);
                     --  For now generate explicit diagnostic, ignore nested
                     --  SCO and proceed???

                     if Enclosing_SCO /= No_SCO_Id then
                        Report
                          (First,
                           "unexpected SCO nesting in "
                           & Image (Enclosing_SCO)
                           & ", discarding nested SCO");
                        return;
                     end if;

                     for L in SCOD.Sloc_Range.L.First_Sloc.Line
                           .. SCOD.Sloc_Range.L.Last_Sloc.Line
                     loop
                        Add_Line_For_Source_Coverage
                          (SCOD.Sloc_Range.Source_File, L, SCO);
                     end loop;

                  when Condition | Operator =>
                     --  Parent is already set to the enclosing decision or
                     --  operator.

                     null;

               end case;

               Sloc_To_SCO_Map (Sloc_Range.Source_File, SCOD.Kind).Insert
                 (Sloc_Range.L, SCO);
               --  Note: we used to handle Constraint_Error here to account for
               --  old compilers that generated junk SCOs with the same source
               --  locations. These bugs have now been fixed, so the
               --  work-around was removed, and if this happened again we'd
               --  propagate the exception.
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
   end Load_SCOs;

   ------------
   -- New_CU --
   ------------

   procedure New_CU (Base_Name : String; Info : CU_Info) is
   begin
      CU_Vector.Append (Info);
      CU_Map.Insert (Base_Name, CU_Vector.Last_Index);
   end New_CU;

   -------------------
   -- Next_BDD_Node --
   -------------------

   function Next_BDD_Node
     (SCO   : SCO_Id;
      Value : Boolean) return BDD.BDD_Node_Id
   is
      use BDD;

      Result : BDD.BDD_Node_Id;

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
      BDDN : constant BDD_Node :=
               BDD_Vector.Element (Next_BDD_Node (SCO, Value));
   begin
      case BDDN.Kind is
         when Outcome =>
            return To_Tristate (BDDN.Decision_Outcome);

         when Condition =>
            return Unknown;

         when others =>
            raise Program_Error;
      end case;
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

   ------------------------------
   -- Report_SCOs_Without_Code --
   ------------------------------

   procedure Report_SCOs_Without_Code is
      use SCO_Vectors;

      procedure Check_Condition (Cur : Cursor);
      --  Check whether this condition has an associated conditional branch

      ---------------------
      -- Check_Condition --
      ---------------------

      procedure Check_Condition (Cur : Cursor) is
         use Ada.Containers;

         SCOD  : SCO_Descriptor renames Element (Cur);
      begin
         if SCOD.Kind = Condition and then SCOD.PC_Set.Length = 0 then
            --  Static analysis failed???

            Report
              (First_Sloc (SCOD.Sloc_Range),
               Msg  => "no conditional branch (in "
                         & Decision_Kind'Image
                             (SCO_Vector
                                (Enclosing_Decision (To_Index (Cur))).D_Kind)
                         & ")",
               Kind => Diagnostics.Error);

            --  Report a static analysis error if one condition has no
            --  associated conditional branch.

            --  Interesting property: we can never do without a condition using
            --  inference of condition values from BDD position, because that
            --  would require that both outgoing edges from the condition also
            --  are conditions (not outcomes), and that can't happen in a short
            --  circuit expression without a diamond (this would require a BDD
            --  involving the Sel ternary operator:
            --    Sel (A, B, C) = (A and then B) or else (not A and then C)

         end if;
      end Check_Condition;

   --  Start of processing for Report_SCOs_Without_Code

   begin
      SCO_Vector.Iterate (Check_Condition'Access);
   end Report_SCOs_Without_Code;

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

end SC_Obligations;
