------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

--  Source Coverage Obligations

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with Diagnostics; use Diagnostics;
with SCOs;        use SCOs;
with Switches;    use Switches;
with Types;       use Types;
with Files_Table; use Files_Table;
with Get_SCOs;

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

      --  The BDD is build by maintaining a stack of triples of BDD node ids.
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
      end record;

      procedure Push (A : Arcs);
      function Pop return Arcs;
      --  Manage a stack of Arcs

      --  Construction of a BDD

      function Create (Decision : SCO_Id) return BDD_Type;
      --  Start construction of a new BDD for the given decision

      procedure Process_Not      (BDD : BDD_Type);
      procedure Process_And_Then (BDD : in out BDD_Type);
      procedure Process_Or_Else  (BDD : in out BDD_Type);
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

   -------------------------------
   -- Main SCO descriptor table --
   -------------------------------

   --  Statement_Kind denotes the various statement kinds identified in SCOs

   type Statement_Kind is
     (Type_Declaration,
      Subtype_Declaration,
      Object_Declaration,
      Renaming_Declaration,
      Generic_Instantiation,
      Case_Statement,
      Exit_Statement,
      For_Loop_Statement,
      If_Statement,
      Pragma_Statement,
      Extended_Return_Statement,
      While_Loop_Statement,
      Other_Statement);

   function To_Statement_Kind (C : Character) return Statement_Kind;
   --  Convert character code for statement kind to corresponding enum value

   use type Pc_Type;
   package PC_Sets is new Ada.Containers.Ordered_Sets (Pc_Type);

   type SCO_Descriptor (Kind : SCO_Kind := SCO_Kind'First) is record
      Origin : Source_File_Index;
      --  ALI file containing this SCO

      Sloc_Range : Source_Location_Range;
      --  For a decision, cumulative range from all conditions

      Parent : SCO_Id := No_SCO_Id;
      --  For a decision, pointer to the enclosing statement (or condition in
      --  the case of a nested decision), unset if decision is part of a
      --  flow control structure.
      --  For a condition, pointer to the enclosing decision.

      case Kind is
         when Statement =>
            S_Kind   : Statement_Kind;
            --  Statement kind indication

            Previous : SCO_Id;
            --  Previous statement in sequence. If this one has been executed
            --  then we can safely infer that the previous one has been as
            --  well (recursively).

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

         when Decision =>
            Is_Complex_Decision : Boolean;
            --  True for complex decisions.
            --  Note that there is always a distinct Condition SCO descriptor,
            --  even for simple decisions.

            Last_Cond_Index : Any_Condition_Index;
            --  Index of last condition in decision (should be > 0 for complex
            --  decisions, = 0 otherwise).

            Decision_BDD : BDD.BDD_Type;
            --  BDD of the decision

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

      procedure Check_Diamonds (BDD : in out BDD_Type) is
         Visited : array (BDD_Node_Id range BDD.First_Node .. BDD.Last_Node)
                     of Boolean := (others => False);

         procedure Visit (Node_Id : BDD_Node_Id);
         --  Visit one node. If it was already seen, note presence of a diamond

         procedure Visit (Node_Id : BDD_Node_Id) is
            Node : BDD_Node renames BDD_Vector.Element (Node_Id);
         begin
            --  Nothing to do if a diamond has already been identified

            if BDD.Diamond_Base /= No_BDD_Node_Id then
               return;
            end if;

            if Visited (Node_Id) then
               BDD.Diamond_Base := Node_Id;
               return;
            end if;

            Visited (Node_Id) := True;

            for J in Boolean'Range loop
               if BDD_Vector.Element (Node.Dests (J)).Kind = Condition then
                  Visit (Node.Dests (J));
               end if;
            end loop;
         end Visit;

      begin
         Visit (BDD.Root_Condition);
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

            case SCO_Vector.Element (Node.C_SCO).Value is
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
              (BDD.Decision,
               "BDD node" & BDD.Diamond_Base'Img
               & " reachable through multiple paths",
               Kind => Warning);
            Report ("OBC does not imply MC/DC coverage", Kind => Warning);
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

            Allocate (BDD,
              BDD_Node'(Kind => Outcome, Decision_Outcome => False),
              Exit_False_Id);
            Allocate (BDD,
              BDD_Node'(Kind => Outcome, Decision_Outcome => True),
              Exit_True_Id);

            Push
              (((False => Exit_False_Id,
                 True  => Exit_True_Id),
                Origin => No_BDD_Node_Id));
         end return;
      end Create;

      ----------------------
      -- Process_And_Then --
      ----------------------

      procedure Process_And_Then (BDD : in out BDD_Type) is
         A : constant Arcs := Pop;
         L : BDD_Node_Id;
      begin
         Allocate (BDD, BDD_Node'(Kind => Jump, Dest => No_BDD_Node_Id), L);

         --  Arcs for right operand: subtree is reached through label L if
         --  left operand is True.

         Push
           (((False => A.Dests (False),
              True  => A.Dests (True)),
             Origin => L));

         --  Arcs for left operand

         Push
           (((False => A.Dests (False),
              True  => L),
             Origin => A.Origin));
      end Process_And_Then;

      -----------------
      -- Process_Not --
      -----------------

      procedure Process_Not (BDD : BDD_Type) is
         pragma Unreferenced (BDD);

         A : constant Arcs := Pop;
      begin
         --  Swap destinations of top arcs

         Push
           (((False => A.Dests (True),
              True  => A.Dests (False)),
             Origin => A.Origin));
      end Process_Not;

      ---------------------
      -- Process_Or_Else --
      ---------------------

      procedure Process_Or_Else (BDD : in out BDD_Type) is
         A : constant Arcs := Pop;
         L : BDD_Node_Id;
      begin
         Allocate (BDD, BDD_Node'(Kind => Jump, Dest => No_BDD_Node_Id), L);

         --  Arcs for right operand: subtree is reached through label L if
         --  left operand is False.

         Push
           (((False => A.Dests (False),
              True  => A.Dests (True)),
             Origin => L));

         --  Arcs for left operand

         Push
           (((False => L,
              True  => A.Dests (True)),
             Origin => A.Origin));
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

         procedure Set_Dest (Origin_Node : in out BDD_Node);
         --  Set destination of Origin_Node to N

         procedure Set_BDD_Node (SCOD : in out SCO_Descriptor);
         --  Set associated node of (Condition) SCOD to N

         --------------
         -- Set_Dest --
         --------------

         procedure Set_Dest (Origin_Node : in out BDD_Node) is
         begin
            Origin_Node.Dest := N;
         end Set_Dest;

         ------------------
         -- Set_BDD_Node --
         ------------------

         procedure Set_BDD_Node (SCOD : in out SCO_Descriptor) is
         begin
            SCOD.BDD_Node := N;
         end Set_BDD_Node;

      begin
         Allocate (BDD,
           (Kind => Condition, C_SCO => Condition_Id, Dests => A.Dests), N);

         if A.Origin /= No_BDD_Node_Id then
            BDD_Vector.Update_Element (A.Origin, Set_Dest'Access);

         else
            pragma Assert (BDD.Root_Condition = No_BDD_Node_Id);
            BDD.Root_Condition := N;
         end if;

         SCO_Vector.Update_Element (Condition_Id, Set_BDD_Node'Access);
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

   end BDD;

   --------------------------
   -- Sloc -> SCO_Id index --
   --------------------------

   package Sloc_To_SCO_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Source_Location_Range,
      Element_Type => SCO_Id);
   Sloc_To_SCO_Map : Sloc_To_SCO_Maps.Map;

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
   begin
      SCO_Vector.Update_Element (SCO, Update'Access);
   end Add_Address;

   ----------------
   -- First_Sloc --
   ----------------

   function First_Sloc (SCO : SCO_Id) return Source_Location is
   begin
      return SCO_Vector.Element (SCO).Sloc_Range.First_Sloc;
   end First_Sloc;

   -------------
   -- Has_SCO --
   -------------

   function Has_SCO
     (Sloc_Begin : Source_Location;
      Sloc_End   : Source_Location) return Boolean
   is
      use Sloc_To_SCO_Maps;

      Position : Cursor :=
                   Sloc_To_SCO_Map.Floor
                     (Source_Location_Range'(First_Sloc => Sloc_End,
                                             Last_Sloc  => No_Location));
      SCO      : SCO_Id;
      SCOD     : SCO_Descriptor;
   begin
      while Position /= No_Element loop
         SCO := Element (Position);
         SCOD := SCO_Vector.Element (SCO);

         if Sloc_End < SCOD.Sloc_Range.First_Sloc then
            --  Negative match, and no chance to have a positive match in the
            --  next SCOs: they all have a higher First_Sloc.
            return False;

         elsif SCOD.Sloc_Range.Last_Sloc < Sloc_Begin then
            --  Negative match, but we may reach a positive match in the next
            --  SCO. Continue.
            null;

         else
            --  The two possible negative matches have been dealt with earlier.
            --  We have a positive match.
            return True;

         end if;

         Next (Position);
      end loop;

      return False;
   end Has_SCO;

   -----------
   -- Image --
   -----------

   function Image (SCO : SCO_Id) return String is
      function Sloc_Image (Sloc_Range : Source_Location_Range) return String;
      --  Return sloc information suffix, or empty string if no sloc known

      ----------------
      -- Sloc_Image --
      ----------------

      function Sloc_Image (Sloc_Range : Source_Location_Range) return String is
      begin
         if Sloc_Range.First_Sloc = No_Location then
            return "";
         else
            return " at " & Image (Sloc_Range);
         end if;
      end Sloc_Image;

   begin
      if SCO = No_SCO_Id then
         return "<no SCO>";
      else
         declare
            SCOD : constant SCO_Descriptor := SCO_Vector.Element (SCO);
         begin
            return "SCO #" & Trim (SCO'Img, Side => Ada.Strings.Both) & ": "
              & SCO_Kind'Image (SCOD.Kind)
              & Sloc_Image (SCOD.Sloc_Range);
         end;
      end if;
   end Image;

   -----------
   -- Index --
   -----------

   function Index (SCO : SCO_Id) return Condition_Index is
   begin
      pragma Assert (Kind (SCO) = Condition);
      return SCO_Vector.Element (SCO).Index;
   end Index;

   ----------
   -- Kind --
   ----------

   function Kind (SCO : SCO_Id) return SCO_Kind is
   begin
      return SCO_Vector.Element (SCO).Kind;
   end Kind;

   ---------------------
   -- Last_Cond_Index --
   ---------------------

   function Last_Cond_Index (SCO : SCO_Id) return Condition_Index is
   begin
      pragma Assert (Kind (SCO) = Decision);
      return SCO_Vector.Element (SCO).Last_Cond_Index;
   end Last_Cond_Index;

   ---------------
   -- Last_Sloc --
   ---------------

   function Last_Sloc (SCO : SCO_Id) return Source_Location is
   begin
      return SCO_Vector.Element (SCO).Sloc_Range.Last_Sloc;
   end Last_Sloc;

   ---------------
   -- Load_SCOs --
   ---------------

   procedure Load_SCOs (ALI_Filename : String) is
      Cur_Source_File : Source_File_Index := No_Source_File;
      Cur_SCO_Unit : SCO_Unit_Index;
      Last_Entry_In_Cur_Unit : Int;

      ALI_Index : Source_File_Index;

      ALI_File : File_Type;
      Line     : String (1 .. 1024);
      Last     : Natural;
      Index    : Natural;

      Previous_Statement       : SCO_Id := No_SCO_Id;
      --  Previous statement in the same CS line, used for chaining of basic
      --  blocks.

      Current_Complex_Decision : SCO_Id := No_SCO_Id;
      --  Complex decision whose conditions are being processed

      Current_Condition_Index  : Any_Condition_Index;
      --  Index of current condition within the current decision (0-based, set
      --  to No_Condition_Index, i.e. -1, before the first condition of the
      --  decision is seen).

      Current_BDD : BDD.BDD_Type;
      --  BDD of current decision

      Last_SCO_Upon_Entry : constant SCO_Id := SCO_Vector.Last_Index;

      function Getc return Character;
      --  Consume and return next character from Line.
      --  Load next line if at end of line. Return ^Z if at end of file.

      function Nextc return Character;
      --  Peek at next character in Line. Return ^Z if at end of file.

      procedure Skipc;
      --  Skip one character in Line

      ----------
      -- Getc --
      ----------

      function Getc return Character is
         Next_Char : constant Character := Nextc;
      begin
         Index := Index + 1;
         if Index > Last + 1 and then not End_Of_File (ALI_File) then
            Get_Line (ALI_File, Line, Last);
            Index := 1;
         end if;
         return Next_Char;
      end Getc;

      -----------
      -- Nextc --
      -----------

      function Nextc return Character is
      begin
         if Index = Last + 1 then
            return ASCII.LF;

         elsif Index in Line'First .. Last then
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

   --  Start of processing for Load_SCOs_From_ALI

   begin
      --  First check whether this ALI has been already loaded. We identify
      --  this by the fact that it already has an assigned Source_File_Index.

      ALI_Index := Get_Index_From_Full_Name (ALI_Filename, Insert => False);
      if ALI_Index /= No_Source_File then
         Report
           ("ignoring duplicate ALI file " & ALI_Filename, Kind => Warning);
         return;
      end if;

      ALI_Index := Get_Index_From_Full_Name (ALI_Filename, Insert => True);
      Open (ALI_File, In_File, ALI_Filename);

      --  Here once the ALI file has been succesfully opened

      if Verbose then
         Put_Line ("Loading SCOs from " & ALI_Filename);
      end if;

      Scan_ALI : loop
         if End_Of_File (ALI_File) then
            --  No SCOs in this ALI

            Close (ALI_File);
            return;
         end if;

         Get_Line (ALI_File, Line, Last);
         case Line (1) is
            when 'C' =>
               exit Scan_ALI;

            when others =>
               null;
         end case;
      end loop Scan_ALI;

      Index := 1;

      Get_SCOs_From_ALI;
      Close (ALI_File);

      --  Walk low-level SCO table for this unit and populate high-level tables

      Cur_SCO_Unit := SCO_Unit_Table.First;
      Last_Entry_In_Cur_Unit := SCOs.SCO_Table.First - 1;
      --  Note, the first entry in the SCO_Unit_Table is unused

      for Cur_SCO_Entry in
        SCOs.SCO_Table.First .. SCOs.SCO_Table.Last
      loop
         if Cur_SCO_Entry > Last_Entry_In_Cur_Unit then
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
               Cur_Source_File := Get_Index_From_Simple_Name
                 (SCOUE.File_Name.all);
            end;
         end if;

         pragma Assert (Cur_Source_File /= No_Source_File);
         Process_Entry : declare
            SCOE : SCOs.SCO_Table_Entry renames
                                     SCOs.SCO_Table.Table (Cur_SCO_Entry);

            function Make_Condition_Value return Tristate;
            --  Map condition value code (t/f/c) in SCOE.C2 to Tristate

            function Make_Sloc
              (SCO_Source_Loc : SCOs.Source_Location) return Source_Location;
            --  Build a Slocs.Source_Location record from the low-level
            --  SCO Sloc info.

            procedure Update_Decision_BDD (SCOD : in out SCO_Descriptor);
            --  Set BDD of decision to Current_BDD

            procedure Update_Decision_Sloc (SCOD : in out SCO_Descriptor);
            --  Update the slocs of a complex decision SCOD from those of the
            --  condition in the current SCOE.

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

            ---------------
            -- Make_Sloc --
            ---------------

            function Make_Sloc
              (SCO_Source_Loc : SCOs.Source_Location) return Source_Location
            is
            begin
               if SCO_Source_Loc = SCOs.No_Source_Location then
                  return Source_Location'
                    (Source_File => No_Source_File, others => <>);
               end if;

               return Source_Location'
                 (Source_File => Cur_Source_File,
                  Line        => Natural (SCO_Source_Loc.Line),
                  Column      => Natural (SCO_Source_Loc.Col));
            end Make_Sloc;

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
               From_Sloc : constant Source_Location := Make_Sloc (SCOE.From);
               To_Sloc   : constant Source_Location := Make_Sloc (SCOE.To);

            begin
               if SCOD.Sloc_Range.First_Sloc = No_Location then
                  SCOD.Sloc_Range.First_Sloc := From_Sloc;
               end if;

               if SCOD.Sloc_Range.Last_Sloc = No_Location
                 or else SCOD.Sloc_Range.Last_Sloc < To_Sloc
               then
                  SCOD.Sloc_Range.Last_Sloc := To_Sloc;
               end if;
            end Update_Decision_Sloc;

         --  Start of processing for Process_Entry

         begin
            case SCOE.C1 is
               when 'S' | 's' =>
                  --  Statement

                  pragma Assert (Current_Complex_Decision = No_SCO_Id);
                  if SCOE.C1 = 'S' then
                     Previous_Statement := No_SCO_Id;
                  end if;

                  SCO_Vector.Append
                    (SCO_Descriptor'(Kind       => Statement,
                                     Origin     => ALI_Index,
                                     Sloc_Range =>
                                       (First_Sloc => Make_Sloc (SCOE.From),
                                        Last_Sloc  => Make_Sloc (SCOE.To)),
                                     S_Kind     => To_Statement_Kind (SCOE.C2),
                                     Previous   => Previous_Statement,
                                     others     => <>));
                  Previous_Statement := SCO_Vector.Last_Index;

               when 'I' | 'E' | 'P' | 'W' | 'X' =>
                  --  Decision

                  pragma Assert (Current_Complex_Decision = No_SCO_Id);
                  SCO_Vector.Append
                    (SCO_Descriptor'(Kind                => Decision,
                                     Origin              => ALI_Index,
                                     Sloc_Range          =>
                                       (First_Sloc => Make_Sloc (SCOE.From),
                                        Last_Sloc  => Make_Sloc (SCOE.To)),
                                     Is_Complex_Decision =>
                                                   not SCOE.Last,
                                     Last_Cond_Index     => 0,
                                     others              => <>));
                  Current_BDD := BDD.Create (SCO_Vector.Last_Index);

                  if SCOE.Last then
                     --  Simple decision: no separate condition SCOE, create
                     --  condition immediately.

                     SCO_Vector.Append
                       (SCO_Descriptor'(Kind       => Condition,
                                        Origin     => ALI_Index,
                                        Sloc_Range =>
                                          (First_Sloc => Make_Sloc (SCOE.From),
                                           Last_Sloc  => Make_Sloc (SCOE.To)),
                                        Parent     => SCO_Vector.Last_Index,
                                        Value      => Make_Condition_Value,
                                        Index      => 0,
                                        others     => <>));
                     BDD.Process_Condition
                       (Current_BDD, SCO_Vector.Last_Index);

                     BDD.Completed (Current_BDD);
                     Current_Condition_Index := 0;
                     SCO_Vector.Update_Element
                       (Current_BDD.Decision, Update_Decision_BDD'Access);

                  else
                     --  Complex decision: conditions appear as distinct SCOEs

                     Current_Complex_Decision := SCO_Vector.Last_Index;
                     Current_Condition_Index  := No_Condition_Index;
                  end if;

               when ' ' =>
                  --  Condition

                  pragma Assert (Current_Complex_Decision /= No_SCO_Id);

                  SCO_Vector.Update_Element
                    (Index   => Current_Complex_Decision,
                     Process => Update_Decision_Sloc'Access);

                  Current_Condition_Index := Current_Condition_Index + 1;
                  SCO_Vector.Append
                    (SCO_Descriptor'(Kind       => Condition,
                                     Origin     => ALI_Index,
                                     Sloc_Range =>
                                     (First_Sloc => Make_Sloc (SCOE.From),
                                      Last_Sloc  => Make_Sloc (SCOE.To)),
                                     Parent     => Current_Complex_Decision,
                                     Value      => Make_Condition_Value,
                                     Index      => Current_Condition_Index,
                                     others     => <>));
                  BDD.Process_Condition (Current_BDD, SCO_Vector.Last_Index);

                  if SCOE.Last then
                     BDD.Completed (Current_BDD);
                     SCO_Vector.Update_Element
                       (Current_BDD.Decision, Update_Decision_BDD'Access);

                     Current_Complex_Decision := No_SCO_Id;
                  end if;

               when '!' =>
                  BDD.Process_Not (Current_BDD);

               when '&' =>
                  BDD.Process_And_Then (Current_BDD);

               when '|' =>
                  BDD.Process_Or_Else (Current_BDD);

               when others =>
                  raise Program_Error
                    with "unexpected SCO entry code: " & SCOE.C1;
            end case;
         end Process_Entry;
      end loop;

      --  Build Sloc -> SCO index and set up Parent links

      for SCO in Last_SCO_Upon_Entry + 1 .. SCO_Vector.Last_Index loop
         declare
            First : Source_Location :=
                      SCO_Vector.Element (SCO).Sloc_Range.First_Sloc;

            procedure Process_Descriptor (SCOD : in out SCO_Descriptor);
            --  Set up parent link for SCOD at index SCO, and insert
            --  Sloc -  > SCO map entry.

            ------------------------
            -- Process_Descriptor --
            ------------------------

            procedure Process_Descriptor (SCOD : in out SCO_Descriptor) is
               Enclosing_SCO : constant SCO_Id := Sloc_To_SCO (First);
            begin
               if Verbose then
                  Put ("Processing: " & Image (SCO));
                  if SCOD.Kind = Decision then
                     if SCOD.Is_Complex_Decision  then
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

                     --  Decisions are not included in the sloc map, instead
                     --  their conditions are.

                     First := No_Location;

                     Add_Line_For_Source_Coverage
                       (SCOD.Sloc_Range.First_Sloc.Source_File,
                        SCOD.Sloc_Range.First_Sloc.Line,
                        SCO);

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

                     pragma Assert (SCOD.Sloc_Range.First_Sloc.Source_File
                                  = SCOD.Sloc_Range.Last_Sloc.Source_File);

                     for L in SCOD.Sloc_Range.First_Sloc.Line
                           .. SCOD.Sloc_Range.Last_Sloc.Line
                     loop
                        Add_Line_For_Source_Coverage
                          (SCOD.Sloc_Range.First_Sloc.Source_File, L, SCO);
                     end loop;

                  when Condition =>
                     --  Parent is already set to the enclosing decision
                     null;

               end case;

               if First /= No_Location then
                  begin
                     Sloc_To_SCO_Map.Insert (SCOD.Sloc_Range, SCO);
                  exception
                     when Constraint_Error =>
                        --  Handle the case of junk nested conditions (happens
                        --  with junk SCOs generated for modular integer
                        --  expressions)???

                        Report
                          (SCO,
                           "same sloc range as "
                           & Image (Sloc_To_SCO_Map.Element (SCOD.Sloc_Range))
                           & ", ignored",
                           Kind => Warning);
                  end;
               end if;
            end Process_Descriptor;

         begin
            SCO_Vector.Update_Element (SCO, Process_Descriptor'Access);
         end;
      end loop;
   end Load_SCOs;

   -------------------
   -- Next_BDD_Node --
   -------------------

   function Next_BDD_Node
     (SCO   : SCO_Id;
      Value : Boolean) return BDD.BDD_Node_Id
   is
      use BDD;

      BDDN_Id   : constant BDD_Node_Id := SCO_Vector.Element (SCO).BDD_Node;
      BDDN      : constant BDD_Node := BDD_Vector.Element (BDDN_Id);
   begin
      return BDDN.Dests (Value);
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

   ------------
   -- Parent --
   ------------

   function Parent (SCO : SCO_Id) return SCO_Id is
   begin
      return SCO_Vector.Element (SCO).Parent;
   end Parent;

   --------------
   -- Previous --
   --------------

   function Previous (SCO : SCO_Id) return SCO_Id is
   begin
      return SCO_Vector.Element (SCO).Previous;
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

         SCOD : SCO_Descriptor renames Element (Cur);
      begin
         if SCOD.Kind = Condition and then SCOD.PC_Set.Length = 0 then
            Report
              (SCOD.Sloc_Range.First_Sloc,
               "no conditional branch for " & Image (To_Index (Cur)),
               Kind => Warning);
         end if;
      end Check_Condition;
   begin
      SCO_Vector.Iterate (Check_Condition'Access);
   end Report_SCOs_Without_Code;

   ------------------
   -- Slocs_To_SCO --
   ------------------

   function Sloc_To_SCO (Sloc : Source_Location) return SCO_Id
   is
      use Sloc_To_SCO_Maps;
      L_Sloc : Source_Location := Sloc;
      Cur    : Cursor;
      SCO    : SCO_Id := No_SCO_Id;

   begin
      if L_Sloc.Column = 0 then
         --  For the case of a lookup with a column of 0, we want a SCO
         --  starting before the end of the given line.

         L_Sloc.Column := Natural'Last;
      end if;

      Cur := Sloc_To_SCO_Map.Floor
               ((First_Sloc => L_Sloc, Last_Sloc => No_Location));

      if Cur = No_Element then
         return No_SCO_Id;
      end if;

      SCO := Element (Cur);

      while SCO /= No_SCO_Id loop
         declare
            SCOD : SCO_Descriptor renames SCO_Vector.Element (SCO);
         begin
            if Sloc.Column = 0 then
               exit when
                 Sloc.Source_File = SCOD.Sloc_Range.First_Sloc.Source_File
                 and then Sloc.Line in SCOD.Sloc_Range.First_Sloc.Line
                                    .. SCOD.Sloc_Range.Last_Sloc.Line;
            else
               exit when
                 SCOD.Sloc_Range.First_Sloc <= Sloc
                   and then Sloc <= SCOD.Sloc_Range.Last_Sloc;
            end if;
         end;
         SCO := SCO_Vector.Element (SCO).Parent;
      end loop;
      return SCO;
   end Sloc_To_SCO;

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
         when 'C'    => return Case_Statement;
         when 'E'    => return Exit_Statement;
         when 'F'    => return For_Loop_Statement;
         when 'I'    => return If_Statement;
         when 'P'    => return Pragma_Statement;
         when 'R'    => return Extended_Return_Statement;
         when 'W'    => return While_Loop_Statement;
         when ' '    => return Other_Statement;
         when others => raise Constraint_Error;
      end case;
   end To_Statement_Kind;

end SC_Obligations;
