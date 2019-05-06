------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

with Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with Checkpoints; use Checkpoints;
with Diagnostics; use Diagnostics;
with Switches;    use Switches;

package body SC_Obligations.BDD is

   package Arcs_Stacks is
     new Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Arcs);

   Arcs_Stack : Arcs_Stacks.Vector;

   procedure Enumerate_Paths (BDD : in out BDD_Type);
   --  Enumerate all possible paths through BDD. For each BDD node,
   --  Offset_For_True is assigned, so that the unique index of each path
   --  is the sum of the offsets for each True condition. Also identify
   --  conditions that can be reached through more than one path from the
   --  root condition, and determine whether each outcome is reachable.
   --  than one path from the root condition). Note that MC/DC is equivalent
   --  to object branch coverage if, and only if, there is no multi-path
   --  condition.

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
      BDD_Node_Id'Read  (S, V.First_Multipath_Condition);
      Reachability'Read (S, V.Reachable_Outcomes);

      if not Version_Less (S, Than => 2) then
         Natural'Read (S, V.Path_Count);
      end if;
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

            --  Checkpoint version 2 data (instrumentation support)

            if not Version_Less (S, 2) then
               Natural'Read (S, V.Path_Offset);
            end if;

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
      BDD_Node_Id'Write  (S, V.First_Multipath_Condition);
      Reachability'Write (S, V.Reachable_Outcomes);

      if not Version_Less (S, Than => 2) then
         Natural'Write (S, V.Path_Count);
      end if;
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

            --  Checkpoint version 2 data (instrumentation support)

            if not Version_Less (S, Than => 2) then
               Natural'Write (S, V.Path_Offset);
            end if;

         when Jump =>
            BDD_Node_Id'Write (S, V.Dest);
      end case;
   end Write;

   ---------------------
   -- Enumerate_Paths --
   ---------------------

   procedure Enumerate_Paths (BDD : in out BDD_Type) is
      Path_Count : array (BDD_Node_Id range BDD.First_Node .. BDD.Last_Node)
        of Integer := (others => 0);

      procedure Visit
        (Node_Id      : BDD_Node_Id;
         Origin_Id    : BDD_Node_Id;
         Origin_Value : Boolean)
        with Post => Path_Count (Node_Id) > 0;
      --  Visit one node. If it was already seen, record presence of a
      --  multi-path condition. Sets Path_Count (Node_Id) to total count
      --  of paths from the identified node.

      -----------
      -- Visit --
      -----------

      procedure Visit
        (Node_Id      : BDD_Node_Id;
         Origin_Id    : BDD_Node_Id;
         Origin_Value : Boolean)
      is
         Node      : BDD_Node renames BDD_Vector.Reference (Node_Id);
         Parent_Id : BDD_Node_Id := Origin_Id;
         C_Value   : Tristate;

         Edge_Count : array (Boolean) of Natural := (others => 0);
         --  Count of paths through each successor to any outcome
      begin
         --  Set Node's parent to Parent_Id. Set C_Value to Node's condition
         --  value. Also markes outcome as reachable if Node is an outcome.

         case Node.Kind is
            when Condition =>

               --  Record first condition that is reachable through multiple
               --  paths.

               if Path_Count (Node_Id) > 0 then
                  if BDD.First_Multipath_Condition = No_BDD_Node_Id then
                     BDD.First_Multipath_Condition := Node_Id;
                     Parent_Id := No_BDD_Node_Id;
                  end if;
                  return;
               end if;

               C_Value           := Value (Node.C_SCO);
               Node.Parent       := Parent_Id;
               Node.Parent_Value := Origin_Value;

               for J in Boolean'Range loop
                  --  Visit destination J if C_Value is Unknown or J

                  if C_Value /= To_Tristate (not J) then
                     Visit
                       (Node.Dests (J),
                        Origin_Id    => Node_Id,
                        Origin_Value => J);
                     Edge_Count (J) := Path_Count (Node.Dests (J));
                  end if;
               end loop;

               --  Compute path index contribution:
               --  * an outcome counts as one path
               --  * a successor condition counts as N paths
               --  * total paths for this node is sum of True and False edge
               --    paths
               --  * offset contribution when True is path count for False
               --    edge.

               Node.Path_Offset := Edge_Count (False);
               Path_Count (Node_Id) :=
                 Edge_Count (False) + Edge_Count (True);

            when Outcome =>
               BDD.Reachable_Outcomes (Node.Decision_Outcome) := True;
               Path_Count (Node_Id) := 1;

            when others =>
               raise Program_Error;
         end case;
      end Visit;

   --  Start of processing for Check_Reachability

   begin
      Visit
        (BDD.Root_Condition,
         Origin_Id    => No_BDD_Node_Id,
         Origin_Value => False);
      BDD.Path_Count := Path_Count (BDD.Root_Condition);
   end Enumerate_Paths;

   ---------------
   -- Completed --
   ---------------

   procedure Completed (BDD : in out BDD_Type) is
      use BDD_Vectors;

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
         declare
            Node : BDD_Node renames BDD_Vector.Reference (J);
         begin
            --  Replace all destinations of Node that denote Jump nodes with
            --  the jump destination.

            case Node.Kind is
               when Jump =>
                  Patch_Jump (Node.Dest);

               when Condition =>
                  Patch_Jump (Node.Dests (False));
                  Patch_Jump (Node.Dests (True));

               when others =>
                  null;
            end case;
         end;
      end loop;

      --  Explore BDD to check for presence of multi-path conditions and
      --  assign offsets for path index computation.

      Enumerate_Paths (BDD);

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

         case Value (Node.C_SCO) is
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
      if BDD.First_Multipath_Condition /= No_BDD_Node_Id then
         Report
           (First_Sloc (BDD.Decision),
            "BDD node" & BDD.First_Multipath_Condition'Img
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

      procedure Set_Dest (Origin_Node : in out BDD_Node);
      --  Set destination of Origin_Node to N

      --------------
      -- Set_Dest --
      --------------

      procedure Set_Dest (Origin_Node : in out BDD_Node) is
      begin
         Origin_Node.Dest := N;
      end Set_Dest;

   --  Start of processing for Process_Condition

   begin
      Allocate
        (BDD, (Kind         => Condition,
               C_SCO        => Condition_Id,
               Dests        => A.Dests,
               others       => <>), N);

      if A.Origin /= No_BDD_Node_Id then
         BDD_Vector.Update_Element (A.Origin, Set_Dest'Access);

      else
         pragma Assert (BDD.Root_Condition = No_BDD_Node_Id);
         BDD.Root_Condition := N;
      end if;

      Set_BDD_Node (Condition_Id, N);
      Set_Operand
        (Operator => A.O_SCO,
         Position => A.O_Pos,
         Operand  => Condition_Id);
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

end SC_Obligations.BDD;
