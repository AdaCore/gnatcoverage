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

with Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with Checkpoints; use Checkpoints;
with Diagnostics; use Diagnostics;
with Outputs;
with Slocs;

package body SC_Obligations.BDD is

   procedure Read (CLS : in out Checkpoint_Load_State; Value : out BDD_Node);
   --  Read a BDD_Node from CLS

   procedure Write (CSS : in out Checkpoint_Save_State; Value : BDD_Node);
   --  Write a BDD_Node to CSS

   procedure Read_BDD_Vector is new
     Read_Vector
       (Index_Type   => Valid_BDD_Node_Id,
        Element_Type => BDD_Node,
        "="          => "=",
        Vectors      => BDD_Vectors,
        Read_Element => Read);

   procedure Write_BDD_Vector is new
     Write_Vector
       (Index_Type    => Valid_BDD_Node_Id,
        Element_Type  => BDD_Node,
        "="           => "=",
        Vectors       => BDD_Vectors,
        Write_Element => Write);

   package Arcs_Stacks is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Arcs);

   Arcs_Stack : Arcs_Stacks.Vector;

   procedure Enumerate_Paths
     (BDD_Vector  : in out BDD_Vectors.Vector;
      BDD         : in out BDD_Type;
      Count_Paths : Boolean);
   --  Enumerate all possible paths through BDD. For each BDD node,
   --  Offset_For_True is assigned, so that the unique index of each path
   --  is the sum of the offsets for each True condition. Also identify
   --  conditions that can be reached through more than one path from the
   --  root condition, and determine whether each outcome is reachable.
   --  than one path from the root condition). Note that MC/DC is equivalent
   --  to object branch coverage if, and only if, there is no multi-path
   --  condition.
   --
   --  If Count_Paths is True, also set BDD.Path_Count.

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (BDD_Vector : in out BDD_Vectors.Vector;
      BDD        : in out BDD_Type;
      Node       : BDD_Node;
      Node_Id    : out BDD_Node_Id) is
   begin
      BDD_Vector.Append (Node);
      Node_Id := BDD_Vector.Last_Index;

      pragma
        Assert
          ((BDD.First_Node = No_BDD_Node_Id)
             = (BDD.Last_Node = No_BDD_Node_Id));
      if BDD.First_Node = No_BDD_Node_Id then
         BDD.First_Node := Node_Id;
         BDD.Last_Node := Node_Id;
      else
         pragma Assert (Node_Id = BDD.Last_Node + 1);
         BDD.Last_Node := Node_Id;
      end if;
   end Allocate;

   ----------
   -- Read --
   ----------

   procedure Read
     (CLS : in out Checkpoints.Checkpoint_Load_State; Value : out BDD_Type) is
   begin
      Value.Decision := CLS.Read_SCO;
      Value.Root_Condition := CLS.Read_BDD_Node;
      Value.First_Node := CLS.Read_BDD_Node;
      Value.Last_Node := CLS.Read_BDD_Node;
      Value.First_Multipath_Condition := CLS.Read_BDD_Node;

      Value.Reachable_Outcomes (False) := CLS.Read_Boolean;
      Value.Reachable_Outcomes (True) := CLS.Read_Boolean;

      Value.Path_Count := CLS.Read_Integer;
   end Read;

   procedure Read (CLS : in out Checkpoint_Load_State; Value : out BDD_Node) is
      New_BDDN : BDD_Node (BDD_Node_Kind'Val (CLS.Read_U8));
      pragma Warnings (Off, New_BDDN);

   begin
      --  Set discriminant

      Value := New_BDDN;

      case Value.Kind is
         when Outcome   =>
            Value.Decision_Outcome := CLS.Read_Boolean;

         when Condition =>
            Value.Parent := CLS.Read_BDD_Node;
            Value.Parent_Value := CLS.Read_Boolean;
            Value.C_SCO := CLS.Read_SCO;
            Value.Dests (False) := CLS.Read_BDD_Node;
            Value.Dests (True) := CLS.Read_BDD_Node;
            Value.Path_Offset := CLS.Read_Integer;

         when Jump      =>
            Value.Dest := CLS.Read_BDD_Node;
      end case;
   end Read;

   procedure Read
     (CLS    : in out Checkpoints.Checkpoint_Load_State;
      Vector : out BDD_Vectors.Vector) is
   begin
      Read_BDD_Vector (CLS, Vector);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (CSS : in out Checkpoints.Checkpoint_Save_State; Value : BDD_Type) is
   begin
      CSS.Write_SCO (Value.Decision);
      CSS.Write_BDD_Node (Value.Root_Condition);
      CSS.Write_BDD_Node (Value.First_Node);
      CSS.Write_BDD_Node (Value.Last_Node);
      CSS.Write_BDD_Node (Value.First_Multipath_Condition);

      CSS.Write (Value.Reachable_Outcomes (False));
      CSS.Write (Value.Reachable_Outcomes (True));

      CSS.Write_Integer (Value.Path_Count);
   end Write;

   procedure Write (CSS : in out Checkpoint_Save_State; Value : BDD_Node) is
   begin
      CSS.Write_U8 (BDD_Node_Kind'Pos (Value.Kind));

      case Value.Kind is
         when Outcome   =>
            CSS.Write (Value.Decision_Outcome);

         when Condition =>
            CSS.Write_BDD_Node (Value.Parent);
            CSS.Write (Value.Parent_Value);
            CSS.Write_SCO (Value.C_SCO);
            CSS.Write_BDD_Node (Value.Dests (False));
            CSS.Write_BDD_Node (Value.Dests (True));
            CSS.Write_Integer (Value.Path_Offset);

         when Jump      =>
            CSS.Write_BDD_Node (Value.Dest);
      end case;
   end Write;

   procedure Write
     (CSS    : in out Checkpoints.Checkpoint_Save_State;
      Vector : BDD_Vectors.Vector) is
   begin
      Write_BDD_Vector (CSS, Vector);
   end Write;

   ---------------------
   -- Enumerate_Paths --
   ---------------------

   procedure Enumerate_Paths
     (BDD_Vector  : in out BDD_Vectors.Vector;
      BDD         : in out BDD_Type;
      Count_Paths : Boolean)
   is
      Dummy : constant Outputs.Context_Handle :=
        Outputs.Create_Context
          ("Analyzing decision at " & Slocs.Image (Sloc_Range (BDD.Decision)));

      --  Note regarding Count_Paths handling: instead of introducing
      --  IF blocks everywhere paths are counted (quite verbose and tedious,
      --  complexity of the BDD traversal is the same anyway), just add a
      --  condition when setting the path count for outcomes (i.e. leave to 0
      --  instead of setting to 1). The rest is just making sums, so we will
      --  get 0 everywhere. BDD.Path_Count is supposed to be set to 0
      --  initially, so the final assignment will not change anything.

      subtype Valid_Node_Id is
        BDD_Node_Id range BDD.First_Node .. BDD.Last_Node;

      Visited    : array (Valid_Node_Id) of Boolean := (others => False);
      Path_Count : array (Valid_Node_Id) of Integer := (others => 0);

      Path_Limit_Error : exception;

      procedure Visit
        (Node_Id      : BDD_Node_Id;
         Origin_Id    : BDD_Node_Id;
         Origin_Value : Boolean)
      with Post => Visited (Node_Id);
      --  Visit one node. If it was already seen, record presence of a
      --  multi-path condition. Sets Path_Count (Node_Id) to total count
      --  of paths from the identified node.
      --
      --  May raise Path_Limit_Error if the enumeration of BDD paths exceeds
      --  the limit set in Path_Count_Limit.

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
         --  value. Also mark Node reachable if it is an outcome.

         case Node.Kind is
            when Condition =>

               --  Record first condition that is reachable through multiple
               --  paths.

               if Visited (Node_Id) then
                  if BDD.First_Multipath_Condition = No_BDD_Node_Id then
                     BDD.First_Multipath_Condition := Node_Id;
                     Parent_Id := No_BDD_Node_Id;
                  end if;
                  return;
               end if;

               C_Value := Value (Node.C_SCO);
               Node.Parent := Parent_Id;
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
               Path_Count (Node_Id) := Edge_Count (False) + Edge_Count (True);

            when Outcome   =>
               BDD.Reachable_Outcomes (Node.Decision_Outcome) := True;
               if Count_Paths then
                  Path_Count (Node_Id) := 1;
               end if;

            when others    =>
               raise Program_Error;
         end case;

         if Path_Count (Node_Id) > Path_Count_Limit then
            raise Path_Limit_Error;
         end if;
         Visited (Node_Id) := True;
      end Visit;

      --  Start of processing for Enumerate_Paths

   begin
      Visit
        (BDD.Root_Condition,
         Origin_Id    => No_BDD_Node_Id,
         Origin_Value => False);
      BDD.Path_Count := Path_Count (BDD.Root_Condition);
   exception
      when Path_Limit_Error =>
         BDD.Path_Count := Path_Count_Limit + 1;
   end Enumerate_Paths;

   ---------------
   -- Completed --
   ---------------

   procedure Completed
     (BDD_Vector  : in out BDD_Vectors.Vector;
      BDD         : in out BDD_Type;
      Count_Paths : Boolean)
   is
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
               when Jump      =>
                  Patch_Jump (Node.Dest);

               when Condition =>
                  Patch_Jump (Node.Dests (False));
                  Patch_Jump (Node.Dests (True));

               when others    =>
                  null;
            end case;
         end;
      end loop;

      --  Explore BDD to check for presence of multi-path conditions and (if
      --  Count_Paths is True) assign offsets for path index computation.

      Enumerate_Paths (BDD_Vector, BDD, Count_Paths);

      if SCOs_Trace.Is_Active then
         Dump_BDD (BDD_Vector, BDD);
      end if;
   end Completed;

   --------------
   -- Dump_BDD --
   --------------

   procedure Dump_BDD (BDD_Vector : BDD_Vectors.Vector; BDD : BDD_Type) is
      procedure Dump_Condition (N : BDD_Node_Id);
      --  Display one condition

      --------------------
      -- Dump_Condition --
      --------------------

      procedure Dump_Condition (N : BDD_Node_Id) is
         use Ada.Strings;

         Node           : BDD_Node renames BDD_Vector.Element (N);
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
               when Outcome   =>
                  Put_Line ("return " & Dest_Node.Decision_Outcome'Img);

               when Condition =>
                  if Dest = Next_Condition then
                     Put_Line ("fallthrough");
                  else
                     Put_Line ("goto" & Dest'Img);
                  end if;

               when others    =>
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

         Put
           ("@" & Trim (N'Img, Side => Both) & ": test " & Image (Node.C_SCO));

         case Value (Node.C_SCO) is
            when False   =>
               --  Static known False
               Put_Line (" (always False)");

            when True    =>
               --  Static known True
               Put_Line (" (always True)");

            when Unknown =>
               --  Real runtime test
               New_Line;
         end case;

         Put_Dest (True, Node.Dests (True));
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
            "BDD node"
            & BDD.First_Multipath_Condition'Img
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

   function Create
     (BDD_Vector : in out BDD_Vectors.Vector; Decision : SCO_Id)
      return BDD_Type
   is
      Exit_False_Id, Exit_True_Id : BDD_Node_Id;
   begin
      return BDD : BDD_Type do
         BDD.Decision := Decision;

         Allocate
           (BDD_Vector,
            BDD,
            BDD_Node'(Kind => Outcome, Decision_Outcome => False),
            Exit_False_Id);
         Allocate
           (BDD_Vector,
            BDD,
            BDD_Node'(Kind => Outcome, Decision_Outcome => True),
            Exit_True_Id);

         Push
           (((False => Exit_False_Id, True => Exit_True_Id),
             Origin     => No_BDD_Node_Id,
             Parent_SCO => Decision,
             O_Pos      => Right));
      end return;
   end Create;

   ----------------------
   -- Process_And_Then --
   ----------------------

   procedure Process_And_Then
     (BDD_Vector : in out BDD_Vectors.Vector;
      O_SCO      : SCO_Id;
      BDD        : in out BDD_Type)
   is
      A : constant Arcs := Pop;
      L : BDD_Node_Id;
   begin
      Set_Operand_Or_Expression
        (SCO => A.Parent_SCO, Position => A.O_Pos, Expr => O_SCO);

      Allocate
        (BDD_Vector, BDD, BDD_Node'(Kind => Jump, Dest => No_BDD_Node_Id), L);

      --  Arcs for right operand: subtree is reached through label L if
      --  left operand is True.

      Push
        (((False => A.Dests (False), True => A.Dests (True)),
          Origin     => L,
          Parent_SCO => O_SCO,
          O_Pos      => Right));

      --  Arcs for left operand

      Push
        (((False => A.Dests (False), True => L),
          Origin     => A.Origin,
          Parent_SCO => O_SCO,
          O_Pos      => Left));
   end Process_And_Then;

   -----------------
   -- Process_Not --
   -----------------

   procedure Process_Not (O_SCO : SCO_Id; BDD : BDD_Type) is
      pragma Unreferenced (BDD);

      A : constant Arcs := Pop;
   begin
      Set_Operand_Or_Expression
        (SCO => A.Parent_SCO, Position => A.O_Pos, Expr => O_SCO);

      --  Swap destinations of top arcs

      Push
        (((False => A.Dests (True), True => A.Dests (False)),
          Origin     => A.Origin,
          Parent_SCO => O_SCO,
          O_Pos      => Right));
   end Process_Not;

   ---------------------
   -- Process_Or_Else --
   ---------------------

   procedure Process_Or_Else
     (BDD_Vector : in out BDD_Vectors.Vector;
      O_SCO      : SCO_Id;
      BDD        : in out BDD_Type)
   is
      A : constant Arcs := Pop;
      L : BDD_Node_Id;
   begin
      Set_Operand_Or_Expression
        (SCO => A.Parent_SCO, Position => A.O_Pos, Expr => O_SCO);
      Allocate
        (BDD_Vector, BDD, BDD_Node'(Kind => Jump, Dest => No_BDD_Node_Id), L);

      --  Arcs for right operand: subtree is reached through label L if
      --  left operand is False.

      Push
        (((False => A.Dests (False), True => A.Dests (True)),
          Origin     => L,
          Parent_SCO => O_SCO,
          O_Pos      => Right));

      --  Arcs for left operand

      Push
        (((False => L, True => A.Dests (True)),
          Origin     => A.Origin,
          Parent_SCO => O_SCO,
          O_Pos      => Left));
   end Process_Or_Else;

   -----------------------
   -- Process_Condition --
   -----------------------

   procedure Process_Condition
     (BDD_Vector   : in out BDD_Vectors.Vector;
      BDD          : in out BDD_Type;
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
        (BDD_Vector,
         BDD,
         (Kind   => Condition,
          C_SCO  => Condition_Id,
          Dests  => A.Dests,
          others => <>),
         N);

      if A.Origin /= No_BDD_Node_Id then
         BDD_Vector.Update_Element (A.Origin, Set_Dest'Access);

      else
         pragma Assert (BDD.Root_Condition = No_BDD_Node_Id);
         BDD.Root_Condition := N;
      end if;

      Set_BDD_Node (Condition_Id, N);
      Set_Operand_Or_Expression
        (SCO => A.Parent_SCO, Position => A.O_Pos, Expr => Condition_Id);
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
