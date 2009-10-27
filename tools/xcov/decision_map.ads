------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

--  Management of the decision map

--  The decision map provides information on conditional branches in object
--  code that correspond to source level decisions. It is used to establish
--  decision coverage (DC) and modified condition/decision coverage (MC/DC)
--  coverage properties.

with Ada.Containers.Ordered_Maps;

with Interfaces;

with SC_Obligations; use SC_Obligations;
with Traces;         use Traces;

package Decision_Map is

   procedure Build_Decision_Map (Exec_Name : String);
   --  Analyze the named executable using the provided ALI list to generate
   --  the decision map file for stateful (historical) traces collection.

   procedure Write_Map (Filename : String);
   --  Write the contents of the decision map to the named file

   ---------------------------------
   -- Control flow graph analysis --
   ---------------------------------

   --  Conditional branch instructions that correspond to conditions in the
   --  sources are annotated with information relating the corresponding edges
   --  of the control flow graph with the logical structure of the decision.
   --  This information is made visible here for the benefit of the trace
   --  analysis circuitry in Coverage.Sources.

   type Edge_Dest_Kind is (Unknown, Condition, Outcome);
   --  Destination of an edge in the control flow graph within an occurrence of
   --  a decision: not determined yet, test another condition, final decision
   --  outcome reached.

   --  Cond_Edge_Info is the information associated with each edge of the
   --  control flow graph.

   type Cond_Edge_Info is record
      Origin         : Tristate := Unknown;
      --  If not Unknown, indicate which value of the tested condition causes
      --  this edge to be taken.

      Destination    : Pc_Type;
      --  Edge destination

      Dest_Kind      : Edge_Dest_Kind := Unknown;
      --  Edge destination classification, if known

      Next_Condition : Integer := -1;
      --  For the case where Dest_Kind is Condition, index within decision of
      --  the next tested condition.

      Outcome        : Tristate       := Unknown;
      --  For the case where Dest_Kind is Outcome, corresponding valuation of
      --  the decision, if known.
   end record;

   --  Cond_Branch_Info is the information associated with each conditional
   --  branch instruction.

   type Edge_Kind is (Branch, Fallthrough);
   type Edges_Type is array (Edge_Kind) of Cond_Edge_Info;

   type Decision_Occurrence;
   type Decision_Occurrence_Access is access all Decision_Occurrence;

   type Cond_Branch_Info is record
      Decision_Occurrence : Decision_Occurrence_Access;
      --  The decision occurrence containing this conditional branch

      Condition           : SCO_Id;
      --  Condition being tested by the conditional branch instruction

      Edges               : Edges_Type;
      --  Edge information for the branch case and fallthrough case
   end record;

   type Condition_Occurrence_Array is array (Natural range <>) of Pc_Type;

   --  In a decision occurrence, each tested condition is represented by
   --  a conditional branch instruction.

   type Decision_Occurrence (Last_Cond_Index : Natural) is limited record
      Decision              : SCO_Id;
      --  The decision being evaluated

      Condition_Occurrences : Condition_Occurrence_Array
                                (0 .. Last_Cond_Index) := (others => No_PC);
      --  The corresponding evaluations of the conditions in the decision

      Seen_Condition        : Integer := -1;
      --  Index of the last seen condition (i.e. highest value such that
      --  Condition_Occurrences (Seen_Condition) /= No_PC).
   end record;

   package Cond_Branch_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Pc_Type,
      Element_Type => Cond_Branch_Info,
      "<"          => Interfaces."<");

   Cond_Branch_Map : Cond_Branch_Maps.Map;

end Decision_Map;
