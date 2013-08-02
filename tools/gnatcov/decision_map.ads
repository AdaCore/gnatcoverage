------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

--  Management of the decision map

--  The decision map provides information on conditional branches in object
--  code that correspond to source level decisions. It is used to establish
--  decision coverage (DC) and modified condition/decision coverage (MC/DC)
--  coverage properties.

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Disassemblers;  use Disassemblers;
with SC_Obligations; use SC_Obligations;
with Traces;         use Traces;
with Traces_Elf;     use Traces_Elf;

package Decision_Map is

   procedure Analyze (Exe_File : Exe_File_Acc);
   --  Build decision map for symbols Exec. Note that the decision map will
   --  contain references to the designated Exe_File: the pointer must remain
   --  valid throughout execution.

   procedure Build_Decision_Map
     (Exec_Name : String; Text_Start : Pc_Type; Map_Filename : String);
   --  Analyze the named executable using the provided ALI list to
   --  generate the decision map file to Map_Filename for stateful
   --  (historical) traces collection.

   ---------------------------------
   -- Control flow graph analysis --
   ---------------------------------

   --  Conditional branch instructions that correspond to conditions in the
   --  sources are annotated with information relating the corresponding edges
   --  of the control flow graph with the logical structure of the decision.
   --  This information is made visible here for the benefit of the trace
   --  analysis circuitry in Coverage.Sources.

   type Edge_Dest_Kind is
     (Unknown, Condition, Outcome, Raise_Exception);
   --  Destination of an edge in the control flow graph within an occurrence of
   --  a decision: not determined yet, test another condition, final decision
   --  outcome reached, or raising an exception.

   --  Cond_Edge_Info is the information associated with each edge of the
   --  control flow graph.

   type Cond_Edge_Info is record
      Origin            : Tristate := Unknown;
      --  If not Unknown, indicate which value of the tested condition causes
      --  this edge to be taken.

      Destination       : Dest;
      --  Edge destination (note: meaningless, and set to (No_PC, No_PC), if
      --  the cond branch instruction for the edge is a conditional return).

      Dest_Kind         : Edge_Dest_Kind := Unknown;
      --  Edge destination classification, if known

      Op_SCO            : SCO_Id := No_SCO_Id;
      --  For an edge that corresponds to the shortcut case of an operator,
      --  reference to the operator SCO.

      Next_Condition    : Any_Condition_Index := No_Condition_Index;
      --  For the case where Dest_Kind is Condition, index within decision of
      --  the next tested condition.

      Outcome           : Tristate := Unknown;
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
      Last_PC             : Pc_Type;
      --  High bound of PC range for this instruction

      Decision_Occurrence : Decision_Occurrence_Access;
      --  The decision occurrence containing this conditional branch

      Condition           : SCO_Id;
      --  Condition being tested by the conditional branch instruction

      Edges               : Edges_Type;
      --  Edge information for the branch case and fallthrough case
   end record;

   --  In a decision occurrence, each tested condition is represented by
   --  one or more conditional branch instruction.

   use type Pc_Type;
   package PC_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Pc_Type);

   type Decision_Occurrence
     (Last_Cond_Index : Condition_Index)
   is limited record
      Decision             : SCO_Id;
      --  The decision being evaluated

      Conditional_Branches : PC_Vectors.Vector;
      --  Conditional branch instructions testing this decision's conditions

      Seen_Condition       : Any_Condition_Index := No_Condition_Index;
      --  Index of the last seen condition (i.e. condition index of the last
      --  test in Conditional_Branches).
   end record;

   --  The cond branch map contains information describing each conditional
   --  branch instruction. For each routine of interest, we keep only one
   --  reference instance; when processing multiple executables, traces from
   --  successive instances are rebased to the PC range of the original one
   --  (see Traces_Names.Add_Code). The Cond_Branch_Map is keyed by an
   --  (Exe, Reference_PC) pair where Exe is the executable from which the
   --  reference version of the enclosing routine is taken, and PC is relative
   --  to Exe. It can thus accomodate the case of two distinct branches
   --  (in different routines) that happen to have the same reference PC
   --  (in different executables).

   type Cond_Branch_Loc is record
      Exe : Exe_File_Acc;
      PC  : Pc_Type;
   end record;

   function "<" (L, R : Cond_Branch_Loc) return Boolean;

   package Cond_Branch_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Cond_Branch_Loc,
      Element_Type => Cond_Branch_Info);

   Cond_Branch_Map : Cond_Branch_Maps.Map;

end Decision_Map;
