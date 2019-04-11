------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

--  Generate SCOs and source code instrumentation

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Rewriting; use Libadalang.Rewriting;

with SC_Obligations;    use SC_Obligations;
with Instrument.Common; use Instrument.Common;
with Strings;
with Types;             use Types;

private package Instrument.Sources is

   type Instrumentation_Entities is record
      Common_Buffers : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
      --  Qualified name for the unit that contains coverage buffer types and
      --  witness subprograms.

      Unit_Buffers : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
      --  Qualified name for the unit that contains addresses to coverage
      --  buffers (Pure_Buffer_Unit).

      Statement_Buffer : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
      --  Qualified name for the buffer corresponding to statement coverage
      --  obligations.

      Decision_Buffer : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
      --  Qualified name for the buffer corresponding to decision coverage
      --  obligations.

      MCDC_Buffer : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
      --  Qualified name for the buffer corresponding to paths in decision
      --  BDDs.

   end record;

   -----------------------------
   -- Bit allocation tracking --
   -----------------------------

   --  Bitmap information for statements:
   --  One bit witnessing "statement executed"

   type Statement_Bit_Ids is record
      LL_S_SCO : Nat;
      Executed : Bit_Id;
   end record;

   package LL_Statement_SCO_Bit_Allocs is
      new Ada.Containers.Vectors (Nat, Statement_Bit_Ids);

   --  Bitmap information for decisions:
   --  One bit witnessing each outcome

   type Outcome_Bit_Ids is array (Boolean) of Any_Bit_Id;
   No_Outcome_Bit_Ids : constant Outcome_Bit_Ids := (others => No_Bit_Id);

   type Decision_Bit_Ids is record
      LL_D_SCO       : Nat;

      Outcome_Bits   : Outcome_Bit_Ids := No_Outcome_Bit_Ids;
      Path_Bits_Base : Any_Bit_Id := No_Bit_Id;
   end record;

   package LL_Decision_SCO_Bit_Allocs is
     new Ada.Containers.Vectors (Nat, Decision_Bit_Ids);

   type LL_Unit_Bit_Allocs is record
      Statement_Bits     : LL_Statement_SCO_Bit_Allocs.Vector;
      Last_Statement_Bit : Any_Bit_Id := No_Bit_Id;

      Decision_Bits    : LL_Decision_SCO_Bit_Allocs.Vector;
      Last_Outcome_Bit : Any_Bit_Id := No_Bit_Id;
      Last_Path_Bit    : Any_Bit_Id := No_Bit_Id;
   end record;

   -----------------------------
   -- Instrumentation context --
   -----------------------------

   --  This is the global state for the process of instrumenting a compilation
   --  unit.

   --  Decisions and conditions are not instrumented during the initial tree
   --  traversal, but after high-level SCOs have been generated, because for
   --  MC/DC instrumentation depends on BDD information.

   type Source_Decision is record
      LL_SCO   : Nat;
      --  Low-level SCO id of decision

      Decision : Expr;
      --  Decision expression

      State     : Unbounded_String;
      --  Name of MC/DC state local variable
   end record;

   type Source_Condition is record
      LL_SCO    : Nat;
      --  Low-level SCO id of condition

      Condition : Expr;
      --  Condition expression

      State     : Unbounded_String;
      --  Name of MC/DC state local variable

      First     : Boolean;
      --  True if this condition is the first one in its decision
   end record;

   package Source_Decision_Vectors is
     new Ada.Containers.Vectors (Natural, Source_Decision);
   package Source_Condition_Vectors is
     new Ada.Containers.Vectors (Natural, Source_Condition);

   type Unit_Inst_Context is record
      Instrumented_Unit : Compilation_Unit_Name;
      --  Name of the compilation unit being instrumented

      SFI : Source_File_Index := No_Source_File;
      --  Source file index of the compilation unit being instrumented

      CU : CU_Id := No_CU_Id;
      --  SCO identifier of the compilation unit being instrumented

      Buffer_Unit : Compilation_Unit_Name;
      --  Name of the compilation unit that holds coverage buffers for the
      --  unit currently being instrumented (see Common.Buffer_Unit).

      Pure_Buffer_Unit : Compilation_Unit_Name;
      --  Name of the compilation unit that holds addresses for the coverage
      --  buffers of the unit being instrumented (see Common.Pure_Buffer_Unit).

      Rewriting_Context : Rewriting_Handle;
      --  Rewriting handle for the instrumentation process

      Local_Decls : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
      --  Declarative part where to insert temporary MC/DC state

      Unit_Bits : LL_Unit_Bit_Allocs;
      --  Record of allocation of coverage buffer bits for low-level SCOs

      Source_Decisions  : Source_Decision_Vectors.Vector;
      Source_Conditions : Source_Condition_Vectors.Vector;
      --  Decisions and (for MC/DC) conditions to be instrumented

      Entities : Instrumentation_Entities;
      --  Bank of nodes to use during instrumentation
   end record;

   procedure Initialize_Rewriting
     (IC                : out Unit_Inst_Context;
      Instrumented_Unit : Compilation_Unit_Name;
      Context           : Analysis_Context);
   --  Initialize a unit instrumentation context for the given unit to
   --  instrument.

   procedure Instrument_Source_File
     (CU_Name   : Compilation_Unit_Name;
      Unit_Info : Instrumented_Unit_Info;
      Prj_Info  : in out Project_Info;
      IC        : in out Inst_Context;
      UIC       : out Unit_Inst_Context);
   --  Generate the instrumented source corresponding to CU_Name/Unit_Info.
   --  Record instrumentation information in IC.
   --
   --  If the unit to instrument is also a main and the --auto-dump-buffers is
   --  active, instrumented code will also dump the coverage buffers.

   procedure Add_Auto_Dump_Buffers
     (IC   : Inst_Context;
      Main : Ada_Qualified_Name;
      URH  : Unit_Rewriting_Handle)
      with Pre => IC.Auto_Dump_Buffers;
   --  Try to insert in the sources of Main (a main subprogram) a call to dump
   --  the list of coverage buffers for all units of interest in Main's
   --  closure. Return without doing anything if unsuccessful.
   --
   --  URH must be a rewriting handle for the body unit that contains Main's
   --  sources.

   function Img (Bit : Any_Bit_Id) return String is
     (Strings.Img (Integer (Bit)));

end Instrument.Sources;
