------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2022, AdaCore                     --
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

--  Instrumentation of a C source file

with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Strings.Unbounded;

with Types; use Types;

with Clang.Index;   use Clang.Index;
with Clang.Rewrite; use Clang.Rewrite;

with Instrument.Base_Types; use Instrument.Base_Types;
with Instrument.Common;     use Instrument.Common;

private package Instrument.C is
   type C_Source_Decision is record
      LL_SCO : Nat;
      --  Low-level SCO id of decision

      Decision : Cursor_T;
      --  Decision expression

      State : Ada.Strings.Unbounded.Unbounded_String;
      --  Name of MC/DC state local variable
   end record;

   type C_Source_Condition is record
      LL_SCO : Nat;
      --  Low-level SCO id of condition

      Condition : Cursor_T;
      --  Condition expression

      State : Ada.Strings.Unbounded.Unbounded_String;
      --  Name of MC/DC state local variable

      First : Boolean;
      --  True if this condition is the first one in its decision
   end record;

   package Source_Decision_Vectors is
     new Ada.Containers.Vectors (Natural, C_Source_Decision);
   package Source_Condition_Vectors is
     new Ada.Containers.Vectors (Natural, C_Source_Condition);

   type C_Unit_Inst_Context is new Instrument.Common.Unit_Inst_Context with
      record
         TU       : Translation_Unit_T;
         Rewriter : Rewriter_T;

         File : Ada.Strings.Unbounded.Unbounded_String;
         --  Original filename

         Source_Decisions  : Source_Decision_Vectors.Vector;
         Source_Conditions : Source_Condition_Vectors.Vector;
         --  Decisions and (for MC/DC) conditions to be instrumented

         MCDC_State_Declaration_Node : Cursor_T;
         --  Where should MCDC state declaration be inserted (the beginning of
         --  the procedure).
      end record;

   type C_Source_Rewriter is tagged limited private;
   --  Helper object to instrument a source file

   procedure Add_Auto_Dump_Buffers
     (IC   : Inst_Context;
      Info : in out Project_Info;
      Main : Compilation_Unit_Name;
      Rew  : C_Source_Rewriter)
     with Pre => IC.Dump_Config.Trigger /= Manual;
   --  Try to insert in the sources of Main (a main subprogram) a call to dump
   --  the list of coverage buffers for all units of interest in Main's
   --  closure. Return without doing anything if unsuccessful.
   --
   --  Info must be the project that owns the Main unit, and URH must be a
   --  rewriting handle for the body unit that contains Main's sources.

   type Instr_Scheme_Type is (Instr_Stmt, Instr_Expr);
   --  Depending on the statement construct, we can instrument it either with
   --  another statement right before, which is the case for most expressions,
   --
   --  int a = 1;
   --
   --  will become
   --
   --  witness;
   --  int a = 1;
   --
   --  or we have to instrument it by augmenting the underlying expression
   --  (when it is a statement expression), like:
   --
   --  while (a = 2) {}
   --
   --  will become
   --
   --  while (witness && a = 2) {}
   --
   --  Here, we can't have the witness call go before the while, as there could
   --  very well be a goto pointing inside the loop, making it skip the
   --  execution of the witness statement, but we would still be executing the
   --  condition of the loop on the second iteration.
   --
   --  TODO: we still have to figure out what should be done with such valid
   --  C++ constructs:
   --
   --  while (int i = calc()){}

   procedure Extend_Statement_Sequence
     (N           : Cursor_T;
      Typ         : Character;
      UIC         : C_Unit_Inst_Context;
      Insertion_N : Cursor_T := Get_Null_Cursor;
      Instr_Scheme : Instr_Scheme_Type := Instr_Stmt);

   procedure Apply (Self : in out C_Source_Rewriter);

   procedure Start_Rewriting
     (Self           : out C_Source_Rewriter;
      Info           : in out Project_Info;
      Input_Filename : String);
   --  Start a rewriting session for the given Input_Filename. If the rewriting
   --  process is successful, the result will be written to a file in
   --  Info.Output_Dir with the basename of Output_Filename.
   --
   --  This registers the output file in Info.Instr_Files.
   --
   --  If there are parsing errors while reading Input_Filename, this raises a
   --  fatal error and prints the corresponding error messages.

   procedure Instrument_Unit
     (CU_Name   : Compilation_Unit_Name;
      IC        : in out Inst_Context;
      Unit_Info : in out Instrumented_Unit_Info);
   --  Instrument a single source file of interest from the project

   procedure Emit_Buffers_List_Unit
     (IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info);
   --  Emit in the root project a unit to contain the list of coverage buffers
   --  for all units of interest.

private

   type C_Source_Rewriter is limited new Ada.Finalization.Limited_Controlled
   with record
      CIdx     : Index_T;
      TU       : Translation_Unit_T;
      Rewriter : Rewriter_T;
      --  Structures that should be freed after rewriting

      Output_Filename : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding procedure Finalize (Self : in out C_Source_Rewriter);

end Instrument.C;
