------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2021, AdaCore                     --
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

--  Instrumentation of an Ada source file

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text; use Langkit_Support.Text;
with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Rewriting; use Libadalang.Rewriting;

with Instrument.Common; use Instrument.Common;

package Instrument.Ada_Unit is

   procedure Instrument_Unit
     (CU_Name   : Compilation_Unit_Name;
      IC        : in out Inst_Context;
      Unit_Info : in out Instrumented_Unit_Info);
   --  Instrument a single source file of interest from the project

   procedure Add_Auto_Dump_Buffers
     (IC   : Inst_Context;
      Info : in out Project_Info;
      Main : Ada_Qualified_Name;
      URH  : Unit_Rewriting_Handle)
     with Pre => IC.Dump_Config.Trigger /= Manual;
   --  Try to insert in the sources of Main (a main subprogram) a call to dump
   --  the list of coverage buffers for all units of interest in Main's
   --  closure. Return without doing anything if unsuccessful.
   --
   --  Info must be the project that owns the Main unit, and URH must be a
   --  rewriting handle for the body unit that contains Main's sources.

   --  Private declarations relative to the AST traversal
private
   --  Insertion_Info defines the current state for traversal of a list of
   --  statements or declarations in which witness calls are inserted.

   type Insertion_Info;
   type Insertion_Info_Access is access all Insertion_Info;

   type Insertion_Method is
     (None, Statement, Declaration, Expression_Function);
   --  Describe how to insert a call to a witness subprogram:
   --
   --  * None: no insertion method yet.
   --
   --  * Statement: insert a witness call statement.
   --
   --  * Declaration: insert a dummy declaration whose initialization
   --    expression is a witness call.
   --
   --  * Expression_Function: wrap the expression in an expression function
   --    in a case expression whose controlling expr is a witness call.

   type Insertion_Info (Method : Insertion_Method := None) is record

      case Method is
         when Statement | Declaration =>
            RH_List : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
            --  Rewriting handle for the statement/declaration list

            Index : Natural := 0;
            --  Index of the element in RH_List being traversed

            Rewriting_Offset : Integer := 0;
            --  Count of nodes inserted/removed in current list so far

            Preelab : Boolean := False;
            --  Whether we are traversing a list of top-level declarations in a
            --  preelaborate package. In this context, we cannot insert witness
            --  calls, precisely because of the preelaborate restriction.

            Parent : Insertion_Info_Access;
            --  Insertion_Info for the upper enclosing list

            case Method is
               when Declaration =>
                  RH_Private_List : Node_Rewriting_Handle :=
                    No_Node_Rewriting_Handle;
                  --  If RH_List is the declarations of a public part, and
                  --  there is a corresponding private part, declarations
                  --  list of the private part.

               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;
   end record;

   type Root_MCDC_State_Inserter is abstract tagged null record;
   --  Abstract interface for a mechanism that allows the insertion of
   --  MC/DC state variables in a given context.

   type Any_MCDC_State_Inserter is access all Root_MCDC_State_Inserter'Class;

   package FQN_Sets is
     new Ada.Containers.Indefinite_Hashed_Sets
       (Text_Type, Ada.Strings.Wide_Wide_Hash, "=");
   --  Hashed set of fully qualified names (stored in normalized form:
   --  lower case, period separated, fully qualified).

   --  Generic_Subp defines the declaration and body for a generic subprogram
   --  that are produced during the tree traversal for the instrumentation
   --  of degenerate subprograms (null procedures and expression functions).

   type Generic_Subp is record
      Generic_Subp_Decl, Generic_Subp_Body : Unbounded_Wide_Wide_String;
   end record;

   package Generic_Subp_Vectors is
     new Ada.Containers.Vectors (Natural, Generic_Subp);

   type Ada_Unit_Inst_Context is new Instrument.Common.Unit_Inst_Context with
      record
         Root_Unit : Compilation_Unit;
         --  Node of compilation unit

         Withed_Units : FQN_Sets.Set;
         --  Set of units for which we have WITH clauses

         Rewriting_Context : Rewriting_Handle;
         --  Rewriting handle for the instrumentation process

         MCDC_State_Inserter : Any_MCDC_State_Inserter;
         --  Service supporting insertion of temporary MC/DC state variables

         Current_Insertion_Info : Insertion_Info_Access;
         --  Insertion_Info for the list being traversed

         Degenerate_Subprogram_Generics : Generic_Subp_Vectors.Vector;
         --  Generics to be generated in the pure buffers unit to support
         --  instrumentation of degenerate subprograms.

         Degenerate_Subprogram_Index : Natural := 0;
         --  Index of last processed degenerate subprogram (null procedure or
         --  expression function) in current unit. This is used to assign
         --  unique names for generated constructs.
      end record;

   function Insert_MCDC_State
     (Inserter : in out Root_MCDC_State_Inserter;
      UIC      : in out Ada_Unit_Inst_Context'Class;
      Name     : String) return String
      is abstract;
   --  Create and insert the declaration of an MC/DC state temporary object.
   --  Returns a System.Address expression denoting the declared state, for
   --  use in witness calls.

end Instrument.Ada_Unit;
