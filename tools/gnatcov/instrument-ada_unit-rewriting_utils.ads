------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2025, AdaCore                     --
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

with Libadalang.Common; use Libadalang.Common;

private package Instrument.Ada_Unit.Rewriting_Utils is

   --  This unit is meant to abstract the AST handling from the logic.
   --  Ideally, we should follow guidelines to make sure it does not become
   --  unmanageable in the future:
   --
   --  1. All subprograms should either be named `Make_...` or `Wrap_In_...`
   --     (this can evolve if needed).
   --  2. The first argument of each subprogram should be the Rewriting_Handle.
   --  3. `Make_...` subprograms should be FUNCTIONS that return a Node.
   --  4. `Wrap_In_...` subprograms should be PROCEDURES to which we pass an
   --     "in out Node" argument.

   function Make_Identifier
     (RH : Rewriting_Handle; Id : Text_Type) return Node_Rewriting_Handle
   is (Create_Token_Node (RH, Libadalang.Common.Ada_Identifier, Id));
   --  Shortcut to create an identifier node

   procedure Wrap_In_Call_Expr
     (RH     : Rewriting_Handle;
      Prefix : Text_Type;
      Node   : in out Node_Rewriting_Handle);
   --  Change "Node" into "Prefix(Node)"

   procedure Wrap_In_Parentheses
     (RH     : Rewriting_Handle;
      Node   : in out Node_Rewriting_Handle);
   --  Change "Node" into "(Node)"

   procedure Wrap_In_If_Expr
     (RH     : Rewriting_Handle;
      Node   : in out Node_Rewriting_Handle);
   --  Change "Node" into "(if Node then True else False)"

end Instrument.Ada_Unit.Rewriting_Utils;
