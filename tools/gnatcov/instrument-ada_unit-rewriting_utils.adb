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

package body Instrument.Ada_Unit.Rewriting_Utils is

   -----------------------
   -- Wrap_In_Call_Expr --
   -----------------------

   procedure Wrap_In_Call_Expr
     (RH     : Rewriting_Handle;
      Prefix : Text_Type;
      Node   : in out Node_Rewriting_Handle) is
   begin
      Node :=
        Create_Call_Expr
          (RH,
           F_Name   => Make_Identifier (RH, Prefix),
           F_Suffix =>
             Create_Regular_Node
               (RH,
                Ada_Assoc_List,
                (1 =>
                   Create_Param_Assoc
                     (RH,
                      F_Designator => No_Node_Rewriting_Handle,
                      F_R_Expr     => Node))));
   end Wrap_In_Call_Expr;

   -------------------------
   -- Wrap_In_Parentheses --
   -------------------------

   procedure Wrap_In_Parentheses
     (RH : Rewriting_Handle; Node : in out Node_Rewriting_Handle) is
   begin
      Node := Create_Paren_Expr (RH, F_Expr => Node);
   end Wrap_In_Parentheses;

   ---------------------
   -- Wrap_In_If_Expr --
   ---------------------

   procedure Wrap_In_If_Expr
     (RH : Rewriting_Handle; Node : in out Node_Rewriting_Handle) is
   begin
      Node :=
        Create_If_Expr
          (RH,
           F_Cond_Expr    => Node,
           F_Then_Expr    => Make_Identifier (RH, To_Text ("True")),
           F_Alternatives => No_Node_Rewriting_Handle,
           F_Else_Expr    => Make_Identifier (RH, To_Text ("False")));

      Wrap_In_Parentheses (RH, Node);
   end Wrap_In_If_Expr;

end Instrument.Ada_Unit.Rewriting_Utils;
