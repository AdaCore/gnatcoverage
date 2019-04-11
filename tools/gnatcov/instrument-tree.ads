------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

--  Tree traversal for instrumentation and SCO generation (adapted from Par_SCO
--  compiler unit).

with Libadalang.Analysis;  use Libadalang.Analysis;

with Instrument.Sources; use Instrument.Sources;
with Text_Files;

private package Instrument.Tree is

   type Dominant_Info is private;
   No_Dominant : constant Dominant_Info;

   procedure Traverse_Declarations_Or_Statements
     (IC      : in out Unit_Inst_Context;
      L       : Ada_Node_List;
      Preelab : Boolean       := False;
      D       : Dominant_Info := No_Dominant;
      P       : Ada_Node      := No_Ada_Node);
   --  Process L, a list of statements or declarations dominated by D. If P is
   --  present, it is processed as though it had been prepended to L. Preelab
   --  is True if L is a list of preelaborable declarations (which do not
   --  allow elaboration code, so do not require any SCOs, and wouldn't allow
   --  insertion of witnesses).

private

   type Dominant_Info is record
      K : Character;
      --  F/T/S/E for a valid dominance marker, or ' ' for no dominant

      N : Ada_Node;
      --  Node providing the Sloc(s) for the dominance marker
   end record;
   No_Dominant : constant Dominant_Info := (' ', No_Ada_Node);

end Instrument.Tree;
