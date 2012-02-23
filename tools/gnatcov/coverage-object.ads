------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

with Traces;       use Traces;
with Traces_Lines; use Traces_Lines;
with Files_Table;  use Files_Table;

package Coverage.Object is

   subtype Known_Insn_State is
     Insn_State range Not_Covered .. Insn_State'Last;

   procedure Update_Line_State
     (L : in out Line_State;
      I : Known_Insn_State);
   --  Update a source line state with the object coverage status of one of its
   --  instructions (for reporting of object coverage as source annotations).

   function Get_Line_State
     (Base  : Traces_Base;
      First : Pc_Type;
      Last  : Pc_Type) return Line_State;
   --  Extract (from Base) the object coverage of the instruction set in
   --  range First .. Last.

   procedure Compute_Line_State (Line : Line_Info_Access);
   --  Set Line.State
end Coverage.Object;
