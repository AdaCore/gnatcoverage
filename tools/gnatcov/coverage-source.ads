------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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

with Files_Table;    use Files_Table;
with Traces;         use Traces;
with Traces_Names;   use Traces_Names;
with Traces_Lines;   use Traces_Lines;
with SC_Obligations; use SC_Obligations;

package Coverage.Source is

   procedure Initialize_SCI;
   --  Initialize source coverage information vector once SCOs have been loaded

   procedure Compute_Source_Coverage
     (Subp_Key  : Subprogram_Key;
      Subp_Info : Subprogram_Info;
      T         : Trace_Entry);
   --  Analyze traces for the given subprogram to determine the coverage state
   --  of each SCO.

   procedure Compute_Line_State
     (Line_Num  : Positive;
      Line_Info : Line_Info_Access);
   --  Set Line.State based on coverage information accumulated on all SCOs
   --  that cover the given line.

   subtype SCO_State is Line_State range Not_Covered .. No_Code;
   function Get_Line_State
     (SCO   : SCO_Id;
      Level : Coverage_Level) return SCO_State;
   --  Return SCO's contribution to the state of the enclosing line, i.e.
   --  SCO's specific coverage state, ignoring any exemptions. This coverage
   --  is cumulative over all SCIs for this SCO, for the case of a SCO that
   --  has multiple tags (i.e. multiple, distinct coverage analyses).

   function Basic_Block_Has_Code (SCO : SCO_Id; Tag : SC_Tag) return Boolean;
   --  True if any SCO in basic block has associated object code with then
   --  given tag.

   procedure Set_Basic_Block_Has_Code (SCO : SCO_Id; Tag : SC_Tag);
   --  Set Basic_Block_Has_Code for SCO as well as all previous SCOs in its
   --  basic block, for the given tag.

end Coverage.Source;
