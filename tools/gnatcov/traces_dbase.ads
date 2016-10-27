------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Containers.Ordered_Sets;
with Interfaces; use Interfaces;
with Traces;     use Traces;

--  This unit manage flat traces for object coverage only

package Traces_Dbase is

   type Traces_Base is limited private;
   type Traces_Base_Acc is access Traces_Base;

   procedure Init_Base (Base : out Traces_Base);
   --  Initialize Base

   procedure Add_Entry
     (Base  : in out Traces_Base;
      First : Pc_Type;
      Last  : Pc_Type;
      Op    : Unsigned_8);
   --  Add a trace entry in the ordered_Set. May discard useless entries
   --  or merge entries.
   --  Needs further description of conditions causing entries to be discarded
   --  or merged???
   --  Doesn't support more than one trace entry with the same First and Last
   --  PC, which means this works only for stateless coverage (not MC/DC)???

   procedure Dump_Traces (Base : Traces_Base);
   --  Dump (on standard output) the contents of Base

   --  Return a trace that contains or follows ADDR.
   --  What does this comment apply to? There is no mention of "ADDR" anywhere
   --  in sight??

   procedure Iterate (Base    : Traces_Base;
                      Process : not null access procedure (E : Trace_Entry));
   --  Call Process for each entry in the Traces_Base

   type Entry_Iterator is limited private;

   procedure Init
     (Base     : Traces_Base;
      Iterator : out Entry_Iterator;
      Pc       : Pc_Type);
   --  Return an iterator that points to the first element before Pc

   procedure Init_Post
     (Base     : Traces_Base;
      Iterator : out Entry_Iterator;
      Pc       : Pc_Type);
   --  Return an iterator that points to the first element which contains Pc
   --  in its range, or (if there is no such element in Base) to the first
   --  element after Pc.

   procedure Get_Next_Trace
     (Trace    : out Trace_Entry;
      Iterator : in out Entry_Iterator);
   --  Return the next entry from Iterator, or Bad_Trace if none is left

   procedure Update_State
     (Base     : in out Traces_Base;
      Iterator : Entry_Iterator;
      State    : Insn_State);
   --  Comments needed???

   procedure Split_Trace
     (Base       : in out Traces_Base;
      Iterator   : in out Entry_Iterator;
      Pc         : Pc_Type;
      Head_State : Insn_State);
   --  Split current trace in two parts:
   --    Head part from original trace start to Pc with state Head_State
   --    Tail part from Pc+1 to original trace end with unchanged state

private

   --  Operations for Ordered_Sets

   function "=" (L, R : Trace_Entry) return Boolean;
   function "<" (L, R : Trace_Entry) return Boolean;

   package Entry_Set is
     new Ada.Containers.Ordered_Sets (Element_Type => Trace_Entry);

   type Traces_Base is record
      Entries : Entry_Set.Set;
      --  Contents of the traces database
   end record;

   type Entry_Iterator is record
      Cur : Entry_Set.Cursor;
   end record;

end Traces_Dbase;
