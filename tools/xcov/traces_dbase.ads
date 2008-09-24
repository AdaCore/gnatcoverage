------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Containers.Ordered_Sets;
with Interfaces; use Interfaces;
with Traces; use Traces;

package Traces_Dbase is
   type Traces_Base is limited private;
   
   type Traces_Base_Acc is access Traces_Base;
   
   --  Add a trace entry in the ordered_Set.  May discard useless entries
   --  or merge entries.
   procedure Add_Entry (Base : in out Traces_Base;
			First : Pc_Type; Last : Pc_Type; Op : Unsigned_8);
   
   --  Walk the set and try to merge entries.
   procedure Merge_Entries (Base : in out Traces_Base);

   --  Dump (on standard output) the content of traces.
   procedure Dump_Traces (Base : Traces_Base);

   --  Return a trace that contains or follows ADDR.
   type Entry_Iterator is limited private;
   procedure Init (Base : Traces_Base;
		   Iterator : out Entry_Iterator; Pc : Pc_Type);
   procedure Get_Next_Trace (Trace : out Trace_Entry;
                             Iterator : in out Entry_Iterator);

   procedure Update_State (Base : in out Traces_Base;
			   Iterator : Entry_Iterator; State : Trace_State);
   procedure Split_Trace (Base : in out Traces_Base;
			  Iterator : in out Entry_Iterator;
                          Pc : Pc_Type;
                          Cur_State, Next_State : Trace_State);

private
   --  Operations for ordered_sets.
   function "=" (L, R : Trace_Entry) return Boolean;
   function "<" (L, R : Trace_Entry) return Boolean;

   package Entry_Set is new Ada.Containers.Ordered_Sets
     (Element_Type => Trace_Entry,
      "<" => "<",
      "=" => "=");
   
   type Traces_Base is new Entry_Set.Set with null record;
   
   type Entry_Iterator is record
      Cur : Entry_Set.Cursor;
   end record;
end Traces_Dbase;
