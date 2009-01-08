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
with Qemu_Traces; use Qemu_Traces;

package body Traces_Dbase is

   function "=" (L, R : Trace_Entry) return Boolean is
   begin
      --  Overlap.
      return L.First <= R.Last and L.Last >= R.First;
   end "=";

   function "<" (L, R : Trace_Entry) return Boolean is
   begin
      --  Disjoint and inferior.
      return L.Last < R.First;
   end "<";

   use Entry_Set;

   procedure Init_Base (Base : in out Traces_Base) is
   begin
      Base.Clear;
   end Init_Base;

   --  Add a trace entry in the ordered_Set.  May discard useless entries
   --  or merge entries.
   procedure Add_Entry (Base : in out Traces_Base;
                        First : Pc_Type; Last : Pc_Type; Op : Unsigned_8)
   is
      Cur : Cursor;
      Status : Boolean;
      E : Trace_Entry;
   begin
      --  Discard fault.
      if (Op and Trace_Op_Fault) /= 0 then
         return;
      end if;

      Insert (Base,
              Trace_Entry'(First, Last, Op, Unknown),
              Cur, Status);
      if Status = False then
         --  Merge
         E := Element (Cur);
         if E.First > First then
            E.First := First;
         end if;
         E.Op := E.Op or Op;
         Replace_Element (Base, Cur, E);
      end if;
   end Add_Entry;

   procedure Dump_Traces (Base : Traces_Base)
   is
      Cur : Cursor := First (Base);
   begin
      while Cur /= No_Element loop
         Dump_Entry (Element (Cur));
         Next (Cur);
      end loop;
   end Dump_Traces;

   procedure Init (Base : Traces_Base;
                   Iterator : out Entry_Iterator; Pc : Pc_Type)
   is
      Key : constant Trace_Entry := (Pc, Pc, 0, Unknown);
   begin
      Iterator := (Cur => Floor (Base, Key));
      if Iterator.Cur = No_Element then
         Iterator.Cur := First (Base);
      end if;
   end Init;

   --  Get the cursor for the current trace.
   function Get_Trace_Cur (Base : Traces_Base;
                           Iterator : Entry_Iterator) return Cursor is
   begin
      if Iterator.Cur = No_Element then
         return Last (Base);
      else
         return Previous (Iterator.Cur);
      end if;
   end Get_Trace_Cur;

   procedure Get_Next_Trace (Trace : out Trace_Entry;
                             Iterator : in out Entry_Iterator) is
   begin
      if Iterator.Cur = No_Element then
         Trace := Bad_Trace;
      else
         Trace := Element (Iterator.Cur);
         Next (Iterator.Cur);
      end if;
   end Get_Next_Trace;

   procedure Update_State (Base : in out Traces_Base;
                           Iterator : Entry_Iterator; State : Trace_State)
   is
      Cur : Cursor;
      Trace : Trace_Entry;
   begin
      Cur := Get_Trace_Cur (Base, Iterator);
      Trace := Element (Cur);
      Trace.State := State;
      Replace_Element (Base, Cur, Trace);
   end Update_State;

   procedure Split_Trace (Base : in out Traces_Base;
                          Iterator : in out Entry_Iterator;
                          Pc : Pc_Type;
                          Prev_State : Trace_State)
   is
      Cur : Cursor;
      Trace, Prev_Trace : Trace_Entry;
   begin
      Cur := Get_Trace_Cur (Base, Iterator);
      Trace := Element (Cur);
      Prev_Trace := Trace;

      --  First modify the element so that Prev_Trace can be inserted
      --  without violating the no-duplicate elements rule.
      Trace.First := Pc + 1;
      Replace_Element (Base, Cur, Trace);

      Prev_Trace.State := Prev_State;
      Prev_Trace.Last := Pc;
      Insert (Base, Prev_Trace);
   end Split_Trace;
end Traces_Dbase;
