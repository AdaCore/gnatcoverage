------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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
      --  This relocation is reflexive and symmetric.
      return L.First <= R.Last and L.Last >= R.First;
   end "=";

   function "<" (L, R : Trace_Entry) return Boolean is
   begin
      --  Disjoint and inferior.
      --  This relation is irreflexive, asymmetric and transitive
      --   (provided A.First < A.Last).
      return L.Last < R.First;
   end "<";

   use Entry_Set;

   function Get_Trace_Cur (Base : Traces_Base;
                           Iterator : Entry_Iterator) return Cursor;

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
   begin
      --  Discard fault.
      if (Op and Trace_Op_Fault) /= 0 then
         return;
      end if;

      --  Try to insert.
      Insert (Base,
              Trace_Entry'(First, Last, Op, Unknown),
              Cur, Status);

      if Status then
         return;
      end if;

      declare
         N_First, N_Last : Pc_Type;
         E : constant Trace_Entry := Element (Cur);
      begin
         if (Op and Trace_Op_Block) = 0 then
            --  Just merge flags.
            if First /= Last then
               raise Program_Error;
            end if;
            Replace_Element (Base, Cur, Trace_Entry'(E.First, E.Last,
                                                     Op or E.Op, E.State));
         else
            --  Merge
            --  First add entries for before and after E.
            if First < E.First then
               Add_Entry (Base, First, E.First - 1, Op);
               N_First := E.First;
            else
               N_First := First;
            end if;

            if Last > E.Last then
               Add_Entry (Base, E.Last + 1, Last, Op);
               N_Last := E.Last;
            else
               N_Last := Last;
            end if;

            --  Then merge with E.
            if E.First < N_First then
               --  Split.
               Replace_Element (Base, Cur, Trace_Entry'(E.First, N_First - 1,
                                                        E.Op, E.State));
               Insert (Base,
                       Trace_Entry'(N_First, N_Last, E.Op or Op, E.State));
               if E.Last > N_Last then
                  Insert (Base,
                          Trace_Entry'(N_Last + 1, E.Last, E.Op, E.State));
               end if;
            elsif E.Last > N_Last then
               pragma Assert (E.First = N_First);
               Replace_Element (Base, Cur, Trace_Entry'(N_First, N_Last,
                                                        Op or E.Op, E.State));
               Insert (Base,
                       Trace_Entry'(N_Last + 1, E.Last, E.Op, E.State));
            else
               pragma Assert (N_First = E.First);
               pragma Assert (N_Last = E.Last);
               Replace_Element (Base, Cur, Trace_Entry'(N_First, N_Last,
                                                        Op or E.Op, E.State));
            end if;
         end if;
      end;
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
