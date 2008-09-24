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
      return L.First = R.First and L.Last = R.Last;
   end "=";

   function "<" (L, R : Trace_Entry) return Boolean is
   begin
      return L.First < R.First;
   end "<";

   use Entry_Set;

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
         E.Op := E.Op or Op;
         Replace_Element (Base, Cur, E);
      end if;
   end Add_Entry;

   --  Walk the set and try to merge entries.
   procedure Merge_Entries (Base : in out Traces_Base)
   is
      Cur : Cursor;
      N_Cur : Cursor;
      E : Trace_Entry;

      --  Merged entry.
      M : Trace_Entry;
      M_Pos : Cursor;
      Modified : Boolean;
   begin
      M_Pos := First (Base);
      if M_Pos = No_Element then
         return;
      end if;
      M := Element (M_Pos);
      Modified := False;

      Cur := Next (M_Pos);

      while Cur /= No_Element loop
         E := Element (Cur);
         N_Cur := Next (Cur);

         --  If two traces are consecutive and the first one does not
         --  terminate with a jump, merge the traces.
         if E.First = M.Last + 1
           and then M.Op = Trace_Op_Block
           and then (E.Op and Trace_Op_Block) /= 0
         then
            M.Last := E.Last;
            M.Op := M.Op or E.Op;
            Modified := True;
            Delete (Base, Cur);

         --  Merge two entries.
         elsif E.First >= M.First
           and then E.Last = M.Last
         then
            M.Op := M.Op or E.Op;
            Modified := True;
            Delete (Base, Cur);

         else
            if Modified then
               Modified := False;
               Replace_Element (Base, M_Pos, M);
            end if;
            M := E;
            M_Pos := Cur;
         end if;

         Cur := N_Cur;
      end loop;
   end Merge_Entries;

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
      if Iterator.Cur = No_Element then
         Cur := Last (Base);
      else
         Cur := Previous (Iterator.Cur);
      end if;
      Trace := Element (Cur);
      Trace.State := State;
      Replace_Element (Base, Cur, Trace);
   end Update_State;

   procedure Split_Trace (Base : in out Traces_Base;
			  Iterator : in out Entry_Iterator;
                          Pc : Pc_Type;
                          Cur_State, Next_State : Trace_State)
   is
      Cur : Cursor;
      Trace, Next_Trace : Trace_Entry;
   begin
      if Iterator.Cur = No_Element then
         Cur := Last (Base);
      else
         Cur := Previous (Iterator.Cur);
      end if;
      Trace := Element (Cur);
      Next_Trace := Trace;
      Trace.State := Cur_State;
      Trace.Last := Pc;
      Replace_Element (Base, Cur, Trace);
      Next_Trace.First := Pc + 1;
      Next_Trace.State := Next_State;
      Insert (Base, Next_Trace);
   end Split_Trace;
end Traces_Dbase;
