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
with Outputs;      use Outputs;

package body Traces_Dbase is

   Trace_Op_Any_Br   : constant Unsigned_8 := Trace_Op_Br0 or Trace_Op_Br1;

   use Entry_Set;

   function Get_Trace_Cur
     (Base     : Traces_Base;
      Iterator : Entry_Iterator) return Cursor;
   --  Comment needed???

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Trace_Entry) return Boolean is
   begin
      --  Overlap and same serial

      --  This relocation is reflexive and symmetric

      return L.Serial = R.Serial
        and then L.First <= R.Last
        and then L.Last >= R.First;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Trace_Entry) return Boolean is
   begin
      if L.Serial < R.Serial then
         return True;

      elsif L.Serial > R.Serial then
         return False;

      else
         --  Disjoint and inferior

         return L.Last < R.First;
      end if;
   end "<";

   ---------------
   -- Init_Base --
   ---------------

   procedure Init_Base
     (Base     : out Traces_Base;
      Full_History : Boolean)
   is
   begin
      Base.Entries.Clear;
      if Full_History then
         Base.Next_Serial := 0;
      else
         Base.Next_Serial := -1;
      end if;
   end Init_Base;

   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry
     (Base  : in out Traces_Base;
      First : Pc_Type;
      Last  : Pc_Type;
      Op    : Unsigned_8)
   is
      Cur : Cursor;
      Merged_Op : Unsigned_8;
      Success : Boolean;
      New_Entry : constant Trace_Entry :=
                    (First  => First,
                     Last   => Last,
                     Serial => Base.Next_Serial,
                     Op     => Op,
                     State  => Unknown);
   begin
      --  Discard empty traces with a warning

      if Last < First then
         Error ("empty trace entry discarded:");
         Dump_Entry (New_Entry);
         return;
      end if;

      --  Increment trace serial number if full history is kept

      if Base.Next_Serial /= -1 then
         Base.Next_Serial := Base.Next_Serial + 1;
      end if;

      --  Try to insert

      Base.Entries.Insert (New_Entry, Cur, Success);
      if Success then
         return;
      end if;

      --  Handle conflicts (case of flat traces only)

      declare
         N_First, N_Last : Pc_Type;
         E : constant Trace_Entry := Element (Cur);
      begin
         --  Prepare merge:
         --   Handle conditionnal dynamic branches.
         --   They are a bit special as only the fallback has a Trace_Op_Br bit
         --   (and this is supposed to be Trace_Op_Br1).  Standard merging will
         --   discard the taken branch.

         --  When we merge an op that has a Br bit with one that has no Br bit,
         --  we assume that both ways were taken.

         Merged_Op := Op or E.Op;

         if (E.Op and Trace_Op_Any_Br) = 0
              xor
            (Op and Trace_Op_Any_Br) = 0
         then
            Merged_Op := Merged_Op or Trace_Op_Br0 or Trace_Op_Br1;
         end if;

         if (Op and Trace_Op_Block) = 0 then
            --  Just merge flags

            if First /= Last then
               raise Program_Error;
            end if;
            Base.Entries.Replace_Element
              (Cur,
               Trace_Entry'(E.First, E.Last, E.Serial, Merged_Op, E.State));

         else
            --  Merge

            --  First add entries for before and after E

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

            --  Then merge with E

            if E.First < N_First then

               --  Split

               Base.Entries.Replace_Element
                 (Cur,
                  Trace_Entry'(E.First, N_First - 1, E.Serial, E.Op, E.State));
               Base.Entries.Insert
                 (Trace_Entry'(N_First, N_Last, E.Serial, Merged_Op, E.State));

               if E.Last > N_Last then
                  Base.Entries.Insert
                    (Trace_Entry'(First  => N_Last + 1,
                                  Last   => E.Last,
                                  Serial => E.Serial,
                                  Op     => E.Op,
                                  State  => E.State));
               end if;

            elsif E.Last > N_Last then
               pragma Assert (E.First = N_First);

               Base.Entries.Replace_Element
                 (Cur,
                  Trace_Entry'(N_First, N_Last, E.Serial, Merged_Op, E.State));

               Base.Entries.Insert
                 (Trace_Entry'(N_Last + 1, E.Last, E.Serial, E.Op, E.State));
            else
               pragma Assert (N_First = E.First);
               pragma Assert (N_Last = E.Last);

               Base.Entries.Replace_Element
                 (Cur,
                  Trace_Entry'(N_First, N_Last, E.Serial, Merged_Op, E.State));
            end if;
         end if;
      end;
   end Add_Entry;

   -----------------
   -- Dump_Traces --
   -----------------

   procedure Dump_Traces (Base : Traces_Base) is
      procedure Dump_Entry (Cur : Cursor);
      --  Dump one entry

      ----------------
      -- Dump_Entry --
      ----------------

      procedure Dump_Entry (Cur : Cursor) is
      begin
         Dump_Entry (Element (Cur));
      end Dump_Entry;

   --  Start of processing for Dump_Traces

   begin
      Base.Entries.Iterate (Dump_Entry'Access);
   end Dump_Traces;

   --------------------
   -- Get_Next_Trace --
   --------------------

   procedure Get_Next_Trace
     (Trace    : out Trace_Entry;
      Iterator : in out Entry_Iterator)
   is
   begin
      if Iterator.Cur = No_Element then
         Trace := Bad_Trace;
      else
         Trace := Element (Iterator.Cur);
         Next (Iterator.Cur);
      end if;
   end Get_Next_Trace;

   -------------------
   -- Get_Trace_Cur --
   -------------------

   function Get_Trace_Cur
     (Base     : Traces_Base;
      Iterator : Entry_Iterator) return Cursor
   is
   begin
      if Iterator.Cur = No_Element then
         return Base.Entries.Last;
      else
         return Previous (Iterator.Cur);
      end if;
   end Get_Trace_Cur;

   ----------
   -- Init --
   ----------

   procedure Init
     (Base     : Traces_Base;
      Iterator : out Entry_Iterator;
      Pc       : Pc_Type)
   is
      Key : constant Trace_Entry :=
              (First  => Pc,
               Last   => Pc,
               Serial => -1,
               Op     => 0,
               State  => Unknown);
   begin
      Iterator := (Cur => Base.Entries.Floor (Key));
      if Iterator.Cur = No_Element then
         Iterator.Cur := Base.Entries.First;
      end if;
   end Init;

   -----------------
   -- Split_Trace --
   -----------------

   procedure Split_Trace
     (Base       : in out Traces_Base;
      Iterator   : in out Entry_Iterator;
      Pc         : Pc_Type;
      Head_State : Insn_State)
   is
      Cur : Cursor;
      Head_Trace, Tail_Trace : Trace_Entry;
   begin
      Cur := Get_Trace_Cur (Base, Iterator);
      Tail_Trace := Element (Cur);
      Head_Trace := Tail_Trace;

      --  Replace current trace with tail

      Tail_Trace.First := Pc + 1;
      Base.Entries.Replace_Element (Cur, Tail_Trace);

      --  Now insert new trace for head with the given state

      Head_Trace.Last := Pc;
      Head_Trace.State := Head_State;
      Base.Entries.Insert (Head_Trace);
   end Split_Trace;

   ------------------
   -- Update_State --
   ------------------

   procedure Update_State
     (Base     : in out Traces_Base;
      Iterator : Entry_Iterator;
      State    : Insn_State)
   is
      Cur : Cursor;
      Trace : Trace_Entry;
   begin
      Cur := Get_Trace_Cur (Base, Iterator);
      Trace := Element (Cur);
      Trace.State := State;
      Base.Entries.Replace_Element (Cur, Trace);
   end Update_State;

end Traces_Dbase;
