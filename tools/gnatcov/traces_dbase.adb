------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Qemu_Traces; use Qemu_Traces;
with Outputs;      use Outputs;

package body Traces_Dbase is

   use Entry_Set;

   function Get_Trace_Cur
     (Base     : Traces_Base;
      Iterator : Entry_Iterator) return Cursor;
   --  Comment needed???

   ---------------
   -- Init_Base --
   ---------------

   procedure Init_Base (Base : out Traces_Base) is
   begin
      Base.Entries.Clear;
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
                     Op     => Op,
                     State  => Unknown);
   begin
      --  Discard empty traces with a warning. Empty traces may be generated
      --  for fault (when a fault happens on a block that has already been
      --  covered). On other cases, empty traces should be considered as bogus
      --  and a warning should be displayed.

      if Last < First then
         if Op /= Trace_Op_Fault then
            Error ("empty trace entry discarded:");
            Dump_Entry (New_Entry);
         end if;

         return;
      end if;

      --  Try to insert

      Base.Entries.Insert (New_Entry, Cur, Success);
      if Success then
         return;
      end if;

      --  Handle conflicts

      declare
         N_First, N_Last : Pc_Type;
         E : constant Trace_Entry := Element (Cur);
      begin
         Merged_Op := Op or E.Op;

         if (Op and Trace_Op_Block) = 0
           and then (Op and Trace_Op_Fault) = 0
         then
            --  Just merge flags

            Base.Entries.Replace_Element
              (Cur,
               Trace_Entry'(E.First, E.Last, Merged_Op, E.State));

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
                  Trace_Entry'(E.First, N_First - 1, E.Op, E.State));
               Base.Entries.Insert
                 (Trace_Entry'(N_First, N_Last, Merged_Op, E.State));

               if E.Last > N_Last then
                  Base.Entries.Insert
                    (Trace_Entry'(First  => N_Last + 1,
                                  Last   => E.Last,
                                  Op     => E.Op,
                                  State  => E.State));
               end if;

            elsif E.Last > N_Last then
               pragma Assert (E.First = N_First);

               Base.Entries.Replace_Element
                 (Cur,
                  Trace_Entry'(N_First, N_Last, Merged_Op, E.State));

               Base.Entries.Insert
                 (Trace_Entry'(N_Last + 1, E.Last, E.Op, E.State));
            else
               pragma Assert (N_First = E.First);
               pragma Assert (N_Last = E.Last);

               Base.Entries.Replace_Element
                 (Cur,
                  Trace_Entry'(N_First, N_Last, Merged_Op, E.State));
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

   -------------
   -- Iterate --
   -------------

   procedure Iterate (Base    : Traces_Base;
                      Process : not null access procedure (E : Trace_Entry))
   is
      procedure Entry_Callback (Cur : Cursor);

      --------------------
      -- Entry_Callback --
      --------------------

      procedure Entry_Callback (Cur : Cursor) is
      begin
         Process (Element (Cur));
      end Entry_Callback;

   begin
      Base.Entries.Iterate (Entry_Callback'Access);
   end Iterate;

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
               Op     => 0,
               State  => Unknown);
   begin
      Iterator := (Cur => Base.Entries.Floor (Key));
      if Iterator.Cur = No_Element then
         Iterator.Cur := Base.Entries.First;
      end if;
   end Init;

   ---------------
   -- Init_Post --
   ---------------

   procedure Init_Post
     (Base     : Traces_Base;
      Iterator : out Entry_Iterator;
      Pc       : Pc_Type)
   is
      Trace : Trace_Entry;
   begin
      Init (Base, Iterator, Pc);

      if Iterator.Cur = No_Element then
         return;
      end if;

      Trace := Element (Iterator.Cur);
      while Trace /= Bad_Trace and then Trace.Last < Pc loop
         Next (Iterator.Cur);

         if Iterator.Cur = No_Element then
            return;
         end if;

         Trace := Element (Iterator.Cur);
      end loop;
   end Init_Post;

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
