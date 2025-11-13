------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
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
with Interfaces;

package body Coverage.Object is

   ------------------------
   -- Compute_Line_State --
   ------------------------

   procedure Compute_Line_State (Line : Line_Info_Access) is
      State : Line_State := No_Code;
   begin
      if Line.Obj_Infos /= null then
         for El of Line.Obj_Infos.all loop
            State := State * El.State;
         end loop;
      end if;
      if Enabled (Branch) then
         Line.State (Coverage_Level_To_Cell (Branch)) := State;
      else
         Line.State (Coverage_Level_To_Cell (Insn)) := State;
      end if;
   end Compute_Line_State;

   type Address_Range is record
      First, Last : Pc_Type;
   end record;

   function "<" (Left, Right : Address_Range) return Boolean;
   --  Lexicographical order

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Address_Range) return Boolean is
      use Interfaces;
   begin
      if Left.First = Right.First then
         return Left.Last < Right.Last;
      else
         return Left.First < Right.First;
      end if;
   end "<";

   package Address_Range_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Address_Range);

   function PC_Range_Covered
     (Ranges : Address_Range_Sets.Set; First, Last : Pc_Type) return Boolean;
   --  Returns whether for every address in First .. Last, there is one
   --  Address_Range of Ranges that contains it.

   ----------------------
   -- PC_Range_Covered --
   ----------------------

   function PC_Range_Covered
     (Ranges : Address_Range_Sets.Set; First, Last : Pc_Type) return Boolean
   is
      use Interfaces;
      use Address_Range_Sets;
      use Ada.Containers;

      Cur           : Cursor;
      Current_Range : Address_Range;
   begin
      if Ranges.Is_Empty then
         return False;
      elsif Ranges.Length = 1 then

         --  If there is only one trace entry, simply check that it covers the
         --  entire range of interest.

         return
           Ranges.First_Element.First <= First
           and then Ranges.First_Element.Last >= Last;
      else

         --  Ranges are sorted. Quick check on the extremes first.

         if First < Ranges.First_Element.First
           or else Last > Ranges.Last_Element.Last
         then
            return False;
         end if;

         --  Check that every PC in the range of interest is covered by one of
         --  the trace entries.

         Cur := Ranges.First;

         for PC in First .. Last loop

            --  See if we can find a range for the current PC. Ranges
            --  previously deemed invalid for a PC can't be valid for a
            --  subsequent PC, so we can just resume our iteration from the
            --  last (or original) position in the set of ranges.

            loop

               pragma Assert (Has_Element (Cur));
               Current_Range := Element (Cur);

               --  If the current PC is before the start of our current range,
               --  PC is not in and no subsequent range would include it either
               --  because .First are increasing.

               if Current_Range.First > PC then
                  return False;
               end if;

               --  PC >= Current_Range.First here. If current PC is within the
               --  range, move to the next PC.

               exit when PC <= Current_Range.Last;

               --  Otherwise, try the next range. There has to be one at this
               --  spot as the early test on extreme bounds ensures that even
               --  the highest PC is <= Last_Range.Last.

               Next (Cur);
            end loop;

            --  Reach here as soon as we found a range for the current PC.
            --  Loop over to the following one.

         end loop;

         --  Reaching here, we found a range for every PC

         return True;
      end if;
   end PC_Range_Covered;

   --------------------
   -- Get_Line_State --
   --------------------

   function Get_Line_State
     (Base : Traces_Base; First : Pc_Type; Last : Pc_Type) return Line_State
   is
      use Interfaces;

      Result : Line_State := No_Code;
      It     : Entry_Iterator;
      T      : Trace_Entry;
      Ranges : Address_Range_Sets.Set;
   begin
      Init_Post (Base, It, First);

      --  Find all trace entries that intersect the address range First .. Last
      --  and update the coverage result according to the coverage state of
      --  each trace entry.

      loop
         Get_Next_Trace (T, It);
         exit when T = Bad_Trace or else T.First > Last;
         Update_Line_State (Result, T.State);
         Ranges.Include ((First => T.First, Last => T.Last));
      end loop;

      --  If there is no trace entry for this instruction range, this can only
      --  mean that it is not covered.

      if Result = No_Code then
         return Not_Covered;
      end if;

      --  We just found all trace entries that intersect the address range
      --  First .. Last. We now need to check that for every address in this
      --  range, there is actually a trace entry that covers it. Otherwise this
      --  means that there is at least one instruction that is not covered.

      if not PC_Range_Covered (Ranges, First, Last) then
         Update_Line_State (Result, Not_Covered);
      end if;

      return Result;
   end Get_Line_State;

   -----------------------
   -- Update_Line_State --
   -----------------------

   procedure Update_Line_State (L : in out Line_State; I : Known_Insn_State) is
   begin
      case L is
         when Not_Covered           =>
            if I = Not_Covered then
               L := Not_Covered;
            else
               L := Partially_Covered;
            end if;

         when Partially_Covered     =>
            null;

         when Covered               =>
            if I = Covered or else I = Both_Taken then
               L := Covered;
            else
               L := Partially_Covered;
            end if;

         when No_Code               =>
            if I = Covered or else I = Both_Taken then
               L := Covered;
            elsif I = Not_Covered then
               L := Not_Covered;
            else
               L := Partially_Covered;
            end if;

         when Not_Coverable         =>

            --  Line can't be marked as not coverable, since there *is* an
            --  associated instruction.

            raise Program_Error
              with
                "Attempting to set an instruction state to Not_Coverable,"
                & " but the instruction comes from an executable.";

         when Undetermined_Coverage =>

            --  Line can't (at the moment) be marked as undetermined line state
            --  when not using source traces.

            raise Program_Error
              with
                "Undetermined_Coverage line state reserved for source"
                & " coverage";

         when Disabled_Coverage     =>

            --  Line can't be marked as disabled coverage line state when not
            --  using source traces.

            raise Program_Error
              with
                "Disabled_Coverage line state reserved for source"
                & " coverage";

      end case;
   end Update_Line_State;

end Coverage.Object;
