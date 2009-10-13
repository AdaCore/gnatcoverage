------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

with Interfaces;

package body Coverage.Object is

   ------------------------
   -- Compute_Line_State --
   ------------------------

   procedure Compute_Line_State (Line : Line_Info_Access) is
      El : Object_Coverage_Info_Acc;
      State : Line_State := No_Code;
   begin
      El := Line.Obj_First;
      while El /= null loop
         Update_Line_State (State, El.State);
         El := El.Next;
      end loop;
      Line.State := State;
   end Compute_Line_State;

   --------------------
   -- Get_Line_State --
   --------------------

   function Get_Line_State
     (Base  : Traces_Base;
      First : Pc_Type;
      Last  : Pc_Type) return Line_State
   is
      use Interfaces;

      Result : Line_State := No_Code;
      It     : Entry_Iterator;
      T      : Trace_Entry;
   begin
      Init_Post (Base, It, First);
      loop
         Get_Next_Trace (T, It);
         exit when T = Bad_Trace or else T.First > Last;
         Update_Line_State (Result, T.State);
      end loop;

      if Result = No_Code then
         --  No trace for this instruction range. This can only mean that
         --  it is not covered.

         Result := Not_Covered;
      end if;

      return Result;
   end Get_Line_State;

   -----------------------
   -- Update_Line_State --
   -----------------------

   procedure Update_Line_State
     (L : in out Line_State;
      I : Known_Insn_State)
   is
   begin
      case L is
         when Not_Covered =>
            if I = Not_Covered then
               L := Not_Covered;
            else
               L := Partially_Covered;
            end if;

         when Partially_Covered =>
            null;

         when Covered =>
            if I = Covered or else I = Both_Taken then
               L := Covered;
            else
               L := Partially_Covered;
            end if;

         when No_Code =>
            if I = Covered or else I = Both_Taken then
               L := Covered;
            elsif I = Not_Covered then
               L := Not_Covered;
            else
               L := Partially_Covered;
            end if;

      end case;
   end Update_Line_State;

end Coverage.Object;
