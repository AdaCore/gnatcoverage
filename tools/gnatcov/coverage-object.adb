------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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
