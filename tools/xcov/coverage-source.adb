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

with Ada.Containers.Vectors;

with SC_Obligations; use SC_Obligations;
with Traces_Lines;   use Traces_Lines;

package body Coverage.Source is

   --  For each source coverage obligation, we maintain a corresponding
   --  source coverage information record, which denotes the coverage state of
   --  the SCO.

   type Source_Coverage_Info
     (Level : Source_Coverage_Level := Stmt;
      Kind  : SCO_Kind := Statement)
   is record
      case Level is
         when Stmt =>
            Executed : Boolean;

         when Decision =>
            Outcome_True, Outcome_False : Boolean;

         when MCDC =>
            --  For decisions, the history of evaluations is recorded here
            null;
      end case;
   end record;

   package SCI_Vectors is new Ada.Containers.Vectors
       (Index_Type   => Valid_SCO_Id,
        Element_Type => Source_Coverage_Info);
   SCI_Vector : SCI_Vectors.Vector;
   pragma Unreferenced (SCI_Vector);

   ------------------------
   -- Compute_Line_State --
   ------------------------

   procedure Compute_Line_State (Line : Line_Info_Access) is
      Obj_Info : Object_Coverage_Info_Acc;
   begin
      if Line.Src_First = null then
         --  No scos associated with this source line.

         --  ??? Have a debug mode to warn if there is object code with
         --  this line ?
         Line.State := No_Code;
         return;
      end if;

      if Line.Obj_First = null then
         --  No object code associated with this source line.
         Line.State := No_Code;
         return;
      end if;

      Obj_Info := Line.Obj_First;
      while Obj_Info /= null loop
         if Obj_Info.State = Partially_Covered
           or else Obj_Info.State = Covered
         then
            Line.State := Covered;
            return;
         end if;
         Obj_Info := Obj_Info.Next;
      end loop;
      Line.State := Not_Covered;
   end Compute_Line_State;

   --------------------
   -- Process_Traces --
   --------------------

   procedure Process_Traces (Base : Traces_Base) is
   begin
      raise Program_Error with "not implemented yet";
   end Process_Traces;

end Coverage.Source;
