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

with Interfaces;

with Elf_Disassemblers; use Elf_Disassemblers;
with MC_DC;             use MC_DC;
with SC_Obligations;    use SC_Obligations;
with Traces;            use Traces;
with Traces_Lines;      use Traces_Lines;

package body Coverage.Source is

   --  For each source coverage obligation, we maintain a corresponding
   --  source coverage information record, which denotes the coverage state of
   --  the SCO.

   package Evaluation_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => MC_DC.Evaluation);

   type Source_Coverage_Info
     (Level : Source_Coverage_Level := Stmt;
      Kind  : SCO_Kind := Statement)
   is record
      case Level is
         when Stmt =>
            Executed : Boolean;
            --  Set True when the statement has been executed

         when Decision =>
            Outcome_True, Outcome_False : Boolean;
            --  Each of these components is set True when the corresponding
            --  outcome has been exercised.

         when MCDC =>
            Evaluations : Evaluation_Vectors.Vector;
            --  History of all evaluations of this decision
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
         --  No SCOs associated with this source line.

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

      --  Coverage status of each SCO is not taken into account???
      --  Should use the corresponding item(s) in SCI_Vector???

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

   procedure Compute_Source_Coverage
     (Subp_Name : String_Access;
      Subp_Info : in out Subprogram_Info)
   is
      pragma Unreferenced (Subp_Name);
      use type Interfaces.Unsigned_32;
      PC       : Pc_Type;
      It       : Entry_Iterator;
      T        : Trace_Entry;
      Insn_Len : Natural;
   begin
      Init (Subp_Info.Traces.all, It, 0);

      loop
         Get_Next_Trace (T, It);
         exit when T = Bad_Trace;

         PC := T.First;
         while PC <= T.Last loop
            Insn_Len :=
              Disa_For_Machine (Machine).
                Get_Insn_Length (Subp_Info.Insns (PC .. Subp_Info.Insns'Last));

            declare
               Insn : Binary_Content renames
                        Subp_Info.Insns (PC .. PC + Pc_Type (Insn_Len) - 1);

               Branch     : Branch_Kind;
               Flag_Indir : Boolean;
               Flag_Cond  : Boolean;
               Dest       : Pc_Type;
               --  Properties of Insn

            begin
               Disa_For_Machine (Machine).Get_Insn_Properties
                 (Insn_Bin   => Insn,
                  Pc         => PC,
                  Branch     => Branch,
                  Flag_Indir => Flag_Indir,
                  Flag_Cond  => Flag_Cond,
                  Dest       => Dest);

               --  Do stuff???

               PC := PC + Pc_Type (Insn_Len);

               --  Handle case where PC wraps

               exit when PC = 0;
            end;
         end loop;
      end loop;

   end Compute_Source_Coverage;

end Coverage.Source;
