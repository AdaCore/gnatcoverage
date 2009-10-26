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

   use Ada.Containers;

   --  For each source coverage obligation, we maintain a corresponding
   --  source coverage information record, which denotes the coverage state of
   --  the SCO. The default initialization must denote a completely uncovered
   --  state.

   package Evaluation_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => MC_DC.Evaluation);

   type Source_Coverage_Info
     (Level : Source_Coverage_Level := Stmt;
      Kind  : SCO_Kind := Statement)
   is record
         Executed : Boolean := False;
         --  Set True when the statement has been executed

         case Kind is
            when Statement | Condition =>
               null;

            when Decision =>
               case Level is
                  when Stmt =>
                     null;

                  when Decision =>
                     Outcome_True, Outcome_False : Boolean := False;
                     --  Each of these components is set True when the
                     --  corresponding outcome has been exercised.

                  when MCDC =>
                     Evaluations : Evaluation_Vectors.Vector;
                     --  History of all evaluations of this decision
               end case;
         end case;
   end record;

   procedure Set_Executed (SCI : in out Source_Coverage_Info);
   --  Set Executed to True

   package SCI_Vectors is new Ada.Containers.Vectors
       (Index_Type   => Valid_SCO_Id,
        Element_Type => Source_Coverage_Info);
   SCI_Vector : SCI_Vectors.Vector;

   ------------------------
   -- Compute_Line_State --
   ------------------------

   procedure Compute_Line_State (Line : Line_Info_Access) is
   begin
      if Line.SCOs.Length = 0 then
         --  No SCOs associated with this source line.

         --  ??? Have a debug mode to warn if there is object code with
         --  this line ?

         Line.State := No_Code;
         return;
      end if;

      if Line.Obj_First = null then
         --  No object code associated with this source line

         Line.State := No_Code;
         return;
      end if;

      --  Statement coverage: Line is covered if any associated SCO is
      --  marked Executed.

      for J in Line.SCOs.First_Index .. Line.SCOs.Last_Index loop
         declare
            SCO : constant SCO_Id := Line.SCOs.Element (J);
         begin
            if SCO in SCI_Vector.First_Index .. SCI_Vector.Last_Index
              and then SCI_Vector.Element (SCO).Executed
            then
               Line.State := Covered;
               return;
            end if;
         end;
      end loop;

      --  Here if no executed Statement SCO

      Line.State := Not_Covered;
   end Compute_Line_State;

   -----------------------------
   -- Compute_Source_Coverage --
   -----------------------------

   procedure Compute_Source_Coverage
     (Subp_Name : String_Access;
      Subp_Info : in out Subprogram_Info)
   is
      pragma Unreferenced (Subp_Name);
      use type Interfaces.Unsigned_32;

      PC                : Pc_Type;
      It                : Entry_Iterator;
      T                 : Trace_Entry;
      Insn_Len          : Natural;
      SCO, S_SCO, P_SCO : SCO_Id;
   begin
      --  Iterate over traces for this routine

      Init (Subp_Info.Traces.all, It, 0);
      loop
         Get_Next_Trace (T, It);
         exit when T = Bad_Trace;

         PC := T.First;
         Trace_Insns :
         while PC <= T.Last loop
            Insn_Len :=
              Disa_For_Machine (Machine).
                Get_Insn_Length (Subp_Info.Insns (PC .. Subp_Info.Insns'Last));

            --  Find SCO for this instruction

            SCO := Sloc_To_SCO (Get_Sloc (Subp_Info.Exec.all, PC));
            if SCO = No_SCO_Id then
               goto Continue_Trace_Insns;
            end if;

            --  Ensure there is a coverage entry for this SCO

            if SCO > SCI_Vector.Last_Index then
               SCI_Vector.Set_Length
                 (Count_Type (SCO - SCI_Vector.First_Index + 1));
               declare
                  New_SCI : Source_Coverage_Info
                              (Kind  => Kind (SCO),
                               Level => Get_Coverage_Level);
               begin
                  SCI_Vector.Replace_Element (SCO, New_SCI);
               end;
            end if;

            --  Mark all enclosing SCOs as executed

            S_SCO := SCO;
            loop
               --  Mark S_SCO as executed

               SCI_Vector.Update_Element (S_SCO, Set_Executed'Access);

               --  For statements, propagate back to beginning of basic block

               if Kind (S_SCO) = Statement then
                  P_SCO := S_SCO;
                  loop
                     P_SCO := Previous (P_SCO);
                     exit when P_SCO = No_SCO_Id
                                 or else SCI_Vector.Element (P_SCO).Executed;
                     SCI_Vector.Update_Element (P_SCO, Set_Executed'Access);
                  end loop;
               end if;

               S_SCO := Parent (S_SCO);
               exit when S_SCO = No_SCO_Id;
            end loop;

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

            end;

            <<Continue_Trace_Insns>>
            PC := PC + Pc_Type (Insn_Len);

            --  Handle case where PC wraps

            exit Trace_Insns when PC = 0;
         end loop Trace_Insns;
      end loop;

   end Compute_Source_Coverage;

   ------------------
   -- Set_Executed --
   ------------------

   procedure Set_Executed (SCI : in out Source_Coverage_Info) is
   begin
      SCI.Executed := True;
   end Set_Executed;

end Coverage.Source;
