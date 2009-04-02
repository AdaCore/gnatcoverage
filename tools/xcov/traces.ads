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

with Interfaces; use Interfaces;
with System;     use System;

package Traces is

   Big_Endian_Host : constant Boolean := Default_Bit_Order = High_Order_First;
   --  Host order is determined by System.Default_Bit_Order

   --  Define the size of the PC.
   subtype Pc_Type is Unsigned_32;
   Pc_Type_Size : constant Unsigned_8 := Pc_Type'Size / System.Storage_Unit;

   --  Type of a branch instruction.
   type Branch_Kind is (Br_None, Br_Call, Br_Ret, Br_Jmp);

   --  Target machine.  The value is the EM field defined by ELF.
   --  Set to 0 when unknown.
   Machine : Unsigned_16 := 0;

   --  High level state of a trace entry.
   type Trace_State is
     (
      --  Not yet filled.
      Unknown,

      --  The code is not covered.  No instruction was executed.
      Not_Covered,

      --  The code is fully covered (and there is no conditionnal branches).
      Covered,

      --  The code is covered, the last instruction is a branch but the
      --  branch was always taken.
      Branch_Taken,

      --  The code is covered, the last instruction is a branch but the
      --  branch was never taken.
      Fallthrough_Taken,

      --  The code is covered, the last instruction is a branch and the
      --  branch was both taken and not taken.
      Both_Taken
      );

   --  Trace entry as put in the traces tree.
   type Trace_Entry is record
      First : Pc_Type;
      Last : Pc_Type;
      Op : Unsigned_8;
      State : Trace_State;
   end record;

   --  An invalid trace.
   Bad_Trace : constant Trace_Entry := (First => 1,
                                        Last => 0,
                                        Op => 0,
                                        State => Unknown);

   --  Display a string for OP.
   procedure Dump_Op (Op : Unsigned_8);

   --  Dump (on standard output) a trace entry.
   procedure Dump_Entry (E : Trace_Entry);

   --  Display a character representing the state.
   procedure Disp_State_Char (State : Trace_State);

   --  Convert hexa-decimal string contained in Line (Pos ..) to a Pc_Type.
   --  Put the result to RES, POS contains the index past the last character
   --  accepted.
   procedure Get_Pc (Res : out Pc_Type; Line : String; Pos : in out Natural);

   type Trace_State_Map is array (Trace_State) of Character;
   Trace_State_Char : constant Trace_State_Map;
   --  One character representation of a state.
   --  Several states can be represented by the same character, if the
   --  difference is not meaningful to the user of xcov. Typically, Covered
   --  and Both_Taken: internally, it conveys the information that the
   --  corresponding instruction is a branch (or not); the user of xcov
   --  has no interest in this distinction.

private
   Trace_State_Char : constant Trace_State_Map :=
     (Unknown => '?',
      Not_Covered => '-',
      Covered => '+',
      Branch_Taken => '>',
      Fallthrough_Taken => 'v',
      Both_Taken => '+');

end Traces;
